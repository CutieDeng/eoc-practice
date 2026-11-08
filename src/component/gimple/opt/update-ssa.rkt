#lang racket/base

;; ============================================================
;; opt: the incremental SSA update with a name replacement table
;; (gcc/tree-into-ssa.cc update_ssa for new_ssa_names / old_ssa_names)
;; ============================================================
;;
;;   (update-ssa st fndecl cfg table #:phis? [phis? #t]) -> (values cfg st)
;;     TABLE: (cons new old) per create_new_def_for, in creation order;
;;     the new names are already defined in CFG and the old names still
;;     used.  A new name replaces OLD and, when OLD is itself a new name,
;;     every name OLD replaces (add_new_name_mapping).  Registered names
;;     whose definition is gone (their block was removed before the
;;     update) are the names to release: their uses are still renamed,
;;     they are dumped, and they are released in version order at the end
;;     (release_ssa_name_after_update_ssa, delete_update_ssa).  PHIS? = TODO_update_ssa (insert PHI nodes for the old names
;;     at the pruned iterated dominance frontier of their definitions),
;;     #f = TODO_update_ssa_no_phi.  The uses reached by a new
;;     definition are rewritten in a dominator walk from the nearest
;;     common dominator of the blocks to update; the dump lines
;;     (dump_update_ssa: the replacement table in version order, then
;;     the start block and the block counts) are added to CFG's messages.
;;   dominator helpers shared with the loop passes:
;;     (dom-of cfg), (dominates? d a b), (idom-of d bb), (ncd d bbs),
;;     (dom-kids d bb) (children in RPO, as dom_walker visits them)
;;
;;   #:released-before: the versions the caller released since the table
;;   was built (cleanup_control_flow_pre's removed blocks); the unregistered
;;   ones were freed at once in GCC and are queued before names_to_release.
;;
;; When every new name is to be released, nothing is inserted, rewritten
;; or dumped (goto done).  The replacement table lists new_ssa_names
;; without the released names.  EXIT is in no dominance frontier
;; (compute_dominance_frontiers walks FOR_EACH_BB_FN); the predecessors
;; of a PHI block are marked even when one is ENTRY.
;;
;; Version gates (tree-into-ssa.cc 12.5, 13.3, 14.2, 15.2): the rewrite
;; walk visits dominator sons in RPO for GCC 12 (dom_walker sorts them)
;; and in the order they were attached for GCC >= 13 (no RPO given),
;; modelled as block-chain order, i.e. dominators freshly computed;
;; successor PHIs are rewritten in mark_phi_for_rewrite order for
;; GCC <= 14 (old names in version order, each in immediate-use order,
;; then the inserted PHIs) and in PHI order for GCC 15.
;;
;; Not ported: symbols to rename (ssa_renaming_needed), virtual operand
;; renaming, get_reaching_def's default-definition fallback (a use no
;; new definition reaches keeps its name), dominator son order after
;; incremental updates (split_edge, iterate_fix_dominators attach the
;; new son last).
;; ============================================================

(require racket/list
         racket/format
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../../../driver/dominance/dom.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         "../cfg.rkt"
         (only-in "../ssa.rkt" stmt-vuse)
         (only-in "../print.rkt" print-expr)
         "ssa-ops.rkt"
         (only-in "mfn.rkt" stmt-use-names stmt-def-names)
         (only-in "range-info.rkt" copy-range-info))

(provide update-ssa dom-of dominates? idom-of ncd dom-kids map-ssa-uses)

(define (dom-of cfg)
  (compute-dom (pvector-cons-left (pvector-cons-left (cfg-bbs cfg) EXIT) ENTRY) ENTRY
               (lambda (i) (for/pvector ([e (in-pvector (cfg-succs cfg i))]) (GEdge-dest e)))))
(define (dominates? d a b) (dom-dominates? d (dom-index d a) (dom-index d b)))
(define (idom-of d bb)
  (define i (dom-index d bb))
  (define j (and i (dom-idom d i)))
  (and j (not (= j i)) (dom-node d j)))
(define (ncd d bbs)
  (define is (for/list ([b (in-list bbs)] #:when (dom-index d b)) (dom-index d b)))
  (and (pair? is) (dom-node d (for/fold ([a (car is)]) ([b (in-list (cdr is))]) (dom-nearest-common-dominator d a b)))))
(define (dom-kids d bb)
  (define i (dom-index d bb))
  (if i (for/list ([k (in-list (sort (pvector->list (dom-children d i)) <))]) (dom-node d k)) '()))

;; the SSA uses of a statement mapped by F (the VUSE included)
(define (map-ssa-uses st g f)
  (define g1 (stmt-map-uses st g f))
  (define v (stmt-vuse g1))
  (if (and v (ssa-name? v)) (stmt-set-flag g1 'vuse (f v)) g1))

(define (update-ssa st0 fndecl cfg0 table #:phis? [phis? #t] #:released-before [released-before '()])
  (let/ec return
  (define major (gcc-major-version st0))
  (define st st0)
  (define cfg cfg0)
  (define stamp (cfg-next-stamp cfg0))
  (define (fresh-stamp!) (begin0 stamp (set! stamp (add1 stamp))))
  (define (msg! fmt . args) (set! cfg (cfg-add-message cfg (apply format fmt args))))
  (define (block-stmts bb) (GBlock-stmts (cfg-block cfg bb)))
  (define (block-phis bb) (GBlock-phis (cfg-block cfg bb)))
  (define (set-stmts! bb v) (set! cfg (set-block cfg (block-set-stmts (cfg-block cfg bb) v))))
  (define (set-phis! bb v) (set! cfg (set-block cfg (block-set-phis (cfg-block cfg bb) v))))
  ;; new version -> the old names it replaces (names_replaced_by)
  (define new->old (make-hasheqv))
  (define news (map car table))
  (define (replaced-set old)
    (define more (hash-ref new->old (ssa-name-version old) '()))
    (cons old (for/list ([o (in-list more)] #:unless (ssa-name=? o old)) o)))
  (for ([p (in-list table)]) (hash-set! new->old (ssa-name-version (car p)) (replaced-set (cdr p))))
  ;; create_new_def_for / duplicate_ssa_name for an inserted PHI
  (define (new-def-for! old)
    (define var (ssa-name-var old))
    (define-values (n st1) (if var (make-ssa-name st var) (make-ssa-name st (tree-type old) #:name (tree-attr old 'identifier #f))))
    (set! st (copy-range-info st1 fndecl old n))
    (set! news (append news (list n)))
    (hash-set! new->old (ssa-name-version n) (replaced-set old))
    n)
  (define (def-site-map)
    (define m (make-hasheqv))
    (for ([bb (in-pvector (cfg-bbs cfg))])
      (for ([ph (in-pvector (block-phis bb))]) (hash-set! m (ssa-name-version (gphi-result ph)) bb))
      (for* ([g (in-pvector (block-stmts bb))] [d (in-list (stmt-def-names g))]) (hash-set! m (ssa-name-version d) bb)))
    m)
  (define dom (dom-of cfg))
  (define (olds-of n) (hash-ref new->old (ssa-name-version n)))
  (define olds-by-version
    (for*/fold ([m (hasheqv)]) ([n (in-list news)] [o (in-list (olds-of n))]) (hash-set m (ssa-name-version o) o)))
  (define old-versions (sort (hash-keys olds-by-version) <))
  (define (old? t) (and (ssa-name? t) (hash-has-key? olds-by-version (ssa-name-version t))))
  (define (new? t) (and (ssa-name? t) (hash-has-key? new->old (ssa-name-version t))))
  (define blocks intbits-empty)
  (define (mark! bb) (set! blocks (intbits-set blocks bb)))
  (define def-blocks (make-hasheqv)) (define phi-blocks (make-hasheqv)) (define livein (make-hasheqv))
  (define (add! h v bb) (hash-set! h v (intbits-set (hash-ref h v intbits-empty) bb)))
  (define (ref h v) (hash-ref h v intbits-empty))
  (define sites (def-site-map))
  ;; names_to_release: registered names whose definition was removed
  (define to-release
    (sort (for/list ([v (in-list (remove-duplicates (append (map ssa-name-version news) (hash-keys olds-by-version))))]
                     #:unless (hash-ref sites v #f)
                     #:unless (let ([o (hash-ref olds-by-version v #f)]) (and o (ssa-default-def? o))))
            v)
          <))
  (define (released? v) (memv v to-release))
  (define (phi-def? v bb) (for/or ([ph (in-pvector (block-phis bb))]) (= (ssa-name-version (gphi-result ph)) v)))
  ;; prepare_def_site_for: a new name is a definition of the names it replaces
  (define (prepare-def-site! name olds)
    (define bb (hash-ref sites (ssa-name-version name) #f))
    (when bb
      (mark! bb)
      (when phis?
        (for ([o (in-list olds)])
          (add! def-blocks (ssa-name-version o) bb)
          (when (phi-def? (ssa-name-version name) bb) (add! phi-blocks (ssa-name-version o) bb))))))
  (for ([n (in-list (sort news < #:key ssa-name-version))]) (prepare-def-site! n (olds-of n)))
  ;; GCC <= 14: the PHIs to rewrite in each block, in the order they were
  ;; marked (mark_phi_for_rewrite: the old names in version order, each
  ;; in immediate-use order, then the inserted PHIs)
  (define phi-mark (make-hasheqv))
  (define (mark-phi! res) (unless (hash-has-key? phi-mark (ssa-name-version res)) (hash-set! phi-mark (ssa-name-version res) (hash-count phi-mark))))
  ;; the releases at the end (delete_update_ssa): the names the caller
  ;; released before (unregistered ones were freed at once), then
  ;; names_to_release in version order
  (define (finish st)
    (define registered (for*/hasheqv ([n (in-list news)] [v (in-list (cons (ssa-name-version n) (map ssa-name-version (olds-of n))))]) (values v #t)))
    (define st1 (for/fold ([st st]) ([v (in-list released-before)] #:unless (hash-ref registered v #f)) (ssa-queue-release st v)))
    (for/fold ([st st1]) ([v (in-list to-release)]) (ssa-queue-release st v)))
  (for ([ov (in-list old-versions)])
    (define o (hash-ref olds-by-version ov))
    (unless (released? ov) (prepare-def-site! o (list o)))
    (let ([uses (for*/list ([bb (in-pvector (cfg-bbs cfg))] [ph (in-pvector (block-phis bb))]
                            [k (in-range (pvector-length (gphi-args ph)))]
                            [a (in-value (pvector-ref (gphi-args ph) k))] #:when (ssa-name=? (cdr a) o))
                  (define sts (phi-arg-stamps ph))
                  (cons (or (and sts (< k (pvector-length sts)) (pvector-ref sts k)) -1) (gphi-result ph)))])
      (for ([u (in-list (sort uses > #:key car))]) (mark-phi! (cdr u))))
    ;; prepare_use_sites_for
    (for ([bb (in-pvector (cfg-bbs cfg))])
      (for* ([ph (in-pvector (block-phis bb))] [a (in-pvector (gphi-args ph))] #:when (ssa-name=? (cdr a) o))
        (mark! bb) (mark! (car a))
        (when (and phis? (not (intbits-ref (ref def-blocks ov) (car a)))) (add! livein ov (car a))))
      (for ([g (in-pvector (block-stmts bb))] #:when (for/or ([u (in-list (stmt-use-names g))]) (ssa-name=? u o)))
        (mark! bb)
        (when (and phis? (not (intbits-ref (ref def-blocks ov) bb))) (add! livein ov bb)))))
  ;; every new name released: nothing to rename, no dump (goto done)
  (when (for/and ([n (in-list news)]) (released? (ssa-name-version n)))
    (return cfg (finish st)))
  ;; prune_unused_phi_nodes: a PHI survives when it is the nearest
  ;; dominating definition of a use (uses propagate to the PHI's preds)
  (define (prune-unused-phi-nodes phis kills uses)
    (cond
      [(intbits-empty? uses) '()]
      [else
       (define phis1 (for/list ([b (in-list phis)] #:unless (and (intbits-ref kills b) (not (intbits-ref uses b)))) b))
       (cond
         [(null? phis1) '()]
         [else
          (define (def-block? b) (or (intbits-ref kills b) (memv b phis1)))
          (define (nearest-def b) (let loop ([b b]) (cond [(not b) #f] [(def-block? b) b] [else (loop (idom-of dom b))])))
          (define live (make-hasheqv))
          (define uses* uses)
          (let loop ([work (intbits->list uses)])
            (unless (null? work)
              (define b (car work))
              (define p (cond [(= b ENTRY) #f] [(memv b phis1) b] [else (let ([i (idom-of dom b)]) (and i (nearest-def i)))]))
              (cond
                [(or (not p) (not (memv p phis1)) (hash-ref live p #f)) (loop (cdr work))]
                [else
                 (hash-set! live p #t)
                 (define more (for/list ([u (in-pvector (cfg-preds cfg p))] #:unless (or (intbits-ref uses* u) (intbits-ref kills u)))
                                (set! uses* (intbits-set uses* u))
                                u))
                 (loop (append (cdr work) more))])))
          (for/list ([b (in-list phis1)] #:when (hash-ref live b #f)) b)])]))
  ;; insert_updated_phi_nodes_for, old names in version order
  (define entry-limit (if (>= major 15) (single-succ cfg ENTRY) ENTRY))
  (when phis?
    (for ([ov (in-list old-versions)])
      (define o (hash-ref olds-by-version ov))
      (define dbs (ref def-blocks ov))
      (unless (intbits-empty? dbs)
        ;; compute_dominance_frontiers walks FOR_EACH_BB_FN: EXIT is in no frontier
        (define idf (sort (for*/list ([i (in-list (intbits->list (dom-idf dom (for/fold ([s intbits-empty]) ([b (in-list (intbits->list dbs))]) (intbits-set s (dom-index dom b))))))]
                                      [b (in-value (dom-node dom i))] #:unless (memv b (list ENTRY EXIT)))
                            b)
                          <))
        (define entry (ncd dom (intbits->list dbs)))
        (define pruned (if (and entry (not (= entry entry-limit)))
                           (for/list ([b (in-list idf)] #:when (and (not (= b entry)) (dominates? dom entry b))) b)
                           idf))
        (unless (null? pruned)
          ;; the blocks and all their predecessors (src->index >= 0 holds for ENTRY)
          (for ([b (in-list pruned)]) (mark! b) (for ([p (in-pvector (cfg-preds cfg b))]) (mark! p)))
          (define points (for/list ([b (in-list pruned)] #:unless (intbits-ref (ref phi-blocks ov) b)) b))
          (for ([b (in-list (sort (prune-unused-phi-nodes points dbs (ref livein ov)) <))])
            (mark! b)
            (define nn (new-def-for! o))
            (mark-phi! nn)
            (define preds (cfg-preds cfg b))
            (set-phis! b (pvector-cons-right (block-phis b)
                                             (gphi #f (hasheq 'arg-stamps (for/pvector ([p (in-pvector preds)]) (fresh-stamp!)))
                                                   (ssa-name-var o) nn (for/pvector ([p (in-pvector preds)]) (cons p o))))))))))
  ;; rewrite_blocks (REWRITE_UPDATE) from the nearest common dominator
  (define start (ncd dom (intbits->list blocks)))
  (define (rewrite-uses g cur)
    (define changed #f)
    (define g1 (map-ssa-uses st g (lambda (t)
                                    (define rd (and (old? t) (hash-ref cur (ssa-name-version t) #f)))
                                    (cond [(and rd (not (ssa-name=? rd t))) (set! changed #t) rd] [else t]))))
    (if changed (stmt-set-flag g1 'stamp (fresh-stamp!)) g1))
  ;; register_new_update_set for a new name, register_new_update_single
  ;; for an old one (a name can be both)
  (define (register cur name)
    (define c1 (if (new? name)
                   (for/fold ([c cur]) ([o (in-list (olds-of name))]) (hash-set c (ssa-name-version o) name))
                   cur))
    (if (old? name) (hash-set c1 (ssa-name-version name) name) c1))
  (define (walk bb cur)
    (define cur1
      (cond
        [(not (intbits-ref blocks bb)) cur]
        [else
         (define c1 (for/fold ([c cur]) ([ph (in-pvector (block-phis bb))]) (register c (gphi-result ph))))
         (define-values (stmts* c2)
           (for/fold ([acc (pvector-empty)] [c c1]) ([g (in-pvector (block-stmts bb))])
             (define g1 (rewrite-uses g c))
             (values (pvector-cons-right acc g1) (for/fold ([c c]) ([d (in-list (stmt-def-names g1))]) (register c d)))))
         (set-stmts! bb stmts*)
         (for ([e (in-list (pvector->list (cfg-succs cfg bb)))])
           (define d (GEdge-dest e))
           (when (and (cfg-block cfg d) (not (pvector-empty? (block-phis d))))
             (define (rewrite-phi ph)
               (define k (for/first ([a (in-pvector (gphi-args ph))] [i (in-naturals)] #:when (= (car a) bb)) i))
               (define a (and k (cdr (pvector-ref (gphi-args ph) k))))
               (define rd (and a (old? a) (hash-ref c2 (ssa-name-version a) #f)))
               (cond
                 [(and rd (not (ssa-name=? rd a)))
                  (define stamps (or (phi-arg-stamps ph) (for/pvector ([x (in-pvector (gphi-args ph))]) #f)))
                  (gphi (gphi-loc ph) (hash-set (gphi-flags ph) 'arg-stamps (pvector-set stamps k (fresh-stamp!))) (gphi-var ph) (gphi-result ph)
                        (pvector-set (gphi-args ph) k (cons bb rd)))]
                 [else ph]))
             ;; GCC <= 14 walks phis_to_rewrite (marking order), GCC 15 the PHI sequence
             (define v (list->vector (pvector->list (block-phis d))))
             (define order
               (if (< major 15)
                   (sort (for/list ([i (in-range (vector-length v))]) i) <
                         #:key (lambda (i) (hash-ref phi-mark (ssa-name-version (gphi-result (vector-ref v i))) +inf.0)))
                   (for/list ([i (in-range (vector-length v))]) i)))
             (for ([i (in-list order)]) (vector-set! v i (rewrite-phi (vector-ref v i))))
             (set-phis! d (list->pvector (vector->list v)))))
         c2]))
    (for ([k (in-list (walk-kids bb))]) (walk k cur1)))
  ;; GCC >= 13 builds the walker without an RPO order: the dominator sons
  ;; are visited in the order they were attached, block-chain order for
  ;; freshly computed dominators (et_set_father puts the newest son
  ;; first, the walk pops the last pushed); GCC 12 sorts them by RPO
  (define chain-pos (for/hasheqv ([b (in-pvector (cfg-bbs cfg0))] [i (in-naturals)]) (values b i)))
  (define (walk-kids bb)
    (if (>= major 13)
        (sort (dom-kids dom bb) < #:key (lambda (b) (hash-ref chain-pos b -1)))
        (dom-kids dom bb)))
  (when start (walk start (hasheqv)))
  ;; dump_update_ssa and the summary lines
  ;; new_ssa_names without the released names (prepare_names_to_update)
  (define live-news (for/list ([n (in-list (sort news < #:key ssa-name-version))] #:unless (released? (ssa-name-version n))) n))
  (unless (null? live-news)
    (msg! "") (msg! "SSA replacement table") (msg! "N_i -> { O_1 ... O_j } means that N_i replaces O_1, ..., O_j") (msg! ""))
  (for ([n (in-list live-news)])
    (msg! "~a -> { ~a}" (print-expr st n)
          (apply string-append (for/list ([o (in-list (sort (olds-of n) < #:key ssa-name-version))]) (string-append (print-expr st o) " ")))))
  (unless (null? to-release)
    (msg! "") (msg! "SSA names to release after updating the SSA web") (msg! "")
    (msg! "~a" (apply string-append (for/list ([v (in-list to-release)])
                                      (string-append (print-expr st (or (hash-ref olds-by-version v #f) (findf (lambda (n) (= (ssa-name-version n) v)) news))) " ")))))
  (msg! "Incremental SSA update started at block: ~a" start)
  (define lbb (last-basic-block cfg))
  (define c (intbits-count blocks))
  (msg! "Number of blocks in CFG: ~a" lbb)
  (msg! "Number of blocks to update: ~a (~a%)" c (~r (round (/ (* c 100) lbb)) #:min-width 3 #:pad-string " "))
  (msg! "") (msg! "")
  ;; delete_update_ssa: the names to release, in version order
  (values cfg (finish st))))
