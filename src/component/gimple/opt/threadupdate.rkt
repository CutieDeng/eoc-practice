#lang racket/base

;; ============================================================
;; opt: the forward jump threader's CFG update
;; (gcc/tree-ssa-threadupdate.cc fwd_jt_path_registry::update_cfg), GCC 15
;; ============================================================
;;
;;   (thread-through-all-blocks st fndecl cfg paths loops)
;;     -> (values cfg st table threaded?)
;;   PATHS: the registered jump threads in registration order, each a
;;   list of (src dest type) with type start / copy-src / copy-joiner /
;;   no-copy (EDGE_START_JUMP_THREAD, EDGE_COPY_SRC_BLOCK,
;;   EDGE_COPY_SRC_JOINER_BLOCK, EDGE_NO_COPY_SRC_BLOCK); LOOPS: the
;;   loops of CFG.  TABLE: the (new . old) pairs create_new_def_for
;;   recorded while duplicating, in creation order, for update_ssa.
;;
;; mark_threaded_blocks (paths without a joiner first, then joiner
;; paths whose later edges carry no other thread, the PHI argument
;; check at a joiner's final destination), then thread_block for every
;; threaded block in post order, once for the paths without a joiner
;; and once for the joiner paths (thread_block_1): the noloop_only
;; checks, the redirection data per distinct path (its incoming edges
;; listed newest first), the duplicates (create_block_for_threading:
;; PHIs and statements copied with new names, the successor edges
;; copied, the count left for update_profile; the second copied block
;; of a path is created before the first), ssa_fix_duplicate_block_edges
;; (a copied block loses its control statement and gets one edge to the
;; path's final destination; a joiner copy keeps its condition and the
;; on-path edge is redirected), compute_path_counts / update_profile,
;; and ssa_redirect_edges (the incoming edge goes to the duplicate, the
;; PHI arguments of the old destination with it).
;;
;; Fixture coverage: thread.c thr (three paths through one block, the
;; template copies), thr_join (a joiner path: the copy keeps its
;; condition, its on-path edge goes to the final destination).
;;
;; The redirection data live in a model of GCC's hash_table (open
;; addressing over prime_tab sizes, hash = index of the final
;; destination, double hashing, expansion at 3/4), so they are visited
;; in GCC's slot order: the first one's copy is the template, the
;; others are copies of the template (their names replace the
;; template's names and, through them, the original ones).
;;
;; Not ported: thread_through_loop_header (an error when a thread
;; through a loop header remains), the removed-edge filter of update_cfg
;; (m_removed_edges), debug statements.  Edges are identified by their
;; (src . dest) pair, so a path whose later edge was redirected by an
;; earlier thread (GCC follows the edge object) is not followed.
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         "../cfg.rkt"
         (only-in "../ssa.rkt" stmt-vdef)
         (only-in "../gimplify.rkt" is-gimple-min-invariant)
         "ssa-ops.rkt"
         (only-in "loops.rkt" loops-innermost-first)
         (only-in "range-info.rkt" copy-range-info))

(provide thread-through-all-blocks)

;; ---- profile arithmetic (profile.h) ------------------------------------
(define MAXP 134217728)
(define (rdiv a b) (quotient (+ a (quotient b 2)) b))
;; profile_count::apply_probability
(define (count-apply c p)
  (cond
    [(and c (= c 0)) 0]
    [(eqv? p MAXP) c]
    [(eqv? p 0) 0]
    [(not (and c p)) #f]
    [else (rdiv (* c p) MAXP)]))
;; profile_count::probability_in (counts are never PRECISE here)
(define (probability-in c overall)
  (cond
    [(not (and c overall)) #f]
    [(= overall 0) #f]
    [(< overall c) MAXP]
    [else (rdiv (* c MAXP) overall)]))
;; profile_probability operator/ and operator-, profile_count operator-
(define (p/ a b)
  (cond
    [(eqv? a 0) 0]
    [(not (and a b)) #f]
    [(>= a b) MAXP]
    [else (min (rdiv (* a MAXP) b) MAXP)]))
(define (p- a b) (if (and a b) (if (>= a b) (- a b) 0) #f))
(define (c- a b) (if (and a b) (if (>= a b) (- a b) 0) #f))
(define (c< a b) (and a b (< a b)))
(define (p> a b) (and a b (> a b)))

(define (thread-through-all-blocks st0 fndecl cfg0 paths0 loops)
  (define st st0)
  (define cfg cfg0)
  (define stamp (cfg-next-stamp cfg0))
  (define (fresh-stamp!) (begin0 stamp (set! stamp (add1 stamp))))

  ;; ---- CFG / profile helpers ---------------------------------------
  (define (block bb) (cfg-block cfg bb))
  (define (phis bb) (GBlock-phis (block bb)))
  (define (set-phis! bb v) (set! cfg (set-block cfg (block-set-phis (block bb) v))))
  (define (succ-dests bb) (for/list ([e (in-pvector (cfg-succs cfg bb))]) (GEdge-dest e)))
  (define (preds bb) (pvector->list (cfg-preds cfg bb)))
  (define (count bb) (prof-count cfg bb))
  (define (set-count! bb c)
    (define pf (GCfg-profile cfg))
    (when pf
      (set! cfg (cfg-set-profile cfg (GProf (if c (intmap-set (GProf-counts pf) bb c) (intmap-remove (GProf-counts pf) bb))
                                            (GProf-probs pf))))))
  (define (prob e) (prof-prob cfg (car e) (cdr e)))
  (define (set-prob! e p)
    (define pf (GCfg-profile cfg))
    (when (and pf p) (set! cfg (cfg-set-profile cfg (GProf (GProf-counts pf) (hash-set (GProf-probs pf) e p))))))
  (define (edge-count e) (count-apply (count (car e)) (prob e)))
  (define (succ-edges bb) (for/list ([d (in-list (succ-dests bb))]) (cons bb d)))
  ;; the PHI argument slot of a new predecessor (reserve_phi_args_for_new_edge)
  (define (append-phi-slot! dest src)
    (set-phis! dest (for/pvector ([ph (in-pvector (phis dest))])
                      (define stamps (or (phi-arg-stamps ph) (for/pvector ([a (in-pvector (gphi-args ph))]) #f)))
                      (gphi (gphi-loc ph) (hash-set (gphi-flags ph) 'arg-stamps (pvector-cons-right stamps #f))
                            (gphi-var ph) (gphi-result ph) (pvector-cons-right (gphi-args ph) (cons src #f))))))
  ;; add_phi_arg for the edge SRC -> DEST of the K-th PHI
  (define (set-phi-arg! dest src k v)
    (set-phis! dest (for/pvector ([ph (in-pvector (phis dest))] [i (in-naturals)])
                      (cond
                        [(not (= i k)) ph]
                        [else
                         (define j (for/first ([a (in-pvector (gphi-args ph))] [x (in-naturals)] #:when (= (car a) src)) x))
                         (define stamps (or (phi-arg-stamps ph) (for/pvector ([a (in-pvector (gphi-args ph))]) #f)))
                         (gphi (gphi-loc ph) (hash-set (gphi-flags ph) 'arg-stamps (pvector-set stamps j (and (ssa-name? v) (fresh-stamp!))))
                               (gphi-var ph) (gphi-result ph) (pvector-set (gphi-args ph) j (cons src v)))]))))
  (define (phi-arg ph pred) (phi-arg-for-pred ph pred))
  ;; unchecked_make_edge / make_single_succ_edge
  (define (make-edge! src dest flags p)
    (define-values (_ok c) (make-edge cfg src dest flags))
    (set! cfg c)
    (set-prob! (cons src dest) p)
    (append-phi-slot! dest src))
  ;; redirect_edge_and_branch: (values e2 created?) where CREATED? means
  ;; the redirected edge itself survives (E2 == E in GCC)
  (define (redirect-and-branch! src old new)
    (define merged? (and (find-edge cfg src new) #t))
    (define-values (c st1) (redirect-edge-and-branch st cfg src old new))
    (set! cfg c) (set! st st1)
    (define e2 (cons src new))
    (cond
      [merged? (values e2 #f)]
      [else (append-phi-slot! new src) (values e2 #t)]))
  ;; remove_ctrl_stmt_and_useless_edges
  (define (remove-ctrl-stmt-and-useless-edges! bb dest-bb)
    (define g (bb-last-stmt cfg bb))
    (when (and g (or (gcond? g) (ggoto? g) (gswitch? g)))
      (set! cfg (bb-remove-last-stmt cfg bb)))
    (let loop ([i 0])
      (when (< i (pvector-length (cfg-succs cfg bb)))
        (define d (GEdge-dest (pvector-ref (cfg-succs cfg bb) i)))
        (cond
          [(not (eqv? d dest-bb)) (set! cfg (remove-edge cfg bb d)) (loop i)]
          [else (set-prob! (cons bb d) MAXP) (loop (add1 i))]))))

  ;; ---- loops -----------------------------------------------------------
  (define orig-of (make-hasheqv))                ; duplicate -> original
  (define (orig bb) (hash-ref orig-of bb bb))
  (define (loop-father bb) (or (loop-of loops (orig bb)) 'root))
  (define (loop-parent l)
    (cond [(eq? l 'root) #f]
          [(Loop-parent l) (findf (lambda (o) (= (Loop-num o) (Loop-parent l))) loops)]
          [else 'root]))
  (define (nested? outer l)
    (let loop ([p (loop-parent l)]) (cond [(not p) #f] [(eq? p outer) #t] [else (loop (loop-parent p))])))
  (define (loop-depth l) (if (eq? l 'root) 0 (Loop-depth l)))
  (define (loop-header l) (if (eq? l 'root) ENTRY (Loop-header l)))
  (define (in-loop? l bb) (or (eq? l 'root) (loop-inside? l (orig bb))))
  (define (loop-exit-edge? l e) (and (in-loop? l (car e)) (not (in-loop? l (cdr e)))))

  ;; ---- names (gimple_duplicate_bb / create_new_def_for) ----------------
  (define table '())
  (define (new-def-for! old)
    (define var (ssa-name-var old))
    (define-values (nn st1) (if var (make-ssa-name st var) (make-ssa-name st (tree-type old) #:name (tree-attr old 'identifier #f))))
    (set! st (copy-range-info st1 fndecl old nn))
    (set! table (cons (cons nn old) table))
    nn)
  (define (copy-stmt g)
    (define g1 (if (stmt-vdef g) (stmt-set-flag g 'vdef (new-def-for! (stmt-vdef g))) g))
    (define g2
      (cond
        [(and (gassign? g1) (ssa-name? (gassign-lhs g1)))
         (gassign (gassign-loc g1) (gassign-flags g1) (new-def-for! (gassign-lhs g1)) (gassign-code g1) (gassign-rhs1 g1) (gassign-rhs2 g1) (gassign-rhs3 g1))]
        [(and (gcall? g1) (gcall-lhs g1) (ssa-name? (gcall-lhs g1)))
         (gcall (gcall-loc g1) (gcall-flags g1) (new-def-for! (gcall-lhs g1)) (gcall-fn g1) (gcall-args g1) (gcall-fntype g1) (gcall-internal? g1) (gcall-ecf g1))]
        [else g1]))
    (stmt-set-flag g2 'stamp (fresh-stamp!)))
  ;; the defining PHI of NAME: (cons bb phi) or #f
  (define (def-phi name)
    (for*/first ([bb (in-pvector (cfg-bbs cfg))] [ph (in-pvector (phis bb))] #:when (ssa-name=? (gphi-result ph) name))
      (cons bb ph)))

  ;; ---- the registry --------------------------------------------------------
  (define (e-of pe) (cons (car pe) (cadr pe)))
  (define (type-of pe) (caddr pe))
  (define aux (make-hash))                       ; edge -> path (e->aux)
  (define (thread-path e) (hash-ref aux e #f))
  (define duplicate-blocks (make-hasheqv))

  ;; mark_threaded_blocks
  (define paths paths0)
  (define tmp (make-hasheqv))
  (define (joiner-path? p) (and (> (length p) 1) (eq? (type-of (cadr p)) 'copy-joiner)))
  (for ([p (in-list paths)] #:when (and (> (length p) 1) (not (joiner-path? p))))
    (hash-set! aux (e-of (car p)) p)
    (hash-set! tmp (cadr (car p)) #t))
  (set! paths
        (for/list ([p (in-list paths)]
                   #:unless (and (joiner-path? p) (thread-path (e-of (car p)))
                                 (not (eq? (thread-path (e-of (car p))) p))))
          (when (and (joiner-path? p) (not (thread-path (e-of (car p))))) (hash-set! aux (e-of (car p)) p))
          p))
  (set! paths
        (for/list ([p (in-list paths)]
                   #:unless (and (joiner-path? p) (eq? (thread-path (e-of (car p))) p)
                                 (for/or ([pe (in-list (cdr p))]) (thread-path (e-of pe)))
                                 (begin (hash-remove! aux (e-of (car p))) #t)))
          (when (and (joiner-path? p) (eq? (thread-path (e-of (car p))) p)) (hash-set! tmp (cadr (car p)) #t))
          p))
  (define threaded-blocks (hash-copy tmp))
  ;; a joiner's final destination must see the same PHI arguments
  (for* ([bb (in-list (sort (hash-keys tmp) <))] [p (in-list (preds bb))])
    (define e (cons p bb))
    (define path (thread-path e))
    (when (and path (joiner-path? path))
      (define final (e-of (last path)))
      (when (find-edge cfg bb (cdr final))
        (unless (for/and ([ph (in-pvector (phis (cdr final)))])
                  (equal? (phi-arg ph bb) (phi-arg ph (car final))))
          (hash-remove! aux e)))))

  ;; ---- compute_path_counts / update_profile --------------------------------
  (define (compute-path-counts rd need-correction?)
    (define e (car (rd-incoming rd)))
    (define path (thread-path e))
    (define elast (e-of (last path)))
    (define in-srcs (map car (rd-incoming rd)))
    (define-values (path-in0 nonpath0)
      (for/fold ([pin 0] [np 0]) ([p (in-list (preds (cdr e)))])
        (define ein (cons p (cdr e)))
        (define ein-path (thread-path ein))
        (cond
          [(and ein-path (memv (car (e-of (car ein-path))) in-srcs)) (values (+ pin (edge-count ein)) np)]
          [(not ein-path) (values pin (+ np (edge-count ein)))]
          [else (values pin np)])))
    (define total (count (cdr e)))
    (define path-in (if (c< total path-in0) total path-in0))
    (define onpath-scale (probability-in path-in total))
    (define-values (has-joiner path-out min-path nonpath)
      (for/fold ([hj #f] [out path-in] [mn path-in] [np nonpath0]) ([pe (in-list (cdr path))])
        (define epath (e-of pe))
        (define hj1 (or hj (eq? (type-of pe) 'copy-joiner)))
        (define cur (if (eq? (type-of pe) 'copy-joiner) (count-apply (edge-count epath) onpath-scale) (edge-count epath)))
        (define np1
          (if (and hj1 (not (equal? epath elast)))
              (for/fold ([np np]) ([s (in-list (preds (cdr epath)))])
                (define ein (cons s (cdr epath)))
                (if (or (equal? ein epath) (hash-ref duplicate-blocks s #f)) np (+ np (edge-count ein))))
              np))
        (values hj1 (if (c< cur out) cur out) (if (c< (edge-count epath) mn) (edge-count epath) mn) np1)))
    (define path-out*
      (cond
        [(and need-correction? has-joiner (c< path-out (c- (edge-count elast) nonpath)))
         (define o (c- (edge-count elast) nonpath))
         (if (and min-path (> o min-path)) min-path o)]
        [else path-out]))
    (values has-joiner path-in path-out*))

  ;; scale the other successors of E's source so that E gets probability NEW
  (define (rescale-others! e new)
    (define old (prob e))
    (cond
      [(p> old new)
       (define rev (p/ (p- MAXP old) (p- MAXP new)))
       (for ([s (in-list (succ-edges (car e)))] #:unless (equal? s e)) (set-prob! s (p/ (prob s) rev)))]
      [(and old new (< old new))
       (define scale (p/ (p- MAXP new) (p- MAXP old)))
       (for ([s (in-list (succ-edges (car e)))] #:unless (equal? s e)) (set-prob! s (count-apply* (prob s) scale)))]
      [else (void)])
    (when new (set-prob! e new)))
  (define (count-apply* a b) (if (and a b) (rdiv (* a b) MAXP) #f))   ; probability operator*
  (define (update-profile! epath edup path-in path-out)
    (when edup
      (rescale-others! edup (probability-in path-out path-in))
      (set-count! (car edup) path-in))
    (unless (eqv? path-in 0)
      (define final (c- (edge-count epath) path-out))
      (set-count! (car epath) (c- (count (car epath)) path-in))
      (rescale-others! epath (probability-in final (count (car epath))))))

  ;; ---- duplicates ------------------------------------------------------------
  ;; get_value_locus_in_path
  (define (value-in-path def path bb idx)
    (cond
      [(or (not path) (= idx 0)) def]
      [else
       (define dp (def-phi def))
       (cond
         [(not dp) def]
         [(< (loop-depth (loop-father (car dp))) (loop-depth (loop-father bb))) def]
         [else
          (or (for/first ([pe (in-list (reverse (take path idx)))] #:when (= (cadr pe) (car dp)))
                (define arg (phi-arg (cdr dp) (car pe)))
                (if (is-gimple-min-invariant st arg) arg def))
              def)])]))
  ;; copy_phi_args
  (define (copy-phi-args! bb src-e tgt-e path idx)
    (for ([ph (in-list (pvector->list (phis bb)))] [k (in-naturals)])
      (define def (phi-arg ph (car src-e)))
      (define v (if (and (ssa-name? def) (not (ssa-virtual? (gphi-result ph)))) (value-in-path def path bb idx) def))
      (set-phi-arg! bb (car tgt-e) k v)))
  (define (update-destination-phis! orig-bb new-bb path idx)
    (for ([d (in-list (succ-dests orig-bb))])
      (copy-phi-args! d (cons orig-bb d) (cons new-bb d) path idx)))
  (define (create-edge-and-update-destination-phis! rd bb idx)
    (define path (rd-path rd))
    (define elast (e-of (last path)))
    (make-edge! bb (cdr elast) EDGE-FALLTHRU MAXP)
    (copy-phi-args! (cdr elast) elast (cons bb (cdr elast)) path idx))
  (define (any-remaining-duplicated? path i)
    (for/or ([pe (in-list (drop path (add1 i)))]) (memq (type-of pe) '(copy-joiner copy-src))))
  ;; copy_phi_arg_into_existing_phi
  (define (copy-phi-arg-into-existing-phi! src-e tgt-e)
    (for ([sph (in-list (pvector->list (phis (cdr src-e))))] [k (in-naturals)])
      (set-phi-arg! (cdr tgt-e) (car tgt-e) k (phi-arg sph (car src-e)))))
  ;; create_block_for_threading (duplicate_block without an edge)
  (define (create-block-for-threading! bb)
    (define after (bb-prev cfg EXIT))
    (define-values (nb c) (create-bb cfg (pvector-empty) after))
    (set! cfg c)
    (hash-set! orig-of nb (orig bb))
    (set-phis! nb (for/pvector ([ph (in-pvector (phis bb))])
                    (gphi (gphi-loc ph) (hasheq 'arg-stamps (pvector-empty)) (gphi-var ph) (new-def-for! (gphi-result ph)) (pvector-empty))))
    (set! cfg (set-block cfg (block-set-stmts (block nb) (for/pvector ([g (in-pvector (GBlock-stmts (block bb)))] #:unless (glabel? g)) (copy-stmt g)))))
    (for ([e (in-list (pvector->list (cfg-succs cfg bb)))])
      (make-edge! nb (GEdge-dest e) (GEdge-flags e) (prob (cons bb (GEdge-dest e)))))
    (set-count! nb #f)
    (hash-set! duplicate-blocks nb #t)
    nb)

  ;; ssa_fix_duplicate_block_edges
  (define (fix-duplicate-block-edges! rd local-bb need-correction?)
    (define multi? (pair? (cdr (rd-incoming rd))))
    (define path (rd-path rd))
    (define elast (e-of (last path)))
    (define-values (has-joiner path-in path-out) (compute-path-counts rd need-correction?))
    (for/fold ([cnt 0]) ([pe (in-list (cdr path))] [i (in-naturals 1)])
      (define epath (e-of pe))
      (define dup (vector-ref (rd-dups rd) cnt))
      (define idx (if multi? 0 i))
      (case (type-of pe)
        [(copy-joiner)
         (update-destination-phis! local-bb dup path idx)
         (define e2
           (cond
             [(not (any-remaining-duplicated? path i))
              (cond
                [(not (= (cdr epath) (cdr elast)))
                 (define-values (e2 created?) (redirect-and-branch! dup (cdr epath) (cdr elast)))
                 (when created? (copy-phi-args! (cdr e2) elast e2 path idx))
                 e2]
                [else (cons dup (cdr epath))])]
             [else
              (define-values (e2 _c) (redirect-and-branch! dup (cdr epath) (vector-ref (rd-dups rd) (add1 cnt))))
              (for/first ([j (in-range (add1 i) (length path))] #:when (eq? (type-of (list-ref path j)) 'copy-src))
                (copy-phi-arg-into-existing-phi! (e-of (list-ref path (sub1 j))) e2))
              e2]))
         (update-profile! epath e2 path-in path-out)]
        [(copy-src)
         (remove-ctrl-stmt-and-useless-edges! dup #f)
         (create-edge-and-update-destination-phis! rd dup idx)
         (update-profile! epath (cons dup (cdr elast)) path-out path-out)]
        [else (update-profile! epath #f path-out path-out)])
      (if (memq (type-of pe) '(copy-joiner copy-src)) (add1 cnt) cnt))
    (void))

  ;; thread_block_1
  (define (thread-block-1! bb joiners?)
    (define rds (htab-new (length (succ-dests bb)) (lambda (rd) (rd-hash (rd-path rd)))))
    (define need-correction? #f)
    (define last-e2 #f)
    (for ([p (in-list (preds bb))])
      (define e (cons p bb))
      (define path (thread-path e))
      (when (and path
                 (not (if joiners? (eq? (type-of (cadr path)) 'copy-src) (eq? (type-of (cadr path)) 'copy-joiner))))
        (define e2 (e-of (last path)))
        (define l (loop-father bb))
        (cond
          ;; a NO_COPY_SRC block that became non-empty
          [(and (eq? (type-of (last path)) 'no-copy)
                (let ([ss (GBlock-stmts (block (car e2)))])
                  (and (not (pvector-empty? ss))
                       (let ([g (pvector-ref ss 0)]) (not (or (gcond? g) (ggoto? g) (gswitch? g)))))))
           (hash-remove! aux e)]
          ;; noloop_only: a loop header buried in the path
          [(and (not (eq? l (loop-father (car e2))))
                (or (not (loop-exit-edge? (loop-father (car e2)) e2)) (nested? l (loop-father (cdr e2)))))
           (hash-remove! aux e)]
          ;; through our own loop header: thread_through_loop_header
          [(for/or ([pe (in-list (cdr path))])
             (and (= (car pe) (loop-header l))
                  (or (not (loop-exit-edge? l e2)) (eq? (type-of (cadr path)) 'copy-joiner))))
           (void)]
          [else
           ;; lookup_redirection_data (e, INSERT)
           (htab-insert! rds (rd-hash path)
                         (lambda (rd) (paths-equal? (rd-path rd) path))
                         (lambda () (rdata path (list e) (vector #f #f)))
                         (lambda (rd) (set-rd-incoming! rd (cons e (rd-incoming rd)))))
           (when (eq? (type-of (cadr path)) 'copy-joiner)
             (if (not last-e2) (set! last-e2 e2) (unless (equal? e2 last-e2) (set! need-correction? #t))))])))
    ;; ssa_create_duplicates: the first redirection data's copy is the
    ;; template; the others copy the template (which still has its
    ;; control statement and edges) and get their edges at once
    (define template #f)
    (for ([rd (in-list (htab-entries rds))])
      (define path (rd-path rd))
      (for/first ([pe (in-list (cddr path))] #:when (memq (type-of pe) '(copy-src copy-joiner)))
        (vector-set! (rd-dups rd) 1 (create-block-for-threading! (car pe))))
      (cond
        [(not template)
         (vector-set! (rd-dups rd) 0 (create-block-for-threading! (car (cadr path))))
         (set! template (vector-ref (rd-dups rd) 0))]
        [else
         (vector-set! (rd-dups rd) 0 (create-block-for-threading! template))
         (fix-duplicate-block-edges! rd bb need-correction?)]))
    ;; ssa_fixup_template_block
    (for/first ([rd (in-list (htab-entries rds))] #:when (eqv? (vector-ref (rd-dups rd) 0) template))
      (fix-duplicate-block-edges! rd bb need-correction?))
    ;; ssa_redirect_edges
    (for* ([rd (in-list (htab-entries rds))] [e (in-list (rd-incoming rd))])
      (define dup (vector-ref (rd-dups rd) 0))
      (define saved (for/list ([ph (in-pvector (phis (cdr e)))]) (phi-arg ph (car e))))
      (define-values (e2 created?) (redirect-and-branch! (car e) (cdr e) dup))
      (unless created? (error 'thread-through-all-blocks "the incoming edge ~a merged with an edge to the duplicate" e))
      ;; flush_pending_stmts
      (for ([v (in-list saved)] [k (in-naturals)] #:when v) (set-phi-arg! dup (car e) k v))
      (hash-remove! aux e))
    (pair? (htab-entries rds)))

  ;; update_cfg: the threaded blocks in post order
  (define order (post-order cfg))
  (define threaded?
    (for/fold ([any #f]) ([bb (in-list order)] #:when (hash-ref threaded-blocks bb #f))
      (define r1 (thread-block-1! bb #f))
      (define r2 (thread-block-1! bb #t))
      (or any r1 r2)))
  (for ([l (in-list (loops-innermost-first loops))]
        #:when (and (hash-ref threaded-blocks (Loop-header l) #f)
                    (cfg-block cfg (Loop-header l))
                    (for/or ([p (in-list (preds (Loop-header l)))]) (thread-path (cons p (Loop-header l))))))
    (error 'thread-through-all-blocks "a jump thread through the header of loop ~a: thread_through_loop_header is not ported" (Loop-num l)))
  (values cfg st (reverse table) threaded?))

;; ---- hash_table (hash-table.h) for the redirection data -----------------
(define PRIMES '(7 13 31 61 127 251 509 1021 2039 4093 8191 16381 32749 65521 131071 262139 524287 1048573))
(define (higher-prime n) (for/first ([p (in-list PRIMES)] #:when (>= p n)) p))
(struct htab ([slots #:mutable] [n #:mutable] hash-of))
(define (htab-new n hash-of) (htab (make-vector (higher-prime n) #f) 0 hash-of))
;; the slot of an equal entry, or the first empty slot of the probe sequence
(define (htab-probe slots h same?)
  (define size (vector-length slots))
  (define h2 (add1 (modulo h (- size 2))))
  (let loop ([i (modulo h size)])
    (define x (vector-ref slots i))
    (if (or (not x) (same? x)) i (loop (let ([j (+ i h2)]) (if (>= j size) (- j size) j))))))
(define (htab-expand! t)
  (define old (htab-slots t))
  (define elts (htab-n t))
  (define nsize (if (> (* elts 2) (vector-length old)) (higher-prime (* elts 2)) (vector-length old)))
  (define new (make-vector nsize #f))
  (for ([x (in-vector old)] #:when x)
    (vector-set! new (htab-probe new ((htab-hash-of t) x) (lambda (y) #f)) x))
  (set-htab-slots! t new))
;; find_slot_with_hash (INSERT): MAKE a new entry or UPDATE! the equal one
(define (htab-insert! t h same? make update!)
  (when (<= (* (vector-length (htab-slots t)) 3) (* (htab-n t) 4)) (htab-expand! t))
  (define i (htab-probe (htab-slots t) h same?))
  (define x (vector-ref (htab-slots t) i))
  (cond
    [x (update! x)]
    [else (vector-set! (htab-slots t) i (make)) (set-htab-n! t (add1 (htab-n t)))]))
;; traverse: slot order
(define (htab-entries t) (for/list ([x (in-vector (htab-slots t))] #:when x) x))

;; redirection_data
(struct rdata (path [incoming #:mutable] dups))
;; redirection_data::hash: the index of the path's final destination
(define (rd-hash path) (cadr (last path)))
(define (rd-path rd) (rdata-path rd))
(define (rd-incoming rd) (rdata-incoming rd))
(define (set-rd-incoming! rd v) (set-rdata-incoming! rd v))
(define (rd-dups rd) (rdata-dups rd))
;; redirection_data::equal: the same edges and types after the first
(define (paths-equal? a b)
  (and (= (length a) (length b))
       (for/and ([x (in-list (cdr a))] [y (in-list (cdr b))]) (equal? x y))))

;; post_order_compute (no ENTRY / EXIT, unreachable blocks kept)
(define (post-order cfg)
  (define visited (make-hasheqv))
  (define out '())
  (let loop ([stack (list (cons ENTRY (map GEdge-dest (pvector->list (cfg-succs cfg ENTRY)))))])
    (unless (null? stack)
      (define top (car stack))
      (define src (car top)) (define rest (cdr top))
      (cond
        [(null? rest)
         (unless (= src ENTRY) (set! out (cons src out)))
         (loop (cdr stack))]
        [else
         (define dest (car rest))
         (define stack1 (cons (cons src (cdr rest)) (cdr stack)))
         (cond
           [(and (not (= dest EXIT)) (not (hash-ref visited dest #f)))
            (hash-set! visited dest #t)
            (define ds (map GEdge-dest (pvector->list (cfg-succs cfg dest))))
            (if (pair? ds)
                (loop (cons (cons dest ds) stack1))
                (begin (set! out (cons dest out)) (loop stack1)))]
           [else (loop stack1)])])))
  (reverse out))
