#lang racket/base

;; ============================================================
;; opt: pass_sink_code (gcc/tree-ssa-sink.cc), GCC 12 and GCC 15
;; ============================================================
;;
;; ((sink-pass unsplit?) st fndecl cfg) -> (values cfg st)
;;
;; loop_optimizer_init (LOOPS_NORMAL), split_edges_for_insertion, the
;; dominator and post-dominator trees, then sink_code_in_bb over the
;; blocks: GCC <= 12 walks the post-dominator tree from EXIT in
;; preorder, GCC >= 13 the inverted reverse post order.  A block that
;; dominates nothing is skipped; its statements are visited backwards:
;;
;;   statement_sink_location: assignments and const / pure calls with a
;;   single definition, no side effects, not ending the block; a dead
;;   one (no use) is removed when it has no VDEF; a store sinks only to
;;   the virtual PHI that is its one use; a statement whose uses are in
;;   several places (or a load) goes to the nearest common dominator of
;;   the use blocks (PHI uses count in the incoming block) when FROMBB
;;   dominates it, a load not past a store (GCC <= 12: the VDEFs on the
;;   VUSE's immediate uses; GCC >= 13: virtual_operand_live); a
;;   statement with one use goes towards the use's block, or the
;;   incoming block of its PHI argument.
;;
;;   select_best_block: GCC <= 12 takes the shallowest loop nest on the
;;   dominator path and otherwise the late block only when its count is
;;   below 75% (82% with memory operands) of the early block's; GCC >= 13
;;   skips blocks do_not_sink rejects (an empty latch) and keeps the
;;   candidate closest to the early block that the current best block
;;   post-dominates (or that falls through into it).
;;
;; The statement moves with gsi_move_before (its uses keep their
;; immediate-use places), "Sinking <stmt>\n from bb N to bb M" is
;; dumped (TDF_VOPS), and the VUSEs of statements a sunk store no longer
;; dominates are rewired.  UNSPLIT? asks for TODO_cleanup_cfg.
;;
;; sink_common_stores_to_bb runs first in every block: stores of one
;; location that define all arguments of the block's virtual PHI (and
;; are used by it alone) are removed and one store is put at the start
;; of the block, of the common value or of a new PHI merging the values;
;; the PHI's result moves to the store and the PHI gets a new virtual
;; name.  It asks for TODO_cleanup_cfg.
;;
;; Not ported (an error when reached): infinite loops
;; (connect_infinite_loops_to_exit), irreducible regions, abnormal edges,
;; EH.
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../../../driver/dominance/dom.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         "../cfg.rkt"
         (only-in "../ssa.rkt" stmt-vuse stmt-vdef virtual-operand?)
         (only-in "../print.rkt" print-cond-rhs print-expr stmt->string)
         "ssa-ops.rkt"
         "mfn.rkt"
         "loops.rkt"
         (only-in "alias.rkt" make-ao-ref stmt-kills-ref? ref-maybe-used-by-stmt?)
         (only-in "generic-fold.rkt" with-gs operand-equal?)
         (only-in "../gimplify.rkt" is-gimple-reg-type))

(provide sink-pass)

(define PARAM-SINK-FREQUENCY-THRESHOLD 75)
(define MAXP 134217728)

(define ((sink-pass unsplit?) st0 fndecl cfg0)
  (define st (gs-set-fndecl st0 fndecl))
  (define cfg1 (normalize-loops st (cfg-clear-messages cfg0)))
  (define-values (cfg2 st2) (split-edges-for-insertion st cfg1))
  (define-values (cfg3 st3 commoned?) (sink-code st2 cfg2))
  (if (or unsplit? commoned?) (cleanup-tree-cfg st3 cfg3 print-cond-rhs) (values cfg3 st3)))

(define (sink-code st0 cfg)
  (define st st0)
  (define major (gcc-major-version st))
  (define nodes (pvector-cons-left (pvector-cons-left (cfg-bbs cfg) EXIT) ENTRY))
  (define dom (compute-dom nodes ENTRY (lambda (i) (for/pvector ([e (in-pvector (cfg-succs cfg i))]) (GEdge-dest e)))))
  (define pdom (compute-dom nodes EXIT (lambda (i) (cfg-preds cfg i))))
  (for ([bb (in-pvector (cfg-bbs cfg))] #:unless (dom-index pdom bb))
    (error 'sink-pass "bb ~a does not reach the exit: connect_infinite_loops_to_exit is not ported" bb))
  (define-values (loops _lb) (find-loops cfg dom))
  (define back (dfs-back-edges cfg))
  (define m (gcfg->mfn cfg))
  (define messages '())
  (define (msg! fmt . args) (set! messages (cons (apply format fmt args) messages)))
  (define released '())

  ;; ---- CFG helpers ---------------------------------------------------
  (define (idom bb) (let* ([i (dom-index dom bb)] [j (and i (dom-idom dom i))]) (and j (not (= i j)) (dom-node dom j))))
  (define (dominated-by? a b)                   ; CDI_DOMINATORS: B dominates A
    (define ia (dom-index dom a)) (define ib (dom-index dom b))
    (and ia ib (dom-dominates? dom ib ia)))
  (define (post-dominated-by? a b)              ; CDI_POST_DOMINATORS: B post-dominates A
    (define ia (dom-index pdom a)) (define ib (dom-index pdom b))
    (and ia ib (dom-dominates? pdom ib ia)))
  (define (ncd a b) (dom-node dom (dom-nearest-common-dominator dom (dom-index dom a) (dom-index dom b))))
  (define (dominates-nothing? bb) (let ([i (dom-index dom bb)]) (or (not i) (pvector-empty? (dom-children dom i)))))
  (define (preds bb) (pvector->list (cfg-preds cfg bb)))
  (define (loop-father bb) (loop-of loops bb))
  (define (loop-depth bb) (let ([l (loop-father bb)]) (if l (Loop-depth l) 0)))
  (define (loop-latch bb) (let ([l (loop-father bb)]) (and l (= 1 (length (Loop-latches l))) (car (Loop-latches l)))))
  (define (empty-block? bb)
    (and (null? (mfn-phis m bb))
         (for/and ([id (in-list (mfn-stmts m bb))]) (glabel? (mfn-stmt m id)))))
  (define (count bb) (prof-count cfg bb))

  ;; ---- virtual_operand_live (GCC >= 13) ------------------------------------
  (define liveout (make-hasheqv))
  (define (virtual-phi-result bb)
    (for/first ([pid (in-list (mfn-phis m bb))] #:when (ssa-virtual? (gphi-result (mfn-stmt m pid))))
      (gphi-result (mfn-stmt m pid))))
  (define (live-in bb)
    (or (virtual-phi-result bb)
        (let/ec return
          (for/fold ([livein #f] [first? #t] #:result livein) ([p (in-list (preds bb))] #:unless (hash-ref back (cons p bb) #f))
            (define lo (live-out p))
            (cond
              [first? (values lo #f)]
              [(not (vop-equal? lo livein)) (return #f)]
              [else (values livein #f)])))))
  (define (live-out bb)
    (cond
      [(= bb ENTRY) (default-vop)]
      [(hash-ref liveout bb #f) => values]
      [else
       (define lo
         (or (for/first ([id (in-list (reverse (mfn-stmts m bb)))]
                         #:when (let ([g (mfn-stmt m id)]) (or (stmt-vdef g) (stmt-vuse g))))
               (define g (mfn-stmt m id))
               (or (stmt-vdef g) (stmt-vuse g)))
             (live-in bb)))
       (hash-set! liveout bb lo)
       lo]))
  (define (default-vop) 'default-vop)
  (define (vop-equal? a b)
    (cond [(and (ssa-name? a) (ssa-name? b)) (ssa-name=? a b)]
          [(ssa-name? a) (and (eq? b 'default-vop) (ssa-default-def? a))]
          [(ssa-name? b) (and (eq? a 'default-vop) (ssa-default-def? b))]
          [else (eq? a b)]))

  ;; ---- uses ------------------------------------------------------------------
  ;; the blocks of the uses of NAME (a PHI argument counts in its incoming block)
  (define (use-blocks name)
    (for*/list ([uid (in-list (mfn-use-ids m name))]
                [b (in-list (let ([g (mfn-stmt m uid)])
                              (if (gphi? g)
                                  (for/list ([a (in-pvector (gphi-args g))] #:when (and (ssa-name? (cdr a)) (ssa-name=? (cdr a) name))) (car a))
                                  (list (mfn-bb m uid)))))])
      b))
  (define (nearest-common-dominator-of-uses name)
    (define bbs (sort (remove-duplicates (use-blocks name)) <))
    (and (not (memv ENTRY bbs)) (pair? bbs)
         (for/fold ([c (car bbs)]) ([b (in-list (cdr bbs))]) (ncd c b))))
  (define (all-uses-same-place? name) (= 1 (length (mfn-use-ids m name))))
  ;; find_bb_for_arg
  (define (bb-for-arg phi name)
    (define hits (for/list ([a (in-pvector (gphi-args phi))] #:when (and (ssa-name? (cdr a)) (ssa-name=? (cdr a) name))) (car a)))
    (and (= 1 (length hits)) (car hits)))

  ;; ---- select_best_block ---------------------------------------------------
  (define (do-not-sink? g early best)
    (and (eqv? best (loop-latch early)) (empty-block? best)))
  (define (select-best-block early late g)
    (if (>= major 13) (select-best-block-15 early late g) (select-best-block-12 early late g)))
  (define (select-best-block-15 early late0 g)
    (define late (let loop ([l late0]) (if (and (not (= l early)) (do-not-sink? g early l)) (loop (idom l)) l)))
    (let loop ([best late] [temp late])
      (cond
        [(= temp early) best]
        [else
         (define t (idom temp))
         (cond
           [(and (not (= t early)) (do-not-sink? g early t)) (loop best t)]
           [(< (loop-depth t) (loop-depth best)) (loop t t)]
           [(> (loop-depth t) (loop-depth best)) (loop best t)]
           [(post-dominated-by? t best) (loop t t)]
           [(and (= 1 (length (preds best))) (= (car (preds best)) t)
                 (let ([e (find-edge cfg t best)])
                   (or (edge-flag? e EDGE-FALLTHRU) (>= (or (prof-prob cfg t best) 0) MAXP))))
            (loop t t)]
           [else (loop best t)])])))
  (define (select-best-block-12 early late g)
    (define best
      (let loop ([best late] [temp late])
        (cond
          [(= temp early) best]
          [else (loop (if (< (loop-depth temp) (loop-depth best)) temp best) (idom temp))])))
    (define threshold (if (or (stmt-vuse g) (stmt-vdef g)) (min 100 (+ PARAM-SINK-FREQUENCY-THRESHOLD 7)) PARAM-SINK-FREQUENCY-THRESHOLD))
    (cond
      [(< (loop-depth best) (loop-depth early)) best]
      [(and (= (loop-depth best) (loop-depth early))
            (not (let ([cb (count best)] [ce (count early)])
                   (and cb ce (>= (* cb 100) (* ce threshold))))))
       best]
      [else early]))

  ;; ---- statement_sink_location -----------------------------------------------
  ;; -> (values target zero-uses?), target = (cons 'before id) | (cons 'end bb) | #f
  (define (after-labels bb)
    (define id (for/first ([id (in-list (mfn-stmts m bb))] #:unless (glabel? (mfn-stmt m id))) id))
    (if id (cons 'before id) (cons 'end bb)))
  (define (sink-location id frombb)
    (define g (mfn-stmt m id))
    (define defs (cond [(gassign? g) (stmt-def-names g)] [(gcall? g) (stmt-def-names g)] [else '()]))
    (define cf (and (gcall? g) (call-ecf st g)))
    (cond
      [(not (or (gassign? g)
                (and (gcall? g)
                     (not (zero? (bitwise-and cf (bitwise-ior ECF-CONST ECF-PURE))))
                     (zero? (bitwise-and cf (if (>= major 13) (bitwise-ior ECF-LOOPING ECF-RETURNS-TWICE) ECF-LOOPING))))))
       (values #f #f)]
      [(not (= 1 (length defs))) (values #f #f)]
      [(or (stmt-ends-bb? g) (stmt-has-side-effects? st g)) (values #f #f)]
      [(mfn-zero-uses? m (car defs)) (values #f #t)]
      [else (values (sink-location* id g (car defs) frombb) #f)]))
  (define (sink-location* id g def frombb)
    (let/ec return
      (define use
        (cond
          [(ssa-virtual? def)
           ;; a store: the one use must be the merging virtual PHI
           (define lhs (stmt-get-lhs g))
           (define r (make-ao-ref st lhs))
           (define u
             (for/fold ([use #f]) ([uid (in-list (mfn-imm-uses m def))])
               (define ug (mfn-stmt m uid))
               (cond
                 [(or (and (not (gphi? ug)) (stmt-get-lhs ug) (equal? (stmt-get-lhs ug) lhs))
                      (and (not (gphi? ug)) (stmt-kills-ref? st ug r)))
                  (when (and (not (= uid id)) (ref-maybe-used-by-stmt? st ug r)) (return #f))
                  use]
                 [(not (gphi? ug)) (return #f)]
                 [(and use (not (= use uid))) (return #f)]
                 [else uid])))
           (unless u (return #f))
           u]
          [(or (stmt-vuse g) (not (all-uses-same-place? def)))
           (define commondom0 (nearest-common-dominator-of-uses def))
           (when (or (not commondom0) (= commondom0 frombb)) (return #f))
           (define commondom
             (cond
               [(stmt-vuse g)
                (when (and (gassign? g) (gassign-single-p st g) (eq? (tree-code (gassign-rhs1 g)) 'var_decl) (decl-flag st (gassign-rhs1 g) 'hard-register #f))
                  (return #f))
                (if (>= major 13) (load-limit-15 g frombb commondom0) (load-limit-12 g frombb commondom0))]
               [else commondom0]))
           (when (= commondom frombb) (return #f))
           (unless (dominated-by? commondom frombb) (return #f))
           (define best (select-best-block frombb commondom g))
           (when (= best frombb) (return #f))
           (return (after-labels best))]
          [else
           (define uid (car (mfn-imm-uses m def)))
           (define ug (mfn-stmt m uid))
           (cond
             [(gphi? ug) uid]
             [else
              (define ubb (mfn-bb m uid))
              (define best (select-best-block frombb ubb g))
              (when (= best frombb) (return #f))
              (return (if (and (< major 13) (= best ubb)) (cons 'before uid) (after-labels best)))])]))
      ;; a PHI use
      (define phi (mfn-stmt m use))
      (define sinkbb (bb-for-arg phi def))
      (unless sinkbb (return #f))
      (define best (select-best-block frombb sinkbb g))
      (cond
        [(= best frombb) #f]
        [(and (>= major 13) (stmt-vdef g) (not (= best sinkbb)) (not (post-dominated-by? best sinkbb))) #f]
        [(and (< major 13) (eqv? best (loop-latch frombb)) (empty-block? best)) #f]
        [else (after-labels best)])))
  ;; GCC >= 13: walk up while the live virtual operand differs from the load's
  (define (load-limit-15 g frombb commondom)
    (let loop ([c commondom])
      (if (and (not (= c frombb)) (not (vop-equal? (live-in c) (stmt-vuse g))))
          (loop (idom c))
          c)))
  ;; GCC <= 12: the VDEFs among the VUSE's immediate uses on the path
  (define (load-limit-12 g frombb commondom0)
    (define vuse (stmt-vuse g))
    (for/fold ([commondom commondom0]) ([uid (in-list (mfn-imm-uses m vuse))])
      (define ug (mfn-stmt m uid))
      (define ubb (mfn-bb m uid))
      (define bb
        (cond
          [(gphi? ug)
           (cond
             [(ssa-name=? (gphi-result ug) vuse) #f]
             [(and (not (= commondom ubb)) (not (dominated-by? commondom ubb)) (post-dominated-by? commondom ubb)) #f]
             [else
              (define ks (for/list ([a (in-pvector (gphi-args ug))] #:when (and (ssa-name? (cdr a)) (ssa-name=? (cdr a) vuse))) (car a)))
              (when (> (length ks) 1) (error 'sink-pass "a VUSE used twice by one virtual PHI is not ported"))
              (car ks)])]
          [(not (stmt-vdef ug)) #f]
          [else ubb]))
      (cond
        [(or (not bb) (not (dominated-by? bb frombb))) commondom]
        [else (ncd bb commondom)])))

  ;; ---- sink_common_stores_to_bb ---------------------------------------------
  ;; stores to the same location ending every predecessor, each used only
  ;; by the virtual PHI, become one store at the start of BB (a PHI merges
  ;; differing values); returns #t when it changed something (TODO_cleanup_cfg)
  (define (operand-eq? a b) (let-values ([(r _s) (with-gs st (lambda () (operand-equal? a b)))]) r))
  (define (clobber? g) (and (gassign? g) (tree-clobber-p (gassign-rhs1 g))))
  (define (virtual-phi-id bb)
    (for/first ([pid (in-list (mfn-phis m bb))] #:when (ssa-virtual? (gphi-result (mfn-stmt m pid)))) pid))
  (define (sink-common-stores! bb)
    (cond
      [(not (and (> (length (preds bb)) 1) (virtual-phi-id bb))) #f]
      [else
       (let loop ([todo #f])
         (define pid (virtual-phi-id bb))
         (define phi (mfn-stmt m pid))
         ;; the common stores defining all PHI arguments: (values first-id args)
         (define-values (first-id args)
           (let/ec return
             (for/fold ([first #f] [acc '()] #:result (values first (reverse acc))) ([a (in-pvector (gphi-args phi))])
               (define arg (cdr a))
               (define did (and (ssa-name? arg) (mfn-def m arg)))
               (define def (and did (mfn-stmt m did)))
               (cond
                 [(not (and def (gassign? def))) (return #f '())]
                 [(for/or ([uid (in-list (mfn-use-ids m arg))]) (not (= uid pid))) (return #f '())]
                 [(not first) (values did (cons arg acc))]
                 [(or (not (operand-eq? (gassign-lhs (mfn-stmt m first)) (gassign-lhs def)))
                      (not (eq? (clobber? (mfn-stmt m first)) (clobber? def))))
                  (return #f '())]
                 [else (values first (cons arg acc))]))))
         (cond
           [(not first-id) todo]
           [else
            (define first (mfn-stmt m first-id))
            (define defs (for/list ([v (in-list args)]) (mfn-stmt m (mfn-def m v))))
            (define allsame (or (clobber? first) (for/and ([d (in-list (cdr defs))]) (operand-eq? (gassign-rhs1 first) (gassign-rhs1 d)))))
            (define type (tree-type (gassign-lhs first)))
            (cond
              [(and (not allsame) (not (is-gimple-reg-type st type))) todo]
              [else
               ;; a PHI merging the stored values
               (define from
                 (cond
                   [allsame (gassign-rhs1 first)]
                   [else
                    (define-values (nm st1) (make-ssa-name st type)) (set! st st1)
                    (mfn-add-phi! m bb (gphi #f (hasheq) #f nm
                                             (for/pvector ([p (in-list (preds bb))] [d (in-list defs)]) (cons p (gassign-rhs1 d)))))
                    nm]))
               ;; remove all the stores (each once)
               (define old-result (gphi-result phi))
               (for ([v (in-list (remove-duplicates args ssa-name=?))])
                 (define did (mfn-def m v))
                 (define g (mfn-stmt m did))
                 (mfn-unlink-vdef! m did)
                 (mfn-remove! m did)
                 (set! released (append released (map ssa-name-version (stmt-def-names g)))))
               ;; the PHI gets a new virtual name, the store its old one
               (define-values (new-vop st2) (make-ssa-name st (ssa-name-var old-result))) (set! st st2)
               (define phi* (mfn-stmt m pid))
               (mfn-replace! m pid (gphi (gphi-loc phi*) (gphi-flags phi*) (gphi-var phi*) new-vop (gphi-args phi*)))
               (define store (stmt-set-flag (stmt-set-flag (gassign (gassign-loc first) (gassign-flags first) (gassign-lhs first)
                                                                    (gassign-code first) from #f #f)
                                                           'vdef old-result)
                                            'vuse new-vop))
               (define target (for/first ([id (in-list (mfn-stmts m bb))] #:unless (glabel? (mfn-stmt m id))) id))
               (if target (mfn-insert-before! m target store) (mfn-append! m bb store))
               (loop #t)])]))]))

  ;; ---- sink_code_in_bb ---------------------------------------------------------
  (define cleanup? #f)
  (define (sink-code-in-bb! bb)
    (when (sink-common-stores! bb) (set! cleanup? #t))
    (unless (dominates-nothing? bb)
      (for ([id (in-list (reverse (mfn-stmts m bb)))])
        (define g (mfn-stmt m id))
        (define-values (target zero?) (sink-location id bb))
        (cond
          [(not target)
           (when (and zero? (not (stmt-vdef g)))
             (define defs (stmt-def-names g))
             (mfn-remove! m id)
             (set! released (append released (map ssa-name-version defs))))]
          [else
           (define tbb (if (eq? (car target) 'end) (cdr target) (mfn-bb m (cdr target))))
           (msg! "Sinking ~a~a" (vops-prefix g) (stmt->string st (mfn-cfg m) bb g))
           (msg! " from bb ~a to bb ~a" bb tbb)
           ;; the VUSEs of statements the moved store no longer dominates
           (when (stmt-vdef g)
             (for ([uid (in-list (mfn-imm-uses m (stmt-vdef g)))])
               (define ug (mfn-stmt m uid))
               (unless (or (gphi? ug) (and (>= major 13) (dominated-by? (mfn-bb m uid) tbb)))
                 (mfn-replace! m uid (stmt-set-flag ug 'vuse (stmt-vuse g))))))
           (if (eq? (car target) 'end) (mfn-move-to-end! m id tbb) (mfn-move-before! m id (cdr target)))]))))
  (define (vops-prefix g)
    (cond
      [(stmt-vdef g) (format "# ~a = VDEF <~a>\n" (print-expr st (stmt-vdef g)) (print-expr st (stmt-vuse g)))]
      [(stmt-vuse g) (format "# VUSE <~a>\n" (print-expr st (stmt-vuse g)))]
      [else ""]))

  ;; the block order
  (define order
    (if (>= major 13)
        (for/list ([b (in-pvector (dom-rpo pdom))] #:unless (memv b (list ENTRY EXIT))) b)
        (let walk ([i 0])
          (define b (dom-node pdom i))
          (append (if (memv b (list ENTRY EXIT)) '() (list b))
                  (append* (for/list ([k (in-pvector (dom-children pdom i))]) (walk k)))))))
  (for ([bb (in-list order)]) (sink-code-in-bb! bb))
  (define cfg1 (for/fold ([c (mfn->gcfg m)]) ([l (in-list (reverse messages))]) (cfg-add-message c l)))
  (values cfg1 (for/fold ([st st]) ([v (in-list released)]) (ssa-queue-release st v)) cleanup?))
