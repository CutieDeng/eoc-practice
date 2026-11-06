#lang racket/base

;; ============================================================
;; Component: Java pre-SSA CFG → SSA (M3), as a port of the SSA driver
;; ============================================================
;;
;; The algorithm lives in driver/ssa/construct.rkt; this module is the
;; kernel-Cfg adapter for the JVM lowering:
;;
;;   - variables: the "local-slot" VarIds 0..local-count-1 (stack-slot
;;     VarIds are fresh by construction and already SSA);
;;   - the entry block defines every local slot (phantom parameter
;;     names, published as 'java/ssa-param-names);
;;   - the graph handed to the driver is the exception-augmented one:
;;     every block in a try-range is an extra predecessor of the
;;     handler, so handler blocks are dominated, get PHIs and are
;;     renamed, while the CFG's terminators stay untouched;
;;   - policies: unpruned PHIs at the whole iterated dominance frontier,
;;     dominator children in block-allocation order, PHI outputs named
;;     when the renamer reaches the block (the numbering this pipeline
;;     always produced).
;;
;; Input : Cfg produced by `jvm-method->cfg` (info carries
;;         'java/max-local, 'java/param-count, optionally
;;         'java/exception-table + 'java/block-order).
;; Output: Cfg with φ nodes in block.phis; all VfInsn inputs/outputs
;;         and terminator VarIds renamed into strict SSA.
;; ============================================================

(require racket/match
         "../../../kernel/ir/cfg/types.rkt"
         "../../../component/cfg/utils/graph-ops.rkt"
         "../../../driver/ssa/construct.rkt"
         "../../../kernel/data/data.rkt")

(provide jvm-cfg->ssa)

;; ============================================================
;; Exception-augmented predecessor / successor closures
;; ============================================================
;;
;; Returns (values preds-fn succs-fn), each wrapping the base
;; terminator-derived closure with exception edges extracted from the
;; 'java/exception-table + 'java/block-order info keys.  Exception
;; predecessors/successors are appended AFTER the normal ones
;; (preserving existing phi source ordering); duplicates are
;; suppressed.
(define (make-augmented-graph cfg)
  (define base-preds (cfg-make-predecessors cfg))
  (define base-succs (cfg-make-successors cfg))
  (define-values (exn-preds exn-succs) (compute-exception-edges cfg))
  (values
    (lambda (bid) (pvector-extend-unique (base-preds bid) (idmap-ref exn-preds bid (pvector-empty))))
    (lambda (bid) (pvector-extend-unique (base-succs bid) (idmap-ref exn-succs bid (pvector-empty))))))

;; Build (values exn-preds exn-succs) maps from the exception table.
;; For each entry (start-bid end-bid handler-bid _) every block in the
;; half-open ordinal window [start-ord, end-ord) is a predecessor of
;; the handler.
(define (compute-exception-edges cfg)
  (define table (cfg-get-info cfg 'java/exception-table #f))
  (define block-order (cfg-get-info cfg 'java/block-order #f))
  (cond
    [(or (not table) (not block-order)) (values idmap-empty idmap-empty)]
    [else
     (define n (pvector-length block-order))
     (define ord-of
       (for/fold ([m idmap-empty]) ([i (in-range n)])
         (idmap-set m (pvector-ref block-order i) i)))
     (for/fold ([preds idmap-empty] [succs idmap-empty]) ([entry (in-pvector table)])
       (define start-bid (car entry))
       (define end-bid (cadr entry))
       (define handler-bid (caddr entry))
       (define start-ord (idmap-ref ord-of start-bid #f))
       (define end-ord (if end-bid (idmap-ref ord-of end-bid #f) n))
       (cond
         [(or (not start-ord) (not end-ord)) (values preds succs)]
         [else
          (for/fold ([pm preds] [sm succs]) ([i (in-range start-ord end-ord)])
            (define b (pvector-ref block-order i))
            (define cur-p (idmap-ref pm handler-bid (pvector-empty)))
            (define cur-s (idmap-ref sm b (pvector-empty)))
            (values (if (pvector-member? cur-p b) pm (idmap-set pm handler-bid (pvector-cons-right cur-p b)))
                    (if (pvector-member? cur-s handler-bid) sm (idmap-set sm b (pvector-cons-right cur-s handler-bid)))))]))]))

(define (pvector-member? pv x) (for/or ([v (in-pvector pv)]) (equal? v x)))
(define (pvector-extend-unique base extra)
  (for/fold ([acc base]) ([x (in-pvector extra)])
    (if (pvector-member? acc x) acc (pvector-cons-right acc x))))

;; ============================================================
;; The adapter
;; ============================================================
;;
;;   cfg         : the Cfg being rewritten
;;   vc          : next fresh VarId
;;   param-names : pvector of the entry definitions, in slot order
;;   phi-vars    : idmap BlockId -> pvector VarId, the slot each PHI of
;;                 the block stands for, parallel to CfgBlock-phis
;;   local-count, preds-fn, succs-fn : fixed for the run

(struct ja (cfg vc param-names phi-vars local-count preds-fn succs-fn))

(define (ja-set-cfg a c) (ja c (ja-vc a) (ja-param-names a) (ja-phi-vars a) (ja-local-count a) (ja-preds-fn a) (ja-succs-fn a)))
(define (local? a v) (and (VarId? v) (< (VarId-id v) (ja-local-count a))))

(define (fresh a v kind)
  (case kind
    [(default) (error 'jvm-cfg->ssa "use of undefined local ~a" v)]
    [else
     (define name (VarId (ja-vc a)))
     (values name
             (ja (ja-cfg a) (add1 (ja-vc a))
                 (if (eq? kind 'entry) (pvector-cons-right (ja-param-names a) name) (ja-param-names a))
                 (ja-phi-vars a) (ja-local-count a) (ja-preds-fn a) (ja-succs-fn a)))]))

(define (update-block a bid f)
  (define blk (cfg-get-block (ja-cfg a) bid))
  (if blk (ja-set-cfg a (cfg-set-block (ja-cfg a) (f blk))) a))

;; defs of the block (local slots written by its instructions), no uses:
;; the port is unpruned
(define (block-summary a bid)
  (define blk (cfg-get-block (ja-cfg a) bid))
  (define kills
    (for*/fold ([acc (pvector-empty)]) ([insn (in-pvector (if blk (CfgBlock-insns blk) (pvector-empty)))]
                                        [out (in-pvector (VfInsn-outputs insn))]
                                        #:when (and (local? a out) (not (pvector-member? acc out))))
      (pvector-cons-right acc out)))
  (values kills (pvector-empty)))

;; a PHI slot per augmented predecessor, placeholder (pred . slot)
(define (insert-phi a bid v _name)
  (define preds ((ja-preds-fn a) bid))
  (define a1 (update-block a bid (lambda (blk)
                                   (struct-copy CfgBlock blk
                                     [phis (pvector-cons-right (CfgBlock-phis blk)
                                                               (PhiInsn v (for/pvector ([p (in-pvector preds)]) (cons p v))))]))))
  (ja (ja-cfg a1) (ja-vc a1) (ja-param-names a1)
      (idmap-set (ja-phi-vars a1) bid (pvector-cons-right (idmap-ref (ja-phi-vars a1) bid (pvector-empty)) v))
      (ja-local-count a1) (ja-preds-fn a1) (ja-succs-fn a1)))

(define (phis a bid)
  (define blk (cfg-get-block (ja-cfg a) bid))
  (define vars (idmap-ref (ja-phi-vars a) bid (pvector-empty)))
  (for/pvector ([v (in-pvector vars)] [phi (in-pvector (if blk (CfgBlock-phis blk) (pvector-empty)))])
    (cons v (if (local? a (PhiInsn-output phi)) #f (PhiInsn-output phi)))))

(define (phi-index a bid v) (pvector-index-of (idmap-ref (ja-phi-vars a) bid (pvector-empty)) v))

(define (update-phi a bid v f)
  (define i (phi-index a bid v))
  (update-block a bid (lambda (blk) (struct-copy CfgBlock blk [phis (pvector-set (CfgBlock-phis blk) i (f (pvector-ref (CfgBlock-phis blk) i)))]))))

(define (set-phi-result a bid v name)
  (update-phi a bid v (lambda (phi) (PhiInsn name (PhiInsn-sources phi)))))

(define (set-phi-arg a bid v pred name)
  (update-phi a bid v (lambda (phi)
                        (PhiInsn (PhiInsn-output phi)
                                 (for/pvector ([src (in-pvector (PhiInsn-sources phi))])
                                   (if (equal? (car src) pred) (cons pred name) src))))))

;; instructions in order (inputs, then outputs), then the terminator
(define (rename-stmts a bid acc use def)
  (define blk (cfg-get-block (ja-cfg a) bid))
  (cond
    [(not blk) (values a acc)]
    [else
     (define-values (insns a1 acc1)
       (for/fold ([out (pvector-empty)] [a a] [acc acc] #:result (values out a acc)) ([insn (in-pvector (CfgBlock-insns blk))])
         (define-values (ins a1 acc1)
           (for/fold ([ins (pvector-empty)] [a a] [acc acc] #:result (values ins a acc)) ([x (in-pvector (VfInsn-inputs insn))])
             (if (local? a x)
                 (let-values ([(n a acc) (use a acc x)]) (values (pvector-cons-right ins n) a acc))
                 (values (pvector-cons-right ins x) a acc))))
         (define-values (outs a2 acc2)
           (for/fold ([outs (pvector-empty)] [a a1] [acc acc1] #:result (values outs a acc)) ([o (in-pvector (VfInsn-outputs insn))])
             (if (local? a o)
                 (let-values ([(n a acc) (def a acc o)]) (values (pvector-cons-right outs n) a acc))
                 (values (pvector-cons-right outs o) a acc))))
         (values (pvector-cons-right out (struct-copy VfInsn insn [inputs ins] [outputs outs])) a2 acc2)))
     (define-values (term a2 acc2) (rewrite-terminator a1 acc1 (CfgBlock-terminator blk) use))
     (values (update-block a2 bid (lambda (blk) (struct-copy CfgBlock blk [insns insns] [terminator term]))) acc2)]))

(define (rewrite-terminator a acc term use)
  (define (rn a acc v) (if (local? a v) (use a acc v) (values v a acc)))
  (match term
    [(Term:cond cv t e) (let-values ([(n a acc) (rn a acc cv)]) (values (Term:cond n t e) a acc))]
    [(Term:switch v cs d) (let-values ([(n a acc) (rn a acc v)]) (values (Term:switch n cs d) a acc))]
    [(Term:ret vs)
     (let-values ([(ns a acc) (for/fold ([ns (pvector-empty)] [a a] [acc acc] #:result (values ns a acc)) ([x (in-pvector vs)])
                                (let-values ([(n a acc) (rn a acc x)]) (values (pvector-cons-right ns n) a acc)))])
       (values (Term:ret ns) a acc))]
    [(Term:throw ex) (let-values ([(n a acc) (rn a acc ex)]) (values (Term:throw n) a acc))]
    [_ (values term a acc)]))

(define (finish a)
  (struct-copy Cfg (cfg-set-info (ja-cfg a) 'java/ssa-param-names (ja-param-names a)) [var-cnt (ja-vc a)]))

(define java-port
  (ssa-port (lambda (a) (Cfg-entry (ja-cfg a)))
            (lambda (a) (list->pvector (cfg-all-block-ids (ja-cfg a))))
            (lambda (a bid) ((ja-succs-fn a) bid))
            (lambda (a bid) ((ja-preds-fn a) bid))
            BlockId-id
            (lambda (a) (for/pvector ([i (in-range (ja-local-count a))]) (VarId i)))
            VarId-id
            block-summary
            (lambda (a) (for/pvector ([i (in-range (ja-local-count a))]) (VarId i)))
            fresh insert-phi phis set-phi-result set-phi-arg rename-stmts finish
            #f 'blocks 'rename))

;; ============================================================
;; Entry
;; ============================================================

(define (jvm-cfg->ssa cfg)
  (define local-count (cfg-get-info cfg 'java/max-local 0))
  (cond
    [(= local-count 0)
     ;; Nothing to SSA-ify, but still publish an empty param-names
     ;; vector so downstream passes can branch on its presence.
     (cfg-set-info cfg 'java/ssa-param-names (pvector-empty))]
    [else
     (define-values (preds-fn succs-fn) (make-augmented-graph cfg))
     (ssa-construct java-port (ja cfg (Cfg-var-cnt cfg) (pvector-empty) idmap-empty local-count preds-fn succs-fn))]))
