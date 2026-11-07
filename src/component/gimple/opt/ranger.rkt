#lang racket/base

;; ============================================================
;; opt: the ranger (gcc/gimple-range.cc gimple_ranger,
;; gimple-range-cache.cc ranger_cache, gimple-range-gori.cc,
;; gimple-range-fold.cc fold_using_range, gimple-range-edge.cc), GCC 15,
;; over an mfn
;; ============================================================
;;
;;   (make-ranger st fndecl m dom) -> R
;;   (rg-range-of-expr R name sid)     range_of_expr at statement SID
;;   (rg-range-of-stmt R sid [name])   range_of_stmt
;;   (rg-range-on-edge R src dest name)
;;   (rg-export-global-ranges R st)    -> (values st lines)
;;       export_global_ranges: every name with a cached global range
;;       that is not VARYING goes through set_range_info; LINES are the
;;       "Global Exported" lines of the names whose range info changed
;;
;; Ported: range_of_expr / range_on_entry / range_on_exit /
;; range_on_edge / range_of_stmt with prefill_stmt_dependencies; the
;; cache's global table with the temporal staleness check
;; (current_p against depend1 / depend2), the on-entry cache filled by
;; range_from_dom (fill_block_cache with dominator information always
;; takes that path), resolve_dom, propagate_updated_value /
;; propagate_cache; GORI: exports / imports per block, def chains,
;; may_recompute_p, outgoing edge ranges of conditions and switches,
;; compute_operand_range through range-op's op1_range / op2_range and
;; the logical && / || combination; fold_using_range for range-op
;; statements, PHIs, calls, addresses, the nonnegative fallback for a
;; VARYING integral result.
;;
;; Not ported: the relation oracle (every relation is VARYING), the
;; inferred-range oracle (non-null pointers after a dereference,
;; nonzero divisors), SCEV and PHI analysis (not initialized by the
;; passes that use the ranger so far), floating point ranges (frange:
;; a float name has no range), prange (pointers are unsigned
;; bit-ranges, see range-op.rkt).
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
         "ssa-ops.rkt"
         "mfn.rkt"
         "irange.rkt"
         "range-op.rkt"
         "range-info.rkt"
         (only-in "fold.rkt" type-precision* type-unsigned?*))

(provide make-ranger rg-range-of-expr rg-range-of-stmt rg-range-on-edge rg-range-on-entry
         rg-export-global-ranges range-ssa? stmt-range-type rg-gori-exports rg-gori-imports
         (struct-out R))

(define ENTRY-BB 0)
(define PARAM-RECOMPUTE-DEPTH 5)
(define PARAM-LOGICAL-DEPTH 6)
(define PARAM-SWITCH-LIMIT 50)
(define PARAM-NONNEG-DEPTH 3)

;; per-name dependency record (range_def_chain::rdc)
(struct rdc ([ssa1 #:mutable] [ssa2 #:mutable] [bm #:mutable] [imports #:mutable]))

(struct R (st fndecl m dom
           names                 ; version -> name
           globals               ; version -> irange          (m_globals)
           stamps                ; version -> int             (temporal_cache)
           [time #:mutable]
           on-entry              ; (version . bb) -> irange
           chains                ; version -> rdc
           outgoing incoming     ; bb -> hasheqv version -> #t
           maybe-variant         ; version -> #t
           switch-edges          ; (src . dest) -> irange
           [stack #:mutable]     ; m_stmt_list
           [update #:mutable]))  ; LIFO update list of blocks

(define (st-of r) (R-st r))

;; ------------------------------------------------------------
;; names, types, statements
;; ------------------------------------------------------------

(define (supported-type? st ty) (and ty (or (integral-type-p st ty) (pointer-type-p st ty))))
;; gimple_range_ssa_p
(define (range-ssa? st t)
  (and (ssa-name? t) (not (virtual-operand? (ssa-name-var t))) (not (ssa-virtual? t))
       (supported-type? st (tree-type t))))
(define (ver t) (ssa-name-version t))
(define (name-of r v) (hash-ref (R-names r) v))

(define (def-id r name) (mfn-def (R-m r) name))
(define (def-bb r name) (let ([id (def-id r name)]) (and id (mfn-bb (R-m r) id))))
(define (stmt r id) (mfn-stmt (R-m r) id))
(define (bb-of r id) (mfn-bb (R-m r) id))
(define (cfg r) (mfn-cfg (R-m r)))
(define (preds r bb) (pvector->list (cfg-preds (cfg r) bb)))
(define (succs r bb) (for/list ([e (in-pvector (cfg-succs (cfg r) bb))]) (GEdge-dest e)))
(define (single-pred? r bb) (= 1 (pvector-length (cfg-preds (cfg r) bb))))

(define (idom r bb)
  (define d (R-dom r))
  (define i (dom-index d bb))
  (define j (and i (dom-idom d i)))
  (and j (not (= j i)) (dom-node d j)))
(define (dominated-by? r a b)
  (define d (R-dom r))
  (define ia (dom-index d a)) (define ib (dom-index d b))
  (and ia ib (dom-dominates? d ib ia)))

;; gimple_get_lhs as an SSA name or #f
(define (stmt-lhs g)
  (cond [(gassign? g) (gassign-lhs g)]
        [(gcall? g) (gcall-lhs g)]
        [(gphi? g) (gphi-result g)]
        [else #f]))

;; gimple_range_type
(define (stmt-range-type st g)
  (cond
    [(gcond? g) (boolean-type st)]
    [else (let ([l (stmt-lhs g)]) (and l (tree-type l)))]))

;; gimple_range_op_handler: (values code op1 op2) or (values #f #f #f)
(define (handler st g)
  (cond
    [(gcond? g)
     (if (and (ro-supported? (gcond-code g)) (supported-type? st (tree-type (gcond-lhs g))))
         (values (gcond-code g) (gcond-lhs g) (gcond-rhs g))
         (values #f #f #f))]
    [(gassign? g)
     (define code (gassign-code g))
     (define op1 (gassign-rhs1 g))
     (cond
       [(and (ro-supported? code) op1 (supported-type? st (tree-type op1)))
        (values code op1 (gassign-rhs2 g))]
       [else (values #f #f #f)])]
    [else (values #f #f #f)]))
(define (handler? st g) (let-values ([(c a b) (handler st g)]) (and c #t)))

;; get_tree_range for a non-SSA operand (or an unsupported SSA name)
(define (tree-range st t)
  (define ty (tree-type t))
  (cond
    [(not (supported-type? st ty)) #f]
    [else
     (define ti (tinfo st ty))
     (case (tree-code t)
       [(integer_cst) (ir-const ti (from-pattern (integer-cst-value t) ti))]
       [(addr_expr) (ir-nonzero ti)]
       [else (ir-varying ti)])]))

;; ------------------------------------------------------------
;; construction
;; ------------------------------------------------------------

(define (make-ranger st fndecl m dom)
  (define names (make-hasheqv))
  (define c (mfn-cfg m))
  (define (note! t) (when (ssa-name? t) (hash-set! names (ver t) t)))
  (for ([bb (in-pvector (cfg-bbs c))])
    (for ([id (in-sequences (in-list (mfn-phis m bb)) (in-list (mfn-stmts m bb)))])
      (define g (mfn-stmt m id))
      (for-each note! (stmt-def-names g))
      (for-each note! (stmt-use-names g))))
  (define r (R st fndecl m dom names (make-hasheqv) (make-hasheqv) 0 (make-hash) (make-hasheqv)
               (make-hasheqv) (make-hasheqv) (make-hasheqv) (make-hash) '() '()))
  ;; ranger_cache: the exports of every block, in index order
  (for ([bb (in-range 0 (add1 (for/fold ([mx 1]) ([b (in-pvector (cfg-bbs c))]) (max mx b))))])
    (when (or (= bb ENTRY-BB) (= bb 1) (cfg-block c bb)) (exports r bb)))
  r)

;; ------------------------------------------------------------
;; GORI: def chains, exports, imports
;; ------------------------------------------------------------

(define (chain r name)
  (hash-ref! (R-chains r) (ver name) (lambda () (rdc 0 0 #f #f))))
(define (has-def-chain? r name) (not (= 0 (rdc-ssa1 (chain r name)))))
(define (set-import! r d imp b)
  (unless (and (not imp) (not b))
    (unless (rdc-imports d) (set-rdc-imports! d (make-hasheqv)))
    (if imp
        (hash-set! (rdc-imports d) (ver imp) #t)
        (for ([k (in-hash-keys b)]) (hash-set! (rdc-imports d) k #t)))))

;; gimple_range_ssa_names: the range SSA operands of a range-op statement
(define (range-ssa-names r g)
  (define st (st-of r))
  (define-values (code a b) (handler st g))
  (if code (filter (lambda (x) (range-ssa? st x)) (list a b)) '()))

(define logical-depth 0)
(define (get-def-chain r name)
  (define d (chain r name))
  (cond
    [(and (has-def-chain? r name) (rdc-bm d)) (rdc-bm d)]
    [(ssa-default-def? name) (set-import! r d name #f) #f]
    [else
     (define id (def-id r name))
     (define ops (if id (range-ssa-names r (stmt r id)) '()))
     (cond
       [(null? ops) (set-import! r d name #f) #f]
       [(= logical-depth PARAM-LOGICAL-DEPTH) #f]
       [else
        (when (> (length ops) 1) (set! logical-depth (add1 logical-depth)))
        (for ([op (in-list ops)]) (register-dependency! r name op (bb-of r id)))
        (when (> (length ops) 1) (set! logical-depth (sub1 logical-depth)))
        (rdc-bm (chain r name))])]))

(define (register-dependency! r name dep [bb #f])
  (when (range-ssa? (st-of r) dep)
    (define d (chain r name))
    (define dv (ver dep))
    (cond
      [(= 0 (rdc-ssa1 d)) (set-rdc-ssa1! d dv)]
      [(and (= 0 (rdc-ssa2 d)) (not (= (rdc-ssa1 d) dv))) (set-rdc-ssa2! d dv)])
    (when bb
      (unless (rdc-bm d) (set-rdc-bm! d (make-hasheqv)))
      (hash-set! (rdc-bm d) dv #t)
      (define did (def-id r dep))
      (cond
        [(and did (= (bb-of r did) bb) (not (mfn-phi? (R-m r) did)))
         (define b (get-def-chain r dep))
         (when b (for ([k (in-hash-keys b)]) (hash-set! (rdc-bm (chain r name)) k #t)))
         (set-import! r (chain r name) #f (or (get-imports r dep) (hasheqv)))]
        [else (set-import! r d dep #f)]))))

(define (get-imports r name)
  (unless (has-def-chain? r name) (get-def-chain r name))
  (rdc-imports (chain r name)))
(define (in-chain? r name def)
  (define c (get-def-chain r def))
  (and c (hash-ref c (ver name) #f)))
(define (chain-import? r name imp)
  (define b (get-imports r name))
  (and b (hash-ref b (ver imp) #f)))
(define (depend1 r name) (let ([v (rdc-ssa1 (chain r name))]) (and (not (= v 0)) (name-of r v))))
(define (depend2 r name) (let ([v (rdc-ssa2 (chain r name))]) (and (not (= v 0)) (name-of r v))))

;; gimple_outgoing_range_stmt_p -> statement id or #f
(define (outgoing-stmt r bb)
  (define id (mfn-last (R-m r) bb))
  (define g (and id (stmt r id)))
  (cond
    [(and g (gcond? g) (handler? (st-of r) g)) id]
    [(and g (gswitch? g)) id]
    [else #f]))

(define (exports r bb)
  (or (hash-ref (R-outgoing r) bb #f)
      (let ([out (make-hasheqv)] [in (make-hasheqv)])
        (hash-set! (R-outgoing r) bb out)
        (hash-set! (R-incoming r) bb in)
        (unless (or (= bb ENTRY-BB) (= bb 1) (= 1 (length (succs r bb))))
          (define gid (outgoing-stmt r bb))
          (define g (and gid (stmt r gid)))
          (define (add! name)
            (when (and name (range-ssa? (st-of r) name))
              (define c (get-def-chain r name))
              (when c (for ([k (in-hash-keys c)]) (hash-set! out k #t)))
              (define imp (get-imports r name))
              (when imp (for ([k (in-hash-keys imp)]) (hash-set! in k #t)))
              (unless (eqv? (def-bb r name) bb) (hash-set! in (ver name) #t))
              (hash-set! out (ver name) #t)))
          (cond
            [(and g (gcond? g)) (add! (gcond-lhs g)) (add! (gcond-rhs g))]
            [(and g (gswitch? g) (<= (length (succs r bb)) PARAM-SWITCH-LIMIT)) (add! (gswitch-index g))])
          (for ([k (in-hash-keys out)]) (hash-set! (R-maybe-variant r) k #t)))
        out)))
(define (rg-gori-exports r bb) (exports r bb))
(define (rg-gori-imports r bb) (exports r bb) (hash-ref (R-incoming r) bb))

(define (export? r name [bb #f])
  (if bb (hash-ref (exports r bb) (ver name) #f) (hash-ref (R-maybe-variant r) (ver name) #f)))
(define (set-range-invariant! r name inv)
  (if inv (hash-remove! (R-maybe-variant r) (ver name)) (hash-set! (R-maybe-variant r) (ver name) #t)))

(define (may-recompute? r name bb [depth -1])
  (define d1 (depend1 r name)) (define d2 (depend2 r name))
  (define id (def-id r name))
  (cond
    [(not d1) #f]
    [(or (not id) (mfn-phi? (R-m r) id) (stmt-has-side-effects? (st-of r) (stmt r id))) #f]
    [(not d2)
     (define dp (if (= depth -1) PARAM-RECOMPUTE-DEPTH depth))
     (define res (export? r d1 bb))
     (if (or res (<= dp 1)) res (may-recompute? r d1 bb (sub1 dp)))]
    [else (or (export? r d1 bb) (export? r d2 bb))]))

(define (has-edge-range? r name [bb #f])
  (or (export? r name bb) (may-recompute? r name bb)))

;; ------------------------------------------------------------
;; outgoing edge ranges (gimple_outgoing_range)
;; ------------------------------------------------------------

;; -> (values stmt-id lhs-range) or (values #f #f)
(define (outgoing-edge-range r src dest)
  (define st (st-of r))
  (cond
    [(= 1 (length (succs r src))) (values #f #f)]
    [else
     (define gid (outgoing-stmt r src))
     (define g (and gid (stmt r gid)))
     (cond
       [(not g) (values #f #f)]
       [(gcond? g)
        (define e (find-edge (cfg r) src dest))
        (define bt (tinfo st (boolean-type st)))
        (values gid (if (edge-flag? e EDGE-TRUE-VALUE) (range-true bt) (range-false bt)))]
       [(> (length (succs r src)) PARAM-SWITCH-LIMIT) (values #f #f)]
       [else
        (unless (hash-has-key? (R-switch-edges r) (cons src dest)) (calc-switch-ranges! r src g))
        (values gid (hash-ref (R-switch-edges r) (cons src dest)))])]))

(define (calc-switch-ranges! r src g)
  (define st (st-of r))
  (define c (cfg r))
  (define idx (gswitch-index g))
  (define t (tinfo st (tree-type idx)))
  (define (label-bb cs) (label-to-block c (tree-operand cs 2)))
  (define default-bb (label-bb (gswitch-default-case g)))
  (define default-range
    (for/fold ([dr (ir-varying t)]) ([cs (in-pvector (gswitch-cases g))])
      (define dest (label-bb cs))
      (cond
        [(= dest default-bb) dr]
        [else
         (define lo (from-pattern (integer-cst-value (tree-operand cs 0)) t))
         (define hi (if (tree-operand cs 1) (from-pattern (integer-cst-value (tree-operand cs 1)) t) lo))
         (define case-range (ir-set t lo hi))
         (define key (cons src dest))
         (hash-set! (R-switch-edges r) key
                    (let ([old (hash-ref (R-switch-edges r) key #f)])
                      (if old (let-values ([(u _c) (ir-union case-range old)]) u) case-range)))
         (if (ir-varying? case-range)
             (ir-undefined t)
             (let-values ([(x _c) (ir-intersect dr (ir-invert case-range))]) x))])))
  (hash-set! (R-switch-edges r) (cons src default-bb) default-range))

;; ------------------------------------------------------------
;; GORI: operand ranges from an outgoing edge
;; ------------------------------------------------------------

;; a range source: get-operand (expr) and get-phi-operand (expr src dest)
(struct src-q (operand phi-operand dep))

(define (logical-stmt? st g)
  (and (gassign? g)
       (or (memq (gassign-code g) '(truth_and_expr truth_or_expr))
           (and (memq (gassign-code g) '(bit_and_expr bit_ior_expr))
                (boolean-type-p st (tree-type (gassign-rhs1 g)))))))

(define (op-type x) (tree-type x))

(define (compute-operand-range r sid lhs name src)
  (define st (st-of r))
  (define g (stmt r sid))
  (cond
    [(ir-undefined? lhs) (ir-undefined (tinfo st (tree-type name)))]
    [(gswitch? g)
     (define op1 (gswitch-index g))
     (cond
       [(ssa-name=? op1 name) lhs]
       [(and (range-ssa? st op1) (in-chain? r name op1)) (compute-operand-range r (def-id r op1) lhs name src)]
       [else #f])]
    [else
     (define-values (code a b) (handler st g))
     (cond
       [(not code) #f]
       [else
        (define op1 (and (range-ssa? st a) a)) (define op2 (and b (range-ssa? st b) b))
        (cond
          [(and op1 (ssa-name=? op1 name)) (compute-operand1-range r g code a b lhs src)]
          [(and op2 (ssa-name=? op2 name)) (compute-operand2-range r g code a b lhs src)]
          [else
           (define in1 (and op1 (in-chain? r name op1)))
           (define in2 (and op2 (in-chain? r name op2)))
           (define-values (c1 c2)
             (cond
               [(and in1 in2)
                (cond [(or (in-chain? r op1 op2) (ssa-name=? op1 op2)) (values #f #t)]
                      [(in-chain? r op2 op1) (values #t #f)]
                      [else (values #t #t)])]
               [else (values in1 in2)]))
           (cond
             [(and (not c1) (not c2)) #f]
             [(ir-varying? lhs) #f]                      ; no relation to use
             [(logical-stmt? st g)
              (define-values (t1 f1) (logical-operands r g code lhs name src op1 c1))
              (define-values (t2 f2) (logical-operands r g code lhs name src op2 c2))
              (logical-combine r code lhs t1 f1 t2 f2)]
             [(and c1 c2)
              (define vr (compute-operand2-range r g code a b lhs src))
              (define x (and vr (compute-operand-range r (def-id r op2) vr name src)))
              (define vr1 (and x (compute-operand1-range r g code a b lhs src)))
              (define y (and vr1 (compute-operand-range r (def-id r op1) vr1 name src)))
              (and y (let-values ([(z _c) (ir-intersect x y)]) z))]
             [c1
              (define vr (compute-operand1-range r g code a b lhs src))
              (and vr (compute-operand-range r (def-id r op1) vr name src))]
             [else
              (define vr (compute-operand2-range r g code a b lhs src))
              (and vr (compute-operand-range r (def-id r op2) vr name src))])])])]))

(define (range-is-either-true-or-false? x)
  (and (not (ir-undefined? x)) (or (ir-singleton x) (not (ir-contains? x 0)))))

(define (logical-operands r g code lhs name src op in-chain)
  (define st (st-of r))
  (define src-sid (and op (range-ssa? st op) (def-id r op)))
  (define bt (tinfo st (boolean-type st)))
  (define (query) ((src-q-operand src) name))
  (cond
    [(or (not in-chain) (not src-sid) (chain-import? r (stmt-lhs g) op))
     (define x (query)) (values x x)]
    [(and (memq code '(bit_ior_expr truth_or_expr)) (ir-zero? lhs))
     (define x (or (compute-operand-range r src-sid (range-false bt) name src) (query))) (values x x)]
    [(and (memq code '(bit_and_expr truth_and_expr)) (ir-equal? lhs (range-true bt)))
     (define x (or (compute-operand-range r src-sid (range-true bt) name src) (query))) (values x x)]
    [else
     (values (or (compute-operand-range r src-sid (range-true bt) name src) (query))
             (or (compute-operand-range r src-sid (range-false bt) name src) (query)))]))

(define (logical-combine r code lhs t1 f1 t2 f2)
  (define st (st-of r))
  (define bt (tinfo st (boolean-type st)))
  (define (isect a b) (let-values ([(x _c) (ir-intersect a b)]) x))
  (define (unite a b) (let-values ([(x _c) (ir-union a b)]) x))
  (cond
    [(and (ir-varying? t1) (ir-varying? f1) (ir-varying? t2) (ir-varying? f2)) #f]
    [(not (range-is-either-true-or-false? lhs))
     (define a (logical-combine r code (range-false bt) t1 f1 t2 f2))
     (define b (and a (logical-combine r code (range-true bt) t1 f1 t2 f2)))
     (and b (unite b a))]
    [(memq code '(bit_and_expr truth_and_expr))
     (if (not (ir-zero? lhs))
         (isect t1 t2)
         (unite (unite (isect f1 f2) (isect t1 f2)) (isect f1 t2)))]
    [else
     (if (ir-zero? lhs)
         (isect f1 f2)
         (unite (unite (isect t1 t2) (isect t1 f2)) (isect f1 t2)))]))

(define (compute-operand1-range r g code a b lhs src)
  (define st (st-of r))
  (define op1-range ((src-q-operand src) a))
  (define res
    (cond
      [b (define op2-range ((src-q-operand src) b))
         (if (ir-undefined? op2-range)
             (and (not (gassign-unary? g)) (ro-op1 st code (op-type a) lhs (ir-varying (tinfo st (op-type b)))))
             (ro-op1 st code (op-type a) lhs op2-range))]
      [else (if (ir-undefined? op1-range) #f (ro-op1 st code (op-type a) lhs op1-range))]))
  (and res (let-values ([(x _c) (ir-intersect res op1-range)]) x)))

(define (compute-operand2-range r g code a b lhs src)
  (define st (st-of r))
  (define op1-range ((src-q-operand src) a))
  (define op2-range ((src-q-operand src) b))
  (define res
    (if (ir-undefined? op1-range)
        (ro-op2 st code (op-type b) lhs (ir-varying (tinfo st (op-type a))))
        (ro-op2 st code (op-type b) lhs op1-range)))
  (and res (let-values ([(x _c) (ir-intersect res op2-range)]) x)))

(define (gassign-unary? g) (and (gassign? g) (not (gassign-rhs2 g))))

;; gori_compute::edge_range_p with the range query Q -> irange or #f
(define (gori-edge-range r src dest name q)
  (define-values (gid lhs) (outgoing-edge-range r src dest))
  (cond
    [(not gid) #f]
    [(export? r name src)
     (define x (compute-operand-range r gid lhs name (stmt-src r gid q)))
     (and x (if (equal? (ti-type (irange-ti x)) (tree-type name)) x (ro-fold (st-of r) 'nop_expr (tree-type name) x (ir-varying (tinfo (st-of r) (tree-type name))))))]
    [(may-recompute? r name src)
     (fold-stmt r (def-id r name) (edge-src r src dest q) #f)]
    [else #f]))

;; ------------------------------------------------------------
;; range queries: Q = (vector range-of-expr range-on-edge)
;; ------------------------------------------------------------

(define (q-expr q) (vector-ref q 0))
(define (q-edge q) (vector-ref q 1))

;; fur_stmt: operands at statement SID through Q
(define (stmt-src r sid q)
  (src-q (lambda (x) ((q-expr q) x sid))
         (lambda (x s d) ((q-edge q) s d x))
         #f))
;; fur_edge: operands on an edge
(define (edge-src r s d q)
  (src-q (lambda (x) ((q-edge q) s d x))
         (lambda (x s2 d2) ((q-edge q) s2 d2 x))
         #f))
(define (global-q r)
  (vector (lambda (x sid) (global-range-of r x))
          (lambda (s d x) (global-range-of r x))))
(define (global-range-of r x)
  (define st (st-of r))
  (cond
    [(range-ssa? st x) (gimple-range-global st (R-fndecl r) x)]
    [else (tree-range st x)]))

;; ------------------------------------------------------------
;; fold_using_range
;; ------------------------------------------------------------

;; -> irange (or #f when the statement has no supported range)
(define (fold-stmt r sid src name0)
  (define st (st-of r))
  (define g (stmt r sid))
  (define name (or name0 (let ([l (stmt-lhs g)]) (and l (ssa-name? l) l))))
  (define res
    (cond
      [(and (gassign? g) (eq? (gassign-code g) 'addr_expr)) (range-of-address r g src)]
      [(handler? st g) (range-of-range-op r g src)]
      [(gphi? g) (range-of-phi r sid g src)]
      [(gcall? g) (range-of-call r g)]
      [else #f]))
  (cond
    [(not res)
     (and name (range-ssa? st name) (gimple-range-global st (R-fndecl r) name))]
    [else
     (define t (irange-ti res))
     (define res1
       (if (and (ir-varying? res) (integral-type-p st (ti-type t)) (stmt-nonnegative? r g 0))
           (ir-nonnegative t)
           res))
     (cond
       [(ir-undefined? res1) res1]
       [(and name (not (equal? (tree-type name) (ti-type t))))
        (if (supported-type? st (tree-type name))
            (ro-fold st 'nop_expr (tree-type name) res1 (ir-varying (tinfo st (tree-type name))))
            res1)]
       [else res1])]))

(define (range-of-range-op r g src)
  (define st (st-of r))
  (define-values (code a b) (handler st g))
  (define type (stmt-range-type st g))
  (define lhs (stmt-lhs g))
  (define range1 ((src-q-operand src) a))
  (cond
    [(not b)
     (define x (ro-fold st code type range1 (ir-varying (tinfo st type))))
     (when (and lhs (src-q-dep src) (range-ssa? st a)) ((src-q-dep src) lhs a))
     x]
    [else
     (define range2 ((src-q-operand src) b))
     (define x (ro-fold st code type range1 range2))
     (when (and lhs (src-q-dep src)) ((src-q-dep src) lhs a) ((src-q-dep src) lhs b))
     x]))

(define (range-of-phi r sid g src)
  (define st (st-of r))
  (define def (gphi-result g))
  (define type (tree-type def))
  (cond
    [(not (supported-type? st type)) #f]
    [else
     (define t (tinfo st type))
     (define bb (bb-of r sid))
     (let/ec return
       (define-values (acc single seen?)
         (for/fold ([acc (ir-undefined t)] [single #f] [seen? #f]) ([a (in-pvector (gphi-args g))])
           (define arg (cdr a))
           (cond
             [(and (ssa-name? arg) (ssa-name=? arg def)) (values acc single seen?)]
             [else
              (define ar ((src-q-phi-operand src) arg (car a) bb))
              (define acc1 (if (ir-undefined? ar) acc (let-values ([(u _c) (ir-union acc ar)]) u)))
              (when (and (not (ir-undefined? ar)) (src-q-dep src) (range-ssa? st arg)) ((src-q-dep src) def arg))
              (define single1 (cond [(not seen?) arg] [(and single (equal? single arg)) single] [else #f]))
              (when (and (ir-varying? acc1) (not single1)) (return acc1))
              (values acc1 single1 #t)])))
       (cond
         [(and single (not (ssa-name? single)))
          (define ar ((src-q-operand src) single))
          (if (ir-singleton ar) ar acc)]
         [else acc]))]))

(define (range-of-call r g)
  (define st (st-of r))
  (define lhs (gcall-lhs g))
  (define type (and lhs (tree-type lhs)))
  (cond
    [(not (and type (supported-type? st type))) #f]
    [else
     (define t (tinfo st type))
     (define x (if (stmt-nonnegative? r g 0) (ir-nonnegative t) (ir-varying t)))
     (if (range-ssa? st lhs)
         (let-values ([(y _c) (ir-intersect x (gimple-range-global st (R-fndecl r) lhs))]) y)
         x)]))

(define (range-of-address r g src)
  (define st (st-of r))
  (define t (tinfo st (tree-type (gassign-lhs g))))
  (define e (gassign-rhs1 g))
  (define base (let loop ([x (tree-operand e 0)]) (if (handled-component-p x) (loop (tree-operand x 0)) x)))
  (cond
    [(and (eq? (tree-code base) 'mem_ref) (ssa-name? (tree-operand base 0)))
     (define p ((src-q-operand src) (tree-operand base 0)))
     (if (and (eq? (tree-code (tree-operand e 0)) 'mem_ref) (integer-zerop (tree-operand base 1)))
         (ir-cast-type p t)
         (if (ir-contains? p 0) (ir-varying t) (ir-nonzero t)))]
    [(decl-p st base) (ir-nonzero t)]
    [else (ir-varying t)]))

;; gimple_stmt_nonnegative_warnv_p (the syntactic subset)
(define (stmt-nonnegative? r g depth)
  (define st (st-of r))
  (define (nonneg-tree t d)
    (cond
      [(not t) #f]
      [(type-unsigned?* st (tree-type t)) (not (pointer-type-p st (tree-type t)))]
      [(eq? (tree-code t) 'integer_cst) (>= (integer-cst-value t) 0)]
      [(ssa-name? t)
       (define id (def-id r t))
       (and id (< d PARAM-NONNEG-DEPTH) (stmt-nonnegative? r (stmt r id) d))]
      [else #f]))
  (cond
    [(gassign? g)
     (define code (gassign-code g))
     (define type (tree-type (gassign-lhs g)))
     (define a (gassign-rhs1 g)) (define b (gassign-rhs2 g))
     (define (rec t) (nonneg-tree t (add1 depth)))
     (cond
       [(and (type-unsigned?* st type) (not (pointer-type-p st type))) #t]
       [(memq code '(lt_expr le_expr gt_expr ge_expr eq_expr ne_expr truth_and_expr truth_or_expr truth_xor_expr truth_not_expr))
        (or (not (= 1 (type-precision* st type))) (type-unsigned?* st type))]
       [(eq? code 'abs_expr) #t]
       [(memq code '(nop_expr convert_expr))
        (define it (tree-type a))
        (cond
          [(and (integral-type-p st it) (type-unsigned?* st it)) (> (type-precision* st type) (type-precision* st it))]
          [(integral-type-p st it) (rec a)]
          [else #f])]
       [(eq? code 'max_expr) (or (rec a) (rec b))]
       [(memq code '(min_expr bit_ior_expr bit_xor_expr trunc_div_expr floor_div_expr ceil_div_expr round_div_expr exact_div_expr))
        (and (rec a) (rec b))]
       [(eq? code 'bit_and_expr) (or (rec a) (rec b))]
       [(eq? code 'mult_expr) (and (equal? a b) (not (type-unsigned?* st type)))]
       [(eq? (gimple-rhs-class st code) 'single) (nonneg-tree a depth)]
       [else #f])]
    [(gphi? g) (for/and ([a (in-pvector (gphi-args g))]) (nonneg-tree (cdr a) (add1 depth)))]
    [else #f]))

;; ------------------------------------------------------------
;; the cache
;; ------------------------------------------------------------

;; temporal_cache
(define (ts r name) (abs (hash-ref (R-stamps r) (ver name) 0)))
(define (always-current? r name) (< (hash-ref (R-stamps r) (ver name) 0) 0))
(define (current? r name)
  (cond
    [(always-current? r name) #t]
    [else
     (define t (ts r name))
     (define d1 (depend1 r name)) (define d2 (depend2 r name))
     (not (or (and d1 (< t (ts r d1))) (and d2 (< t (ts r d2)))))]))
(define (set-timestamp! r name)
  (set-R-time! r (add1 (R-time r)))
  (hash-set! (R-stamps r) (ver name) (R-time r)))
(define (set-always-current! r name v)
  (define t0 (ts r name))
  (define t (if (= t0 0) (begin (set-R-time! r (add1 (R-time r))) (R-time r)) t0))
  (hash-set! (R-stamps r) (ver name) (if v (- t) t)))

;; get_global_range (2 arguments) -> (values range had?)
(define (cache-global r name)
  (define g (hash-ref (R-globals r) (ver name) #f))
  (if g (values g #t) (values (gimple-range-global (st-of r) (R-fndecl r) name) #f)))
;; get_global_range (3 arguments) -> (values range had? current?)
(define (cache-global/current r name)
  (define-values (x had) (cache-global r name))
  (define cur (and had (or (and (ir-singleton x) #t) (current? r name))))
  (unless had (hash-set! (R-globals r) (ver name) x))   ; after inlining: no fold with the global query
  (unless cur (set-always-current! r name #t))
  (values x had cur))

(define (set-global! r name x changed)
  (set-always-current! r name #f)
  (cond
    [(not changed) (unless (current? r name) (set-timestamp! r name))]
    [else
     (define existed (hash-has-key? (R-globals r) (ver name)))
     (hash-set! (R-globals r) (ver name) x)
     (when existed (propagate-updated-value! r name (or (def-bb r name) ENTRY-BB)))
     (when (or (ir-singleton x) (and (pointer-type-p (st-of r) (tree-type name)) (ir-nonzero? x)))
       (set-range-invariant! r name #t))
     (set-timestamp! r name)]))

(define (on-entry r name bb) (hash-ref (R-on-entry r) (cons (ver name) bb) #f))
(define (set-on-entry! r name bb x) (hash-set! (R-on-entry r) (cons (ver name) bb) x))

;; range_of_def
(define (range-of-def r name)
  (define g (hash-ref (R-globals r) (ver name) #f))
  (cond
    [g g]
    [else
     (define id (def-id r name))
     (define s (and id (stmt r id)))
     (if (and s (stmt-lhs s) (ssa-name? (stmt-lhs s)) (ssa-name=? (stmt-lhs s) name))
         (or (fold-stmt r id (stmt-src r id (global-q r)) name) (gimple-range-global (st-of r) (R-fndecl r) name))
         (gimple-range-global (st-of r) (R-fndecl r) name))]))

(define (entry-range r name bb mode)
  (cond
    [(= bb ENTRY-BB) (gimple-range-global (st-of r) (R-fndecl r) name)]
    [(not (has-edge-range? r name)) (range-of-def r name)]
    [(on-entry r name bb) => values]
    [(range-from-dom r name bb mode) => values]
    [else (range-of-def r name)]))

(define (exit-range r name bb mode)
  (cond
    [(= bb ENTRY-BB) (gimple-range-global (st-of r) (R-fndecl r) name)]
    [(eqv? (def-bb r name) bb) (range-of-def r name)]
    [else (entry-range r name bb mode)]))

(define (cache-q r)
  (vector (lambda (x sid) (cache-range-of-expr r x sid))
          (lambda (s d x) (cache-range-on-edge r s d x))))

(define (edge-range r src dest name mode)
  (define x (exit-range r name src mode))
  (define er (gori-edge-range r src dest name (cache-q r)))
  (if er (let-values ([(y _c) (ir-intersect x er)]) y) x))

(define (cache-range-of-expr r name sid)
  (define st (st-of r))
  (cond
    [(not (range-ssa? st name)) (tree-range st name)]
    [else
     (define bb (bb-of r sid))
     (if (eqv? (def-bb r name) bb) (range-of-def r name) (entry-range r name bb 'none))]))
(define (cache-range-on-edge r src dest name)
  (define st (st-of r))
  (if (range-ssa? st name) (edge-range r src dest name 'none) (tree-range st name)))

;; block_range -> irange or #f
(define (block-range r bb name calc)
  (cond
    [(not (has-edge-range? r name)) #f]
    [else
     (define ok
       (or (not calc)
           (let ([db (or (def-bb r name) (and (ssa-default-def? name) ENTRY-BB))])
             (and db (not (= db bb))
                  (begin (fill-block-cache! r name bb) #t)))))
     (and ok (on-entry r name bb))]))

(define (fill-block-cache! r name bb)
  (unless (on-entry r name bb)
    (define x (range-from-dom r name bb 'fill))
    (set-on-entry! r name bb x)))

;; range_from_dom -> irange or #f
(define (range-from-dom r name start mode)
  (cond
    [(eq? mode 'none) #f]
    [else
     (define db (or (def-bb r name) ENTRY-BB))
     (define-values (r0 _had) (cache-global r name))
     (define first-bb (if (and (= start 1) (single-pred? r start)) (car (preds r start)) (idom r start)))
     (define-values (rr work)
       (let loop ([bb first-bb] [prev start] [work '()] [cur r0])
         (cond
           [(not bb) (values cur work)]
           [else
            (define work1
              (cond
                [(has-edge-range? r name bb) (cons prev work)]
                [(and (> (length (preds r prev)) 1)
                      (has-edge-range? r name (car (preds r prev)))
                      (for/and ([p (in-list (preds r prev))]) (or (= p bb) (dominated-by? r p bb))))
                 (cons prev work)]
                [else work]))
            (cond
              [(= db bb) (values cur work1)]
              [(on-entry r name bb) => (lambda (x) (values x work1))]
              [else (loop (idom r bb) bb work1 cur)])])))
     ;; pop the work list (most recent first)
     (for/fold ([cur rr]) ([prev (in-list work)])
       (cond
         [(not (single-pred? r prev))
          (if (eq? mode 'fill) (resolve-dom r name prev cur) cur)]
         [else
          (define p (car (preds r prev)))
          (define er (gori-edge-range r p prev name (cache-q r)))
          (if er (let-values ([(y _c) (ir-intersect cur er)]) y) cur)]))]))

(define (resolve-dom r name bb cur)
  (define db (def-bb r name))
  (define dbb (idom r bb))
  (when (and dbb (not (on-entry r name dbb)) (not (eqv? db dbb)))
    (set-on-entry! r name dbb cur))
  (define t (irange-ti cur))
  (define x
    (for/fold ([x (ir-undefined t)]) ([p (in-list (preds r bb))] #:unless (dominated-by? r p bb))
      (define er (edge-range r p bb name 'read-only))
      (let-values ([(u _c) (ir-union x er)]) u)))
  (set-on-entry! r name bb x)
  x)

;; propagate_updated_value / propagate_cache
(define (propagate-updated-value! r name bb)
  (for ([d (in-list (succs r bb))] #:when (on-entry r name d)) (update-add! r d))
  (unless (null? (R-update r)) (propagate-cache! r name)))
(define (update-add! r bb) (unless (memv bb (R-update r)) (set-R-update! r (cons bb (R-update r)))))
(define (propagate-cache! r name)
  (let loop ()
    (unless (null? (R-update r))
      (define bb (car (R-update r)))
      (set-R-update! r (cdr (R-update r)))
      (define cur (on-entry r name bb))
      (define t (irange-ti cur))
      (define new
        (let/ec return
          (for/fold ([x (ir-undefined t)]) ([p (in-list (preds r bb))])
            (define er (edge-range r p bb name 'read-only))
            (define-values (u _c) (ir-union x er))
            (if (ir-varying? u) (return u) u))))
      (unless (ir-equal? new cur)
        (set-on-entry! r name bb new)
        (for ([d (in-list (succs r bb))] #:when (on-entry r name d)) (update-add! r d)))
      (loop))))

;; ------------------------------------------------------------
;; gimple_ranger
;; ------------------------------------------------------------

(define (ranger-q r)
  (vector (lambda (x sid) (rg-range-of-expr r x sid))
          (lambda (s d x) (rg-range-on-edge r s d x))))

;; fur_depend: operands through the ranger, dependencies registered
(define (depend-src r sid)
  (src-q (lambda (x) (rg-range-of-expr r x sid))
         (lambda (x s d) (rg-range-on-edge r s d x))
         (lambda (lhs op) (register-dependency! r lhs op))))

(define (fold-internal r sid name) (fold-stmt r sid (depend-src r sid) name))

(define (rg-range-of-expr r name sid)
  (define st (st-of r))
  (cond
    [(not (range-ssa? st name)) (or (tree-range st name) #f)]
    [else
     (define bb (bb-of r sid))
     (define did (def-id r name))
     (cond
       [(and did (= (bb-of r did) bb))
        (define-values (g had) (cache-global r name))
        (if had
            (or (block-range r bb name #f) g)
            (rg-range-of-stmt r did name))]
       [else (rg-range-on-entry r bb name)])]))

(define (rg-range-on-entry r bb name)
  (define st (st-of r))
  (cond
    [(not (range-ssa? st name)) (tree-range st name)]
    [else
     (define x (range-of-stmt-name r name))
     (define er (block-range r bb name #t))
     (if er (let-values ([(y _c) (ir-intersect x er)]) y) x)]))

(define (rg-range-on-exit r bb name)
  (define st (st-of r))
  (cond
    [(not (range-ssa? st name)) (tree-range st name)]
    [else
     (define db (def-bb r name))
     (define sid (if (eqv? db bb) (def-id r name) (mfn-last (R-m r) bb)))
     (if sid (rg-range-of-expr r name sid) (rg-range-on-entry r bb name))]))

(define (rg-range-on-edge r src dest name)
  (define st (st-of r))
  (cond
    [(not (supported-type? st (tree-type name))) #f]
    [(not (range-ssa? st name)) (tree-range st name)]
    [else
     (define x (rg-range-on-exit r src name))
     (define er (cache-range-on-edge r src dest name))
     (let-values ([(y _c) (ir-intersect x er)]) y)]))

;; range_of_stmt for an SSA name (its definition; a default definition
;; has a GIMPLE_NOP that folds to the global range)
(define (range-of-stmt-name r name)
  (define did (def-id r name))
  (if did (rg-range-of-stmt r did name) (range-of-default-def r name)))

(define (range-of-default-def r name)
  (define-values (x had cur) (cache-global/current r name))
  (cond
    [(and had cur) x]
    [else
     (define tmp (gimple-range-global (st-of r) (R-fndecl r) name))
     (define-values (y changed) (ir-intersect x tmp))
     (set-global! r name y changed)
     y]))

(define (rg-range-of-stmt r sid [name0 #f])
  (define st (st-of r))
  (define g (stmt r sid))
  (define name (or name0 (let ([l (stmt-lhs g)]) (and l (ssa-name? l) l))))
  (cond
    [(not name)
     (define x (fold-internal r sid #f))
     (when (and x (gcond? g))
       (define bb (bb-of r sid))
       (for ([v (in-list (sort (hash-keys (exports r bb)) <))])
         (propagate-updated-value! r (name-of r v) bb)))
     x]
    [(not (range-ssa? st name)) (tree-range st name)]
    [else
     (define-values (x had cur) (cache-global/current r name))
     (cond
       [(and had cur) x]
       [else
        (unless had (prefill-stmt-dependencies! r name))
        (define tmp (or (fold-internal r sid name) (ir-varying (tinfo st (tree-type name)))))
        (define-values (y changed) (ir-intersect x tmp))
        (set-global! r name y changed)
        y])]))

(define (prefill-name! r name)
  (define st (st-of r))
  (when (range-ssa? st name)
    (define did (def-id r name))
    (define g (and did (stmt r did)))
    (when (and g (or (handler? st g) (gphi? g)))
      (define-values (_x had) (cache-global r name))
      (unless had
        (cache-global/current r name)
        (set-R-stack! r (cons name (R-stack r)))))))

(define (prefill-stmt-dependencies! r ssa)
  (define st (st-of r))
  (define did (def-id r ssa))
  (when (and did (not (ssa-default-def? ssa)))
    (define g0 (stmt r did))
    (when (or (handler? st g0) (gphi? g0))
      (define start (length (R-stack r)))
      (set-R-stack! r (cons ssa (R-stack r)))
      (let loop ()
        (when (> (length (R-stack r)) start)
          (define top (car (R-stack r)))
          (cond
            [(not top)
             (set-R-stack! r (cdr (R-stack r)))
             (define nm (car (R-stack r)))
             (set-R-stack! r (cdr (R-stack r)))
             (when (> (length (R-stack r)) start)
               (define x (or (fold-internal r (def-id r nm) nm) (ir-varying (tinfo st (tree-type nm)))))
               (define-values (tmp _had) (cache-global r nm))
               (define-values (y changed) (ir-intersect tmp x))
               (set-global! r nm y changed))]
            [else
             (set-R-stack! r (cons #f (R-stack r)))
             (define g (stmt r (def-id r top)))
             (cond
               [(gphi? g) (for ([a (in-pvector (gphi-args g))]) (prefill-name! r (cdr a)))]
               [else
                (define-values (code a b) (handler st g))
                (when code
                  (when b (prefill-name! r b))
                  (when a (prefill-name! r a)))])])
          (loop))))))

;; export_global_ranges
(define (rg-export-global-ranges r st0)
  (for/fold ([st st0] [lines '()] #:result (values st (reverse lines)))
            ([v (in-list (sort (hash-keys (R-globals r)) <))])
    (define name (name-of r v))
    (define x (hash-ref (R-globals r) v))
    (cond
      [(and (range-ssa? st name) (not (ir-varying? x)))
       (define-values (st1 line) (set-range-info st (R-fndecl r) name x))
       (values st1 (if line (cons line lines) lines))]
      [else (values st lines)])))
