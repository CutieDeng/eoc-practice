#lang racket/base

;; ============================================================
;; opt: pass_iv_optimize (gcc/tree-ssa-loop-ivopts.cc)
;; ============================================================
;;
;; (ivopts-pass st fndecl cfg) -> (values cfg st)
;;
;; Per loop, innermost first (tree_ssa_iv_optimize_loop):
;;   1. induction variables: the bivs of the header PHIs (find_bivs,
;;      mark_bivs) and the givs of the body statements (find_givs), with
;;      the bases canonicalized by alloc_iv (GCC >= 13: through the
;;      unsigned type and an affine combination; before: only address
;;      expressions);
;;   2. the interesting uses, in groups: nonlinear uses, compares,
;;      memory references (split_address_groups);
;;   3. the candidates: the standard ones, the bivs (and their
;;      originals), the ones derived from the uses and the common bases;
;;   4. the costs: of each candidate, of each (group, candidate) pair
;;      through affine combinations and the target costs of
;;      target-costs.rkt, and the register pressure;
;;   5. the cheapest set (find_optimal_iv_set: two greedy searches);
;;   6. the new IVs (create_iv), the rewritten uses (nonlinear, address
;;      through create_mem_ref, compare by elimination or by expression),
;;      and the IVs left unused are released (release_defs_bitset).
;;
;; IVOPTS_DEBUG=1 prints the TDF_DETAILS tables (candidates, costs, the
;; selected set) to stderr, in GCC's format, for comparison with
;; -fdump-tree-ivopts-details.
;;
;; alloc_iv's iv_can_overflow_p and avg_loop_niter use the loop bounds
;; cunroll left (niter-bounds.rkt / loop-bounds.rkt).
;;
;; Not ported: doloop prediction (aarch64's predict_doloop_p is false),
;; the autoincrement rewriting (IP_BEFORE_USE / IP_AFTER_USE positions:
;; aarch64 sets every USE_*_INCREMENT to 0, so no such candidate exists),
;; USE_PTR_ADDRESS (internal-function memory
;; accesses), symbols in addresses (move_fixed_address_to_symbol),
;; the debug statement fixups of remove_unused_ivs, range information in
;; tree-affine / split_constant_offset.
;; ============================================================

(require racket/list racket/string
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         "../cfg.rkt"
         (only-in "loops.rkt" loops-innermost-first loops-preorder)
         (only-in "../ssa.rkt" virtual-operand?)
         (only-in "../gimplify.rkt" is-gimple-val is-gimple-min-invariant)
         (only-in "../print.rkt" print-expr stmt->string print-type)
         "ssa-ops.rkt"
         "generic-fold.rkt"
         "tree-affine.rkt"
         "target-costs.rkt"
         "scev.rkt"
         (only-in "niter-bounds.rkt" estimated-loop-iterations likely-max-loop-iterations)
         "update-ssa.rkt")

(provide ivopts-pass)

(define debug? (and (getenv "IVOPTS_DEBUG") #t))
(define (dbg fmt . args) (when debug? (apply eprintf fmt args)))
(define (pe x) (print-expr (gs) x))
(define (sstr g)
  (define bb (or (and (cur-data) (D-stmt-bb (data)) (hash-ref (D-stmt-bb (data)) g #f)) 0))
  (stmt->string (gs) (ctx-cfg) bb g))

;; ------------------------------------------------------------
;; costs (comp_cost)
;; ------------------------------------------------------------

(define INFTY 1000000000)
(struct ccost (cost complexity scratch) #:transparent)
(define no-cost (ccost 0 0 0))
(define infinite-cost (ccost INFTY 0 INFTY))
(define (cc n [cx 0]) (ccost n cx 0))
(define (inf? c) (= (ccost-cost c) INFTY))
(define (c+ a b)
  (if (or (inf? a) (inf? b)) infinite-cost
      (ccost (+ (ccost-cost a) (ccost-cost b)) (+ (ccost-complexity a) (ccost-complexity b)) (ccost-scratch a))))
(define (c- a b)
  (if (inf? a) infinite-cost
      (ccost (- (ccost-cost a) (ccost-cost b)) (- (ccost-complexity a) (ccost-complexity b)) (ccost-scratch a))))
(define (c+n a n) (cond [(>= n INFTY) infinite-cost] [(inf? a) a] [else (struct-copy ccost a [cost (+ (ccost-cost a) n)])]))
(define (c-n a n) (if (inf? a) a (struct-copy ccost a [cost (- (ccost-cost a) n)])))
(define (c< a b)
  (if (= (ccost-cost a) (ccost-cost b))
      (< (ccost-complexity a) (ccost-complexity b))
      (< (ccost-cost a) (ccost-cost b))))
(define (c= a b) (and (= (ccost-cost a) (ccost-cost b)) (= (ccost-complexity a) (ccost-complexity b))))
(define (c<= a b) (or (c< a b) (c= a b)))

;; ------------------------------------------------------------
;; the data of one loop (struct ivopts_data)
;; ------------------------------------------------------------

(struct iv (base base-object step ssa-name nonlin-use biv? no-overflow have-address-use) #:mutable)
(struct vinfo (name iv has-nonlin-use preserve-biv inv-id) #:mutable)
;; OP: the operand position in STMT ('cond-lhs 'cond-rhs 'rhs1 'lhs) or #f
(struct use (id group-id type mem-type iv stmt op addr-base addr-offset) #:mutable)
(struct group (id type related uses cost-map selected) #:mutable)
(struct cand (id important involves-undefs pos incremented-at var-before var-after iv cost cost-step
                 ainc-use inv-vars inv-exprs orig-iv) #:mutable)
(struct cpair (cand cost comp inv-vars inv-exprs value) #:mutable)

(struct D (loop speed vinfo relevant important groups cands max-inv-var max-inv-expr inv-exprs
                bivs-not-used regs-used body body-includes-call single-exit? consider-all
                stmt-bb stmt-uid toremove)
  #:mutable)
(define cur-data (make-parameter #f))
(define (data) (cur-data))

(define (address-p t) (memq t '(ref-address ptr-address)))

(define (name-info v)
  (define h (D-vinfo (data)))
  (or (hash-ref h (ssa-name-version v) #f)
      (let ([x (vinfo #f #f #f #f 0)]) (hash-set! h (ssa-name-version v) x) x)))
(define (relevant-add! v) (hash-set! (D-relevant (data)) (ssa-name-version v) #t))
(define (relevant-versions) (sort (hash-keys (D-relevant (data))) <))
(define (ver-info n) (hash-ref (D-vinfo (data)) n #f))

;; ------------------------------------------------------------
;; the loop body, statement positions
;; ------------------------------------------------------------

(define (cfg*) (ctx-cfg))
(define (bb-stmts bb) (pvector->list (GBlock-stmts (cfg-block (cfg*) bb))))
(define (bb-phis bb) (pvector->list (GBlock-phis (cfg-block (cfg*) bb))))
(define (inside? l bb) (loop-inside? l bb))

;; get_loop_body: dfs_enumerate_from (header, reverse)
(define (get-loop-body l)
  (define visited (make-hasheqv))
  (hash-set! visited (Loop-header l) #t)
  (let loop ([stack (list (Loop-header l))] [acc (list (Loop-header l))])
    (cond
      [(null? stack) (reverse acc)]
      [else
       (define bb (car stack))
       (define-values (st2 acc2)
         (for/fold ([st (cdr stack)] [acc acc]) ([p (in-pvector (cfg-preds (cfg*) bb))]
                                                 #:when (and (not (hash-ref visited p #f)) (inside? l p)))
           (hash-set! visited p #t)
           (values (cons p st) (cons p acc))))
       (loop st2 acc2)])))

;; renumber_gimple_stmt_uids_in_blocks: (stmt -> bb), (stmt -> uid)
(define (index-statements! body)
  (define sbb (make-hasheq)) (define uid (make-hasheq))
  (for/fold ([n 0]) ([bb (in-list body)])
    (for/fold ([n n]) ([g (in-list (append (bb-phis bb) (bb-stmts bb)))])
      (hash-set! sbb g bb) (hash-set! uid g n) (add1 n)))
  (set-D-stmt-bb! (data) sbb) (set-D-stmt-uid! (data) uid))
(define (stmt-bb g) (or (hash-ref (D-stmt-bb (data)) g #f) (error 'ivopts "statement not in the loop: ~a" g)))
(define (stmt-uid g) (hash-ref (D-stmt-uid (data)) g))

(define (latch) (car (Loop-latches (D-loop (data)))))
(define (last-stmt bb) (let ([s (bb-stmts bb)]) (and (pair? s) (last s))))

;; ip_normal_pos: the block before the latch when it ends with the exit test
(define (ip-normal-pos l)
  (define lt (car (Loop-latches l)))
  (define preds (cfg-preds (cfg*) lt))
  (cond
    [(not (= 1 (pvector-length preds))) #f]
    [else
     (define bb (pvector-ref preds 0))
     (define g (last-stmt bb))
     (cond
       [(not (and g (gcond? g))) #f]
       [else
        (define ss (pvector->list (cfg-succs (cfg*) bb)))
        (define exit (if (= (GEdge-dest (car ss)) lt) (cadr ss) (car ss)))
        (and (not (inside? l (GEdge-dest exit))) bb)])]))
(define (ip-end-pos l) (car (Loop-latches l)))
(define (empty-block? bb) (and (null? (bb-stmts bb)) (null? (bb-phis bb))))

(define (stmt-after-ip-normal-pos g)
  (define l (D-loop (data)))
  (define bb (ip-normal-pos l)) (define sbb (stmt-bb g))
  (cond [(= sbb (latch)) #t] [(not (= sbb bb)) #f] [else (eq? g (last-stmt bb))]))
(define (stmt-after-inc-pos c g true-if-equal)
  (define cbb (stmt-bb (cand-incremented-at c))) (define sbb (stmt-bb g))
  (cond
    [(not (dominated-by? sbb cbb)) #f]
    [(not (= sbb cbb)) #t]
    [(and true-if-equal (= (stmt-uid g) (stmt-uid (cand-incremented-at c)))) #t]
    [else (> (stmt-uid g) (stmt-uid (cand-incremented-at c)))]))
(define (stmt-after-increment c g)
  (case (cand-pos c)
    [(end) #f]
    [(normal) (stmt-after-ip-normal-pos g)]
    [(original after-use) (stmt-after-inc-pos c g #f)]
    [(before-use) (stmt-after-inc-pos c g #t)]))

;; the def statement / block of an SSA name (#f for default definitions)
(define (def-stmt v) (def-stmt-of v))
(define (def-bb v) (def-bb-of v))

;; ------------------------------------------------------------
;; induction variables
;; ------------------------------------------------------------

;; get_base_address: through handled components and MEM_REF[&x]
(define (get-base-address t)
  (let loop ([t t])
    (cond
      [(handled-component-p t) (loop (tree-operand t 0))]
      [(and (eq? (tree-code t) 'mem_ref) (eq? (tree-code (tree-operand t 0)) 'addr_expr)) (loop (tree-operand (tree-operand t 0) 0))]
      [else t])))

;; walk_tree callback of determine_base_object
(define (determine-base-object e)
  (let/ec k
    (define obj #f)
    (let walk ([t e])
      (when (Tree? t)
        (define code (tree-code t))
        (define o
          (cond
            [(eq? code 'addr_expr)
             (define b (get-base-address (tree-operand t 0)))
             (cond [(not b) t]
                   [(not (eq? (tree-code b) 'mem_ref)) (gf-convert (ptr-type (gs)) (addr-of b))]
                   [else #f])]
            [(and (eq? code 'ssa_name) (ty-ptr? (tree-type t))) (gf-convert (ptr-type (gs)) t)]
            [else #f]))
        (cond
          [o (when obj (k (icst (int-type) 0))) (set! obj o)]
          [(not (expr-p (gs) t)) (void)])
        (when (and (expr-p (gs) t) (or o (not o)))
          (for ([x (in-pvector (tree-ops t))]) (walk x)))))
    obj))
(define (int-type) (or (find-int-by-name "int") (error 'ivopts "no int type")))
(define (find-int-by-name nm)
  (for/first ([p (in-list (intmap->literal-datum (GS-types (gs))))]
              #:when (let ([t (cdr p)]) (and (eq? (tree-code t) 'integer_type) (equal? (type-name-string (gs) (car p)) nm))))
    (car p)))
(define (addr-of t)
  (let-values ([(pt st) (pointer-to (gs) (tree-type t))])
    (gs! st)
    (make-tree 'addr_expr #:type pt #:ops (list t) #:attrs (list (cons 'constant #f)))))

;; contain_complex_addr_expr (GCC 12)
(define (contain-complex-addr-expr e0)
  (define e (gf-strip-nops e0))
  (case (tree-code e)
    [(pointer_plus_expr plus_expr minus_expr)
     (or (contain-complex-addr-expr (tree-operand e 0)) (contain-complex-addr-expr (tree-operand e 1)))]
    [(addr_expr) (not (decl-p (gs) (tree-operand e 0)))]
    [else #f]))

;; alloc_iv
(define (alloc-iv base0 step [no-overflow #f])
  (define expr (gf-strip-nops base0))
  (define base
    (cond
      [(>= (gcc-major-version (gs)) 13)
       (define e (gf-convert (utype-for (tree-type expr)) expr))
       (gf-convert (tree-type base0) (aff->tree (tree->aff e (tree-type e))))]
      [(or (and (eq? (tree-code expr) 'addr_expr) (not (decl-p (gs) (tree-operand expr 0))))
           (contain-complex-addr-expr expr))
       (gf-convert (tree-type base0) (aff->tree (tree->aff expr (tree-type expr))))]
      [else base0]))
  (iv base (determine-base-object base) step #f #f #f
      (or no-overflow (not (iv-can-overflow? (D-loop (data)) (tree-type base) base step)))
      #f))

(define (set-iv! v base step no-overflow)
  (define info (name-info v))
  (relevant-add! v)
  (define x (alloc-iv base step no-overflow))
  (set-iv-ssa-name! x v)
  (set-vinfo-iv! info x))

;; get_iv
(define (get-iv v)
  (define type (tree-type v))
  (cond
    [(not (or (ty-ptr? type) (ty-int? type))) #f]
    [else
     (unless (vinfo-iv (name-info v))
       (define bb (def-bb v))
       (when (or (not bb) (not (inside? (D-loop (data)) bb)))
         (set-iv! v v (icst (if (ty-ptr? type) (sizetype (gs)) type) 0) #t)))
     (vinfo-iv (name-info v))]))

;; extract_single_var_from_expr
(define (extract-single-var e)
  (cond
    [(or (not e) (min-invariant? e)) #f]
    [(and (Tree? e) (expr-p (gs) e))
     (or (for/or ([o (in-pvector (tree-ops e))]) (extract-single-var o))
         (and (eq? (tree-code e) 'ssa_name) e))]
    [else (and (Tree? e) (eq? (tree-code e) 'ssa_name) e)]))

(define (phi-arg-from ph pred) (phi-arg-for-pred ph pred))
(define (preheader-src) (loop-preheader-src (D-loop (data))))

;; find_bivs
(define (find-bivs!)
  (define l (D-loop (data)))
  (for/fold ([found #f]) ([ph (in-list (bb-phis (Loop-header l)))])
    (define res (gphi-result ph))
    (define a (and (not (virtual-operand? (ssa-name-var res))) (not (ssa-virtual? res))
                   (simple-iv l l res #t)))
    (cond
      [(or (not a) (integer-zerop (affine-iv-step a))) found]
      [else
       (define stop (extract-single-var (affine-iv-step a)))
       (define base0 (expand-simple-operations (phi-arg-from ph (preheader-src)) stop))
       (define type (tree-type res))
       (define base (gf-convert type base0))
       (define step (if (ty-ptr? type) (gf-convert (sizetype (gs)) (affine-iv-step a)) (gf-convert type (affine-iv-step a))))
       (set-iv! res base step (affine-iv-no-overflow a))
       #t])))

;; mark_bivs
(define (mark-bivs!)
  (define l (D-loop (data)))
  (set-D-bivs-not-used! (data) 0)
  (for ([ph (in-list (bb-phis (Loop-header l)))])
    (define x (get-iv (gphi-result ph)))
    (when x
      (define var (phi-arg-from ph (latch)))
      (define def (and (eq? (tree-code var) 'ssa_name) (def-stmt var)))
      (unless (and def (gphi? def) (= (def-bb var) (Loop-header l)))
        (define incr (and (eq? (tree-code var) 'ssa_name) (get-iv var)))
        (when incr
          (define incr-bb (def-bb var))
          (when (and incr-bb (eq? (bb-loop incr-bb) l))
            (set-iv-biv?! x #t) (set-iv-biv?! incr #t)
            (when (iv-no-overflow x) (set-D-bivs-not-used! (data) (add1 (D-bivs-not-used (data)))))
            (when (iv-no-overflow incr) (set-D-bivs-not-used! (data) (add1 (D-bivs-not-used (data)))))))))))

;; find_givs
(define (find-givs!)
  (define l (D-loop (data)))
  (for* ([bb (in-list (D-body (data)))] [g (in-list (bb-stmts bb))])
    (when (and (gassign? g) (eq? (tree-code (gassign-lhs g)) 'ssa_name))
      (define lhs (gassign-lhs g))
      (define a (simple-iv l (bb-loop bb) lhs #t))
      (when a
        (define stop (extract-single-var (affine-iv-step a)))
        (set-iv! lhs (expand-simple-operations (affine-iv-base a) stop) (affine-iv-step a) (affine-iv-no-overflow a))))))

(define (find-induction-variables!)
  (and (find-bivs!)
       (begin (find-givs!) (mark-bivs!)
              (when debug?
                (dbg "\n<Induction Vars>:\n")
                (for ([n (in-list (relevant-versions))])
                  (define x (vinfo-iv (ver-info n)))
                  (when (and x (iv-step x) (not (integer-zerop (iv-step x)))) (dump-iv x #t 0))))
              #t)))

(define (dump-iv x name? indent)
  (define p (make-string (* 2 indent) #\space))
  (when (and name? (iv-ssa-name x)) (dbg "~aIV struct:\n~a  SSA_NAME:\t~a\n" p p (pe (iv-ssa-name x))))
  (unless (and name? (iv-ssa-name x)) (dbg "~aIV struct:\n" p))
  (dbg "~a  Type:\t~a\n~a  Base:\t~a\n~a  Step:\t~a\n" p (print-type (gs) (tree-type (iv-base x))) p (pe (iv-base x)) p (pe (iv-step x)))
  (when (and (iv-base-object x) (not (integer-zerop (iv-base-object x)))) (dbg "~a  Object:\t~a\n" p (pe (iv-base-object x))))
  (dbg "~a  Biv:\t~a\n~a  Overflowness wrto loop niter:\t~a\n" p (if (iv-biv? x) "Y" "N") p (if (iv-no-overflow x) "No-overflow" "Overflow")))

;; ------------------------------------------------------------
;; uses
;; ------------------------------------------------------------

(define (record-use! g op x stmt type mem-type addr-base addr-offset)
  (define u (use (length (group-uses g)) (group-id g) type mem-type x stmt op addr-base addr-offset))
  (set-group-uses! g (append (group-uses g) (list u)))
  u)

;; record_invariant
(define (record-invariant! op nonlinear)
  (when (and (Tree? op) (eq? (tree-code op) 'ssa_name) (not (ssa-virtual? op)))
    (define bb (def-bb op))
    (unless (and bb (inside? (D-loop (data)) bb))
      (define info (name-info op))
      (set-vinfo-name! info op)
      (set-vinfo-has-nonlin-use! info (or (vinfo-has-nonlin-use info) nonlinear))
      (when (zero? (vinfo-inv-id info))
        (set-D-max-inv-var! (data) (add1 (D-max-inv-var (data))))
        (set-vinfo-inv-id! info (D-max-inv-var (data))))
      (relevant-add! op))))

(define (record-group! type)
  (define g (group (length (D-groups (data))) type (make-hasheqv) '() #f #f))
  (set-D-groups! (data) (append (D-groups (data)) (list g)))
  g)

;; record_group_use
(define (record-group-use! op x stmt type mem-type)
  (define-values (addr-base addr-offset)
    (cond
      [(address-p type)
       (if (>= (gcc-major-version (gs)) 13)
           (let-values ([(b o) (split-constant-offset (iv-base x))]) (values b (icst-val o)))
           (strip-offset (iv-base x)))]
      [else (values #f 0)]))
  (define g
    (or (and (address-p type)
             (for/first ([g (in-list (D-groups (data)))]
                         #:when (let ([u (car (group-uses g))])
                                  (and (address-p (use-type u))
                                       (operand-equal? (iv-base-object x) (iv-base-object (use-iv u)))
                                       (operand-equal? (iv-step x) (iv-step (use-iv u)))
                                       (operand-equal? addr-base (use-addr-base u)))))
               g))
        (record-group! type)))
  (record-use! g op x stmt type mem-type addr-base addr-offset))

;; find_interesting_uses_op
(define (find-interesting-uses-op! op)
  (cond
    [(not (and (Tree? op) (eq? (tree-code op) 'ssa_name))) #f]
    [else
     (define x (get-iv op))
     (cond
       [(not x) #f]
       [(iv-nonlin-use x)]
       [(integer-zerop (iv-step x)) (record-invariant! op #t) #f]
       [else
        (define u (record-group-use! #f x (def-stmt op) 'nonlinear #f))
        (set-iv-nonlin-use! x u)
        u])]))

;; extract_cond_operands: (values kind control-op bound-op iv-var iv-bound)
;; KIND: 'na 'expr 'expr2 'elim; the ops are operand positions
(define const-iv (iv #f #f #f #f #f #f #t #f))
(define (cond-operands g)
  (define-values (p0 p1) (if (gcond? g) (values 'cond-lhs 'cond-rhs) (values 'rhs1 'rhs2)))
  (define (iv-of p)
    (define o (stmt-operand g p))
    (if (eq? (tree-code o) 'ssa_name) (get-iv o) const-iv))
  (define (zstep? x) (or (eq? x const-iv) (integer-zerop (iv-step x))))
  (define i0 (iv-of p0)) (define i1 (iv-of p1))
  (cond
    [(and i0 i1 (not (zstep? i0)) (not (zstep? i1))) (values 'expr2 p0 p1 i0 i1)]
    [(and (or (not i0) (zstep? i0)) (or (not i1) (zstep? i1))) (values 'na p0 p1 i0 i1)]
    [else
     (define-values (q0 q1 j0 j1) (if (or (not i0) (zstep? i0)) (values p1 p0 i1 i0) (values p0 p1 i0 i1)))
     (values (cond [(not j1) 'expr] [(and (not (zstep? j0)) (zstep? j1)) 'elim] [else 'na]) q0 q1 j0 j1)]))

(define (stmt-operand g p)
  (case p
    [(cond-lhs) (gcond-lhs g)] [(cond-rhs) (gcond-rhs g)]
    [(rhs1) (gassign-rhs1 g)] [(rhs2) (gassign-rhs2 g)] [(lhs) (gassign-lhs g)]))

;; find_interesting_uses_cond
(define (find-interesting-uses-cond! g)
  (define-values (kind pv pb iv-var iv-bound) (cond-operands g))
  (cond
    [(eq? kind 'na)
     (find-interesting-uses-op! (stmt-operand g pv))
     (find-interesting-uses-op! (stmt-operand g pb))]
    [else
     (record-group-use! pv iv-var g 'compare #f)
     (when (eq? kind 'expr2) (record-group-use! pb iv-bound g 'compare #f))]))

;; expr_invariant_in_loop_p
(define (expr-invariant-in-loop? l e)
  (cond
    [(min-invariant? e) #t]
    [(eq? (tree-code e) 'ssa_name) (let ([bb (def-bb e)]) (not (and bb (inside? l bb))))]
    [(not (expr-p (gs) e)) #f]
    [else (for/and ([o (in-pvector (tree-ops e))]) (or (not o) (expr-invariant-in-loop? l o)))]))

;; find_deriving_biv_for_expr
(define (find-deriving-biv e)
  (cond
    [(or (not e) (min-invariant? e)) #f]
    [(and (expr-p (gs) e) (for/or ([o (in-pvector (tree-ops e))]) (find-deriving-biv o)))]
    [(not (eq? (tree-code e) 'ssa_name)) #f]
    [else
     (define x (get-iv e))
     (cond
       [(or (not x) (integer-zerop (iv-step x))) #f]
       [(iv-biv? x) x]
       [else
        (define g (def-stmt e))
        (cond
          [(gphi? g)
           (cond
             [(eq? (bb-loop (def-bb e)) (D-loop (data))) #f]
             [(ssa-virtual? (gphi-result g)) #f]
             [else (for/or ([a (in-pvector (gphi-args g))]) (and (eq? (tree-code (cdr a)) 'ssa_name) (find-deriving-biv (cdr a))))])]
          [(not (gassign? g)) #f]
          [(eq? (gimple-rhs-class (gs) (gassign-code g)) 'single) (find-deriving-biv (gassign-rhs1 g))]
          [(memq (gassign-code g) '(mult_expr plus_expr minus_expr pointer_plus_expr))
           (or (find-deriving-biv (gassign-rhs2 g)) (find-deriving-biv (gassign-rhs1 g)))]
          [(memq (gassign-code g) '(nop_expr convert_expr)) (find-deriving-biv (gassign-rhs1 g))]
          [else #f])])]))

;; record_biv_for_address_use
(define (record-biv-for-address-use! b)
  (when (and b (iv-biv? b) (not (integer-zerop (iv-step b))) (not (iv-have-address-use b)) (iv-no-overflow b))
    (define type (tree-type (iv-base b)))
    (when (and (ty-int? type) (not (ty-ptr? type)))
      (set-iv-have-address-use! b #t)
      (set-D-bivs-not-used! (data) (sub1 (D-bivs-not-used (data))))
      (define base1 (gf-build2 'plus_expr type (iv-base b) (iv-step b)))
      (for ([n (in-list (relevant-versions))])
        (define x (vinfo-iv (ver-info n)))
        (when (and x (iv-biv? x) (not (integer-zerop (iv-step x))) (not (iv-have-address-use x)) (iv-no-overflow x)
                   (eqv? type (tree-type (iv-base x))) (ty-int? (tree-type (iv-base x)))
                   (operand-equal? (iv-step b) (iv-step x)))
          (define base2 (gf-build2 'plus_expr type (iv-base x) (iv-step x)))
          (when (or (operand-equal? base1 (iv-base x)) (operand-equal? base2 (iv-base b)))
            (set-iv-have-address-use! x #t)
            (set-D-bivs-not-used! (data) (sub1 (D-bivs-not-used (data))))))))))

;; for_each_index with a functional callback: CB (base idx) -> (values ok? new-idx);
;; (values ok? new-ref)
(define (for-each-index ref cb)
  (let walk ([r ref])
    (case (tree-code r)
      [(ssa_name) (cb r r)]
      [(mem_ref)
       (define-values (ok i) (cb r (tree-operand r 0)))
       (values ok (and ok (tree-set-operand r 0 i)))]
      [(bit_field_ref view_convert_expr realpart_expr imagpart_expr)
       (define-values (ok x) (walk (tree-operand r 0)))
       (values ok (and ok (tree-set-operand r 0 x)))]
      [(component_ref)
       (define idx (and (> (tree-operand-length r) 2) (tree-operand r 2)))
       (define-values (ok1 i1) (if idx (cb r idx) (values #t #f)))
       (cond
         [(not ok1) (values #f #f)]
         [else
          (define r1 (if idx (tree-set-operand r 2 i1) r))
          (define-values (ok x) (walk (tree-operand r1 0)))
          (values ok (and ok (tree-set-operand r1 0 x)))])]
      [(array_ref array_range_ref)
       (define-values (ok1 i1) (cb r (tree-operand r 1)))
       (cond
         [(not ok1) (values #f #f)]
         [else
          (define r1 (tree-set-operand r 1 i1))
          (define-values (ok x) (walk (tree-operand r1 0)))
          (values ok (and ok (tree-set-operand r1 0 x)))])]
      [(constructor addr_expr) (values #t r)]
      [else (values #t r)])))

;; idx_find_step
(define (idx-find-step stmt step-box)
  (lambda (base idx)
    (define l (D-loop (data)))
    (cond
      [(and (eq? (tree-code base) 'component_ref) (> (tree-operand-length base) 2) (tree-operand base 2))
       (values (expr-invariant-in-loop? l (tree-operand base 2)) idx)]
      [(and (memq (tree-code base) '(array_ref array_range_ref))
            (not (expr-invariant-in-loop? l (array-ref-element-size base))))
       (values #f idx)]
      [(not (eq? (tree-code idx) 'ssa_name)) (values #t idx)]
      [else
       (define x (get-iv idx))
       (cond
         [(not x) (values #f idx)]
         [(integer-zerop (iv-step x)) (values #t (iv-base x))]
         [else
          (define step0 (if (memq (tree-code base) '(array_ref array_range_ref))
                            (array-ref-element-size base)
                            (icst (sizetype (gs)) 1)))
          (cond
            [(not (icst? step0)) (values #f (iv-base x))]
            [else
             (define use-ovf (and (iv-no-overflow x) (nowrap-type-p (tree-type (iv-step x)))))
             (define-values (ok nb ns) (convert-affine-scev l (sizetype (gs)) (iv-base x) (iv-step x) stmt use-ovf #f))
             (cond
               [(not ok) (values #f (iv-base x))]
               [else
                (define st (sizetype (gs)))
                (set-box! step-box (gf-build2 'plus_expr st (unbox step-box) (gf-build2 'mult_expr st step0 ns)))
                (when (positive? (D-bivs-not-used (data)))
                  (record-biv-for-address-use! (if (iv-biv? x) x (find-deriving-biv (iv-ssa-name x)))))
                (values #t (iv-base x))])])])])))

(define (array-ref-element-size r)
  (define et (type-type (gs) (tree-type (tree-operand r 0))))
  (or (type-attr (gs) et 'size-unit #f) (icst (sizetype (gs)) (quotient (icst-val (type-attr (gs) et 'size)) 8))))

;; may_be_nonaddressable_p
(define (may-be-nonaddressable? e)
  (case (tree-code e)
    [(var_decl) (and (decl-flag (gs) e 'hard-register #f) #t)]
    [(target_mem_ref mem_ref) #f]
    [(bit_field_ref array_ref array_range_ref) (may-be-nonaddressable? (tree-operand e 0))]
    [(component_ref) (or (and (tree-attr (tree-operand e 1) 'nonaddressable #f) #t) (may-be-nonaddressable? (tree-operand e 0)))]
    [(view_convert_expr nop_expr convert_expr) #t]
    [else #f]))

;; find_interesting_uses_address
(define (find-interesting-uses-address! stmt op)
  (define ref (stmt-operand stmt op))
  (define (fail)
    (for-each-index ref (lambda (base idx)
                          (find-interesting-uses-op! idx)
                          (values #t idx))))
  (cond
    [(stmt-has-volatile-ops? stmt) (fail)]
    [(memq (tree-code ref) '(bit_field_ref target_mem_ref)) (fail)]
    [else
     (define step-box (box (icst (sizetype (gs)) 0)))
     (define-values (ok base1) (for-each-index ref (idx-find-step stmt step-box)))
     (cond
       [(or (not ok) (integer-zerop (unbox step-box))) (fail)]
       [(may-be-nonaddressable? base1) (fail)]
       [else
        (define a (build-addr base1))
        (define x (alloc-iv a (unbox step-box)))
        (if (not (iv-base-object x))
            (fail)
            (record-group-use! op x stmt 'ref-address (tree-type ref)))])]))

;; build_fold_addr_expr (&MEM[p, 0] -> p)
(define (build-addr t)
  (if (and (eq? (tree-code t) 'mem_ref) (integer-zerop (tree-operand t 1)))
      (tree-operand t 0)
      (addr-of t)))

;; find_interesting_uses_stmt
(define (find-interesting-uses-stmt! g)
  (for ([o (in-list (if (gphi? g)
                        (for/list ([a (in-pvector (gphi-args g))]) (cdr a))
                        (stmt-real-uses g)))])
    (record-invariant! o #f))
  (cond
    [(gcond? g) (find-interesting-uses-cond! g)]
    [(and (gassign? g)
          (let ([lhs (gassign-lhs g)])
            (and (eq? (tree-code lhs) 'ssa_name) (let ([x (get-iv lhs)]) (and x (not (integer-zerop (iv-step x))))))))
     (void)]
    [(and (gassign? g) (eq? (gimple-rhs-class (gs) (gassign-code g)) 'single)
          (or (reference-class? (gassign-rhs1 g)) (is-gimple-val (gs) (gassign-rhs1 g))))
     (if (reference-class? (gassign-rhs1 g))
         (find-interesting-uses-address! g 'rhs1)
         (find-interesting-uses-op! (gassign-rhs1 g)))
     (when (reference-class? (gassign-lhs g)) (find-interesting-uses-address! g 'lhs))]
    [(and (gassign? g) (eq? (tree-code-class (gs) (gassign-code g)) 'tcc_comparison))
     (find-interesting-uses-cond! g)]
    [(and (gphi? g) (= (stmt-bb g) (Loop-header (D-loop (data))))
          (let ([x (get-iv (gphi-result g))]) (and x (not (integer-zerop (iv-step x))))))
     (void)]
    [else
     (for ([o (in-list (if (gphi? g) (for/list ([a (in-pvector (gphi-args g))]) (cdr a)) (stmt-real-uses g)))])
       (when (and (Tree? o) (eq? (tree-code o) 'ssa_name) (get-iv o))
         (find-interesting-uses-op! o)))]))

(define (reference-class? t) (and (Tree? t) (eq? (tree-code-class (gs) (tree-code t)) 'tcc_reference)))

;; find_interesting_uses_outside
(define (find-interesting-uses-outside! src dest)
  (for ([ph (in-list (bb-phis dest))])
    (define d (phi-arg-from ph src))
    (unless (and (Tree? d) (eq? (tree-code d) 'ssa_name) (ssa-virtual? d))
      (find-interesting-uses-op! d))))

;; split_small_address_groups_p
(define (split-small-address-groups?)
  (let loop ([gs* (D-groups (data))] [distinct 1])
    (cond
      [(null? gs*) (<= distinct 2)]
      [else
       (define g (car gs*))
       (cond
         [(= 1 (length (group-uses g))) (loop (cdr gs*) distinct)]
         [else
          (set-group-uses! g (sort (group-uses g) < #:key use-addr-offset))
          (cond
            [(> distinct 2) (loop (cdr gs*) distinct)]
            [else
             (define d
               (let walk ([pre (car (group-uses g))] [rest (cdr (group-uses g))] [d 1])
                 (cond
                   [(or (null? rest) (> d 2)) d]
                   [(not (= (use-addr-offset (car rest)) (use-addr-offset pre))) (walk (car rest) (cdr rest) (add1 d))]
                   [else (walk pre (cdr rest) d)])))
             (loop (cdr gs*) d)])])])))

;; split_address_groups
(define (split-address-groups!)
  (define split? (split-small-address-groups?))
  (let loop ([i 0])
    (when (< i (length (D-groups (data))))
      (define g (list-ref (D-groups (data)) i))
      (define u0 (car (group-uses g)))
      (set-use-id! u0 0) (set-use-group-id! u0 (group-id g))
      (when (> (length (group-uses g)) 1)
        (define new-group #f)
        (define kept
          (for/fold ([kept (list u0)] #:result (reverse kept)) ([next (in-list (cdr (group-uses g)))])
            (define off (- (use-addr-offset next) (use-addr-offset u0)))
            (cond
              [(and (not (zero? off)) (or split? (not (addr-offset-valid? u0 off))))
               (unless new-group (set! new-group (record-group! (group-type g))))
               (set-group-uses! new-group (append (group-uses new-group) (list next)))
               kept]
              [else
               (set-use-id! next (length kept)) (set-use-group-id! next (group-id g))
               (cons next kept)])))
        (set-group-uses! g kept))
      (loop (add1 i)))))

(define (addr-offset-valid? u off)
  (memory-address-valid? (machine-mode-of (use-mem-type u)) (mem-parts #f #t #f #f off)))

;; find_interesting_uses
(define (find-interesting-uses!)
  (define l (D-loop (data)))
  (for ([bb (in-list (D-body (data)))])
    (for ([e (in-pvector (cfg-succs (cfg*) bb))])
      (define d (GEdge-dest e))
      (unless (or (= d EXIT) (inside? l d))
        (find-interesting-uses-outside! bb d)))
    (for ([g (in-list (bb-phis bb))]) (find-interesting-uses-stmt! g))
    (for ([g (in-list (bb-stmts bb))]) (find-interesting-uses-stmt! g)))
  (split-address-groups!)
  (when debug?
    (dbg "\n<IV Groups>:\n")
    (for ([g (in-list (D-groups (data)))])
      (dbg "Group ~a:\n  Type:\t~a\n" (group-id g)
           (case (group-type g) [(nonlinear) "GENERIC"] [(ref-address) "REFERENCE ADDRESS"] [(ptr-address) "PTR ADDRESS"] [else "COMPARE"]))
      (for ([u (in-list (group-uses g))])
        (dbg "  Use ~a.~a:\n    At stmt:\t~a\n    At pos:\t~a\n" (group-id g) (use-id u)
             (sstr (use-stmt u)) (if (use-op u) (pe (stmt-operand (use-stmt u) (use-op u))) ""))
        (dump-iv (use-iv u) #f 2)))
    (dbg "\n")))

;; ------------------------------------------------------------
;; strip_offset / split_constant_offset
;; ------------------------------------------------------------

(define (cst-fits-hwi? x) (and (icst? x) #t))
(define (int-cst-value x) (sext-to (icst-val x) (ty-prec (tree-type x))))

;; strip_offset_1: (values expr offset)
(define (strip-offset-1 expr0 inside-addr top-compref)
  (define orig-type (tree-type expr0))
  (define expr (gf-strip-nops expr0))
  (define type (tree-type expr))
  (define code (tree-code expr))
  (define v15? (>= (gcc-major-version (gs)) 13))
  (define (default-recurse op1)
    ;; recurse into the first operand
    (define-values (op0 off0) (strip-offset-1 (tree-operand expr 0) inside-addr #f))
    (cond
      [(and (eq? op0 (tree-operand expr 0)) (or (not op1) (eq? op1 (tree-operand expr 1)))) (values expr0 off0)]
      [else
       (define e1 (tree-set-operand expr 0 op0))
       (define e2 (if op1 (tree-set-operand e1 1 op1) e1))
       (values (gf-convert orig-type e2) off0)]))
  (case code
    [(pointer_plus_expr plus_expr minus_expr)
     (define-values (op0 off0) (strip-offset-1 (tree-operand expr 0) #f #f))
     (define-values (op1 off1) (strip-offset-1 (tree-operand expr 1) #f #f))
     (define off (if (eq? code 'minus_expr) (- off0 off1) (+ off0 off1)))
     (cond
       [(and (eq? op0 (tree-operand expr 0)) (eq? op1 (tree-operand expr 1))) (values expr0 off)]
       [(integer-zerop op1) (values (gf-convert orig-type op0) off)]
       [(integer-zerop op0)
        (define e
          (cond
            [(eq? code 'minus_expr)
             (if (and v15? (ty-undef? type))
                 (let ([u (utype-for type)]) (gf-build1 'negate_expr u (gf-convert u op1)))
                 (gf-build1 'negate_expr type op1))]
            [else op1]))
        (values (gf-convert orig-type e) off)]
       [else
        (define e
          (if (and v15? (ty-undef? type))
              (let ([u (utype-for type)])
                (gf-build2 (if (eq? code 'pointer_plus_expr) 'plus_expr code) u (gf-convert u op0) (gf-convert u op1)))
              (gf-build2 code type op0 op1)))
        (values (gf-convert orig-type e) off)])]
    [(mult_expr)
     (define op1 (tree-operand expr 1))
     (cond
       [(not (cst-fits-hwi? op1)) (values expr0 0)]
       [else
        (define-values (op0 off0) (strip-offset-1 (tree-operand expr 0) #f #f))
        (cond
          [(eq? op0 (tree-operand expr 0)) (values expr0 0)]
          [else
           (define off (* off0 (int-cst-value op1)))
           (define e
             (cond
               [(integer-zerop op0) op0]
               [(and v15? (ty-undef? type))
                (let ([u (utype-for type)]) (gf-build2 'mult_expr u (gf-convert u op0) (gf-convert u op1)))]
               [else (gf-build2 'mult_expr type op0 op1)]))
           (values (gf-convert orig-type e) off)])])]
    [(array_ref array_range_ref)
     (cond
       [(not inside-addr) (values expr0 0)]
       [else
        (define step (array-ref-element-size expr))
        (cond
          [(not (cst-fits-hwi? step)) (default-recurse #f)]
          [else
           (define-values (op1 off1) (strip-offset-1 (tree-operand expr 1) #f #f))
           (define off (* off1 (int-cst-value step)))
           (cond
             [(and top-compref (integer-zerop op1))
              (define-values (op0 off0) (strip-offset-1 (tree-operand expr 0) inside-addr top-compref))
              (values op0 (+ off off0))]
             [else
              (define-values (e o) (default-recurse op1))
              (values e (+ off o))])])])]
    [(component_ref)
     (cond
       [(not inside-addr) (values expr0 0)]
       [else
        (define field (tree-operand expr 1))
        (define foff (tree-attr field 'offset #f)) (define boff (tree-attr field 'bit-offset #f))
        (cond
          [(and top-compref (icst? foff) (icst? boff))
           (define-values (op0 off0) (strip-offset-1 (tree-operand expr 0) inside-addr top-compref))
           (define b (int-cst-value boff))
           (define abs-off (let ([a (quotient (abs b) 8)]) (if (< b 0) (- a) a)))
           (values op0 (+ off0 (int-cst-value foff) abs-off))]
          [else (default-recurse #f)])])]
    [(addr_expr)
     (define-values (op0 off0) (strip-offset-1 (tree-operand expr 0) #t #t))
     (cond
       [(eq? op0 (tree-operand expr 0)) (values expr0 off0)]
       [else (values (gf-convert orig-type (build-addr op0)) off0)])]
    [(mem_ref)
     (define-values (op0 off0) (strip-offset-1 (tree-operand expr 0) #f #f))
     (cond
       [(eq? op0 (tree-operand expr 0)) (values expr0 off0)]
       [else (values (gf-convert orig-type (tree-set-operand expr 0 op0)) off0)])]
    [else
     ;; ptrdiff_tree_p
     (if (and (icst? expr) (not (zero? (int-cst-value expr))))
         (values (icst orig-type 0) (int-cst-value expr))
         (values expr0 0))]))

(define (strip-offset e) (strip-offset-1 e #f #f))

;; split_constant_offset (tree-data-ref.cc): (values var off) with OFF an
;; ssizetype constant; ranges are not used (a narrower unsigned type
;; than sizetype gives up)
(define (ssizetype) (stype-for (sizetype (gs))))
(define (split-constant-offset exp)
  (define limit (box 8))
  (define-values (v o) (sco exp limit))
  (values (gf-convert (tree-type exp) v) o))
(define (sco exp limit)
  (define type (tree-type exp))
  (define r (and (not (eq? (tree-code exp) 'polynomial_chrec))
                 (not (eq? (gimple-rhs-class (gs) (tree-code exp)) 'ternary))
                 (let-values ([(code op0 op1) (extract-ops exp)])
                   (sco-1 type op0 code op1 limit))))
  (cond
    [r (values (car r) (cdr r))]
    [else (values (if (and (ty-int? type) (not (ty-ptr? type))) (gf-convert (sizetype (gs)) exp) exp)
                  (icst (ssizetype) 0))]))
(define (extract-ops e)
  (case (gimple-rhs-class (gs) (tree-code e))
    [(binary) (values (tree-code e) (tree-operand e 0) (tree-operand e 1))]
    [(unary) (values (tree-code e) (tree-operand e 0) #f)]
    [else (values (tree-code e) e #f)]))
(define (size-binop code a b)
  (define t (ssizetype))
  (icst t (case code [(plus_expr) (+ (int-cst-value a) (int-cst-value b))]
                     [(minus_expr) (- (int-cst-value a) (int-cst-value b))]
                     [(mult_expr) (* (int-cst-value a) (int-cst-value b))])))
(define (distributive-ok? type) (or (>= (ty-prec type) (ty-prec (sizetype (gs)))) (ty-undef? type)))
(define (sco-1 type op0 code op1 limit)
  (define st (sizetype (gs)))
  (case code
    [(integer_cst) (cons (icst st 0) (gf-convert (ssizetype) op0))]
    [(pointer_plus_expr)
     (define-values (v0 o0) (sco op0 limit))
     (define-values (v1 o1) (sco op1 limit))
     (cons (gf-build2 'pointer_plus_expr type v0 v1) (size-binop 'plus_expr o0 o1))]
    [(plus_expr minus_expr)
     (define-values (v0 o0) (sco op0 limit))
     (define-values (v1 o1) (sco op1 limit))
     (and (distributive-ok? type)
          (cons (gf-build2 code st v0 v1) (size-binop code o0 o1)))]
    [(mult_expr)
     (and (icst? op1)
          (let-values ([(v0 o0) (sco op0 limit)])
            (and (distributive-ok? type)
                 (cons (gf-build2 'mult_expr st v0 (gf-convert st op1))
                       (size-binop 'mult_expr o0 (gf-convert (ssizetype) op1))))))]
    [(addr_expr)
     (define-values (core bitpos toffset) (get-inner-reference (tree-operand op0 0)))
     (and (zero? (remainder bitpos 8))
          (let* ([base (build-addr core)] [off0 (icst (ssizetype) (quotient bitpos 8))])
            (define-values (b2 off2)
              (if toffset
                  (let-values ([(pv po) (sco toffset limit)])
                    (values (fold-build-pointer-plus base pv) (size-binop 'plus_expr off0 po)))
                  (values base off0)))
            (cons (gf-convert type b2) off2)))]
    [(ssa_name)
     (define g (def-stmt op0))
     (cond
       [(not (gassign? g)) #f]
       [(zero? (unbox limit)) #f]
       [else
        (set-box! limit (sub1 (unbox limit)))
        (sco-1 type (gassign-rhs1 g) (gassign-code g) (gassign-rhs2 g) limit)])]
    [(nop_expr convert_expr)
     (define itype (tree-type op0))
     (cond
       [(and (or (ty-ptr? itype) (ty-int? itype)) (or (ty-ptr? type) (ty-int? type))
             (or (eq? (ty-ptr? type) (ty-ptr? itype))
                 (and (= (ty-prec type) (ty-prec st)) (= (ty-prec itype) (ty-prec st)))))
        (define-values (v o) (sco op0 limit))
        (cond
          [(ty-ptr? type) (cons (gf-convert type v) o)]
          [(ty-ptr? itype) (cons (gf-convert st v) o)]
          ;; nop_conversion_for_offset_p without ranges
          [(or (>= (ty-prec type) (ty-prec st))
               (and (< (ty-prec itype) (ty-prec type)) (or (ty-uns? itype) (not (ty-uns? type)))))
           (cons v o)]
          [else #f])]
       [else #f])]
    [else #f]))

;; ------------------------------------------------------------
;; invariant variables and expressions
;; ------------------------------------------------------------

;; find_inv_vars: the invariant ids in E (a list, sorted), recording the
;; invariants that the expansion of simple operations brought in
(define (find-inv-vars e)
  (define ids (make-hasheqv))
  (let walk ([t e])
    (when (Tree? t)
      (cond
        [(eq? (tree-code t) 'ssa_name)
         (define info (name-info t))
         (unless (vinfo-iv info)
           (define bb (def-bb t))
           (when (or (not bb) (not (inside? (D-loop (data)) bb)))
             (define st (if (ty-ptr? (tree-type t)) (sizetype (gs)) (tree-type t)))
             (set-iv! t t (icst st 0) #t)
             (record-invariant! t #f)))
         (when (and (positive? (vinfo-inv-id info)) (not (vinfo-has-nonlin-use info)))
           (hash-set! ids (vinfo-inv-id info) #t))]
        [else (for ([o (in-pvector (tree-ops t))]) (walk o))])))
  (sort (hash-keys ids) <))

;; get_loop_invariant_expr: the id of INV-EXPR (operand_equal_p) or #f
(define (get-loop-invariant-expr e0)
  (define e (gf-strip-nops e0))
  (cond
    [(or (icst? e) (eq? (tree-code e) 'ssa_name)) #f]
    [else
     (define tab (D-inv-exprs (data)))
     (or (for/first ([p (in-list tab)] #:when (operand-equal? (car p) e)) (cdr p))
         (let ([id (add1 (D-max-inv-expr (data)))])
           (set-D-max-inv-expr! (data) id)
           (set-D-inv-exprs! (data) (append tab (list (cons e id))))
           id))]))

;; ------------------------------------------------------------
;; candidates
;; ------------------------------------------------------------

;; generic_type_for
(define (generic-type-for t)
  (cond [(ty-ptr? t) (utype-for t)] [(ty-uns? t) t] [else (utype-for t)]))

;; ssa_name_maybe_undef_p (mark_ssa_maybe_undefs): a default definition
;; of something other than a parameter
(define (maybe-undef? v)
  (and (ssa-default-def? v)
       (let ([var (ssa-name-var v)]) (not (and var (memq (tree-code var) '(parm_decl result_decl)))))))
(define (involves-undef? e)
  (let walk ([t e])
    (and (Tree? t)
         (cond
           [(eq? (tree-code t) 'ssa_name) (maybe-undef? t)]
           [(expr-p (gs) t) (for/or ([o (in-pvector (tree-ops t))]) (walk o))]
           [else #f]))))

;; add_candidate_1
(define (add-candidate-1! base0 step0 important pos u incremented-at [orig-iv #f])
  (let/ec return
    (define involves (involves-undef? base0))
    (when (and involves (not (eq? pos 'original))) (return #f))
    (define important1 (and important (not involves)))
    (define-values (base step)
      (cond
        [(eq? pos 'original) (values base0 step0)]
        [else
         (define t (generic-type-for (tree-type base0)))
         (if (eqv? t (tree-type base0))
             (values base0 step0)
             (values (gf-convert t base0) (gf-convert t step0)))]))
    (define c
      (or (for/first ([c (in-list (D-cands (data)))]
                      #:when (and (eq? (cand-pos c) pos)
                                  (eq? (cand-incremented-at c) incremented-at)
                                  (not (and (memq pos '(after-use before-use)) (not (eq? (cand-ainc-use c) u))))
                                  (operand-equal? base (iv-base (cand-iv c)))
                                  (operand-equal? step (iv-step (cand-iv c)))
                                  (= (ty-prec (tree-type base)) (ty-prec (tree-type (iv-base (cand-iv c)))))))
            c)
          (let ()
            (define id (length (D-cands (data))))
            (define x (alloc-iv base step))
            (define var (and (not (eq? pos 'original))
                             (let-values ([(v st) (create-tmp-var-raw (gs) (tree-type base) "ivtmp")]) (gs! st) v)))
            (define c (cand id important1 involves pos incremented-at var var x 0 0 #f '() '() orig-iv))
            (set-D-cands! (data) (append (D-cands (data)) (list c)))
            (unless (icst? step)
              (define ivs (find-inv-vars step))
              (define e (get-loop-invariant-expr step))
              (if e
                  (begin (set-cand-inv-exprs! c (list e)) (set-cand-inv-vars! c '()))
                  (set-cand-inv-vars! c ivs)))
            (set-cand-ainc-use! c (and (memq pos '(after-use before-use)) u))
            (when debug? (dump-cand c))
            c)))
    (set-cand-important! c (or (cand-important c) important1))
    (when u (hash-set! (group-related (list-ref (D-groups (data)) (use-group-id u))) (cand-id c) #t))
    c))

;; allow_ip_end_pos_p
(define (allow-ip-end-pos?)
  (define l (D-loop (data)))
  (define last (last-stmt (ip-end-pos l)))
  (cond
    [(and (>= (gcc-major-version (gs)) 13) last (stmt-ends-bb? last)) #f]
    [(not (ip-normal-pos l)) #t]
    [(not (empty-block? (ip-end-pos l))) #t]
    [else #f]))

;; add_candidate
(define (add-candidate! base step important u [orig-iv #f])
  (define l (D-loop (data)))
  (when (ip-normal-pos l) (add-candidate-1! base step important 'normal u #f orig-iv))
  (when (allow-ip-end-pos?) (add-candidate-1! base step important 'end u #f orig-iv)))

(define (long-type) (or (find-int-by-name "long int") (nonstandard-int-type 64 #f)))

;; add_standard_iv_candidates
(define (add-standard-iv-candidates!)
  (define it (int-type))
  (add-candidate! (icst it 0) (icst it 1) #t #f)
  (define lt (long-type))
  (when (and lt (> (ty-prec lt) (ty-prec it)) (<= (ty-prec lt) 64))
    (add-candidate! (icst lt 0) (icst lt 1) #t #f)))

;; add_iv_candidate_for_biv
(define (add-iv-candidate-for-biv! x)
  (define type (tree-type (iv-base x)))
  (define st (sizetype (gs)))
  (cond
    [(and (iv-no-overflow x) (iv-have-address-use x) (ty-int? type) (not (ty-ptr? type)) (< (ty-prec type) (ty-prec st)))
     (add-candidate! (gf-convert st (iv-base x)) (gf-convert st (iv-step x)) #t #f x)
     (when (iv-nonlin-use x) (add-candidate! (iv-base x) (iv-step x) #t #f))]
    [else (add-candidate! (iv-base x) (iv-step x) #t #f)])
  (if (ty-ptr? type)
      (add-candidate! (icst st 0) (iv-step x) #t #f)
      (add-candidate! (icst type 0) (iv-step x) #t #f))
  (define ph (def-stmt (iv-ssa-name x)))
  (when (gphi? ph)
    (define def (phi-arg-from ph (latch)))
    (define dg (and (eq? (tree-code def) 'ssa_name) (def-stmt def)))
    (unless (gphi? dg)
      (define c (add-candidate-1! (iv-base x) (iv-step x) #t 'original #f dg))
      (when c (set-cand-var-before! c (iv-ssa-name x)) (set-cand-var-after! c def)))))

(define (add-iv-candidate-for-bivs!)
  (for ([n (in-list (relevant-versions))])
    (define x (vinfo-iv (ver-info n)))
    (when (and x (iv-biv? x) (not (integer-zerop (iv-step x))))
      (add-iv-candidate-for-biv! x))))

;; record_common_cand: the common candidates in insertion order,
;; (mcons (cons base step) uses)
(define common-cands (make-parameter #f))
(define (record-common-cand! base step u)
  (define b (common-cands))
  (define e (for/first ([e (in-list (unbox b))]
                        #:when (and (operand-equal? (car (mcar e)) base) (operand-equal? (cdr (mcar e)) step)))
              e))
  (if e
      (set-mcdr! e (append (mcdr e) (list u)))
      (set-box! b (append (unbox b) (list (mcons (cons base step) (list u)))))))

;; add_iv_candidate_derived_from_uses
(define (add-iv-candidate-derived-from-uses!)
  (define l (D-loop (data)))
  ;; qsort by the number of uses, descending (equal counts keep their order)
  (define sorted (sort (unbox (common-cands)) > #:key (lambda (e) (length (mcdr e)))))
  (let loop ([es sorted])
    (when (and (pair? es) (> (length (mcdr (car es))) 1))
      (define e (car es))
      (define c1 (and (ip-normal-pos l) (add-candidate-1! (car (mcar e)) (cdr (mcar e)) #f 'normal #f #f)))
      (define c2 (and (allow-ip-end-pos?) (add-candidate-1! (car (mcar e)) (cdr (mcar e)) #f 'end #f #f)))
      (for ([u (in-list (mcdr e))])
        (define g (list-ref (D-groups (data)) (use-group-id u)))
        (when c1 (hash-set! (group-related g) (cand-id c1) #t))
        (when c2 (hash-set! (group-related g) (cand-id c2) #t)))
      (loop (cdr es))))
  (set-box! (common-cands) '()))

;; add_iv_candidate_for_use
(define (add-iv-candidate-for-use! u)
  (define x (use-iv u))
  (define basetype (tree-type (iv-base x)))
  (cond
    [(not (or (eq? (type-code (gs) basetype) 'integer_type) (ty-ptr? basetype)))
     (error 'ivopts "use of a non-integer type not ported")]
    [else
     (add-candidate! (iv-base x) (iv-step x) #f u)
     (record-common-cand! (iv-base x) (iv-step x) u)
     (record-common-cand! (icst (if (ty-ptr? basetype) (sizetype (gs)) basetype) 0) (iv-step x) u)
     ;; preferred_mem_scale_factor: the target's scaled index is not cheaper
     (when (and (icst? (iv-step x)) (address-p (use-type u)))
       (define fact (preferred-mem-scale-factor (machine-mode-of (use-mem-type u))))
       (define s (int-cst-value (iv-step x)))
       (when (and (not (= fact 1)) (zero? (remainder s fact)))
         (add-candidate! (icst (sizetype (gs)) 0) (icst (sizetype (gs)) (quotient s fact)) #t #f)))
     (define-values (sbase off) (strip-offset (iv-base x)))
     (when (or (not (zero? off)) (not (eq? sbase (iv-base x))))
       (record-common-cand! sbase (iv-step x) u)
       (add-candidate! sbase (iv-step x) #f u))
     (define b (gf-strip-nops (iv-base x)))
     (when (and (iv-base-object x) (eq? (tree-code b) 'pointer_plus_expr))
       (define step (gf-convert (sizetype (gs)) (gf-strip-nops (iv-step x))))
       (define b1 (tree-operand b 1))
       (record-common-cand! b1 step u)
       (cond
         [(>= (gcc-major-version (gs)) 13)
          (define-values (ab ao) (split-constant-offset b1))
          (unless (integer-zerop ao) (record-common-cand! ab step u))]
         [else
          (define-values (sb so) (strip-offset b1))
          (unless (zero? so) (record-common-cand! sb step u))]))
     ;; at last the autoincrement candidates (important)
     (when (address-p (use-type u)) (add-autoinc-candidates! (iv-base x) (iv-step x) u))]))

;; add_autoinc_candidates: an increment once per iteration (the use in
;; the loop itself, dominating the latch), a constant step equal to the
;; access size (or its negation), and the target's USE_*_INCREMENT /
;; USE_*_DECREMENT for the access mode (target-costs.rkt; all false on
;; aarch64, so no candidate is added there)
(define (add-autoinc-candidates! base step u)
  (define l (D-loop (data)))
  (define ubb (stmt-bb (use-stmt u)))
  (when (and (eq? (bb-loop ubb) l) (dominated-by? (latch) ubb) (icst? step))
    (define mode (machine-mode-of (use-mem-type u)))
    (define size (mode-size mode))
    (define s (int-cst-value step))
    (when (or (and (use-autoinc? 'pre-inc mode) (= size s)) (and (use-autoinc? 'pre-dec mode) (= size (- s))))
      (define new-base
        (if (ty-ptr? (tree-type base))
            (gf-build2 'pointer_plus_expr (tree-type base) base (gf-build1 'negate_expr (tree-type step) step))
            (gf-build2 'minus_expr (tree-type base) base (gf-convert (tree-type base) step))))
      (add-candidate-1! new-base step #t 'before-use u (use-stmt u)))
    (when (or (and (use-autoinc? 'post-inc mode) (= size s)) (and (use-autoinc? 'post-dec mode) (= size (- s))))
      (add-candidate-1! base step #t 'after-use u (use-stmt u)))))

(define (add-iv-candidate-for-groups!)
  (for ([g (in-list (D-groups (data)))]) (add-iv-candidate-for-use! (car (group-uses g))))
  (add-iv-candidate-derived-from-uses!))

;; record_important_candidates
(define (record-important-candidates!)
  (define imp (for/list ([c (in-list (D-cands (data)))] #:when (cand-important c)) (cand-id c)))
  (set-D-important! (data) imp)
  (set-D-consider-all! (data) (<= (length (D-cands (data))) 40))
  (for ([g (in-list (D-groups (data)))])
    (for ([i (in-list imp)]) (hash-set! (group-related g) i #t))))

(define (find-iv-candidates!)
  (add-standard-iv-candidates!)
  (add-iv-candidate-for-bivs!)
  (add-iv-candidate-for-groups!)
  ;; set_autoinc_for_original_candidates: autoinc_possible_for_pair needs
  ;; can_autoinc from get_address_cost_ainc, never set when the target has
  ;; no autoincrement (use-autoinc? is false for every mode on aarch64)
  (record-important-candidates!)
  (unless (D-consider-all (data))
    (for ([g (in-list (D-groups (data)))] #:when (eq? (group-type g) 'compare))
      (for ([c (in-list (D-cands (data)))]) (hash-set! (group-related g) (cand-id c) #t))))
  (when debug?
    (dbg "\n<Important Candidates>:\t")
    (for ([c (in-list (D-cands (data)))] #:when (cand-important c)) (dbg " ~a," (cand-id c)))
    (dbg "\n\n<Group, Cand> Related:\n")
    (for ([g (in-list (D-groups (data)))])
      (dbg "  Group ~a:\t~a\n" (group-id g) (string-join (map number->string (sort (hash-keys (group-related g)) <)) ", ")))
    (dbg "\n")))

(define (dump-cand c)
  (dbg "Candidate ~a:\n" (cand-id c))
  (when (cand-var-before c)
    (dbg "  Var befor: ~a\n  Var after: ~a\n" (var-print (cand-var-before c)) (var-print (cand-var-after c))))
  (dbg "  Incr POS: ~a\n" (case (cand-pos c) [(normal) "before exit test"] [(end) "at end"] [(before-use) "before use"]
                            [(after-use) "after use"] [(original) "orig biv"]))
  (dump-iv (cand-iv c) #f 1))
(define (var-print v) (if (eq? (tree-code v) 'ssa_name) (pe v) (decl-print-name* v)))
(define (decl-print-name* v) (or (tree-attr v 'name #f) "?"))

;; ------------------------------------------------------------
;; costs
;; ------------------------------------------------------------

;; avg_loop_niter: the estimate + 1, else the likely bound + 1 when it is
;; at most param_avg_loop_niter (10); the bounds were recorded by cunroll
(define (avg-loop-niter)
  (define l (D-loop (data)))
  ;; *_stmt_executions_int: the bound + 1, -1 when it does not fit a
  ;; signed HOST_WIDE_INT
  (define (hwi+1 n) (and n (< (add1 n) (expt 2 63)) (add1 n)))
  (define e (hwi+1 (estimated-loop-iterations l)))
  (cond
    [e e]
    [else
     (define lk (hwi+1 (likely-max-loop-iterations l)))
     (if (or (not lk) (> lk 10)) 10 lk)]))
;; adjust_setup_cost
(define (adjust-setup-cost cost [round-up #f])
  (cond
    [(>= cost INFTY) cost]
    [(D-speed (data)) (quotient (+ cost (if round-up (sub1 (avg-loop-niter)) 0)) (avg-loop-niter))]
    [else cost]))

(define (tmode t) (machine-mode-of t))

;; get_shiftadd_cost: a cost or #f
(define (get-shiftadd-cost expr mode cost0 cost1 mult speed)
  (define cst (tree-operand mult 1))
  (define v (int-cst-value cst))
  (define m (and (positive? v) (= v (expt 2 (sub1 (integer-length v)))) (sub1 (integer-length v))))
  (cond
    [(not (and m (< m (min 64 (mode-bits mode))))) #f]
    [else
     (define op1 (gf-strip-nops (tree-operand expr 1)))
     (define mult-in-op1 (operand-equal? op1 mult))
     (define as-cost (+ (add-cost speed mode) (shift-cost speed mode m)))
     (define sa-cost (if (not (eq? (tree-code expr) 'minus_expr))
                         (shiftadd-cost speed mode m)
                         (if mult-in-op1 (shiftsub1-cost speed mode m) (shiftsub0-cost speed mode m))))
     (define res (c+ (cc (min as-cost sa-cost)) (if mult-in-op1 cost0 cost1)))
     (define multop (gf-strip-nops (tree-operand mult 0)))
     (if (is-gimple-val (gs) multop) res (c+ res (force-expr-to-var-cost multop speed)))]))

;; force_expr_to_var_cost
(define (force-expr-to-var-cost expr0 speed)
  (define expr (gf-strip-nops expr0))
  (define code (tree-code expr))
  (cond
    [(memq code '(ssa_name var_decl parm_decl result_decl)) no-cost]
    [(min-invariant? expr)
     (cond
       [(icst? expr) (cc (integer-cost speed))]
       [(and (eq? code 'addr_expr) (memq (tree-code (tree-operand expr 0)) '(var_decl parm_decl result_decl)))
        (cc (symbol-cost speed))]
       [else (cc (address-const-cost speed))])]
    [else
     (define-values (op0 op1)
       (case code
         [(pointer_plus_expr plus_expr minus_expr mult_expr exact_div_expr trunc_div_expr bit_and_expr bit_ior_expr
                             lshift_expr rshift_expr lt_expr le_expr gt_expr ge_expr eq_expr ne_expr max_expr min_expr)
          (values (gf-strip-nops (tree-operand expr 0)) (gf-strip-nops (tree-operand expr 1)))]
         [(nop_expr convert_expr negate_expr bit_not_expr) (values (gf-strip-nops (tree-operand expr 0)) #f)]
         [(cond_expr) (values (gf-strip-nops (tree-operand expr 1)) (gf-strip-nops (tree-operand expr 2)))]
         [else (values 'other #f)]))
     (cond
       [(eq? op0 'other) (cc (target-spill-cost speed))]
       [(and (eq? code 'exact_div_expr) (< (gcc-major-version (gs)) 13)) (cc (target-spill-cost speed))]
       [else
        (define (sub-cost o) (if (or (not o) (eq? (tree-code o) 'ssa_name) (constant-class-p (gs) o)) no-cost (force-expr-to-var-cost o speed)))
        (define cost0 (sub-cost op0)) (define cost1 (sub-cost op1))
        (define mode (tmode (tree-type expr)))
        (define cost
          (case code
            [(pointer_plus_expr plus_expr minus_expr negate_expr)
             (define base (cc (add-cost speed mode)))
             (define mult (and (not (eq? code 'negate_expr))
                               (cond [(eq? (tree-code op1) 'mult_expr) op1] [(eq? (tree-code op0) 'mult_expr) op0] [else #f])))
             (or (and mult (scalar-int-mode? mode) (icst? (tree-operand mult 1))
                      (get-shiftadd-cost expr mode cost0 cost1 mult speed))
                 base)]
            [(nop_expr convert_expr)
             (cc (convert-cost (tmode (tree-type expr)) (tmode (tree-type op0)) speed))]
            [(mult_expr)
             (cond
               [(icst? op0) (cc (mult-by-coeff-cost (int-cst-value op0) mode speed))]
               [(icst? op1) (cc (mult-by-coeff-cost (int-cst-value op1) mode speed))]
               [else 'spill])]
            [(exact_div_expr trunc_div_expr)
             (define d (tree-operand expr 1))
             (if (and (icst? d) (positive? (icst-val d)) (= (icst-val d) (expt 2 (sub1 (integer-length (icst-val d))))))
                 (cc (add-cost speed mode))
                 (cc (target-spill-cost speed)))]
            [(bit_and_expr bit_ior_expr bit_not_expr lshift_expr rshift_expr) (cc (add-cost speed mode))]
            [(cond_expr)
             (define c0 (gf-strip-nops (tree-operand expr 0)))
             (if (or (eq? (tree-code c0) 'ssa_name) (constant-class-p (gs) c0)) no-cost (force-expr-to-var-cost c0 speed))]
            [else (cc (add-cost speed mode))]))
        (cond
          [(eq? cost 'spill) (cc (target-spill-cost speed))]
          ;; the shift-and-add case returns its own total
          [(and (memq code '(pointer_plus_expr plus_expr minus_expr))
                (let ([mult (cond [(eq? (tree-code op1) 'mult_expr) op1] [(eq? (tree-code op0) 'mult_expr) op0] [else #f])])
                  (and mult (scalar-int-mode? mode) (icst? (tree-operand mult 1))
                       (get-shiftadd-cost expr mode cost0 cost1 mult speed))))
           cost]
          [else (c+ (c+ cost cost0) cost1)])])]))

;; force_var_cost: (values cost inv-vars)
(define (force-var-cost expr)
  (if (not expr)
      (values no-cost '())
      (let ([ivs (find-inv-vars expr)])
        (values (force-expr-to-var-cost expr (D-speed (data))) ivs))))

;; var_at_stmt
(define (var-at-stmt c g) (if (stmt-after-increment c g) (cand-var-after c) (cand-var-before c)))

;; determine_common_wider_type: (values type a b)
(define (determine-common-wider-type a b)
  (define atype (tree-type a))
  (cond
    [(not (memq (tree-code a) '(nop_expr convert_expr))) (values atype a b)]
    [else
     (define suba (tree-operand a 0))
     (define wider (tree-type suba))
     (cond
       [(< (ty-prec wider) (ty-prec atype)) (values atype a b)]
       [(not (memq (tree-code b) '(nop_expr convert_expr))) (values atype a b)]
       [(not (= (ty-prec wider) (ty-prec (tree-type (tree-operand b 0))))) (values atype a b)]
       [else (values wider suba (tree-operand b 0))])]))

;; constant_multiple_of: the multiplier or #f
(define (constant-multiple-of top bot)
  (cond
    [(>= (gcc-major-version (gs)) 13)
     (aff-constant-multiple (tree->aff top (tree-type top)) (tree->aff bot (tree-type bot)))]
    [else (constant-multiple-of-12 top bot)]))
(define (constant-multiple-of-12 top0 bot0)
  (define prec (ty-prec (tree-type top0)))
  (define top (gf-strip-nops top0)) (define bot (gf-strip-nops bot0))
  (cond
    [(operand-equal? top bot) 1]
    [else
     (case (tree-code top)
       [(mult_expr)
        (define mby (tree-operand top 1))
        (and (icst? mby)
             (let ([r (constant-multiple-of-12 (tree-operand top 0) bot)])
               (and r (sext-to (* r (icst-val mby)) prec))))]
       [(plus_expr minus_expr)
        (define p0 (constant-multiple-of-12 (tree-operand top 0) bot))
        (define p1 (and p0 (constant-multiple-of-12 (tree-operand top 1) bot)))
        (and p1 (sext-to (+ p0 (if (eq? (tree-code top) 'minus_expr) (- p1) p1)) prec))]
       [(integer_cst)
        (and (icst? bot)
             (let ([p0 (sext-to (icst-val top) (ty-prec (tree-type top)))]
                   [p1 (sext-to (icst-val bot) (ty-prec (tree-type bot)))])
               (and (not (zero? p1)) (zero? (remainder p0 p1)) (sext-to (quotient p0 p1) prec))))]
       [else #f])]))

;; get_computation_aff_1: (values aff-inv aff-var rat) or (values #f #f #f)
(define (get-computation-aff-1 at u c)
  (define ubase0 (iv-base (use-iv u))) (define ustep (iv-step (use-iv u)))
  (define cbase0 (iv-base (cand-iv c))) (define cstep0 (iv-step (cand-iv c)))
  (define utype (tree-type ubase0)) (define ctype (tree-type cbase0))
  (cond
    [(> (ty-prec utype) (ty-prec ctype)) (values #f #f #f)]
    [else
     (define var0 (var-at-stmt c at))
     (define uutype (utype-for utype))
     (define-values (cbase cstep var)
       (cond
         [(< (ty-prec utype) (ty-prec ctype))
          (define-values (cb cs)
            (if (and (cand-orig-iv c) (memq (tree-code cbase0) '(nop_expr convert_expr))
                     (or (memq (tree-code cstep0) '(nop_expr convert_expr)) (icst? cstep0)))
                (let* ([ib (tree-operand cbase0 0)]
                       [is (if (memq (tree-code cstep0) '(nop_expr convert_expr)) (tree-operand cstep0 0) cstep0)])
                  (if (<= (ty-prec (tree-type ib)) (ty-prec uutype)) (values ib is) (values cbase0 cstep0)))
                (values cbase0 cstep0)))
          (values (gf-convert uutype cb) (gf-convert uutype cs) (gf-convert uutype var0))]
         [else (values cbase0 cstep0 var0)]))
     (define rat
       (if (and (eq? (cand-pos c) 'original) (eq? (cand-incremented-at c) (use-stmt u)))
           1
           (constant-multiple-of ustep cstep)))
     (cond
       [(not rat) (values #f #f #f)]
       [else
        (define-values (common ubase cbase*) (determine-common-wider-type ubase0 cbase))
        (define aff-inv0 (tree->aff ubase common))
        (define aff-cbase0 (tree->aff cbase* common))
        (define aff-var0 (tree->aff var uutype))
        (define aff-cbase
          (if (stmt-after-increment c at)
              (aff-add aff-cbase0 (tree->aff (if (eqv? common uutype) cstep (gf-convert common cstep)) common))
              aff-cbase0))
        (define aff-inv1 (aff-add aff-inv0 (aff-scale aff-cbase (- rat))))
        (define aff-inv (if (eqv? common uutype) aff-inv1 (aff-convert aff-inv1 uutype)))
        (values aff-inv (aff-scale aff-var0 rat) rat)])]))

(define (get-computation-aff at u c)
  (define-values (inv var rat) (get-computation-aff-1 at u c))
  (and inv (aff-add inv var)))

;; get_use_type
(define (get-use-type u)
  (if (eq? (use-type u) 'ref-address)
      (let-values ([(pt st) (pointer-to (gs) (tree-type (stmt-operand (use-stmt u) (use-op u))))]) (gs! st) pt)
      (tree-type (iv-base (use-iv u)))))

(define (get-computation-at at u c)
  (define a (get-computation-aff at u c))
  (and a (gf-convert (get-use-type u) (aff->tree a))))

;; get_address_cost: (values cost inv-vars inv-expr)
(define (get-address-cost u c aff-inv0 aff-var ratio)
  (define speed (D-speed (data)))
  (define mem-mode (machine-mode-of (use-mem-type u)))
  (define aff-inv aff-inv0)
  (define simple-inv #t)
  (define parts (mem-parts #f #t #f #f #f))
  (define ok-with-ratio #f) (define ok-without-ratio #f)
  (cond
    [(not (aff-const? aff-inv))
     (set! parts (struct-copy mem-parts parts [index #t]))
     (set! ok-without-ratio (memory-address-valid? mem-mode parts))
     (unless (= ratio 1)
       (set! ok-with-ratio (memory-address-valid? mem-mode (struct-copy mem-parts parts [step ratio]))))
     (when ok-with-ratio (set! parts (struct-copy mem-parts parts [step ratio])))
     (cond
       [(or ok-with-ratio ok-without-ratio)
        (unless (zero? (aff-offset aff-inv))
          (define p2 (struct-copy mem-parts parts [offset (aff-offset aff-inv)]))
          (when (memory-address-valid? mem-mode p2)
            (set! parts p2)
            (set! aff-inv (struct-copy aff aff-inv [offset 0]))))
        ;; move_fixed_address_to_symbol: no fixed-address objects (not ported)
        (void)]
       [else (set! parts (struct-copy mem-parts parts [index #f]))])]
    [else
     ;; get_address_cost_ainc: infinite when the target has no
     ;; USE_*_INCREMENT / DECREMENT for the mode (aarch64), so can_autoinc
     ;; stays false and the plain address cost is used
     (unless (aff-zero? aff-inv)
       (define p2 (struct-copy mem-parts parts [offset (aff-offset aff-inv)]))
       (when (memory-address-valid? mem-mode p2)
         (set! parts p2)
         (set! aff-inv (struct-copy aff aff-inv [offset 0]))))])
  (set! simple-inv (and simple-inv (or (aff-const? aff-inv) (aff-singleton-var? aff-inv))))
  (define comp-inv (and (not (aff-zero? aff-inv)) (aff->tree aff-inv)))
  (define-values (cost0 inv-vars0) (if comp-inv (force-var-cost comp-inv) (values no-cost '())))
  (define var-cost
    (c+ (if (and (not (= ratio 1)) (not (mem-parts-step parts))) (cc (mult-by-coeff-cost ratio (addr-mode) speed)) no-cost)
        (if (and comp-inv (not (mem-parts-index parts))) (cc (add-cost speed (addr-mode))) no-cost)))
  (define-values (cost1 inv-vars inv-expr)
    (cond
      [(and comp-inv (not simple-inv))
       (define e (get-loop-invariant-expr comp-inv))
       (define c (adjust-setup-cost (ccost-cost cost0) #t))
       (values (ccost c (ccost-complexity cost0) c) (if e '() inv-vars0) e)]
      [else (values cost0 inv-vars0 #f)]))
  (define cost2 (c+ (c+ cost1 var-cost) (cc (address-cost mem-mode parts speed))))
  (define cx (+ (if (mem-parts-symbol parts) 1 0)
                (if (and (mem-parts-step parts) ok-without-ratio) 1 0)
                (if (and (mem-parts-base parts) (mem-parts-index parts)) 1 0)
                (if (and (mem-parts-offset parts) (not (zero? (mem-parts-offset parts)))) 1 0)))
  (values (ccost (ccost-cost cost2) (+ (ccost-complexity cost2) cx) (ccost-scratch cost2)) inv-vars inv-expr))
(define (addr-mode) "DI")

;; get_computation_cost: (values cost inv-vars inv-expr)
(define (get-computation-cost u c address?)
  (define at (use-stmt u))
  (define ubase (iv-base (use-iv u))) (define cbase (iv-base (cand-iv c)))
  (define utype (tree-type ubase)) (define ctype (tree-type cbase))
  (define speed (D-speed (data)))
  (define uobj (iv-base-object (use-iv u))) (define cobj (iv-base-object (cand-iv c)))
  (cond
    [(> (ty-prec utype) (ty-prec ctype)) (values infinite-cost '() #f)]
    [(and (or address? (and uobj cobj (ty-ptr? (tree-type uobj)) (ty-ptr? (tree-type cobj))))
          uobj cobj (not (operand-equal? uobj cobj)))
     (values infinite-cost '() #f)]
    [else
     (define-values (aff-inv aff-var rat) (get-computation-aff-1 at u c))
     (cond
       [(or (not aff-inv) (not (< (- (expt 2 63)) rat (expt 2 63)))) (values infinite-cost '() #f)]
       [address?
        (define-values (cost ivs e) (get-address-cost u c aff-inv aff-var rat))
        (values (scaled-cost at cost) ivs e)]
       [else
        (define simple-inv (or (aff-const? aff-inv) (aff-singleton-var? aff-inv)))
        (define st (stype-for (aff-type aff-inv)))
        (define inv2 (aff-convert aff-inv st))
        (define comp-inv (and (not (aff-zero? inv2)) (aff->tree inv2)))
        (define-values (cost0 ivs0) (force-var-cost comp-inv))
        (define-values (cost1 ivs e)
          (cond
            [(and comp-inv (not simple-inv))
             (define e (get-loop-invariant-expr comp-inv))
             (define c1 (adjust-setup-cost (ccost-cost cost0)))
             (values (ccost c1 (ccost-complexity cost0) c1) (if e '() ivs0) e)]
            [(and comp-inv (constant-class-p (gs) comp-inv)) (values no-cost ivs0 #f)]
            [else (values cost0 ivs0 #f)]))
        (define cost2 (if (< (ty-prec utype) (ty-prec ctype))
                          (c+ cost1 (cc (convert-cost (tmode utype) (tmode ctype) speed)))
                          cost1))
        (define aratio (if (and (< rat 0) comp-inv (not (integer-zerop comp-inv))) (- rat) rat))
        (define cost3 (if (not (= rat 1)) (c+ cost2 (cc (mult-by-coeff-cost aratio (tmode utype) speed))) cost2))
        (define cost4 (if (and comp-inv (not (integer-zerop comp-inv))) (c+ cost3 (cc (add-cost speed (tmode utype)))) cost3))
        (values (scaled-cost at cost4) ivs e)])]))

;; get_scaled_computation_cost_at: the body blocks are not scaled on the
;; fixtures (determine_scaling_factor)
(define (scaled-cost at cost) cost)

;; determine_group_iv_cost_generic
(define (determine-group-iv-cost-generic! g c)
  (define u (car (group-uses g)))
  (cond
    [(and (eq? (cand-pos c) 'original) (eq? (cand-incremented-at c) (use-stmt u)))
     (set-group-iv-cost! g c no-cost '() #f #f '())
     #t]
    [(and (cand-involves-undefs c) (not (operand-equal? (iv-base (cand-iv c)) (iv-base (use-iv u))))) #f]
    [else
     (define-values (cost ivs e) (get-computation-cost u c #f))
     (set-group-iv-cost! g c cost ivs #f #f (if e (list e) '()))
     (not (inf? cost))]))

;; determine_group_iv_cost_address
(define (determine-group-iv-cost-address! g c)
  (define u (car (group-uses g)))
  (define-values (cost ivs e) (get-computation-cost u c #t))
  (define exprs (if e (list e) '()))
  (define-values (sum exprs2)
    (for/fold ([sum cost] [exprs exprs]) ([next (in-list (cdr (group-uses g)))] #:unless (inf? sum))
      (define-values (c2 _ivs e2) (get-computation-cost next c #t))
      (values (c+ sum c2) (if e2 (remove-duplicates (append exprs (list e2))) exprs))))
  (set-group-iv-cost! g c sum ivs #f #f (sort exprs2 <))
  (not (inf? sum)))

;; iv_period
(define (iv-period x)
  (define step (iv-step x))
  (define t (utype-for (tree-type step)))
  (define v (bitwise-and (icst-val step) (sub1 (arithmetic-shift 1 (ty-prec t)))))
  (define zeros (let loop ([v v] [n 0]) (if (or (zero? v) (odd? v)) n (loop (arithmetic-shift v -1) (add1 n)))))
  (icst t (sub1 (arithmetic-shift 1 (- (ty-prec t) zeros)))))

;; iv_elimination_compare
(define (iv-elimination-compare u)
  (define l (D-loop (data)))
  (define ex-bb (stmt-bb (use-stmt u)))
  (define ss (pvector->list (cfg-succs (cfg*) ex-bb)))
  (define exit (if (inside? l (GEdge-dest (car ss))) (cadr ss) (car ss)))
  (if (edge-flag? (find-edge (cfg*) ex-bb (GEdge-dest exit)) EDGE-TRUE-VALUE) 'eq_expr 'ne_expr))

;; cand_value_at: an aff
(define (cand-value-at c at desc)
  (define x (cand-iv c))
  (define type (tree-type (iv-base x)))
  (define steptype (if (ty-ptr? type) (sizetype (gs)) (utype-for type)))
  (define ctl (niter-desc-control desc))
  (define-values (niter after-adjust)
    (let ([after (stmt-after-increment c at)] [niter (niter-desc-niter desc)])
      (cond
        [(and after ctl (affine-iv-no-overflow ctl) (integer-onep (affine-iv-step ctl))
              (memq (niter-desc-cmp desc) '(lt_expr ne_expr))
              (niter-desc-bound desc) (eq? (tree-code (niter-desc-bound desc)) 'ssa_name))
         (cond
           [(integer-onep (affine-iv-base ctl)) (values (niter-desc-bound desc) #f)]
           [(and (eq? (tree-code niter) 'minus_expr) (integer-onep (tree-operand niter 1))) (values (tree-operand niter 0) #f)]
           [else (values niter after)])]
        [else (values niter after)])))
  (define step (aff-convert (tree->aff (iv-step x) (tree-type (iv-step x))) steptype))
  (define nit (aff-convert (tree->aff niter (tree-type niter)) steptype))
  (define delta0 (aff-mult nit step))
  (define delta (if after-adjust (aff-add delta0 step) delta0))
  (define val0 (tree->aff (iv-base x) type))
  (aff-add (if (ty-ptr? type) val0 (aff-convert val0 steptype)) delta))

;; difference_cannot_overflow_p
(define (difference-cannot-overflow? base0 offset)
  (cond
    [(not (nowrap-type-p (tree-type base0))) #f]
    [else
     (define base (expand-simple-operations base0 #f))
     (define-values (code e1 e2)
       (cond
         [(eq? (tree-code base) 'ssa_name)
          (define g (def-stmt base))
          (if (and (gassign? g) (eq? (gimple-rhs-class (gs) (gassign-code g)) 'binary))
              (values (gassign-code g) (gassign-rhs1 g) (gassign-rhs2 g))
              (values #f #f #f))]
         [(eq? (gimple-rhs-class (gs) (tree-code base)) 'binary)
          (values (tree-code base) (tree-operand base 0) (tree-operand base 1))]
         [else (values #f #f #f)]))
     (define cache (make-hasheqv))
     (define (def-of v) (let ([g (def-stmt v)]) (and (gassign? g) g)))
     (define (expand e) (tree->aff-expand e (tree-type e) def-of cache))
     (case code
       [(plus_expr)
        (define aoff (aff-scale (expand offset) -1))
        (or (aff-zero? (aff-add (expand e2) aoff))
            (aff-zero? (aff-add (expand e1) aoff)))]
       [(pointer_plus_expr) (aff-zero? (aff-add (expand e2) (aff-scale (expand offset) -1)))]
       [else #f])]))

;; iv_elimination_compare_lt: the new comparison or #f
(define (iv-elimination-compare-lt c comp desc)
  (define ctype (tree-type (iv-base (cand-iv c))))
  (define nit-type (tree-type (niter-desc-niter desc)))
  (define mbz (niter-desc-may-be-zero desc))
  (define-values (a b)
    (case (tree-code mbz)
      [(gt_expr)
       (define op0 (tree-operand mbz 0))
       (if (and (eq? (tree-code op0) 'plus_expr) (integer-onep (tree-operand op0 1)))
           (values (tree-operand op0 0) (tree-operand mbz 1))
           (values #f #f))]
      [(lt_expr)
       (define op1 (tree-operand mbz 1))
       (if (and (eq? (tree-code op1) 'plus_expr) (integer-onep (tree-operand op1 1)))
           (values (tree-operand op1 0) (tree-operand mbz 0))
           (values #f #f))]
      [else (values #f #f)]))
  (cond
    [(or (not (eq? (cand-pos c) 'original)) (not (nowrap-type-p ctype))) #f]
    [(not (D-single-exit? (data))) #f]
    [(not (icst? (iv-step (cand-iv c)))) #f]
    [(not a) #f]
    [else
     (define nit (aff-scale (tree->aff (niter-desc-niter desc) nit-type) -1))
     (define tmpa (aff-scale (tree->aff (gf-convert nit-type a) nit-type) -1))
     (define tmpb (aff-add (aff-add (tree->aff (gf-convert nit-type b) nit-type) tmpa) nit))
     (cond
       [(or (not (null? (aff-elts tmpb))) (not (= (aff-offset tmpb) 1))) #f]
       [else
        (define st (tree-type (iv-step (cand-iv c))))
        (define offset (gf-build2 'mult_expr st (iv-step (cand-iv c)) (gf-convert st a)))
        (cond
          [(not (difference-cannot-overflow? (iv-base (cand-iv c)) offset)) #f]
          [else
           (define step (int-cst-value (iv-step (cand-iv c))))
           (define newc (if (< step 0) 'gt_expr 'lt_expr))
           (case comp [(ne_expr) newc] [(eq_expr) (invert-cmp newc)])])])]))

;; may_eliminate_iv: (values bound comp) or (values #f #f)
(define (may-eliminate-iv u c)
  (define l (D-loop (data)))
  (define ex-bb (stmt-bb (use-stmt u)))
  (let/ec return
    (unless (icst? (iv-step (cand-iv c))) (return #f #f))
    (unless (and (eq? (use-stmt u) (last-stmt ex-bb)) (gcond? (use-stmt u)) (dominated-by? (latch) ex-bb))
      (return #f #f))
    (define ss (pvector->list (cfg-succs (cfg*) ex-bb)))
    (define exit-e (if (inside? l (GEdge-dest (car ss))) (cadr ss) (car ss)))
    (when (inside? l (GEdge-dest exit-e)) (return #f #f))
    (define desc (number-of-iterations-exit l (cons ex-bb (GEdge-dest exit-e))))
    (unless desc (return #f #f))
    (define period (iv-period (cand-iv c)))
    (define after (stmt-after-increment c (use-stmt u)))
    (cond
      [(icst? (niter-desc-niter desc))
       (define n (icst-val (niter-desc-niter desc)))
       (if after
           (unless (< n (icst-val period)) (return #f #f))
           (when (< (icst-val period) n) (return #f #f)))]
      [else
       (define max-niter (+ (niter-desc-max desc) (if after 1 0)))
       ;; max_loop_iterations (the recorded loop bound) is not ported
       (when (> max-niter (icst-val period)) (return #f #f))])
    (define bnd (cand-value-at c (use-stmt u) desc))
    (define bound (gf-convert (tree-type (iv-base (cand-iv c))) (aff->tree bnd)))
    (define comp (iv-elimination-compare u))
    (when (let-values ([(exp? _ovf) (expression-expensive-p bound)]) exp?) (return #f #f))
    (cond
      [(not (integer-zerop (niter-desc-may-be-zero desc)))
       (define c2 (iv-elimination-compare-lt c comp desc))
       (if c2 (values bound c2) (values #f #f))]
      [else (values bound comp)])))

;; parm_decl_cost
(define (parm-decl-cost bound)
  (define s (gf-strip-nops bound))
  (if (and (eq? (tree-code s) 'ssa_name) (ssa-default-def? s)
           (let ([v (ssa-name-var s)]) (and v (eq? (tree-code v) 'parm_decl)))
           (D-body-includes-call (data)))
      4
      0))

;; determine_group_iv_cost_cond
(define (determine-group-iv-cost-cond! g c)
  (define u (car (group-uses g)))
  (define-values (kind pv pb iv-var cmp-iv) (cond-operands (use-stmt u)))
  (define control (stmt-operand (use-stmt u) pv))
  (define bound-cst (stmt-operand (use-stmt u) pb))
  (define-values (elim-cost elim-vars elim-expr bound comp)
    (cond
      [(eq? kind 'elim)
       (define-values (bnd cmp) (may-eliminate-iv u c))
       (cond
         [bnd
          (define-values (ec ivs) (force-var-cost bnd))
          (define ec1 (cond [(zero? (ccost-cost ec)) (struct-copy ccost ec [cost (parm-decl-cost bnd)])]
                            [(icst? bnd) (struct-copy ccost ec [cost 0])]
                            [else ec]))
          (define-values (ivs2 e2) (if (> (length ivs) 1) (values '() (get-loop-invariant-expr bnd)) (values ivs #f)))
          (values (struct-copy ccost ec1 [cost (adjust-setup-cost (ccost-cost ec1))]) ivs2 e2 bnd cmp)]
         [else (values infinite-cost '() #f #f #f)])]
      [else (values infinite-cost '() #f #f #f)]))
  (define elim-cost2
    (if (and (not (inf? elim-cost)) (integer-zerop bound-cst)
             (or (operand-equal? control (cand-var-after c)) (operand-equal? control (cand-var-before c))))
        (c-n elim-cost 1)
        elim-cost))
  (define-values (ex-cost ex-vars0 ex-expr) (get-computation-cost u c #f))
  (define ex-vars (if (and cmp-iv (not (eq? cmp-iv const-iv)))
                      (sort (remove-duplicates (append ex-vars0 (find-inv-vars (iv-base cmp-iv)))) <)
                      ex-vars0))
  (define-values (bc _bv) (force-var-cost bound-cst))
  (define bc1 (cond [(zero? (ccost-cost bc)) (struct-copy ccost bc [cost (parm-decl-cost bound-cst)])]
                    [(icst? bound-cst) (struct-copy ccost bc [cost 0])]
                    [else bc]))
  (define ex-cost2 (c+ ex-cost bc1))
  (if (c<= elim-cost2 ex-cost2)
      (set-group-iv-cost! g c elim-cost2 elim-vars bound comp (if elim-expr (list elim-expr) '()))
      (set-group-iv-cost! g c ex-cost2 ex-vars #f #f (if ex-expr (list ex-expr) '())))
  (not (inf? (if (c<= elim-cost2 ex-cost2) elim-cost2 ex-cost2))))

;; set_group_iv_cost (consider_all_candidates: a map by candidate id)
(define (set-group-iv-cost! g c cost ivs value comp exprs)
  (unless (inf? cost)
    (hash-set! (group-cost-map g) (cand-id c) (cpair c cost comp ivs exprs value))))
(define (get-group-iv-cost g c) (and c (hash-ref (group-cost-map g) (cand-id c) #f)))

(define (determine-group-iv-cost! g c)
  (case (group-type g)
    [(nonlinear) (determine-group-iv-cost-generic! g c)]
    [(ref-address ptr-address) (determine-group-iv-cost-address! g c)]
    [(compare) (determine-group-iv-cost-cond! g c)]))

;; determine_group_iv_costs
(define (determine-group-iv-costs!)
  (for ([g (in-list (D-groups (data)))]) (set-group-cost-map! g (make-hasheqv)))
  (for ([g (in-list (D-groups (data)))])
    (cond
      [(D-consider-all (data)) (for ([c (in-list (D-cands (data)))]) (determine-group-iv-cost! g c))]
      [else
       (for ([j (in-list (sort (hash-keys (group-related g)) <))])
         (unless (determine-group-iv-cost! g (list-ref (D-cands (data)) j))
           (hash-remove! (group-related g) j)))]))
  (when debug?
    (dbg "\n<Invariant Vars>:\n")
    (for ([n (in-list (relevant-versions))])
      (define info (ver-info n))
      (when (positive? (vinfo-inv-id info))
        (dbg "Inv ~a:\t~a~a\n" (vinfo-inv-id info) (pe (vinfo-name info)) (if (vinfo-has-nonlin-use info) "" "\t(eliminable)"))))
    (dbg "\n<Invariant Expressions>:\n")
    (for ([p (in-list (D-inv-exprs (data)))]) (dbg "inv_expr ~a: \t~a\n" (cdr p) (pe (car p))))
    (dbg "\n<Group-candidate Costs>:\n")
    (for ([g (in-list (D-groups (data)))])
      (dbg "Group ~a:\n  cand\tcost\tcompl.\tinv.expr.\tinv.vars\n" (group-id g))
      (for ([i (in-list (sort (hash-keys (group-cost-map g)) <))])
        (define p (hash-ref (group-cost-map g) i))
        (dbg "  ~a\t~a\t~a\t~a\t~a\n" i (ccost-cost (cpair-cost p)) (ccost-complexity (cpair-cost p))
             (if (null? (cpair-inv-exprs p)) "NIL;" (string-append (string-join (map number->string (cpair-inv-exprs p)) ", ") ";"))
             (if (null? (cpair-inv-vars p)) "NIL;" (string-join (map number->string (cpair-inv-vars p)) ", "))))
      (dbg "\n"))
    (dbg "\n")))

;; determine_iv_cost
(define (determine-iv-cost! c)
  (define base (iv-base (cand-iv c)))
  (define-values (cb _ivs) (force-var-cost base))
  (define cost-base (if (zero? (ccost-cost cb)) 4 (ccost-cost cb)))
  (define cost-step (add-cost (D-speed (data)) (tmode (tree-type base))))
  (define cost0 (+ cost-step (adjust-setup-cost cost-base)))
  (define vb (cand-var-before c))
  (define cost1 (if (or (not (eq? (cand-pos c) 'original))
                        (not (ssa-name-var vb))
                        (tree-attr (ssa-name-var vb) 'artificial #f))
                    (add1 cost0) cost0))
  (define cost2 (if (and (eq? (cand-pos c) 'end) (empty-block? (ip-end-pos (D-loop (data))))) (add1 cost1) cost1))
  (set-cand-cost! c cost2)
  (set-cand-cost-step! c cost-step))

(define (determine-iv-costs!)
  (dbg "<Candidate Costs>:\n  cand\tcost\n")
  (for ([c (in-list (D-cands (data)))])
    (determine-iv-cost! c)
    (dbg "  ~a\t~a\n" (cand-id c) (cand-cost c)))
  (dbg "\n"))

;; ivopts_estimate_reg_pressure
(define (estimate-reg-pressure n-invs n-cands)
  (define speed (D-speed (data)))
  (define n-old (D-regs-used (data))) (define n-new (+ n-invs n-cands))
  (define needed (+ n-new n-old))
  (define avail (if (D-body-includes-call (data)) (- (target-avail-regs) (target-clobbered-regs)) (target-avail-regs)))
  (define cost
    (cond
      [(< (+ needed (target-res-regs)) avail) n-new]
      [(<= needed avail) (* (target-reg-cost speed) needed)]
      [(<= n-cands avail) (+ (* (target-reg-cost speed) avail) (* (target-spill-cost speed) (- needed avail)))]
      [else (+ (* (target-reg-cost speed) avail) (* (target-spill-cost speed) (- n-cands avail) 2)
               (* (target-spill-cost speed) (- needed n-cands)))]))
  (+ cost n-cands))

;; determine_set_costs
(define (determine-set-costs!)
  (define l (D-loop (data)))
  (define n1 (for/sum ([ph (in-list (bb-phis (Loop-header l)))])
               (define op (gphi-result ph))
               (if (or (ssa-virtual? op) (get-iv op) (not (or (ty-ptr? (tree-type op)) (ty-int? (tree-type op))))) 0 1)))
  (define n2 (for/sum ([n (in-list (relevant-versions))])
               (define info (ver-info n))
               (if (and (positive? (vinfo-inv-id info)) (vinfo-has-nonlin-use info)) 1 0)))
  (set-D-regs-used! (data) (+ n1 n2))
  (dbg "<Global Costs>:\n  target_avail_regs ~a\n  target_clobbered_regs ~a\n  target_reg_cost ~a\n  target_spill_cost ~a\n  regs_used ~a\n"
       (target-avail-regs) (target-clobbered-regs) (target-reg-cost (D-speed (data))) (target-spill-cost (D-speed (data))) (+ n1 n2))
  (dbg "\n"))

;; ------------------------------------------------------------
;; the set of candidates (class iv_ca)
;; ------------------------------------------------------------

(struct ivca (upto bad-groups cand-for-group n-cand-uses cands n-cands n-invs cand-use-cost cand-cost
                   n-inv-var-uses n-inv-expr-uses cost)
  #:mutable)
;; a delta: list of (vector group old-cp new-cp), applied in order
(define (iv-ca-new)
  (ivca 0 0 (make-vector (length (D-groups (data))) #f) (make-vector (length (D-cands (data))) 0) (make-hasheqv)
        0 0 no-cost 0 (make-vector (add1 (D-max-inv-var (data))) 0) (make-vector (add1 (D-max-inv-expr (data))) 0) no-cost))
(define (ivca-cands-sorted s) (sort (hash-keys (ivca-cands s)) <))

;; cheaper_cost_pair
(define (cheaper-cost-pair? a b)
  (cond
    [(not a) #f]
    [(not b) #t]
    [(c< (cpair-cost a) (cpair-cost b)) #t]
    [(c< (cpair-cost b) (cpair-cost a)) #f]
    [else (< (cand-cost (cpair-cand a)) (cand-cost (cpair-cand b)))]))
(define (compare-cost-pair a b)
  (cond [(cheaper-cost-pair? a b) -1] [(cheaper-cost-pair? b a) 1] [else 0]))

(define (iv-ca-cand-for-group s g) (vector-ref (ivca-cand-for-group s) (group-id g)))

(define (iv-ca-recount-cost! s)
  (set-ivca-cost! s (c+n (c+n (ivca-cand-use-cost s) (ivca-cand-cost s))
                         (estimate-reg-pressure (ivca-n-invs s) (ivca-n-cands s)))))

(define (remove-invs! s invs counts)
  (for ([i (in-list invs)])
    (vector-set! counts i (sub1 (vector-ref counts i)))
    (when (zero? (vector-ref counts i)) (set-ivca-n-invs! s (sub1 (ivca-n-invs s))))))
(define (add-invs! s invs counts)
  (for ([i (in-list invs)])
    (vector-set! counts i (add1 (vector-ref counts i)))
    (when (= 1 (vector-ref counts i)) (set-ivca-n-invs! s (add1 (ivca-n-invs s))))))

;; iv_ca_set_no_cp
(define (iv-ca-set-no-cp! s g)
  (define cp (iv-ca-cand-for-group s g))
  (when cp
    (define c (cpair-cand cp)) (define cid (cand-id c))
    (set-ivca-bad-groups! s (add1 (ivca-bad-groups s)))
    (vector-set! (ivca-cand-for-group s) (group-id g) #f)
    (vector-set! (ivca-n-cand-uses s) cid (sub1 (vector-ref (ivca-n-cand-uses s) cid)))
    (when (zero? (vector-ref (ivca-n-cand-uses s) cid))
      (hash-remove! (ivca-cands s) cid)
      (set-ivca-n-cands! s (sub1 (ivca-n-cands s)))
      (set-ivca-cand-cost! s (- (ivca-cand-cost s) (cand-cost c)))
      (remove-invs! s (cand-inv-vars c) (ivca-n-inv-var-uses s))
      (remove-invs! s (cand-inv-exprs c) (ivca-n-inv-expr-uses s)))
    (set-ivca-cand-use-cost! s (c- (ivca-cand-use-cost s) (cpair-cost cp)))
    (remove-invs! s (cpair-inv-vars cp) (ivca-n-inv-var-uses s))
    (remove-invs! s (cpair-inv-exprs cp) (ivca-n-inv-expr-uses s))
    (iv-ca-recount-cost! s)))

;; iv_ca_set_cp
(define (iv-ca-set-cp! s g cp)
  (unless (eq? (iv-ca-cand-for-group s g) cp)
    (when (iv-ca-cand-for-group s g) (iv-ca-set-no-cp! s g))
    (when cp
      (define c (cpair-cand cp)) (define cid (cand-id c))
      (set-ivca-bad-groups! s (sub1 (ivca-bad-groups s)))
      (vector-set! (ivca-cand-for-group s) (group-id g) cp)
      (vector-set! (ivca-n-cand-uses s) cid (add1 (vector-ref (ivca-n-cand-uses s) cid)))
      (when (= 1 (vector-ref (ivca-n-cand-uses s) cid))
        (hash-set! (ivca-cands s) cid #t)
        (set-ivca-n-cands! s (add1 (ivca-n-cands s)))
        (set-ivca-cand-cost! s (+ (ivca-cand-cost s) (cand-cost c)))
        (add-invs! s (cand-inv-vars c) (ivca-n-inv-var-uses s))
        (add-invs! s (cand-inv-exprs c) (ivca-n-inv-expr-uses s)))
      (set-ivca-cand-use-cost! s (c+ (ivca-cand-use-cost s) (cpair-cost cp)))
      (add-invs! s (cpair-inv-vars cp) (ivca-n-inv-var-uses s))
      (add-invs! s (cpair-inv-exprs cp) (ivca-n-inv-expr-uses s))
      (iv-ca-recount-cost! s))))

;; iv_ca_add_group
(define (iv-ca-add-group! s g)
  (set-ivca-upto! s (add1 (ivca-upto s)))
  (set-ivca-bad-groups! s (add1 (ivca-bad-groups s)))
  (define (best-of ids best)
    (for/fold ([best best]) ([i (in-list ids)])
      (define cp (get-group-iv-cost g (list-ref (D-cands (data)) i)))
      (if (cheaper-cost-pair? cp best) cp best)))
  (define b1 (best-of (ivca-cands-sorted s) #f))
  (define b2 (or b1 (best-of (D-important (data)) #f)))
  (iv-ca-set-cp! s g b2))

(define (iv-ca-cost s) (if (positive? (ivca-bad-groups s)) infinite-cost (ivca-cost s)))

;; iv_ca_compare_deps
(define (iv-ca-compare-deps s g old new)
  (define o (ivca-n-invs s))
  (iv-ca-set-cp! s g new)
  (define n (ivca-n-invs s))
  (iv-ca-set-cp! s g old)
  (cond [(> n o) 1] [(< n o) -1] [else 0]))

;; deltas: lists of (vector group old new); iv_ca_delta_add conses at the head
(define (delta-add g old new rest) (cons (vector g old new) rest))
(define (delta-commit! s delta forward)
  (define d (if forward delta (for/list ([x (in-list (reverse delta))]) (vector (vector-ref x 0) (vector-ref x 2) (vector-ref x 1)))))
  (for ([x (in-list d)]) (iv-ca-set-cp! s (vector-ref x 0) (vector-ref x 2))))

(define (iv-ca-cand-used? s c) (positive? (vector-ref (ivca-n-cand-uses s) (cand-id c))))

;; iv_ca_extend: (values cost delta n-ivs)
(define (iv-ca-extend s c min-ncand)
  (define delta
    (for/fold ([delta '()]) ([i (in-range (ivca-upto s))])
      (define g (list-ref (D-groups (data)) i))
      (define old (iv-ca-cand-for-group s g))
      (cond
        [(and old (eq? (cpair-cand old) c)) delta]
        [else
         (define new (get-group-iv-cost g c))
         (cond
           [(not new) delta]
           [(not min-ncand)
            (define cmp-invs (iv-ca-compare-deps s g old new))
            (define cmp-cost (compare-cost-pair new old))
            (if (or (> cmp-invs 0) (> cmp-cost 0) (and (= cmp-cost 0) (= cmp-invs 0)))
                delta
                (delta-add g old new delta))]
           [else (delta-add g old new delta)])])))
  (delta-commit! s delta #t)
  (define cost (iv-ca-cost s))
  (define n (ivca-n-cands s))
  (delta-commit! s delta #f)
  (values cost delta n))

;; iv_ca_narrow: (values cost delta)
(define (iv-ca-narrow s c start)
  (let/ec return
    (define delta
      (for/fold ([delta '()]) ([g (in-list (D-groups (data)))])
        (define old (iv-ca-cand-for-group s g))
        (cond
          [(not (eq? (cpair-cand old) c)) delta]
          [else
           (define best-cost (iv-ca-cost s))
           (define new0 (get-group-iv-cost g start))
           (define new
             (for/fold ([new new0] [best best-cost] #:result new)
                       ([ci (in-list (if (D-consider-all (data))
                                         (ivca-cands-sorted s)
                                         (for/list ([i (in-list (ivca-cands-sorted s))] #:when (hash-ref (group-related g) i #f)) i)))])
               (cond
                 [(or (= ci (cand-id c)) (and start (= ci (cand-id start)))) (values new best)]
                 [else
                  (define cp (get-group-iv-cost g (list-ref (D-cands (data)) ci)))
                  (cond
                    [(not cp) (values new best)]
                    [else
                     (iv-ca-set-cp! s g cp)
                     (define ac (iv-ca-cost s))
                     (if (c< ac best) (values cp ac) (values new best))])])))
           (iv-ca-set-cp! s g old)
           (unless new (return infinite-cost '()))
           (delta-add g old new delta)])))
    (delta-commit! s delta #t)
    (define cost (iv-ca-cost s))
    (delta-commit! s delta #f)
    (values cost delta)))

;; iv_ca_prune: (values cost delta)
(define (iv-ca-prune s except)
  (define-values (best-cost best-delta)
    (for/fold ([best (iv-ca-cost s)] [bd #f]) ([i (in-list (ivca-cands-sorted s))])
      (define c (list-ref (D-cands (data)) i))
      (cond
        [(eq? c except) (values best bd)]
        [else
         (define-values (ac ad) (iv-ca-narrow s c except))
         (if (c< ac best) (values ac ad) (values best bd))])))
  (cond
    [(not best-delta) (values best-cost '())]
    [else
     (delta-commit! s best-delta #t)
     (define-values (cost d2) (iv-ca-prune s except))
     (delta-commit! s best-delta #f)
     (values cost (append best-delta d2))]))

;; iv_ca_replace: (values cost delta)
(define (iv-ca-replace s)
  (define orig (iv-ca-cost s))
  (let/ec return
    (for ([i (in-list (ivca-cands-sorted s))])
      (define n (vector-ref (ivca-n-cand-uses s) i))
      (unless (or (= n 1) (> n 10))
        (define c (list-ref (D-cands (data)) i))
        (define act
          (for/fold ([act '()]) ([j (in-range (ivca-upto s))])
            (define g (list-ref (D-groups (data)) j))
            (define old (iv-ca-cand-for-group s g))
            (cond
              [(not (eq? (cpair-cand old) c)) act]
              [else
               (define ks (if (D-consider-all (data)) (range (length (D-cands (data)))) (sort (hash-keys (group-related g)) <)))
               (define best
                 (for/fold ([best old]) ([k (in-list ks)])
                   (cond
                     [(= k (cand-id (cpair-cand old))) best]
                     [else
                      (define cp (get-group-iv-cost g (list-ref (D-cands (data)) k)))
                      (if (and cp (cheaper-cost-pair? cp best)) cp best)])))
               (if (eq? best old) act (delta-add g old best act))])))
        (unless (null? act)
          (delta-commit! s act #t)
          (define-values (ac td) (iv-ca-prune s #f))
          (delta-commit! s act #f)
          (when (c< ac orig) (return ac (append act td))))))
    (values orig '())))

;; try_add_cand_for
(define (try-add-cand-for! s g originalp)
  (iv-ca-add-group! s g)
  (define best-cost0 (iv-ca-cost s))
  (define cp0 (iv-ca-cand-for-group s g))
  (define best-delta0 (if cp0 (delta-add g #f cp0 '()) '()))
  (when cp0 (iv-ca-set-no-cp! s g))
  (define-values (best-cost best-delta)
    (for/fold ([bc best-cost0] [bd best-delta0]) ([i (in-list (sort (hash-keys (group-related g)) <))])
      (define c (list-ref (D-cands (data)) i))
      (cond
        [(and originalp (not (eq? (cand-pos c) 'original))) (values bc bd)]
        [(and (not originalp) (iv-base-object (cand-iv c))) (values bc bd)]
        [(iv-ca-cand-used? s c) (values bc bd)]
        [else
         (define cp (get-group-iv-cost g c))
         (cond
           [(not cp) (values bc bd)]
           [else
            (iv-ca-set-cp! s g cp)
            (define-values (ac ad _n) (iv-ca-extend s c #t))
            (iv-ca-set-no-cp! s g)
            (define ad2 (delta-add g #f cp ad))
            (if (c< ac bc) (values ac ad2) (values bc bd))])])))
  (define-values (best-cost2 best-delta2)
    (cond
      [(inf? best-cost)
       (for/fold ([bc best-cost] [bd best-delta]) ([i (in-list (sort (hash-keys (group-cost-map g)) <))])
         (define cp (hash-ref (group-cost-map g) i))
         (define c (cpair-cand cp))
         (cond
           [(and (cand-important c) originalp (eq? (cand-pos c) 'original)) (values bc bd)]
           [(and (cand-important c) (not originalp) (not (iv-base-object (cand-iv c)))) (values bc bd)]
           [(iv-ca-cand-used? s c) (values bc bd)]
           [else
            (iv-ca-set-cp! s g cp)
            (define-values (ac ad _n) (iv-ca-extend s c #t))
            (iv-ca-set-no-cp! s g)
            (define ad2 (delta-add g (iv-ca-cand-for-group s g) cp ad))
            (if (c< ac bc) (values ac ad2) (values bc bd))]))]
      [else (values best-cost best-delta)]))
  (delta-commit! s best-delta2 #t)
  (not (inf? best-cost2)))

(define (get-initial-solution originalp)
  (define s (iv-ca-new))
  (and (for/and ([g (in-list (D-groups (data)))]) (try-add-cand-for! s g originalp)) s))

;; try_improve_iv_set: (values improved? try-replace)
(define (try-improve-iv-set! s try-replace)
  (define best-cost0 (iv-ca-cost s))
  (define-values (best-cost best-delta)
    (for/fold ([bc best-cost0] [bd #f]) ([c (in-list (D-cands (data)))])
      (cond
        [(iv-ca-cand-used? s c) (values bc bd)]
        [else
         (define-values (ac0 ad0 n) (iv-ca-extend s c #f))
         (cond
           [(null? ad0) (values bc bd)]
           [else
            (define-values (ac ad)
              (cond
                [(<= n 10)
                 (delta-commit! s ad0 #t)
                 (define-values (pc pd) (iv-ca-prune s c))
                 (delta-commit! s ad0 #f)
                 (values pc (append ad0 pd))]
                [else (values ac0 ad0)]))
            (if (c< ac bc) (values ac ad) (values bc bd))])])))
  (cond
    [best-delta
     (delta-commit! s best-delta #t)
     (values (c= best-cost (iv-ca-cost s)) try-replace)]
    [else
     (define-values (pc pd) (iv-ca-prune s #f))
     (define-values (bc bd tr)
       (cond
         [(and (null? pd) try-replace)
          (define-values (rc rd) (iv-ca-replace s))
          (values rc rd #f)]
         [else (values pc pd try-replace)]))
     (cond
       [(null? bd) (values #f tr)]
       [else
        (delta-commit! s bd #t)
        (values (c= bc (iv-ca-cost s)) tr)])]))

(define (find-optimal-iv-set-1 originalp)
  (define s (get-initial-solution originalp))
  (cond
    [(not s) (dbg "Unable to substitute for ivs, failed.\n") #f]
    [else
     (when debug? (dbg "Initial set of candidates:\n") (iv-ca-dump s))
     (let loop ([tr #t])
       (define-values (improved tr2) (try-improve-iv-set! s tr))
       (when improved
         (when debug? (dbg "Improved to:\n") (iv-ca-dump s))
         (loop tr2)))
     (if (inf? (iv-ca-cost s)) #f s)]))

(define (find-optimal-iv-set)
  (define origset (find-optimal-iv-set-1 #t))
  (define set (find-optimal-iv-set-1 #f))
  (cond
    [(and (not origset) (not set)) #f]
    [else
     (define oc (if origset (iv-ca-cost origset) infinite-cost))
     (define c (if set (iv-ca-cost set) infinite-cost))
     (dbg "Original cost ~a (complexity ~a)\n\nFinal cost ~a (complexity ~a)\n\n" (ccost-cost oc) (ccost-complexity oc) (ccost-cost c) (ccost-complexity c))
     (define best (if (c<= oc c) origset set))
     (for ([g (in-list (D-groups (data)))]) (set-group-selected! g (cpair-cand (iv-ca-cand-for-group best g))))
     best]))

(define (iv-ca-dump s)
  (define c (iv-ca-cost s))
  (dbg "  cost: ~a (complexity ~a)\n  reg_cost: ~a\n  cand_cost: ~a\n  cand_group_cost: ~a (complexity ~a)\n  candidates: ~a\n"
       (ccost-cost c) (ccost-complexity c) (estimate-reg-pressure (ivca-n-invs s) (ivca-n-cands s))
       (ivca-cand-cost s) (ccost-cost (ivca-cand-use-cost s)) (ccost-complexity (ivca-cand-use-cost s))
       (string-join (map number->string (ivca-cands-sorted s)) ", "))
  (for ([i (in-range (ivca-upto s))])
    (define g (list-ref (D-groups (data)) i))
    (define cp (iv-ca-cand-for-group s g))
    (if cp
        (dbg "   group:~a --> iv_cand:~a, cost=(~a,~a)\n" i (cand-id (cpair-cand cp)) (ccost-cost (cpair-cost cp)) (ccost-complexity (cpair-cost cp)))
        (dbg "   group:~a --> ??\n" i)))
  (dbg "  invariant variables: ~a\n  invariant expressions: ~a\n\n"
       (string-join (for/list ([i (in-range 1 (vector-length (ivca-n-inv-var-uses s)))] #:when (positive? (vector-ref (ivca-n-inv-var-uses s) i))) (number->string i)) ", ")
       (string-join (for/list ([i (in-range 1 (vector-length (ivca-n-inv-expr-uses s)))] #:when (positive? (vector-ref (ivca-n-inv-expr-uses s) i))) (number->string i)) ", ")))

;; ------------------------------------------------------------
;; rewriting
;; ------------------------------------------------------------
;; The rewrite phase edits RW (a box holding the cfg); statements are
;; found by identity (eq?) and every replacement updates the uses and
;; candidates that point at the old statement.

(define rw (make-parameter #f))
(define stamp-box (make-parameter #f))
(define (rcfg) (unbox (rw)))
(define (rcfg! c) (set-box! (rw) c))
(define (new-stamp! old new)
  (define-values (g n) (restamp old new (unbox (stamp-box))))
  (set-box! (stamp-box) n)
  g)

(define (rw-stmts bb) (pvector->list (GBlock-stmts (cfg-block (rcfg) bb))))
(define (rw-set-stmts! bb lst) (rcfg! (set-block (rcfg) (block-set-stmts (cfg-block (rcfg) bb) (list->pvector lst)))))

;; the statement G changed into NEW: references follow
(define (stmt-replaced! old new)
  (define bb (hash-ref (D-stmt-bb (data)) old #f))
  (define uid (hash-ref (D-stmt-uid (data)) old #f))
  (when bb (hash-set! (D-stmt-bb (data)) new bb) (hash-set! (D-stmt-uid (data)) new uid))
  (for* ([g (in-list (D-groups (data)))] [u (in-list (group-uses g))] #:when (eq? (use-stmt u) old))
    (set-use-stmt! u new))
  (for ([c (in-list (D-cands (data)))] #:when (eq? (cand-incremented-at c) old))
    (set-cand-incremented-at! c new)))

(define (replace-stmt! bb old new0)
  (define new (new-stamp! old new0))
  (rw-set-stmts! bb (for/list ([g (in-list (rw-stmts bb))]) (if (eq? g old) new g)))
  (stmt-replaced! old new)
  new)
(define (insert-before! bb at gs*)
  (define stamped (for/list ([g (in-list gs*)]) (new-stamp! #f g)))
  (rw-set-stmts! bb (append* (for/list ([g (in-list (rw-stmts bb))]) (if (eq? g at) (append stamped (list g)) (list g)))))
  (for ([g (in-list stamped)]) (hash-set! (D-stmt-bb (data)) g bb) (hash-set! (D-stmt-uid (data)) g 0)))

;; gsi_insert_seq_on_edge_immediate (the edge needs no split on the fixtures)
(define (insert-on-edge! src dest gs*)
  (unless (null? gs*)
    (define stamped (for/list ([g (in-list gs*)]) (new-stamp! #f g)))
    (define dpreds (cfg-preds (rcfg) dest))
    (cond
      [(and (= 1 (pvector-length dpreds)) (null? (pvector->list (GBlock-phis (cfg-block (rcfg) dest)))) (not (= dest EXIT)))
       (define ss (rw-stmts dest))
       (define-values (labels rest) (splitf-at ss glabel?))
       (rw-set-stmts! dest (append labels stamped rest))]
      [(and (= 1 (pvector-length (cfg-succs (rcfg) src))) (not (= src ENTRY))
            (let ([l (let ([s (rw-stmts src)]) (and (pair? s) (last s)))]) (or (not l) (not (stmt-ends-bb? l)))))
       (rw-set-stmts! src (append (rw-stmts src) stamped))]
      [else (error 'ivopts "inserting on an edge that needs splitting is not ported")])))

(define (build-assign lhs e)
  (define code (tree-code e))
  (cond
    [(memq (gimple-rhs-class (gs) code) '(single)) (gassign #f (hasheq) lhs code e #f #f)]
    [(= (tree-operand-length e) 1) (gassign #f (hasheq) lhs code (tree-operand e 0) #f #f)]
    [(= (tree-operand-length e) 2) (gassign #f (hasheq) lhs code (tree-operand e 0) (tree-operand e 1) #f)]
    [else (gassign #f (hasheq) lhs code (tree-operand e 0) (tree-operand e 1) (tree-operand e 2))]))

(define (new-ssa type-or-var)
  (let-values ([(n st) (make-ssa-name (gs) type-or-var)]) (gs! st) n))

;; force_gimple_operand (EXPR, &stmts, true, VAR) with a predicate:
;; (values val stmts); VAR names the result (made before the operands)
(define (force-gimple-operand* e var [pred (lambda (x) (is-gimple-val (gs) x))])
  (cond
    [(and (is-gimple-val (gs) e) (pred e)) (values e '())]
    [else
     (define result (and var (new-ssa var)))
     (define pre '())
     (define (val! x)
       (cond
         [(is-gimple-val (gs) x) x]
         [else
          (define r (rhs! x))
          (define n (new-ssa (tree-type x)))
          (set! pre (cons (build-assign n r) pre))
          n]))
     (define (rhs! x)
       (if (is-gimple-val (gs) x)
           x
           (make-tree (tree-code x) #:type (tree-type x)
                      #:ops (for/list ([i (in-range (tree-operand-length x))]) (val! (tree-operand x i))))))
     (cond
       [result
        (define r (rhs! e))
        (values result (reverse (cons (build-assign result r) pre)))]
       [else
        (define v (val! e))
        (values v (reverse pre))])]))

;; create_iv (BASE, PLUS_EXPR, STEP, VAR, loop, INCR-AT / AFTER):
;; (values var-before var-after)
(define (create-iv! base step0 var incr-bb incr-before after?)
  (define l (D-loop (data)))
  (define vb (new-ssa var)) (define va (new-ssa var))
  (define-values (op step1)
    (cond
      [(icst? step0)
       (cond
         [(ty-uns? (tree-type step0))
          (define s1 (gf-build1 'negate_expr (tree-type step0) step0))
          (if (< (icst-val s1) (icst-val step0)) (values 'minus_expr s1) (values 'plus_expr step0))]
         [(< (icst-val step0) 0) (values 'minus_expr (gf-build1 'negate_expr (tree-type step0) step0))]
         [else (values 'plus_expr step0)])]
      [else (values 'plus_expr step0)]))
  (define-values (op2 step2)
    (if (ty-ptr? (tree-type base))
        (let ([s (gf-convert (sizetype (gs)) step1)])
          (values 'pointer_plus_expr (if (eq? op 'minus_expr) (gf-build1 'negate_expr (tree-type s) s) s)))
        (values op step1)))
  (define-values (step sstmts) (force-gimple-operand* step2 #f))
  (insert-on-edge! (preheader-src) (Loop-header l) sstmts)
  (define incr (gassign #f (hasheq) va op2 vb step #f))
  (cond
    [after? (rw-set-stmts! incr-bb (append (rw-stmts incr-bb) (list (new-stamp! #f incr))))]
    [else (insert-before! incr-bb incr-before (list incr))])
  (hash-set! (D-stmt-bb (data)) incr incr-bb)
  (define-values (initial istmts) (force-gimple-operand* base var))
  (insert-on-edge! (preheader-src) (Loop-header l) istmts)
  (define h (Loop-header l))
  (define preds (cfg-preds (rcfg) h))
  (define args (for/pvector ([p (in-pvector preds)]) (cons p (if (= p (latch)) va initial))))
  (define phi (new-stamp! #f (gphi #f (hasheq) #f vb args)))
  (define b (cfg-block (rcfg) h))
  (rcfg! (set-block (rcfg) (block-set-phis b (pvector-cons-right (GBlock-phis b) phi))))
  (values vb va))

;; the incrementing statement of VA (the last one inserted with that lhs)
(define (find-def-stmt-rw v)
  (for*/first ([bb (in-list (D-body (data)))] [g (in-list (rw-stmts bb))]
               #:when (and (gassign? g) (eq? (tree-code (gassign-lhs g)) 'ssa_name)
                           (= (ssa-name-version (gassign-lhs g)) (ssa-name-version v))))
    (cons bb g)))

;; create_new_iv
(define (create-new-iv! c)
  (define l (D-loop (data)))
  (case (cand-pos c)
    [(original)
     (set-vinfo-preserve-biv! (name-info (cand-var-before c)) #t)
     (set-vinfo-preserve-biv! (name-info (cand-var-after c)) #t)
     (define u (find-interesting-uses-op! (cand-var-after c)))
     (set-group-selected! (list-ref (D-groups (data)) (use-group-id u)) c)]
    [else
     (define var (cand-var-before c))
     (gs! (gimple-add-tmp-var (gs) var))
     (define-values (bb before after?)
       (case (cand-pos c)
         [(normal) (let ([bb (ip-normal-pos l)]) (values bb (last (rw-stmts bb)) #f))]
         [(end) (values (ip-end-pos l) #f #t)]
         ;; IP_BEFORE_USE / IP_AFTER_USE exist only with autoincrement
         ;; addressing (use-autoinc?), which aarch64 does not enable
         [else (error 'ivopts "autoincrement positions: the target enables no autoincrement")]))
     (define-values (vb va) (create-iv! (iv-base (cand-iv c)) (iv-step (cand-iv c)) var bb before after?))
     (set-cand-var-before! c vb) (set-cand-var-after! c va)]))

;; rewrite_use_nonlinear_expr
(define (rewrite-use-nonlinear! u c)
  (define g (use-stmt u))
  (define l (D-loop (data)))
  (let/ec return
    (when (and (eq? (cand-pos c) 'original) (eq? (cand-incremented-at c) g))
      (define op
        (and (memq (gassign-code g) '(plus_expr minus_expr pointer_plus_expr))
             (cond
               [(operand-equal? (gassign-rhs1 g) (cand-var-before c)) (gassign-rhs2 g)]
               [(operand-equal? (gassign-rhs2 g) (cand-var-before c)) (gassign-rhs1 g)]
               [else #f])))
      (when op
        (when (expr-invariant-in-loop? l op) (return (void)))
        (when (eq? (tree-code op) 'ssa_name)
          (define x (get-iv op))
          (when (and x (integer-zerop (iv-step x))) (return (void))))))
    ;; a PHI: a biv kept by a selected original candidate is not replaced
    (when (and (gphi? g) (vinfo-preserve-biv (name-info (gphi-result g)))) (return (void)))
    (error 'ivopts "rewriting a nonlinear use by another candidate (the recomputation of rewrite_use_nonlinear_expr) is not ported")))

;; adjust_iv_update_pos
(define (adjust-iv-update-pos! c u)
  (when (eq? (cand-pos c) 'normal)
    (define p (find-def-stmt-rw (cand-var-after c)))
    (define bb (car p)) (define iv-update (cdr p))
    (define ss (rw-stmts bb))
    (define n (length ss))
    (when (and (>= n 3) (gcond? (list-ref ss (- n 1))) (eq? (list-ref ss (- n 2)) iv-update))
      (define g (list-ref ss (- n 3)))
      (when (and (gassign? g) (eq? g (use-stmt u)) (eq? (tree-code (gassign-lhs g)) 'ssa_name))
        (dbg "Reordering \n~a\n~a\n\n" (sstr iv-update) (sstr g))
        (rw-set-stmts! bb (append (take ss (- n 3)) (list iv-update g (list-ref ss (- n 1)))))
        (set-cand-pos! c 'before-use)
        (set-cand-incremented-at! c g)))))

;; create_mem_ref (tree-ssa-address.cc), the paths taken on the fixtures:
;; the address parts, gimplified, as a TARGET_MEM_REF
(define (create-mem-ref! bb at type addr0 alias-ptr-type iv-cand base-hint)
  (define st (sizetype (gs)))
  (define addr addr0)
  (define offset (if (zero? (aff-offset addr)) #f (aff-offset addr)))
  (define base #f) (define index #f) (define step #f)
  ;; move_hint_to_base / move_variant_to_index
  (cond
    [base-hint
     (define i (index-where (aff-elts addr) (lambda (p) (and (= 1 (cdr p)) (operand-equal? (car p) base-hint)))))
     (when i
       (set! base (gf-convert (let-values ([(pt s) (pointer-to (gs) (void-type (gs)))]) (gs! s) pt) (car (list-ref (aff-elts addr) i))))
       (set! addr (aff-remove-elt addr i)))]
    [else
     (define i (index-where (aff-elts addr) (lambda (p) (operand-equal? (car p) iv-cand))))
     (when i
       (set! index (gf-convert st (car (list-ref (aff-elts addr) i))))
       (set! step (cdr (list-ref (aff-elts addr) i)))
       (set! addr (aff-remove-elt addr i)))])
  (unless (null? (aff-elts addr))
    (error 'ivopts "create_mem_ref: remaining address elements are not ported"))
  ;; gimplify_mem_ref_parts
  (define pre '())
  (when base
    (define-values (v s) (force-gimple-operand* base #f (lambda (x) (eq? (tree-code x) 'ssa_name))))
    (set! pre (append pre s)) (set! base v))
  (when index
    (define-values (v s) (force-gimple-operand* index #f))
    (set! pre (append pre s)) (set! index v))
  (define parts (mem-parts #f (and base #t) (and index #t) (and step (not (= step 1)) step) offset))
  (unless (memory-address-valid? (machine-mode-of type) parts)
    (error 'ivopts "create_mem_ref: the simplification of an invalid address is not ported"))
  (insert-before! bb at pre)
  (define off (icst alias-ptr-type (or offset 0)))
  (cond
    [(and base (ty-ptr? (tree-type base)))
     (make-tree 'target_mem_ref #:type type
                #:ops (list base off (and index index) (and index step (not (= step 1)) (icst st step)) #f))]
    [else (error 'ivopts "create_mem_ref: a base that is not a pointer is not ported")]))

;; rewrite_use_address
(define (rewrite-use-address! u c)
  (adjust-iv-update-pos! c u)
  (define g (use-stmt u))
  (define a (get-computation-aff g u c))
  (define v (var-at-stmt c g))
  (define hint (and (iv-base-object (cand-iv c)) v))
  (define ref (stmt-operand g (use-op u)))
  (define alias (let-values ([(pt s) (pointer-to (gs) (type-main-variant (gs) (tree-type ref)))]) (gs! s) pt))
  (define bb (stmt-bb g))
  (define new-ref (create-mem-ref! bb g (use-mem-type u) a alias v hint))
  (define g2 (set-stmt-operand g (use-op u) new-ref))
  (replace-stmt! bb g g2))

(define (set-stmt-operand g p v)
  (case p
    [(rhs1) (gassign (gassign-loc g) (gassign-flags g) (gassign-lhs g)
                     (if (eq? (gimple-rhs-class (gs) (gassign-code g)) 'single) (tree-code v) (gassign-code g))
                     v (gassign-rhs2 g) (gassign-rhs3 g))]
    [(lhs) (gassign (gassign-loc g) (gassign-flags g) v (gassign-code g) (gassign-rhs1 g) (gassign-rhs2 g) (gassign-rhs3 g))]
    [(cond-lhs) (gcond (gcond-loc g) (gcond-flags g) (gcond-code g) v (gcond-rhs g) (gcond-true-label g) (gcond-false-label g))]
    [(cond-rhs) (gcond (gcond-loc g) (gcond-flags g) (gcond-code g) (gcond-lhs g) v (gcond-true-label g) (gcond-false-label g))]))

;; rewrite_use_compare
(define (rewrite-use-compare! u c)
  (define g (use-stmt u))
  (define bb (stmt-bb g))
  (define grp (list-ref (D-groups (data)) (use-group-id u)))
  (define cp (get-group-iv-cost grp c))
  (cond
    [(cpair-value cp)
     (define v (var-at-stmt c g))
     (dbg "Replacing exit test: ~a\n" (sstr g))
     (define bound (gf-convert (tree-type v) (cpair-value cp)))
     (define-values (op stmts) (force-gimple-operand* bound #f))
     (insert-on-edge! (preheader-src) (Loop-header (D-loop (data))) stmts)
     (replace-stmt! bb g (gcond (gcond-loc g) (gcond-flags g) (cpair-comp cp) v op (gcond-true-label g) (gcond-false-label g)))]
    [else
     (define comp (get-computation-at g u c))
     (define old (stmt-operand g (use-op u)))
     (define-values (op stmts) (force-gimple-operand* comp (ssa-name-var old)))
     (insert-before! bb g stmts)
     (replace-stmt! bb g (set-stmt-operand g (use-op u) op))]))

(define (rewrite-groups!)
  (for ([g (in-list (D-groups (data)))])
    (define c (group-selected g))
    (for ([u (in-list (group-uses g))])
      (case (group-type g)
        [(nonlinear) (rewrite-use-nonlinear! u c)]
        [(ref-address ptr-address) (rewrite-use-address! u c)]
        [(compare) (rewrite-use-compare! u c)]))))

;; remove_unused_ivs: the versions to release
(define (remove-unused-ivs!)
  (for ([n (in-list (relevant-versions))])
    (define info (ver-info n))
    (define x (vinfo-iv info))
    (when (and x (not (integer-zerop (iv-step x))) (zero? (vinfo-inv-id info)) (not (iv-nonlin-use x)) (not (vinfo-preserve-biv info)))
      (set-box! (D-toremove (data)) (cons (iv-ssa-name x) (unbox (D-toremove (data))))))))

;; release_defs_bitset: from the highest version down, a definition is
;; removed once no other definition to remove uses it
(define (release-defs! cfg0 st0 names)
  (define cfg cfg0) (define st st0)
  (define pending (sort names > #:key ssa-name-version))
  (define (uses-of v)
    (for*/list ([bb (in-pvector (cfg-bbs cfg))] [g (in-pvector (GBlock-stmts (cfg-block cfg bb)))]
                #:when (for/or ([u (in-list (stmt-real-uses g))]) (and (eq? (tree-code u) 'ssa_name) (= (ssa-name-version u) (ssa-name-version v)))))
      g))
  (let loop ([pending pending])
    (unless (null? pending)
      (define todo (for/hasheqv ([p (in-list pending)]) (values (ssa-name-version p) #t)))
      (define v (for/first ([p (in-list pending)]
                            #:unless (for/or ([g (in-list (uses-of p))])
                                       (and (gassign? g) (eq? (tree-code (gassign-lhs g)) 'ssa_name)
                                            (hash-ref todo (ssa-name-version (gassign-lhs g)) #f))))
                  p))
      (define v* (or v (car pending)))
      (set! cfg
            (for/fold ([cfg cfg]) ([bb (in-pvector (cfg-bbs cfg))])
              (define b (cfg-block cfg bb))
              (set-block cfg (block-set-stmts b (for/pvector ([g (in-pvector (GBlock-stmts b))]
                                                              #:unless (and (gassign? g) (eq? (tree-code (gassign-lhs g)) 'ssa_name)
                                                                            (= (ssa-name-version (gassign-lhs g)) (ssa-name-version v*))))
                                                  g)))))
      (set! st (ssa-queue-release st (ssa-name-version v*)))
      (loop (remove v* pending eq?))))
  (values cfg st))

;; ------------------------------------------------------------
;; the pass
;; ------------------------------------------------------------

(define (ivopts-pass st0 fndecl cfg0)
  (define st (gs-set-fndecl st0 fndecl))
  (define cfg (cfg-clear-messages cfg0))
  (define headers
    (let-values ([(r s) (with-loops st fndecl cfg
                          (lambda () (for/list ([l (in-list (loops-innermost-first (ctx-loops)))]) (Loop-header l))))])
      r))
  (define toremove (box '()))
  (for ([h (in-list headers)])
    (define-values (res st1)
      (with-loops st fndecl cfg
        (lambda ()
          (define l (findf (lambda (l) (= (Loop-header l) h)) (ctx-loops)))
          (and l (optimize-loop l toremove)))))
    (set! st st1)
    (when res (set! cfg res)))
  (define-values (cfg2 st2) (release-defs! cfg st (unbox toremove)))
  (values cfg2 st2))


;; tree_ssa_iv_optimize_loop: the new cfg or #f
(define (optimize-loop l toremove)
  (define body (get-loop-body l))
  (define exit (let ([e (single-exit l)]) (and e (just-once-each-iteration? l (car e)) e)))
  (define d (D l #t (make-hasheqv) (make-hasheqv) '() '() '() 0 0 '() 0 0 body
               (loop-body-includes-call? body) (and exit (loop-only-exit? l body exit)) #t #f #f toremove))
  (parameterize ([cur-data d] [common-cands (box '())])
    (index-statements! body)
    (dbg "Processing loop ~a\n" (Loop-num l))
    (cond
      [(not (find-induction-variables!)) #f]
      [else
       (find-interesting-uses!)
       (cond
         [(> (length (D-groups d)) 250) #f]
         [else
          (dbg "Predict doloop failure due to target specific checks.\n")
          (find-iv-candidates!)
          (determine-iv-costs!)
          (determine-group-iv-costs!)
          (determine-set-costs!)
          (define set (find-optimal-iv-set))
          (cond
            [(not set) #f]
            [else
             (parameterize ([rw (box (ctx-cfg))] [stamp-box (box (cfg-next-stamp (ctx-cfg)))])
               (for ([i (in-list (ivca-cands-sorted set))]) (create-new-iv! (list-ref (D-cands d) i)))
               (when debug?
                 (dbg "Selected IV set for loop ~a, ~a avg niters, ~a IVs:\n" (Loop-num l) (avg-loop-niter) (length (ivca-cands-sorted set)))
                 (for ([i (in-list (ivca-cands-sorted set))]) (dump-cand (list-ref (D-cands d) i)))
                 (dbg "\n"))
               (rewrite-groups!)
               (remove-unused-ivs!)
               (rcfg))])])])))

;; loop_body_includes_call: a call that is not an inexpensive builtin
(define (loop-body-includes-call? body)
  (for*/or ([bb (in-list body)] [g (in-list (bb-stmts bb))]) (gcall? g)))

;; loop_only_exit_p: no statement before the exit may leave the loop
;; (calls that may not return)
(define (loop-only-exit? l body exit)
  (and (= 1 (length (loop-exit-edges l)))
       (not (for*/or ([bb (in-list body)] [g (in-list (bb-stmts bb))]) (gcall? g)))))
