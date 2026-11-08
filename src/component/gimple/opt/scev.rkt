#lang racket/base

;; ============================================================
;; opt: scalar evolutions and loop iteration counts
;; (gcc/tree-chrec.cc, gcc/tree-scalar-evolution.cc,
;;  gcc/tree-ssa-loop-niter.cc)
;; ============================================================
;;
;; A chain of recurrence {LEFT, +, RIGHT}_N is a tree with code
;; polynomial_chrec, operands (LEFT RIGHT), attribute 'var N (the loop
;; number) and the type of LEFT.  chrec_dont_know / chrec_known /
;; chrec_not_analyzed_yet are the symbols 'dont-know 'known 'not-analyzed.
;;
;;   (with-loops st fndecl cfg thunk) -> (values result st)
;;     the analysis context of one function (loop tree, dominators,
;;     SSA definitions, the scalar evolution cache, the iteration
;;     counts); the functions below run inside it
;;   (ctx-loops) (loop-num l) (loop-by-num n) (bb-loop bb)
;;   (single-exit l) -> (cons src dest) | #f
;;   (analyze-scalar-evolution l var), (instantiate-parameters l chrec),
;;   (analyze-scalar-evolution-in-loop wrto use var) -> (values ev folded-casts?)
;;   (compute-overall-effect-of-inner-loop l ev)
;;   (number-of-latch-executions l)
;;   (number-of-iterations-exit l exit) -> niter-desc | #f
;;   (simple-iv wrto use op allow-nonconstant-step?) -> affine-iv | #f
;;   chrec algebra: chrec-fold-plus/-minus/-multiply, chrec-convert,
;;   chrec-convert-rhs, chrec-apply, build-polynomial-chrec, ...
;;   (expression-expensive-p e) -> (values expensive? cond-overflow?)
;;
;; GCC 12 differences: chrec_apply has no "{a, +, a} (x-1) -> a*x" case
;; and computes "a + b*x" in the chrec's type; chrec_fold_plus_1 strips
;; sign conversions to unsigned for any converted operand;
;; chrec_fold_multiply neither swaps a chrec into the first operand nor
;; checks the step product for overflow.
;;
;; Not ported: peeled chrecs (simplify_peeled_chrec), condition PHIs
;; with a back edge, inner loop PHIs (multi-level nests), ADDR_EXPR and
;; BIT_AND_EXPR evolutions, the TDF_SCEV dumps, number_of_iterations_ne
;; with a non-unit step, the bit-count / popcount niters, simplification
;; by outer evolutions (-O3), scev_probably_wraps_p beyond the
;; nowrap_type_p / constant-iteration cases.
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../../../driver/dominance/dom.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         "../cfg.rkt"
         (only-in "../gimplify.rkt" is-gimple-min-invariant)
         "ssa-ops.rkt"
         (only-in "mfn.rkt" stmt-def-names)
         (only-in "range-info.rkt" gimple-range-global)
         (only-in "irange.rkt" ir-varying? ir-undefined? ir-lower ir-upper)
         (only-in "loop-bounds.rkt" loop-bound lbnd-upper)
         "generic-fold.rkt")

(provide with-loops ctx-loops ctx-cfg loop-num loop-by-num bb-loop loop-depth loop-outer nested-in? single-exit
         loop-exit-edges loop-preheader-src
         chrec-dont-know? auto-chrec? poly? chrec-var chrec-left chrec-right chrec-type
         build-polynomial-chrec chrec-fold-plus chrec-fold-minus chrec-fold-multiply chrec-convert chrec-convert-rhs
         chrec-apply chrec-contains-undetermined? tree-contains-chrecs? chrec-contains-symbols-defined-in-loop?
         evolution-function-is-affine? no-evolution-in-loop?
         analyze-scalar-evolution instantiate-parameters analyze-scalar-evolution-in-loop
         compute-overall-effect-of-inner-loop number-of-latch-executions
         (struct-out niter-desc) (struct-out affine-iv) number-of-iterations-exit simple-iv
         expression-expensive-p scev-reset!
         find-loop-niter find-loop-niter-by-eval just-once-each-iteration? loop-latch
         expand-simple-operations def-stmt-of def-bb-of dominated-by? superloop-at-depth convert-affine-scev
         min-invariant?
         ctx-decl current-loop-ctx loop-body-blocks number-of-iterations-exit-assumptions
         initial-condition evolution-part-in-loop-num initial-condition-in-loop-num stmt-loop-of-name
         iv-can-overflow?)

;; ------------------------------------------------------------
;; the context
;; ------------------------------------------------------------

(struct lctx (decl cfg dom loops def-bb def-stmt scev niters) #:mutable)
(define cur (make-parameter #f))
(define (C) (cur))
(define (major) (gcc-major-version (gs)))

(define (with-loops st decl cfg thunk)
  (with-gs (gs-set-fndecl st decl)
    (lambda ()
      (define dom (cfg-dom cfg))
      (define-values (loops _b) (find-loops cfg dom))
      (define def-bb (make-hasheqv)) (define def-stmt (make-hasheqv))
      (for ([bb (in-pvector (cfg-bbs cfg))])
        (define b (cfg-block cfg bb))
        (for ([ph (in-pvector (GBlock-phis b))])
          (hash-set! def-bb (ssa-name-version (gphi-result ph)) bb)
          (hash-set! def-stmt (ssa-name-version (gphi-result ph)) ph))
        (for* ([g (in-pvector (GBlock-stmts b))] [d (in-list (stmt-def-names g))])
          (hash-set! def-bb (ssa-name-version d) bb)
          (hash-set! def-stmt (ssa-name-version d) g)))
      (parameterize ([cur (lctx decl cfg dom loops def-bb def-stmt (make-hash) (make-hasheqv))])
        (thunk)))))
(define (scev-reset!) (set-lctx-scev! (C) (make-hash)) (set-lctx-niters! (C) (make-hasheqv)))

(define (ctx-loops) (lctx-loops (C)))
(define (ctx-decl) (lctx-decl (C)))
(define (current-loop-ctx) (C))
;; get_loop_body: dfs_enumerate_from (header, reverse) inside the loop
(define (loop-body-blocks l)
  (define cfg (lctx-cfg (C)))
  (define visited (make-hasheqv))
  (hash-set! visited (Loop-header l) #t)
  (let walk ([stack (list (Loop-header l))] [acc (list (Loop-header l))])
    (cond
      [(null? stack) (reverse acc)]
      [else
       (define-values (st2 acc2)
         (for/fold ([st (cdr stack)] [acc acc]) ([p (in-pvector (cfg-preds cfg (car stack)))]
                                                 #:when (and (not (hash-ref visited p #f)) (loop-inside? l p)))
           (hash-set! visited p #t)
           (values (cons p st) (cons p acc))))
       (walk st2 acc2)])))
(define (ctx-cfg) (lctx-cfg (C)))
;; loops: a Loop or #f for the function (loop 0)
(define (loop-num l) (if l (Loop-num l) 0))
(define (loop-by-num n) (and (> n 0) (findf (lambda (l) (= (Loop-num l) n)) (lctx-loops (C)))))
(define (bb-loop bb) (loop-of (lctx-loops (C)) bb))
(define (loop-depth l) (if l (Loop-depth l) 0))
(define (loop-outer l) (and l (Loop-parent l) (loop-by-num (Loop-parent l))))
(define (same-loop? a b) (= (loop-num a) (loop-num b)))
;; flow_loop_nested_p (OUTER, INNER): INNER strictly inside OUTER
(define (nested-in? outer inner)
  (and inner (let up ([x (loop-outer inner)]) (cond [(same-loop? x outer) #t] [(not x) #f] [else (up (loop-outer x))]))))
(define (inside? l bb) (or (not l) (loop-inside? l bb)))
(define (superloop-at-depth l d) (if (<= (loop-depth l) d) l (superloop-at-depth (loop-outer l) d)))
(define (find-common-loop a b)
  (let loop ([a a] [b b])
    (cond [(same-loop? a b) a]
          [(> (loop-depth a) (loop-depth b)) (loop (loop-outer a) b)]
          [else (loop a (loop-outer b))])))
(define (loop-exit-edges l)
  (for*/list ([src (in-list (intbits->list (Loop-body l)))]
              [e (in-pvector (cfg-succs (lctx-cfg (C)) src))]
              #:unless (loop-inside? l (GEdge-dest e)))
    (cons src (GEdge-dest e))))
(define (single-exit l)
  (define es (if l (loop-exit-edges l) '()))
  (and (= 1 (length es)) (car es)))
(define (loop-preheader-src l)
  (define ps (for/list ([p (in-pvector (cfg-preds (lctx-cfg (C)) (Loop-header l)))] #:unless (loop-inside? l p)) p))
  (and (= 1 (length ps)) (car ps)))
(define (loop-latch l) (car (Loop-latches l)))
(define (dominated-by? a b)
  (define d (lctx-dom (C)))
  (and (dom-index d a) (dom-index d b) (dom-dominates? d (dom-index d b) (dom-index d a))))
(define (idom bb)
  (define d (lctx-dom (C)))
  (define i (dom-index d bb)) (define j (and i (dom-idom d i)))
  (and j (not (= j i)) (dom-node d j)))
(define (def-stmt-of v) (hash-ref (lctx-def-stmt (C)) (ssa-name-version v) #f))
(define (def-bb-of v) (hash-ref (lctx-def-bb (C)) (ssa-name-version v) #f))
(define (stmt-loop-of-name v) (let ([bb (def-bb-of v)]) (and bb (bb-loop bb))))
(define (loop-phi-node? ph bb)
  (define l (bb-loop bb))
  (and l (= (Loop-header l) bb)))

;; ------------------------------------------------------------
;; chrecs
;; ------------------------------------------------------------

(define (auto-chrec? x) (symbol? x))
(define (chrec-dont-know? x) (eq? x 'dont-know))
(define (poly? x) (and (Tree? x) (eq? (tree-code x) 'polynomial_chrec)))
(define (chrec-var x) (tree-attr x 'var))
(define (chrec-left x) (tree-operand x 0))
(define (chrec-right x) (tree-operand x 1))
(define (chrec-type x) (if (auto-chrec? x) #f (tree-type x)))
(define (chrec-loop x) (loop-by-num (chrec-var x)))
(define (convert? x) (and (Tree? x) (memq (tree-code x) '(nop_expr convert_expr)) #t))
(define (chrec-zerop x) (and (icst? x) (zero? (icst-val x))))
(define (min-invariant? x) (and (Tree? x) (is-gimple-min-invariant (gs) x)))

(define (make-poly var left right)
  (make-tree 'polynomial_chrec #:type (tree-type left) #:ops (list left right) #:attrs (hasheq 'var var)))

(define (build-polynomial-chrec var left right)
  (cond
    [(or (chrec-dont-know? left) (chrec-dont-know? right)) 'dont-know]
    [(let-values ([(ok? val) (no-evolution-in-loop? left var)]) (not (and ok? val))) 'dont-know]
    [(chrec-zerop right) left]
    [else (make-poly var left right)]))

(define (tree-contains-chrecs? e [size-box #f])
  (let walk ([e e])
    (cond
      [(not e) #f]
      [else
       (when size-box (set-box! size-box (add1 (unbox size-box))))
       (cond
         [(auto-chrec? e) #t]
         [(poly? e) #t]
         [(not (Tree? e)) #f]
         [(memq (tree-code e) '(ssa_name integer_cst real_cst var_decl parm_decl)) #f]
         [else (for/or ([i (in-range (tree-operand-length e))]) (walk (tree-operand e i)))])])))

(define (chrec-contains-undetermined? e)
  (let walk ([e e])
    (cond [(chrec-dont-know? e) #t]
          [(or (not (Tree? e)) (memq (tree-code e) '(ssa_name integer_cst))) #f]
          [else (for/or ([i (in-range (tree-operand-length e))]) (walk (tree-operand e i)))])))

(define (chrec-contains-symbols-defined-in-loop? e loop-nb)
  (define l (loop-by-num loop-nb))
  (let walk ([e e])
    (cond
      [(or (not (Tree? e)) (auto-chrec? e)) #f]
      [(min-invariant? e) #f]
      [(eq? (tree-code e) 'ssa_name)
       (cond
         [(ssa-default-def? e) #f]
         [else (define dl (stmt-loop-of-name e))
               (define bb (def-bb-of e))
               (and bb (or (same-loop? l dl) (nested-in? l dl)))])]
      [else (for/or ([i (in-range (tree-operand-length e))]) (walk (tree-operand e i)))])))

(define (initial-condition c) (if (poly? c) (initial-condition (chrec-left c)) c))
;; chrec_component_in_loop_num: #f for NULL_TREE
(define (chrec-component-in-loop-num c loop-nb right?)
  (define l (loop-by-num loop-nb))
  (cond
    [(auto-chrec? c) c]
    [(poly? c)
     (define cl (chrec-loop c))
     (cond
       [(same-loop? cl l)
        (define component (if right? (chrec-right c) (chrec-left c)))
        (if (or (not (poly? (chrec-left c))) (not (= (chrec-var (chrec-left c)) (chrec-var c))))
            component
            (build-polynomial-chrec loop-nb (chrec-component-in-loop-num (chrec-left c) loop-nb right?) component))]
       [(nested-in? cl l) #f]
       [else (chrec-component-in-loop-num (chrec-left c) loop-nb right?)])]
    [else (if right? #f c)]))
(define (evolution-part-in-loop-num c n) (chrec-component-in-loop-num c n #t))
(define (initial-condition-in-loop-num c n) (chrec-component-in-loop-num c n #f))

(define (hide-evolution-in-other-loops-than-loop c loop-nb)
  (define l (loop-by-num loop-nb))
  (cond
    [(auto-chrec? c) c]
    [(poly? c)
     (define cl (chrec-loop c))
     (cond
       [(same-loop? cl l) (build-polynomial-chrec loop-nb (hide-evolution-in-other-loops-than-loop (chrec-left c) loop-nb) (chrec-right c))]
       [(nested-in? cl l) (initial-condition c)]
       [(nested-in? l cl) (hide-evolution-in-other-loops-than-loop (chrec-left c) loop-nb)]
       [else 'dont-know])]
    [else c]))

;; no_evolution_in_loop_p: (values computable? invariant?)
(define (no-evolution-in-loop? c loop-nb)
  (cond
    [(or (eq? c 'not-analyzed) (chrec-dont-know? c) (chrec-contains-symbols-defined-in-loop? c loop-nb)) (values #f #f)]
    [else
     (define s (hide-evolution-in-other-loops-than-loop (gf-strip-nops* c) loop-nb))
     (values #t (not (tree-contains-chrecs? s)))]))
(define (gf-strip-nops* c) (if (auto-chrec? c) c (gf-strip-nops c)))

(define (evolution-function-is-constant? c) (min-invariant? c))
(define (expr-invariant-in-loop? l e)
  (let walk ([e e])
    (cond
      [(not (Tree? e)) #t]
      [(min-invariant? e) #t]
      [(eq? (tree-code e) 'ssa_name)
       (or (ssa-default-def? e) (let ([bb (def-bb-of e)]) (or (not bb) (not (loop-inside? l bb)))))]
      [else (for/and ([i (in-range (tree-operand-length e))]) (walk (tree-operand e i)))])))
(define (evolution-function-is-invariant? c loop-nb)
  (let rec ([c c])
    (cond
      [(evolution-function-is-constant? c) #t]
      [(and (Tree? c) (eq? (tree-code c) 'ssa_name) (or (= loop-nb 0) (expr-invariant-in-loop? (loop-by-num loop-nb) c))) #t]
      [(poly? c)
       (not (or (= (chrec-var c) loop-nb)
                (nested-in? (loop-by-num loop-nb) (chrec-loop c))
                (not (rec (chrec-right c)))
                (not (rec (chrec-left c)))))]
      [(and (Tree? c) (memv (tree-operand-length c) '(1 2)))
       (for/and ([i (in-range (tree-operand-length c))]) (rec (tree-operand c i)))]
      [else #f])))
(define (evolution-function-is-affine? c)
  (and (poly? c)
       (evolution-function-is-invariant? (chrec-right c) (chrec-var c))
       (or (not (poly? (chrec-right c))) (evolution-function-is-affine? (chrec-right c)))))

(define (eq-evolutions? a b)
  (cond
    [(or (not (Tree? a)) (not (Tree? b)) (not (eq? (tree-code a) (tree-code b)))) #f]
    [(operand-equal? a b) #t]
    [else
     (case (tree-code a)
       [(polynomial_chrec) (and (= (chrec-var a) (chrec-var b)) (eq-evolutions? (chrec-left a) (chrec-left b)) (eq-evolutions? (chrec-right a) (chrec-right b)))]
       [(plus_expr mult_expr minus_expr pointer_plus_expr)
        (and (eq-evolutions? (tree-operand a 0) (tree-operand b 0)) (eq-evolutions? (tree-operand a 1) (tree-operand b 1)))]
       [(nop_expr convert_expr) (eq-evolutions? (tree-operand a 0) (tree-operand b 0))]
       [else #f])]))
(define (chrec-merge a b)
  (cond
    [(or (chrec-dont-know? a) (chrec-dont-know? b)) 'dont-know]
    [(or (eq? a 'known) (eq? b 'known)) 'known]
    [(eq? a 'not-analyzed) b]
    [(eq? b 'not-analyzed) a]
    [(eq-evolutions? a b) a]
    [else 'dont-know]))

(define (fold-auto a b)
  (cond [(or (chrec-dont-know? a) (chrec-dont-know? b)) 'dont-know]
        [(or (eq? a 'known) (eq? b 'known)) 'known]
        [(or (eq? a 'not-analyzed) (eq? b 'not-analyzed)) 'not-analyzed]
        [else 'dont-know]))

(define (minus-one t) (icst t -1))

(define (chrec-fold-plus-poly-poly code t p0 p1)
  (define l0 (chrec-loop p0)) (define l1 (chrec-loop p1))
  (define rtype (if (eq? code 'pointer_plus_expr) (chrec-type p1) t))
  (cond
    [(nested-in? l0 l1)
     (if (memq code '(plus_expr pointer_plus_expr))
         (build-polynomial-chrec (chrec-var p1) (chrec-fold-plus t p0 (chrec-left p1)) (chrec-right p1))
         (build-polynomial-chrec (chrec-var p1) (chrec-fold-minus t p0 (chrec-left p1))
                                 (chrec-fold-multiply t (chrec-right p1) (minus-one t))))]
    [(nested-in? l1 l0)
     (if (memq code '(plus_expr pointer_plus_expr))
         (build-polynomial-chrec (chrec-var p0) (chrec-fold-plus t (chrec-left p0) p1) (chrec-right p0))
         (build-polynomial-chrec (chrec-var p0) (chrec-fold-minus t (chrec-left p0) p1) (chrec-right p0)))]
    [(not (same-loop? l0 l1)) 'dont-know]
    [else
     (define-values (left right)
       (if (memq code '(plus_expr pointer_plus_expr))
           (values (chrec-fold-plus t (chrec-left p0) (chrec-left p1)) (chrec-fold-plus rtype (chrec-right p0) (chrec-right p1)))
           (values (chrec-fold-minus t (chrec-left p0) (chrec-left p1)) (chrec-fold-minus t (chrec-right p0) (chrec-right p1)))))
     (if (chrec-zerop right) left (build-polynomial-chrec (chrec-var p0) left right))]))

(define (chrec-fold-multiply-poly-poly t p0 p1)
  (define l0 (chrec-loop p0)) (define l1 (chrec-loop p1))
  (cond
    [(nested-in? l0 l1)
     (build-polynomial-chrec (chrec-var p1) (chrec-fold-multiply t (chrec-left p1) p0) (chrec-right p1))]
    [(nested-in? l1 l0)
     (build-polynomial-chrec (chrec-var p0) (chrec-fold-multiply t (chrec-left p0) p1) (chrec-right p0))]
    [(not (same-loop? l0 l1)) 'dont-know]
    [else
     (define t0 (chrec-fold-multiply t (chrec-left p0) (chrec-left p1)))
     (define t1a (chrec-fold-multiply t (chrec-left p0) (chrec-right p1)))
     (define t1b (chrec-fold-plus t t1a (chrec-fold-multiply t (chrec-right p0) (chrec-left p1))))
     (define t2 (chrec-fold-multiply t (chrec-right p0) (chrec-right p1)))
     (define t1 (chrec-fold-plus t t1b t2))
     (define t2b (chrec-fold-multiply t (icst t 2) t2))
     (define var (chrec-var p0))
     (build-polynomial-chrec var t0 (build-polynomial-chrec var t1 t2b))]))

;; the sign-conversion stripping of chrec_fold_plus_1: (T)X with X unsigned of T's precision
(define (strip-sign-conv-ok? t op)
  (define ot (tree-type (tree-operand op 0)))
  (and (ty-int? t) (ty-int? ot) (tree-nop-conversion-p t ot) (ty-uns? ot) ot))

(define (chrec-fold-plus-1 code t op0 op1)
  (define (rec-plus a b) (if (memq code '(plus_expr pointer_plus_expr)) (chrec-fold-plus t a b) (chrec-fold-minus t a b)))
  (cond
    [(or (auto-chrec? op0) (auto-chrec? op1)) (fold-auto op0 op1)]
    [(poly? op0)
     (cond
       [(poly? op1) (chrec-fold-plus-poly-poly code t op0 op1)]
       [(and (convert? op1)
             (if (>= (major) 13) (tree-contains-chrecs? op1) #t))
        (define ot (strip-sign-conv-ok? t op1))
        (cond
          [(>= (major) 13)
           (define tem (and ot (chrec-convert ot op0)))
           (if (and tem (poly? tem))
               (chrec-convert t (chrec-fold-plus-1 code ot tem (tree-operand op1 0)))
               'dont-know)]
          [ot (chrec-convert t (chrec-fold-plus-1 code ot (chrec-convert ot op0) (tree-operand op1 0)))]
          [(tree-contains-chrecs? op1) 'dont-know]
          [else (build-polynomial-chrec (chrec-var op0) (rec-plus (chrec-left op0) op1) (chrec-right op0))])]
       [else (build-polynomial-chrec (chrec-var op0) (rec-plus (chrec-left op0) op1) (chrec-right op0))])]
    [(and (convert? op0) (if (>= (major) 13) (tree-contains-chrecs? op0) #t)
          (or (strip-sign-conv-ok? t op0) (tree-contains-chrecs? op0)))
     (define ot (strip-sign-conv-ok? t op0))
     (if ot
         (chrec-convert t (chrec-fold-plus-1 code ot (tree-operand op0 0) (chrec-convert ot op1)))
         'dont-know)]
    [(poly? op1)
     (if (memq code '(plus_expr pointer_plus_expr))
         (build-polynomial-chrec (chrec-var op1) (chrec-fold-plus t op0 (chrec-left op1)) (chrec-right op1))
         (build-polynomial-chrec (chrec-var op1) (chrec-fold-minus t op0 (chrec-left op1))
                                 (chrec-fold-multiply t (chrec-right op1) (minus-one t))))]
    [(and (convert? op1) (tree-contains-chrecs? op1))
     (define ot (and (>= (major) 13) (strip-sign-conv-ok? t op1)))
     (if ot
         (chrec-convert t (chrec-fold-plus-1 code ot (chrec-convert ot op0) (tree-operand op1 0)))
         'dont-know)]
    [else
     (define size (box 0))
     (define c0 (tree-contains-chrecs? op0 size))
     (define c1 (tree-contains-chrecs? op1 size))
     (cond
       [(and (or c0 c1) (< (unbox size) param-scev-max-expr-size))
        (make-tree code #:type t #:ops (list op0 op1))]
       [(< (unbox size) param-scev-max-expr-size)
        (if (eq? code 'pointer_plus_expr)
            (gf-build2 'pointer_plus_expr t (gf-convert t op0) op1)
            (gf-build2 code t (gf-convert t op0) (gf-convert t op1)))]
       [else 'dont-know])]))
(define param-scev-max-expr-size 100)

(define (chrec-fold-plus t op0 op1)
  (cond
    [(or (auto-chrec? op0) (auto-chrec? op1)) (fold-auto op0 op1)]
    [(integer-zerop op0) (chrec-convert t op1)]
    [(integer-zerop op1) (chrec-convert t op0)]
    [else (chrec-fold-plus-1 (if (ty-ptr? t) 'pointer_plus_expr 'plus_expr) t op0 op1)]))
(define (chrec-fold-minus t op0 op1)
  (cond
    [(or (auto-chrec? op0) (auto-chrec? op1)) (fold-auto op0 op1)]
    [(integer-zerop op1) op0]
    [else (chrec-fold-plus-1 'minus_expr t op0 op1)]))

(define (chrec-fold-multiply t op0* op1*)
  (define-values (op0 op1)
    (if (and (>= (major) 13) (not (poly? op0*)) (poly? op1*)) (values op1* op0*) (values op0* op1*)))
  (cond
    [(or (auto-chrec? op0) (auto-chrec? op1)) (fold-auto op0 op1)]
    [(poly? op0)
     (cond
       [(poly? op1) (chrec-fold-multiply-poly-poly t op0 op1)]
       [(and (convert? op1) (tree-contains-chrecs? op1))
        (define ot (and (>= (major) 13) (strip-sign-conv-ok? t op1)))
        (define tem (and ot (chrec-convert ot op0)))
        (if (and tem (poly? tem))
            (chrec-convert t (chrec-fold-multiply ot tem (tree-operand op1 0)))
            'dont-know)]
       [(integer-onep op1) op0]
       [(integer-zerop op1) (icst t 0)]
       [(and (>= (major) 13) (ty-int? t) (ty-undef? t) (not (integer-zerop (chrec-left op0)))
             (icst? op1) (icst? (chrec-right op0))
             (not (int-fits-type? (* (icst-val (chrec-right op0)) (icst-val op1)) t)))
        (define u (utype-for t))
        (define uop1 (chrec-convert-rhs u op1))
        (define left (chrec-fold-multiply u (chrec-convert-rhs u (chrec-left op0)) uop1))
        (define right (chrec-fold-multiply u (chrec-convert-rhs u (chrec-right op0)) uop1))
        (chrec-convert-rhs t (build-polynomial-chrec (chrec-var op0) left right))]
       [else
        (build-polynomial-chrec (chrec-var op0) (chrec-fold-multiply t (chrec-left op0) op1) (chrec-fold-multiply t (chrec-right op0) op1))])]
    [(and (convert? op0) (tree-contains-chrecs? op0))
     (define ot (and (>= (major) 13) (strip-sign-conv-ok? t op0)))
     (if ot
         (chrec-convert t (chrec-fold-multiply ot (tree-operand op0 0) (chrec-convert ot op1)))
         'dont-know)]
    [(integer-onep op0) op1]
    [(integer-zerop op0) (icst t 0)]
    [(poly? op1)
     ;; GCC 12 (no swap above)
     (build-polynomial-chrec (chrec-var op1) (chrec-fold-multiply t (chrec-left op1) op0) (chrec-fold-multiply t (chrec-right op1) op0))]
    [(and (convert? op1) (tree-contains-chrecs? op1))
     (if (>= (major) 13) (chrec-fold-multiply t op1 op0) 'dont-know)]
    [(integer-onep op1) op0]
    [(integer-zerop op1) (icst t 0)]
    [else (gf-build2 'mult_expr t op0 op1)]))

;; ------------------------------------------------------------
;; conversions
;; ------------------------------------------------------------

(define (chrec-convert-rhs t c [at #f])
  (chrec-convert (if (ty-ptr? t) (sizetype (gs)) t) c at))
(define (chrec-convert t c [at #f] [use-overflow-semantics #t] [from #f])
  (chrec-convert-1 t c at use-overflow-semantics from))

(define (useless-conv? outer inner) (or (eqv? outer inner) (useless-type-conversion-p (gs) outer inner)))

(define (chrec-convert-1 t c at use-ovf from)
  (cond
    [(auto-chrec? c) c]
    [(useless-conv? t (chrec-type c)) c]
    [else
     (define ct (chrec-type c))
     (define converted
       (and (evolution-function-is-affine? c)
            (let-values ([(ok base step) (convert-affine-scev (chrec-loop c) t (chrec-left c) (chrec-right c) at use-ovf from)])
              (and ok (build-polynomial-chrec (chrec-var c) base step)))))
     (cond
       [converted converted]
       [else
        (define res
          (cond
            [(and use-ovf (memq (tree-code c) '(plus_expr minus_expr))
                  (ty-int? t) (ty-int? ct) (> (ty-prec t) (ty-prec ct)) (ty-undef? ct))
             (gf-build2 (tree-code c) t (gf-convert t (tree-operand c 0)) (gf-convert t (tree-operand c 1)))]
            [(and use-ovf (poly? c) (ty-int? ct) (ty-int? t) (ty-undef? t) (< (ty-prec t) (ty-prec ct)))
             (define u (utype-for t))
             (chrec-convert-1 t (build-polynomial-chrec (chrec-var c) (gf-convert u (chrec-left c)) (gf-convert u (chrec-right c))) at use-ovf from)]
            [(poly? c) (make-tree 'nop_expr #:type t #:ops (list c))]
            [else (gf-convert t c)]))
        (if (and (icst? res) (ty-int? t) (not (int-fits-type? (icst-val res) t))) 'dont-know res)])]))

;; convert_affine_scev: (values ok? base step)
(define (convert-affine-scev l t base step at use-ovf from)
  (define ct (tree-type step))
  (define step-type (if (ty-ptr? t) (sizetype (gs)) t))
  (define must-check-src0 (< (ty-prec ct) (ty-prec t)))
  (define enforce (and use-ovf (nowrap-type-p t)))
  (define-values (must-check-src must-check-rslt)
    (cond
      [(not enforce) (values must-check-src0 #f)]
      [must-check-src0 (values #t (and (ty-uns? t) (not (ty-uns? ct))))]
      [(and (eq? (ty-uns? ct) (ty-uns? t)) (= (ty-prec ct) (ty-prec t))) (values #t #f)]
      [else (values must-check-src0 #t)]))
  (cond
    [(and must-check-src (scev-probably-wraps? from base step at l use-ovf)) (values #f #f #f)]
    [else
     (define new-base (chrec-convert t base at use-ovf))
     (define step1 (if (and (> (ty-prec step-type) (ty-prec ct)) (ty-uns? ct))
                       (chrec-convert (stype-for ct) step at use-ovf)
                       step))
     (define new-step (chrec-convert step-type step1 at use-ovf))
     (cond
       [(or (auto-chrec? new-base) (auto-chrec? new-step)) (values #f #f #f)]
       [(and must-check-rslt (scev-probably-wraps? #f new-base new-step at l #f)) (values #f #f #f)]
       [else (values #t new-base new-step)])]))

;; scev_probably_wraps_p: an IV of a type whose overflow is undefined does
;; not wrap when the language semantics may be used; otherwise only an
;; IV whose type range covers BASE + STEP * (niter + 1) is known not to
(define (scev-probably-wraps? var base step at l use-ovf)
  (cond
    [(chrec-contains-undetermined? base) #t]
    [(chrec-contains-undetermined? step) #t]
    [(integer-zerop step) #f]
    [(and use-ovf (nowrap-type-p (tree-type base))) #f]
    [else
     (define nit (and l (number-of-latch-executions l)))
     (cond
       [(and (icst? base) (icst? step) nit (icst? nit))
        (define t (tree-type base))
        (define last (+ (icst-val base) (* (icst-val step) (icst-val nit))))
        (not (int-fits-type? last t))]
       [else #t])]))

;; ------------------------------------------------------------
;; chrec_apply
;; ------------------------------------------------------------

;; tree_fold_binomial: C(n, k) in TYPE or #f
(define (tree-fold-binomial t n k)
  (cond
    [(= k 0) (icst t 1)]
    [(= k 1) (gf-convert t n)]
    [else
     (define num (icst-val n))
     (cond
       [(< num k) #f]
       [else
        (define v (for/fold ([acc 1]) ([i (in-range k)]) (* acc (- num i))))
        (define d (for/fold ([acc 1]) ([i (in-range 2 (add1 k))]) (* acc i)))
        (define r (quotient v d))
        (and (int-fits-type? r t) (icst t r))])]))

(define (chrec-evaluate var c n k)
  (define t (tree-type c))
  (define var-loop (loop-by-num var))
  (define c1 (let loop ([c c]) (if (and (poly? c) (nested-in? var-loop (chrec-loop c))) (loop (chrec-left c)) c)))
  (define ctype (if (and (ty-int? t) (not (ty-wraps? t))) (utype-for t) t))
  (cond
    [(and (poly? c1) (= (chrec-var c1) var))
     (define arg1 (chrec-evaluate var (chrec-right c1) n (add1 k)))
     (define b (and (not (chrec-dont-know? arg1)) (tree-fold-binomial ctype n k)))
     (cond
       [(chrec-dont-know? arg1) 'dont-know]
       [(not b) 'dont-know]
       [else
        (define l (chrec-convert ctype (chrec-left c1)))
        (chrec-fold-plus ctype (gf-build2 'mult_expr ctype l b) arg1)])]
    [else
     (define b (tree-fold-binomial ctype n k))
     (if b (gf-build2 'mult_expr ctype (chrec-convert ctype c1) b) 'dont-know)]))

(define (chrec-apply var c x)
  (define t (chrec-type c))
  (cond
    [(or (auto-chrec? c) (auto-chrec? x) (chrec-contains-symbols-defined-in-loop? c var)) 'dont-know]
    [(poly? c)
     (cond
       [(evolution-function-is-affine? c)
        (define r (chrec-right c)) (define l (chrec-left c))
        (cond
          [(not (= (chrec-var c) var))
           (build-polynomial-chrec (chrec-var c) (chrec-apply var l x) (chrec-apply var r x))]
          ;; "{a, +, a}" (x-1) -> "a*x" (GCC >= 13)
          [(and (>= (major) 13) (operand-equal? l r) (Tree? x) (eq? (tree-code x) 'plus_expr)
                (icst? (tree-operand x 1)) (= (icst-val (tree-operand x 1)) (if (ty-uns? (tree-type x)) (ty-max (tree-type x)) -1))
                (not (ty-ptr? t)) (>= (ty-prec (tree-type x)) (ty-prec t)))
           (define r1 (chrec-fold-plus (tree-type x) x (icst (tree-type x) 1)))
           (chrec-fold-multiply t r (chrec-convert-rhs t r1))]
          [(>= (major) 13)
           ;; "{a, +, b} (x)" -> "a + b*x", the increment in an unsigned type
           (define rt (tree-type r))
           (define utype (if (and (ty-int? rt) (not (ty-wraps? rt))) (utype-for rt) rt))
           (define res (chrec-fold-multiply utype (chrec-convert utype r) (chrec-convert-rhs utype x)))
           (if (and (icst? res) (int-fits-type? (icst-val (gf-convert rt res)) rt)
                    (int-fits-type? (wrap-signed (icst-val res) rt) rt)
                    (fits-original? res rt))
               (chrec-fold-plus t l (chrec-convert rt res))
               (chrec-convert t (chrec-fold-plus utype (chrec-convert utype l) res)))]
          [else
           (define x1 (chrec-convert-rhs t x))
           (chrec-fold-plus t l (chrec-fold-multiply (tree-type x1) r x1))])]
       [(and (icst? x) (> (icst-val x) 0))
        (chrec-convert t (chrec-evaluate var c x 0))]
       [else 'dont-know])]
    [(convert? c) (chrec-convert (tree-type c) (chrec-apply var (tree-operand c 0) x))]
    [else c]))
;; int_fits_type_p of an unsigned constant in the (signed) original type
(define (fits-original? res rt) (int-fits-type? (icst-val res) rt))
(define (wrap-signed v t) v)

;; ------------------------------------------------------------
;; the scalar evolution analyzer
;; ------------------------------------------------------------

(define (block-before-loop l) (if l (loop-preheader-src l) ENTRY))

(define (get-scalar-evolution below v)
  (cond
    [(and (Tree? v) (eq? (tree-code v) 'ssa_name))
     (if (and (not (ssa-default-def? v)) (def-bb-of v))
         (hash-ref (lctx-scev (C)) (cons below (ssa-name-version v)) 'not-analyzed)
         v)]
    [(and (Tree? v) (memq (tree-code v) '(integer_cst real_cst))) v]
    [else 'not-analyzed]))
(define (set-scalar-evolution! below v c)
  (hash-set! (lctx-scev (C)) (cons below (ssa-name-version v)) c))

(define (analyze-scalar-evolution l v)
  (cond
    [(not l) v]
    [else
     (define r (get-scalar-evolution (block-before-loop l) v))
     (if (eq? r 'not-analyzed) (analyze-scalar-evolution-1 l v) r)]))

(define (analyze-scalar-evolution-1 l v)
  (cond
    [(not (and (Tree? v) (eq? (tree-code v) 'ssa_name))) (interpret-expr l #f v)]
    [else
     (define bb (def-bb-of v))
     (define def-loop (and bb (bb-loop bb)))
     (define res0
       (cond
         [(or (not bb) (not (inside? l bb))) (follow-copies-to-constant v)]
         [(not (same-loop? l def-loop))
          (define r1 (analyze-scalar-evolution-1 def-loop v))
          (define skip (superloop-at-depth def-loop (add1 (loop-depth l))))
          (define r2 (compute-overall-effect-of-inner-loop skip r1))
          (if (chrec-contains-symbols-defined-in-loop? r2 (loop-num l)) (analyze-scalar-evolution-1 l r2) r2)]
         [else
          (define g (def-stmt-of v))
          (cond
            [(gassign? g) (interpret-rhs-expr l g (tree-type (gassign-lhs g)) (gassign-rhs1 g) (gassign-code g) (gassign-rhs2 g))]
            [(gphi? g) (if (loop-phi-node? g bb) (interpret-loop-phi l g) (interpret-condition-phi l g))]
            [else 'dont-know])]))
     (define res (if (chrec-dont-know? res0) v res0))
     (when (and bb (same-loop? l def-loop)) (set-scalar-evolution! (block-before-loop l) v res))
     res]))

(define (follow-copies-to-constant v)
  (define res
    (let loop ([r v])
      (cond
        [(and (Tree? r) (eq? (tree-code r) 'ssa_name) (def-stmt-of r))
         (define g (def-stmt-of r))
         (cond
           [(gphi? g)
            (define vals (remove-duplicates (for/list ([a (in-pvector (gphi-args g))]) (cdr a)) operand-equal?))
            (if (= 1 (length vals)) (loop (car vals)) r)]
           [(and (gassign? g) (eq? (gimple-rhs-class (gs) (gassign-code g)) 'single)) (loop (gassign-rhs1 g))]
           [else r])]
        [else r])))
  (if (and (Tree? res) (memq (tree-code res) '(integer_cst real_cst))) res v))

(define (interpret-expr l at e)
  (cond
    [(auto-chrec? e) e]
    [(or (poly? e) (memq (tree-code e) '(call_expr cond_expr))) 'dont-know]
    [else
     (define cls (gimple-rhs-class (gs) (tree-code e)))
     (case cls
       [(binary) (interpret-rhs-expr l at (tree-type e) (tree-operand e 0) (tree-code e) (tree-operand e 1))]
       [(unary) (interpret-rhs-expr l at (tree-type e) (tree-operand e 0) (tree-code e) #f)]
       [else (interpret-rhs-expr l at (tree-type e) e (tree-code e) #f)])]))

(define (conditionally-executed? l at)
  (and at (not (dominated-by? (loop-latch l) (stmt-bb at)))))
(define stmt-bb-cache (make-weak-hasheq))
(define (stmt-bb g)
  (define d (for/or ([n (in-list (stmt-def-names g))]) (def-bb-of n)))
  d)

(define (interpret-rhs-expr l at t rhs1 code rhs2)
  (define (ae x) (analyze-scalar-evolution l x))
  (define (inst x) (instantiate-parameters l x))
  (define (ctype-for)
    (if (and at (ty-int? t) (not (ty-wraps? t)) (conditionally-executed? l at)) (utype-for t) t))
  (cond
    [(and (eq? (gimple-rhs-class (gs) code) 'single) (min-invariant? rhs1)) (chrec-convert t rhs1 at)]
    [(eq? code 'ssa_name) (chrec-convert t (ae rhs1) at)]
    [else
     (case code
       [(pointer_plus_expr)
        (define c1 (inst (chrec-convert t (ae rhs1) at)))
        (define c2 (inst (chrec-convert (tree-type rhs2) (ae rhs2) at)))
        (chrec-fold-plus t c1 c2)]
       [(plus_expr minus_expr mult_expr)
        (define ct (ctype-for))
        (define c1 (inst (chrec-convert ct (ae rhs1) at)))
        (define c2 (inst (chrec-convert ct (ae rhs2) at)))
        (define res (case code
                      [(plus_expr) (chrec-fold-plus ct c1 c2)]
                      [(minus_expr) (chrec-fold-minus ct c1 c2)]
                      [else (chrec-fold-multiply ct c1 c2)]))
        (if (eqv? ct t) res (chrec-convert t res at))]
       [(negate_expr)
        (define ct (ctype-for))
        (define c1 (inst (chrec-convert ct (ae rhs1) at)))
        (define res (chrec-fold-multiply ct c1 (gf-convert ct (icst ct -1))))
        (if (eqv? ct t) res (chrec-convert t res at))]
       [(bit_not_expr)
        (define c1 (inst (chrec-convert t (ae rhs1) at)))
        (chrec-fold-minus t (icst t -1) c1)]
       [(nop_expr convert_expr)
        (define rt (tree-type rhs1))
        (define g (and (Tree? rhs1) (eq? (tree-code rhs1) 'ssa_name) (def-stmt-of rhs1)))
        (define c1
          (if (and (ty-int? t) (ty-int? rt) (< (ty-prec t) (ty-prec rt)) (ty-undef? t)
                   g (gassign? g) (eq? (gimple-rhs-class (gs) (gassign-code g)) 'binary) (icst? (gassign-rhs2 g)))
              (interpret-rhs-expr l at (utype-for t) (gassign-rhs1 g) (gassign-code g) (gassign-rhs2 g))
              (ae rhs1)))
        (chrec-convert t c1 at #t rhs1)]
       [else 'dont-know])]))

(define (interpret-condition-phi l ph)
  (let loop ([as (pvector->list (gphi-args ph))] [res 'not-analyzed])
    (if (or (null? as) (chrec-dont-know? res))
        res
        (loop (cdr as) (chrec-merge res (analyze-scalar-evolution l (cdr (car as))))))))

(define (interpret-loop-phi l ph)
  (define init (analyze-initial-condition ph))
  (analyze-evolution-in-loop ph init))

(define (analyze-initial-condition ph)
  (define bb (def-bb-of (gphi-result ph)))
  (define l (bb-loop bb))
  (define init
    (let loop ([as (pvector->list (gphi-args ph))] [init 'not-analyzed])
      (cond
        [(or (null? as) (chrec-dont-know? init)) init]
        [else
         (define a (car as))
         (loop (cdr as)
               (cond
                 [(loop-inside? l (car a)) init]
                 [(eq? init 'not-analyzed) (cdr a)]
                 [(and (Tree? (cdr a)) (eq? (tree-code (cdr a)) 'ssa_name)) 'dont-know]
                 [else (chrec-merge init (cdr a))]))])))
  (define init1 (if (eq? init 'not-analyzed) 'dont-know init))
  (if (auto-chrec? init1) init1 (follow-copies-to-constant init1)))

(define (analyze-evolution-in-loop ph init)
  (define bb (def-bb-of (gphi-result ph)))
  (define l (bb-loop bb))
  (let loop ([as (pvector->list (gphi-args ph))] [ef 'not-analyzed])
    (define a (and (pair? as) (car as)))
    (cond
      [(or (null? as) (chrec-dont-know? ef)) ef]
      [(not (loop-inside? l (car a))) (loop (cdr as) ef)]
      [else
       (define arg (cdr a))
       (define-values (res ev)
         (cond
           [(and (Tree? arg) (eq? (tree-code arg) 'ssa_name))
            (define-values (r e) (follow-ssa-edge-expr l ph init ph arg 'dont-know 0))
            (define e2
              (if (and (not (poly? e))
                       (let-values ([(ok? val) (no-evolution-in-loop? e (loop-num l))]) (and ok? val))
                       (not (and (Tree? init) (Tree? e) (operand-equal? init e))))
                  'dont-know e))
            (values r e2)]
           [else (values 'false 'dont-know)]))
       ;; peeled chrecs are not built (simplify_peeled_chrec not ported)
       (loop (cdr as) (chrec-merge ef (if (eq? res 'true) ev 'dont-know)))])))

;; scev_dfs: (values t_bool evolution); t_bool 'true / 'false / 'dont-know
(define param-scev-max-expr-complexity 10)
(define (follow-ssa-edge-expr l halting init at expr ev limit)
  (define (fail) (values 'false ev))
  (cond
    [(and (Tree? expr) (eq? (tree-code expr) 'ssa_name))
     (define def (def-stmt-of expr))
     (cond
       [(not def) (fail)]
       [(> limit param-scev-max-expr-complexity) (values 'dont-know 'dont-know)]
       [(gphi? def)
        (define dbb (def-bb-of expr))
        (cond
          [(not (loop-phi-node? def dbb)) (follow-ssa-edge-in-condition-phi l halting init def ev limit)]
          [(eq? def halting) (values 'true expr)]
          [else
           (define dl (bb-loop dbb))
           (cond
             [(same-loop? dl l) (fail)]
             [(nested-in? l dl) (values 'false ev)]          ; inner loop PHIs: not ported
             [else (fail)])])]
       [(not (gassign? def)) (fail)]
       [else
        (define code (gassign-code def))
        (case (gimple-rhs-class (gs) code)
          [(binary unary single)
           (follow-code l halting init def code (tree-type (gassign-lhs def)) (gassign-rhs1 def) (gassign-rhs2 def) ev limit)]
          [else (fail)])])]
    [else
     (case (tree-code expr)
       [(nop_expr convert_expr) (follow-code l halting init at (tree-code expr) (tree-type expr) (tree-operand expr 0) #f ev limit)]
       [(pointer_plus_expr plus_expr minus_expr)
        (follow-code l halting init at (tree-code expr) (tree-type expr)
                     (strip-useless-conv (tree-operand expr 0)) (strip-useless-conv (tree-operand expr 1)) ev limit)]
       [else (follow-code l halting init at (tree-code expr) (tree-type expr) expr #f ev limit)])]))
(define (strip-useless-conv e)
  (let loop ([e e])
    (if (and (convert? e) (useless-conv? (tree-type e) (tree-type (tree-operand e 0)))) (loop (tree-operand e 0)) e)))

(define (follow-code l halting init at code t rhs0 rhs1 ev limit)
  (define (ssa? x) (and (Tree? x) (eq? (tree-code x) 'ssa_name)))
  (case code
    [(nop_expr convert_expr)
     (cond
       [(not (tree-nop-conversion-p t (tree-type rhs0))) (values 'false ev)]
       [else
        (define-values (r e) (follow-ssa-edge-expr l halting init at rhs0 ev limit))
        (values r (if (eq? r 'true) (chrec-convert t e at) e))])]
    [(integer_cst) (values 'false ev)]
    [(pointer_plus_expr plus_expr minus_expr)
     (cond
       [(and (ssa? rhs0) (or (not (ssa? rhs1)) (eq? code 'minus_expr)))
        (define-values (r e) (follow-ssa-edge-expr l halting init at rhs0 ev limit))
        (values r (if (eq? r 'true) (add-to-evolution l halting init (chrec-convert t e at) code rhs1 at) e))]
       [else (follow-ssa-edge-binary l halting init at t rhs0 code rhs1 ev limit)])]
    [else (values 'false ev)]))

(define (follow-ssa-edge-binary l halting init at t rhs0 code rhs1 ev limit)
  (define (ssa? x) (and (Tree? x) (eq? (tree-code x) 'ssa_name)))
  (case code
    [(pointer_plus_expr plus_expr)
     (cond
       [(ssa? rhs0)
        ;; both SSA names (the other cases are handled by the caller)
        (define limit1 (add1 limit))
        (define-values (r e) (follow-ssa-edge-expr l halting init at rhs0 ev limit1))
        (cond
          [(eq? r 'true) (values r (add-to-evolution l halting init (chrec-convert t e at) code rhs1 at))]
          [(eq? r 'false)
           (define-values (r2 e2) (follow-ssa-edge-expr l halting init at rhs1 ev limit1))
           (values r2 (if (eq? r2 'true) (add-to-evolution l halting init (chrec-convert t e2 at) code rhs0 at) e2))]
          [else (values r e)])]
       [(ssa? rhs1)
        (define-values (r e) (follow-ssa-edge-expr l halting init at rhs1 ev limit))
        (values r (if (eq? r 'true) (add-to-evolution l halting init (chrec-convert t e at) code rhs0 at) e))]
       [else (values 'false ev)])]
    [else (values 'false ev)]))

(define (follow-ssa-edge-in-condition-phi l halting init ph ev limit)
  (define args (pvector->list (gphi-args ph)))
  (define (branch a lim)
    (define b (cdr a))
    (if (and (Tree? b) (eq? (tree-code b) 'ssa_name))
        (follow-ssa-edge-expr l halting init ph b init lim)
        (values 'false 'dont-know)))
  (define-values (r0 e0) (branch (car args) limit))
  (cond
    [(memq r0 '(false dont-know)) (values r0 ev)]
    [else
     (let loop ([as (cdr args)] [i 1] [acc e0])
       (cond
         [(null? as) (values 'true acc)]
         [(chrec-dont-know? acc) (values 'true acc)]
         [else
          (define-values (r e) (branch (car as) (+ limit i)))
          (if (memq r '(false dont-know)) (values r acc) (loop (cdr as) (add1 i) (chrec-merge acc e)))]))]))

(define (add-to-evolution l halting init before code to-add at)
  (define t (chrec-type to-add))
  (cond
    [(not to-add) before]
    [(poly? to-add) 'dont-know]
    [else
     (define ta (if (eq? code 'minus_expr) (chrec-fold-multiply t to-add (icst t -1)) to-add))
     (add-to-evolution-1 l halting init before ta at)]))

(define (add-to-evolution-1 l halting init before to-add at)
  (define loop-nb (loop-num l))
  (cond
    [(poly? before)
     (define chl (chrec-loop before))
     (cond
       [(or (same-loop? chl l) (nested-in? chl l))
        (define t (chrec-type before))
        (define-values (var left right)
          (if (same-loop? chl l)
              (values (chrec-var before) (chrec-left before) (chrec-right before))
              (values loop-nb before (icst t 0))))
        (define ta (chrec-convert t to-add at))
        (define r1 (chrec-convert-rhs t right at))
        (define r2 (chrec-fold-plus (chrec-type r1) r1 ta))
        (if (and (ty-int? t) (not (ty-wraps? t)) (icst? r2) (not (int-fits-type? (icst-val r2) (tree-type r2))))
            'dont-know
            (build-polynomial-chrec var left r2))]
       [else
        (define left (add-to-evolution-1 l halting init (chrec-left before) to-add at))
        (define right (chrec-convert-rhs (chrec-type left) (chrec-right before) at))
        (build-polynomial-chrec (chrec-var before) left right)])]
    [(chrec-dont-know? before) 'dont-know]
    [else
     (define right (chrec-convert-rhs (chrec-type before) to-add at))
     (define stripped (gf-strip-nops before))
     (define left (if (and (Tree? stripped) (eq? (tree-code stripped) 'ssa_name)
                           (= (ssa-name-version stripped) (ssa-name-version (gphi-result halting))))
                      (if (auto-chrec? init) init (gf-convert (tree-type before) init))
                      before))
     (build-polynomial-chrec loop-nb left right)]))

;; ------------------------------------------------------------
;; instantiation
;; ------------------------------------------------------------

(define global-cache (make-parameter #f))
(define (with-instantiate-cache thunk)
  (if (global-cache) (thunk) (parameterize ([global-cache (make-hash)]) (thunk))))

;; instantiate_scev with INSTANTIATE_BELOW the preheader edge of L
(define (instantiate-parameters l c)
  (with-instantiate-cache (lambda () (instantiate-scev-r (loop-preheader-dest l) l #f c #f 0))))
(define (loop-preheader-dest l) (Loop-header l))

(define (instantiate-scev-r below-dest evo-loop inner c fold-box size)
  (cond
    [(> size param-scev-max-expr-size) 'dont-know]
    [(or (not c) (auto-chrec? c) (min-invariant? c)) c]
    [else
     (define size1 (add1 size))
     (case (tree-code c)
       [(ssa_name) (instantiate-scev-name below-dest evo-loop inner c fold-box size1)]
       [(polynomial_chrec)
        (define cl (chrec-loop c))
        (define op0 (instantiate-scev-r below-dest evo-loop cl (chrec-left c) fold-box size1))
        (cond
          [(chrec-dont-know? op0) 'dont-know]
          [else
           (define op1 (instantiate-scev-r below-dest evo-loop cl (chrec-right c) fold-box size1))
           (cond
             [(chrec-dont-know? op1) 'dont-know]
             [(and (eq? op0 (chrec-left c)) (eq? op1 (chrec-right c))) c]
             [else (build-polynomial-chrec (chrec-var c) op0 (chrec-convert-rhs (chrec-type op0) op1))])])]
       [(pointer_plus_expr plus_expr minus_expr mult_expr)
        (define code (tree-code c)) (define t (chrec-type c))
        (define c0 (tree-operand c 0)) (define c1 (tree-operand c 1))
        (define op0 (instantiate-scev-r below-dest evo-loop inner c0 fold-box size1))
        (cond
          [(chrec-dont-know? op0) 'dont-know]
          [else
           (define op1 (if (eq? c0 c1) op0 (instantiate-scev-r below-dest evo-loop inner c1 fold-box size1)))
           (cond
             [(chrec-dont-know? op1) 'dont-know]
             [(or (not (eq? c0 op0)) (not (eq? c1 op1)))
              (define a (chrec-convert t op0)) (define b (chrec-convert-rhs t op1))
              (case code
                [(pointer_plus_expr plus_expr) (chrec-fold-plus t a b)]
                [(minus_expr) (chrec-fold-minus t a b)]
                [else (chrec-fold-multiply t a b)])]
             [else c])])]
       [(nop_expr convert_expr)
        (define t (tree-type c)) (define op (tree-operand c 0))
        (define op0 (instantiate-scev-r below-dest evo-loop inner op fold-box size1))
        (cond
          [(chrec-dont-know? op0) 'dont-know]
          [fold-box
           (define tmp (chrec-convert-aggressive t op0 fold-box))
           (cond
             [tmp tmp]
             [(unbox fold-box) (if (eq? op0 op) c (gf-convert t op0))]
             [else (chrec-convert t op0)])]
          [else (chrec-convert t op0)])]
       [(negate_expr bit_not_expr)
        (define t (tree-type c)) (define op (tree-operand c 0))
        (define op0 (instantiate-scev-r below-dest evo-loop inner op fold-box size1))
        (cond
          [(chrec-dont-know? op0) 'dont-know]
          [(not (eq? op op0))
           (define a (chrec-convert t op0))
           (if (eq? (tree-code c) 'bit_not_expr)
               (chrec-fold-minus t (icst t -1) a)
               (chrec-fold-multiply t (icst t -1) a))]
          [else c])]
       [(integer_cst real_cst) c]
       [else 'dont-know])]))

(define (instantiate-scev-name below-dest evo-loop inner c fold-box size)
  (define def-bb (def-bb-of c))
  (cond
    [(or (not def-bb) (not (dominated-by? def-bb below-dest))) c]
    [else
     (define key (cons (ssa-name-version c) below-dest))
     (define cached (hash-ref (global-cache) key 'not-analyzed))
     (cond
       [(not (eq? cached 'not-analyzed)) cached]
       [else
        (hash-set! (global-cache) key 'dont-know)
        (define def-loop (find-common-loop evo-loop (bb-loop def-bb)))
        (define res
          (cond
            [(not (dominated-by? (if def-loop (Loop-header def-loop) ENTRY) below-dest))
             (define g (def-stmt-of c))
             (cond
               [(and (gassign? g) (eq? (gimple-rhs-class (gs) (gassign-code g)) 'unary))
                (define op0 (instantiate-scev-r below-dest evo-loop inner (gassign-rhs1 g) fold-box size))
                (if (chrec-dont-know? op0) 'dont-know (gf-build1 (gassign-code g) (tree-type c) op0))]
               [(and (gassign? g) (eq? (gimple-rhs-class (gs) (gassign-code g)) 'binary))
                (define op0 (instantiate-scev-r below-dest evo-loop inner (gassign-rhs1 g) fold-box size))
                (define op1 (and (not (chrec-dont-know? op0)) (instantiate-scev-r below-dest evo-loop inner (gassign-rhs2 g) fold-box size)))
                (if (or (chrec-dont-know? op0) (chrec-dont-know? op1)) 'dont-know (gf-build2 (gassign-code g) (tree-type c) op0 op1))]
               [else 'dont-know])]
            [else
             (define r (analyze-scalar-evolution def-loop c))
             (cond
               [(and (Tree? r) (eq? (tree-code r) 'ssa_name) (ssa-default-def? r)) r]
               [(and (Tree? r) (eq? (tree-code r) 'ssa_name)
                     (> (loop-depth (stmt-loop-of-name r)) (loop-depth def-loop)))
                'dont-know]                                    ; loop_closed_phi_def path not ported
               [(not (chrec-dont-know? r))
                (if (and inner (not (same-loop? (bb-loop def-bb) inner)) (not (nested-in? (bb-loop def-bb) inner)))
                    'dont-know
                    (instantiate-scev-r below-dest evo-loop inner r fold-box size))]
               [else r])]))
        (hash-set! (global-cache) key res)
        res])]))

(define (chrec-convert-aggressive t c fold-box)
  (cond
    [(or (auto-chrec? c) (not (poly? c))) #f]
    [(> (ty-prec t) (ty-prec (tree-type c))) #f]
    [(useless-conv? t (tree-type c)) #f]
    [else
     (define affine
       (and (not (unbox fold-box)) (evolution-function-is-affine? c)
            (let-values ([(ok b s) (convert-affine-scev (chrec-loop c) t (chrec-left c) (chrec-right c) #f #t #f)])
              (and ok (build-polynomial-chrec (chrec-var c) b s)))))
     (cond
       [affine affine]
       [else
        (define rtype (if (ty-ptr? t) (sizetype (gs)) t))
        (define lc (or (chrec-convert-aggressive t (chrec-left c) fold-box) (chrec-convert t (chrec-left c))))
        (define rc (or (chrec-convert-aggressive rtype (chrec-right c) fold-box) (chrec-convert rtype (chrec-right c))))
        (set-box! fold-box #t)
        (build-polynomial-chrec (chrec-var c) lc rc)])]))

;; resolve_mixers
(define (resolve-mixers l c)
  (define fold-box (box #f))
  (define r (with-instantiate-cache (lambda () (instantiate-scev-r (Loop-header l) l #f c fold-box 0))))
  (values r (unbox fold-box)))

;; analyze_scalar_evolution_in_loop: (values ev folded-casts?)
(define (analyze-scalar-evolution-in-loop wrto use v)
  (let loop ([use use] [ev v] [folded #f])
    (define tmp (analyze-scalar-evolution use ev))
    (define-values (ev1 f1) (resolve-mixers use tmp))
    (define folded1 (or folded f1))
    (cond
      [(same-loop? use wrto) (values ev1 folded1)]
      [else
       (define-values (ok? val) (no-evolution-in-loop? ev1 (loop-num use)))
       (if (and ok? val) (loop (loop-outer use) ev1 folded1) (values 'dont-know folded1))])))

(define (compute-overall-effect-of-inner-loop l ev)
  (cond
    [(chrec-dont-know? ev) 'dont-know]
    [(poly? ev)
     (define inner (chrec-loop ev))
     (cond
       [(or (same-loop? inner l) (nested-in? l inner))
        (define nb (number-of-latch-executions inner))
        (cond
          [(chrec-dont-know? nb) 'dont-know]
          [else
           (define r0 (chrec-apply (Loop-num inner) ev nb))
           (define r1 (if (chrec-contains-symbols-defined-in-loop? r0 (loop-num l)) (instantiate-parameters l r0) r0))
           (compute-overall-effect-of-inner-loop l r1)])]
       [else ev])]
    [else
     (define-values (ok? val) (no-evolution-in-loop? ev (loop-num l)))
     (if (and ok? val) ev 'dont-know)]))

;; ------------------------------------------------------------
;; iteration counts
;; ------------------------------------------------------------

(struct affine-iv (base step no-overflow) #:transparent #:mutable)
(struct niter-desc (assumptions may-be-zero niter max control cmp bound) #:transparent #:mutable)

(define (number-of-latch-executions l)
  (define nc (lctx-niters (C)))
  (cond
    [(hash-ref nc (Loop-num l) #f)]
    [else
     (define exit (single-exit l))
     (define d (and exit (number-of-iterations-exit l exit)))
     (define res0 (if d (niter-desc-niter d) 'dont-know))
     (define mbz (and d (niter-desc-may-be-zero d)))
     (define res
       (cond
         [(or (chrec-dont-know? res0) (not mbz) (integer-zerop mbz)) res0]
         [(and (icst? mbz) (not (zero? (icst-val mbz)))) (icst (tree-type res0) 0)]
         [(comparison-code? (tree-code mbz)) (gf-build3 'cond_expr (tree-type res0) mbz (icst (tree-type res0) 0) res0)]
         [else 'dont-know]))
     (hash-set! nc (Loop-num l) res)
     res]))

(define (bool-true) (boolean-cst #t))
(define (bool-false) (boolean-cst #f))
(define (integer-nonzerop* e) (and (icst? e) (not (zero? (icst-val e)))))

;; number_of_iterations_exit (every_iteration = true): niter-desc or #f
(define (number-of-iterations-exit l exit)
  (define d (number-of-iterations-exit-assumptions l exit #t))
  (and d (integer-nonzerop* (niter-desc-assumptions d)) d))

(define (number-of-iterations-exit-assumptions l exit every-iteration)
  (define cfg (lctx-cfg (C)))
  (define src (car exit))
  (define safe (dominated-by? (loop-latch l) src))
  (define g (bb-last-stmt cfg src))
  (cond
    [(and every-iteration (not safe)) #f]
    [(not (gcond? g)) #f]
    [else
     (define exit-true? (edge-flag? (find-edge cfg src (cdr exit)) EDGE-TRUE-VALUE))
     (define code (if exit-true? (invert-cmp (gcond-code g)) (gcond-code g)))
     (define op0 (gcond-lhs g)) (define op1 (gcond-rhs g))
     (define type (tree-type op0))
     (cond
       [(not (memq code '(gt_expr ge_expr lt_expr le_expr ne_expr))) #f]   ; EQ: number_of_iterations_cltz not ported
       [(not (or (eq? (type-code (gs) type) 'integer_type) (ty-ptr? type))) #f]
       [else
        (define stmt-loop (bb-loop src))
        (define iv0 (simple-iv l stmt-loop op0 #f))
        (define iv1 (and iv0 (simple-iv l stmt-loop op1 #f)))
        (cond
          [(not (and iv0 iv1)) #f]
          [else
           (set-affine-iv-base! iv0 (expand-simple-operations (affine-iv-base iv0) #f))
           (set-affine-iv-base! iv1 (expand-simple-operations (affine-iv-base iv1) #f))
           (define only-exit (= 1 (length (loop-exit-edges l))))
           (define d (number-of-iterations-cond l type iv0 code iv1 only-exit safe))
           (cond
             [(not d) #f]
             [else
              (set-niter-desc-assumptions! d (simplify-using-initial-conditions l (niter-desc-assumptions d)))
              (set-niter-desc-may-be-zero! d (simplify-using-initial-conditions l (niter-desc-may-be-zero d)))
              (when (icst? (niter-desc-niter d)) (set-niter-desc-max! d (icst-val (niter-desc-niter d))))
              (and (not (integer-zerop (niter-desc-assumptions d))) d)])])])]))

;; simple_iv_with_niters (no IV_NITERS): affine-iv or #f
(define (simple-iv wrto use op allow-nonconstant-step)
  (define type (tree-type op))
  (cond
    [(not (or (ty-ptr? type) (ty-int? type))) #f]
    [else
     (define-values (ev folded) (analyze-scalar-evolution-in-loop wrto use op))
     (cond
       [(or (chrec-contains-undetermined? ev) (chrec-contains-symbols-defined-in-loop? ev (loop-num wrto))) #f]
       [(not (tree-contains-chrecs? ev))
        (define et (if (ty-ptr? (tree-type ev)) (sizetype (gs)) (tree-type ev)))
        (affine-iv ev (icst et 0) #t)]
       [(not (poly? ev)) #f]
       [(not (= (chrec-var ev) (loop-num wrto))) #f]
       [else
        (define step (chrec-right ev))
        (cond
          [(or (and (not allow-nonconstant-step) (not (icst? step))) (tree-contains-chrecs? step)) #f]
          [(tree-contains-chrecs? (chrec-left ev)) #f]
          [else
           (define no-ovf0 (and (not folded) (nowrap-type-p type)))
           (define no-ovf (or no-ovf0 (not (iv-can-overflow? wrto type (chrec-left ev) step))))
           ;; the (signed T)((unsigned T)base + step) rewrite of the base is not ported
           (affine-iv (chrec-left ev) step no-ovf)])])]))

;; iv_can_overflow_p (GCC 15 form; GCC 12 is the same for SSA names and
;; constants): the base and step ranges against the loop's recorded
;; upper bound on the number of iterations
(define (iv-can-overflow? l type base step)
  (define (range-of x)
    (cond
      [(icst? x) (let* ([t (tree-type x)] [p (ty-prec t)] [v (icst-val x)])
                   (define sv (if (and (not (ty-uns? t)) (bitwise-bit-set? v (sub1 p))) (- v (arithmetic-shift 1 p)) v))
                   (cons sv sv))]
      [(and (ssa-name? x) (ty-int? (tree-type x))) (global-range x)]
      [else #f]))                                ; get_tree_range of an expression: not ported
  (cond
    [(integer-zerop step) #f]
    [else
     (define br (and (ty-int? (tree-type base)) (range-of base)))
     (define sr (and br (ty-int? (tree-type step)) (range-of step)))
     (define nit (and sr l (lbnd-upper (loop-bound (gs) (lctx-decl (C)) (Loop-num l)))))
     (cond
       [(not nit) #t]
       [else
        (define prec (ty-prec type))
        (define nit1 (add1 nit))
        (define tmin (ty-min type)) (define tmax (ty-max type))
        (define mask (sub1 (arithmetic-shift 1 prec)))
        (or (> (integer-length nit1) prec)
            (and (or (ty-uns? type) (>= (cdr sr) 0))
                 (let ([m (* (bitwise-and (cdr sr) mask) nit1)])
                   (or (> (integer-length m) prec) (> m (- tmax (cdr br))))))
            (and (not (ty-uns? type)) (< (car sr) 0)
                 (let ([m (* (- (car sr)) nit1)])
                   (or (> (integer-length m) prec) (> m (- (car br) tmin))))))])]))

;; number_of_iterations_cond: niter-desc or #f
(define (number-of-iterations-cond l type iv0* code* iv1* only-exit every-iteration)
  (define swap? (or (memq code* '(ge_expr gt_expr)) (and (eq? code* 'ne_expr) (integer-zerop (affine-iv-step iv0*)))))
  (define iv0 (if swap? iv1* iv0*)) (define iv1 (if swap? iv0* iv1*))
  (define code (if swap? (swap-cmp code*) code*))
  (define d (niter-desc (bool-true) (bool-false) #f 0 #f #f #f))
  (when (ty-ptr? type) (set-affine-iv-no-overflow! iv0 #t) (set-affine-iv-no-overflow! iv1 #t))
  (define exit-must-be-taken
    (and only-exit (or (and (not (integer-zerop (affine-iv-step iv0))) (affine-iv-no-overflow iv0))
                       (and (not (integer-zerop (affine-iv-step iv1))) (affine-iv-no-overflow iv1)))))
  (cond
    [(and (not every-iteration) (or (not (affine-iv-no-overflow iv0)) (not (affine-iv-no-overflow iv1)) (memq code '(ne_expr eq_expr)))) #f]
    [(and (not (integer-zerop (affine-iv-step iv0))) (not (integer-zerop (affine-iv-step iv1)))) #f]  ; both sides vary: not ported
    [(and (integer-zerop (affine-iv-step iv0)) (integer-zerop (affine-iv-step iv1))) #f]
    [else
     (define tem (gf-binary code (boolean-type (gs)) (affine-iv-base iv0) (affine-iv-base iv1)))
     (cond
       [(and tem (integer-zerop tem))
        (and every-iteration
             (begin (set-niter-desc-niter! d (icst (utype-for type) 0)) (set-niter-desc-max! d 0) d))]
       [else
        (define-values (below up) (bound-difference l (affine-iv-base iv1) (affine-iv-base iv0)))
        (case code
          [(ne_expr) (and (number-of-iterations-ne l type iv0 (affine-iv-base iv1) d exit-must-be-taken below up) d)]
          [(lt_expr) (and (number-of-iterations-lt l type iv0 iv1 d exit-must-be-taken below up) d)]
          [(le_expr) (and (number-of-iterations-le l type iv0 iv1 d exit-must-be-taken below up) d)]
          [else #f])])]))

;; number_of_iterations_ne: IV != FINAL
(define (number-of-iterations-ne l type iv final d exit-must-be-taken below0 up0)
  (define niter-type (utype-for type))
  (set-niter-desc-control! d iv) (set-niter-desc-bound! d final) (set-niter-desc-cmp! d 'ne_expr)
  (define step (affine-iv-step iv))
  (define neg? (and (icst? step) (bitwise-bit-set? (bitwise-and (icst-val step) (sub1 (arithmetic-shift 1 (ty-prec (tree-type step))))) (sub1 (ty-prec (tree-type step))))))
  ;; rearrange to S * i <> C with S positive, in the unsigned type
  (define s (if neg?
                (gf-build1 'negate_expr niter-type (gf-convert niter-type step))
                (gf-convert niter-type step)))
  (define c (if neg?
                (gf-build2 'minus_expr niter-type (gf-convert niter-type (affine-iv-base iv)) (gf-convert niter-type final))
                (gf-build2 'minus_expr niter-type (gf-convert niter-type final) (gf-convert niter-type (affine-iv-base iv)))))
  (define-values (below up) (if neg? (values (- up0) (- below0)) (values below0 up0)))
  ;; number_of_iterations_ne_max: for a constant C the count itself
  (set-niter-desc-max! d (if (and (icst? c) (icst? s) (= 1 (icst-val s))) (icst-val c) (ty-max niter-type)))
  (define (simple-cond?)
    ;; the no-overflow proof of the control IV: BASE evaluates toward FINAL
    ;; (or BASE - STEP does, after loop header copying)
    (cond
      [(affine-iv-no-overflow iv) #f]
      [(not (integer-onep s)) #f]                              ; multiple_of_p: not ported
      [else
       (define cnd (gf-build2 (if neg? 'ge_expr 'le_expr) (boolean-type (gs)) (affine-iv-base iv) final))
       (define t (simplify-using-initial-conditions l cnd))
       (and (icst? t) (= 1 (icst-val t)))]))
  (cond
    [(simple-cond?)
     (set-affine-iv-no-overflow! (niter-desc-control d) #t)
     (set-niter-desc-niter! d (gf-build2 'exact_div_expr niter-type c s))
     #t]
    [(and (icst? s))
     ;; nsd (step, size of mode): the count is inverse (s/d) * (c/d) mod 2^(prec - bits)
     (define sv (icst-val s))
     (define bits (let loop ([k 0]) (if (or (= k (ty-prec niter-type)) (bitwise-bit-set? sv k)) k (loop (add1 k)))))
     (define prec (ty-prec niter-type))
     (define bound (sub1 (arithmetic-shift 1 (- prec bits))))
     (define dv (arithmetic-shift 1 bits))
     (define s2 (arithmetic-shift sv (- bits)))
     (unless exit-must-be-taken
       (define assumption (gf-build2 'eq_expr (boolean-type (gs)) (gf-build2 'floor_mod_expr niter-type c (icst niter-type dv)) (icst niter-type 0)))
       (unless (integer-nonzerop* assumption)
         (set-niter-desc-assumptions! d (gf-build2 'truth_and_expr (boolean-type (gs)) (niter-desc-assumptions d) assumption))))
     (define c2 (gf-build2 'exact_div_expr niter-type c (icst niter-type dv)))
     (set-niter-desc-niter! d
       (if (= s2 1)
           c2
           (gf-build2 'bit_and_expr niter-type (gf-build2 'mult_expr niter-type c2 (icst niter-type (modular-inverse s2 (add1 bound)))) (icst niter-type bound))))
     #t]
    [else #f]))
(define (modular-inverse a m)
  (let loop ([t 0] [nt 1] [r m] [nr (modulo a m)])
    (if (zero? nr) (modulo t m) (let ([q (quotient r nr)]) (loop nt (- t (* q nt)) nr (- r (* q nr)))))))

(define (number-of-iterations-lt l type iv0 iv1 d exit-must-be-taken below up)
  (define niter-type (utype-for type))
  (if (not (integer-zerop (affine-iv-step iv0)))
      (begin (set-niter-desc-control! d iv0) (set-niter-desc-cmp! d 'lt_expr) (set-niter-desc-bound! d (affine-iv-base iv1)))
      (begin (set-niter-desc-control! d iv1) (set-niter-desc-cmp! d 'gt_expr) (set-niter-desc-bound! d (affine-iv-base iv0))))
  (define (sign-bit? x) (and (icst? x) (< (icst-val x) 0)))
  (cond
    [(or (sign-bit? (affine-iv-step iv0))
         (and (not (integer-zerop (affine-iv-step iv1))) (not (sign-bit? (affine-iv-step iv1)))))
     #f]                                                        ; number_of_iterations_until_wrap: not ported
    [else
     (define delta (gf-build2 'minus_expr niter-type (gf-convert niter-type (affine-iv-base iv1)) (gf-convert niter-type (affine-iv-base iv0))))
     (cond
       [(or (and (integer-onep (affine-iv-step iv0)) (integer-zerop (affine-iv-step iv1)))
            (and (all-ones? (affine-iv-step iv1)) (integer-zerop (affine-iv-step iv0))))
        (when (< below 0)
          (set-niter-desc-may-be-zero! d (gf-build2 'lt_expr (boolean-type (gs)) (affine-iv-base iv1) (affine-iv-base iv0))))
        (set-niter-desc-niter! d delta)
        (set-niter-desc-max! d (wrap-to niter-type up))
        #t]
       [else #f])]))                                            ; non-unit steps: not ported
(define (all-ones? x) (and (icst? x) (= (icst-val x) (if (ty-uns? (tree-type x)) (ty-max (tree-type x)) -1))))
(define (wrap-to t n) (let ([m (bitwise-and n (sub1 (arithmetic-shift 1 (ty-prec t))))]) m))

(define (number-of-iterations-le l type iv0 iv1 d exit-must-be-taken below up)
  (define type1 (if (ty-ptr? type) (sizetype (gs)) type))
  (define ok
    (cond
      [(and (not exit-must-be-taken) (not (ty-ptr? type)))
       (define assumption
         (if (not (integer-zerop (affine-iv-step iv0)))
             (gf-build2 'ne_expr (boolean-type (gs)) (affine-iv-base iv1) (icst type (ty-max type)))
             (gf-build2 'ne_expr (boolean-type (gs)) (affine-iv-base iv0) (icst type (ty-min type)))))
       (cond
         [(integer-zerop assumption) #f]
         [else
          (unless (integer-nonzerop* assumption)
            (set-niter-desc-assumptions! d (gf-build2 'truth_and_expr (boolean-type (gs)) (niter-desc-assumptions d) assumption)))
          #t])]
      [else #t]))
  (and ok
       (begin
         (if (not (integer-zerop (affine-iv-step iv0)))
             (set-affine-iv-base! iv1 (if (ty-ptr? type)
                                          (gf-build2 'pointer_plus_expr type (affine-iv-base iv1) (icst (sizetype (gs)) 1))
                                          (gf-build2 'plus_expr type1 (affine-iv-base iv1) (icst type1 1))))
             (set-affine-iv-base! iv0 (if (ty-ptr? type)
                                          (gf-build2 'pointer_plus_expr type (affine-iv-base iv0) (icst (sizetype (gs)) -1))
                                          (gf-build2 'minus_expr type1 (affine-iv-base iv0) (icst type1 1)))))
         (number-of-iterations-lt l type iv0 iv1 d exit-must-be-taken (add1 below) (add1 up)))))

;; bound_difference: (values below up) of X - Y in full precision
(define (split-to-var-and-offset e)
  (case (and (Tree? e) (tree-code e))
    [(plus_expr pointer_plus_expr minus_expr)
     (define op1 (tree-operand e 1))
     (if (icst? op1)
         (values (tree-operand e 0) (let ([v (signed-value op1)]) (if (eq? (tree-code e) 'minus_expr) (- v) v)))
         (values e 0))]
    [(integer_cst) (values (icst (tree-type e) 0) (icst-val e))]
    [else (values e 0)]))
(define (signed-value c)
  (define t (tree-type c)) (define v (icst-val c)) (define p (ty-prec t))
  (if (and (ty-uns? t) (bitwise-bit-set? v (sub1 p))) (- v (arithmetic-shift 1 p)) v))

(define (dominating-guards l)
  ;; the conditions on the single-predecessor dominators of the header:
  ;; list of (list c0 cmp c1) with the comparison as it holds on the edge
  (define cfg (lctx-cfg (C)))
  (let loop ([bb (Loop-header l)] [acc '()] [cnt 0])
    (cond
      [(or (not bb) (= bb ENTRY) (>= cnt 8)) (reverse acc)]
      [(not (single-pred-p cfg bb)) (loop (idom bb) acc cnt)]
      [else
       (define p (pvector-ref (cfg-preds cfg bb) 0))
       (define e (find-edge cfg p bb))
       (cond
         [(not (or (edge-flag? e EDGE-TRUE-VALUE) (edge-flag? e EDGE-FALSE-VALUE))) (loop (idom bb) acc cnt)]
         [else
          (define g (bb-last-stmt cfg p))
          (define cmp (if (edge-flag? e EDGE-FALSE-VALUE) (invert-cmp (gcond-code g)) (gcond-code g)))
          (loop (idom bb) (cons (list (gcond-lhs g) cmp (gcond-rhs g)) acc) (add1 cnt))])])))

(define (global-range v)
  (define r (gimple-range-global (gs) (lctx-decl (C)) v))
  (and r (not (ir-varying? r)) (not (ir-undefined? r)) (cons (ir-lower r) (ir-upper r))))

(define (determine-value-range l type var off)
  (define tmin (ty-min type)) (define tmax (ty-max type))
  (cond
    [(integer-zerop var) (values off off)]
    [(and (Tree? var) (eq? (tree-code var) 'ssa_name) (ty-int? type))
     (define cfg (lctx-cfg (C)))
     (define pre (loop-preheader-src l))
     (define vr (global-range var))
     (define-values (minv maxv rtype)
       (for/fold ([minv (and vr (car vr))] [maxv (and vr (cdr vr))] [rtype (if vr 'range 'varying)])
                 ([ph (in-pvector (GBlock-phis (cfg-block cfg (Loop-header l))))])
         (define a (and pre (for/first ([a (in-pvector (gphi-args ph))] #:when (= (car a) pre)) (cdr a))))
         (define pr (and a (operand-equal? a var) (global-range (gphi-result ph))))
         (cond
           [(not pr) (values minv maxv rtype)]
           [(not (eq? rtype 'range)) (values (car pr) (cdr pr) 'range)]
           [else (values (max minv (car pr)) (min maxv (cdr pr)) 'range)])))
     (define-values (minm0 maxm0) (if (eq? rtype 'range) (values minv maxv) (values tmin tmax)))
     (define-values (minm maxm)
       (for/fold ([lo minm0] [hi maxm0]) ([gd (in-list (dominating-guards l))])
         (refine-value-range-using-guard type var (car gd) (cadr gd) (caddr gd) lo hi)))
     (define lo (+ minm off)) (define hi (+ maxm off))
     (cond
       [(or (nowrap-type-p type) (zero? off) (and (< off 0) (>= lo tmin)) (and (> off 0) (<= hi tmax))) (values lo hi)]
       [else (static-range type off tmin tmax)])]
    [else (static-range type off tmin tmax)]))
(define (static-range type off tmin tmax)
  (cond [(not (nowrap-type-p type)) (values tmin tmax)]
        [(< off 0) (values tmin (+ tmax off))]
        [else (values (+ tmin off) tmax)]))

(define (refine-value-range-using-guard type var c0* cmp* c1* below up)
  (define no-wrap (nowrap-type-p type))
  (define tmin (ty-min type)) (define tmax (ty-max type))
  (define-values (c0 c1 cmp ok)
    (case cmp*
      [(lt_expr le_expr gt_expr ge_expr)
       (define a (gf-strip-sign-nops c0*)) (define b (gf-strip-sign-nops c1*))
       (values a b cmp* (useless-conv? (tree-type a) type))]
      [else (values c0* c1* cmp* #f)]))                         ; EQ / NE guards: not ported
  (cond
    [(not ok) (values below up)]
    [else
     (define-values (varc0a offc0a) (split-to-var-and-offset (expand-simple-operations c0 #f)))
     (define-values (varc1a offc1a) (split-to-var-and-offset (expand-simple-operations c1 #f)))
     (define-values (varc0 offc0 varc1 offc1 cmp1)
       (cond
         [(operand-equal? var varc1a) (values varc1a offc1a varc0a offc0a (swap-cmp cmp))]
         [else (values varc0a offc0a varc1a offc1a cmp)]))
     (cond
       [(not (operand-equal? var varc0)) (values below up)]
       [else
        (define-values (minc1a maxc1a)
          (cond
            [(integer-zerop varc1) (values 0 0)]
            [(and (Tree? varc1) (eq? (tree-code varc1) 'ssa_name) (global-range varc1)) => (lambda (r) (values (car r) (cdr r)))]
            [else (values tmin tmax)]))
        (define minc1b (+ minc1a offc1)) (define maxc1b (+ maxc1a offc1))
        (define c1-ok (or no-wrap (zero? offc1) (and (< offc1 0) (>= minc1b tmin)) (and (> offc1 0) (<= maxc1b tmax))))
        (cond
          [(not c1-ok) (values below up)]
          [else
           (define minc1c (max minc1b tmin)) (define maxc1c (min maxc1b tmax))
           (define-values (cmp2 minc1d maxc1d)
             (case cmp1
               [(lt_expr) (values 'le_expr minc1c (sub1 maxc1c))]
               [(gt_expr) (values 'ge_expr (add1 minc1c) maxc1c)]
               [else (values cmp1 minc1c maxc1c)]))
           (define minc1 (- minc1d offc0)) (define maxc1 (- maxc1d offc0))
           (define c0-ok (or no-wrap (zero? offc0)
                             (and (eq? cmp2 'le_expr) (< offc0 0) (<= maxc1 tmax))
                             (and (eq? cmp2 'ge_expr) (> offc0 0) (>= minc1 tmin))))
           (cond
             [(not c0-ok) (values below up)]
             [(eq? cmp2 'le_expr) (values below (min up maxc1))]
             [else (values (max below minc1) up)])])])]))

(define (bound-difference l x* y*)
  (define x (gf-strip-sign-nops x*)) (define y (gf-strip-sign-nops y*))
  (define type (tree-type x))
  (define-values (varx offx) (split-to-var-and-offset x))
  (define-values (vary offy) (split-to-var-and-offset y))
  (define-values (below0 up0)
    (cond
      [(and (not (integer-zerop varx)) (operand-equal? varx vary))
       (bound-difference-of-offsetted-base type offx offy)]
      [else
       (define-values (minx maxx) (determine-value-range l type varx offx))
       (define-values (miny maxy) (determine-value-range l type vary offy))
       (values (- minx maxy) (- maxx miny))]))
  (cond
    [(and (integer-zerop varx) (integer-zerop vary)) (values below0 up0)]
    [else
     (for/fold ([below below0] [up up0]) ([gd (in-list (dominating-guards l))])
       (refine-bounds-using-guard type varx offx vary offy (car gd) (cadr gd) (caddr gd) below up))]))

(define (bound-difference-of-offsetted-base type x y)
  (define rel (- x y))
  (cond
    [(zero? rel) (values 0 0)]
    [else
     (define m (arithmetic-shift 1 (ty-prec type)))
     (define may-wrap (not (nowrap-type-p type)))
     (cond
       [(not may-wrap) (values rel rel)]
       [(> rel 0) (values (- rel m) rel)]
       [else (values rel (+ rel m))])]))

(define (refine-bounds-using-guard type varx offx vary offy c0* cmp* c1* below up)
  (define no-wrap (nowrap-type-p type))
  (define-values (c0 c1 cmp ok)
    (case cmp*
      [(lt_expr le_expr gt_expr ge_expr)
       (define a (gf-strip-sign-nops c0*)) (define b (gf-strip-sign-nops c1*))
       (values a b cmp* (useless-conv? (tree-type a) type))]
      [else (values c0* c1* cmp* #f)]))
  (cond
    [(not ok) (values below up)]
    [else
     (define-values (vc0a oc0a) (split-to-var-and-offset (expand-simple-operations c0 #f)))
     (define-values (vc1a oc1a) (split-to-var-and-offset (expand-simple-operations c1 #f)))
     (define-values (varc0 offc0 varc1 offc1 cmp1)
       (if (operand-equal? varx vc1a) (values vc1a oc1a vc0a oc0a (swap-cmp cmp)) (values vc0a oc0a vc1a oc1a cmp)))
     (cond
       [(not (and (operand-equal? varx varc0) (operand-equal? vary varc1))) (values below up)]
       [else
        (define-values (lbound oc0 oc1 lx ly cmp2)
          (if (memq cmp1 '(gt_expr ge_expr))
              (values #t offc1 offc0 offy offx (swap-cmp cmp1))
              (values #f offc0 offc1 offx offy cmp1)))
        (define vx (if lbound vary varx)) (define vy (if lbound varx vary))
        (define x-ok (or no-wrap (integer-zerop vx) (>= lx oc0)))
        (define y-ok (or no-wrap (integer-zerop vy) (<= ly oc1)))
        (cond
          [(not (and x-ok y-ok)) (values below up)]
          [else
           (define bnd0 (+ (- lx ly) oc1 (- oc0)))
           (define bnd (if (eq? cmp2 'lt_expr) (sub1 bnd0) bnd0))
           (if lbound
               (values (max below (- bnd)) up)
               (values below (min up bnd)))])])]))

;; expand_simple_operations
(define (expand-simple-operations e stop)
  (cond
    [(not e) e]
    [(min-invariant? e) e]
    [(and (Tree? e) (not (eq? (tree-code e) 'ssa_name)) (memq (tree-code-class (gs) (tree-code e)) '(tcc_unary tcc_binary tcc_comparison tcc_expression)))
     (define ops (for/list ([i (in-range (tree-operand-length e))]) (tree-operand e i)))
     (define new (for/list ([o (in-list ops)]) (if o (expand-simple-operations o stop) o)))
     (cond
       [(andmap eq? ops new) e]
       [else
        (case (length new)
          [(1) (gf-build1 (tree-code e) (tree-type e) (car new))]
          [(2) (gf-build2 (tree-code e) (tree-type e) (car new) (cadr new))]
          [(3) (gf-build3 (tree-code e) (tree-type e) (car new) (cadr new) (caddr new))]
          [else e])])]
    [(or (not (and (Tree? e) (eq? (tree-code e) 'ssa_name))) (and stop (operand-equal? e stop))) e]
    [else
     (define g (def-stmt-of e))
     (cond
       [(not g) e]
       [(gphi? g)
        (cond
          [(not (= 1 (pvector-length (gphi-args g)))) e]
          [else
           (define a (cdr (pvector-ref (gphi-args g) 0)))
           (define dest (def-bb-of e))
           (define src (pvector-ref (cfg-preds (lctx-cfg (C)) dest) 0))
           (if (and (Tree? a) (eq? (tree-code a) 'ssa_name) (not (same-loop? (bb-loop src) (bb-loop dest))))
               e
               (expand-simple-operations a stop))])]
       [(not (gassign? g)) e]
       [else
        (define code (gassign-code g)) (define r1 (gassign-rhs1 g))
        (case (gimple-rhs-class (gs) code)
          [(single)
           (cond [(min-invariant? r1) r1]
                 [(eq? code 'ssa_name) (expand-simple-operations r1 stop)]
                 [else e])]
          [else
           (case code
             [(nop_expr convert_expr) (gf-build1 code (tree-type e) (expand-simple-operations r1 stop))]
             [(plus_expr minus_expr mult_expr pointer_plus_expr)
              (define r2 (gassign-rhs2 g))
              (if (min-invariant? r2)
                  (gf-build2 code (tree-type e) (expand-simple-operations r1 stop) r2)
                  e)]
             [else e])])])]))

;; simplify_using_initial_conditions: only constant results of the
;; dominating conditions are used (tree_simplify_using_condition_1 with
;; fold of truth operations on constants)
(define (simplify-using-initial-conditions l e)
  (cond
    [(icst? e) e]
    [else
     (define folded (gf-binary* e))
     (if (and folded (icst? folded)) folded e)]))
;; fold of a comparison tree (constant operands)
(define (gf-binary* e)
  (and (Tree? e) (comparison-code? (tree-code e))
       (gf-binary (tree-code e) (tree-type e) (tree-operand e 0) (tree-operand e 1))))

;; ------------------------------------------------------------
;; expression_expensive_p: (values expensive? cond-overflow?)
;; ------------------------------------------------------------

(define (expression-expensive-p e)
  (define cache (make-hash))
  (define cost (box 0))
  (define cond-ovf (box #f))
  (define (gimple-val? x) (or (min-invariant? x) (and (Tree? x) (eq? (tree-code x) 'ssa_name))))
  (define (rec x cost)
    (cond
      [(gimple-val? x) #f]
      [else
       (define code (tree-code x))
       (cond
         [(and (memq code '(trunc_div_expr ceil_div_expr floor_div_expr round_div_expr trunc_mod_expr ceil_mod_expr floor_mod_expr round_mod_expr exact_div_expr))
               (not (let ([d (tree-operand x 1)]) (and (icst? d) (> (icst-val d) 0) (= (bitwise-and (icst-val d) (sub1 (icst-val d))) 0)))))
          #t]
         [(hash-ref cache x #f)
          => (lambda (lc) (set-box! cost (+ (unbox cost) (unbox lc))) #f)]
         [else
          (define local (box 1))
          (hash-set! cache x local)
          (define op-cost (box 0))
          (define cls (tree-code-class (gs) code))
          (cond
            [(eq? code 'cond_expr)
             (cond
               [(or (rec (tree-operand x 0) op-cost)
                    (and (expr-node? (tree-operand x 1)) (expr-node? (tree-operand x 2)))
                    (rec (tree-operand x 1) op-cost) (rec (tree-operand x 2) op-cost))
                #t]
               [else (set-box! cond-ovf #t) (set-box! local (+ (unbox local) (unbox op-cost)))
                     (set-box! cost (+ (unbox cost) (unbox op-cost) 1)) #f])]
            [(memq cls '(tcc_binary tcc_comparison))
             (cond [(or (rec (tree-operand x 1) op-cost) (rec (tree-operand x 0) op-cost)) #t]
                   [else (set-box! local (+ (unbox local) (unbox op-cost))) (set-box! cost (+ (unbox cost) (unbox op-cost) 1)) #f])]
            [(eq? cls 'tcc_unary)
             (cond [(rec (tree-operand x 0) op-cost) #t]
                   [else (set-box! local (+ (unbox local) (unbox op-cost))) (set-box! cost (+ (unbox cost) (unbox op-cost) 1)) #f])]
            [else #t])])]))
  (define exp? (rec e cost))
  (values (or exp? (> (unbox cost) (add1 (hash-count cache)))) (unbox cond-ovf)))
(define (expr-node? x) (and (Tree? x) (not (memq (tree-code x) '(ssa_name integer_cst real_cst var_decl parm_decl)))))

;; ------------------------------------------------------------
;; find_loop_niter / find_loop_niter_by_eval
;; ------------------------------------------------------------

;; just_once_each_iteration_p (no irreducible regions)
(define (just-once-each-iteration? l bb)
  (and (dominated-by? (loop-latch l) bb) (same-loop? (bb-loop bb) l)))

;; find_loop_niter: (values niter exit)
(define (find-loop-niter l)
  (let loop ([exits (loop-exit-edges l)] [niter #f] [exit #f])
    (cond
      [(null? exits) (values (or niter 'dont-know) exit)]
      [else
       (define ex (car exits))
       (define d (number-of-iterations-exit l ex))
       (cond
         [(not d) (loop (cdr exits) niter exit)]
         [(integer-nonzerop* (niter-desc-may-be-zero d)) (values (icst (find-unsigned-int) 0) ex)]
         [(not (integer-zerop (niter-desc-may-be-zero d))) (loop (cdr exits) niter exit)]
         [else
          (define a (niter-desc-niter d))
          (cond
            [(not niter) (loop (cdr exits) a ex)]
            [(not (icst? a)) (loop (cdr exits) niter exit)]
            [(not (icst? niter)) (loop (cdr exits) a ex)]
            [(< (icst-val a) (icst-val niter)) (loop (cdr exits) a ex)]
            [else (loop (cdr exits) niter exit)])])])))
(define (find-unsigned-int) (utype-for (integer-type-of-precision 32)))
(define (integer-type-of-precision p)
  (for/first ([q (in-list (intmap->literal-datum (GS-types (gs))))]
              #:when (let ([t (cdr q)]) (and (eq? (tree-code t) 'integer_type) (eqv? (tree-attr t 'precision #f) p)
                                             (not (tree-attr t 'unsigned #f)) (not (tree-attr t 'main-variant #f)))))
    (car q)))

;; find_loop_niter_by_eval at -O1 (no -fexpensive-optimizations): (values niter exit)
(define max-iterations-to-track 1000)
(define (find-loop-niter-by-eval l)
  (define exits (loop-exit-edges l))
  (cond
    [(> (length exits) 1) (values 'dont-know #f)]
    [else
     (for/fold ([niter #f] [exit #f] #:result (values (or niter 'dont-know) exit))
               ([ex (in-list exits)] #:when (just-once-each-iteration? l (car ex)))
       (define a (loop-niter-by-eval l ex))
       (cond
         [(chrec-contains-undetermined? a) (values niter exit)]
         [(and niter (not (< (icst-val a) (icst-val niter)))) (values niter exit)]
         [else (values a ex)]))]))

(define (chain-of-csts-start l x)
  (define g (def-stmt-of x))
  (define bb (def-bb-of x))
  (cond
    [(or (not bb) (not (loop-inside? l bb))) #f]
    [(gphi? g) (and (= bb (Loop-header l)) g)]
    [(not (gassign? g)) #f]
    [(memq (gimple-rhs-class (gs) (gassign-code g)) '(ternary)) #f]
    [(or (stmt-vuse* g) (memq (tree-code-class (gs) (gassign-code g)) '(tcc_reference))
         (and (eq? (gassign-code g) 'addr_expr) (not (min-invariant? (gassign-rhs1 g)))))
     #f]
    [else
     (define uses (filter (lambda (t) (and (Tree? t) (eq? (tree-code t) 'ssa_name))) (list (gassign-rhs1 g) (gassign-rhs2 g))))
     (and (= 1 (length uses)) (chain-of-csts-start l (car uses)))]))
(define (stmt-vuse* g) (stmt-flag g 'vuse #f))

(define (get-base-for l x)
  (cond
    [(min-invariant? x) #f]
    [else
     (define ph (chain-of-csts-start l x))
     (cond
       [(not ph) #f]
       [else
        (define pre (loop-preheader-src l))
        (define init (phi-arg ph pre)) (define next (phi-arg ph (loop-latch l)))
        (cond
          [(not (min-invariant? init)) #f]
          [(and (Tree? next) (eq? (tree-code next) 'ssa_name) (not (eq? (chain-of-csts-start l next) ph))) #f]
          [else ph])])]))
(define (phi-arg ph pred) (for/first ([a (in-pvector (gphi-args ph))] #:when (= (car a) pred)) (cdr a)))

(define (get-val-for x base)
  (cond
    [(not x) base]
    [(min-invariant? x) x]
    [else
     (define g (def-stmt-of x))
     (cond
       [(gphi? g) base]
       [(and (eq? (gassign-code g) 'ssa_name)) (get-val-for (gassign-rhs1 g) base)]
       [(and (eq? (gimple-rhs-class (gs) (gassign-code g)) 'unary) (Tree? (gassign-rhs1 g)) (eq? (tree-code (gassign-rhs1 g)) 'ssa_name))
        (gf-build1 (gassign-code g) (tree-type (gassign-lhs g)) (get-val-for (gassign-rhs1 g) base))]
       [else
        (define r1 (gassign-rhs1 g)) (define r2 (gassign-rhs2 g))
        (define-values (a b)
          (if (and (Tree? r1) (eq? (tree-code r1) 'ssa_name)) (values (get-val-for r1 base) r2) (values r1 (get-val-for r2 base))))
        (gf-build2 (gassign-code g) (tree-type (gassign-lhs g)) a b)])]))

(define (loop-niter-by-eval l ex)
  (define cfg (lctx-cfg (C)))
  (define g (bb-last-stmt cfg (car ex)))
  (cond
    [(not (gcond? g)) 'dont-know]
    [else
     (define cmp (if (edge-flag? (find-edge cfg (car ex) (cdr ex)) EDGE-TRUE-VALUE) (invert-cmp (gcond-code g)) (gcond-code g)))
     (cond
       [(not (memq cmp '(eq_expr ne_expr gt_expr ge_expr lt_expr le_expr))) 'dont-know]
       [else
        (define ops (list (gcond-lhs g) (gcond-rhs g)))
        (define bases
          (for/list ([o (in-list ops)])
            (if (min-invariant? o)
                (list #f o #f)
                (let ([ph (get-base-for l o)])
                  (and ph (list o (phi-arg ph (loop-preheader-src l)) (phi-arg ph (loop-latch l))))))))
        (cond
          [(memq #f bases) 'dont-know]                       ; the string-compare case is not ported
          [else
           (let loop ([i 0] [vals (map cadr bases)])
             (cond
               [(>= i max-iterations-to-track) 'dont-know]
               [else
                (define avals (for/list ([b (in-list bases)] [v (in-list vals)]) (get-val-for (car b) v)))
                (define acnd (gf-binary cmp (boolean-type (gs)) (car avals) (cadr avals)))
                (cond
                  [(and acnd (integer-zerop acnd)) (icst (find-unsigned-int) i)]
                  [else
                   (define nvals (for/list ([b (in-list bases)] [v (in-list vals)]) (get-val-for (caddr b) v)))
                   (cond
                     [(not (andmap min-invariant? nvals)) 'dont-know]
                     [(and (operand-equal? (car nvals) (car vals)) (operand-equal? (cadr nvals) (cadr vals))) 'dont-know]
                     [else (loop (add1 i) nvals)])])]))])])]))
