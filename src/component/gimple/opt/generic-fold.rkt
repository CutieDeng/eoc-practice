#lang racket/base

;; ============================================================
;; opt: GENERIC folding for the loop analyses (gcc/fold-const.cc
;; fold_convert / fold_build1 / fold_build2 / const_binop and the
;; generic_simplify patterns of match.pd that fire on the trees the
;; scalar evolution and iteration count analyses build)
;; ============================================================
;;
;; The functions read the GIMPLE state from a parameter (with-gs) so that
;; unsigned_type_for can register a type deep inside an analysis:
;;   (with-gs st thunk) -> (values result st*)   (gs) / (gs! st)
;; Types:  (ty-prec t) (ty-uns? t) (ty-int? t) (ty-ptr? t)
;;         (ty-wraps? t)       TYPE_OVERFLOW_WRAPS
;;         (ty-undef? t)       TYPE_OVERFLOW_UNDEFINED
;;         (nowrap-type-p t)   (tree-ssa-loop-niter.cc)
;;         (utype-for t) (stype-for t)   unsigned_type_for / signed_type_for
;;         (ty-min t) (ty-max t)          bounds as exact integers
;; Constants: (icst t n) builds an INTEGER_CST (wrapped), (icst? x),
;;         (icst-val x), (int-fits-type? n t)
;; Folding: (gf-convert t e) fold_convert, (gf-build1 code t a),
;;         (gf-build2 code t a b), (gf-build3 code t a b c),
;;         (gf-binary code t a b) -> tree or #f (fold_binary),
;;         (const-binop code t a b) -> (values tree overflow?)
;; Predicates: (operand-equal? a b), (tree-nop-conversion-p outer inner),
;;         (negate-expr-p e), (gf-strip-nops e), (gf-strip-sign-nops e)
;;
;; Patterns ported (in fold_binary order: constants, canonical operand
;; order, generic_simplify, then the fold-const.cc cases):
;;   x + 0, x - 0, x * 0, x * 1, x - x
;;   (A +- CST1) +- CST2 -> A + CST3 (also through a nop conversion)
;;   (T)(A +- B) for nested conversions (the "two conversions in a row"
;;   rules), (T)x with T the type of x
;;   A - CST -> A + -CST (negate_expr_p)
;;   (A * C) +- A, (A * C1) +- (A * C2), (A * C) +- C (fold_plusminus_mult_expr)
;;   (A * C1) * C2 -> A * (C1 * C2)
;;   comparisons of constants, and with operands swapped to canonical order
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../state.rkt"
         "../tree.rkt"
         (only-in "../gimplify.rkt" tree-swap-operands-p unsigned-type-for))

(provide with-gs gs gs!
         ty-prec ty-uns? ty-int? ty-ptr? ty-wraps? ty-undef? nowrap-type-p utype-for stype-for ty-min ty-max
         nonstandard-int-type
         icst icst? icst-val int-fits-type? boolean-cst
         operand-equal? tree-nop-conversion-p negate-expr-p gf-strip-nops gf-strip-sign-nops
         gf-convert gf-build1 gf-build2 gf-build3 gf-binary const-binop gf-negate
         comparison-code? invert-cmp swap-cmp commutative-code?)

;; ------------------------------------------------------------
;; the state
;; ------------------------------------------------------------

(define gs-box (make-parameter #f))
(define (gs) (unbox (gs-box)))
(define (gs! s) (set-box! (gs-box) s))
(define (with-gs st thunk)
  (define b (box st))
  (define r (parameterize ([gs-box b]) (thunk)))
  (values r (unbox b)))

;; ------------------------------------------------------------
;; types
;; ------------------------------------------------------------

(define (ty-ptr? t) (pointer-type-p (gs) t))
(define (ty-int? t) (integral-type-p (gs) t))
(define (ty-prec t) (or (type-attr (gs) t 'precision #f) (if (ty-ptr? t) 64 64)))
(define (ty-uns? t) (or (ty-ptr? t) (and (type-attr (gs) t 'unsigned #f) #t)))
;; no -fwrapv / -fwrapv-pointer
(define (ty-wraps? t) (and (not (ty-ptr? t)) (ty-uns? t)))
(define (ty-undef? t) (or (ty-ptr? t) (and (ty-int? t) (not (ty-uns? t)))))
(define (nowrap-type-p t) (or (ty-ptr? t) (and (ty-int? t) (ty-undef? t))))
(define (ty-min t) (if (ty-uns? t) 0 (- (arithmetic-shift 1 (sub1 (ty-prec t))))))
(define (ty-max t) (if (ty-uns? t) (sub1 (arithmetic-shift 1 (ty-prec t))) (sub1 (arithmetic-shift 1 (sub1 (ty-prec t))))))

;; an integer type of precision PREC and signedness UNS? from the type
;; table: the main variant without qualifiers; for pointers the
;; unsigned type of pointer precision (sizetype's precision)
(define (find-int-type prec uns?)
  (for/first ([p (in-list (intmap->literal-datum (GS-types (gs))))]
              #:when (let ([t (cdr p)])
                       (and (eq? (tree-code t) 'integer_type)
                            (eq? (and (tree-attr t 'unsigned #f) #t) uns?)
                            (eqv? (tree-attr t 'precision #f) prec)
                            (zero? (tree-attr t 'quals 0))
                            (not (tree-attr t 'main-variant #f))
                            (not (tree-attr t 'sizetype #f))
                            (not (tree-attr t 'nonstandard #f)))))
    (car p)))
;; build_nonstandard_integer_type (PREC, UNS?): a nameless INTEGER_TYPE,
;; one per precision and signedness (nonstandard_integer_type_cache);
;; printed as "unsigned long" etc. (print.rkt)
(define (nonstandard-int-type prec uns?)
  (or (for/first ([p (in-list (intmap->literal-datum (GS-types (gs))))]
                  #:when (let ([t (cdr p)])
                           (and (eq? (tree-code t) 'integer_type) (tree-attr t 'nonstandard #f)
                                (eq? (and (tree-attr t 'unsigned #f) #t) uns?)
                                (eqv? (tree-attr t 'precision #f) prec))))
        (car p))
      (let* ([like (or (find-int-type prec uns?) (find-int-type prec (not uns?)))]
             [base (type-ref (gs) like)])
        (let-values ([(uid st) (fresh-type-uid (gs))])
          (define t (make-tree 'integer_type
                               #:attrs (hash-set* (hash-remove (hash-remove (Tree-attrs base) 'main-variant) 'name)
                                                  'uid uid 'unsigned uns? 'nonstandard #t)))
          (let-values ([(u st2) (add-type st t)]) (gs! st2) u)))))
;; signed_or_unsigned_type_for: T itself when it already has that
;; signedness, else build_nonstandard_integer_type of T's precision
(define (signed-or-unsigned-type-for uns? t)
  (cond
    [(and (ty-int? t) (eq? (ty-uns? t) uns?) (not (ty-ptr? t))) t]
    [else (nonstandard-int-type (ty-prec t) uns?)]))
(define (utype-for t) (signed-or-unsigned-type-for #t t))
(define (stype-for t) (signed-or-unsigned-type-for #f t))
(define (unsigned-type-for* st t) (unsigned-type-for st t))
(define (signed-type-for* st t) (error 'signed-type-for "no signed type of precision ~a" (type-attr st t 'precision)))

;; ------------------------------------------------------------
;; constants
;; ------------------------------------------------------------

(define (icst? x) (and (Tree? x) (eq? (tree-code x) 'integer_cst)))
(define (icst-val x) (integer-cst-value x))
(define (icst t n) (build-int-cst (gs) t n))
(define (boolean-cst v) (icst (boolean-type (gs)) (if v 1 0)))
(define (int-fits-type? n t) (and (>= n (ty-min t)) (<= n (ty-max t))))
;; the value of X reinterpreted in T's signedness and precision
(define (wrap t n)
  (define prec (ty-prec t))
  (define m (bitwise-and n (sub1 (arithmetic-shift 1 prec))))
  (if (and (not (ty-uns? t)) (bitwise-bit-set? m (sub1 prec))) (- m (arithmetic-shift 1 prec)) m))

(define (comparison-code? c) (and (memq c '(lt_expr le_expr gt_expr ge_expr eq_expr ne_expr)) #t))
(define (commutative-code? c) (and (memq c '(plus_expr mult_expr min_expr max_expr bit_ior_expr bit_xor_expr bit_and_expr ne_expr eq_expr truth_and_expr truth_or_expr truth_xor_expr)) #t))
(define (swap-cmp c) (case c [(lt_expr) 'gt_expr] [(gt_expr) 'lt_expr] [(le_expr) 'ge_expr] [(ge_expr) 'le_expr] [else c]))
(define (invert-cmp c) (case c [(lt_expr) 'ge_expr] [(ge_expr) 'lt_expr] [(gt_expr) 'le_expr] [(le_expr) 'gt_expr] [(eq_expr) 'ne_expr] [(ne_expr) 'eq_expr] [else #f]))

;; const_binop / int_const_binop: (values result overflow?) or (values #f #f)
(define (const-binop code t a b)
  (cond
    [(and (icst? a) (icst? b))
     (define x (icst-val a)) (define y (icst-val b))
     (define (arith v) (values (icst t v) (not (int-fits-type? v t))))
     (case code
       [(plus_expr pointer_plus_expr) (arith (+ x y))]
       [(minus_expr) (arith (- x y))]
       [(mult_expr) (arith (* x y))]
       [(trunc_div_expr exact_div_expr) (if (zero? y) (values #f #f) (arith (quotient x y)))]
       [(floor_div_expr) (if (zero? y) (values #f #f) (arith (floor (/ x y))))]
       [(ceil_div_expr) (if (zero? y) (values #f #f) (arith (ceiling (/ x y))))]
       [(trunc_mod_expr) (if (zero? y) (values #f #f) (arith (remainder x y)))]
       [(bit_and_expr) (values (icst t (bitwise-and x y)) #f)]
       [(bit_ior_expr) (values (icst t (bitwise-ior x y)) #f)]
       [(bit_xor_expr) (values (icst t (bitwise-xor x y)) #f)]
       [(min_expr) (values (icst t (min x y)) #f)]
       [(max_expr) (values (icst t (max x y)) #f)]
       [(lshift_expr) (arith (arithmetic-shift x y))]
       [(rshift_expr) (arith (arithmetic-shift x (- y)))]
       [(lt_expr) (values (icst t (if (< x y) 1 0)) #f)]
       [(le_expr) (values (icst t (if (<= x y) 1 0)) #f)]
       [(gt_expr) (values (icst t (if (> x y) 1 0)) #f)]
       [(ge_expr) (values (icst t (if (>= x y) 1 0)) #f)]
       [(eq_expr) (values (icst t (if (= x y) 1 0)) #f)]
       [(ne_expr) (values (icst t (if (= x y) 0 1)) #f)]
       [(truth_and_expr truth_andif_expr) (values (icst t (if (and (not (zero? x)) (not (zero? y))) 1 0)) #f)]
       [(truth_or_expr truth_orif_expr) (values (icst t (if (or (not (zero? x)) (not (zero? y))) 1 0)) #f)]
       [else (values #f #f)])]
    [else (values #f #f)]))

;; ------------------------------------------------------------
;; predicates
;; ------------------------------------------------------------

;; operand_equal_p (flags 0): constants by value, precision and sign
(define (operand-equal? a b)
  (cond
    [(eq? a b) #t]
    [(or (not (Tree? a)) (not (Tree? b))) (equal? a b)]
    [(not (eq? (tree-code a) (tree-code b))) #f]
    [else
     (case (tree-code a)
       [(integer_cst) (and (= (icst-val a) (icst-val b))
                           (eq? (ty-uns? (tree-type a)) (ty-uns? (tree-type b)))
                           (= (ty-prec (tree-type a)) (ty-prec (tree-type b))))]
       [(ssa_name) (= (ssa-name-version a) (ssa-name-version b))]
       [(var_decl parm_decl result_decl function_decl label_decl field_decl const_decl) (eqv? (decl-uid a) (decl-uid b))]
       [(polynomial_chrec)
        (and (= (tree-attr a 'var) (tree-attr b 'var))
             (operand-equal? (tree-operand a 0) (tree-operand b 0))
             (operand-equal? (tree-operand a 1) (tree-operand b 1)))]
       [else
        (and (= (tree-operand-length a) (tree-operand-length b))
             (or (not (memq (tree-code a) '(nop_expr convert_expr)))
                 (and (eq? (ty-uns? (tree-type a)) (ty-uns? (tree-type b)))
                      (= (ty-prec (tree-type a)) (ty-prec (tree-type b)))))
             (for/and ([i (in-range (tree-operand-length a))])
               (operand-equal? (tree-operand a i) (tree-operand b i))))])]))

;; tree_nop_conversion_p: same precision, both integral / pointer
(define (tree-nop-conversion-p outer inner)
  (and (or (ty-int? outer) (ty-ptr? outer)) (or (ty-int? inner) (ty-ptr? inner))
       (= (ty-prec outer) (ty-prec inner))))

(define (nop? e) (and (Tree? e) (memq (tree-code e) '(nop_expr convert_expr non_lvalue_expr)) #t))
;; STRIP_NOPS: conversions between types of the same precision (mode)
(define (gf-strip-nops e)
  (let loop ([e e])
    (if (and (nop? e) (tree-nop-conversion-p (tree-type e) (tree-type (tree-operand e 0))))
        (loop (tree-operand e 0))
        e)))
;; STRIP_SIGN_NOPS: also keeping the signedness
(define (gf-strip-sign-nops e)
  (let loop ([e e])
    (if (and (nop? e) (tree-nop-conversion-p (tree-type e) (tree-type (tree-operand e 0)))
             (eq? (ty-uns? (tree-type e)) (ty-uns? (tree-type (tree-operand e 0)))))
        (loop (tree-operand e 0))
        e)))

;; negate_expr_p (the integer subset)
(define (negate-expr-p e)
  (define t (tree-type e))
  (case (tree-code e)
    [(integer_cst)
     (or (ty-uns? t) (not (ty-undef? t)) (not (= (icst-val e) (ty-min t))))]
    [(negate_expr) (not (ty-undef? t))]
    [(minus_expr) (not (ty-undef? t))]
    [else #f]))

;; negate_expr: fold_build1 NEGATE_EXPR
(define (gf-negate e) (gf-build1 'negate_expr (tree-type e) e))

;; ------------------------------------------------------------
;; builders
;; ------------------------------------------------------------

(define (mk1 code t a) (recalculate-side-effects (gs) (build1 (gs) code t a)))
(define (mk2 code t a b) (recalculate-side-effects (gs) (build2 (gs) code t a b)))

;; fold_convert_loc for integral / pointer targets
(define (gf-convert t e)
  (define o (tree-type e))
  (cond
    [(eqv? t o) e]
    [(and (icst? e) (or (ty-int? t) (ty-ptr? t))) (icst t (icst-val e))]
    [(or (ty-int? o) (ty-ptr? o)) (gf-build1 'nop_expr t e)]
    [else (mk1 'nop_expr t e)]))

(define (gf-build1 code t a)
  (or (gf-unary code t a) (mk1 code t a)))
(define (gf-build2 code t a b)
  (or (gf-binary code t a b) (mk2 code t a b)))
(define (gf-build3 code t a b c)
  (cond
    [(and (eq? code 'cond_expr) (icst? a)) (if (zero? (icst-val a)) c b)]
    [(and (eq? code 'cond_expr) (operand-equal? b c)) b]
    [else (recalculate-side-effects (gs) (build3 (gs) code t a b c))]))

;; fold_unary_loc
(define (gf-unary code t a)
  (define arg0 (if (memq code '(nop_expr convert_expr negate_expr)) (gf-strip-sign-nops a) a))
  (case code
    [(nop_expr convert_expr)
     (cond
       [(icst? arg0) (and (or (ty-int? t) (ty-ptr? t)) (icst t (icst-val arg0)))]
       [(eqv? t (tree-type a)) a]                               ; strip-useless (GENERIC: identical type)
       [(nop? a) (convert-of-convert t a)]
       [else #f])]
    [(negate_expr)
     (cond
       [(icst? arg0) (icst t (- (icst-val arg0)))]
       [(eq? (tree-code a) 'negate_expr) (tree-operand a 0)]
       [else #f])]
    [else #f]))

;; match.pd "Handle cases of two conversions in a row"
(define (convert-of-convert t a)
  (define x (tree-operand a 0))
  (define inside (tree-type x)) (define inter (tree-type a))
  (define inside-int (ty-int? inside)) (define inside-ptr (ty-ptr? inside))
  (define inter-int (ty-int? inter)) (define inter-ptr (ty-ptr? inter))
  (define final-int (ty-int? t)) (define final-ptr (ty-ptr? t))
  (define inside-prec (ty-prec inside)) (define inter-prec (ty-prec inter)) (define final-prec (ty-prec t))
  (define inside-uns (ty-uns? inside)) (define inter-uns (ty-uns? inter)) (define final-uns (ty-uns? t))
  (define (main v) (type-main-variant (gs) v))
  (cond
    [(and (eqv? (main t) (main inside)) (or inter-int inter-ptr) final-int (>= inter-prec final-prec))
     (gf-build1 'nop_expr t x)]
    [(and inter-int inside-int final-int (>= inter-prec inside-prec) (eq? inter-uns inside-uns))
     (gf-build1 'nop_expr t x)]
    [(and inside-int inter-int final-int
          (or (and (< inside-prec inter-prec) (< inter-prec final-prec) inside-uns (not inter-uns))
              (= final-prec inter-prec)))
     (gf-build1 'nop_expr t x)]
    [(and (or (>= inter-prec inside-prec) (>= inter-prec final-prec))
          (not (and inside-int inter-int (not (eq? inter-uns inside-uns)) (< inter-prec final-prec)))
          (eq? (and inter-uns (> inter-prec inside-prec)) (and final-uns (> final-prec inter-prec)))
          (not (and inside-ptr (not (= inter-prec final-prec))))
          (not (and final-ptr (not (= inside-prec inter-prec)))))
     (gf-build1 'nop_expr t x)]
    [else #f]))

;; fold_binary_loc: tree or #f
(define (gf-binary code t op0 op1)
  (define cmp? (comparison-code? code))
  (define arg0 (if cmp? (gf-strip-sign-nops op0) (gf-strip-nops op0)))
  (define arg1 (if cmp? (gf-strip-sign-nops op1) (gf-strip-nops op1)))
  (define-values (c _ovf) (if (and (icst? arg0) (icst? arg1)) (const-binop code t arg0 arg1) (values #f #f)))
  (cond
    [c (if (eqv? (tree-type c) t) c (gf-convert t c))]
    [(and (commutative-code? code) (tree-swap-operands-p (gs) op0 op1)) (gf-build2 code t op1 op0)]
    [(and cmp? (tree-swap-operands-p (gs) op0 op1)) (gf-build2 (swap-cmp code) t op1 op0)]
    [(generic-simplify code t op0 op1)]
    [else (fold-binary-cases code t op0 op1 arg0 arg1)]))

;; the match.pd patterns
(define (generic-simplify code t op0 op1)
  (case code
    [(plus_expr pointer_plus_expr minus_expr)
     (cond
       ;; (op @0 integer_zerop) -> @0
       [(integer-zerop op1) (gf-convert t op0)]
       ;; (minus @0 @0) -> 0
       [(and (eq? code 'minus_expr) (operand-equal? op0 op1) (ty-int? t)) (icst t 0)]
       [(and (memq code '(plus_expr minus_expr)) (assoc-constants code t op0 op1))]
       [else #f])]
    [(mult_expr)
     (cond
       [(integer-zerop op1) (icst t 0)]
       [(integer-onep op1) (gf-convert t op0)]
       ;; (mult (mult @0 INTEGER_CST@1) INTEGER_CST@2) when the product does not overflow
       [(and (icst? op1) (eq? (tree-code op0) 'mult_expr) (icst? (tree-operand op0 1)))
        (define-values (c ovf) (const-binop 'mult_expr t (tree-operand op0 1) op1))
        (and c (not ovf) (gf-build2 'mult_expr t (tree-operand op0 0) c))]
       [else #f])]
    [else #f]))

;; (A +- CST1) +- CST2 -> A + CST3, with an optional nop conversion
;; around the inner operation
(define (assoc-constants outer t op0 op1)
  (define inner-e
    (cond
      [(memq (tree-code op0) '(plus_expr minus_expr)) op0]
      [(and (nop? op0) (memq (tree-code (tree-operand op0 0)) '(plus_expr minus_expr))
            (tree-nop-conversion-p t (tree-type (tree-operand op0 0))))
       (tree-operand op0 0)]
      [else #f]))
  (and inner-e (icst? op1) (icst? (tree-operand inner-e 1))
       (let* ([inner (tree-code inner-e)]
              [neg-inner (if (eq? inner 'plus_expr) 'minus_expr 'plus_expr)]
              [a (tree-operand inner-e 0)] [c1 (tree-operand inner-e 1)] [c2 op1]
              [at (tree-type a)])
         (cond
           [(not (ty-undef? t))
            ;; the outer type wraps: (view_convert @0) + (inner_op! @2 (view_convert @1))
            (and (not (icst? a))
                 (let-values ([(c ovf) (if (eq? outer 'plus_expr)
                                           (const-binop inner t c2 (gf-convert t c1))
                                           (const-binop neg-inner t c2 (gf-convert t c1)))])
                   (and c (gf-build2 outer t (gf-convert t a) c))))]
           [(not (ty-undef? at))
            ;; the inner type wraps
            (let-values ([(c ovf) (if (eq? outer 'plus_expr)
                                      (const-binop inner at (gf-convert at c2) c1)
                                      (const-binop neg-inner at (gf-convert at c2) c1))])
              (and c (gf-convert t (gf-build2 outer at a c))))]
           [(eqv? t at)
            (let-values ([(c ovf) (const-binop (if (eq? outer inner) 'plus_expr 'minus_expr) t c1 c2)])
              (cond
                [(not c) #f]
                [(not ovf) (gf-build2 inner t a c)]
                [(= (icst-val c) (ty-min t)) (gf-build2 neg-inner t a c)]
                [else (let ([u (utype-for t)])
                        (gf-convert t (gf-build2 inner u (gf-convert u a) (gf-convert u c))))]))]
           [else #f]))))

;; the fold-const.cc cases after generic_simplify
(define (fold-binary-cases code t op0 op1 arg0 arg1)
  (case code
    [(plus_expr minus_expr)
     (cond
       ;; A - CST -> A + -CST
       [(and (eq? code 'minus_expr) (icst? op1) (negate-expr-p op1) (ty-int? t))
        (gf-build2 'plus_expr t (gf-convert t op0) (gf-negate op1))]
       [(and (or (eq? (tree-code arg0) 'mult_expr) (eq? (tree-code arg1) 'mult_expr))
             (eq? (ty-uns? t) (ty-uns? (tree-type arg0))) (eq? (ty-uns? t) (ty-uns? (tree-type arg1))))
        (plusminus-mult code t arg0 arg1)]
       [else #f])]
    [else #f]))

;; fold_plusminus_mult_expr: (A * C) +- (B * C) -> (A+-B) * C, (A * C) +- A -> A * (C+-1)
(define (plusminus-mult code0 t arg0 arg1)
  (define code code0)
  (define-values (arg00 arg01)
    (cond [(eq? (tree-code arg0) 'mult_expr) (values (tree-operand arg0 0) (tree-operand arg0 1))]
          [(icst? arg0) (values (icst t 1) arg0)]
          [else (values arg0 (icst t 1))]))
  (define-values (arg10 arg11)
    (cond [(eq? (tree-code arg1) 'mult_expr) (values (tree-operand arg1 0) (tree-operand arg1 1))]
          [(icst? arg1)
           ;; as we canonicalize A - 2 to A + -2 get rid of that sign
           (if (and (< (icst-val arg1) 0) (negate-expr-p arg1) (eq? code 'plus_expr))
               (begin (set! code 'minus_expr) (values (icst t 1) (gf-negate arg1)))
               (values (icst t 1) arg1))]
          [else (values arg1 (icst t 1))]))
  (define-values (same alt0 alt1)
    (cond [(operand-equal? arg00 arg10) (values arg00 arg01 arg11)]
          [(operand-equal? arg01 arg11) (values arg01 arg00 arg10)]
          [(operand-equal? arg00 arg11) (values arg00 arg01 arg10)]
          [(operand-equal? arg01 arg10) (values arg01 arg00 arg11)]
          [else (values #f #f #f)]))
  (cond
    [(not same) #f]
    [(or (not (ty-int? t)) (ty-wraps? t) (icst? same))
     (gf-build2 'mult_expr t (gf-build2 code t (gf-convert t alt0) (gf-convert t alt1)) (gf-convert t same))]
    [else
     ;; same may be zero or minus one: the sum in an unsigned type, kept only when constant
     (define u (utype-for t))
     (define tem (gf-build2 code u (gf-convert u alt0) (gf-convert u alt1)))
     (and (icst? tem)
          (not (= (icst-val tem) (arithmetic-shift 1 (sub1 (ty-prec u)))))
          (gf-build2 'mult_expr t (gf-convert t tem) same))]))
