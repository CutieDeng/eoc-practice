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
;;   A - B -> A + (-B) and (-A) - B -> (-B) - A for negate_expr_p B
;;   (A * C) +- A, (A * C1) +- (A * C2), (A * C) +- C (fold_plusminus_mult_expr)
;;   MULT_EXPR rules of match.pd in their order: (A * C1) * C2 (kept on
;;   overflow when the type wraps), GCC 15 (A * C1 + C2) * C3 for
;;   wrapping types, (-A) * negatable B, (X /[ex] A) * A, GCC 15
;;   (X /[ex] C1) * (C1 * C2), ((X /[ex] A) +- B) * A (GCC 12) /
;;   ((X /[ex] C1) +- C2) * (C1 * C3) (GCC 15), X * -1 -> -X,
;;   (X * C) * Y -> (X * Y) * C (single_use is true on GENERIC)
;;   x * -C -> -x * C, extract_muldiv for X * C (fold_binary MULT_EXPR:
;;   constants, conversions, negations, (A +- C1) * C distributed,
;;   (A * C1) * C and (A /[ex] C1) * C associated), with GCC's static
;;   recursion limit of 3; ctype by mode size
;;   GCC >= 15: (A * B) + (-C) -> (B - C/A) * A when C is a multiple of A
;;   negate_expr_p and negate_expr / fold_negate_expr_1 (integer cases)
;;   comparisons of constants, and with operands swapped to canonical order
;;
;; Not ported: TREE_OVERFLOW (constants carry no overflow flag, so
;; extract_muldiv's overflow refusals see only the arithmetic of the
;; current step); extract_muldiv's MIN / MAX (GCC 12 distributes them
;; over MULT, GCC 15 does not), ABS, shifts and the division / modulus
;; codes; fold_binary_op_with_conditional_arg and the COMPOUND_EXPR
;; hoisting; the PLUS / MINUS rules X + (X / C) * -C -> X % C, the
;; (plus (plus mult foo) mult) reassociation and the associate:
;; split_tree block; get_range_pos_neg beyond constants and narrower
;; unsigned conversions; const_binop computes in the result type (GCC:
;; the first operand's type), which differs only for divisions and
;; shifts of mixed signedness.
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

;; negate_expr_p (fold-const.cc, the integer cases; the same in 12.5 and
;; 15.2): TYPE is the type before STRIP_SIGN_NOPS
(define (negate-expr-p e)
  (define type (tree-type e))
  (define t (gf-strip-sign-nops e))
  (define tt (tree-type t))
  (define (op i) (tree-operand t i))
  (define (non-pow2-cst? x) (and (icst? x) (not (= 1 (bit-count* (abs (signed-or-unsigned (icst-val x) (tree-type x))))))))
  (case (tree-code t)
    [(integer_cst)
     (or (and (ty-int? type) (ty-uns? type))
         ;; may_negate_without_overflow_p
         (and (not (ty-uns? tt)) (not (= (signed-or-unsigned (icst-val t) tt) (ty-min tt)))))]
    [(bit_not_expr) (and (ty-int? type) (ty-wraps? type))]
    [(negate_expr) #t]                                  ; !TYPE_OVERFLOW_SANITIZED
    [(plus_expr)
     (and (not (and (ty-int? type) (not (ty-wraps? type))))
          (or (negate-expr-p (op 1)) (negate-expr-p (op 0))))]
    [(minus_expr) (or (not (ty-int? type)) (ty-wraps? type))]
    [(mult_expr)
     (cond
       [(ty-uns? type) #f]
       ;; INT_MIN/n * n: negating one operand overflows unless an operand
       ;; is a constant that is not a power of two
       [(and (ty-int? tt) (not (ty-wraps? tt)) (not (or (non-pow2-cst? (op 0)) (non-pow2-cst? (op 1))))) #f]
       [else (or (negate-expr-p (op 1)) (negate-expr-p (op 0)))])]
    [(trunc_div_expr round_div_expr exact_div_expr)
     (cond
       [(ty-uns? type) #f]
       [(and (icst? (op 0)) (negate-expr-p (op 0))) #t]
       [(or (not (ty-int? tt)) (ty-wraps? tt) (and (icst? (op 1)) (not (integer-onep (op 1))))) (negate-expr-p (op 1))]
       [else #f])]
    [(rshift_expr) (and (icst? (op 1)) (= (icst-val (op 1)) (sub1 (ty-prec type))))]
    [else #f]))

;; negate_expr: STRIP_SIGN_NOPS, fold_negate_expr or a NEGATE_EXPR,
;; converted back to the original type
(define (gf-negate e)
  (define type (tree-type e))
  (define t (gf-strip-sign-nops e))
  (gf-convert type (or (fold-negate-expr t) (mk1 'negate_expr (tree-type t) t))))
;; fold_negate_expr
(define (fold-negate-expr e)
  (define type (tree-type e))
  (define tem (fold-negate-expr-1 (gf-strip-sign-nops e)))
  (and tem (gf-convert type tem)))
;; fold_negate_expr_1 (integer cases; TREE_OVERFLOW is not modelled, and
;; without -fsanitize the negated constant is always returned)
(define (fold-negate-expr-1 t)
  (define type (tree-type t))
  (define (op i) (tree-operand t i))
  (case (tree-code t)
    [(bit_not_expr) (and (ty-int? type) (gf-build2 'plus_expr type (op 0) (icst type 1)))]
    [(integer_cst) (icst type (- (icst-val t)))]
    [(negate_expr) (op 0)]
    [(plus_expr)
     (cond
       [(negate-expr-p (op 1)) (gf-build2 'minus_expr type (gf-negate (op 1)) (op 0))]
       [(negate-expr-p (op 0)) (gf-build2 'minus_expr type (gf-negate (op 0)) (op 1))]
       [else #f])]
    [(minus_expr) (gf-build2 'minus_expr type (op 1) (op 0))]
    [(mult_expr)
     (cond
       [(ty-uns? type) #f]
       [(negate-expr-p (op 1)) (gf-build2 'mult_expr type (op 0) (gf-negate (op 1)))]
       [(negate-expr-p (op 0)) (gf-build2 'mult_expr type (gf-negate (op 0)) (op 1))]
       [else #f])]
    [(trunc_div_expr round_div_expr exact_div_expr)
     (cond
       [(ty-uns? type) #f]
       [(and (icst? (op 0)) (negate-expr-p (op 0))) (gf-build2 (tree-code t) type (gf-negate (op 0)) (op 1))]
       [(and (or (not (ty-int? type)) (ty-wraps? type) (and (icst? (op 1)) (not (integer-onep (op 1)))))
             (negate-expr-p (op 1)))
        (gf-build2 (tree-code t) type (op 0) (gf-negate (op 1)))]
       [else #f])]
    [(rshift_expr)
     (and (icst? (op 1)) (= (icst-val (op 1)) (sub1 (ty-prec type)))
          (let ([nt (if (ty-uns? type) (stype-for type) (utype-for type))])
            (gf-convert type (gf-build2 'rshift_expr nt (gf-convert nt (op 0)) (op 1)))))]
    [else #f]))

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
    ;; fold_negate_expr of the sign-stripped operand
    [(negate_expr) (let ([tem (fold-negate-expr arg0)]) (and tem (gf-convert t tem)))]
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
       [(and (eq? code 'plus_expr) (>= (gcc-major-version (gs)) 15) (factor-mult-negative-constant t op0 op1))]
       [else #f])]
    [(mult_expr) (simplify-mult t op0 op1)]
    [else #f]))

;; the match.pd rules rooted at MULT_EXPR that fire on integer GENERIC,
;; in match.pd order (12.5 / 15.2 line numbers)
(define (simplify-mult t op0 op1)
  (define v15? (>= (gcc-major-version (gs)) 15))
  (define (strip-conv x) (if (nop? x) (tree-operand x 0) x))
  (define (types-match? a b) (eqv? (type-main-variant (gs) a) (type-main-variant (gs) b)))
  (define (wide-mul a b) ; wi::mul (a, b, TYPE_SIGN (t)): (values product overflow?)
    (define p (* (signed-or-unsigned (icst-val a) (tree-type a)) (signed-or-unsigned (icst-val b) (tree-type b))))
    (values p (not (int-fits-type? p t))))
  (cond
    [(integer-zerop op1) (icst t 0)]
    [(integer-onep op1) (gf-convert t op0)]
    ;; 490 / 640: (mult (mult @0 INTEGER_CST@1) INTEGER_CST@2) unless the
    ;; product overflows a type whose overflow is undefined
    [(and (icst? op1) (eq? (tree-code op0) 'mult_expr) (icst? (tree-operand op0 1))
          (let-values ([(p ovf) (wide-mul (tree-operand op0 1) op1)])
            (and (or (not ovf) (ty-wraps? t)) (gf-build2 'mult_expr t (tree-operand op0 0) (icst t p)))))]
    ;; 15: 654: (mult (plus (mult @0 C1) C2) C3) -> (plus (mult @0 C1*C3) C2*C3),
    ;; on GENERIC only when the type wraps
    [(and v15? (icst? op1) (not (ty-undef? t)) (eq? (tree-code op0) 'plus_expr) (icst? (tree-operand op0 1))
          (eq? (tree-code (tree-operand op0 0)) 'mult_expr) (icst? (tree-operand (tree-operand op0 0) 1)))
     (define m (tree-operand op0 0))
     (define-values (mul _o1) (wide-mul (tree-operand m 1) op1))
     (define-values (add _o2) (wide-mul (tree-operand op0 1) op1))
     (gf-build2 'plus_expr t (gf-build2 'mult_expr t (tree-operand m 0) (icst t mul)) (icst t add))]
    ;; 1604 / 2110: (mult:c (convert1? (negate @0)) (convert2? negate_expr_p@1))
    [(for/or ([a (in-list (list op0 op1))] [b (in-list (list op1 op0))])
       (define na (strip-conv a))
       (and (eq? (tree-code na) 'negate_expr) (negate-expr-p (strip-conv b))
            (let ([x (tree-operand na 0)] [y (strip-conv b)])
              (and (tree-nop-conversion-p t (tree-type x)) (tree-nop-conversion-p t (tree-type y))
                   (gf-build2 'mult_expr t (gf-convert t x) (gf-convert t (gf-negate y)))))))]
    ;; 3855 / 5584: (mult (convert1? (exact_div @0 @@1)) (convert2? @1)) -> (convert @0)
    [(let ([d (strip-conv op0)])
       (and (eq? (tree-code d) 'exact_div_expr) (operand-equal? (tree-operand d 1) (strip-conv op1))
            (gf-convert t (tree-operand d 0))))]
    ;; 15: 5589: (mult (convert? (exact_div @0 INTEGER_CST@1)) INTEGER_CST@2), @2 a multiple of @1
    [(and v15? (icst? op1)
          (let ([d (strip-conv op0)])
            (and (eq? (tree-code d) 'exact_div_expr) (icst? (tree-operand d 1))
                 (let ([c1 (signed-or-unsigned (icst-val (tree-operand d 1)) (tree-type (tree-operand d 1)))]
                       [c2 (signed-or-unsigned (icst-val op1) (tree-type op1))])
                   (and (not (zero? c1)) (zero? (remainder c2 c1))
                        (gf-build2 'mult_expr t (gf-convert t (tree-operand d 0)) (icst t (quotient c2 c1))))))))]
    ;; 3868 / 5615: ((X /[ex] A) +- B) * A
    [(exact-div-plusminus-mult t op0 op1 v15?)]
    ;; 3920 / 5686: x * -1 -> -x
    [(integer-all-ones? op1) (gf-build1 'negate_expr t op0)]
    ;; 3926 / 5692: (mult:c (mult @0 INTEGER_CST@1) @2) -> (mult (mult @0 @2) @1),
    ;; @2 not a constant, @1 neither 0 nor -1 (single_use is true on GENERIC)
    [(for/or ([a (in-list (list op0 op1))] [b (in-list (list op1 op0))])
       (and (eq? (tree-code a) 'mult_expr) (icst? (tree-operand a 1)) (not (icst? b))
            (not (integer-zerop (tree-operand a 1)))
            (not (integer-all-ones? (tree-operand a 1)))
            (gf-build2 'mult_expr t (gf-build2 'mult_expr t (tree-operand a 0) b) (tree-operand a 1))))]
    [else #f]))

;; integer_minus_onep: all ones in the constant's precision
(define (integer-all-ones? c)
  (and (icst? c) (let ([m (sub1 (arithmetic-shift 1 (ty-prec (tree-type c))))]) (= (bitwise-and (icst-val c) m) m))))

;; ((X /[ex] A) +- B) * A (GCC 12) and ((X /[ex] C1) +- C2) * (C1 * C3)
;; (GCC 15; get_range_pos_neg sees constants and narrower unsigned
;; conversions only)
(define (exact-div-plusminus-mult t op0 op1 v15?)
  (define (strip-conv x) (if (nop? x) (tree-operand x 0) x))
  (define (types-match? a b) (eqv? (type-main-variant (gs) a) (type-main-variant (gs) b)))
  (define pm (strip-conv op0))
  (and (memq (tree-code pm) '(plus_expr minus_expr)) (icst? (tree-operand pm 1)) (icst? op1)
       (let* ([code (tree-code pm)] [d (strip-conv (tree-operand pm 0))] [c2 (tree-operand pm 1)])
         (and (eq? (tree-code d) 'exact_div_expr) (icst? (tree-operand d 1))
              (let* ([x (tree-operand d 0)] [a (tree-operand d 1)]
                     [sv (lambda (c) (signed-or-unsigned (icst-val c) (tree-type c)))]
                     [u (lambda () (utype-for t))])
                (and (tree-nop-conversion-p t (tree-type c2)) (tree-nop-conversion-p (tree-type x) (tree-type c2))
                     (cond
                       [(not v15?)
                        ;; @1 of the pattern: the multiplier must equal A
                        (and (operand-equal? a op1)
                             (let* ([p (* (sv a) (sv c2))] [ovf (not (int-fits-type? p t))])
                               (if (and (types-match? t (tree-type c2)) (types-match? (tree-type x) (tree-type c2)) (not ovf))
                                   (gf-build2 code t x (icst t p))
                                   (let ([ut (u)])
                                     (gf-convert t (gf-build2 code ut (gf-convert ut x)
                                                              (gf-build2 'mult_expr ut (gf-convert ut a) (gf-convert ut c2))))))))]
                       [else
                        (define c1 (sv a)) (define c3 (sv op1))
                        (and (not (zero? c1)) (zero? (remainder c3 c1))
                             (let* ([factor (quotient c3 c1)]
                                    [p (* (sv c2) c3)] [ovf (not (int-fits-type? p t))]
                                    [pos-neg (cond [(icst? x) (if (< (sv x) 0) 2 1)]
                                                   [(and (nop? x) (ty-int? (tree-type (tree-operand x 0))) (ty-uns? (tree-type (tree-operand x 0)))
                                                         (< (ty-prec (tree-type (tree-operand x 0))) (ty-prec (tree-type x))))
                                                    1]
                                                   [else 3])])
                               (if (and (types-match? t (tree-type c2)) (types-match? (tree-type x) (tree-type c2)) (not ovf)
                                        (or (ty-uns? t) (= factor 1)
                                            (not (= 3 (bitwise-ior pos-neg (if (not (eq? (eq? code 'plus_expr) (< (sv c2) 0))) 1 2))))))
                                   (gf-build2 code t (gf-build2 'mult_expr t x (icst t factor)) (icst t p))
                                   (let ([ut (u)])
                                     (gf-convert t (gf-build2 code ut (gf-build2 'mult_expr ut (gf-convert ut x) (icst ut factor))
                                                              (gf-build2 'mult_expr ut (gf-convert ut op1) (gf-convert ut c2))))))))])))))))

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

;; GCC >= 15 match.pd: (A * B) + (-C) -> (B - C/A) * A, if C is a multiple of A
(define (factor-mult-negative-constant t op0 op1)
  (and (eq? (type-code (gs) t) 'integer_type)
       (icst? op1) (eq? (tree-code op0) 'mult_expr)
       (let* ([prec (ty-prec t)]
              [c2 (signed-of (icst-val op1) prec)]
              ;; mult:c with integer_nonzerop@0: the constant factor
              [a (tree-operand op0 0)] [b (tree-operand op0 1)]
              [c0t (cond [(and (icst? b) (not (integer-zerop b))) b] [(and (icst? a) (not (integer-zerop a))) a] [else #f])]
              [x (and c0t (if (eq? c0t b) a b))])
         (and c0t (< c2 0) (not (= c2 (- (arithmetic-shift 1 (sub1 prec)))))
              (let* ([c0 (if (ty-uns? t) (bitwise-and (icst-val c0t) (sub1 (arithmetic-shift 1 prec))) (icst-val c0t))]
                     [c2abs (abs c2)])
                (and (not (zero? c0)) (zero? (remainder c2abs (abs c0)))
                     (let ([div (quotient c2 (signed-of (icst-val c0t) prec))])
                       (gf-build2 'mult_expr t (gf-build2 'plus_expr t x (icst t div)) c0t))))))))
(define (signed-of v prec)
  (let ([m (bitwise-and v (sub1 (arithmetic-shift 1 prec)))])
    (if (bitwise-bit-set? m (sub1 prec)) (- m (arithmetic-shift 1 prec)) m)))

;; extract_muldiv (code MULT_EXPR): T * C rewritten so that the
;; multiplication disappears into T, or #f; recursion stops past depth 3
(define muldiv-depth 0)
(define (extract-muldiv t c wide-type)
  (cond
    [(> muldiv-depth 3) #f]
    [else
     (set! muldiv-depth (add1 muldiv-depth))
     (begin0 (with-handlers ([(lambda (e) #t) (lambda (e) (set! muldiv-depth (sub1 muldiv-depth)) (raise e))])
               (extract-muldiv-1 t c wide-type))
             (set! muldiv-depth (sub1 muldiv-depth)))]))
(define (expression-class? x)
  (and (Tree? x) (memq (tree-code-class (gs) (tree-code x)) '(tcc_comparison tcc_unary tcc_binary tcc_vl_exp tcc_expression)) #t))
;; TREE_OVERFLOW of a constant folded in type T (unsigned arithmetic wraps silently)
(define (cst-overflow? ovf t) (and ovf (not (ty-uns? t)) (not (ty-ptr? t))))
(define (extract-muldiv-1 t c wide-type)
  (define type (tree-type t))
  (define tcode (tree-code t))
  ;; GET_MODE_SIZE (SCALAR_INT_TYPE_MODE) of both types: the narrowest
  ;; integer mode holding the precision
  (define (mode-size ty) (let ([p (ty-prec ty)]) (cond [(<= p 8) 1] [(<= p 16) 2] [(<= p 32) 4] [(<= p 64) 8] [else 16])))
  (define ctype (if (and wide-type (> (mode-size wide-type) (mode-size type))) wide-type type))
  (define (op i) (tree-operand t i))
  (cond
    [(integer-zerop c) #f]
    [else
     (case tcode
       [(integer_cst)
        (define-values (tem ovf) (const-binop 'mult_expr ctype (gf-convert ctype t) (gf-convert ctype c)))
        (and tem (not (cst-overflow? ovf ctype)) tem)]
       [(nop_expr convert_expr non_lvalue_expr)
        (define op0 (op 0))
        (define it (tree-type op0))
        (cond
          [(not (ty-int? it)) #f]
          [(and (expression-class? op0)
                (or (and (ty-wraps? it) (> (ty-prec ctype) (ty-prec it)))
                    (< (ty-prec type) (ty-prec it))
                    (and (ty-undef? it) (not (ty-undef? type)))))
           #f]
          [else
           (define cv (signed-or-unsigned (icst-val c) (tree-type c)))
           (define t2 (gf-convert it c))
           (and (icst? t2)
                (not (and (not (ty-uns? it)) (not (int-fits-type? cv it))))       ; TREE_OVERFLOW of the conversion
                (extract-muldiv op0 t2 ctype))])]
       [(negate_expr)
        (define t1 (extract-muldiv (op 0) c wide-type))
        (and t1 (gf-build1 'negate_expr ctype (gf-convert ctype t1)))]
       [(plus_expr minus_expr)
        (define op0 (op 0)) (define op1 (op 1))
        (define t1 (extract-muldiv op0 c wide-type))
        (define t2 (extract-muldiv op1 c wide-type))
        (cond
          [(and t1 t2 (ty-wraps? ctype))
           (gf-build2 tcode ctype (gf-convert ctype t1) (gf-convert ctype t2))]
          [else
           ;; a subtraction becomes an addition of the negated operand
           (define-values (tc o0 o1)
             (cond
               [(eq? tcode 'minus_expr)
                (define n1 (gf-negate op1))
                (if (icst? op0) (values 'plus_expr n1 op0) (values 'plus_expr op0 n1))]
               [else (values tcode op0 op1)]))
           (cond
             [(not (icst? o1)) #f]
             [else
              (define-values (m ovf) (const-binop 'mult_expr ctype (gf-convert ctype o1) (gf-convert ctype c)))
              (cond
                [(or (not m) (and (cst-overflow? ovf ctype) (not (ty-wraps? ctype)))) #f]
                [(and (ty-uns? ctype) (not (eqv? ctype type))) #f]
                [(and (ty-wraps? ctype)
                      (let ([cv (abs (signed-or-unsigned (icst-val c) (tree-type c)))])
                        (not (and (< cv (arithmetic-shift 1 63)) (= 1 (bit-count* cv))))))
                 (gf-build2 tc ctype (gf-build2 'mult_expr ctype (gf-convert ctype o0) (gf-convert ctype c)) m)]
                [else #f])])])]
       [(mult_expr exact_div_expr)
        (define op0 (op 0)) (define op1 (op 1))
        (cond
          [(and (eq? tcode 'mult_expr) (ty-wraps? ctype) (extract-muldiv op0 c wide-type))
           => (lambda (t1) (gf-build2 tcode ctype (gf-convert ctype t1) (gf-convert ctype op1)))]
          [(and (eq? tcode 'mult_expr) (ty-wraps? ctype) (extract-muldiv op1 c wide-type))
           => (lambda (t1) (gf-build2 tcode ctype (gf-convert ctype op0) (gf-convert ctype t1)))]
          [(not (icst? op1)) #f]
          [(eq? tcode 'mult_expr)
           ;; the same operation: associate when the product does not overflow
           (define prec (ty-prec ctype))
           (define uns (ty-uns? ctype))
           (define a (if uns (icst-val op1) (signed-of (icst-val op1) prec)))
           (define b (if uns (icst-val c) (signed-of (icst-val c) prec)))
           (define mul (* a b))
           (define ovf (not (if uns (< mul (arithmetic-shift 1 prec)) (and (>= mul (ty-min ctype)) (<= mul (ty-max ctype))))))
           (if (and ovf (not uns)) #f (gf-build2 tcode ctype (gf-convert ctype op0) (icst ctype mul)))]
          [(ty-undef? ctype)
           ;; (A /[ex] C1) * C: one constant a multiple of the other
           (define a (icst-val op1)) (define b (icst-val c))
           (cond
             [(and (not (zero? b)) (zero? (remainder a b)))
              (gf-build2 tcode ctype (gf-convert ctype op0) (gf-convert ctype (icst (tree-type op1) (quotient a b))))]
             [(and (not (zero? a)) (zero? (remainder b a)))
              (gf-build2 'mult_expr ctype (gf-convert ctype op0) (gf-convert ctype (icst (tree-type c) (quotient b a))))]
             [else #f])]
          [else #f])]
       [else #f])]))
(define (signed-or-unsigned v t) (if (ty-uns? t) v (signed-of v (ty-prec t))))
(define (bit-count* n) (let loop ([n n] [k 0]) (if (zero? n) k (loop (arithmetic-shift n -1) (+ k (bitwise-and n 1))))))

;; the fold-const.cc cases after generic_simplify
(define (fold-binary-cases code t op0 op1 arg0 arg1)
  (case code
    [(mult_expr)
     (cond
       [(ty-int? t)
        (cond
          ;; x * -C -> -x * C if x is easily negatable
          [(and (icst? op1) (< (signed-or-unsigned (icst-val op1) (tree-type op1)) 0)
                (negate-expr-p op0) (negate-expr-p op1)
                (let ([tem (gf-negate op1)]) (and (icst? tem) (not (eq? tem op1)) tem)))
           => (lambda (tem) (gf-build2 'mult_expr t (gf-convert t (gf-negate op0)) tem))]
          [(and (icst? arg1) (extract-muldiv op0 arg1 #f)) => (lambda (tem) (gf-convert t tem))]
          [else #f])]
       [else #f])]
    [(plus_expr minus_expr)
     (cond
       ;; (-A) - B -> (-B) - A where B is easily negated
       [(and (eq? code 'minus_expr) (eq? (tree-code arg0) 'negate_expr) (negate-expr-p op1))
        (gf-build2 'minus_expr t (gf-negate op1) (gf-convert t (tree-operand arg0 0)))]
       ;; A - B -> A + (-B) if B is easily negatable
       [(and (eq? code 'minus_expr) (negate-expr-p op1) (ty-int? t))
        (gf-build2 'plus_expr t (gf-convert t arg0) (gf-negate op1))]
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
