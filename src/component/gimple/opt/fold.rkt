#lang racket/base

;; ============================================================
;; opt: constant folding of GIMPLE statements (the subset of
;; gimple_simplify / fold_binary / fold_unary that works on constants)
;; ============================================================
;;
;; GCC's gimple_simplify is generated from match.pd; its constant
;; folding goes through const_binop / const_unop (fold-const.cc).  This
;; module ports that constant part for integral and pointer types:
;;
;;   (gimple-simplify-const st g valueize) -> tree or #f
;;      the statement's result as a constant once its operands are
;;      valueized (SSA names replaced by VALUEIZE), or #f
;;   (fold-const-binop st code type a b) / (fold-const-unop st code type a)
;;
;; Not ported: match.pd identities (x + 0, x * 1, x & -1, ...),
;; floating point, vectors, overflow flags, saturating/rounding
;; divisions.
;; ============================================================

(require racket/math
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt")

(provide gimple-simplify-const fold-const-binop fold-const-unop fold-const-compare
         type-precision* type-unsigned?*)

(define (icst? t) (and t (eq? (tree-code t) 'integer_cst)))
;; TYPE_PRECISION / TYPE_SIGN as the constant folder sees them (pointers
;; are 64-bit unsigned; the dump carries no precision for them)
(define (type-precision* st ty) (or (type-attr st ty 'precision #f) 64))
(define (type-unsigned?* st ty) (or (pointer-type-p st ty) (and (type-attr st ty 'unsigned #f) #t)))

(define (fold-const-compare st code type a b)
  (and (icst? a) (icst? b)
       (let ([x (integer-cst-value a)] [y (integer-cst-value b)])
         (define r
           (case code
             [(lt_expr) (< x y)] [(le_expr) (<= x y)] [(gt_expr) (> x y)] [(ge_expr) (>= x y)]
             [(eq_expr) (= x y)] [(ne_expr) (not (= x y))]
             [else 'unknown]))
         (and (boolean? r) (build-int-cst st type (if r 1 0))))))

;; const_binop (integral): #f when not foldable (division by zero, bad shift)
(define (fold-const-binop st code type a b)
  (cond
    [(memq code '(lt_expr le_expr gt_expr ge_expr eq_expr ne_expr)) (fold-const-compare st code type a b)]
    [(not (and (icst? a) (icst? b))) #f]
    [(not (or (integral-type-p st type) (pointer-type-p st type))) #f]
    [else
     (define x (integer-cst-value a)) (define y (integer-cst-value b))
     (define prec (type-precision* st type))
     (define (wrap n) (build-int-cst st type n))
     (case code
       [(plus_expr pointer_plus_expr) (wrap (+ x y))]
       [(minus_expr pointer_diff_expr) (wrap (- x y))]
       [(mult_expr) (wrap (* x y))]
       [(bit_and_expr) (wrap (bitwise-and x y))]
       [(bit_ior_expr) (wrap (bitwise-ior x y))]
       [(bit_xor_expr) (wrap (bitwise-xor x y))]
       [(min_expr) (wrap (min x y))]
       [(max_expr) (wrap (max x y))]
       [(trunc_div_expr exact_div_expr) (and (not (zero? y)) (wrap (quotient x y)))]
       [(trunc_mod_expr) (and (not (zero? y)) (wrap (remainder x y)))]
       [(floor_div_expr) (and (not (zero? y)) (wrap (floor (/ x y))))]
       [(floor_mod_expr) (and (not (zero? y)) (wrap (modulo x y)))]
       [(ceil_div_expr) (and (not (zero? y)) (wrap (ceiling (/ x y))))]
       [(lshift_expr) (and (<= 0 y) (< y prec) (wrap (arithmetic-shift x y)))]
       [(rshift_expr) (and (<= 0 y) (< y prec) (wrap (arithmetic-shift x (- y))))]
       [(lrotate_expr rrotate_expr)
        (and (<= 0 y) (< y prec)
             (let* ([ux (bitwise-and x (sub1 (arithmetic-shift 1 prec)))]
                    [n (if (eq? code 'lrotate_expr) y (- prec y))])
               (wrap (bitwise-and (bitwise-ior (arithmetic-shift ux n) (arithmetic-shift ux (- n prec)))
                                  (sub1 (arithmetic-shift 1 prec))))))]
       [else #f])]))

;; const_unop (integral)
(define (fold-const-unop st code type a)
  (cond
    [(not (icst? a)) #f]
    [(not (or (integral-type-p st type) (pointer-type-p st type))) #f]
    [else
     (define x (integer-cst-value a))
     (case code
       [(negate_expr) (build-int-cst st type (- x))]
       [(bit_not_expr) (build-int-cst st type (bitwise-not x))]
       [(abs_expr absu_expr) (build-int-cst st type (abs x))]
       [(nop_expr convert_expr fix_trunc_expr)
        (and (or (integral-type-p st (tree-type a)) (pointer-type-p st (tree-type a)))
             (build-int-cst st type x))]
       [else #f])]))

;; gimple_simplify on a statement whose operands are valueized first
(define (gimple-simplify-const st g valueize)
  (cond
    [(gcond? g)
     (fold-const-compare st (gcond-code g) (boolean-type st) (valueize (gcond-lhs g)) (valueize (gcond-rhs g)))]
    [(gassign? g)
     (define code (gassign-code g))
     (define type (tree-type (gassign-lhs g)))
     (case (gimple-rhs-class st code)
       [(unary) (fold-const-unop st code type (valueize (gassign-rhs1 g)))]
       [(binary) (fold-const-binop st code type (valueize (gassign-rhs1 g)) (valueize (gassign-rhs2 g)))]
       [else #f])]
    [else #f]))
