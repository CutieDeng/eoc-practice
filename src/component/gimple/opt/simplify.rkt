#lang racket/base

;; ============================================================
;; opt: fold_stmt (gimple-fold.cc fold_stmt_1) and the subset of
;; gimple_simplify (match.pd) the optimizers need, over an mfn
;; ============================================================
;;
;; (fold-stmt st m id valueize) -> changed?
;;    fold_stmt_1 with inplace = false on the statement ID of M:
;;      1. [TARGET_]MEM_REF address canonicalization
;;         (maybe_canonicalize_mem_ref_addr, the &DECL / &component
;;         cases), &MEM[ssa + cst] -> ssa p+ cst
;;      2. operand order of commutative / comparison codes
;;         (tree_swap_operands_p, swap_tree_comparison)
;;      3. gimple_simplify: operands valueized through VALUEIZE and
;;         the defining statements of SSA operands looked through
;;         (gimple_match_op); the rules below are the match.pd
;;         patterns ported so far, result re-simplified like
;;         gimple_resimplify
;;      4. the post-simplification rules of fold_stmt_1: `_1 == 0/1`
;;         on same-typed operands (copy / BIT_NOT / BIT_XOR), the
;;         return value through VALUEIZE
;;
;; match.pd rules ported (fold.rkt holds the constant folding):
;;   x + 0, x - 0, x * 1, x * 0, x & 0, x & -1, x | 0, x | -1, x ^ 0,
;;   x / 1, x << 0, x >> 0, x - x, x ^ x, x & x, x | x, -(-x), ~~x,
;;   (T) x for a useless conversion,
;;   -(A + C) -> -C - A, -(A - B) -> B - A, -1 - A -> ~A, ~A + 1 -> -A,
;;   -A - 1 -> ~A, x == x / x != x, (a cmp b) != 0 / == 0 on booleans,
;;   bool_var != 0 -> bool_var and the other three fold_binary forms.
;; A rule whose result needs more than one statement is not applied.
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         "../gimplify.rkt"
         "ssa-ops.rkt"
         "fold.rkt"
         "mfn.rkt")

(provide follow-ssa-edges? fold-stmt simplify-assign-rhs simplify-cond canonicalize-mem-ref-addr gimple-fold-stmt-to-constant
         commutative-code? swap-tree-comparison invert-tree-comparison
         get-addr-base-and-unit-offset mem-ref-offset build-fold-addr-expr
         negate-expr-p may-propagate-copy)

(define (icst? t) (and t (eq? (tree-code t) 'integer_cst)))

;; may_propagate_copy (no abnormal edges)
;; gimple_simplify's valueization of SSA operands for looking through
;; their definitions: #t = follow_all_ssa_edges, #f = no_follow_ssa_edges
;; (fold_stmt's default, gimple_fold_stmt_to_constant_1 in the threader)
(define follow-ssa-edges? (make-parameter #t))

(define (may-propagate-copy st dest orig)
  (and (useless-type-conversion-p st (tree-type dest) (tree-type orig))
       (not (and (ssa-name? dest) (ssa-virtual? dest)))))

;; ------------------------------------------------------------
;; tree codes
;; ------------------------------------------------------------

(define commutative-codes
  '(plus_expr mult_expr mult_highpart_expr min_expr max_expr bit_ior_expr bit_xor_expr bit_and_expr
    ne_expr eq_expr unordered_expr ordered_expr uneq_expr ltgt_expr truth_and_expr truth_xor_expr truth_or_expr
    widen_mult_expr))
(define (commutative-code? code) (and (memq code commutative-codes) #t))
(define (comparison-code? st code) (eq? (tree-code-class st code) 'tcc_comparison))
(define (swap-tree-comparison code)
  (case code
    [(lt_expr) 'gt_expr] [(gt_expr) 'lt_expr] [(le_expr) 'ge_expr] [(ge_expr) 'le_expr]
    [(unlt_expr) 'ungt_expr] [(ungt_expr) 'unlt_expr] [(unle_expr) 'unge_expr] [(unge_expr) 'unle_expr]
    [else code]))
;; invert_tree_comparison for integral operands (no NaNs involved)
(define (invert-tree-comparison code)
  (case code
    [(eq_expr) 'ne_expr] [(ne_expr) 'eq_expr] [(lt_expr) 'ge_expr] [(ge_expr) 'lt_expr]
    [(gt_expr) 'le_expr] [(le_expr) 'gt_expr] [else #f]))

;; negate_expr_p (fold-const.cc), the INTEGER_CST and NEGATE_EXPR cases
(define (negate-expr-p st t)
  (cond
    [(not t) #f]
    [(icst? t)
     (define ty (tree-type t))
     (or (type-unsigned?* st ty)
         ;; not the most negative value
         (not (= (integer-cst-value t) (- (arithmetic-shift 1 (sub1 (type-precision* st ty)))))))]
    [(eq? (tree-code t) 'negate_expr) #t]
    [else #f]))

;; ------------------------------------------------------------
;; addresses
;; ------------------------------------------------------------

;; mem_ref_offset: the second MEM_REF operand as an integer
(define (mem-ref-offset t) (integer-cst-value (tree-operand t 1)))

;; get_addr_base_and_unit_offset (no valueization): (values base byte-offset) or (values #f #f)
(define (get-addr-base-and-unit-offset st exp)
  (let loop ([exp exp] [off 0])
    (case (tree-code exp)
      [(component_ref)
       (define field (tree-operand exp 1))
       (define foff (tree-attr field 'offset #f))          ; DECL_FIELD_OFFSET (bytes)
       (define boff (tree-attr field 'bit-offset #f))      ; DECL_FIELD_BIT_OFFSET
       (cond
         [(and (icst? foff) (icst? boff) (zero? (remainder (integer-cst-value boff) 8)))
          (loop (tree-operand exp 0) (+ off (integer-cst-value foff) (quotient (integer-cst-value boff) 8)))]
         [else (values #f #f)])]
      [(array_ref)
       (define index (tree-operand exp 1))
       (define domain (type-attr st (tree-type (tree-operand exp 0)) 'domain #f))
       (define low (and domain (type-attr st domain 'min #f)))
       (define unit (type-attr st (tree-type exp) 'size-unit #f))
       (cond
         [(and (icst? index) (or (not low) (icst? low)) (icst? unit))
          (loop (tree-operand exp 0)
                (+ off (* (- (integer-cst-value index) (if low (integer-cst-value low) 0)) (integer-cst-value unit))))]
         [else (values #f #f)])]
      [(realpart_expr view_convert_expr) (loop (tree-operand exp 0) off)]
      [(imagpart_expr)
       (define u (type-attr st (tree-type exp) 'size-unit #f))
       (if (icst? u) (loop (tree-operand exp 0) (+ off (integer-cst-value u))) (values #f #f))]
      [(mem_ref)
       (define base (tree-operand exp 0))
       (cond
         [(eq? (tree-code base) 'addr_expr)
          (values (tree-operand base 0) (+ off (mem-ref-offset exp)))]
         [else (values exp off)])]
      [(bit_field_ref) (values #f #f)]
      [else (values exp off)])))

;; build_fold_addr_expr: &t, looking through *p and MEM[p, 0]
(define (build-fold-addr-expr st t)
  (cond
    [(and (eq? (tree-code t) 'mem_ref) (integer-zerop (tree-operand t 1))) (tree-operand t 0)]
    [(eq? (tree-code t) 'indirect_ref) (tree-operand t 0)]
    [else
     (let-values ([(pt st*) (pointer-to st (tree-type t))])
       ;; the state grows only when the pointer type is new; the caller
       ;; does not thread it, so such a type is created afresh each time
       ;; (its uid is never printed)
       (make-tree 'addr_expr #:type pt #:ops (list t) #:attrs (list (cons 'constant #f))))]))

;; ------------------------------------------------------------
;; maybe_canonicalize_mem_ref_addr: (values t changed?)
;; ------------------------------------------------------------

(define (canonicalize-mem-ref-addr st t0)
  (define addr? (eq? (tree-code t0) 'addr_expr))
  (define t (if addr? (tree-operand t0 0) t0))
  ;; walk to the innermost base, remembering the path
  (define-values (base rebuild)
    (let loop ([x t] [k (lambda (y) y)])
      (if (handled-component-p x)
          (loop (tree-operand x 0) (lambda (y) (k (tree-set-operand x 0 y))))
          (values x k))))
  (define changed #f)
  (define base1
    (cond
      [(and (eq? (tree-code base) 'mem_ref)
            (eq? (tree-code (tree-operand base 0)) 'addr_expr)
            (let ([a (tree-operand (tree-operand base 0) 0)])
              (or (eq? (tree-code a) 'mem_ref) (handled-component-p a))))
       (define-values (b off) (get-addr-base-and-unit-offset st (tree-operand (tree-operand base 0) 0)))
       (cond
         [b
          (set! changed #t)
          (tree-set-operand (tree-set-operand base 0 (build-fold-addr-expr st b))
                            1 (build-int-cst st (tree-type (tree-operand base 1)) (+ (mem-ref-offset base) off)))]
         [else base])]
      [else base]))
  ;; MEM[&decl, 0] -> decl
  (define base2
    (cond
      [(and (eq? (tree-code base1) 'mem_ref)
            (eq? (tree-code (tree-operand base1 0)) 'addr_expr)
            (integer-zerop (tree-operand base1 1))
            (let* ([decl (tree-operand (tree-operand base1 0) 0)]
                   [alias-type (tree-type (tree-operand base1 1))])
              (and (decl-p st decl)
                   (eq? (and (tree-this-volatile base1) #t) (and (tree-this-volatile decl) #t))
                   (eqv? (type-main-variant st (tree-type decl)) (type-main-variant st (type-type st alias-type)))
                   (eqv? (type-attr st (tree-type decl) 'align #f) (type-attr st (tree-type base1) 'align #f))
                   (types-compatible-p st (tree-type base1) (tree-type decl)))))
       (set! changed #t)
       (tree-operand (tree-operand base1 0) 0)]
      [else base1]))
  (define t1 (rebuild base2))
  (values (if addr? (tree-set-operand t0 0 t1) t1) changed))

;; ------------------------------------------------------------
;; gimple_simplify
;; ------------------------------------------------------------

;; an operand's defining assignment, looked through VALUEIZE: (values code op1 op2) or #f
(define (def-rhs m valueize t)
  (define v (if (ssa-name? t) (valueize t) t))
  (define g (and (ssa-name? v) (follow-ssa-edges?) (mfn-def-stmt m v)))
  (and g (gassign? g) (values (gassign-code g) (gassign-rhs1 g) (gassign-rhs2 g))))

(define (all-ones? st t)
  (and (icst? t)
       (let ([prec (type-precision* st (tree-type t))])
         (= (bitwise-and (integer-cst-value t) (sub1 (arithmetic-shift 1 prec))) (sub1 (arithmetic-shift 1 prec))))))

(define (same-operand? a b) (or (equal? a b) (and (ssa-name? a) (ssa-name? b) (ssa-name=? a b))))

;; one simplification step on (code op1 op2 op3) with result TYPE:
;; #f, or (list code op1 op2 op3) with a GIMPLE rhs class (single rhs = a value)
(define (simplify-step st m valueize type code a b c)
  (define (val x) (list (tree-code x) x #f #f))
  (define (un code x) (list code x #f #f))
  (define (bin code x y) (list code x y #f))
  (define integral? (integral-type-p st type))
  (define cls (gimple-rhs-class st code))
  (define (defr t) (if (ssa-name? t) (let-values ([(dc d1 d2) (def-rhs* t)]) (values dc d1 d2)) (values #f #f #f)))
  (define (def-rhs* t)
    (define g (and (ssa-name? t) (follow-ssa-edges?) (mfn-def-stmt m t)))
    (if (and g (gassign? g)) (values (gassign-code g) (gassign-rhs1 g) (gassign-rhs2 g)) (values #f #f #f)))
  (cond
    ;; constants
    [(and (eq? cls 'binary) (fold-const-binop st code type a b)) => val]
    [(and (eq? cls 'unary) (fold-const-unop st code type a)) => val]
    [(eq? cls 'binary)
     (define-values (ac a1 a2) (defr a))
     (define-values (bc b1 b2) (defr b))
     (cond
       ;; x + 0, x - 0, x | 0, x ^ 0, x << 0, x >> 0
       [(and (memq code '(plus_expr pointer_plus_expr minus_expr bit_ior_expr bit_xor_expr lshift_expr rshift_expr))
             (integer-zerop b) (useless-type-conversion-p st type (tree-type a)))
        (val a)]
       ;; x * 1, x / 1
       [(and (memq code '(mult_expr trunc_div_expr exact_div_expr)) (integer-onep b)) (val a)]
       ;; x * 0, x & 0
       [(and (memq code '(mult_expr bit_and_expr)) (integer-zerop b) integral?) (val (build-int-cst st type 0))]
       ;; x & -1
       [(and (eq? code 'bit_and_expr) (all-ones? st b)) (val a)]
       ;; x | -1
       [(and (eq? code 'bit_ior_expr) (all-ones? st b) integral?) (val (build-int-cst st type -1))]
       ;; x - x, x ^ x
       [(and (memq code '(minus_expr bit_xor_expr)) integral? (same-operand? a b)) (val (build-int-cst st type 0))]
       ;; x & x, x | x
       [(and (memq code '(bit_and_expr bit_ior_expr)) (same-operand? a b)) (val a)]
       ;; x == x, x != x
       [(and (memq code '(eq_expr ne_expr)) integral? (same-operand? a b) (integral-type-p st (tree-type a)))
        (val (build-int-cst st type (if (eq? code 'eq_expr) 1 0)))]
       ;; -1 - A -> ~A
       [(and (eq? code 'minus_expr) (all-ones? st a) integral?) (un 'bit_not_expr b)]
       ;; (cmp a b) != 0 / == 0 / == 1 / != 1 with a boolean comparison result
       [(and (memq code '(eq_expr ne_expr)) (icst? b) (or (integer-zerop b) (integer-onep b))
             ac (comparison-code? st ac) (boolean-type-p st (tree-type a)) (integral-type-p st (tree-type a1)))
        (define keep? (eq? (eq? code 'ne_expr) (integer-zerop b)))
        (cond
          [keep? (bin ac a1 a2)]
          [(invert-tree-comparison ac) => (lambda (ic) (bin ic a1 a2))]
          [else #f])]
       ;; ~A + 1 -> -A ; -A - 1 -> ~A
       [(and (eq? code 'plus_expr) (integer-onep b) (eq? ac 'bit_not_expr) (useless-type-conversion-p st type (tree-type a1)))
        (un 'negate_expr a1)]
       [(and (eq? code 'minus_expr) (integer-onep b) (eq? ac 'negate_expr) (useless-type-conversion-p st type (tree-type a1)))
        (un 'bit_not_expr a1)]
       [else #f])]
    [(eq? cls 'unary)
     (define-values (ac a1 a2) (defr a))
     (cond
       ;; useless conversion
       [(and (memq code '(nop_expr convert_expr)) (useless-type-conversion-p st type (tree-type a))) (val a)]
       ;; -(-x), ~~x
       [(and (eq? code 'negate_expr) (eq? ac 'negate_expr) (useless-type-conversion-p st type (tree-type a1))) (val a1)]
       [(and (eq? code 'bit_not_expr) (eq? ac 'bit_not_expr) (useless-type-conversion-p st type (tree-type a1))) (val a1)]
       ;; -(A + C) -> -C - A   (negate_expr_p C)
       [(and (eq? code 'negate_expr) (eq? ac 'plus_expr) integral?
             (or (negate-expr-p st a2) (negate-expr-p st a1)))
        (define-values (x y) (if (negate-expr-p st a2) (values a1 a2) (values a2 a1)))
        (define ny (if (icst? y) (fold-const-unop st 'negate_expr type y) #f))
        (and ny (bin 'minus_expr ny x))]
       ;; -(A - B) -> B - A
       [(and (eq? code 'negate_expr) (eq? ac 'minus_expr) integral?) (bin 'minus_expr a2 a1)]
       [else #f])]
    [else #f]))

;; canonical operand order for a binary rhs: (values code a b changed?)
(define (canonicalize-binary st code a b)
  (cond
    [(and (or (comparison-code? st code) (commutative-code? code)) (tree-swap-operands-p st a b))
     (values (swap-tree-comparison code) b a #t)]
    [else (values code a b #f)]))

;; gimple_simplify + gimple_resimplify on an assignment rhs:
;; #f or (list code op1 op2 op3), operands valueized
(define (simplify-assign-rhs st m valueize type code a b c)
  (define (v x) (if (ssa-name? x) (valueize x) x))
  (let loop ([code code] [a (v a)] [b (v b)] [c (v c)] [n 0] [changed #f])
    (define-values (code1 a1 b1 swapped?)
      (if (eq? (gimple-rhs-class st code) 'binary) (canonicalize-binary st code a b) (values code a b #f)))
    (define r (and (< n 4) (simplify-step st m valueize type code1 a1 b1 c)))
    (cond
      [r (loop (car r) (v (cadr r)) (v (caddr r)) (v (cadddr r)) (add1 n) #t)]
      [(or changed swapped?) (list code1 a1 b1 c)]
      [else #f])))

;; GIMPLE_COND: #f or (list code lhs rhs) (a constant result is (list code 1 0) style INTEGER_CST compare)
(define (simplify-cond st m valueize code a b)
  (define (v x) (if (ssa-name? x) (valueize x) x))
  (define bool (boolean-type st))
  (let loop ([code code] [a (v a)] [b (v b)] [n 0] [changed #f])
    (define-values (code1 a1 b1 swapped?) (canonicalize-binary st code a b))
    (define r (and (< n 4) (simplify-step st m valueize bool code1 a1 b1 #f)))
    (cond
      [(and r (eq? (gimple-rhs-class st (car r)) 'single))
       ;; a value: constant -> code (val != 0); an SSA bool -> (val != 0)
       (define x (cadr r))
       (cond
         [(icst? x) (list 'ne_expr x (build-int-cst st (tree-type x) 0))]
         [else (list 'ne_expr x (build-int-cst st (tree-type x) 0))])]
      [r (loop (car r) (v (cadr r)) (v (caddr r)) (add1 n) #t)]
      [(or changed swapped?) (list code1 a1 b1)]
      [else #f])))

;; ------------------------------------------------------------
;; fold_stmt_1 (not in place)
;; ------------------------------------------------------------

(define (fold-stmt st m id valueize)
  (define g0 (mfn-stmt m id))
  (define changed #f)
  (define (note! g) (set! changed #t) g)
  ;; 1-2: references and operand order
  (define g1
    (cond
      [(gassign? g0)
       (cond
         [(gassign-single-p st g0)
          (define rhs (gassign-rhs1 g0)) (define lhs (gassign-lhs g0))
          (define-values (rhs1 ch1)
            (if (or (eq? (tree-code-class st (tree-code rhs)) 'tcc_reference) (eq? (tree-code rhs) 'addr_expr))
                (canonicalize-mem-ref-addr st rhs)
                (values rhs #f)))
          (define-values (lhs1 ch2)
            (if (eq? (tree-code-class st (tree-code lhs)) 'tcc_reference) (canonicalize-mem-ref-addr st lhs) (values lhs #f)))
          (when (or ch1 ch2) (set! changed #t))
          ;; &MEM[ssa + cst] -> ssa p+ cst
          (cond
            [(and (eq? (tree-code rhs1) 'addr_expr)
                  (let ([inner (tree-operand rhs1 0)])
                    (and (eq? (tree-code inner) 'mem_ref) (ssa-name? (tree-operand inner 0)) (icst? (tree-operand inner 1)))))
             (define inner (tree-operand rhs1 0))
             (note! (gassign (gassign-loc g0) (gassign-flags g0) lhs1 'pointer_plus_expr (tree-operand inner 0)
                             (fold-convert st (sizetype st) (tree-operand inner 1)) #f))]
            [else (gassign (gassign-loc g0) (gassign-flags g0) lhs1 (tree-code rhs1) rhs1 #f #f)])]
         [else
          (define code (gassign-code g0))
          (define-values (code1 a b swapped?)
            (if (eq? (gimple-rhs-class st code) 'binary)
                (canonicalize-binary st code (gassign-rhs1 g0) (gassign-rhs2 g0))
                (values code (gassign-rhs1 g0) (gassign-rhs2 g0) #f)))
          (if swapped?
              (note! (gassign (gassign-loc g0) (gassign-flags g0) (gassign-lhs g0) code1 a b (gassign-rhs3 g0)))
              g0)])]
      [(gcond? g0)
       (define-values (code1 a b swapped?) (canonicalize-binary st (gcond-code g0) (gcond-lhs g0) (gcond-rhs g0)))
       (if swapped?
           (note! (gcond (gcond-loc g0) (gcond-flags g0) code1 a b (gcond-true-label g0) (gcond-false-label g0)))
           g0)]
      [else g0]))
  ;; 3: gimple_simplify
  (define g2
    (cond
      [(gassign? g1)
       (define lhs (gassign-lhs g1))
       (define r (and (not (eq? (gimple-rhs-class st (gassign-code g1)) 'single))
                      (simplify-assign-rhs st m valueize (tree-type lhs) (gassign-code g1)
                                           (gassign-rhs1 g1) (gassign-rhs2 g1) (gassign-rhs3 g1))))
       (cond
         [(and r (let ([rt (if (eq? (gimple-rhs-class st (car r)) 'single) (tree-type (cadr r)) (tree-type lhs))])
                   (useless-type-conversion-p st (tree-type lhs) rt)))
          (note! (gassign (gassign-loc g1) (gassign-flags g1) lhs (car r) (cadr r) (caddr r) (cadddr r)))]
         [else g1])]
      [(gcond? g1)
       (define r (simplify-cond st m valueize (gcond-code g1) (gcond-lhs g1) (gcond-rhs g1)))
       (if r (note! (gcond (gcond-loc g1) (gcond-flags g1) (car r) (cadr r) (caddr r) (gcond-true-label g1) (gcond-false-label g1))) g1)]
      [else g1]))
  ;; 4: post rules
  (define g3
    (cond
      [(and (gassign? g2) (memq (gassign-code g2) '(eq_expr ne_expr))
            (let ([op2 (gassign-rhs2 g2)])
              (and (icst? op2) (or (integer-zerop op2) (integer-onep op2))
                   (useless-type-conversion-p st (tree-type (gassign-lhs g2)) (tree-type (gassign-rhs1 g2))))))
       (define op1 (gassign-rhs1 g2)) (define op2 (gassign-rhs2 g2))
       (define type (tree-type op1))
       (define logical-not? (or (and (eq? (gassign-code g2) 'eq_expr) (integer-zerop op2))
                                (and (eq? (gassign-code g2) 'ne_expr) (integer-onep op2))))
       (note!
        (cond
          [(not logical-not?) (gassign (gassign-loc g2) (gassign-flags g2) (gassign-lhs g2) (tree-code op1) op1 #f #f)]
          [(eqv? (type-precision* st type) 1) (gassign (gassign-loc g2) (gassign-flags g2) (gassign-lhs g2) 'bit_not_expr op1 #f #f)]
          [else (gassign (gassign-loc g2) (gassign-flags g2) (gassign-lhs g2) 'bit_xor_expr op1 (build-int-cst st type 1) #f)]))]
      [(and (greturn? g2) (ssa-name? (greturn-retval g2)))
       (define ret (greturn-retval g2))
       (define val (valueize ret))
       (if (and val (not (equal? val ret)) (may-propagate-copy st ret val))
           (note! (greturn (greturn-loc g2) (greturn-flags g2) val))
           g2)]
      [else g2]))
  (when changed (mfn-replace! m id g3))
  changed)

;; ------------------------------------------------------------
;; gimple_fold_stmt_to_constant_1 with one valueization function: a
;; constant, an SSA name, or #f
;; ------------------------------------------------------------

(define (gimple-fold-stmt-to-constant st m g valueize)
  (define (v t) (if (ssa-name? t) (valueize t) t))
  (cond
    [(gassign? g)
     (define lhs (gassign-lhs g))
     (define code (gassign-code g))
     (define r (and (not (eq? (gimple-rhs-class st code) 'single))
                    (simplify-assign-rhs st m valueize (tree-type lhs) code (gassign-rhs1 g) (gassign-rhs2 g) (gassign-rhs3 g))))
     (cond
       [(and r (eq? (gimple-rhs-class st (car r)) 'single)
             (let ([x (cadr r)]) (or (ssa-name? x) (is-gimple-min-invariant st x))))
        (cadr r)]
       [else
        (case (gimple-rhs-class st code)
          [(single)
           (define rhs (gassign-rhs1 g))
           (cond
             [(ssa-name? rhs) (v rhs)]
             [(eq? (tree-code rhs) 'constructor) #f]
             [(eq? (tree-code-class st (tree-code rhs)) 'tcc_reference) (fold-const-aggregate-ref st rhs v)]
             [(decl-p st rhs) (get-symbol-constant-value st rhs)]
             [(is-gimple-min-invariant st rhs) rhs]
             [else #f])]
          [(binary)
           (cond
             [(memq code '(eq_expr ne_expr))
              (define op0 (gassign-rhs1 g))
              (and (useless-type-conversion-p st (tree-type lhs) (tree-type op0))
                   (let* ([op1 (v (gassign-rhs2 g))] [op0* (v op0)])
                     (define-values (a b) (if (icst? op0*) (values op1 op0*) (values op0* op1)))
                     (and (icst? b)
                          (or (and (eq? code 'ne_expr) (integer-zerop b)) (and (eq? code 'eq_expr) (integer-onep b)))
                          a)))]
             [else #f])]
          [(ternary)
           (define op0 (v (gassign-rhs1 g)))
           (and (eq? code 'cond_expr) (icst? op0)
                (v (if (integer-zerop op0) (gassign-rhs3 g) (gassign-rhs2 g))))]
          [else #f])])]
    [(gcond? g)
     (define r (simplify-cond st m valueize (gcond-code g) (gcond-lhs g) (gcond-rhs g)))
     (and r (icst? (cadr r)) (icst? (caddr r))
          (fold-const-compare st (car r) (boolean-type st) (cadr r) (caddr r)))]
    [else #f]))
