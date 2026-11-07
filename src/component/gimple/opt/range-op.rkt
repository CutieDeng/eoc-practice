#lang racket/base

;; ============================================================
;; opt: range operators (gcc/range-op.cc, GCC 15) for integer ranges
;; ============================================================
;;
;;   (ro-supported? code)
;;   (ro-fold st code type lh rh)      -> irange      fold_range
;;   (ro-op1 st code type lhs op2)     -> irange | #f op1_range
;;   (ro-op2 st code type lhs op1)     -> irange | #f op2_range
;;
;; TYPE is the result type for fold and the operand's type for op1 /
;; op2.  Unary operators get the varying range of TYPE as RH (what
;; fold_using_range passes).  Codes: plus, minus, mult, negate,
;; bit_not, nop / convert (operator_cast), the six comparisons,
;; bit_and / bit_ior / bit_xor, min, max, ssa_name (identity),
;; integer_cst, pointer_plus.  Pointers are ranges over their unsigned
;; bits (prange is not ported): pointer_plus knows nonzero / zero /
;; varying only.
;;
;; The base fold_range (wi_fold_in_parts per pair, the union over pair
;; products, value_range_with_overflow, update_known_bitmask through
;; tree-ssa-ccp's bit_value_binop / bit_value_unop) is ported exactly.
;; No relation oracle: every relation is VREL_VARYING, so the relation
;; effects and relop_early_resolve's relation cases never apply.
;; Not ported: division, shifts, abs, the widening operators,
;; operator_mult::op1_range's division path.
;; ============================================================

(require racket/list
         "../../../kernel/ir/gimple/types.rkt"
         "../state.rkt"
         "../tree.rkt"
         "irange.rkt"
         "bit-value.rkt")

(provide ro-supported? ro-fold ro-op1 ro-op2 range-true range-false range-true-and-false
         wi-add wi-sub value-range-with-overflow)

(define supported
  '(plus_expr minus_expr mult_expr negate_expr bit_not_expr nop_expr convert_expr
    lt_expr le_expr gt_expr ge_expr eq_expr ne_expr bit_and_expr bit_ior_expr bit_xor_expr
    min_expr max_expr ssa_name integer_cst pointer_plus_expr))
(define (ro-supported? code) (and (memq code supported) #t))

(define (bool-ti st) (tinfo st (boolean-type st)))
(define (range-true t) (ir-set t 1 1))
(define (range-false t) (ir-set t 0 0))
(define (range-true-and-false t) (ir-set t 0 1))

;; ------------------------------------------------------------
;; wide_int arithmetic with overflow kinds
;; ------------------------------------------------------------

(define (wrap x t) (from-pattern x t))
(define (ovf-kind x t) (cond [(> x (ti-max t)) 'overflow] [(< x (ti-min t)) 'underflow] [else 'none]))
(define (wi-add x y t) (define r (+ x y)) (values (wrap r t) (ovf-kind r t)))
(define (wi-sub x y t) (define r (- x y)) (values (wrap r t) (ovf-kind r t)))

;; value_range_from_overflowed_bounds
(define (range-from-overflowed-bounds t wmin wmax)
  (define tmin1 (wrap (+ wmax 1) t))
  (define tmax1 (wrap (- wmin 1) t))
  (define covers (or (< tmin1 wmax) (> tmax1 wmin)))
  (if (or covers (> tmin1 tmax1)) (ir-varying t) (ir-set-anti t tmin1 tmax1)))

;; value_range_with_overflow
(define (value-range-with-overflow t wmin wmax [min-ovf 'none] [max-ovf 'none])
  (cond
    [(and (= 1 (ti-prec t)) (not (= wmax wmin))) (ir-varying t)]
    [(ti-wraps t)
     (cond
       [(eq? (not (eq? min-ovf 'none)) (not (eq? max-ovf 'none)))
        (if (> wmin wmax) (ir-varying t) (ir-set t wmin wmax))]
       [(or (and (eq? min-ovf 'underflow) (eq? max-ovf 'none))
            (and (eq? max-ovf 'overflow) (eq? min-ovf 'none)))
        (range-from-overflowed-bounds t wmin wmax)]
       [else (ir-varying t)])]
    [else
     (cond
       [(or (and (eq? min-ovf 'overflow) (eq? max-ovf 'overflow))
            (and (eq? min-ovf 'underflow) (eq? max-ovf 'underflow)))
        (ir-undefined t)]
       [else
        (define lb (case min-ovf [(underflow) (ti-min t)] [(overflow) (ti-max t)] [else wmin]))
        (define ub (case max-ovf [(underflow) (ti-min t)] [(overflow) (ti-max t)] [else wmax]))
        (ir-set t lb ub)])]))

;; create_possibly_reversed_range
(define (possibly-reversed t lb ub)
  (if (> lb ub) (range-from-overflowed-bounds t lb ub) (ir-set t lb ub)))

;; ------------------------------------------------------------
;; wi_fold per operator
;; ------------------------------------------------------------

(define (wi-fold code t a b c d)          ; lh = [a, b], rh = [c, d]
  (case code
    [(plus_expr)
     (define-values (lo o1) (wi-add a c t)) (define-values (hi o2) (wi-add b d t))
     (value-range-with-overflow t lo hi o1 o2)]
    [(minus_expr)
     (define-values (lo o1) (wi-sub a d t)) (define-values (hi o2) (wi-sub b c t))
     (value-range-with-overflow t lo hi o1 o2)]
    [(min_expr) (value-range-with-overflow t (min a c) (min b d))]
    [(max_expr) (value-range-with-overflow t (max a c) (max b d))]
    [(mult_expr) (mult-fold t a b c d)]
    [(bit_and_expr) (and-fold t a b c d)]
    [(bit_ior_expr) (ior-fold t a b c d)]
    [(bit_xor_expr) (xor-fold t a b c d)]
    [else (ir-varying t)]))

;; operator_mult::wi_op_overflows -> (values res overflowed?)
(define (mult-op t w0 w1)
  (define p (* w0 w1))
  (cond
    [(eq? (ovf-kind p t) 'none) (values p #f)]
    [(not (ti-wraps t))
     (values (if (or (ti-uns t) (eq? (< w0 0) (< w1 0))) (ti-max t) (ti-min t)) #f)]
    [else (values (wrap p t) #t)]))

(define (mult-fold t a b c d)
  (cond
    [(not (ti-wraps t))
     ;; cross_product_operator::wi_cross_product
     (let/ec return
       (define (op x y) (let-values ([(r ov) (mult-op t x y)]) (when ov (return (ir-varying t))) r))
       (define cp1 (op a c))
       (define cp3 (if (= a b) cp1 (op b c)))
       (define cp2 (if (= c d) cp1 (op a d)))
       (define cp4 (if (= a b) cp2 (op b d)))
       (define-values (x1 x2) (if (> cp1 cp2) (values cp2 cp1) (values cp1 cp2)))
       (define-values (x3 x4) (if (> cp3 cp4) (values cp4 cp3) (values cp3 cp4)))
       (value-range-with-overflow t (min x1 x3) (max x2 x4)))]
    [else
     ;; the widest2_int algorithm for wrapping types
     (define prec (ti-prec t))
     (define size (arithmetic-shift 1 prec)) (define sizem1 (sub1 size))
     (define-values (min0 max0) (if (and (ti-uns t) (< size (+ a b))) (values (- a size) (- b size)) (values a b)))
     (define-values (min1 max1) (if (and (ti-uns t) (< size (+ c d))) (values (- c size) (- d size)) (values c d)))
     (define ps (sort (list (* min0 min1) (* min0 max1) (* max0 min1) (* max0 max1)) <))
     (define prod0 (first ps)) (define prod3 (last ps))
     (cond
       [(>= (- prod3 prod0) sizem1)
        (cond
          [(and (ti-uns t) (= c d) (>= (exact-log2 c) 0) (> prec 1))
           (define-values (r _c) (ir-union (ir-set t c (ti-max t)) (ir-zero t)))
           r]
          [else (ir-varying t)])]
       [else (possibly-reversed t (wrap prod0 t) (wrap prod3 t))])]))

;; wi_optimize_and_or -> irange or #f
(define (optimize-and-or code t a b c d)
  (define prec (ti-prec t))
  (define-values (mask lo hi)
    (cond [(= c d) (values c a b)] [(= a b) (values a c d)] [else (values #f #f #f)]))
  (cond
    [(not mask) #f]
    [else
     (define w0 (to-pattern (if (eq? code 'bit_ior_expr) (bitwise-not mask) mask) prec))
     (define-values (m n)
       (cond
         [(zero? w0) (values 0 prec)]
         [else
          (define n (let loop ([x w0] [k 0]) (if (odd? x) k (loop (arithmetic-shift x -1) (add1 k)))))
          (define w (to-pattern (bitwise-not (bitwise-ior w0 (mask-low n))) prec))
          (if (zero? w)
              (values (- prec n) n)
              (values (- (let loop ([x w] [k 0]) (if (odd? x) k (loop (arithmetic-shift x -1) (add1 k)))) n) n))]))
     (define new-mask* (to-pattern (bitwise-not (mask-low (+ m n))) prec))   ; wi::mask (m + n, true, prec)
     (cond
       [(not (= (bitwise-and new-mask* (to-pattern lo prec)) (bitwise-and new-mask* (to-pattern hi prec)))) #f]
       [else
        (define-values (rl ru)
          (if (eq? code 'bit_and_expr)
              (values (wrap (bitwise-and lo mask) t) (wrap (bitwise-and hi mask) t))
              (values (wrap (bitwise-ior lo mask) t) (wrap (bitwise-ior hi mask) t))))
        (define r (value-range-with-overflow t rl ru))
        (if (and (eq? code 'bit_ior_expr) (not (= mask 0)))
            (let-values ([(r2 _c) (ir-intersect r (ir-nonzero t))]) r2)
            r)])]))

;; wi_set_zero_nonzero_bits -> (values maybe-nonzero mustbe-nonzero), as patterns
(define (zero-nonzero-bits t lb ub)
  (define prec (ti-prec t))
  (cond
    [(= lb ub) (values (to-pattern lb prec) (to-pattern lb prec))]
    [(or (>= lb 0) (< ub 0))
     (define l (to-pattern lb prec)) (define u (to-pattern ub prec))
     (define xm (bitwise-xor l u))
     (define mz (bitwise-ior l u)) (define mb (bitwise-and l u))
     (if (zero? xm)
         (values mz mb)
         (let ([m (mask-low (sub1 (integer-length xm)))])
           (values (bitwise-ior mz m) (bitwise-and mb (bitwise-not m)))))]
    [else (values (sub1 (arithmetic-shift 1 prec)) 0)]))

;; wi_optimize_signed_bitwise_op -> irange or #f
(define (optimize-signed-bitwise t a b c d)
  (define prec (ti-prec t))
  (define (clrsb x) (clrsb-in x prec))          ; X is the signed value
  (define nc (min (clrsb a) (clrsb b) (clrsb c) (clrsb d)))
  (cond
    [(= nc 0) #f]
    [else
     (define rprec (- prec nc 1))
     (value-range-with-overflow t (wrap (mask-high rprec) t) (wrap (mask-low rprec) t))]))

(define (and-fold t a b c d)
  (or (optimize-and-or 'bit_and_expr t a b c d)
      (let ()
        (define-values (mz-l mb-l) (zero-nonzero-bits t a b))
        (define-values (mz-r mb-r) (zero-nonzero-bits t c d))
        (define lb0 (wrap (bitwise-and mb-l mb-r) t))
        (define ub0 (wrap (bitwise-and mz-l mz-r) t))
        (define ub1 (let* ([u ub0]
                           [u (if (and (< b 0) (< d 0)) (min u b d) u)]
                           [u (if (>= a 0) (min u b) u)]
                           [u (if (>= c 0) (min u d) u)])
                      u))
        (define-values (lb ub)
          (if (and (> lb0 ub1) (not (ti-uns t))
                   (or (and (= a b) (= a (ti-min t))) (and (= c d) (= c (ti-min t)))))
              (values (ti-min t) 0)
              (values lb0 ub1)))
        (cond
          [(> lb ub) (or (and (not (ti-uns t)) (optimize-signed-bitwise t a b c d)) (ir-varying t))]
          [else (value-range-with-overflow t lb ub)]))))

(define (ior-fold t a b c d)
  (or (optimize-and-or 'bit_ior_expr t a b c d)
      (let ()
        (define-values (mz-l mb-l) (zero-nonzero-bits t a b))
        (define-values (mz-r mb-r) (zero-nonzero-bits t c d))
        (define lb0 (wrap (bitwise-ior mb-l mb-r) t))
        (define ub (wrap (bitwise-ior mz-l mz-r) t))
        (define lb (let* ([l lb0]
                          [l (if (and (>= a 0) (>= c 0)) (max l a c) l)]
                          [l (if (< b 0) (max l a) l)]
                          [l (if (< d 0) (max l c) l)])
                     l))
        (cond
          [(> lb ub)
           (cond
             [(or (> a 0) (> c 0) (< b 0) (< d 0)) (ir-nonzero t)]
             [(and (not (ti-uns t)) (optimize-signed-bitwise t a b c d)) => values]
             [else (ir-varying t)])]
          [else (value-range-with-overflow t lb ub)]))))

(define (xor-fold t a b c d)
  (define prec (ti-prec t))
  (define-values (mz-l mb-l) (zero-nonzero-bits t a b))
  (define-values (mz-r mb-r) (zero-nonzero-bits t c d))
  (define zero-bits (to-pattern (bitwise-ior (bitwise-and mb-l mb-r) (bitwise-not (bitwise-ior mz-l mz-r))) prec))
  (define one-bits (to-pattern (bitwise-ior (bitwise-and mb-l (bitwise-not mz-r)) (bitwise-and mb-r (bitwise-not mz-l))) prec))
  (define ub (wrap (bitwise-not zero-bits) t))
  (define lb (wrap one-bits t))
  (define r
    (cond
      [(or (< lb 0) (>= ub 0)) (value-range-with-overflow t lb ub)]
      [(and (not (ti-uns t)) (optimize-signed-bitwise t a b c d)) => values]
      [else (ir-varying t)]))
  (if (or (< b c) (< d a) (not (= one-bits 0)))
      (let-values ([(r2 _c) (ir-intersect r (ir-nonzero t))]) r2)
      r))

;; wi_fold_in_parts
(define (fold-in-parts code t a b c d)
  (define rh-range (- d c)) (define lh-range (- b a))
  (define (u r1 r2) (let-values ([(r _c) (ir-union r1 r2)]) r))
  (cond
    [(and (> rh-range 0) (< rh-range 4))
     (let* ([r (fold-in-parts code t a b c c)]
            [r (if (> rh-range 1) (u r (fold-in-parts code t a b (+ c 1) (+ c 1))) r)]
            [r (if (= rh-range 3) (u r (fold-in-parts code t a b (+ c 2) (+ c 2))) r)])
       (u r (fold-in-parts code t a b d d)))]
    [(and (> lh-range 0) (< lh-range 4))
     (let* ([r (wi-fold code t a a c d)]
            [r (if (> lh-range 1) (u r (wi-fold code t (+ a 1) (+ a 1) c d)) r)]
            [r (if (= lh-range 3) (u r (wi-fold code t (+ a 2) (+ a 2) c d)) r)])
       (u r (wi-fold code t b b c d)))]
    [else (wi-fold code t a b c d)]))

;; update_known_bitmask
(define unary-codes '(nop_expr convert_expr negate_expr bit_not_expr abs_expr absu_expr))
(define (update-known-bitmask r code lh rh)
  (cond
    [(or (ir-undefined? r) (ir-undefined? lh) (ir-undefined? rh) (ir-singleton r)) r]
    [else
     (define t (irange-ti r))
     (define prec (ti-prec t)) (define uns (ti-uns t))
     (define lt (irange-ti lh)) (define rt (irange-ti rh))
     (define lb (ir-get-bitmask lh)) (define rb (ir-get-bitmask rh))
     (define-values (v m)
       (if (memq code unary-codes)
           (bv-unop (if (memq code '(nop_expr convert_expr)) 'convert_expr code) uns prec
                    (ext (cdr lb) (ti-prec lt) (ti-uns lt)) (ext (car lb) (ti-prec lt) (ti-uns lt)) (ti-uns lt) (ti-prec lt))
           ;; widest_int::from (bits, TYPE_SIGN (result type))
           (bv-binop code uns prec
                     (ext (cdr lb) (ti-prec lt) uns) (ext (car lb) (ti-prec lt) uns) (ti-uns lt) (ti-prec lt)
                     (ext (cdr rb) (ti-prec rt) uns) (ext (car rb) (ti-prec rt) uns) (ti-uns rt) (ti-prec rt))))
     (define mask (to-pattern m prec))
     (define value (bitwise-and (to-pattern v prec) (to-pattern (bitwise-not mask) prec)))
     (ir-update-bitmask r (cons mask value))]))

;; range_operator::fold_range for the wi_fold based operators
(define (base-fold code t lh rh)
  (cond
    [(or (ir-undefined? lh) (ir-undefined? rh)) (ir-varying t)]
    [else
     (define nl (ir-num-pairs lh)) (define nr (ir-num-pairs rh))
     (define r
       (cond
         [(or (and (= nl 1) (= nr 1)) (> (* nl nr) 12))
          (fold-in-parts code t (ir-lower lh) (ir-upper lh) (ir-lower rh) (ir-upper rh))]
         [else
          (let/ec return
            (for*/fold ([r (ir-undefined t)]) ([p (in-list (irange-pairs lh))] [q (in-list (irange-pairs rh))])
              (define-values (r1 _c) (ir-union r (fold-in-parts code t (car p) (cdr p) (car q) (cdr q))))
              (if (ir-varying? r1) (return r1) r1)))]))
     (update-known-bitmask r code lh rh)]))

;; ------------------------------------------------------------
;; casts
;; ------------------------------------------------------------

(define (cast-fold-pair t inner-t lo hi)
  (define outer-prec (ti-prec t)) (define inner-prec (ti-prec inner-t))
  (define truncating (< outer-prec inner-prec))
  (cond
    [(and truncating
          (let* ([diff (from-pattern (- hi lo) inner-t)])
            (not (zero? (arithmetic-shift diff (- outer-prec))))))
     (ir-varying t)]
    [else
     (possibly-reversed t (from-pattern lo t) (from-pattern hi t))]))

(define (cast-fold t inner)
  (cond
    [(ir-undefined? inner) (ir-varying t)]
    [else
     (define it (irange-ti inner))
     (define ps (irange-pairs inner))
     ;; the union stops (without the bitmask update) once a later pair makes it varying
     (let/ec return
       (define r
         (for/fold ([r (cast-fold-pair t it (car (car ps)) (cdr (car ps)))]) ([p (in-list (cdr ps))])
           (define-values (r1 _c) (ir-union r (cast-fold-pair t it (car p) (cdr p))))
           (if (ir-varying? r1) (return r1) r1)))
       (update-known-bitmask r 'convert_expr inner (ir-varying t)))]))

;; operator_cast::op1_range (integral operands): T is the operand's type,
;; LHS the range of the cast's result, OP2 the operand's known range
(define (cast-op1 st t lhs op2)
  (define lt (irange-ti lhs))
  (define (isect a b) (let-values ([(x _c) (ir-intersect a b)]) x))
  (define (unite a b) (let-values ([(x _c) (ir-union a b)]) x))
  (cond
    [(ir-undefined? lhs) #f]
    [(< (ti-prec lt) (ti-prec t))
     ;; truncating cast
     (cond
       [(ir-varying? lhs) (isect (ir-varying t) op2)]
       [else
        (define prec (ti-prec t)) (define lprec (ti-prec lt))
        (define converted (cast-fold t (cast-fold (ti (ti-type lt) lprec #t #t) lhs)))
        (define lim (arithmetic-shift 1 lprec))
        (define r0 (unite (possibly-reversed t (from-pattern lim t) (from-pattern (mask-low (sub1 prec)) t)) converted))
        (define lim2 (to-pattern (bitwise-not (mask-low lprec)) prec))
        (define neg0 (base-fold 'plus_expr t converted (ir-const t (from-pattern lim2 t))))
        (define min-val (arithmetic-shift 1 (sub1 prec)))
        (define neg (if (= lim2 min-val) neg0 (unite neg0 (ir-set t (from-pattern min-val t) (from-pattern (sub1 lim2) t)))))
        (isect (unite r0 neg) op2)])]
    [else
     (define tmp (if (= (ti-prec lt) (ti-prec t)) lhs (isect (cast-fold lt (ir-varying t)) lhs)))
     (cast-fold t tmp)]))

;; ------------------------------------------------------------
;; comparisons
;; ------------------------------------------------------------

;; get_bool_state -> 'empty 'false 'true 'full
(define (bool-state lhs)
  (cond [(ir-undefined? lhs) 'empty]
        [(ir-zero? lhs) 'false]
        [(ir-contains? lhs 0) 'full]
        [else 'true]))

(define (build-lt t v) (if (= v (ti-min t)) (ir-undefined t) (ir-set t (ti-min t) (sub1 v))))
(define (build-le t v) (ir-set t (ti-min t) v))
(define (build-gt t v) (if (= v (ti-max t)) (ir-undefined t) (ir-set t (add1 v) (ti-max t))))
(define (build-ge t v) (ir-set t v (ti-max t)))

(define (compare-fold bt code op1 op2)
  (cond
    [(or (ir-undefined? op1) (ir-undefined? op2)) (ir-varying bt)]
    [else
     (case code
       [(eq_expr ne_expr)
        (define c1 (ir-singleton op1)) (define c2 (ir-singleton op2))
        (define is-eq (eq? code 'eq_expr))
        (cond
          [(and c1 c2) (if (eq? (= c1 c2) is-eq) (range-true bt) (range-false bt))]
          [else
           (define-values (tmp _c) (ir-intersect op1 op2))
           (cond
             [(or (ir-undefined? tmp)
                  (and c2 (not (bm-member? (irange-ti op1) (ir-get-bitmask op1) c2)))
                  (and c1 (not (bm-member? (irange-ti op2) (ir-get-bitmask op2) c1))))
              (if is-eq (range-false bt) (range-true bt))]
             [else (range-true-and-false bt)])])]
       [(lt_expr)
        (cond
          [(< (ir-upper op1) (ir-lower op2)) (range-true bt)]
          [(not (< (ir-lower op1) (ir-upper op2))) (range-false bt)]
          [(and (ir-zero? op2) (let ([t1 (irange-ti op1)]) (or (ti-uns t1) (not (bitwise-bit-set? (ir-nonzero-bits op1) (sub1 (ti-prec t1)))))))
           (range-false bt)]
          [else (range-true-and-false bt)])]
       [(le_expr)
        (cond [(<= (ir-upper op1) (ir-lower op2)) (range-true bt)]
              [(not (<= (ir-lower op1) (ir-upper op2))) (range-false bt)]
              [else (range-true-and-false bt)])]
       [(gt_expr)
        (cond [(> (ir-lower op1) (ir-upper op2)) (range-true bt)]
              [(not (> (ir-upper op1) (ir-lower op2))) (range-false bt)]
              [else (range-true-and-false bt)])]
       [(ge_expr)
        (cond [(>= (ir-lower op1) (ir-upper op2)) (range-true bt)]
              [(not (>= (ir-upper op1) (ir-lower op2))) (range-false bt)]
              [else (range-true-and-false bt)])])]))

;; op1_range of a comparison (OP2 is the other operand); op2_range uses
;; the swapped comparison with the same builder table
(define (compare-op1 t code lhs op2)
  (define st (bool-state lhs))
  (cond
    [(and (memq code '(lt_expr le_expr gt_expr ge_expr)) (ir-undefined? op2)) #f]
    [(eq? st 'empty) (ir-undefined t)]
    [(eq? st 'full) (ir-varying t)]
    [else
     (define true? (eq? st 'true))
     (case code
       [(eq_expr ne_expr)
        (define want-eq (if (eq? code 'eq_expr) true? (not true?)))
        (cond
          [want-eq (ir-cast-type op2 t)]
          [(and (not (ir-undefined? op2)) (ir-singleton op2)) (ir-invert (ir-cast-type op2 t))]
          [else (ir-varying t)])]
       [(lt_expr) (if true? (build-lt t (ir-upper op2)) (build-ge t (ir-lower op2)))]
       [(le_expr) (if true? (build-le t (ir-upper op2)) (build-gt t (ir-lower op2)))]
       [(gt_expr) (if true? (build-gt t (ir-lower op2)) (build-le t (ir-upper op2)))]
       [(ge_expr) (if true? (build-ge t (ir-lower op2)) (build-lt t (ir-upper op2)))])]))
(define (swap-cmp code)
  (case code [(lt_expr) 'gt_expr] [(gt_expr) 'lt_expr] [(le_expr) 'ge_expr] [(ge_expr) 'le_expr] [else code]))

;; ------------------------------------------------------------
;; bit_and op1_range (operator_bitwise_and)
;; ------------------------------------------------------------

(define (masked-increment val-in mask sgnbit prec)
  (define val (to-pattern (bitwise-xor val-in sgnbit) prec))
  (let loop ([i 0] [bit 1])
    (cond
      [(>= i prec) (to-pattern (bitwise-xor val sgnbit) prec)]
      [(zero? (bitwise-and mask bit)) (loop (add1 i) (* 2 bit))]
      [else
       (define res (bitwise-and (to-pattern (bitwise-and (+ val bit) (bitwise-not (sub1 bit))) prec) mask))
       (if (> res val) (to-pattern (bitwise-xor res sgnbit) prec) (loop (add1 i) (* 2 bit)))])))

(define (nonzero-from-mask t lhs)
  (if (or (ir-undefined? lhs) (ir-contains? lhs 0)) (ir-varying t) (ir-nonzero t)))

(define (and-simple-op1 t lhs op2)
  (cond
    [(not (ir-singleton op2)) (nonzero-from-mask t lhs)]
    [else
     (define prec (ti-prec t))
     (define cst2v (to-pattern (ir-lower op2) prec))
     (define cst2n (and (not (ti-uns t)) (< (ir-lower op2) 0)))
     (define sgnbit (if cst2n (arithmetic-shift 1 (sub1 prec)) 0))
     (define valv (to-pattern (ir-lower lhs) prec))
     (define-values (minv nothing?)
       (let ([m (bitwise-and valv cst2v)])
         (cond [(= m valv) (values m #f)]
               [else (define m2 (masked-increment valv cst2v sgnbit prec))
                     (values m2 (= m2 valv))])))
     (define maxv0 (mask-low (- prec (if cst2n 1 0))))
     (define r0 (if nothing? (ir-varying t) (possibly-reversed t (from-pattern minv t) (from-pattern maxv0 t))))
     (define valv2 (to-pattern (ir-upper lhs) prec))
     (define minv2 (bitwise-and valv2 cst2v))
     (let/ec return
       (define maxv1
         (cond [(= minv2 valv2) valv2]
               [else (define m (masked-increment valv2 cst2v sgnbit prec))
                     (when (= m valv2) (return (if nothing? (ir-undefined t) r0)))
                     (sub1 m)]))
       (define maxv (to-pattern (bitwise-ior maxv1 (bitwise-not cst2v)) prec))
       (define upper (possibly-reversed t (from-pattern sgnbit t) (from-pattern maxv t)))
       (let-values ([(r _c) (ir-intersect r0 upper)]) r))]))

(define (and-op1 st t lhs op2)
  (cond
    [(ir-undefined? lhs) #f]
    [(boolean-type-p st (ti-type t))
     (if (eq? (bool-state lhs) 'true) (range-true t) (range-true-and-false t))]
    [else
     (define r0
       (for/fold ([r (ir-undefined t)]) ([p (in-list (irange-pairs lhs))])
         (define-values (u _c) (ir-union r (and-simple-op1 t (ir-set (irange-ti lhs) (car p) (cdr p)) op2)))
         u))
     (define r1 (if (ir-undefined? r0) (nonzero-from-mask t lhs) r0))
     (define m (ir-singleton lhs))
     (cond
       [(and m (ir-equal? lhs op2))
        (ir-update-bitmask r1 (cons (to-pattern (bitwise-not m) (ti-prec t)) (to-pattern m (ti-prec t))))]
       [(and (ir-zero? lhs) (ir-singleton op2))
        (define nz (to-pattern (bitwise-not (ir-nonzero-bits op2)) (ti-prec t)))
        (define tmp (ir-set-nonzero-bits (ir-varying t) nz))
        (let-values ([(r _c) (ir-intersect r1 tmp)]) r)]
       [else r1])]))

(define (ior-op1 st t lhs op2)
  (cond
    [(ir-undefined? lhs) #f]
    [(boolean-type-p st (ti-type t))
     (if (eq? (bool-state lhs) 'false) (range-false t) (range-true-and-false t))]
    [(ir-zero? lhs) (ir-zero t)]
    [(and (not (ti-uns t)) (>= (ir-lower lhs) 0)) (ir-set t 0 (ti-max t))]
    [else (ir-varying t)]))

(define (xor-op1 st t lhs op2)
  (cond
    [(or (ir-undefined? lhs) (ir-varying? lhs)) lhs]
    [(boolean-type-p st (ti-type t))
     (case (bool-state lhs)
       [(true) (cond [(ir-varying? op2) (ir-varying t)]
                     [(ir-zero? op2) (range-true t)]
                     [(or (ir-undefined? op2) (ir-contains? op2 0)) (range-true-and-false t)]
                     [else (range-false t)])]
       [(false) (ir-cast-type op2 t)]
       [else (ir-varying t)])]
    [else (ir-varying t)]))

;; ------------------------------------------------------------
;; entry points
;; ------------------------------------------------------------

(define (ro-fold st code type lh rh)
  (define t (tinfo st type))
  (case code
    [(ssa_name integer_cst) (if (ir-undefined? lh) lh (ir-cast-type lh t))]
    [(nop_expr convert_expr) (cast-fold t lh)]
    [(negate_expr) (if (or (ir-undefined? lh) (ir-undefined? rh)) (ir-varying t) (base-fold 'minus_expr t (ir-zero t) lh))]
    [(bit_not_expr)
     (cond
       [(or (ir-undefined? lh) (ir-undefined? rh)) (ir-varying t)]
       [(boolean-type-p st type) (if (ir-varying? lh) lh (ir-invert lh))]
       [else (base-fold 'minus_expr t (ir-const t -1) lh)])]
    [(lt_expr le_expr gt_expr ge_expr eq_expr ne_expr) (compare-fold t code lh rh)]
    [(pointer_plus_expr)
     (cond
       [(or (ir-undefined? lh) (ir-undefined? rh)) (ir-varying t)]
       [(and (= 0 (ir-lower lh) (ir-upper lh)) (= (ir-lower rh) (ir-upper rh))) (ir-const t (ir-lower rh))]
       ;; -fdelete-null-pointer-checks is on
       [(and (or (not (and (<= (ir-lower lh) 0) (>= (ir-upper lh) 0)))
                 (not (and (<= (ir-lower rh) 0) (>= (ir-upper rh) 0))))
             (not (ti-wraps t)))
        (ir-nonzero t)]
       [else (ir-varying t)])]
    [else (base-fold code t lh rh)]))

(define (ro-op1 st code type lhs op2)
  (define t (tinfo st type))
  (case code
    [(ssa_name) lhs]
    [(plus_expr) (if (ir-undefined? lhs) #f (ro-fold st 'minus_expr type lhs op2))]
    [(minus_expr) (if (ir-undefined? lhs) #f (ro-fold st 'plus_expr type lhs op2))]
    [(pointer_plus_expr) #f]
    [(negate_expr) (ro-fold st 'negate_expr type lhs op2)]
    [(bit_not_expr) (if (ir-undefined? lhs) #f (ro-fold st 'bit_not_expr type lhs op2))]
    [(nop_expr convert_expr) (cast-op1 st t lhs op2)]
    [(lt_expr le_expr gt_expr ge_expr eq_expr ne_expr) (compare-op1 t code lhs op2)]
    [(bit_and_expr) (and-op1 st t lhs op2)]
    [(bit_ior_expr) (ior-op1 st t lhs op2)]
    [(bit_xor_expr) (xor-op1 st t lhs op2)]
    [(mult_expr)
     (cond
       [(or (ir-undefined? lhs) (ti-wraps t)) #f]
       [(and (ir-singleton op2) (not (= 0 (ir-singleton op2)))) #f]       ; trunc_div not ported
       [(not (ir-contains? lhs 0)) (ir-nonzero t)]
       [else #f])]
    [else #f]))

(define (ro-op2 st code type lhs op1)
  (define t (tinfo st type))
  (case code
    [(plus_expr) (ro-op1 st 'plus_expr type lhs op1)]
    [(minus_expr) (if (ir-undefined? lhs) #f (ro-fold st 'minus_expr type op1 lhs))]
    [(lt_expr le_expr gt_expr ge_expr eq_expr ne_expr) (compare-op1 t (swap-cmp code) lhs op1)]
    [(bit_and_expr) (and-op1 st t lhs op1)]
    [(bit_ior_expr) (ior-op1 st t lhs op1)]
    [(bit_xor_expr) (xor-op1 st t lhs op1)]
    [(mult_expr) (ro-op1 st 'mult_expr type lhs op1)]
    [else #f]))
