#lang racket/base

;; ============================================================
;; opt: widest_int arithmetic on exact integers and the known-bits
;; transfer functions of tree-ssa-ccp.cc (bit_value_unop /
;; bit_value_binop), shared by CCP and range-op's update_known_bitmask
;; ============================================================
;;
;;   (bv-unop code uns prec rval rmask runs rprec) -> (values val mask)
;;   (bv-binop code uns width r1val r1mask r1uns r1prec r2val r2mask r2uns r2prec)
;;     -> (values val mask)
;;   values and masks are exact integers (a -1 mask is "all bits unknown")
;; ============================================================

(provide BIG zext sext ext bit-and-not ctz popcount clz-in clrsb-in scmp ucmp wcmp ltu? neg?
         mask-low mask-high lrotate rrotate exact-log2 udiv-trunc
         value-mask-to-min-max bv-unop bv-binop)

;; ------------------------------------------------------------
;; widest_int arithmetic on exact integers
;; ------------------------------------------------------------

(define BIG 100000)
(define (zext x prec) (bitwise-and x (sub1 (arithmetic-shift 1 prec))))
(define (sext x prec)
  (define m (zext x prec))
  (if (bitwise-bit-set? m (sub1 prec)) (- m (arithmetic-shift 1 prec)) m))
(define (ext x prec unsigned?) (if unsigned? (zext x prec) (sext x prec)))
(define (bit-and-not a b) (bitwise-and a (bitwise-not b)))
(define (ctz x) (if (zero? x) BIG (let loop ([x x] [n 0]) (if (odd? x) n (loop (arithmetic-shift x -1) (add1 n))))))
(define (popcount x) (if (< x 0) BIG (let loop ([x x] [n 0]) (if (zero? x) n (loop (arithmetic-shift x -1) (+ n (bitwise-and x 1)))))))
;; wi::clz / wi::clrsb normalized to WIDTH (as the C code does with get_precision)
(define (clz-in x width) (max 0 (- width (integer-length x))))
(define (clrsb-in x width) (max 0 (- width 1 (integer-length x))))
;; unsigned comparison of widest_ints: negative numbers are the large ones
(define (scmp a b) (cond [(< a b) -1] [(> a b) 1] [else 0]))
(define (ucmp a b) (cond [(and (< a 0) (>= b 0)) 1] [(and (>= a 0) (< b 0)) -1] [else (scmp a b)]))
(define (wcmp a b unsigned?) (if unsigned? (ucmp a b) (scmp a b)))
(define (ltu? a b) (< (ucmp a b) 0))
(define (neg? x unsigned?) (and (not unsigned?) (< x 0)))
(define (mask-low n) (sub1 (arithmetic-shift 1 n)))          ; wi::mask (n, false)
(define (mask-high n) (bitwise-not (mask-low n)))            ; wi::mask (n, true)
(define (lrotate x n width)
  (define ux (zext x width))
  (zext (bitwise-ior (arithmetic-shift ux n) (arithmetic-shift ux (- n width))) width))
(define (rrotate x n width) (lrotate x (- width n) width))
(define (exact-log2 x) (if (and (> x 0) (= x (arithmetic-shift 1 (sub1 (integer-length x))))) (sub1 (integer-length x)) -1))
(define (udiv-trunc a b) (quotient a b))

;; ------------------------------------------------------------
;; bit_value_unop / bit_value_binop
;; ------------------------------------------------------------

;; value_mask_to_min_max -> (values min max)
(define (value-mask-to-min-max val mask unsigned? prec)
  (define mn (bit-and-not val mask))
  (define mx (bitwise-ior val mask))
  (cond
    [(and (not unsigned?) (< mask 0))
     (define sign-bit (arithmetic-shift 1 (sub1 prec)))
     (values (sext (bitwise-xor mn sign-bit) prec) (sext (bitwise-xor mx sign-bit) prec))]
    [else (values mn mx)]))

;; -> (values val mask)
(define (bv-unop code uns prec rval rmask runs rprec)
  (case code
    [(bit_not_expr) (values (bitwise-not rval) rmask)]
    [(negate_expr)
     (define-values (tv tm) (bv-unop 'bit_not_expr uns prec rval rmask uns prec))
     (bv-binop 'plus_expr uns prec tv tm uns prec 1 0 uns prec)]
    [(nop_expr convert_expr)
     (define m (ext rmask rprec runs)) (define v (ext rval rprec runs))
     (values (ext v prec uns) (ext m prec uns))]
    [(abs_expr absu_expr)
     (cond
       [(= (sext rmask rprec) -1) (values 0 -1)]
       [(< rmask 0)
        (define-values (tv tm) (bv-unop 'negate_expr runs rprec rval rmask uns prec))
        (define tm* (bitwise-ior tm rmask (bitwise-xor rval tv)))
        (values (ext tv prec uns) (ext tm* prec uns))]
       [(< rval 0) (bv-unop 'negate_expr uns prec rval rmask uns prec)]
       [else (values rval rmask)])]
    [else (values 0 -1)]))

;; bit_value_mult_const -> (values val mask)
(define (bv-mult-const uns width rval rmask c0)
  (define rval-lo (bit-and-not rval rmask))
  (cond
    [(not (zero? rval-lo))
     (let loop ([c c0] [sum-val 0] [sum-mask 0])
       (cond
         [(zero? c) (values (ext sum-val width uns) (ext sum-mask width uns))]
         [else
          (define bitpos (ctz c))
          (define term-mask (arithmetic-shift rmask bitpos))
          (define term-val (arithmetic-shift rval-lo bitpos))
          (define lo (+ sum-val term-val))
          (define hi (+ (bitwise-ior sum-val sum-mask) (bitwise-ior term-val term-mask)))
          (loop (bitwise-xor c (arithmetic-shift 1 bitpos)) lo
                (bitwise-ior sum-mask term-mask (bitwise-xor lo hi)))]))]
    [else
     (let loop ([c c0] [sum-mask 0])
       (cond
         [(zero? c) (values 0 (ext sum-mask width uns))]
         [else
          (define bitpos (ctz c))
          (define term-mask (arithmetic-shift rmask bitpos))
          (define hi (+ sum-mask term-mask))
          (loop (bitwise-xor c (arithmetic-shift 1 bitpos)) (bitwise-ior sum-mask term-mask hi))]))]))

;; get_individual_bits: up to MAX single-bit values of X, lowest first
(define (individual-bits x max)
  (let loop ([x x] [acc '()] [n 0])
    (if (or (>= n max) (zero? x))
        (reverse acc)
        (let ([b (arithmetic-shift 1 (ctz x))]) (loop (bitwise-xor x b) (cons b acc) (add1 n))))))

(define gray-code-bit-flips
  #(0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 4 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 5
    0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 4 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0))

;; bit_value_binop (the widest_int level) -> (values val mask)
(define (bv-binop code0 uns width r1val r1mask r1uns r1prec r2val r2mask r2uns r2prec)
  (define swap? (memq code0 '(ge_expr gt_expr)))
  (define code (case code0 [(ge_expr) 'le_expr] [(gt_expr) 'lt_expr] [else code0]))
  (define (rshift x n) (arithmetic-shift x (- n)))
  (define (lshift x n) (arithmetic-shift x n))
  (case code
    [(bit_and_expr)
     (values (bitwise-and r1val r2val)
             (bitwise-and (bitwise-ior r1mask r2mask) (bitwise-ior r1val r1mask) (bitwise-ior r2val r2mask)))]
    [(bit_ior_expr)
     (values (bitwise-ior r1val r2val)
             (bit-and-not (bitwise-ior r1mask r2mask)
                          (bitwise-ior (bit-and-not r1val r1mask) (bit-and-not r2val r2mask))))]
    [(bit_xor_expr) (values (bitwise-xor r1val r2val) (bitwise-ior r1mask r2mask))]
    [(lrotate_expr rrotate_expr)
     (cond
       [(zero? r2mask)
        (cond
          [(zero? r2val) (values r1val r1mask)]
          [else
           (define-values (shift code*)
             (if (neg? r2val r2uns)
                 (values (- r2val) (if (eq? code 'rrotate_expr) 'lrotate_expr 'rrotate_expr))
                 (values r2val code)))
           (define rot (if (eq? code* 'rrotate_expr) rrotate lrotate))
           (values (ext (rot r1val shift width) width uns) (ext (rot r1mask shift width) width uns))])]
       [(and (ltu? (bitwise-ior r2val r2mask) width) (<= (popcount r2mask) 4))
        (define rot (if (eq? code 'rrotate_expr) rrotate lrotate))
        (define bits (list->vector (individual-bits r2mask 4)))
        (define count (sub1 (arithmetic-shift 1 (vector-length bits))))
        (define shift0 (bit-and-not r2val r2mask))
        (let loop ([i 0] [shift shift0] [res-val (rot r1val shift0 width)] [res-mask (rot r1mask shift0 width)])
          (cond
            [(>= i count) (values (ext (bit-and-not res-val res-mask) width uns) (ext res-mask width uns))]
            [else
             (define shift* (bitwise-xor shift (vector-ref bits (vector-ref gray-code-bit-flips i))))
             (define tv (rot r1val shift* width)) (define tm (rot r1mask shift* width))
             (loop (add1 i) shift* res-val (bitwise-ior res-mask tm (bitwise-xor res-val tv)))]))]
       [else (values 0 -1)])]
    [(lshift_expr rshift_expr)
     (cond
       [(zero? r2mask)
        (cond
          [(zero? r2val) (values r1val r1mask)]
          [(neg? r2val r2uns) (values 0 -1)]
          [(eq? code 'rshift_expr)
           (values (rshift (ext r1val width uns) r2val) (rshift (ext r1mask width uns) r2val))]
          [else (values (ext (lshift r1val r2val) width uns) (ext (lshift r1mask r2val) width uns))])]
       [(ltu? (bitwise-ior r2val r2mask) width)
        (cond
          [(<= (popcount r2mask) 4)
           (define bits (list->vector (individual-bits r2mask 4)))
           (define count (sub1 (arithmetic-shift 1 (vector-length bits))))
           (define shift0 (bit-and-not r2val r2mask))
           (define-values (arg-val arg-mask sh)
             (if (eq? code 'rshift_expr)
                 (values (ext r1val width uns) (ext r1mask width uns) rshift)
                 (values r1val r1mask lshift)))
           (let loop ([i 0] [shift shift0] [res-val (sh arg-val shift0)] [res-mask (sh arg-mask shift0)])
             (cond
               [(>= i count)
                (define rm (ext res-mask width uns)) (define rv (ext res-val width uns))
                (values (bit-and-not rv rm) rm)]
               [else
                (define shift* (bitwise-xor shift (vector-ref bits (vector-ref gray-code-bit-flips i))))
                (define tv (sh arg-val shift*)) (define tm (sh arg-mask shift*))
                (loop (add1 i) shift* res-val (bitwise-ior res-mask tm (bitwise-xor res-val tv)))]))]
          [(zero? (bitwise-ior r1val r1mask)) (values 0 0)]
          [(eq? code 'lshift_expr)
           (define tmp (lshift (lshift (mask-low width) (ctz (bitwise-ior r1val r1mask))) (bit-and-not r2val r2mask)))
           (values 0 (ext tmp width uns))]
          [(not (neg? (bitwise-ior r1val r1mask) uns))
           (define arg (bitwise-ior r1val r1mask))
           (define lz (clz-in (zext arg width) width))
           (define tmp (rshift (rshift (mask-low width) lz) (bit-and-not r2val r2mask)))
           (values 0 (ext tmp width uns))]
          [(not (< r1mask 0))
           (define arg (bit-and-not r1val r1mask))
           (define sb (clrsb-in (sext arg width) width))
           (define tmp (rshift (rshift (mask-low width) sb) (bit-and-not r2val r2mask)))
           (values (sext (bitwise-not tmp) width) (sext tmp width))]
          [else (values 0 -1)])]
       [else (values 0 -1)])]
    [(plus_expr pointer_plus_expr)
     (define lo (ext (+ (bit-and-not r1val r1mask) (bit-and-not r2val r2mask)) width uns))
     (define hi (ext (+ (bitwise-ior r1val r1mask) (bitwise-ior r2val r2mask)) width uns))
     (values lo (ext (bitwise-ior r1mask r2mask (bitwise-xor lo hi)) width uns))]
    [(minus_expr pointer_diff_expr)
     (define lo (ext (- (bit-and-not r1val r1mask) (bitwise-ior r2val r2mask)) width uns))
     (define hi (ext (- (bitwise-ior r1val r1mask) (bit-and-not r2val r2mask)) width uns))
     (values lo (ext (bitwise-ior r1mask r2mask (bitwise-xor lo hi)) width uns))]
    [(mult_expr)
     (cond
       [(and (zero? r2mask) (not (neg? r2val uns)) (< (popcount r2val) 8))
        (bv-mult-const uns width r1val r1mask r2val)]
       [(and (zero? r1mask) (not (neg? r1val uns)) (< (popcount r1val) 8))
        (bv-mult-const uns width r2val r2mask r1val)]
       [else
        (define r1tz (ctz (bitwise-ior r1val r1mask)))
        (define r2tz (ctz (bitwise-ior r2val r2mask)))
        (cond
          [(>= (+ r1tz r2tz) width) (values 0 0)]
          [(> (+ r1tz r2tz) 0) (values 0 (ext (mask-high (+ r1tz r2tz)) width uns))]
          [else (values 0 -1)])])]
    [(eq_expr ne_expr)
     (define m (bitwise-ior r1mask r2mask))
     (if (not (= (bit-and-not r1val m) (bit-and-not r2val m)))
         (values (if (eq? code 'eq_expr) 0 1) 0)
         (values 0 1))]
    [(lt_expr le_expr)
     (define-values (o1val o1mask o2val o2mask)
       (if swap? (values r2val r2mask r1val r1mask) (values r1val r1mask r2val r2mask)))
     (define-values (min1 max1) (value-mask-to-min-max o1val o1mask r1uns r1prec))
     (define-values (min2 max2) (value-mask-to-min-max o2val o2mask r1uns r1prec))
     (define maxmin (wcmp max1 min2 r1uns))
     (define minmax (wcmp min1 max2 r1uns))
     (cond
       [(< maxmin (if (eq? code 'le_expr) 1 0)) (values 1 0)]
       [(> minmax (if (eq? code 'lt_expr) -1 0)) (values 0 0)]
       [(= maxmin minmax) (values (if (eq? code 'le_expr) 1 0) 0)]
       [else (values 0 1)])]
    [(min_expr max_expr)
     (define-values (min1 max1) (value-mask-to-min-max r1val r1mask uns width))
     (define-values (min2 max2) (value-mask-to-min-max r2val r2mask uns width))
     (cond
       [(<= (wcmp max1 min2 uns) 0) (if (eq? code 'min_expr) (values r1val r1mask) (values r2val r2mask))]
       [(>= (wcmp min1 max2 uns) 0) (if (eq? code 'min_expr) (values r2val r2mask) (values r1val r1mask))]
       [else (values r1val (bitwise-ior r1mask r2mask (bitwise-xor r1val r2val)))])]
    [(trunc_mod_expr)
     (define r1max (bitwise-ior r1val r1mask))
     (define r2max (bitwise-ior r2val r2mask))
     (define shift (if (zero? r2mask) (exact-log2 r2val) -1))
     (define-values (tv tm)
       (if (not (= shift -1))
           (bv-binop 'bit_and_expr uns width r1val r1mask r1uns r1prec (sub1 r2val) r2mask r2uns r2prec)
           (values #f #f)))
     (cond
       [(and tv (or uns (not (< r1max 0)) (and (zero? tm) (zero? tv)))) (values tv tm)]
       [(or uns (and (not (< r1max 0)) (not (< r2max 0))))
        (define r2min (bit-and-not r2val r2mask))
        (cond
          [(zero? r2min) (values 0 -1)]
          [(ltu? r1max r2min) (values r1val r1mask)]
          [else
           (define bits0 (integer-length (zext r2max width)))
           (define bits (if (= r2max (arithmetic-shift 1 bits0)) (sub1 bits0) bits0))
           (values 0 (mask-low bits))])]
       [else (values 0 -1)])]
    [(exact_div_expr trunc_div_expr)
     (define r1max (bitwise-ior r1val r1mask))
     (define r2max (bitwise-ior r2val r2mask))
     (define shift
       (if (and (zero? r2mask) (or (eq? code 'exact_div_expr) uns (not (< r1max 0)))) (exact-log2 r2val) -1))
     (cond
       [(not (= shift -1))
        (bv-binop 'rshift_expr uns width r1val r1mask r1uns r1prec shift r2mask r2uns r2prec)]
       [(or uns (and (not (< r1max 0)) (not (< r2max 0))))
        (define r2min (bit-and-not r2val r2mask))
        (cond
          [(zero? r2min) (values 0 -1)]
          [(ltu? r1max r2min) (values 0 0)]
          [else
           (define upper (udiv-trunc (zext r1max width) r2min))
           (values 0 (mask-low (integer-length upper)))])]
       [else (values 0 -1)])]
    [else (values 0 -1)]))
