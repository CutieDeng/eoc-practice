#lang racket/base

;; ============================================================
;; opt: integer ranges (gcc/value-range.cc irange, irange_bitmask;
;; gcc/value-range-pretty-print.cc), GCC 15 semantics
;; ============================================================
;;
;; An irange is a sorted list of disjoint sub-ranges [lo, hi] of a
;; type plus an explicit bitmask (known bits).  Bounds are exact
;; integers in the type's signedness (a signed int is in
;; [-2^31, 2^31 - 1]); a bitmask is (mask . value) of bit patterns in
;; [0, 2^prec), mask = all ones meaning unknown.  The number of
;; sub-ranges is unbounded (int_range_max; HARD_MAX_RANGES = 255 is not
;; reached by anything the port computes).
;;
;; Every operation follows the GCC member function of the same name,
;; including its "changed?" result and the places that normalize the
;; kind, because which range a ranger query ends up with depends on
;; those details (set_range_from_bitmask after a bitmask change,
;; get_bitmask derived from the bounds on demand, ...).
;;
;;   (tinfo st type) -> ti        (prec, unsigned?, overflow wraps?)
;;   constructors: ir-undefined ir-varying ir-set ir-set-anti ir-zero
;;                 ir-nonzero ir-nonnegative ir-const
;;   queries: ir-undefined? ir-varying? ir-singleton ir-zero? ir-nonzero?
;;            ir-contains? ir-lower ir-upper ir-pairs ir-equal?
;;            ir-get-bitmask ir-nonzero-bits
;;   operations (each -> (values range changed?) unless noted):
;;            ir-union ir-intersect ir-intersect-bounds
;;            ir-invert (-> range) ir-update-bitmask (-> range)
;;            ir-set-nonzero-bits (-> range)
;;   (ir->string st r)  "[irange] int [0, +INF] MASK 0x.. VALUE 0x.."
;; ============================================================

(require racket/list
         racket/string
         "../state.rkt"
         "../tree.rkt"
         (only-in "../print.rkt" print-type)
         (only-in "fold.rkt" type-precision* type-unsigned?*))

(provide (struct-out ti) (struct-out irange) tinfo
         ti-min ti-max to-pattern from-pattern
         bm-unknown bm-unknown? bm-equal? bm-intersect bm-union bm-member?
         ir-undefined ir-varying ir-set ir-set-anti ir-zero ir-nonzero ir-nonnegative ir-const
         ir-undefined? ir-varying? ir-range? ir-singleton ir-zero? ir-nonzero? ir-contains?
         ir-lower ir-upper ir-num-pairs ir-equal?
         ir-get-bitmask ir-nonzero-bits ir-explicit-bitmask-unknown?
         ir-union ir-intersect ir-intersect-bounds ir-invert ir-update-bitmask ir-set-nonzero-bits
         ir-set-range-from-bitmask ir-normalize-kind ir-cast-type
         ir->string)

;; ------------------------------------------------------------
;; type information
;; ------------------------------------------------------------

;; prec, unsigned?, TYPE_OVERFLOW_WRAPS (unsigned; no -fwrapv)
(struct ti (type prec uns wraps) #:transparent)
(define (tinfo st type)
  (define uns (type-unsigned?* st type))
  ;; pointers: unsigned, but their overflow is undefined
  (ti type (type-precision* st type) uns (and uns (not (pointer-type-p st type)))))

(define (ti-min t) (if (ti-uns t) 0 (- (arithmetic-shift 1 (sub1 (ti-prec t))))))
(define (ti-max t) (if (ti-uns t) (sub1 (arithmetic-shift 1 (ti-prec t))) (sub1 (arithmetic-shift 1 (sub1 (ti-prec t))))))
(define (all-ones t) (sub1 (arithmetic-shift 1 (ti-prec t))))
;; a bound as a PREC-bit pattern and back
(define (to-pattern x prec) (bitwise-and x (sub1 (arithmetic-shift 1 prec))))
(define (from-pattern p t)
  (define x (to-pattern p (ti-prec t)))
  (if (and (not (ti-uns t)) (bitwise-bit-set? x (sub1 (ti-prec t)))) (- x (arithmetic-shift 1 (ti-prec t))) x))

;; ------------------------------------------------------------
;; irange_bitmask: (mask . value)
;; ------------------------------------------------------------

(define (bm-unknown t) (cons (all-ones t) 0))
(define (bm-unknown? t b) (= (car b) (all-ones t)))
(define (bm-equal? t a b)
  (define u1 (bm-unknown? t a)) (define u2 (bm-unknown? t b))
  (if (or u1 u2) (eq? u1 u2) (and (= (car a) (car b)) (= (cdr a) (cdr b)))))
(define (bm-intersect t a b)
  (define m1 (car a)) (define v1 (cdr a)) (define m2 (car b)) (define v2 (cdr b))
  (if (not (zero? (bitwise-and (to-pattern (bitwise-not (bitwise-ior m1 m2)) (ti-prec t)) (bitwise-xor v1 v2))))
      (bm-unknown t)
      (cons (bitwise-and m1 m2) (bitwise-ior v1 v2))))
(define (bm-union t a b)
  (cons (bitwise-ior (car a) (car b) (bitwise-xor (cdr a) (cdr b))) (bitwise-and (cdr a) (cdr b))))
(define (bm-member? t b val)
  (cond
    [(bm-unknown? t b) #t]
    [else
     (define v (to-pattern val (ti-prec t)))
     (define res0 (bitwise-and (car b) v))
     (define res (if (zero? (cdr b)) res0 (bitwise-ior res0 (bitwise-and (to-pattern (bitwise-not (car b)) (ti-prec t)) (cdr b)))))
     (= res v)]))

;; get_bitmask_from_range
(define (bitmask-from-range t lo hi)
  (define mn (to-pattern lo (ti-prec t))) (define mx (to-pattern hi (ti-prec t)))
  (cond
    [(= mn mx) (cons 0 mn)]
    [else
     (define xorv (sub1 (arithmetic-shift 1 (integer-length (bitwise-xor mn mx)))))
     (cons (bitwise-ior mn xorv) 0)]))

;; ------------------------------------------------------------
;; the range value
;; ------------------------------------------------------------

;; kind: 'undefined 'varying 'range; pairs: list of (lo . hi); bm: explicit bitmask
(struct irange (ti kind pairs bm) #:transparent)

(define (ir-undefined t) (irange t 'undefined '() (bm-unknown t)))
(define (ir-varying t) (irange t 'varying (list (cons (ti-min t) (ti-max t))) (bm-unknown t)))
;; irange::set (VR_RANGE)
(define (ir-set t lo hi)
  (if (and (= lo (ti-min t)) (= hi (ti-max t)))
      (ir-varying t)
      (irange t 'range (list (cons lo hi)) (bm-unknown t))))
;; irange::set (VR_ANTI_RANGE)
(define (ir-set-anti t lo hi)
  (define left (and (not (= lo (ti-min t))) (cons (ti-min t) (sub1 lo))))
  (define right (and (not (= hi (ti-max t))) (cons (add1 hi) (ti-max t))))
  (define ps (filter values (list left right)))
  (if (null? ps) (ir-undefined t) (irange t 'range ps (bm-unknown t))))
(define (ir-zero t) (ir-set t 0 0))
(define (ir-const t v) (ir-set t v v))
;; irange::set_nonzero: unsigned [1, MAX] with the kind set directly
(define (ir-nonzero t)
  (if (ti-uns t) (irange t 'range (list (cons 1 (ti-max t))) (bm-unknown t)) (ir-set-anti t 0 0)))
(define (ir-nonnegative t) (ir-set t 0 (ti-max t)))
;; the same bounds in another type of the same signedness / precision
(define (ir-cast-type r t) (irange t (irange-kind r) (irange-pairs r) (irange-bm r)))

(define (ir-undefined? r) (eq? (irange-kind r) 'undefined))
(define (ir-varying? r) (eq? (irange-kind r) 'varying))
(define (ir-range? r) (eq? (irange-kind r) 'range))
(define (ir-num-pairs r) (if (ir-undefined? r) 0 (length (irange-pairs r))))
(define (ir-lower r) (car (first (irange-pairs r))))
(define (ir-upper r) (cdr (last (irange-pairs r))))
(define (ir-explicit-bitmask-unknown? r) (bm-unknown? (irange-ti r) (irange-bm r)))

;; singleton_p: the value or #f
(define (ir-singleton r)
  (and (= 1 (ir-num-pairs r)) (= (ir-lower r) (ir-upper r)) (ir-lower r)))
(define (ir-zero? r)
  (and (ir-range? r) (= 1 (ir-num-pairs r)) (= 0 (ir-lower r)) (= 0 (ir-upper r))))
(define (ir-nonzero? r)
  (and (not (ir-undefined? r)) (ir-equal? r (ir-set-anti (irange-ti r) 0 0))))

;; contains_p
(define (ir-contains? r cst)
  (define t (irange-ti r))
  (cond
    [(ir-undefined? r) #f]
    [(and (not (bm-unknown? t (irange-bm r))) (not (= cst 0))
          (zero? (bitwise-and (bm-nonzero-bits (irange-bm r)) (to-pattern cst (ti-prec t)))))
     #f]
    [else
     (let loop ([ps (irange-pairs r)])
       (cond [(null? ps) #f]
             [(< cst (car (car ps))) #f]
             [(<= cst (cdr (car ps))) #t]
             [else (loop (cdr ps))]))]))
(define (bm-nonzero-bits b) (bitwise-ior (car b) (cdr b)))

;; get_bitmask: the bounds' bitmask intersected with the explicit one
(define (ir-get-bitmask r)
  (define t (irange-ti r))
  (define b (bitmask-from-range t (ir-lower r) (ir-upper r)))
  (if (bm-unknown? t (irange-bm r)) b (bm-intersect t b (irange-bm r))))
(define (ir-nonzero-bits r) (bm-nonzero-bits (ir-get-bitmask r)))

;; operator==
(define (ir-equal? a b)
  (cond
    [(not (= (ir-num-pairs a) (ir-num-pairs b))) #f]
    [(= 0 (ir-num-pairs a)) #t]
    [(not (equal? (irange-pairs a) (irange-pairs b))) #f]
    [else
     (define t (irange-ti a))
     (define b1 (ir-get-bitmask a)) (define b2 (ir-get-bitmask b))
     (and (= (car b1) (car b2)) (or (bm-unknown? t b1) (= (cdr b1) (cdr b2))))]))

;; varying_compatible_p
(define (varying-compatible? r)
  (define t (irange-ti r))
  (cond
    [(not (= 1 (ir-num-pairs r))) #f]
    [(ir-varying? r) #t]
    [else (and (= (ir-lower r) (ti-min t)) (= (ir-upper r) (ti-max t)) (bm-unknown? t (irange-bm r)))]))

;; normalize_kind
(define (ir-normalize-kind r)
  (cond
    [(= 0 (length (irange-pairs r))) (ir-undefined (irange-ti r))]
    [(and (varying-compatible? r) (ir-range? r)) (irange (irange-ti r) 'varying (irange-pairs r) (irange-bm r))]
    [else r]))

(define (with-pairs r ps) (irange (irange-ti r) 'range ps (irange-bm r)))
(define (with-bm r b) (irange (irange-ti r) (irange-kind r) (irange-pairs r) b))

;; ------------------------------------------------------------
;; bitmask updates
;; ------------------------------------------------------------

;; set_range_from_bitmask -> (values r changed?)
(define (ir-set-range-from-bitmask r)
  (define t (irange-ti r))
  (define b (irange-bm r))
  (define mask (car b)) (define value (cdr b))
  (cond
    [(bm-unknown? t b) (values r #f)]
    [(= mask 0) (values (ir-set t (from-pattern value t) (from-pattern value t)) #t)]
    [else
     (define nzb (bm-nonzero-bits b))
     (define pc (let loop ([x nzb] [n 0]) (if (zero? x) n (loop (arithmetic-shift x -1) (+ n (bitwise-and x 1))))))
     (cond
       [(= pc 1)
        (define nz (from-pattern nzb t))
        (cond
          [(not (ir-contains? r nz)) (values r #f)]
          [else
           (define has-zero (ir-contains? r 0))
           (define r1 (with-bm (ir-set t nz nz) (cons nzb 0)))
           (define-values (r2 _c) (if has-zero (ir-union r1 (ir-zero t)) (values r1 #f)))
           (values r2 #t)])]
       [(= pc 0) (values (ir-zero t) #t)]
       [else
        (define z (let loop ([m mask] [n 0]) (if (odd? m) n (loop (arithmetic-shift m -1) (add1 n)))))
        (cond
          [(= z 0) (values r #f)]
          [else
           (define ub (sub1 (arithmetic-shift 1 z)))
           (define v (bitwise-and value ub))
           (define-values (mr _c1) (ir-intersect (ir-set t 0 ub) (ir-set-anti t v v)))
           (define mr2 (ir-invert mr))
           (define-values (r2 _c2) (ir-intersect r mr2))
           (values r2 #t)])])]))

;; update_bitmask
(define (ir-update-bitmask r b)
  (define t (irange-ti r))
  (cond
    [(bm-equal? t (irange-bm r) b) r]
    [else
     (define r1 (irange t (if (and (ir-varying? r) (not (bm-unknown? t b))) 'range (irange-kind r)) (irange-pairs r) b))
     (define-values (r2 ok?) (ir-set-range-from-bitmask r1))
     (if ok? r2 (ir-normalize-kind r1))]))

;; vrange::set_nonzero_bits
(define (ir-set-nonzero-bits r bits)
  (ir-update-bitmask r (cons (to-pattern bits (ti-prec (irange-ti r))) 0)))

;; intersect_bitmask -> (values r changed?)
(define (intersect-bitmask r o)
  (define t (irange-ti r))
  (cond
    [(or (bm-unknown? t (irange-bm o)) (bm-equal? t (irange-bm r) (irange-bm o))) (values r #f)]
    [else
     (define save (ir-get-bitmask r))
     (define b (bm-intersect t save (ir-get-bitmask o)))
     (cond
       [(bm-equal? t save b) (values r #f)]
       [else
        (define r1 (with-bm r b))
        (cond
          [(bm-equal? t save (ir-get-bitmask r1)) (values r1 #f)]
          [else
           (define-values (r2 ok?) (ir-set-range-from-bitmask r1))
           (values (if ok? r2 (ir-normalize-kind r1)) #t)])])]))

;; union_bitmask -> (values r changed?)
(define (union-bitmask r o)
  (define t (irange-ti r))
  (cond
    [(bm-equal? t (irange-bm r) (irange-bm o)) (values r #f)]
    [else
     (define save (ir-get-bitmask r))
     (define b (bm-union t save (ir-get-bitmask o)))
     (cond
       [(and (bm-equal? t save b) (or (not (bm-unknown? t b)) (bm-unknown? t (irange-bm r)))) (values r #f)]
       [else
        (define r1 (with-bm r b))
        (if (bm-equal? t save (ir-get-bitmask r1))
            (values r1 #f)
            (values (ir-normalize-kind r1) #t))])]))

;; ------------------------------------------------------------
;; union
;; ------------------------------------------------------------

(define (ir-union a b)
  (define t (irange-ti a))
  (cond
    [(ir-undefined? b) (values a #f)]
    [(ir-undefined? a) (values b #t)]
    [(ir-varying? a) (values a #f)]
    [(ir-varying? b) (values (ir-varying t) #t)]
    [(and (= 1 (ir-num-pairs a)) (= 1 (ir-num-pairs b))) (single-pair-union a b)]
    [(and (ir-range? a) (> (ir-lower b) (ir-upper a))) (union-append a b)]
    [(irange-contains? a b) (union-bitmask a b)]
    [else
     ;; sort the pairs of both by lower bound, then merge overlapping / touching ones
     (define merged
       (let loop ([x (irange-pairs a)] [y (irange-pairs b)] [acc '()])
         (cond
           [(and (null? x) (null? y)) (reverse acc)]
           [(null? y) (loop (cdr x) y (cons (car x) acc))]
           [(null? x) (loop x (cdr y) (cons (car y) acc))]
           [(<= (car (car x)) (car (car y))) (loop (cdr x) y (cons (car x) acc))]
           [else (loop x (cdr y) (cons (car y) acc))])))
     (define norm
       (let loop ([ps (cdr merged)] [cur (car merged)] [acc '()])
         (cond
           [(null? ps) (reverse (cons cur acc))]
           [(>= (add1 (cdr cur)) (car (car ps)))
            (loop (cdr ps) (cons (car cur) (max (cdr cur) (cdr (car ps)))) acc)]
           [else (loop (cdr ps) (car ps) (cons cur acc))])))
     (define r1 (with-pairs a norm))
     (define-values (r2 bc) (union-bitmask r1 b))
     (values (if bc r2 (ir-normalize-kind r2)) #t)]))

(define (single-pair-union a b)
  (define lo1 (ir-lower a)) (define hi1 (ir-upper a)) (define lo2 (ir-lower b)) (define hi2 (ir-upper b))
  (cond
    [(<= lo1 lo2)
     (cond
       [(<= hi2 hi1) (union-bitmask a b)]
       [else
        (define ps (if (>= (add1 hi1) lo2) (list (cons lo1 hi2)) (list (cons lo1 hi1) (cons lo2 hi2))))
        (define r1 (with-pairs a ps))
        (define-values (r2 bc) (union-bitmask r1 b))
        (values (if bc r2 (ir-normalize-kind r2)) #t)])]
    [else
     (define ps
       (cond
         [(>= hi2 hi1) (list (cons lo2 hi2))]
         [(>= (add1 hi2) lo1) (list (cons lo2 hi1))]
         [else (list (cons lo2 hi2) (cons lo1 hi1))]))
     (define r1 (with-pairs a ps))
     (define-values (r2 bc) (union-bitmask r1 b))
     (values (if bc r2 (ir-normalize-kind r2)) #t)]))

(define (union-append a b)
  (define pa (irange-pairs a)) (define pb (irange-pairs b))
  (define ps
    (if (= (add1 (ir-upper a)) (ir-lower b))
        (append (drop-right pa 1) (list (cons (car (last pa)) (cdr (car pb)))) (cdr pb))
        (append pa pb)))
  (define r1 (with-pairs a ps))
  (define-values (r2 bc) (union-bitmask r1 b))
  (values (if bc r2 (ir-normalize-kind r2)) #t))

;; irange_contains_p: every pair of B inside some pair of A
(define (irange-contains? a b)
  (for/and ([pb (in-list (irange-pairs b))])
    (for/or ([pa (in-list (irange-pairs a))]) (and (>= (car pb) (car pa)) (<= (cdr pb) (cdr pa))))))

;; ------------------------------------------------------------
;; intersect
;; ------------------------------------------------------------

;; irange::intersect (lb, ub): only the pairs; (values r changed?)
(define (ir-intersect-bounds r lb ub)
  (cond
    [(ir-undefined? r) (values r #f)]
    [(and (>= (ir-lower r) lb) (<= (ir-upper r) ub)) (values r #f)]
    [else
     (define ps
       (let loop ([ps (irange-pairs r)] [acc '()])
         (cond
           [(null? ps) (reverse acc)]
           [(< ub (car (car ps))) (reverse acc)]
           [(< (cdr (car ps)) lb) (loop (cdr ps) acc)]
           [else
            (define l (max lb (car (car ps))))
            (if (< ub (cdr (car ps)))
                (reverse (cons (cons l ub) acc))
                (loop (cdr ps) (cons (cons l (cdr (car ps))) acc)))])))
     (if (null? ps)
         (values (ir-undefined (irange-ti r)) #t)
         (values (irange (irange-ti r) 'range ps (irange-bm r)) #t))]))

(define (ir-intersect a b)
  (cond
    [(ir-undefined? a) (values a #f)]
    [(ir-undefined? b) (values (ir-undefined (irange-ti a)) #t)]
    [(ir-varying? b) (values a #f)]
    [(ir-varying? a) (values (ir-cast-type b (irange-ti a)) #t)]
    [(= 1 (ir-num-pairs b))
     (define-values (r1 c1) (ir-intersect-bounds a (ir-lower b) (ir-upper b)))
     (cond
       [(ir-undefined? r1) (values r1 #t)]
       [else
        (define-values (r2 c2) (intersect-bitmask r1 b))
        (define res (or c1 c2))
        (values (if res (ir-normalize-kind r2) r2) res)])]
    [(irange-contains? b a) (intersect-bitmask a b)]
    [else
     ;; walk both pair lists
     (define pa (list->vector (irange-pairs a))) (define pb (list->vector (irange-pairs b)))
     (define out
       (let loop ([i 0] [i2 0] [acc '()])
         (cond
           [(or (>= i (vector-length pb)) (>= i2 (vector-length pa))) (reverse acc)]
           [else
            (define ru (cdr (vector-ref pb i))) (define rl (car (vector-ref pb i)))
            (define r2l (car (vector-ref pa i2))) (define r2u (cdr (vector-ref pa i2)))
            (cond
              [(< ru r2l) (loop (add1 i) i2 acc)]
              [(< r2u rl) (loop i (add1 i2) acc)]
              [else
               (define l (if (>= rl r2l) rl r2l))
               (if (<= ru r2u)
                   (loop (add1 i) i2 (cons (cons l ru) acc))
                   (loop i (add1 i2) (cons (cons l r2u) acc)))])])))
     (cond
       [(null? out) (values (ir-undefined (irange-ti a)) #t)]
       [else
        (define r1 (irange (irange-ti a) 'range out (irange-bm a)))
        (define-values (r2 bc) (intersect-bitmask r1 b))
        (values (if bc r2 (ir-normalize-kind r2)) #t)])]))

;; ------------------------------------------------------------
;; invert (the result has an unknown bitmask)
;; ------------------------------------------------------------

(define (ir-invert r)
  (define t (irange-ti r))
  (define ps (irange-pairs r))
  (define left (if (= (ti-min t) (ir-lower r)) '() (list (cons (ti-min t) (sub1 (ir-lower r))))))
  (define middle
    (for/list ([p (in-list ps)] [q (in-list (if (null? ps) '() (cdr ps)))])
      (cons (add1 (cdr p)) (sub1 (car q)))))
  (define right (if (= (ti-max t) (ir-upper r)) '() (list (cons (add1 (ir-upper r)) (ti-max t)))))
  (irange t 'range (append left middle right) (bm-unknown t)))

;; ------------------------------------------------------------
;; printing (vrange_printer::visit)
;; ------------------------------------------------------------

(define (ir->string st r)
  (define t (irange-ti r))
  (cond
    [(ir-undefined? r) "[irange] UNDEFINED"]
    [else
     (define head (format "[irange] ~a " (print-type st (ti-type t))))
     (cond
       [(ir-varying? r) (string-append head "VARYING")]
       [else
        (define (bound x)
          (cond
            [(and (not (ti-uns t)) (= x (ti-min t)) (not (= 1 (ti-prec t)))) "-INF"]
            [(and (= x (ti-max t)) (not (= 1 (ti-prec t)))) "+INF"]
            [else (number->string x)]))
        (define b (irange-bm r))
        (string-append head
                       (apply string-append (for/list ([p (in-list (irange-pairs r))]) (format "[~a, ~a]" (bound (car p)) (bound (cdr p)))))
                       (if (bm-unknown? t b) ""
                           (format " MASK 0x~a VALUE 0x~a" (number->string (car b) 16) (number->string (cdr b) 16))))])]))
