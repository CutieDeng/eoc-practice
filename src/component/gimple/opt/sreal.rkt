#lang racket/base

;; ============================================================
;; opt: GCC's profile arithmetic, bit for bit
;; ============================================================
;;
;; sreal (gcc/sreal.{h,cc}): a 31-bit significand (normalized into
;; [2^29, 2^30) with rounding) and an exponent; the frequency
;; propagation of estimate_bb_frequencies is done in it and the dumped
;; counts depend on its rounding.
;;
;; profile_probability (gcc/profile-count.h): a 29-bit fixed point with
;; max_probability = 2^27; REG_BR_PROB_BASE (10000) based predictor hit
;; rates convert through from_reg_br_prob_base.  Here a probability is
;; its m_val (an exact integer) or #f for uninitialized; the quality
;; bits are not tracked (GUESSED everywhere in this pass).
;; ============================================================

(provide (struct-out sreal) make-sreal sreal-zero sreal-one
         sreal+ sreal- sreal* sreal/ sreal< sreal<= sreal= sreal>
         sreal-shift sreal->nearest-int sreal->int sreal->double sreal->exact
         REG-BR-PROB-BASE max-probability prob-from-reg-br-prob-base prob-to-reg-br-prob-base
         prob-invert prob-minus prob-div-int prob-always prob-never prob-very-unlikely prob-even
         prob->sreal rdiv)

;; ------------------------------------------------------------
;; sreal
;; ------------------------------------------------------------

(define PART-BITS 31)
(define MIN-SIG (arithmetic-shift 1 (- PART-BITS 2)))        ; 2^29
(define MAX-SIG (sub1 (arithmetic-shift 1 (- PART-BITS 1)))) ; 2^30 - 1
(define MAX-EXP (quotient 2147483647 4))

(struct sreal (sig exp) #:transparent)

(define (floor-log2 x) (sub1 (integer-length x)))

;; sreal::normalize
(define (make-sreal sig0 [exp 0])
  (define asig (abs sig0))
  (cond
    [(zero? sig0) (sreal 0 (- MAX-EXP))]
    [(< asig MIN-SIG)
     ;; normalize_up
     (define shift (- PART-BITS 2 (floor-log2 asig)))
     (define sig (arithmetic-shift asig shift))
     (define e (- exp shift))
     (if (< e (- MAX-EXP))
         (sreal 0 (- MAX-EXP))
         (sreal (if (< sig0 0) (- sig) sig) e))]
    [(> asig MAX-SIG)
     ;; normalize_down
     (define shift (+ (- (floor-log2 asig) PART-BITS) 2))
     (define last-bit (bitwise-and (arithmetic-shift asig (- (sub1 shift))) 1))
     (define sig1 (+ (arithmetic-shift asig (- shift)) last-bit))
     (define-values (sig e)
       (if (> sig1 MAX-SIG) (values (arithmetic-shift sig1 -1) (+ exp shift 1)) (values sig1 (+ exp shift))))
     (cond
       [(> e MAX-EXP) (sreal (if (< sig0 0) (- MAX-SIG) MAX-SIG) MAX-EXP)]
       [else (sreal (if (< sig0 0) (- sig) sig) e)])]
    [else (sreal sig0 exp)]))

(define sreal-zero (make-sreal 0))
(define sreal-one (make-sreal 1))

;; sreal::shift_right (rounds)
(define (shift-right r s)
  (sreal (arithmetic-shift (+ (sreal-sig r) (arithmetic-shift 1 (sub1 s))) (- s)) (+ (sreal-exp r) s)))

(define (sreal+ a b)
  (define-values (x y) (if (< (sreal-exp a) (sreal-exp b)) (values b a) (values a b)))
  (define dexp (- (sreal-exp x) (sreal-exp y)))
  (cond
    [(> dexp PART-BITS) x]
    [else
     (define yy (if (zero? dexp) y (shift-right y dexp)))
     (make-sreal (+ (sreal-sig x) (sreal-sig yy)) (sreal-exp x))]))

(define (sreal- a b)
  (define-values (x y sign) (if (< (sreal-exp a) (sreal-exp b)) (values b a -1) (values a b 1)))
  (define dexp (- (sreal-exp x) (sreal-exp y)))
  (cond
    [(> dexp PART-BITS) (sreal (* sign (sreal-sig x)) (sreal-exp x))]
    [else
     (define yy (if (zero? dexp) y (shift-right y dexp)))
     (make-sreal (* sign (- (sreal-sig x) (sreal-sig yy))) (sreal-exp x))]))

(define (sreal* a b)
  (if (or (< (abs (sreal-sig a)) MIN-SIG) (< (abs (sreal-sig b)) MIN-SIG))
      (sreal 0 (- MAX-EXP))
      (make-sreal (* (sreal-sig a) (sreal-sig b)) (+ (sreal-exp a) (sreal-exp b)))))

(define (sreal/ a b)
  (define sign (if (< (sreal-sig a) 0) -1 1))
  (make-sreal (* sign (quotient (arithmetic-shift (abs (sreal-sig a)) PART-BITS) (sreal-sig b)))
              (- (sreal-exp a) (sreal-exp b) PART-BITS)))

(define (sreal-shift r s) (if (zero? (sreal-sig r)) r (sreal (sreal-sig r) (+ (sreal-exp r) s))))

(define (sreal< a b)
  (cond
    [(= (sreal-exp a) (sreal-exp b)) (< (sreal-sig a) (sreal-sig b))]
    [else
     (define na (< (sreal-sig a) 0)) (define nb (< (sreal-sig b) 0))
     (cond
       [(not (eq? na nb)) (and na (not nb))]
       [else (define r (< (sreal-exp a) (sreal-exp b))) (if na (not r) r)])]))
(define (sreal= a b) (and (= (sreal-exp a) (sreal-exp b)) (= (sreal-sig a) (sreal-sig b))))
(define (sreal<= a b) (or (sreal< a b) (sreal= a b)))
(define (sreal> a b) (not (sreal<= a b)))

(define (sreal->int r)
  (define sign (if (< (sreal-sig r) 0) -1 1))
  (define a (abs (sreal-sig r)))
  (define e (sreal-exp r))
  (cond
    [(<= e (- PART-BITS)) 0]
    [(>= e PART-BITS) (* sign 9223372036854775807)]
    [(> e 0) (* sign (arithmetic-shift a e))]
    [(< e 0) (* sign (arithmetic-shift a e))]
    [else (sreal-sig r)]))

(define (sreal->nearest-int r)
  (define sign (if (< (sreal-sig r) 0) -1 1))
  (define a (abs (sreal-sig r)))
  (define e (sreal-exp r))
  (cond
    [(<= e (- PART-BITS)) 0]
    [(>= e PART-BITS) (* sign 9223372036854775807)]
    [(> e 0) (* sign (arithmetic-shift a e))]
    [(< e 0) (* sign (+ (arithmetic-shift a e) (bitwise-and (arithmetic-shift a (+ e 1)) 1)))]   ; (a >> -e) + ((a >> (-e - 1)) & 1)
    [else (sreal-sig r)]))

(define (sreal->exact r) (* (sreal-sig r) (expt 2 (sreal-exp r))))
(define (sreal->double r) (exact->inexact (sreal->exact r)))

;; ------------------------------------------------------------
;; profile_probability
;; ------------------------------------------------------------

(define REG-BR-PROB-BASE 10000)
(define max-probability (arithmetic-shift 1 27))
(define (rdiv x y) (quotient (+ x (quotient y 2)) y))

(define (prob-from-reg-br-prob-base v) (rdiv (* v max-probability) REG-BR-PROB-BASE))
(define (prob-to-reg-br-prob-base p) (rdiv (* p REG-BR-PROB-BASE) max-probability))
(define prob-always max-probability)
(define prob-never 0)
(define (prob-invert p) (and p (- prob-always p)))
(define (prob-minus a b) (and a b (if (>= a b) (- a b) 0)))
;; apply_scale (1, den) with safe_scale_64bit: RDIV
(define (prob-div-int p den) (and p (min (rdiv p den) max-probability)))
(define prob-very-unlikely (sub1 (prob-div-int prob-always 2000)))
(define prob-even (prob-div-int prob-always 2))
;; to_sreal: m_val >> 27
(define (prob->sreal p) (sreal-shift (make-sreal p) (- 27)))
