#lang racket/base

;; ============================================================
;; opt: expmed.cc cost tables (init_expmed) and the constant-multiply
;; search (synth_mult, choose_mult_variant, mult_by_coeff_cost)
;; ============================================================
;;
;; Cost tables (init_expmed_one_mode / init_expmed_one_conv): set_src_cost
;; (aarch64-rtx-costs.rkt) of
;;   add       (plus R R)            neg     (neg R)        mul (mult R R)
;;   shift m   (ashift R (const_int m)), 0 for m = 0
;;   shiftadd m   (plus (mult R 2^m) R)     add_cost for m = 0
;;   shiftsub0 m  (minus (mult R 2^m) R)    add_cost for m = 0
;;   shiftsub1 m  (minus R (mult R 2^m))    add_cost for m = 0
;;   convert to <- from: (zero_extend:to (reg:from)), or (truncate ...)
;;     when TO is narrower
;;   zero      (const_int 0) in QImode
;; Each is a pure function of (gcc major, speed, mode, m) and memoized.
;;
;; synth_mult caches its results in alg_hash (1031 entries indexed by
;; (t ^ mode ^ speed * 256) % 1031, a colliding write replaces the
;; entry); a cached entry changes the later searches (an alg_impossible
;; entry prunes, an algorithm entry restricts the search to its kind).
;; The cache is explicit state here: an intmap index -> entry, passed in
;; and returned (same input, same output).  GCC clears it once per
;; compilation (init_expmed) and the RTL expansion of the functions
;; compiled before (expand_mult and friends) also writes it; those
;; writes are not modelled.
;;
;; The mode numbers in the hash index are the machine_mode enum values
;; of the aarch64 builds (insn-modes.h of the installed plugin headers).
;; ============================================================

(require racket/intmap
         "aarch64-rtx-costs.rkt")

(provide add-cost neg-cost mul-cost shift-cost shiftadd-cost shiftsub0-cost shiftsub1-cost convert-cost zero-cost
         alg-hash-empty synth-mult choose-mult-variant mult-by-coeff-cost
         (struct-out mult-cost) (struct-out algorithm))

(define M64 (sub1 (arithmetic-shift 1 64)))
(define (u64 v) (bitwise-and v M64))
(define (s64 v) (let ([u (u64 v)]) (if (>= u (arithmetic-shift 1 63)) (- u (arithmetic-shift 1 64)) u)))
(define (mode-bits m) (* 8 (rtl-mode-size m)))
(define (mode-mask m) (sub1 (arithmetic-shift 1 (mode-bits m))))

;; ------------------------------------------------------------
;; init_expmed
;; ------------------------------------------------------------

(define memo (make-hash))
(define-syntax-rule (memoized key body) (hash-ref! memo key (lambda () body)))

(define (R m) (list 'reg m))
;; all->pow2[m]: GEN_INT (HOST_WIDE_INT_1 << m), a signed HOST_WIDE_INT
(define (pow2 m) (list 'const_int (s64 (arithmetic-shift 1 m))))
(define (check-m who mode m)
  (unless (< -1 m (min 64 (mode-bits mode))) (error who "shift count ~a out of range for ~a" m mode)))

(define (add-cost major speed mode)
  (memoized (list 'add major speed mode) (set-src-cost major `(plus ,mode ,(R mode) ,(R mode)) mode speed)))
(define (neg-cost major speed mode)
  (memoized (list 'neg major speed mode) (set-src-cost major `(neg ,mode ,(R mode)) mode speed)))
(define (mul-cost major speed mode)
  (memoized (list 'mul major speed mode) (set-src-cost major `(mult ,mode ,(R mode) ,(R mode)) mode speed)))
(define (shift-cost major speed mode m)
  (check-m 'shift-cost mode m)
  (if (zero? m)
      0
      (memoized (list 'shift major speed mode m)
                (set-src-cost major `(ashift ,mode ,(R mode) (const_int ,m)) mode speed))))
(define (shiftadd-cost major speed mode m)
  (check-m 'shiftadd-cost mode m)
  (if (zero? m)
      (add-cost major speed mode)
      (memoized (list 'shiftadd major speed mode m)
                (set-src-cost major `(plus ,mode (mult ,mode ,(R mode) ,(pow2 m)) ,(R mode)) mode speed))))
(define (shiftsub0-cost major speed mode m)
  (check-m 'shiftsub0-cost mode m)
  (if (zero? m)
      (add-cost major speed mode)
      (memoized (list 'shiftsub0 major speed mode m)
                (set-src-cost major `(minus ,mode (mult ,mode ,(R mode) ,(pow2 m)) ,(R mode)) mode speed))))
(define (shiftsub1-cost major speed mode m)
  (check-m 'shiftsub1-cost mode m)
  (if (zero? m)
      (add-cost major speed mode)
      (memoized (list 'shiftsub1 major speed mode m)
                (set-src-cost major `(minus ,mode ,(R mode) (mult ,mode ,(R mode) ,(pow2 m))) mode speed))))
;; init_expmed_one_conv: zero- and sign-extension are assumed to cost the same
(define (convert-cost major to from speed)
  (memoized (list 'convert major speed to from)
            (set-src-cost major
                          (if (< (mode-bits to) (mode-bits from)) `(truncate ,to ,(R from)) `(zero_extend ,to ,(R from)))
                          to speed)))
(define (zero-cost major speed)
  (memoized (list 'zero major speed) (set-src-cost major '(const_int 0) "QI" speed)))

;; ------------------------------------------------------------
;; synth_mult
;; ------------------------------------------------------------

;; struct mult_cost
(struct mult-cost (cost latency) #:transparent)
;; struct algorithm: COST and the operations, the last first, each
;; (cons alg-code log)
(struct algorithm (cost ops) #:transparent)
;; struct alg_hash_entry
(struct entry (t mode speed alg cost) #:transparent)

(define NUM-ALG-HASH-ENTRIES 1031)
(define alg-hash-empty intmap-empty)

;; CHEAPER_MULT_COST (X, Y), MULT_COST_LESS (X, Y-integer)
(define (cheaper? x y)
  (or (< (mult-cost-cost x) (mult-cost-cost y))
      (and (= (mult-cost-cost x) (mult-cost-cost y)) (< (mult-cost-latency x) (mult-cost-latency y)))))
(define (mult-cost-less? x y)
  (or (< (mult-cost-cost x) y) (and (= (mult-cost-cost x) y) (< (mult-cost-latency x) y))))

;; enum machine_mode of the aarch64 builds
(define (mode-number major m)
  (define base (case major [(12) 13] [(15) 14] [else (error 'expmed "machine_mode numbers of gcc ~a not transcribed" major)]))
  (+ base (case m [("QI") 0] [("HI") 1] [("SI") 2] [("DI") 3] [else (error 'expmed "mode ~a" m)])))

(define (ctz v) (if (zero? v) 64 (let loop ([v v] [n 0]) (if (odd? v) n (loop (arithmetic-shift v -1) (add1 n))))))
(define (floor-log2 v) (sub1 (integer-length v)))
(define (exact-log2-u v) (if (and (> v 0) (= v (arithmetic-shift 1 (floor-log2 v)))) (floor-log2 v) -1))

;; (synth-mult major H T LIMIT MODE SPEED) -> (values algorithm H):
;; the cheapest algorithm found for multiplying by the unsigned
;; HOST_WIDE_INT T within LIMIT; its cost is LIMIT + 1 when none is
(define (synth-mult major H orig-t limit mode speed)
  (define fail (algorithm (mult-cost (add1 (mult-cost-cost limit)) (add1 (mult-cost-latency limit))) #f))
  (define maxm (min 64 (mode-bits mode)))
  (define t (bitwise-and orig-t (mode-mask mode)))
  (cond
    [(or (< (mult-cost-cost limit) 0) (and (= (mult-cost-cost limit) 0) (<= (mult-cost-latency limit) 0)))
     (values fail H)]
    ;; t == 1 costs nothing
    [(= t 1) (values (algorithm (mult-cost 0 0) (list (cons 'alg_m 0))) H)]
    [(= t 0)
     (define z (zero-cost major speed))
     (if (mult-cost-less? limit z)
         (values fail H)
         (values (algorithm (mult-cost z z) (list (cons 'alg_zero 0))) H))]
    [else
     (define hash-index (modulo (bitwise-xor t (mode-number major mode) (if speed 256 0)) NUM-ALG-HASH-ENTRIES))
     (define e (intmap-ref H hash-index #f))
     (define hit (and e (= (entry-t e) t) (equal? (entry-mode e) mode) (eq? (entry-speed e) speed) e))
     (cond
       ;; impossible within a limit at least as restrictive
       [(and hit (eq? (entry-alg hit) 'alg_impossible) (not (cheaper? (entry-cost hit) limit)))
        (values fail H)]
       ;; the cached algorithm costs more than LIMIT
       [(and hit (not (eq? (entry-alg hit) 'alg_impossible)) (cheaper? limit (entry-cost hit)))
        (values fail H)]
       [else
        (define cache-alg (and hit (not (eq? (entry-alg hit) 'alg_impossible)) (entry-alg hit)))
        (search major H t orig-t limit mode speed maxm hash-index cache-alg fail)])]))

;; the body of synth_mult after the cache lookup.  CACHE-ALG: #f, or the
;; cached algorithm's kind, which alone is tried (cache_hit)
(define (search major H0 t orig-t limit mode speed maxm hash-index cache-alg fail)
  (define (add) (add-cost major speed mode))
  (define (shift m) (shift-cost major speed mode m))
  ;; the search state: (vector best-cost best-ops H); best-ops #f until found
  (define (try st q op-cost op-latency op log #:min-latency? [min-latency? #f])
    (define best (vector-ref st 0))
    (define-values (a H1)
      (synth-mult major (vector-ref st 2) q
                  (mult-cost (- (mult-cost-cost best) op-cost) (- (mult-cost-latency best) op-latency)) mode speed))
    (define c (+ (mult-cost-cost (algorithm-cost a)) op-cost))
    (define l0 (+ (mult-cost-latency (algorithm-cost a)) op-latency))
    (define l (if (and min-latency? (< l0 op-cost)) op-cost l0))
    (if (cheaper? (mult-cost c l) best)
        (vector (mult-cost c l) (cons (cons op log) (algorithm-ops a)) H1)
        (vector best (vector-ref st 1) H1)))
  (define (min-shift-cost m) (min (* m (add)) (shift m)))

  ;; a group of zero bits at the low end: multiply by the rest, then shift
  (define (do-alg-shift st)
    (define m (ctz t))
    (cond
      [(< m maxm)
       (define op-cost (min-shift-cost m))
       (define st1 (try st (arithmetic-shift t (- m)) op-cost op-cost 'alg_shift m))
       ;; ORIG_T as a signed number, when negative
       (if (>= orig-t (arithmetic-shift 1 63))
           (try st1 (u64 (bitwise-not (arithmetic-shift (u64 (bitwise-not orig-t)) (- m)))) op-cost op-cost 'alg_shift m)
           st1)]
      [else st]))

  ;; an odd number: add or subtract one
  (define (do-alg-addsub-t-m2 st)
    (define w (let loop ([w 1]) (cond [(> w M64) 0] [(zero? (bitwise-and w t)) w] [else (loop (arithmetic-shift w 1))])))
    (define st1
      (if (or (= w 0) (and (> w 2) (not (= t 3))))
          ;; T ends with ...111: multiply by T + 1 and subtract
          (try st (u64 (+ t 1)) (add) (add) 'alg_sub_t_m2 0)
          ;; T ends with ...01 or ...011: multiply by T - 1 and add
          (try st (u64 (- t 1)) (add) (add) 'alg_add_t_m2 0)))
    ;; a * -7, a * -15, ...: a - a * 2^m
    (define v (u64 (+ (- orig-t) 1)))
    (define m (exact-log2-u v))
    (cond
      [(and (>= m 0) (< m maxm))
       (define op-cost0 (+ (add) (shift m)))
       (define ss1 (shiftsub1-cost major speed mode m))
       (define-values (op-cost op-latency) (if (<= ss1 op-cost0) (values ss1 ss1) (values op-cost0 (add))))
       (try st1 (arithmetic-shift v (- m)) op-cost op-latency 'alg_sub_t_m2 m)]
      [else st1]))

  ;; factors t = q (2^m +- 1), the largest first; the first one found ends the search
  (define (do-alg-addsub-factor st)
    (let loop ([m (floor-log2 (- t 1))] [st st])
      (cond
        [(< m 2) st]
        [else
         (define d+ (+ (arithmetic-shift 1 m) 1))
         (define d- (- (arithmetic-shift 1 m) 1))
         (cond
           [(and (zero? (remainder t d+)) (> t d+) (< m maxm) (or (not cache-alg) (eq? cache-alg 'alg_add_factor)))
            (define op-cost (let ([c (+ (add) (shift m))] [sa (shiftadd-cost major speed mode m)]) (if (<= sa c) sa c)))
            (try st (quotient t d+) op-cost op-cost 'alg_add_factor m #:min-latency? #t)]
           [(and (zero? (remainder t d-)) (> t d-) (< m maxm) (or (not cache-alg) (eq? cache-alg 'alg_sub_factor)))
            (define op-cost (let ([c (+ (add) (shift m))] [ss (shiftsub0-cost major speed mode m)]) (if (<= ss c) ss c)))
            (try st (quotient t d-) op-cost op-cost 'alg_sub_factor m #:min-latency? #t)]
           [else (loop (sub1 m) st)])])))

  ;; shift-and-add: a*3, a*5, a*9
  (define (do-alg-add-t2-m st)
    (define q (- t 1))
    (define m (ctz q))
    (if (and (not (zero? q)) (< m maxm))
        (let ([op-cost (shiftadd-cost major speed mode m)])
          (try st (arithmetic-shift q (- m)) op-cost op-cost 'alg_add_t2_m m))
        st))
  (define (do-alg-sub-t2-m st)
    (define q (u64 (+ t 1)))
    (define m (ctz q))
    (if (and (not (zero? q)) (< m maxm))
        (let ([op-cost (shiftsub0-cost major speed mode m)])
          (try st (arithmetic-shift q (- m)) op-cost op-cost 'alg_sub_t2_m m))
        st))

  (define st0 (vector limit #f H0))
  (define st
    (case cache-alg
      [(alg_shift) (do-alg-shift st0)]
      [(alg_add_t_m2 alg_sub_t_m2) (do-alg-addsub-t-m2 st0)]
      [(alg_add_factor alg_sub_factor) (do-alg-addsub-factor st0)]
      [(alg_add_t2_m) (do-alg-add-t2-m st0)]
      [(alg_sub_t2_m) (do-alg-sub-t2-m st0)]
      [(#f)
       (let* ([st (if (even? t) (do-alg-shift st0) st0)]
              [st (if (odd? t) (do-alg-addsub-t-m2 st) st)]
              [st (do-alg-addsub-factor st)]
              [st (if (odd? t) (do-alg-add-t2-m st) st)]
              [st (if (odd? t) (do-alg-sub-t2-m st) st)])
         st)]
      [else (error 'synth-mult "cached algorithm ~a" cache-alg)]))

  ;; done
  (define best (vector-ref st 0))
  (define ops (vector-ref st 1))
  (define H (vector-ref st 2))
  (cond
    ;; nothing found: alg_impossible within LIMIT
    [(not (cheaper? best limit))
     (values fail (intmap-set H hash-index (entry t mode speed 'alg_impossible limit)))]
    [else
     (define H* (if cache-alg H (intmap-set H hash-index (entry t mode speed (car (car ops)) best))))
     ;; too long a sequence for struct algorithm
     (if (= (sub1 (length ops)) 64)
         (values fail H*)
         (values (algorithm best ops) H*))]))

;; choose_mult_variant: (values algorithm variant ok? H), VARIANT one of
;; 'basic 'negate 'add
(define (choose-mult-variant major H val mode mult-cost0 speed)
  (cond
    [(< mult-cost0 0) (values #f 'basic #f H)]
    [else
     ;; any constant multiplication takes less than 2 * bits additions
     (define mult-cost* (min mult-cost0 (* 2 (mode-bits mode) (add-cost major speed mode))))
     (define (limit-for alg op-cost)
       (if (mult-cost-less? (algorithm-cost alg) mult-cost*)
           (mult-cost (- (mult-cost-cost (algorithm-cost alg)) op-cost) (- (mult-cost-latency (algorithm-cost alg)) op-cost))
           (mult-cost (- mult-cost* op-cost) (- mult-cost* op-cost))))
     (define (plus-op a op-cost)
       (algorithm (mult-cost (+ (mult-cost-cost (algorithm-cost a)) op-cost) (+ (mult-cost-latency (algorithm-cost a)) op-cost))
                  (algorithm-ops a)))
     (define-values (alg1 H1) (synth-mult major H (u64 val) (mult-cost mult-cost* mult-cost*) mode speed))
     ;; the negated value fits an unsigned int (HOST_BITS_PER_INT >= the mode's bits)
     (define-values (alg2 var2 H2)
       (cond
         [(<= (mode-bits mode) 32)
          (define op-cost (neg-cost major speed mode))
          (define-values (a H*) (synth-mult major H1 (u64 (- val)) (limit-for alg1 op-cost) mode speed))
          (define a* (plus-op a op-cost))
          (if (cheaper? (algorithm-cost a*) (algorithm-cost alg1)) (values a* 'negate H*) (values alg1 'basic H*))]
         [else (values alg1 'basic H1)]))
     ;; VAL - 1, then an addition (gcc >= 13 skips HOST_WIDE_INT_MIN in a narrower mode)
     (define-values (alg3 var3 H3)
       (cond
         [(or (< major 13) (not (= (s64 val) (- (arithmetic-shift 1 63)))) (= (mode-bits mode) 64))
          (define op-cost (add-cost major speed mode))
          (define-values (a H*) (synth-mult major H2 (u64 (- val 1)) (limit-for alg2 op-cost) mode speed))
          (define a* (plus-op a op-cost))
          (if (cheaper? (algorithm-cost a*) (algorithm-cost alg2)) (values a* 'add H*) (values alg2 var2 H*))]
         [else (values alg2 var2 H2)]))
     (values alg3 var3 (mult-cost-less? (algorithm-cost alg3) mult-cost*) H3)]))

;; mult_by_coeff_cost: (values cost H).  SPEED is the caller's (it prices
;; the plain multiply); SYNTH-SPEED is optimize_insn_for_speed_p (), which
;; the search uses
(define (mult-by-coeff-cost major H coeff mode speed synth-speed)
  (define max-cost (mul-cost major speed mode))
  (define-values (alg variant ok? H*) (choose-mult-variant major H coeff mode max-cost synth-speed))
  (values (if ok? (mult-cost-cost (algorithm-cost alg)) max-cost) H*))

;; ------------------------------------------------------------
;; against GCC: test/integration/expmed/expmed.<version>.rktd, written by
;; gccast-plugin/oracle/expmed.sh (the tables, mult_by_coeff_cost with a
;; cleared alg_hash, a sequence sharing alg_hash and its final entries)
;; ------------------------------------------------------------

(module+ test
  (require rackunit racket/file racket/list racket/runtime-path)
  (define-runtime-path oracle-dir "../../../../test/integration/expmed")
  (define algs '#(alg_unknown alg_zero alg_m alg_shift alg_add_t_m2 alg_sub_t_m2 alg_add_factor alg_sub_factor
                  alg_add_t2_m alg_sub_t2_m alg_impossible))
  (define (check-version ver)
    (define major (string->number (car (regexp-split #rx"[.]" ver))))
    (define data (file->list (build-path oracle-dir (format "expmed.~a.rktd" ver))))
    (define bad '())
    (define (check what ours theirs) (unless (equal? ours theirs) (set! bad (cons (list what ours theirs) bad))))
    (define H-final alg-hash-empty)
    (for ([d (in-list data)])
      (case (car d)
        [(modes) (for ([p (in-list (cdr d))]) (check (list 'mode (car p)) (mode-number major (car p)) (cadr p)))]
        [(zero) (check (list 'zero (cadr d)) (zero-cost major (= 1 (cadr d))) (caddr d))]
        [(table)
         (define speed (= 1 (list-ref d 1)))
         (define m (list-ref d 2))
         (for ([f (in-list (drop d 3))])
           (case (car f)
             [(add) (check (list 'add speed m) (add-cost major speed m) (cadr f))]
             [(neg) (check (list 'neg speed m) (neg-cost major speed m) (cadr f))]
             [(mul) (check (list 'mul speed m) (mul-cost major speed m) (cadr f))]
             [(shift shiftadd shiftsub0 shiftsub1)
              (define fn (case (car f)
                           [(shift) shift-cost] [(shiftadd) shiftadd-cost] [(shiftsub0) shiftsub0-cost] [(shiftsub1) shiftsub1-cost]))
              (for ([v (in-list (cdr f))] [i (in-naturals)]) (check (list (car f) speed m i) (fn major speed m i) v))]
             [(convert)
              (for ([p (in-list (cdr f))]) (check (list 'convert speed m (car p)) (convert-cost major m (car p) speed) (cadr p)))]))]
        [(coeff)
         (define speed (= 1 (list-ref d 1)))
         (define synth-speed (= 1 (list-ref d 2)))
         (define m (list-ref d 3))
         (for ([p (in-list (drop d 4))])
           (define-values (c H) (mult-by-coeff-cost major alg-hash-empty (car p) m speed synth-speed))
           (check (list 'coeff speed synth-speed m (car p)) c (cadr p)))]
        [(sequence)
         (set! H-final
               (for/fold ([H alg-hash-empty]) ([q (in-list (cdr d))])
                 (define-values (c H*) (mult-by-coeff-cost major H (list-ref q 0) (list-ref q 1) (= 1 (list-ref q 2)) (= 1 (list-ref q 3))))
                 (check (cons 'sequence q) c (list-ref q 4))
                 H*))]
        [(hash)
         (define theirs
           (for/list ([e (in-list (cdr d))])
             (list (list-ref e 0) (list-ref e 1) (list-ref e 2) (= 1 (list-ref e 3)) (vector-ref algs (list-ref e 4))
                   (list-ref e 5) (list-ref e 6))))
         (define ours
           (for*/list ([i (in-range NUM-ALG-HASH-ENTRIES)] [e (in-value (intmap-ref H-final i #f))] #:when e)
             (list i (entry-t e) (entry-mode e) (entry-speed e) (entry-alg e)
                   (mult-cost-cost (entry-cost e)) (mult-cost-latency (entry-cost e)))))
         (check 'alg-hash ours theirs)]))
    (check-equal? (take (reverse bad) (min 5 (length bad))) '() (format "gcc ~a" ver)))
  (check-version "12.5.0")
  (check-version "15.2.0"))
