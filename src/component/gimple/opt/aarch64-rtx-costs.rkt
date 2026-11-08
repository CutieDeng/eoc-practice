#lang racket/base

;; ============================================================
;; opt: the aarch64 rtx costs of the RTL shapes expmed.cc builds its
;; cost tables from (rtlanal.cc rtx_cost / set_src_cost;
;; config/aarch64/aarch64.cc aarch64_rtx_costs, aarch64_rtx_mult_cost,
;; aarch64_cheap_mult_shift_p, aarch64_strip_extend,
;; aarch64_rtx_arith_op_extract_p, aarch64_extend_bitfield_pattern_p)
;; ============================================================
;;
;; (set-src-cost major x mode speed) = rtx_cost (x, mode, SET, 1, speed)
;;
;; Tuning: both Homebrew builds default to -mcpu=apple-m1.
;;   gcc 12.5.0: the darwin patch (gcc-12.4.0.diff) declares apple-m1
;;     with cortexa73_tunings: insn_extra_cost = cortexa57_extra_costs,
;;     extra_tuning_flags = AARCH64_EXTRA_TUNE_NONE.
;;   gcc 15.2.0: aarch64-cores.def declares apple-m1 with
;;     generic_armv8_a_tunings: cortexa76_extra_costs,
;;     AARCH64_EXTRA_TUNE_BASE (which includes CHEAP_SHIFT_EXTEND).
;; Only the scalar integer ALU and MULT rows of the cost tables are
;; transcribed (aarch-cost-tables.h; the same text in 12.5 and 15.2).
;;
;; The aarch64_rtx_costs cases ported are the scalar integer paths of
;; CONST_INT (0 only), NEG, MINUS, PLUS, ZERO_EXTEND, the shifts and
;; MULT; between 12.5 and 15.2 they differ only in PLUS with a
;; constant operand (aarch64_pluslong_immediate, 15), which is not
;; ported.  Anything else raises an error naming it.
;;
;; RTL: (reg MODE), (const_int N), (CODE MODE OP ...), CODE one of
;; plus minus neg mult ashift ashiftrt lshiftrt zero_extend
;; sign_extend truncate, MODE one of "QI" "HI" "SI" "DI".
;; ============================================================

(provide set-src-cost rtx-cost costs-n-insns rtl-mode-size rtl-scalar-int-mode?)

(define (costs-n-insns n) (* n 4))
(define (unported what . args)
  (error 'aarch64-rtx-costs "not ported: ~a" (apply format what args)))

;; ------------------------------------------------------------
;; the tuning (struct cpu_cost_table, scalar integer rows)
;; ------------------------------------------------------------

(struct alu-costs (arith logical shift shift-reg arith-shift arith-shift-reg log-shift log-shift-reg
                         extend extend-arith bfi bfx))
(struct mult-costs (simple flag-setting extend add extend-add idiv))
(struct cost-table (alu mult-si mult-di))
(struct tuning (name extra-cost cheap-shift-extend?))

(define N costs-n-insns)
(define cortexa57-extra-costs
  (cost-table (alu-costs 0 0 0 0 (N 1) (N 1) (N 1) (N 1) 0 (N 1) (N 1) 0)
              (mult-costs (N 2) (N 3) (N 2) (N 2) (N 2) (N 18))
              (mult-costs (N 4) 0 (N 2) (N 4) (N 2) (N 34))))
(define cortexa76-extra-costs
  (cost-table (alu-costs 0 0 0 0 (N 1) (N 1) 0 (N 1) 0 (N 1) (N 1) 0)
              (mult-costs (N 1) (N 2) (N 1) (N 1) (N 1) (N 6))
              (mult-costs (N 3) 0 (N 1) (N 3) (N 1) (N 10))))

(define cortexa73-tunings (tuning "cortexa73" cortexa57-extra-costs #f))
(define generic-armv8-a-tunings (tuning "generic_armv8_a" cortexa76-extra-costs #t))
(define (tuning-for major)
  (case major
    [(12) cortexa73-tunings]
    [(15) generic-armv8-a-tunings]
    [else (error 'aarch64-rtx-costs "the apple-m1 tuning of gcc ~a is not transcribed" major)]))

;; ------------------------------------------------------------
;; RTL accessors
;; ------------------------------------------------------------

(define (rtl-mode-size m) (case m [("QI") 1] [("HI") 2] [("SI") 4] [("DI") 8] [(#f) 0] [else (unported "mode ~a" m)]))
(define (rtl-scalar-int-mode? m) (and (member m '("QI" "HI" "SI" "DI")) #t))
(define (code x) (car x))
;; GET_MODE: #f for VOIDmode (a const_int)
(define (mode x) (if (eq? (car x) 'const_int) #f (cadr x)))
(define (xexp x i) (list-ref x (+ 2 i)))
(define (operands x) (case (car x) [(reg const_int) '()] [else (cddr x)]))
(define (const-int? x) (eq? (car x) 'const_int))
(define (intval x) (cadr x))
(define (reg? x) (eq? (car x) 'reg))
(define (shift-code? c) (memq c '(ashift ashiftrt lshiftrt)))   ; aarch64_shift_p
(define (extend-code? c) (memq c '(zero_extend sign_extend)))
;; exact_log2 takes an unsigned HOST_WIDE_INT: INTVAL HOST_WIDE_INT_MIN is 2^63
(define (exact-log2 v0)
  (define v (bitwise-and v0 (sub1 (arithmetic-shift 1 64))))
  (if (and (> v 0) (= v (arithmetic-shift 1 (sub1 (integer-length v))))) (sub1 (integer-length v)) -1))

;; ------------------------------------------------------------
;; rtx_cost
;; ------------------------------------------------------------

(define (set-src-cost major x m speed) (rtx-cost (tuning-for major) x m 'set 1 speed))

(define (rtx-cost t x mode0 outer opno speed)
  (define c (code x))
  (define m (or (mode x) mode0))
  ;; the operands' costs plus TOTAL
  (define (sum-operands total)
    (for/fold ([total total]) ([op (in-list (operands x))] [i (in-naturals)])
      (+ total (rtx-cost t op m c i speed))))
  (case c
    [(reg) 0]
    ;; modes_tieable_p (12.5, 15.2): scalar integer modes tie
    [(truncate)
     (if (and (rtl-scalar-int-mode? m) (rtl-scalar-int-mode? (mode (xexp x 0))))
         (sum-operands 0)
         (unported "truncate ~a <- ~a" m (mode (xexp x 0))))]
    [else
     (define-values (done? total) (aarch64-rtx-costs t x m outer opno speed))
     (if done? total (sum-operands total))]))

;; aarch64_rtx_costs: (values handled? cost)
(define (aarch64-rtx-costs t x m outer param speed)
  (define alu (cost-table-alu (tuning-extra-cost t)))
  (define base (costs-n-insns 1))
  (case (code x)
    [(const_int)
     ;; 0 is XZR/WZR; anything else costs aarch64_internal_mov_immediate
     (if (zero? (intval x)) (values #t 0) (unported "rtx_cost of (const_int ~a)" (intval x)))]
    [(neg)
     (unless (rtl-scalar-int-mode? m) (unported "neg in ~a" m))
     (when (memq (code (xexp x 0)) '(eq ne lt le gt ge ltu leu gtu geu)) (unported "CSETM"))
     ;; SUB wzr, X
     (cost-minus t 'neg '(const_int 0) (xexp x 0) m speed)]
    [(minus) (cost-minus t 'minus (xexp x 0) (xexp x 1) m speed)]
    [(plus) (cost-plus t (xexp x 0) (xexp x 1) m speed)]
    [(zero_extend)
     (define op0 (xexp x 0))
     (cond
       ;; a write of a W register zeroes the upper half
       [(and (equal? m "DI") (equal? (mode op0) "SI"))
        (define op-cost (rtx-cost t op0 #f 'zero_extend 0 speed))
        (values #t (if (zero? op-cost) base op-cost))]
       [(eq? (code op0) 'mem) (unported "zero_extend of mem")]
       [(extend-bitfield-pattern x)
        => (lambda (op)
             (values #t (+ base (rtx-cost t op m 'zero_extend 0 speed) (if speed (alu-costs-bfx alu) 0))))]
       ;; an AND instead of UXTB/UXTH
       [else (values #f (+ base (if speed (alu-costs-logical alu) 0)))])]
    [(ashift ashiftrt lshiftrt)
     (unless (rtl-scalar-int-mode? m) (unported "shift in ~a" m))
     (define op0 (xexp x 0))
     (define op1 (xexp x 1))
     (cond
       [(const-int? op1)
        ;; LSL (immediate), ASR (immediate), UBFM, UBFIZ; extends are free
        (define op0* (if (extend-code? (code op0)) (xexp op0 0) op0))
        (values #t (+ base (if speed (alu-costs-shift alu) 0) (rtx-cost t op0* #f 'ashift 0 speed)))]
       [(memq (code op1) '(subreg and)) (unported "shift by ~a" (code op1))]
       ;; LSLV, ASRV
       [else (values #f (+ base (if speed (alu-costs-shift-reg alu) 0)))])]
    [(mult)
     (values #t (+ base (aarch64-rtx-mult-cost t x 'mult #f speed)))]
    [else (unported "aarch64_rtx_costs of ~a" (code x))]))

;; the MINUS case from cost_minus; CODE is NEG or MINUS
(define (cost-minus t code0 op0 op1 m speed)
  (define alu (cost-table-alu (tuning-extra-cost t)))
  (unless (rtl-scalar-int-mode? m) (unported "minus in ~a" m))
  (define c0 (+ (costs-n-insns 1) (rtx-cost t op0 m 'minus 0 speed)))
  (cond
    [(const-int? op1) (unported "SUB (immediate)")]
    ;; SUB (extended register)
    [(arith-op-extract? op1)
     (define op1* (strip-extend op1 #t))
     (values #t (+ c0 (if speed (alu-costs-extend-arith alu) 0) (rtx-cost t op1* #f (code op1) 0 speed)))]
    [else
     (define new-op1 (strip-extend op1 #f))
     (cond
       ;; an FMA-alike operation
       [(or (eq? (code new-op1) 'mult) (shift-code? (code new-op1)))
        (values #t (+ c0 (aarch64-rtx-mult-cost t new-op1 'mult code0 speed)))]
       [else
        (values #t (+ c0 (rtx-cost t new-op1 #f 'minus 1 speed) (if speed (alu-costs-arith alu) 0)))])]))

(define (cost-plus t op0 op1 m speed)
  (define alu (cost-table-alu (tuning-extra-cost t)))
  (unless (rtl-scalar-int-mode? m) (unported "plus in ~a" m))
  (cond
    [(memq (code op0) '(eq ne lt le gt ge ltu leu gtu geu)) (unported "CSINC")]
    [(const-int? op1) (unported "ADD (immediate)")]
    [else
     (define c1 (+ (costs-n-insns 1) (rtx-cost t op1 m 'plus 1 speed)))
     (cond
       ;; ADD (extended register)
       [(arith-op-extract? op0)
        (define op0* (strip-extend op0 #t))
        (values #t (+ c1 (if speed (alu-costs-extend-arith alu) 0) (rtx-cost t op0* #f (code op0*) 0 speed)))]
       [else
        ;; strip any extend, leave shifts behind for mult_cost
        (define new-op0 (strip-extend op0 #f))
        (cond
          [(or (eq? (code new-op0) 'mult) (shift-code? (code new-op0)))
           (values #t (+ c1 (aarch64-rtx-mult-cost t new-op0 'mult 'plus speed)))]
          [else
           (values #t (+ c1 (rtx-cost t new-op0 #f 'plus 0 speed) (if speed (alu-costs-arith alu) 0)))])])]))

;; aarch64_rtx_mult_cost (X, CODE = MULT, OUTER, SPEED), scalar integer modes
(define (aarch64-rtx-mult-cost t x code0 outer speed)
  (define ec (tuning-extra-cost t))
  (define alu (cost-table-alu ec))
  (define m (mode x))
  (unless (rtl-scalar-int-mode? m) (unported "mult cost in ~a" m))
  (define compound? (and (memq outer '(plus minus)) #t))
  (define op0 (xexp x 0))
  (define op1 (xexp x 1))
  (cond
    ;; the multiply is canonicalized as a shift
    [(or (shift-code? (code x)) (and (const-int? op1) (> (exact-log2 (intval op1)) 0)))
     (define extend? (and (extend-code? (code op0)) #t))
     (define c (cond
                 [(not speed) 0]
                 [compound?
                  (cond
                    [(cheap-mult-shift? t x) 0]
                    ;; ARITH + shift-by-register
                    [(reg? op1) (alu-costs-arith-shift-reg alu)]
                    ;; ARITH + extended register
                    [extend? (alu-costs-extend-arith alu)]
                    ;; ARITH + shift-by-immediate
                    [else (alu-costs-arith-shift alu)])]
                 ;; LSL (immediate)
                 [else (alu-costs-shift alu)]))
     (+ c (rtx-cost t (if extend? (strip-extend op0 #t) op0) #f code0 0 speed))]
    [else
     ;; MNEG: a compound MSUB
     (define neg? (eq? (code op0) 'neg))
     (define op0* (if neg? (xexp op0 0) op0))
     (define compound*? (or compound? neg?))
     (cond
       ;; [SU]MULL, [SU]MADDL
       [(or (and (eq? (code op0*) 'zero_extend) (eq? (code op1) 'zero_extend))
            (and (eq? (code op0*) 'sign_extend) (eq? (code op1) 'sign_extend)))
        (define row (cost-table-mult-si ec))
        (+ (rtx-cost t (xexp op0* 0) #f 'mult 0 speed) (rtx-cost t (xexp op1 0) #f 'mult 1 speed)
           (if speed (if compound*? (mult-costs-extend-add row) (mult-costs-extend row)) 0))]
       ;; MUL, MADD, MSUB
       [else
        (define row (if (equal? m "DI") (cost-table-mult-di ec) (cost-table-mult-si ec)))
        (+ (rtx-cost t op0* m 'mult 0 speed) (rtx-cost t op1 m 'mult 1 speed)
           (if speed (if compound*? (mult-costs-add row) (mult-costs-simple row)) 0))])]))

;; aarch64_cheap_mult_shift_p
(define (cheap-mult-shift? t x)
  (define op0 (xexp x 0))
  (define op1 (xexp x 1))
  (cond
    [(not (tuning-cheap-shift-extend? t)) #f]
    [(eq? (code op0) 'sign_extend) #f]
    [(and (eq? (code x) 'ashift) (const-int? op1) (<= 0 (intval op1) 4)) #t]
    [(not (and (eq? (code x) 'mult) (const-int? op1))) #f]
    [else (let ([l2 (exact-log2 (intval op1))]) (and (> l2 0) (<= l2 4)))]))

;; aarch64_rtx_arith_op_extract_p
(define (arith-op-extract? x) (and (extend-code? (code x)) (reg? (xexp x 0))))

;; aarch64_strip_extend
(define (strip-extend x strip-shift?)
  (cond
    [(not (rtl-scalar-int-mode? (mode x))) x]
    [(eq? (code x) 'and) (unported "aarch64_strip_extend of and")]
    [else
     (define op (if (and strip-shift? (eq? (code x) 'ashift) (const-int? (xexp x 1)) (<= 0 (intval (xexp x 1)) 4))
                    (xexp x 0)
                    x))
     (if (extend-code? (code op)) (xexp op 0) op)]))

;; aarch64_extend_bitfield_pattern_p: the operand or #f
(define (extend-bitfield-pattern x)
  (define outer-code (code x))
  (define outer-mode (mode x))
  (cond
    [(and (not (extend-code? outer-code)) (not (member outer-mode '("SI" "DI")))) #f]
    [else
     (define inner (xexp x 0))
     (define small? (member (mode inner) '("QI" "HI")))
     (case (code inner)
       [(ashift) (and (const-int? (xexp inner 1)) small? (xexp inner 0))]
       [(lshiftrt) (and (eq? outer-code 'zero_extend) (const-int? (xexp inner 1)) small? (xexp inner 0))]
       [(ashiftrt) (and (eq? outer-code 'sign_extend) (const-int? (xexp inner 1)) small? (xexp inner 0))]
       [else #f])]))
