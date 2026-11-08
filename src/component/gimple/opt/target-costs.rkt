#lang racket/base

;; ============================================================
;; opt: the target cost hooks the loop optimizers read (aarch64,
;; -mcpu=apple-m1, the default of the gcc 12.5.0 / 15.2.0 builds the
;; fixtures were dumped with)
;; ============================================================
;;
;; GCC computes these from RTL (expmed.cc init_expmed, rtlanal.cc
;; address_cost, the aarch64 rtx cost tables); the RTL layer is not
;; ported, so the values are the ones the compilers report.  Each was
;; measured with -fdump-tree-ivopts-details and --param avg-loop-niter=1
;; (which makes the setup costs visible undivided) on the fixtures:
;;
;;   add_cost (SI/DI)             4    candidate costs 9 = 4 + 4 + 1
;;   convert_cost (DI <- SI)      4    (sizetype) (int) (x + c): 13
;;   convert_cost (SI <- DI)      0    group 0 cost of the sizetype biv: 0
;;   mult_by_coeff_cost 1         0    (synth_mult: alg_m)
;;   mult_by_coeff_cost 12, -12   8 (gcc 15) / 12 (gcc 12)
;;   address_cost: aarch64_address_cost ported (below)
;;   force_expr_to_var_cost: integer 4, symbol 25 (speed) / 13 (size),
;;     address 5, target_spill_cost 16 (speed) / 4 (size),
;;     target_reg_cost 4 (speed), 29 available, 19 call-clobbered,
;;     3 reserved registers
;;
;; A cost that was not measured raises an error naming it, so that a new
;; fixture shows what to measure.
;;
;; Addresses (memory_address_addr_space_p, aarch64_classify_address):
;; base, base + index, base + index * size, base + imm (9-bit signed
;; unscaled or 12-bit unsigned scaled by the access size); nothing else
;; (no absolute addresses, no base + index + imm).
;; ============================================================

(require "../state.rkt"
         "generic-fold.rkt")

(provide (struct-out mem-parts)
         machine-mode-of mode-size mode-bits scalar-int-mode?
         add-cost shift-cost shiftadd-cost shiftsub0-cost shiftsub1-cost convert-cost mult-by-coeff-cost
         integer-cost symbol-cost address-const-cost target-spill-cost target-reg-cost
         target-avail-regs target-clobbered-regs target-res-regs
         memory-address-valid? address-cost preferred-mem-scale-factor use-autoinc?)

;; the parts of an address (struct mem_address): SYMBOL BASE INDEX are
;; present or not, STEP is #f or the index multiplier, OFFSET #f or an integer
(struct mem-parts (symbol base index step offset) #:transparent)

(define (machine-mode-of t)
  (cond
    [(ty-ptr? t) "DI"]
    [else (or (type-attr (gs) t 'mode #f) (case (ty-prec t) [(8) "QI"] [(16) "HI"] [(32) "SI"] [else "DI"]))]))
(define (mode-size m) (case m [("QI") 1] [("HI") 2] [("SI" "SF") 4] [("DI" "DF") 8] [("TI" "TF") 16] [else (error 'mode-size "~a" m)]))
(define (mode-bits m) (* 8 (mode-size m)))
(define (scalar-int-mode? m) (and (member m '("QI" "HI" "SI" "DI" "TI")) #t))

(define (major) (gcc-major-version (gs)))
(define (unmeasured what) (error 'target-costs "~a has not been measured for gcc ~a" what (major)))

(define (add-cost speed mode)
  (if (member mode '("SI" "DI")) 4 (unmeasured (format "add_cost (~a)" mode))))
(define (shift-cost speed mode m) (unmeasured (format "shift_cost (~a, ~a)" mode m)))
(define (shiftadd-cost speed mode m) (unmeasured (format "shiftadd_cost (~a, ~a)" mode m)))
(define (shiftsub0-cost speed mode m) (unmeasured (format "shiftsub0_cost (~a, ~a)" mode m)))
(define (shiftsub1-cost speed mode m) (unmeasured (format "shiftsub1_cost (~a, ~a)" mode m)))
(define (convert-cost to from speed)
  (cond
    [(equal? to from) 0]
    [(and (equal? to "DI") (equal? from "SI")) 4]
    [(and (equal? to "SI") (equal? from "DI")) 0]
    [else (unmeasured (format "convert_cost (~a <- ~a)" to from))]))
(define (mult-by-coeff-cost coef mode speed)
  (cond
    ;; synth_mult: multiplying by 1 is the operand itself
    [(= coef 1) 0]
    [(and (equal? mode "DI") (memv coef '(12 -12))) (if (>= (major) 13) 8 12)]
    [else (unmeasured (format "mult_by_coeff_cost (~a, ~a)" coef mode))]))

;; USE_{LOAD,STORE}_{PRE,POST}_{INCREMENT,DECREMENT} (MODE): aarch64.h
;; defines all eight as 0 in 12.5 and 15.2 ("Disable auto-increment in
;; move_by_pieces et al."), although HAVE_{PRE,POST}_{IN,DE}CREMENT are 1;
;; ivopts therefore adds no autoincrement candidate and
;; get_address_cost_ainc is infinite.  KIND: 'pre-inc 'pre-dec 'post-inc 'post-dec
(define (use-autoinc? kind mode) #f)

(define (integer-cost speed) 4)
(define (symbol-cost speed) (if speed 25 13))
(define (address-const-cost speed) 5)
(define (target-spill-cost speed) (if speed 16 4))
(define (target-reg-cost speed) (if speed 4 (unmeasured "target_reg_cost (size)")))
(define (target-avail-regs) 29)
(define (target-clobbered-regs) 19)
(define (target-res-regs) 3)

;; aarch64_classify_address for the shapes addr_for_mem_ref builds
(define (memory-address-valid? mode p)
  (define size (mode-size mode))
  (define (imm-ok? off)
    (or (<= -256 off 255)
        (and (zero? (remainder off size)) (<= 0 (quotient off size) 4095))))
  (define off (and (mem-parts-offset p) (not (zero? (mem-parts-offset p))) (mem-parts-offset p)))
  (define step (mem-parts-step p))
  (cond
    [(mem-parts-symbol p) #f]
    [(not (mem-parts-base p))
     ;; an index alone is a register; an offset or a scaled index alone is not an address
     (and (mem-parts-index p) (not step) (not off))]
    [(mem-parts-index p)
     (and (not off) (or (not step) (= step 1) (= step size)))]
    [off (imm-ok? off)]
    [else #t]))

;; address_cost (rtlanal.cc): 1000 for an address the target refuses,
;; else aarch64_address_cost (the same in 12.5 and 15.2): the
;; classify_address type's cost plus, for a scaled index, the
;; addr_scale_costs entry of the access size.  The tables of the two
;; tunings apple-m1 selects (12.5 darwin: cortexa73_tunings ->
;; generic_addrcost_table; 15.2: generic_armv8_a_addrcost_table) are equal.
(define addrcost-scale-hi 1) (define addrcost-scale-si 0) (define addrcost-scale-di 0) (define addrcost-scale-ti 1)
(define addrcost-register-offset 0) (define addrcost-imm-offset 0)
(define (address-cost mode p speed)
  (cond
    [(not (memory-address-valid? mode p)) 1000]
    ;; ADDRESS_REG_REG: base + index, shifted when the index is scaled
    [(and (mem-parts-base p) (mem-parts-index p))
     (define shifted? (and (mem-parts-step p) (not (= 1 (mem-parts-step p)))))
     (+ addrcost-register-offset
        (if shifted?
            (case (mode-bits mode) [(16) addrcost-scale-hi] [(32) addrcost-scale-si] [(64) addrcost-scale-di] [else addrcost-scale-ti])
            0))]
    ;; ADDRESS_REG_IMM: a register (an index alone is one) with an offset, possibly 0
    [else addrcost-imm-offset]))

;; preferred_mem_scale_factor: a scaled index is not cheaper
(define (preferred-mem-scale-factor mode) 1)
