#lang racket/base

;; AArch64 Assembly Syntax Validator
;;
;; Validates parsed IR for syntactic correctness.
;; Separate from type-check.rkt which handles type compatibility.

(require racket/match
         "../../../../cutie-ftree/pvector.rkt"
         "../../../../cutie-ftree/ordered-map.rkt"
         "../ir/types.rkt"
         "../ir/cfg.rkt"
         "../ir/config.rkt"
         "../ir/sve-insns.rkt")

(provide
 ;; Main validation entry points
 validate-function
 validate-cfg
 validate-block
 validate-insn

 ;; Validation result
 ValidationResult ValidationResult?
 ValidationResult-ok? ValidationResult-errors
 validation-ok
 validation-error
 validation-errors

 ;; Individual validators
 valid-x-register?
 valid-w-register?
 valid-z-register?
 valid-p-register?
 valid-param-reg?
 valid-sve-instruction?)

;; ============================================================================
;; Validation Result
;; ============================================================================

(struct ValidationResult (ok? errors) #:prefab)

(define (validation-ok)
  (ValidationResult #t '()))

(define (validation-error msg ctx)
  (ValidationResult #f (list (cons msg ctx))))

(define (validation-errors errs)
  (if (null? errs)
      (validation-ok)
      (ValidationResult #f errs)))

;; Combine results
(define (combine-results . results)
  (define all-errors
    (apply append (map ValidationResult-errors results)))
  (validation-errors all-errors))

;; ============================================================================
;; Register Validation
;; ============================================================================

;; x-registers: x0-x30
(define (valid-x-register? reg)
  (and (Reg:x? reg)
       (<= 0 (Reg:x-id reg) 30)))

;; w-registers: w0-w30
(define (valid-w-register? reg)
  (and (Reg:w? reg)
       (<= 0 (Reg:w-id reg) 30)))

;; z-registers: z0-z31
(define (valid-z-register? reg)
  (and (Reg:z? reg)
       (<= 0 (Reg:z-id reg) 31)))

;; p-registers: p0-p15
(define (valid-p-register? reg)
  (and (Reg:p? reg)
       (<= 0 (Reg:p-id reg) 15)))

;; v-registers: v0-v31
(define (valid-v-register? reg)
  (and (Reg:v? reg)
       (<= 0 (Reg:v-id reg) 31)
       (memq (Reg:v-width reg) '(b h s d q))))

;; Valid parameter register (for function params)
(define (valid-param-reg? reg)
  (or (and (Reg:x? reg) (<= 0 (Reg:x-id reg) 7))   ; x0-x7
      (and (Reg:w? reg) (<= 0 (Reg:w-id reg) 7))   ; w0-w7
      (and (Reg:z? reg) (<= 0 (Reg:z-id reg) 7))   ; z0-z7
      (and (Reg:p? reg) (<= 0 (Reg:p-id reg) 3))   ; p0-p3
      (and (Reg:v? reg) (<= 0 (Reg:v-id reg) 7)))) ; v0-v7

;; ============================================================================
;; Instruction Validation
;; ============================================================================

;; Validate a single instruction
(define (validate-insn insn #:config [config #f])
  (match insn
    [(Insn:arith op dst src1 src2)
     (combine-results
      (validate-arith-operands op dst src1 src2))]

    [(Insn:arith2 op dst src)
     (combine-results
      (validate-unary-operands op dst src))]

    [(Insn:load op dst addr)
     (combine-results
      (validate-load-operands op dst addr))]

    [(Insn:store op src addr)
     (combine-results
      (validate-store-operands op src addr))]

    [(Insn:sve op pred dst srcs)
     (validate-sve-instruction op pred dst srcs config)]

    [(Insn:sve-load op pred dst addr)
     (validate-sve-load op pred dst addr config)]

    [(Insn:sve-store op pred src addr)
     (validate-sve-store op pred src addr config)]

    [(Insn:branch op target)
     (validate-branch-target target)]

    [(Insn:cond-branch op cond target)
     (combine-results
      (validate-condition-code cond)
      (validate-branch-target target))]

    [(Insn:ret)
     (validation-ok)]

    [(AsmInsn op args)
     (validate-generic-insn op args config)]

    [_ (validation-error "unknown instruction type" insn)]))

;; Validate arithmetic operands
(define (validate-arith-operands op dst src1 src2)
  (cond
    ;; Integer ops use x/w registers
    [(memq op '(add sub mul sdiv udiv and orr eor bic))
     (combine-results
      (validate-gpr-operand dst "destination")
      (validate-gpr-or-imm-operand src1 "source1")
      (validate-gpr-or-imm-operand src2 "source2"))]

    ;; FP ops use v/d/s registers
    [(memq op '(fadd fsub fmul fdiv))
     (combine-results
      (validate-fp-operand dst "destination")
      (validate-fp-operand src1 "source1")
      (validate-fp-operand src2 "source2"))]

    [else (validation-ok)]))

;; Validate unary operands
(define (validate-unary-operands op dst src)
  (combine-results
   (validate-gpr-or-fp-operand dst "destination")
   (validate-gpr-or-fp-operand src "source")))

;; Validate load operands
(define (validate-load-operands op dst addr)
  (combine-results
   (validate-gpr-or-fp-operand dst "destination")
   (validate-memory-operand addr)))

;; Validate store operands
(define (validate-store-operands op src addr)
  (combine-results
   (validate-gpr-or-fp-operand src "source")
   (validate-memory-operand addr)))

;; Validate SVE instruction
(define (validate-sve-instruction op pred dst srcs config)
  (cond
    ;; Check if SVE is enabled
    [(and config (not (AsmConfig-sve? config)))
     (validation-error "SVE instruction used but SVE not enabled" op)]

    ;; Check if SVE2-only instruction
    [(and (sve2-op? op)
          config
          (not (AsmConfig-sve2? config)))
     (validation-error "SVE2 instruction used but SVE2 not enabled" op)]

    [else
     (combine-results
      (validate-predicate-operand pred)
      (validate-z-operand dst "destination")
      (validate-sve-sources srcs))]))

(define (valid-sve-instruction? op config)
  (cond
    [(sve2-op? op) (and config (AsmConfig-sve2? config))]
    [(sve-op? op) (and config (AsmConfig-sve? config))]
    [else #f]))

;; Validate SVE load
(define (validate-sve-load op pred dst addr config)
  (combine-results
   (if (and config (not (AsmConfig-sve? config)))
       (validation-error "SVE load used but SVE not enabled" op)
       (validation-ok))
   (validate-predicate-operand pred)
   (validate-z-operand dst "destination")
   (validate-memory-operand addr)))

;; Validate SVE store
(define (validate-sve-store op pred src addr config)
  (combine-results
   (if (and config (not (AsmConfig-sve? config)))
       (validation-error "SVE store used but SVE not enabled" op)
       (validation-ok))
   (validate-predicate-operand pred)
   (validate-z-operand src "source")
   (validate-memory-operand addr)))

;; Validate generic instruction
(define (validate-generic-insn op args config)
  ;; Basic validation - just check arg count for known ops
  (cond
    [(sve-op? op)
     (if (and config (not (AsmConfig-sve? config)))
         (validation-error "SVE instruction used but SVE not enabled" op)
         (validation-ok))]
    [(sve2-op? op)
     (if (and config (not (AsmConfig-sve2? config)))
         (validation-error "SVE2 instruction used but SVE2 not enabled" op)
         (validation-ok))]
    [else (validation-ok)]))

;; ============================================================================
;; Operand Type Validators
;; ============================================================================

(define (validate-gpr-operand op name)
  (if (or (Reg:x? op) (Reg:w? op) (Reg:sp? op) (Reg:xzr? op) (Reg:wzr? op))
      (validation-ok)
      (validation-error (format "~a must be GPR" name) op)))

(define (validate-gpr-or-imm-operand op name)
  (if (or (gpr? op) (Imm? op) (Imm:shifted? op))
      (validation-ok)
      (validation-error (format "~a must be GPR or immediate" name) op)))

(define (validate-fp-operand op name)
  (if (or (Reg:d? op) (Reg:s? op) (Reg:h? op)
          (and (Reg:v? op) (memq (Reg:v-width op) '(d s h))))
      (validation-ok)
      (validation-error (format "~a must be FP register" name) op)))

(define (validate-gpr-or-fp-operand op name)
  (if (or (gpr? op) (simd-reg? op))
      (validation-ok)
      (validation-error (format "~a must be GPR or FP register" name) op)))

(define (validate-z-operand op name)
  (if (Reg:z? op)
      (if (valid-z-register? op)
          (validation-ok)
          (validation-error (format "~a: invalid z-register id" name) op))
      (validation-error (format "~a must be z-register" name) op)))

(define (validate-predicate-operand op)
  (if (Reg:p? op)
      (if (valid-p-register? op)
          (validation-ok)
          (validation-error "invalid p-register id" op))
      (validation-error "expected predicate register" op)))

(define (validate-sve-sources srcs)
  (apply combine-results
         (for/list ([src (in-list srcs)]
                    [i (in-naturals)])
           (validate-z-operand src (format "source~a" i)))))

(define (validate-memory-operand op)
  (if (mem-addr? op)
      (validation-ok)
      (validation-error "expected memory address" op)))

(define (validate-branch-target target)
  (if (or (label? target) (Reg:x? target))
      (validation-ok)
      (validation-error "expected label or register for branch target" target)))

(define (validate-condition-code cond)
  (if (condition-code? cond)
      (validation-ok)
      (validation-error "invalid condition code" cond)))

;; ============================================================================
;; Block Validation
;; ============================================================================

(define (validate-block block #:config [config #f])
  (define insn-results
    (for/list ([insn (in-pvector (AsmBlock-insns block))])
      (validate-insn insn #:config config)))

  (define term-result
    (if (AsmBlock-terminator block)
        (validate-terminator (AsmBlock-terminator block))
        (validation-error "block missing terminator" block)))

  (apply combine-results (cons term-result insn-results)))

(define (validate-terminator term)
  (match term
    [(Term:ret) (validation-ok)]
    [(Term:jump target) (validate-block-target target)]
    [(Term:cond cond then else)
     (combine-results
      (validate-condition-code cond)
      (validate-block-target then)
      (if else (validate-block-target else) (validation-ok)))]
    [(Term:unreachable) (validation-ok)]
    [_ (validation-error "unknown terminator" term)]))

(define (validate-block-target target)
  (if (or (BlockId? target) (label? target))
      (validation-ok)
      (validation-error "invalid block target" target)))

;; ============================================================================
;; CFG Validation
;; ============================================================================

(define (validate-cfg cfg #:config [config #f])
  (define block-results
    (for/list ([block (in-cfg-blocks cfg)])
      (validate-block block #:config config)))

  (define entry-result
    (if (AsmCfg-entry cfg)
        (if (cfg-get-block cfg (AsmCfg-entry cfg))
            (validation-ok)
            (validation-error "entry block not found" (AsmCfg-entry cfg)))
        (validation-error "CFG missing entry block" cfg)))

  (apply combine-results (cons entry-result block-results)))

;; ============================================================================
;; Function Validation
;; ============================================================================

(define (validate-function fn #:config [config #f])
  (define param-results
    (for/list ([param (in-list (AsmFunction-params fn))])
      (validate-param param)))

  (define body-result
    (if (AsmCfg? (AsmFunction-body fn))
        (validate-cfg (AsmFunction-body fn) #:config config)
        (validation-ok)))

  (apply combine-results (cons body-result param-results)))

(define (validate-param param)
  (if (valid-param-reg? (AsmParam-reg param))
      (validation-ok)
      (validation-error "invalid parameter register" param)))
