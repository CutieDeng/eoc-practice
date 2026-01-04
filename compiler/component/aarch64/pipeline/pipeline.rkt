#lang racket/base

;; ============================================================================
;; AArch64-SVE-ASM Pipeline
;; ============================================================================
;;
;; Main entry point for the aarch64-sve-asm compiler pipeline.
;; Provides unified interface for parsing, validating, type-checking,
;; and interpreting aarch64 assembly code.
;;
;; Usage:
;;   (require "compiler/component/aarch64/pipeline/pipeline.rkt")
;;   (define result (compile-and-run my-fn #:args '(1 2 3)))
;;
;; ============================================================================

(require racket/class
         racket/match
         racket/list
         "../ir/types.rkt"
         "../ir/cfg.rkt"
         "../ir/config.rkt"
         "../ir/sve-insns.rkt"
         "../frontend/parser.rkt"
         (prefix-in v: "../frontend/validator.rkt")
         (prefix-in tc: "../frontend/type-check.rkt")
         "../interp/state.rkt"
         "../interp/base.rkt"
         "../interp/sve.rkt"
         "../../../../cutie-ftree/pvector.rkt"
         "../../../pipeline/common/pass.rkt")

(provide
 ;; High-level API
 compile-function
 compile-and-run
 compile-and-interpret
 parse-and-validate

 ;; Pipeline construction
 make-aarch64-pipeline
 aarch64-base-pipeline
 aarch64-sve-pipeline

 ;; Individual passes
 parse-pass
 validate-pass
 type-check-pass

 ;; Interpreter helpers
 make-base-interp
 make-sve-interp
 run-function
 run-cfg

 ;; Re-exports for convenience
 (all-from-out "../ir/types.rkt")
 (all-from-out "../ir/cfg.rkt")
 (all-from-out "../ir/config.rkt")
 (all-from-out "../frontend/parser.rkt")
 (all-from-out "../interp/state.rkt")
 (all-from-out "../interp/base.rkt")
 (all-from-out "../interp/sve.rkt"))

;; ============================================================================
;; Result Types
;; ============================================================================

;; Compilation result
(struct CompileResult (
  success?      ; Boolean
  ir            ; AsmFunction or #f
  cfg           ; AsmCfg or #f
  errors        ; (Listof Error) or '()
  warnings      ; (Listof Warning) or '()
) #:prefab)

;; Execution result
(struct ExecResult (
  success?      ; Boolean
  value         ; Return value or #f
  state         ; Final MachineState
  error         ; Error message or #f
) #:prefab)

(provide (struct-out CompileResult)
         (struct-out ExecResult))

;; ============================================================================
;; Individual Passes
;; ============================================================================

;; Parse pass: S-expr -> AsmFunction
(define parse-pass
  (make-pass 'parse
    (lambda (sexp)
      (parse-asm-fn sexp))
    #:description "Parse S-expression to IR"))

;; Validate pass: AsmFunction -> AsmFunction (or error)
(define validate-pass
  (make-pass 'validate
    (lambda (fn)
      (define result (v:validate-function fn))
      (if (v:ValidationResult-ok? result)
          fn
          (error 'validate "Validation failed: ~a" (v:ValidationResult-errors result))))
    #:description "Validate syntax and structure"
    #:requires '(parse)))

;; Type check pass: AsmFunction -> AsmFunction (or error)
(define type-check-pass
  (make-pass 'type-check
    (lambda (fn)
      (define result (tc:type-check-function fn))
      (if (tc:TypeResult-ok? result)
          fn
          (error 'type-check "Type check failed: ~a" (tc:TypeResult-errors result))))
    #:description "Check type compatibility"
    #:requires '(validate)))

;; ============================================================================
;; Pipeline Construction
;; ============================================================================

;; Base pipeline (parsing, validation, type checking)
(define aarch64-base-pipeline
  (make-pipeline 'aarch64-base
    (list parse-pass
          validate-pass
          type-check-pass)))

;; SVE pipeline (includes SVE-specific passes)
(define aarch64-sve-pipeline
  (make-pipeline 'aarch64-sve
    (list parse-pass
          validate-pass
          type-check-pass)
    #:config (hash 'sve #t 'sve2 #f)))

;; Create a custom pipeline
(define (make-aarch64-pipeline #:name [name 'aarch64-custom]
                               #:sve? [sve? #f]
                               #:sve2? [sve2? #f]
                               #:extra-passes [extra '()])
  (make-pipeline name
    (append (list parse-pass validate-pass type-check-pass) extra)
    #:config (hash 'sve sve? 'sve2 sve2?)))

;; ============================================================================
;; High-Level API
;; ============================================================================

;; Compile a function (parse + validate + type-check)
(define (compile-function sexp
                          #:config [config #f]
                          #:pipeline [pipeline aarch64-base-pipeline])
  (with-handlers
    ([exn:fail?
      (lambda (e)
        (CompileResult #f #f #f (list (exn-message e)) '()))])

    (define-values (ir stats) (run-pipeline-with-stats pipeline sexp))
    (CompileResult #t ir (AsmFunction-body ir) '() '())))

;; Parse and validate (returns IR or raises error)
(define (parse-and-validate sexp)
  (define result (compile-function sexp))
  (if (CompileResult-success? result)
      (CompileResult-ir result)
      (error 'parse-and-validate "Compilation failed: ~a"
             (CompileResult-errors result))))

;; Compile and run with interpreter
(define (compile-and-run sexp
                         #:args [args '()]
                         #:config [config #f]
                         #:max-steps [max-steps 10000])
  (define compile-result (compile-function sexp #:config config))

  (if (not (CompileResult-success? compile-result))
      (ExecResult #f #f (make-empty-state)
                  (format "Compilation failed: ~a"
                          (CompileResult-errors compile-result)))
      (let ()
        (define fn (CompileResult-ir compile-result))
        (define cfg (CompileResult-cfg compile-result))

        ;; Create interpreter
        (define interp
          (if (and config (config-sve? config))
              (make-sve-interp #:config config)
              (new interp-aarch64-base% [config config])))

        ;; Load arguments
        (load-function-args interp fn args)

        ;; Execute
        (with-handlers
          ([exn:fail?
            (lambda (e)
              (ExecResult #f #f (send interp get-state) (exn-message e)))])

          (define result (send interp run-cfg cfg #:max-steps max-steps))
          (define final-state (send interp get-state))
          (define return-val (state-read-x final-state 0))

          (ExecResult #t return-val final-state #f)))))

;; Compile and interpret (simpler interface)
(define (compile-and-interpret sexp args #:config [config #f])
  (define result (compile-and-run sexp #:args args #:config config))
  (if (ExecResult-success? result)
      (ExecResult-value result)
      (error 'compile-and-interpret "Execution failed: ~a"
             (ExecResult-error result))))

;; ============================================================================
;; Interpreter Helpers
;; ============================================================================

;; Create base interpreter
(define (make-base-interp #:config [config #f])
  (new interp-aarch64-base% [config config]))

;; Run a parsed function with arguments
(define (run-function fn args #:config [config #f] #:max-steps [max-steps 10000])
  (define interp
    (if (and config (config-sve? config))
        (make-sve-interp #:config config)
        (make-base-interp #:config config)))

  (load-function-args interp fn args)

  (define cfg (AsmFunction-body fn))
  (define result (send interp run-cfg cfg #:max-steps max-steps))
  (define final-state (send interp get-state))

  (values (state-read-x final-state 0)
          final-state
          result))

;; Run a CFG directly
(define (run-cfg cfg #:interp [interp #f] #:config [config #f])
  (define the-interp
    (or interp
        (if (and config (config-sve? config))
            (make-sve-interp #:config config)
            (make-base-interp #:config config))))

  (send the-interp run-cfg cfg))

;; ============================================================================
;; Internal Helpers
;; ============================================================================

;; Load function arguments into interpreter state
(define (load-function-args interp fn args)
  (define params (AsmFunction-params fn))

  (for ([param (in-list params)]
        [arg (in-list args)]
        [i (in-naturals)])
    (define reg (AsmParam-reg param))
    (match reg
      [(Reg:x id) (send interp write-x id arg)]
      [(Reg:w id) (send interp write-x id (bitwise-and arg #xFFFFFFFF))]
      ;; For SVE registers, would need SVE interp
      [_ (void)])))

;; Check if config enables SVE
(define (config-sve? config)
  (and config (AsmConfig-sve? config)))

;; ============================================================================
;; Example Usage (commented out)
;; ============================================================================

#|
;; Simple function
(define add-fn
  '(asm-fn add-two
     ([x0 : i64] [x1 : i64])
     -> i64
     (add x0 x0 x1)
     (ret)))

;; Compile and run
(define result (compile-and-run add-fn #:args '(10 32)))
(printf "Result: ~a\n" (ExecResult-value result))
;; => 42

;; SVE function
(define sve-fn
  '(asm-fn vec-sum
     ([x0 : (ptr f32)] [x1 : i64])
     -> i64
     (whilelt p0 xzr x1)
     (ld1w z0 p0 [x0])
     ;; ... reduction ...
     (ret)))

(define config config/sve-256)
(define sve-result (compile-and-run sve-fn
                     #:args '(ptr len)
                     #:config config))
|#
