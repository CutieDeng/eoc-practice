#lang racket/base

;; ============================================================================
;; AArch64-SVE-ASM Compiler - Main Entry Point
;; ============================================================================
;;
;; Simple API for compiling aarch64-asm S-expressions to assembly files.
;;
;; Usage:
;;   (require "compiler/component/aarch64/main.rkt")
;;
;;   ;; Compile S-expr to .s file
;;   (compile my-fn "output.s")
;;
;;   ;; Compile with SVE enabled
;;   (compile my-fn "output.s" #:config config/sve-256)
;;
;;   ;; Get assembly as string
;;   (compile-to-string my-fn)
;;
;; ============================================================================

(require racket/match
         racket/file
         racket/set
         "ir/types.rkt"
         "ir/cfg.rkt"
         "ir/config.rkt"
         "frontend/parser.rkt"
         "frontend/validator.rkt"
         "frontend/type-check.rkt"
         "analysis/liveness.rkt"
         "backend/regalloc.rkt"
         "backend/emit.rkt"
         "../../../cutie-ftree/pvector.rkt"
         "../../../cutie-ftree/ordered-map.rkt")

(provide
 ;; Main API
 compile
 compile-to-string
 compile-sexp

 ;; Pipeline stages (for advanced use)
 parse
 validate
 type-check
 allocate
 emit

 ;; Re-exports
 (all-from-out "ir/config.rkt")
 (all-from-out "ir/types.rkt"))

;; ============================================================================
;; Compilation Result
;; ============================================================================

(struct CompileResult (
  success?      ; Boolean
  output        ; String (assembly) or #f
  ir            ; AsmFunction or #f
  errors        ; List of error messages
) #:prefab)

(provide (struct-out CompileResult))

;; ============================================================================
;; Pipeline Stages
;; ============================================================================

;; Stage 1: Parse S-expression to IR
(define (parse sexp)
  (with-handlers ([exn:fail? (lambda (e) (values #f (exn-message e)))])
    (values (parse-asm-fn sexp) #f)))

;; Stage 2: Validate syntax
(define (validate fn)
  (define result (validate-function fn))
  (if (ValidationResult-ok? result)
      (values fn #f)
      (values #f (format "Validation failed: ~a" (ValidationResult-errors result)))))

;; Stage 3: Type check
(define (type-check fn config)
  (define result (type-check-function fn #:config config))
  (if (TypeResult-ok? result)
      (values fn #f)
      (values #f (format "Type check failed: ~a" (TypeResult-errors result)))))

;; Stage 4: Register allocation
(define (allocate fn)
  (define cfg (AsmFunction-body fn))
  (define alloc-result (allocate-registers cfg))

  (if (AllocationResult-success? alloc-result)
      (let ()
        (define new-cfg (apply-allocation cfg alloc-result))
        (define new-fn (struct-copy AsmFunction fn [body new-cfg]))
        (values new-fn #f))
      (values #f (format "Register allocation failed: ~a spills needed"
                         (set-count (AllocationResult-spilled alloc-result))))))

;; Stage 5: Emit assembly
(define (emit fn)
  (with-handlers ([exn:fail? (lambda (e) (values #f (exn-message e)))])
    (values (emit-to-string fn) #f)))

;; ============================================================================
;; Main Compilation Pipeline
;; ============================================================================

(define (compile-sexp sexp #:config [config #f])
  ;; Stage 1: Parse
  (define-values (fn-parsed parse-err) (parse sexp))
  (when parse-err
    (return-error parse-err))

  ;; Stage 2: Validate
  (define-values (fn-validated validate-err) (validate fn-parsed))
  (when validate-err
    (return-error validate-err))

  ;; Stage 3: Type check
  (define-values (fn-checked type-err) (type-check fn-validated config))
  (when type-err
    (return-error type-err))

  ;; Stage 4: Register allocation
  (define-values (fn-allocated alloc-err) (allocate fn-checked))
  (when alloc-err
    (return-error alloc-err))

  ;; Stage 5: Emit
  (define-values (asm-output emit-err) (emit fn-allocated))
  (when emit-err
    (return-error emit-err))

  (CompileResult #t asm-output fn-allocated '()))

(define-syntax-rule (return-error msg)
  (CompileResult #f #f #f (list msg)))

;; ============================================================================
;; Simple API
;; ============================================================================

;; Compile S-expr and get assembly string
(define (compile-to-string sexp #:config [config #f])
  (define result (compile-sexp sexp #:config config))
  (if (CompileResult-success? result)
      (CompileResult-output result)
      (error 'compile-to-string "Compilation failed: ~a"
             (CompileResult-errors result))))

;; Compile S-expr and write to file
(define (compile sexp output-path #:config [config #f])
  (define result (compile-sexp sexp #:config config))

  (if (CompileResult-success? result)
      (begin
        (call-with-output-file output-path
          (lambda (out)
            (display (CompileResult-output result) out))
          #:exists 'replace)
        #t)
      (error 'compile "Compilation failed: ~a"
             (CompileResult-errors result))))

;; ============================================================================
;; Convenience: Compile from file
;; ============================================================================

(define (compile-file input-path output-path #:config [config #f])
  (define sexp (call-with-input-file input-path read))
  (compile sexp output-path #:config config))

(provide compile-file)
