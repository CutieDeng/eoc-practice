#lang racket/base

;; ============================================================
;; X86 Compilation Pipeline
;; ============================================================
;;
;; Complete pipeline from AST to x86 assembly.
;; Based on Essentials of Compilation (EoC) book.
;;
;; Stages:
;; 1. uniquify        - Make variable names unique
;; 2. remove-complex  - ANF transformation
;; 3. explicate-control - Convert to C-var IR
;; 4. select-instructions - Convert to x86-var
;; 5. assign-homes    - Assign variables to stack
;; 6. patch-instructions - Fix x86 constraints
;; 7. prelude-conclusion - Add prologue/epilogue
;; 8. emit-x86        - Generate assembly text
;;
;; ============================================================

(require "passes/uniquify.rkt"
         "passes/remove-complex.rkt"
         "passes/explicate-control.rkt"
         "passes/select-instructions.rkt"
         "passes/assign-homes.rkt"
         "passes/patch-instructions.rkt"
         "passes/prelude-conclusion.rkt"
         "passes/emit-x86.rkt")

(provide
  ;; Main pipeline
  compile-program
  compile-to-x86

  ;; Individual passes (for debugging/testing)
  uniquify
  remove-complex-opera*
  explicate-control
  select-instructions
  assign-homes
  patch-instructions
  prelude-and-conclusion
  emit-x86

  ;; Partial pipelines
  compile-to-cvar
  compile-to-x86var
  compile-to-x86-unpatched)

;; ============================================================
;; Main Pipeline
;; ============================================================

;; Compile AST program to x86 assembly string
(define (compile-program prog)
  (emit-x86
   (compile-to-x86 prog)))

;; Compile AST to X86Program
(define (compile-to-x86 prog)
  (prelude-and-conclusion
   (patch-instructions
    (assign-homes
     (select-instructions
      (explicate-control
       (remove-complex-opera*
        (uniquify prog))))))))

;; ============================================================
;; Partial Pipelines (for testing/debugging)
;; ============================================================

;; Compile to C-var IR
(define (compile-to-cvar prog)
  (explicate-control
   (remove-complex-opera*
    (uniquify prog))))

;; Compile to X86-var (before assign-homes)
(define (compile-to-x86var prog)
  (select-instructions
   (compile-to-cvar prog)))

;; Compile to X86 (before patching)
(define (compile-to-x86-unpatched prog)
  (assign-homes
   (compile-to-x86var prog)))
