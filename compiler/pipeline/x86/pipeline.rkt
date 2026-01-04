#lang racket/base

;; ============================================================
;; X86 Compilation Pipeline
;; ============================================================
;;
;; Complete pipeline from AST to x86 assembly.
;; Based on Essentials of Compilation (EoC) book.
;;
;; Uses the Pass framework for pipeline orchestration.
;; Passes are imported from the component layer.
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

(require "../common/pass.rkt"
         "../../component/lvar/lvar.rkt"
         "../../component/x86var/x86var.rkt")

(provide
  ;; Pipeline
  x86-pipeline
  x86-codegen-pipeline

  ;; Main pipeline functions
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
  compile-to-x86-unpatched

  ;; Pass definitions
  uniquify-pass
  remove-complex-pass
  explicate-control-pass
  select-instructions-pass
  assign-homes-pass
  patch-instructions-pass
  prelude-conclusion-pass
  emit-x86-pass)

;; ============================================================
;; Pass Definitions
;; ============================================================

(define uniquify-pass
  (make-pass 'uniquify uniquify
    #:description "Make variable names unique using integer IDs"))

(define remove-complex-pass
  (make-pass 'remove-complex remove-complex-opera*
    #:description "Transform to A-normal form (ANF)"
    #:requires '(uniquify)))

(define explicate-control-pass
  (make-pass 'explicate-control explicate-control
    #:description "Convert to C-var IR with explicit control flow"
    #:requires '(remove-complex)))

(define select-instructions-pass
  (make-pass 'select-instructions select-instructions
    #:description "Convert C-var to x86-var instructions"
    #:requires '(explicate-control)))

(define assign-homes-pass
  (make-pass 'assign-homes assign-homes
    #:description "Assign variables to stack locations"
    #:requires '(select-instructions)))

(define patch-instructions-pass
  (make-pass 'patch-instructions patch-instructions
    #:description "Fix x86 instruction constraints"
    #:requires '(assign-homes)))

(define prelude-conclusion-pass
  (make-pass 'prelude-conclusion prelude-and-conclusion
    #:description "Add function prologue and epilogue"
    #:requires '(patch-instructions)))

(define emit-x86-pass
  (make-pass 'emit-x86 emit-x86
    #:description "Generate x86 assembly text"
    #:requires '(prelude-conclusion)))

;; ============================================================
;; Pipeline Definitions
;; ============================================================

;; Full pipeline from AST to X86Program
(define x86-pipeline
  (make-pipeline 'x86
    (list uniquify-pass
          remove-complex-pass
          explicate-control-pass
          select-instructions-pass
          assign-homes-pass
          patch-instructions-pass
          prelude-conclusion-pass)))

;; Full pipeline including emit (produces string)
(define x86-codegen-pipeline
  (make-pipeline 'x86-codegen
    (list uniquify-pass
          remove-complex-pass
          explicate-control-pass
          select-instructions-pass
          assign-homes-pass
          patch-instructions-pass
          prelude-conclusion-pass
          emit-x86-pass)))

;; ============================================================
;; Main Pipeline Functions
;; ============================================================

;; Compile AST program to x86 assembly string
(define (compile-program prog)
  (run-pipeline x86-codegen-pipeline prog))

;; Compile AST to X86Program
(define (compile-to-x86 prog)
  (run-pipeline x86-pipeline prog))

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
