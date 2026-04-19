#lang racket/base

;; ============================================================
;; Tests: Java compilation pipeline wiring
;; ============================================================

(require rackunit
         racket/runtime-path
         "pipeline.rkt"
         "../../frontend/java/reader.rkt"
         "../../kernel/ir/jvm/types.rkt"
         "../../kernel/ir/rvsdg/rvsdg.rkt")

(define-runtime-path fixture-class-transform
  "../../../test/integration/ClassTransform.dat")

(module+ test
  ;; End-to-end: read → CFG → SSA → RVSDG for every method.  With
  ;; M4 Gamma + M5 Theta + the object-creation opcode coverage in
  ;; jvm-to-cfg, every fixture method should lower to a Lambda — with
  ;; one documented exception: `test()` carries a try/catch whose
  ;; handler block is graph-unreachable from the method entry, so
  ;; `ssa-construct`'s dominator-tree walk leaves its pre-SSA VarIds
  ;; un-renamed; C3 Kappa lowering now visits those handler blocks
  ;; and trips on the stale VarIds.  Fixing SSA to process handler
  ;; subtrees (either via SSA-only exception edges or by re-rooting
  ;; rename-block at each handler with initial-local stacks) is the
  ;; pending C4 work item; until then `test()` is expected to error.
  (test-case "java-compile-class returns per-method results"
    (define results (java-compile-class fixture-class-transform))
    (check-true (pair? results))
    (for ([kv (in-list results)])
      (define name (car kv))
      (define val  (cdr kv))
      (cond
        [(equal? name "test")
         ;; Known-incomplete: SSA leaves handler-block VarIds un-renamed.
         ;; Assert it surfaces as the per-method error fallback rather
         ;; than crashing the whole compile.
         (check-true (and (pair? val) (eq? (car val) 'error))
                     (format "method ~a expected to error (SSA handler-block gap)"
                             name))]
        [else
         (check-pred Lambda? val
                     (format "method ~a did not lower to a Lambda" name))])))

  (test-case "java-method->cfg / ->ssa-cfg / ->rvsdg compose correctly"
    (define klass (read-jvm-class-file fixture-class-transform))
    (define init-m
      (for/or ([mth (JvmClass-methods klass)])
        (and (equal? (JvmMethod-name mth) "init") mth)))
    (check-not-false init-m)
    ;; Each staged entry point must return a usable artefact.
    (check-true (and (java-method->cfg init-m) #t))
    (check-true (and (java-method->ssa-cfg init-m) #t))
    (check-pred Lambda? (java-method->rvsdg init-m)))

  (test-case "pipeline-info advertises current scope"
    (define info (java-pipeline-info))
    (check-equal? (hash-ref info 'name) 'java)
    (check-equal? (hash-ref info 'status) 'partial)))
