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
  ;; jvm-to-cfg, every fixture method should now lower to a Lambda.
  (test-case "java-compile-class returns per-method results"
    (define results (java-compile-class fixture-class-transform))
    (check-true (pair? results))
    ;; Every method must produce a Lambda -- no error fallbacks.
    (for ([kv (in-list results)])
      (check-pred Lambda? (cdr kv)
                  (format "method ~a did not lower to a Lambda" (car kv)))))

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
