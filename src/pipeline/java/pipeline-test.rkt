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
  ;; End-to-end: read → CFG → SSA → RVSDG for every method.  C4
  ;; closed the SSA handler-block rename gap (ssa-construct now
  ;; augments the dominance / rename graph with exception edges) and
  ;; the Kappa join refinement in cfg-to-rvsdg promotes the true
  ;; try/handler merge point past any trivial Term:jump chain, so
  ;; every fixture method — including `test()` whose try/catch merges
  ;; past a one-block forwarder — should now lower to a Lambda.
  (test-case "java-compile-class returns per-method results"
    (define results (java-compile-class fixture-class-transform))
    (check-true (pair? results))
    (for ([kv (in-list results)])
      (define name (car kv))
      (define val  (cdr kv))
      (check-pred Lambda? val
                  (format "method ~a did not lower to a Lambda" name))))

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
