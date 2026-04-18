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
  ;; End-to-end: read → CFG → SSA → RVSDG for every method.  Linear
  ;; (init) and single if-else diamond (test) methods should produce
  ;; Lambdas; methods with loops / switches still surface as
  ;; ('error . message) until Theta/Kappa recovery lands.  Either
  ;; way, the wiring itself must not throw.
  (test-case "java-compile-class returns per-method results"
    (define results (java-compile-class fixture-class-transform))
    (check-true (pair? results))
    (define (pick name)
      (for/or ([kv (in-list results)])
        (and (equal? (car kv) name) (cdr kv))))
    ;; init must succeed -- it's linear.
    (define init-r (pick "init"))
    (check-not-false init-r)
    (check-pred Lambda? init-r)
    ;; test must now succeed post-Gamma-recovery (single if-else).
    (define test-r (pick "test"))
    (check-not-false test-r)
    (check-pred Lambda? test-r))

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
