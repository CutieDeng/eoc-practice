#lang racket/base

;; Golden test for the Java RVSDG lowering: every method of
;; ClassTransform.dat must lower to exactly the datum recorded in
;; test/integration/java-rvsdg-golden.rktd.  Regenerate the file only for
;; an intended output change (see scripts in doc/roadmap.md Phase 3.1).

(require rackunit racket/runtime-path racket/list
         "pipeline.rkt"
         "../../frontend/java/reader.rkt"
         "../../kernel/ir/jvm/types.rkt"
         "../../kernel/ir/gimple/datum.rkt")

(define-runtime-path fixture "../../../test/integration/ClassTransform.dat")
(define-runtime-path golden "../../../test/integration/java-rvsdg-golden.rktd")

(module+ test
  (test-case "Java RVSDG output equals the golden snapshot, method by method"
    (define expected (with-input-from-file golden read))
    (define klass (read-jvm-class-file fixture))
    (define methods (JvmClass-methods klass))
    (check-equal? (length methods) (length expected))
    (for ([m (in-list methods)] [e (in-list expected)])
      (check-equal? (JvmMethod-name m) (car e))
      (define actual (value->datum (java-method->rvsdg m)))
      (check-equal? actual (caddr e) (format "method ~a ~a" (car e) (cadr e))))))
