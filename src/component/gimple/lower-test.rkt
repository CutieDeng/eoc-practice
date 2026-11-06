#lang racket/base

;; Alignment test (pure pipeline): import + genericize + gimplify + lower +
;; print must reproduce GCC's -fdump-tree-lower (renumbered) for the fixtures.

(require rackunit racket/runtime-path racket/string racket/file
         "../../kernel/data/data.rkt"
         "../../kernel/ir/gimple/types.rkt"
         "../../kernel/ir/gccast/types.rkt"
         "../../frontend/gccast/reader.rkt"
         (only-in "gimplify-test.rkt" normalize-gimple-text first-diff)
         "state.rkt" "import.rkt" "genericize.rkt" "gimplify.rkt" "lower.rkt" "print.rkt")

(provide lower-tu lower-fn)

(define-runtime-path fixtures "../../../test/integration/gccast")

;; one function through lower: (values decl body local-decls st)
(define (lower-fn st fn)
  (define-values (decl body st1) (import-fn st fn))
  (define-values (body* st2) (c-genericize st1 decl body))
  (define-values (bind st3) (gimplify-function st2 decl body*))
  (define-values (lowered locals st4) (lower-function st3 decl bind))
  (values decl lowered locals st4))

;; (values text st)
(define (lower-tu tu)
  (define o (open-output-string))
  (define st*
    (for/fold ([st (import-tu tu)]) ([fn (in-list (TU-fns tu))])
      (define-values (decl body locals st1) (lower-fn st fn))
      (print-lowered-function st1 decl locals body o)
      st1))
  (values (get-output-string o) st*))

(module+ test
  (for* ([ver '("15.2.0" "12.5.0")]
         [name '("basic.c" "calls.c" "struct.c" "locals.c" "ccp.c")])
    (test-case (format "~a ~a: lower dump aligns with GCC (pure pipeline)" name ver)
      (define tu (read-gccast-file (build-path fixtures (format "~a.~a.dat" name ver))))
      (define-values (text st) (lower-tu tu))
      (define ours (normalize-gimple-text text))
      (define gcc (normalize-gimple-text (file->string (build-path fixtures (format "~a.~a.lower" name ver)))))
      (unless (string=? ours gcc)
        (fail (format "mismatch (~a ~a)\n~a\n--- ours ---\n~a\n--- gcc ---\n~a" name ver (first-diff ours gcc) ours gcc))))))
