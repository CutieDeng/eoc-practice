#lang racket/base

;; Alignment test for gimple-low: genericize + gimplify + lower + print
;; must reproduce GCC's -fdump-tree-lower (renumbered) for the fixtures.

(require rackunit racket/runtime-path racket/string racket/file racket/list
         "../generic/tree.rkt" "../generic/import.rkt"
         "genericize.rkt" "gimplify.rkt" "gimple-low.rkt" "gimple-print.rkt"
         (only-in "gimplify-test.rkt" normalize-gimple-text)
         "../../../frontend/gccast/reader.rkt"
         "../../../kernel/ir/gccast/types.rkt")

(provide lower-tu->text)

(define-runtime-path fixtures "../../../../test/integration/gccast")

(define (lower-tu->text tu)
  (install-tu-context! tu)
  (parameterize ([tmp-var-id (box 0)] [uid-counter (box 100000)])
    (define o (open-output-string))
    (for ([fn (TU-fns tu)])
      (define-values (decl body) (import-fn tu fn))
      (define b (box body))
      (c-genericize! decl (box-ref b))
      (define bind (gimplify-function decl (unbox b)))
      (define lo (lower-function decl bind))
      (print-lowered-function decl (lowered-local-decls lo) (lowered-body lo) o))
    (get-output-string o)))

(define (first-diff a b)
  (define la (string-split a "\n" #:trim? #f)) (define lb (string-split b "\n" #:trim? #f))
  (for/first ([i (in-naturals)] [x (in-list (append la (make-list (max 0 (- (length lb) (length la))) "<eof>")))]
              [y (in-list (append lb (make-list (max 0 (- (length la) (length lb))) "<eof>")))]
              #:unless (string=? x y))
    (format "line ~a:\n  ours:   ~a\n  gcc:    ~a" (add1 i) x y)))

(module+ test
  (for* ([ver '("15.2.0" "12.5.0")]
         [name '("basic.c" "calls.c" "struct.c" "locals.c")])
    (test-case (format "~a ~a: lower dump aligns with GCC" name ver)
      (define tu (read-gccast-file (build-path fixtures (format "~a.~a.dat" name ver))))
      (define ours (normalize-gimple-text (lower-tu->text tu)))
      (define gcc (normalize-gimple-text (file->string (build-path fixtures (format "~a.~a.lower" name ver)))))
      (unless (string=? ours gcc)
        (fail (format "mismatch (~a ~a)\n~a\n--- ours ---\n~a\n--- gcc ---\n~a" name ver (first-diff ours gcc) ours gcc))))))
