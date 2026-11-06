#lang racket/base

;; Alignment test for gimple-to-cfg: genericize + gimplify + lower + build-cfg
;; + print must reproduce GCC's -fdump-tree-cfg (renumbered) for the fixtures.
;; locals.c is excluded: its GIMPLE_TRY needs pass_lower_eh first.

(require rackunit racket/runtime-path racket/string racket/file racket/list
         "../generic/tree.rkt" "../generic/import.rkt"
         "genericize.rkt" "gimplify.rkt" "gimple-low.rkt" "gimple-to-cfg.rkt" "gimple-print.rkt"
         (only-in "gimplify-test.rkt" normalize-gimple-text)
         "../../../frontend/gccast/reader.rkt"
         "../../../kernel/ir/gccast/types.rkt")

(provide cfg-tu->text normalize-cfg-text)

(define-runtime-path fixtures "../../../../test/integration/gccast")

(define (cfg-tu->text tu)
  (install-tu-context! tu)
  (parameterize ([tmp-var-id (box 0)] [uid-counter (box 100000)])
    (define o (open-output-string))
    (for ([fn (TU-fns tu)])
      (define-values (decl body) (import-fn tu fn))
      (define b (box body))
      (c-genericize! decl (box-ref b))
      (define bind (gimplify-function decl (unbox b)))
      (define lo (lower-function decl bind))
      (define cfg (build-cfg decl (lowered-body lo) #:cond->string print-cond-rhs))
      (print-cfg-function decl (lowered-local-decls lo) cfg o))
    (get-output-string o)))

;; keep the ";; N succs" lines (they pin the edge order), drop the other ";;" lines
(define (normalize-cfg-text s)
  (normalize-gimple-text
   (string-join (for/list ([l (string-split s "\n" #:trim? #f)])
                  (if (regexp-match? #px"^;; [0-9]+ succs \\{" l) (string-append "succs" (substring l 2)) l))
                "\n")))

(define (first-diff a b)
  (define la (string-split a "\n" #:trim? #f)) (define lb (string-split b "\n" #:trim? #f))
  (for/first ([i (in-naturals)] [x (in-list (append la (make-list (max 0 (- (length lb) (length la))) "<eof>")))]
              [y (in-list (append lb (make-list (max 0 (- (length la) (length lb))) "<eof>")))]
              #:unless (string=? x y))
    (format "line ~a:\n  ours:   ~a\n  gcc:    ~a" (add1 i) x y)))

(module+ test
  (for* ([ver '("15.2.0" "12.5.0")]
         [name '("basic.c" "calls.c" "struct.c")])
    (test-case (format "~a ~a: cfg dump aligns with GCC" name ver)
      (define tu (read-gccast-file (build-path fixtures (format "~a.~a.dat" name ver))))
      (define ours (normalize-cfg-text (cfg-tu->text tu)))
      (define gcc (normalize-cfg-text (file->string (build-path fixtures (format "~a.~a.cfg" name ver)))))
      (unless (string=? ours gcc)
        (fail (format "mismatch (~a ~a)\n~a\n--- ours ---\n~a\n--- gcc ---\n~a" name ver (first-diff ours gcc) ours gcc))))))
