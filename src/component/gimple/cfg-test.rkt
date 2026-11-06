#lang racket/base

;; Alignment test (pure pipeline): import + genericize + gimplify + lower +
;; build-cfg + print must reproduce GCC's -fdump-tree-cfg (renumbered).
;; locals.c is excluded: its GIMPLE_TRY needs pass_lower_eh first.

(require rackunit racket/runtime-path racket/string racket/file
         "../../kernel/data/data.rkt"
         "../../kernel/ir/gimple/types.rkt"
         "../../kernel/ir/gimple/datum.rkt"
         "../../kernel/ir/gccast/types.rkt"
         "../../frontend/gccast/reader.rkt"
         (only-in "gimplify-test.rkt" normalize-gimple-text first-diff)
         (only-in "lower-test.rkt" lower-fn)
         "state.rkt" "import.rkt" "cfg.rkt" "print.rkt")

(provide cfg-tu normalize-cfg-text)

(define-runtime-path fixtures "../../../test/integration/gccast")

;; (values text st cfgs)
(define (cfg-tu tu)
  (define o (open-output-string))
  (define-values (st* cfgs)
    (for/fold ([st (import-tu tu)] [cfgs '()] #:result (values st (reverse cfgs))) ([fn (in-list (TU-fns tu))])
      (define-values (decl body locals st1) (lower-fn st fn))
      (define-values (cfg st2) (build-cfg st1 decl body #:cond->string print-cond-rhs))
      (print-cfg-function st2 decl locals cfg o)
      (values st2 (cons cfg cfgs))))
  (values (get-output-string o) st* cfgs))

;; keep the ";; N succs" lines (they pin the edge order), drop the other ";;" lines
(define (normalize-cfg-text s)
  (normalize-gimple-text
   (string-join (for/list ([l (string-split s "\n" #:trim? #f)])
                  (if (regexp-match? #px"^;; [0-9]+ succs \\{" l) (string-append "succs" (substring l 2)) l))
                "\n")))

(module+ test
  (for* ([ver '("15.2.0" "12.5.0")]
         [name '("basic.c" "calls.c" "struct.c")])
    (test-case (format "~a ~a: cfg dump aligns with GCC (pure pipeline)" name ver)
      (define tu (read-gccast-file (build-path fixtures (format "~a.~a.dat" name ver))))
      (define-values (text st cfgs) (cfg-tu tu))
      (define ours (normalize-cfg-text text))
      (define gcc (normalize-cfg-text (file->string (build-path fixtures (format "~a.~a.cfg" name ver)))))
      (unless (string=? ours gcc)
        (fail (format "mismatch (~a ~a)\n~a\n--- ours ---\n~a\n--- gcc ---\n~a" name ver (first-diff ours gcc) ours gcc)))
      ;; the CFG is a datum
      (for ([cfg (in-list cfgs)])
        (define d (value->datum cfg))
        (check-equal? (datum->value (read (open-input-string (format "~s" d)))) cfg)))))
