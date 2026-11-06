#lang racket/base

;; Alignment test: ... + build-cfg + build-ssa + print must reproduce GCC's
;; -fdump-tree-ssa (pass_build_ssa, -O0) for the fixtures, modulo the
;; numbering of artificial names.  locals.c waits for pass_lower_eh.

(require rackunit racket/runtime-path racket/string racket/file
         "../../kernel/data/data.rkt"
         "../../kernel/ir/gimple/types.rkt"
         "../../kernel/ir/gimple/datum.rkt"
         "../../kernel/ir/gccast/types.rkt"
         "../../frontend/gccast/reader.rkt"
         (only-in "gimplify-test.rkt" normalize-gimple-text first-diff)
         (only-in "lower-test.rkt" lower-fn)
         "state.rkt" "import.rkt" "cfg.rkt" "ssa.rkt" "print.rkt")

(provide ssa-tu ssa-fn)

(define-runtime-path fixtures "../../../test/integration/gccast")

;; one function through SSA: (values decl cfg locals st)
(define (ssa-fn st fn)
  (define-values (decl body locals st1) (lower-fn st fn))
  (define-values (cfg st2) (build-cfg st1 decl body #:cond->string print-cond-rhs))
  (define-values (cfg* st3) (build-ssa st2 decl cfg))
  (values decl cfg* locals st3))

;; (values text st cfgs); VOPS? selects the -vops dump format
(define (ssa-tu tu [vops? #f])
  (define o (open-output-string))
  (define-values (st* cfgs)
    (parameterize ([print-vops? vops?])
      (for/fold ([st (import-tu tu)] [cfgs '()] #:result (values st (reverse cfgs))) ([fn (in-list (TU-fns tu))])
        (define-values (decl cfg locals st1) (ssa-fn st fn))
        (print-ssa-function st1 decl locals cfg o)
        (values st1 (cons cfg cfgs)))))
  (values (get-output-string o) st* cfgs))

(module+ test
  (for* ([ver '("15.2.0" "12.5.0")]
         [name '("basic.c" "calls.c" "struct.c" "ccp.c")]
         [oracle '("ssa" "ssavops")])
    (test-case (format "~a ~a: ~a dump aligns with GCC" name ver oracle)
      (define tu (read-gccast-file (build-path fixtures (format "~a.~a.dat" name ver))))
      (define-values (text st cfgs) (ssa-tu tu (equal? oracle "ssavops")))
      (define ours (normalize-gimple-text text))
      (define gcc (normalize-gimple-text (file->string (build-path fixtures (format "~a.~a.~a" name ver oracle)))))
      (unless (string=? ours gcc)
        (fail (format "mismatch (~a ~a)\n~a\n--- ours ---\n~a\n--- gcc ---\n~a" name ver (first-diff ours gcc) ours gcc)))
      (for ([cfg (in-list cfgs)])
        (define d (value->datum cfg))
        (check-equal? (datum->value (read (open-input-string (format "~s" d)))) cfg)))))
