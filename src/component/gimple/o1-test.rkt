#lang racket/base

;; -O1 alignment: the plugin's -O1 GENERIC (<name>.<ver>.O1.dat) through the
;; same chain with GS-optimize = 1 must reproduce GCC's -O1 dumps of
;; gimplify / lower / cfg / ssa and then of every ported -O1 pass.  Functions are compared one by one (GCC
;; orders them by call-graph position when optimizing), each renumbered.

(require rackunit racket/runtime-path racket/string racket/file racket/list
         "../../kernel/data/data.rkt"
         "../../kernel/ir/gimple/types.rkt"
         "../../kernel/ir/gccast/types.rkt"
         "../../frontend/gccast/reader.rkt"
         (only-in "gimplify-test.rkt" normalize-gimple-text first-diff)
         "state.rkt" "import.rkt" "genericize.rkt" "gimplify.rkt" "lower.rkt" "cfg.rkt" "ssa.rkt" "print.rkt"
         "opt/ccp.rkt")

;; the -O1 passes ported so far, in GCC's pass order (doc/roadmap.md 7.1)
(define stages '(gimple lower cfg ssa ccp1))

(provide split-functions o1-stage-texts)

(define-runtime-path fixtures "../../../test/integration/gccast")

;; text -> hash name -> normalized function text (";;" lines dropped).  A
;; function's chunk ends at its closing "}" and starts after the previous
;; one, so the pass messages / succs lines printed before a signature stay
;; with the function they belong to.
(define (split-functions text)
  (define lines (for/list ([l (string-split text "\n" #:trim? #f)] #:unless (string-prefix? l ";;")) l))
  (define chunks
    (let loop ([ls lines] [cur '()] [acc '()])
      (cond
        [(null? ls) (reverse (if (null? cur) acc (cons (reverse cur) acc)))]
        [(string=? (car ls) "}") (loop (cdr ls) '() (cons (reverse (cons "}" cur)) acc))]
        [else (loop (cdr ls) (cons (car ls) cur) acc)])))
  (for/hash ([c (in-list chunks)]
             #:when (for/or ([l (in-list c)]) (regexp-match? #px"^[A-Za-z_].*\\)\\s*$" l)))
    (define sig (for/first ([l (in-list c)] #:when (regexp-match? #px"^[A-Za-z_].*\\)\\s*$" l)) l))
    (define m (regexp-match #px"([A-Za-z_][A-Za-z0-9_]*) \\(" sig))
    (values (cadr m) (normalize-gimple-text (string-join c "\n")))))

;; per-stage dump texts of a TU: hash stage -> text
(define (o1-stage-texts tu)
  (define outs (for/hash ([s stages]) (values s (open-output-string))))
  (for/fold ([st (import-tu tu #:optimize 1)]) ([fn (in-list (TU-fns tu))])
    (define-values (decl body st1) (import-fn st fn))
    (define-values (body* st2) (c-genericize st1 decl body))
    (define-values (bind st3) (gimplify-function st2 decl body*))
    (print-function st3 decl bind (hash-ref outs 'gimple))
    (define-values (lowered locals st4) (lower-function st3 decl bind))
    (print-lowered-function st4 decl locals lowered (hash-ref outs 'lower))
    (define-values (cfg st5) (build-cfg st4 decl lowered #:cond->string print-cond-rhs))
    (print-cfg-function st5 decl locals cfg (hash-ref outs 'cfg))
    (define-values (cfg* st6) (build-ssa st5 decl cfg))
    ;; TODO_remove_unused_locals runs with pass_build_ssa; the optimizers keep the list
    (define locals-ssa (remove-unused-locals st6 cfg* locals))
    (print-ssa-function st6 decl locals-ssa cfg* (hash-ref outs 'ssa))
    (define-values (cfg-ccp st7) (ccp st6 decl cfg*))
    (print-ssa-function st7 decl locals-ssa cfg-ccp (hash-ref outs 'ccp1))
    st7)
  (for/hash ([(k o) (in-hash outs)]) (values k (get-output-string o))))

(module+ test
  (for* ([ver '("15.2.0" "12.5.0")]
         [name '("basic.c" "calls.c" "struct.c" "ccp.c")])
    (define tu (read-gccast-file (build-path fixtures (format "~a.~a.O1.dat" name ver))))
    (define texts (o1-stage-texts tu))
    (for ([stage stages])
      (test-case (format "~a ~a -O1: ~a dump aligns with GCC" name ver stage)
        (define ours (split-functions (hash-ref texts stage)))
        (define gcc (split-functions (file->string (build-path fixtures (format "~a.~a.O1.~a" name ver stage)))))
        (check-equal? (sort (hash-keys ours) string<?) (sort (hash-keys gcc) string<?))
        (for ([(fname g) (in-hash gcc)] #:when (hash-ref ours fname #f))
          (define o (hash-ref ours fname))
          (unless (string=? o g)
            (fail (format "mismatch (~a ~a -O1 ~a ~a)\n~a\n--- ours ---\n~a\n--- gcc ---\n~a" name ver stage fname (first-diff o g) o g))))))))
