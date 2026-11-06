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
         "opt/passes.rkt")

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
  ;; the function signature is the line before the body's opening brace
  (define (signature c)
    (let loop ([ls c] [prev #f])
      (cond
        [(null? ls) #f]
        [(string=? (car ls) "{") prev]
        [else (loop (cdr ls) (car ls))])))
  (for/hash ([c (in-list chunks)]
             #:when (let ([sig (signature c)]) (and sig (regexp-match? #px"([A-Za-z_][A-Za-z0-9_]*) \\(" sig))))
    (define m (regexp-match #px"([A-Za-z_][A-Za-z0-9_]*) \\(" (signature c)))
    (values (cadr m) (normalize-gimple-text (string-join c "\n")))))

;; per-stage dump texts of a TU: hash stage -> text.  The lowering
;; stages print their own dump format; every optimizer pass prints the
;; SSA-form dump with the local-decl list TODO_remove_unused_locals left
;; at pass_build_ssa (the early passes do not prune it).
(define (o1-stage-texts tu)
  (define outs (make-hasheq))
  (define (out s) (hash-ref! outs s open-output-string))
  (for/fold ([st (import-tu tu #:optimize 1)]) ([fn (in-list (TU-fns tu))])
    (define-values (decl body st1) (import-fn st fn))
    (define-values (body* st2) (c-genericize st1 decl body))
    (define-values (bind st3) (gimplify-function st2 decl body*))
    (print-function st3 decl bind (out 'gimple))
    (define-values (lowered locals st4) (lower-function st3 decl bind))
    (print-lowered-function st4 decl locals lowered (out 'lower))
    (define-values (cfg st5) (build-cfg st4 decl lowered #:cond->string print-cond-rhs))
    (print-cfg-function st5 decl locals cfg (out 'cfg))
    (define-values (cfg* st6) (build-ssa st5 decl cfg))
    (define locals-ssa (remove-unused-locals st6 cfg* locals))
    (print-ssa-function st6 decl locals-ssa cfg* (out 'ssa))
    (for/fold ([cfg cfg*] [st st6] [locals locals-ssa] #:result st) ([p (in-list (o1-passes st6))])
      (define-values (cfg1 st1) ((pass-fn p) st decl cfg))
      (define locals1 (if (pass-prunes-locals? p) (remove-unused-locals st1 cfg1 locals) locals))
      (print-ssa-function st1 decl locals1 cfg1 (out (pass-name p)))
      (values cfg1 st1 locals1)))
  (for/hash ([(k o) (in-hash outs)]) (values k (get-output-string o))))

;; the stages compared, in dump order; a stage without a GCC dump file
;; for that version (a pass GCC does not run there) is skipped
(define lowering-stages '(gimple lower cfg ssa))
;; ealias at GCC 12 dumps the points-to constraints and sets, which are not reproduced
(define (stage-excluded? ver stage) (and (eq? stage 'ealias) (string=? ver "12.5.0")))

(module+ test
  (for* ([ver '("15.2.0" "12.5.0")]
         [name '("basic.c" "calls.c" "struct.c" "ccp.c")])
    (define tu (read-gccast-file (build-path fixtures (format "~a.~a.O1.dat" name ver))))
    (define texts (o1-stage-texts tu))
    (define stages (append lowering-stages (map pass-name (o1-passes (import-tu tu #:optimize 1)))))
    (for ([stage stages]
          #:unless (stage-excluded? ver stage)
          #:when (file-exists? (build-path fixtures (format "~a.~a.O1.~a" name ver stage))))
      (test-case (format "~a ~a -O1: ~a dump aligns with GCC" name ver stage)
        (define ours (split-functions (hash-ref texts stage)))
        (define gcc (split-functions (file->string (build-path fixtures (format "~a.~a.O1.~a" name ver stage)))))
        (check-equal? (sort (hash-keys ours) string<?) (sort (hash-keys gcc) string<?))
        (for ([(fname g) (in-hash gcc)] #:when (hash-ref ours fname #f))
          (define o (hash-ref ours fname))
          (unless (string=? o g)
            (fail (format "mismatch (~a ~a -O1 ~a ~a)\n~a\n--- ours ---\n~a\n--- gcc ---\n~a" name ver stage fname (first-diff o g) o g))))))))
