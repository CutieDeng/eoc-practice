#lang racket/base

;; -O1 alignment: the plugin's -O1 GENERIC (<name>.<ver>.O1.dat) through the
;; same chain with GS-optimize = 1 must reproduce GCC's -O1 dumps of
;; gimplify / lower / cfg / ssa and then of every ported -O1 pass.  Functions are compared one by one (GCC
;; orders them by call-graph position when optimizing) with their SSA
;; versions and temporary numbers as printed: only the D.N uids are
;; renumbered (they start after GCC's builtin declarations).

(require rackunit racket/runtime-path racket/string racket/file racket/list
         "../../kernel/data/data.rkt"
         "../../kernel/ir/gimple/types.rkt"
         "../../kernel/ir/gccast/types.rkt"
         "../../frontend/gccast/reader.rkt"
         (only-in "gimplify-test.rkt" normalize-gimple-text first-diff)
         "state.rkt" "import.rkt" "genericize.rkt" "gimplify.rkt" "lower.rkt" "cfg.rkt" "ssa.rkt" "print.rkt"
         "opt/passes.rkt"
         "opt/ipa.rkt"
         "opt/inline.rkt"
         "opt/fixup-cfg.rkt")

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
    (values (cadr m) (normalize-gimple-text (string-join c "\n") #:rename-ssa? #f))))

;; per-stage dump texts of a TU: hash stage -> text.  The lowering
;; stages print their own dump format; every optimizer pass prints the
;; SSA-form dump with the local-decl list TODO_remove_unused_locals left
;; at pass_build_ssa (the early passes do not prune it).
(define (o1-stage-texts tu)
  (define outs (make-hasheq))
  (define (out s) (hash-ref! outs s open-output-string))
  ;; pass_build_ssa_passes runs for every function (callee first) before
  ;; pass_local_optimization_passes: a callee's pure/const result is not
  ;; known while its callers are gimplified and put into SSA
  ;; the front end parses and genericizes every function, then
  ;; pop_file_scope builds the TRANSLATION_UNIT_DECL (one decl uid), then
  ;; the cgraph analysis gimplifies and lowers
  (define-values (st-gen gens)
    (for/fold ([st (import-tu tu #:optimize 1)] [acc '()] #:result (values st (reverse acc))) ([fn (in-list (TU-fns tu))])
      (define-values (decl body st1) (import-fn st fn))
      (define-values (body* st2) (c-genericize st1 decl body))
      (values st2 (cons (cons decl body*) acc))))
  (define st-tu (let-values ([(_uid st) (fresh-uid st-gen)]) st))
  (define-values (st-ssa fns)
    (for/fold ([st st-tu] [acc '()] #:result (values st (reverse acc))) ([g (in-list gens)])
      (define decl (car g)) (define body* (cdr g))
      (define st2 st)
      (define-values (bind st3) (gimplify-function st2 decl body*))
      (print-function st3 decl bind (out 'gimple))
      (define-values (lowered locals st4) (lower-function st3 decl bind))
      (print-lowered-function st4 decl locals lowered (out 'lower))
      (define-values (cfg st5) (build-cfg st4 decl lowered #:cond->string print-cond-rhs))
      (print-cfg-function st5 decl locals cfg (out 'cfg))
      (define-values (cfg* st6) (build-ssa st5 decl cfg))
      (define locals-ssa (remove-unused-locals st6 cfg* locals))
      (print-ssa-function st6 decl locals-ssa cfg* (out 'ssa))
      (values st6 (cons (vector decl cfg* locals-ssa (GS-ssa-version st6)) acc))))
  (define-values (st-early fns-early)
    (for/fold ([st st-ssa] [acc '()] #:result (values st (reverse acc))) ([f (in-list fns)])
      (define decl (vector-ref f 0))
      (define st0 (gs-set-ssa-version (gs-set-fndecl st decl) (vector-ref f 3)))
      (define-values (cfg* st* locals*)
        (for/fold ([cfg (vector-ref f 1)] [st st0] [locals (vector-ref f 2)]) ([p (in-list (o1-passes st0))])
          (define-values (cfg1 st1) (run-pass p st decl cfg))
          (define locals1 (if (pass-prunes-locals? p) (remove-unused-locals st1 cfg1 locals) locals))
          (print-ssa-function st1 decl locals1 cfg1 (out (pass-name p)))
          (values cfg1 st1 locals1)))
      (values st* (cons (vector decl cfg* locals* (GS-ssa-version st*)) acc))))
  ;; the IPA stage, then all_optimizations per remaining function: the
  ;; inline transform (with its fixup / cleanup / remove_unused_locals
  ;; TODOs) precedes the first late pass
  (define-values (st-ipa fns-late plan) (ipa-stage st-early fns-early))
  (for/fold ([st st-ipa]) ([f (in-list fns-late)])
    (define decl (vector-ref f 0))
    (define st0 (gs-set-ssa-version (gs-set-fndecl st decl) (vector-ref f 3)))
    (define my-plan (hash-ref plan (decl-uid decl) #f))
    (define-values (cfg-i locals-i st-i)
      (cond
        [my-plan
         (define-values (c l s) (inline-calls st0 decl (vector-ref f 1) (vector-ref f 2) my-plan))
         (print-ssa-function s decl l c (out 'inline))      ; not a GCC dump: the transform's result for debugging
         (define-values (c2 s2) (fixup-cfg-pass s decl c))
         (values c2 (remove-unused-locals s2 c2 l) s2)]
        [else (values (vector-ref f 1) (vector-ref f 2) st0)]))
    (for/fold ([cfg cfg-i] [st st-i] [locals locals-i] #:result st)
              ([p (in-list (late-passes st-i))] #:when (pass-enabled? p st decl cfg))
      ;; the local decls, for passes that dump the function (dump_function_to_file)
      (define-values (cfg1 st1a) (run-pass p (set-decl-flag st decl 'local-decls locals) decl cfg))
      (define-values (locals0 st1) (take-added-locals st1a decl locals))
      (define locals1 (if (pass-prunes-locals? p) (remove-unused-locals st1 cfg1 locals0) locals0))
      (print-ssa-function st1 decl locals1 cfg1 (out (pass-name p)))
      (values cfg1 st1 locals1)))
  (for/hash ([(k o) (in-hash outs)]) (values k (get-output-string o))))

;; the stages compared, in dump order; a stage without a GCC dump file
;; for that version (a pass GCC does not run there) is skipped
(define lowering-stages '(gimple lower cfg ssa))
;; ealias at GCC 12 dumps the points-to constraints and sets, which are not reproduced
;; waccess2 dumps pointer_query cache counters that depend on GCC internals
(define (stage-excluded? ver stage)
  (or (and (memq stage '(ealias alias)) (string=? ver "12.5.0"))
      (eq? stage 'waccess2)))

(module+ test
  (for* ([ver '("15.2.0" "12.5.0")]
         [name '("basic.c" "calls.c" "struct.c" "ccp.c" "thread.c")])
    (define tu (read-gccast-file (build-path fixtures (format "~a.~a.O1.dat" name ver))))
    (define texts (o1-stage-texts tu))
    (define stages (append lowering-stages (map pass-name (o1-passes (import-tu tu #:optimize 1)))
                           (map pass-name (late-passes (import-tu tu #:optimize 1)))))
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
