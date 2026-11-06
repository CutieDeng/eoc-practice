#lang racket/base

;; S1 acceptance: every fixture imports into Tree values + GS, and the
;; result survives a datum round trip (write -> read -> datum->value).

(require rackunit racket/runtime-path racket/string
         "../../kernel/data/data.rkt"
         "../../kernel/ir/gimple/types.rkt"
         "../../kernel/ir/gimple/datum.rkt"
         "../../kernel/ir/gccast/types.rkt"
         "../../frontend/gccast/reader.rkt"
         "state.rkt" "tree.rkt" "import.rkt")

(define-runtime-path fixtures "../../../test/integration/gccast")

(define (round-trip v)
  (define s (let ([o (open-output-string)]) (write (value->datum v) o) (get-output-string o)))
  (check-false (regexp-match? #rx"#pvector|#<" s) "datum must be readable")
  (datum->value (read (open-input-string s))))

(module+ test
  (for* ([ver '("15.2.0" "12.5.0")]
         [name '("basic.c" "calls.c" "struct.c" "locals.c" "method.cc")])
    (test-case (format "~a ~a: import + datum round trip" name ver)
      (define tu (read-gccast-file (build-path fixtures (format "~a.~a.dat" name ver))))
      (define st0 (import-tu tu))
      (check-true (> (intmap-count (GS-types st0)) 0))
      (check-true (exact-integer? (void-type st0)))
      (check-true (boolean-type-p st0 (boolean-type st0)))
      (check-true (pointer-type-p st0 (ptr-type st0)))
      (for ([fn (in-list (TU-fns tu))])
        (define-values (decl body st) (import-fn st0 fn))
        (check-eq? (tree-code decl) 'function_decl)
        (check-true (string? (decl-name decl)))
        (check-true (decl-eq? decl (GS-fndecl st)))
        (when body (check-true (symbol? (tree-code body))))
        (define v (list decl body st))
        (check-equal? (round-trip v) v))))

  (test-case "builders allocate through the state"
    (define tu (read-gccast-file (build-path fixtures "basic.c.15.2.0.dat")))
    (define st0 (import-tu tu))
    (define-values (decl body st1) (import-fn st0 (car (TU-fns tu))))
    (define-values (l1 st2) (create-artificial-label st1))
    (define-values (l2 st3) (create-artificial-label st2))
    (check-false (decl-eq? l1 l2))
    (check-equal? (GS-next-uid st3) (+ 2 (GS-next-uid st1)))
    (define-values (tmp st4) (create-tmp-var-raw st3 (size-type st3) "iftmp"))
    (check-equal? (decl-name tmp) "iftmp.0")
    (define-values (ssa st5) (make-ssa-name st4 (size-type st4)))
    (check-equal? (ssa-name-version ssa) 1)
    (define-values (p st6) (pointer-to st5 (size-type st5)))
    (define-values (p2 st7) (pointer-to st6 (size-type st6)))
    (check-eqv? p p2)
    (check-true (pointer-type-p st7 p))
    (check-eqv? (type-type st7 p) (size-type st7))
    (define zero (build-int-cst st7 (size-type st7) 0))
    (check-true (integer-zerop zero))
    (check-equal? (integer-cst-value (fold-build2 st7 'plus_expr (size-type st7) zero (build-int-cst st7 (size-type st7) 5))) 5)
    ;; decl flags live in the state, not in the value
    (define st8 (set-decl-flag st7 tmp 'addressable #t))
    (check-true (decl-flag st8 tmp 'addressable))
    (check-false (decl-flag st7 tmp 'addressable))
    (check-false (tree-attr tmp 'addressable))))
