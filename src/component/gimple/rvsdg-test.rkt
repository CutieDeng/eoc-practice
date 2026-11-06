#lang racket/base

;; GCfg -> RVSDG through the shared structurizer: every function of the C
;; fixtures lowers to a Lambda with the expected Gamma / Theta structure,
;; and the result is a datum.

(require rackunit racket/runtime-path racket/list
         "../../kernel/data/data.rkt"
         "../../kernel/ir/rvsdg/rvsdg.rkt"
         "../../kernel/ir/gimple/datum.rkt"
         "../../kernel/ir/gccast/types.rkt"
         "../../frontend/gccast/reader.rkt"
         (only-in "ssa-test.rkt" ssa-fn)
         "state.rkt" "import.rkt" "rvsdg.rkt")

(define-runtime-path fixtures "../../../test/integration/gccast")

;; (name . Lambda) for every function of a fixture
(define (lower-tu tu)
  (for/fold ([st (import-tu tu)] [acc '()] #:result (reverse acc)) ([fn (in-list (TU-fns tu))])
    (define-values (decl cfg locals st1) (ssa-fn st fn))
    (values st1 (cons (cons (Fn-name fn) (gimple->rvsdg st1 decl cfg)) acc))))

;; count node kinds over all nested regions
(define (count-kinds r)
  (define counts (make-hasheq))
  (let walk ([r r])
    (for ([kv (in-idmap (Region-node->value r))])
      (define v (cdr kv))
      (cond
        [(Gamma? v) (hash-update! counts 'gamma add1 0) (for ([sub (let ([rs (Gamma-regions v)]) (if (pvector? rs) (in-pvector rs) (in-list rs)))]) (walk sub))]
        [(Theta? v) (hash-update! counts 'theta add1 0) (walk (Theta-region v))]
        [(Simple? v) (hash-update! counts (if (pair? (Simple-op v)) (car (Simple-op v)) (Simple-op v)) add1 0)]
        [else (hash-update! counts 'other add1 0)])))
  counts)

(module+ test
  (for ([ver '("15.2.0" "12.5.0")])
    (define (lams name) (lower-tu (read-gccast-file (build-path fixtures (format "~a.~a.dat" name ver)))))
    (test-case (format "basic.c ~a: if/else -> Gamma, for loop -> Theta" ver)
      (define l (lams "basic.c"))
      (check-equal? (map car l) '("f" "g"))
      (for ([p l]) (check-pred Lambda? (cdr p)))
      (define kf (count-kinds (Lambda-region (cdr (first l)))))
      (define kg (count-kinds (Lambda-region (cdr (second l)))))
      (check-equal? (hash-ref kf 'gamma 0) 1)
      (check-equal? (hash-ref kf 'theta 0) 0)
      (check-equal? (hash-ref kg 'theta 0) 1)
      (check-true (>= (hash-ref kf 'return 0) 1)))
    ;; Two shapes the structurizer (component/rvsdg/structurize.rkt) does not
    ;; handle yet; the C fixtures expose them.  They are asserted here so the
    ;; test flips when the structurizer learns them:
    ;;   calls.c use : short-circuit ladder (`a > 0 && b < 10`, `a == 0 || !p`):
    ;;                 the else target is also reached from inside the then arm
    ;;   struct.c sum: `break` inside a while body (two loop exits joining)
    (test-case (format "calls.c ~a: twice lowers; the short-circuit ladder in use is a known structurizer gap" ver)
      (define tu (read-gccast-file (build-path fixtures (format "calls.c.~a.dat" ver))))
      (define st0 (import-tu tu))
      (define-values (d1 cfg1 l1 st1) (ssa-fn st0 (first (TU-fns tu))))
      (define kt (count-kinds (Lambda-region (gimple->rvsdg st1 d1 cfg1))))
      (check-equal? (hash-ref kt 'gamma 0) 0)
      (check-true (>= (hash-ref kt 'gimple 0) 1))
      (define-values (d2 cfg2 l2 st2) (ssa-fn st1 (second (TU-fns tu))))
      (check-exn #rx"pick-phi-source" (lambda () (gimple->rvsdg st2 d2 cfg2))))
    (test-case (format "struct.c ~a: break inside a loop is a known structurizer gap" ver)
      (define tu (read-gccast-file (build-path fixtures (format "struct.c.~a.dat" ver))))
      (define-values (d cfg l st) (ssa-fn (import-tu tu) (first (TU-fns tu))))
      (check-exn #rx"unresolved phi" (lambda () (gimple->rvsdg st d cfg))))
    (test-case (format "basic.c ~a: every Lambda is a datum" ver)
      (for ([p (lams "basic.c")])
        (define d (value->datum (cdr p)))
        (check-equal? (datum->value (read (open-input-string (format "~s" d)))) (cdr p))))))
