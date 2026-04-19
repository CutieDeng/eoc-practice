#lang racket/base

;; ============================================================
;; Tests: RVSDG Region Builder — Kappa construction
;; ============================================================
;;
;; These tests exercise the Kappa-specific helpers in builder.rkt
;; (region-add-region-arg, region-add-region-result, build-handler-
;; region-entry, install-kappa-terminal) directly, with no Java
;; pipeline involvement.  They assert the kernel-level contract for
;; a terminal Kappa: N ctx inputs, 0 outputs; try-region opens with a
;; `Simple '(region-arg N)` producer; each handler-region opens with
;; a `Simple '(region-arg (+ N 1))` producer and surfaces the
;; exception-ref as the extra output.

(require rackunit
         "builder.rkt"
         "../../../kernel/ir/rvsdg/rvsdg.rkt"
         (except-in "../../../kernel/data/data.rkt" integer-compare))

(module+ test
  ;; Helpers reused across test cases.
  (define (node-count r)
    (ordered-map-count (Region-node->value r)))
  (define (node-ops r)
    (for/list ([kv (in-ordered-map (Region-node->value r))])
      (define v (cdr kv))
      (cond
        [(Simple? v) (Simple-op v)]
        [(Kappa? v) 'kappa]
        [(Throw? v) 'throw]
        [else (object-name v)])))
  (define (first-kappa r)
    (for/or ([kv (in-ordered-map (Region-node->value r))])
      (and (Kappa? (cdr kv)) (cdr kv))))

  ;; Build a terminated try-region.  If n-ctx >= 1, returns the first
  ;; ctx arg; if n-ctx = 0, installs a zero-arg 'return sink.  In
  ;; both shapes the region is internally terminated, matching the
  ;; terminal-Kappa contract.
  (define (mk-trivial-try-region n-ctx)
    (define r0 (region-empty))
    (define-values (r1 ctx-outs) (region-add-region-arg r0 n-ctx))
    (cond
      [(zero? n-ctx)
       (define-values (r2 _nid _ins _outs)
         (region-add-node r1 (Simple 'return) 0 0))
       r2]
      [else
       (define-values (r2 _nid ret-ins _outs)
         (region-add-node r1 (Simple 'return) 1 0))
       (define-values (r3 _w)
         (region-add-wire r2 (pvector-ref ctx-outs 0) (pvector-ref ret-ins 0)))
       r3]))

  ;; Build a handler-region that throws its received exception-ref.
  (define (mk-throw-handler-region n-ctx)
    (define-values (r1 _ctx-outs exn-out) (build-handler-region-entry n-ctx))
    (define-values (r2 _nid thr-ins _outs)
      (region-add-node r1 (Throw #f) 1 0))
    (define-values (r3 _w)
      (region-add-wire r2 exn-out (pvector-ref thr-ins 0)))
    r3)

  ;; ----- region-arg / region-result primitives -----
  (test-case "region-add-region-arg allocates a Simple '(region-arg N) producer"
    (define r0 (region-empty))
    (define-values (r1 outs) (region-add-region-arg r0 3))
    (check-equal? (pvector-length outs) 3)
    (check-equal? (node-count r1) 1)
    (define values-list
      (for/list ([kv (in-ordered-map (Region-node->value r1))]) (cdr kv)))
    (check-equal? (length values-list) 1)
    (check-pred Simple? (car values-list))
    (check-equal? (Simple-op (car values-list)) '(region-arg 3)))

  (test-case "region-add-region-result allocates a Simple '(region-result M) consumer"
    (define r0 (region-empty))
    (define-values (r1 ins) (region-add-region-result r0 2))
    (check-equal? (pvector-length ins) 2)
    (check-equal? (node-count r1) 1)
    (define v
      (cdar (for/list ([kv (in-ordered-map (Region-node->value r1))]) kv)))
    (check-pred Simple? v)
    (check-equal? (Simple-op v) '(region-result 2)))

  ;; ----- handler-region entry -----
  (test-case "build-handler-region-entry surfaces N ctx outputs + exception-ref"
    (define-values (r ctx-outs exn-out) (build-handler-region-entry 2))
    (check-equal? (pvector-length ctx-outs) 2)
    (check-pred OutputId? exn-out)
    ;; The exn output is the last of the region-arg's N+1 outputs.
    (define arg-node-op
      (for/or ([kv (in-ordered-map (Region-node->value r))])
        (define v (cdr kv))
        (and (Simple? v) (Simple-op v))))
    (check-equal? arg-node-op '(region-arg 3))
    ;; ctx outs and exn out are all distinct.
    (define all-ids
      (append (for/list ([o (in-pvector ctx-outs)]) (OutputId-id o))
              (list (OutputId-id exn-out))))
    (check-equal? (length (remove-duplicates all-ids)) 3))

  (test-case "build-handler-region-entry with n-ctx=0 still yields an exn output"
    (define-values (r ctx-outs exn-out) (build-handler-region-entry 0))
    (check-equal? (pvector-length ctx-outs) 0)
    (check-pred OutputId? exn-out)
    (define arg-node-op
      (for/or ([kv (in-ordered-map (Region-node->value r))])
        (define v (cdr kv))
        (and (Simple? v) (Simple-op v))))
    (check-equal? arg-node-op '(region-arg 1)))

  ;; ----- terminal Kappa end-to-end -----
  (test-case "install-kappa-terminal: single-handler terminal Kappa wires correctly"
    ;; Parent region has one upstream value (a synthetic 'param
    ;; producer) that feeds both the try's ctx and is reachable by
    ;; the (unused) handler.  Try-region just returns it; handler
    ;; rethrows the exception-ref.
    (define parent0 (region-empty))
    (define-values (parent1 _pid _pins param-outs)
      (region-add-node parent0 (Simple 'param) 0 1))
    (define ctx-oid (pvector-ref param-outs 0))
    (define try-r (mk-trivial-try-region 1))
    (define handler-r (mk-throw-handler-region 1))
    (define handlers
      (pvector-cons-right (pvector-empty)
                          (cons "java.lang.RuntimeException" handler-r)))
    (define-values (parent2 knid)
      (install-kappa-terminal parent1
                              (pvector-cons-right (pvector-empty) ctx-oid)
                              try-r
                              handlers))
    ;; Kappa node exists in parent, with exactly 1 input and 0 outputs.
    (define kappa (first-kappa parent2))
    (check-pred Kappa? kappa)
    (define kin-info (ordered-map-ref (Region-node->input parent2) knid))
    (define kout-info (ordered-map-ref (Region-node->output parent2) knid))
    (check-equal? (cdr kin-info) 1 "terminal Kappa should take N=1 ctx input")
    (check-equal? (cdr kout-info) 0 "terminal Kappa should have 0 outputs")
    ;; Handlers field carries the catch-type.
    (define hs (Kappa-handlers kappa))
    (check-equal? (pvector-length hs) 1)
    (check-equal? (car (pvector-ref hs 0)) "java.lang.RuntimeException")
    (check-eq? (cdr (pvector-ref hs 0)) handler-r)
    ;; try-region reference preserved.
    (check-eq? (Kappa-try-region kappa) try-r)
    ;; Parent wire count: one wire from param output to Kappa's
    ;; single input port.
    (check-equal? (ordered-map-count (Region-wire->input parent2)) 1))

  (test-case "install-kappa-terminal: handler list order preserved (catch-all last)"
    ;; Two handlers, catch-all last.  Asserts pvector order survives
    ;; through the Kappa struct.
    (define try-r (mk-trivial-try-region 0))
    (define h1 (mk-throw-handler-region 0))
    (define h2 (mk-throw-handler-region 0))
    (define handlers
      (let* ([p0 (pvector-empty)]
             [p1 (pvector-cons-right p0 (cons "java.io.IOException" h1))]
             [p2 (pvector-cons-right p1 (cons #f h2))])
        p2))
    (define parent0 (region-empty))
    (define-values (parent1 knid)
      (install-kappa-terminal parent0 (pvector-empty) try-r handlers))
    (define kappa (first-kappa parent1))
    (check-pred Kappa? kappa)
    (define hs (Kappa-handlers kappa))
    (check-equal? (pvector-length hs) 2)
    (check-equal? (car (pvector-ref hs 0)) "java.io.IOException")
    (check-false  (car (pvector-ref hs 1))
                  "catch-all is signalled by #f catch-type"))

  ;; Build a try-region that falls through with M region-result inputs
  ;; wired from its N ctx region-arg outputs.  Requires M <= N so we can
  ;; wire result-ins[i] <- ctx-outs[i]; sufficient for the current tests.
  (define (mk-convergent-try-region n-ctx m-out)
    (define r0 (region-empty))
    (define-values (r1 ctx-outs) (region-add-region-arg r0 n-ctx))
    (define-values (r2 res-ins) (region-add-region-result r1 m-out))
    (for/fold ([r r2])
              ([i (in-range m-out)])
      (define-values (r* _w)
        (region-add-wire r (pvector-ref ctx-outs i) (pvector-ref res-ins i)))
      r*))

  ;; Build a convergent handler-region that feeds its ctx args (and
  ;; ignores the exception-ref) through to its region-result sink.
  (define (mk-convergent-handler-region n-ctx m-out)
    (define-values (r1 ctx-outs _exn-out) (build-handler-region-entry n-ctx))
    (define-values (r2 res-ins) (region-add-region-result r1 m-out))
    (for/fold ([r r2])
              ([i (in-range m-out)])
      (define-values (r* _w)
        (region-add-wire r (pvector-ref ctx-outs i) (pvector-ref res-ins i)))
      r*))

  (test-case "install-kappa-convergent: allocates M outputs and wires N ctx inputs"
    (define n-ctx 2)
    (define m-out 2)
    (define try-r (mk-convergent-try-region n-ctx m-out))
    (define handler-r (mk-convergent-handler-region n-ctx m-out))
    (define handlers
      (pvector-cons-right (pvector-empty)
                          (cons "java.lang.Exception" handler-r)))
    (define parent0 (region-empty))
    (define-values (parent1 _pid _pins param-outs)
      (region-add-node parent0 (Simple 'param) 0 n-ctx))
    (define ctx-oids
      (for/pvector ([o (in-pvector param-outs)]) o))
    (define-values (parent2 knid out-oids)
      (install-kappa-convergent parent1 ctx-oids try-r handlers m-out))
    (define kappa (first-kappa parent2))
    (check-pred Kappa? kappa)
    (define kin-info (ordered-map-ref (Region-node->input parent2) knid))
    (define kout-info (ordered-map-ref (Region-node->output parent2) knid))
    (check-equal? (cdr kin-info) n-ctx "convergent Kappa should take N ctx inputs")
    (check-equal? (cdr kout-info) m-out "convergent Kappa should surface M outputs")
    (check-equal? (pvector-length out-oids) m-out)
    (for ([o (in-pvector out-oids)]) (check-pred OutputId? o))
    ;; N wires from param to Kappa's ctx ports.
    (check-equal? (ordered-map-count (Region-wire->input parent2)) n-ctx))

  (test-case "install-kappa-convergent: zero-ctx still produces M outputs"
    (define m-out 1)
    (define try-r
      (let-values ([(r _ins) (region-add-region-result
                              (let-values ([(r1 _outs)
                                            (region-add-region-arg
                                             (region-empty) 0)])
                                r1)
                              m-out)])
        ;; The region-result's one input is left unwired; parent-side
        ;; shape is still valid for the purposes of this builder test.
        r))
    (define handler-r
      (let-values ([(r1 _ctx-outs _exn-out) (build-handler-region-entry 0)])
        (let-values ([(r2 _ins) (region-add-region-result r1 m-out)])
          r2)))
    (define handlers
      (pvector-cons-right (pvector-empty) (cons #f handler-r)))
    (define parent0 (region-empty))
    (define-values (parent1 knid out-oids)
      (install-kappa-convergent parent0 (pvector-empty) try-r handlers m-out))
    (check-equal? (pvector-length out-oids) m-out)
    (define kin-info (ordered-map-ref (Region-node->input parent1) knid))
    (check-equal? (cdr kin-info) 0
                  "zero-ctx convergent Kappa should have 0 input ports")
    (check-equal? (ordered-map-count (Region-wire->input parent1)) 0))

  (test-case "install-kappa-terminal: zero-ctx Kappa is permitted"
    (define try-r (mk-trivial-try-region 0))
    (define handler-r (mk-throw-handler-region 0))
    (define handlers
      (pvector-cons-right (pvector-empty) (cons #f handler-r)))
    (define parent0 (region-empty))
    (define-values (parent1 knid)
      (install-kappa-terminal parent0 (pvector-empty) try-r handlers))
    (define kin-info (ordered-map-ref (Region-node->input parent1) knid))
    (check-equal? (cdr kin-info) 0
                  "Kappa with no ctx vars should have 0 input ports")
    (check-equal? (ordered-map-count (Region-wire->input parent1)) 0)))

;; remove-duplicates is not re-exported by kernel/data; inline it.
(define (remove-duplicates xs)
  (let loop ([xs xs] [seen '()])
    (cond
      [(null? xs) '()]
      [(member (car xs) seen) (loop (cdr xs) seen)]
      [else (cons (car xs) (loop (cdr xs) (cons (car xs) seen)))])))
