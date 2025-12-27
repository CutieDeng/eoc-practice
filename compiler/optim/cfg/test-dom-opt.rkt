#lang racket/base

;; ============================================================
;; Tests for Dominator-based Optimization
;; ============================================================

(require rackunit racket/match racket/list)
(require "dom-opt.rkt")
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")

;; ============================================================
;; Helper Functions
;; ============================================================

(define test-count 0)

(define (count-blocks cfg)
  (length (cfg-all-block-ids cfg)))

(define (get-terminator cfg bid)
  (define block (cfg-get-block cfg bid))
  (and block (CfgBlock-terminator block)))

(define (is-unconditional-jump? term)
  (TermJump? term))

(define (is-conditional-branch? term)
  (TermBranch? term))

;; ============================================================
;; Test 1: Nested condition simplification
;; ============================================================
;; entry: if (x) goto A else goto B
;; A: if (x) goto C else goto D  <- x is known true in A
;; After optimization: A: goto C

(define (test-nested-condition)
  (printf "Test ~a: Nested condition simplification... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (a-bid cfg2) (cfg-create-block cfg1))
  (define-values (b-bid cfg3) (cfg-create-block cfg2))
  (define-values (c-bid cfg4) (cfg-create-block cfg3))
  (define-values (d-bid cfg5) (cfg-create-block cfg4))

  (define cfg6 (cfg-set-entry cfg5 entry-bid))

  (define v-x (VarId 'x))

  ;; entry: if (x) goto A else goto B
  (define cfg7
    (cfg-block-set-terminator cfg6 entry-bid
      (TermBranch v-x a-bid b-bid)))

  ;; A: if (x) goto C else goto D (redundant - x is true here)
  (define cfg8
    (cfg-block-set-terminator cfg7 a-bid
      (TermBranch v-x c-bid d-bid)))

  ;; B, C, D: return
  (define cfg9
    (cfg-block-set-terminator cfg8 b-bid
      (TermReturn '())))
  (define cfg10
    (cfg-block-set-terminator cfg9 c-bid
      (TermReturn '())))
  (define cfg11
    (cfg-block-set-terminator cfg10 d-bid
      (TermReturn '())))

  (define result (cfg-dom-opt cfg11))

  ;; A should now be unconditional jump to C
  (define a-term (get-terminator result a-bid))
  (check-true (is-unconditional-jump? a-term)
              "A should be unconditional jump")
  (check-equal? (TermJump-target a-term) c-bid
                "A should jump to C")

  (printf "passed~n"))

;; ============================================================
;; Test 2: False branch simplification
;; ============================================================
;; entry: if (x) goto A else goto B
;; B: if (x) goto C else goto D  <- x is known false in B
;; After optimization: B: goto D

(define (test-false-branch)
  (printf "Test ~a: False branch simplification... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (a-bid cfg2) (cfg-create-block cfg1))
  (define-values (b-bid cfg3) (cfg-create-block cfg2))
  (define-values (c-bid cfg4) (cfg-create-block cfg3))
  (define-values (d-bid cfg5) (cfg-create-block cfg4))

  (define cfg6 (cfg-set-entry cfg5 entry-bid))

  (define v-x (VarId 'x))

  ;; entry: if (x) goto A else goto B
  (define cfg7
    (cfg-block-set-terminator cfg6 entry-bid
      (TermBranch v-x a-bid b-bid)))

  ;; A: return
  (define cfg8
    (cfg-block-set-terminator cfg7 a-bid
      (TermReturn '())))

  ;; B: if (x) goto C else goto D (x is false here)
  (define cfg9
    (cfg-block-set-terminator cfg8 b-bid
      (TermBranch v-x c-bid d-bid)))

  ;; C, D: return
  (define cfg10
    (cfg-block-set-terminator cfg9 c-bid
      (TermReturn '())))
  (define cfg11
    (cfg-block-set-terminator cfg10 d-bid
      (TermReturn '())))

  (define result (cfg-dom-opt cfg11))

  ;; B should now be unconditional jump to D
  (define b-term (get-terminator result b-bid))
  (check-true (is-unconditional-jump? b-term))
  (check-equal? (TermJump-target b-term) d-bid)

  (printf "passed~n"))

;; ============================================================
;; Test 3: Deep nesting
;; ============================================================
;; entry: if (x) goto A else goto B
;; A: if (y) goto C else goto D
;; C: if (x) goto E else goto F  <- x is still known true
;; After: C: goto E

(define (test-deep-nesting)
  (printf "Test ~a: Deep nesting... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (a-bid cfg2) (cfg-create-block cfg1))
  (define-values (b-bid cfg3) (cfg-create-block cfg2))
  (define-values (c-bid cfg4) (cfg-create-block cfg3))
  (define-values (d-bid cfg5) (cfg-create-block cfg4))
  (define-values (e-bid cfg6) (cfg-create-block cfg5))
  (define-values (f-bid cfg7) (cfg-create-block cfg6))

  (define cfg8 (cfg-set-entry cfg7 entry-bid))

  (define v-x (VarId 'x))
  (define v-y (VarId 'y))

  ;; entry: if (x) goto A else goto B
  (define cfg9
    (cfg-block-set-terminator cfg8 entry-bid
      (TermBranch v-x a-bid b-bid)))

  ;; A: if (y) goto C else goto D
  (define cfg10
    (cfg-block-set-terminator cfg9 a-bid
      (TermBranch v-y c-bid d-bid)))

  ;; B, D: return
  (define cfg11
    (cfg-block-set-terminator cfg10 b-bid
      (TermReturn '())))
  (define cfg12
    (cfg-block-set-terminator cfg11 d-bid
      (TermReturn '())))

  ;; C: if (x) goto E else goto F (x is still known true from entry)
  (define cfg13
    (cfg-block-set-terminator cfg12 c-bid
      (TermBranch v-x e-bid f-bid)))

  ;; E, F: return
  (define cfg14
    (cfg-block-set-terminator cfg13 e-bid
      (TermReturn '())))
  (define cfg15
    (cfg-block-set-terminator cfg14 f-bid
      (TermReturn '())))

  (define result (cfg-dom-opt cfg15))

  ;; C should now be unconditional jump to E
  (define c-term (get-terminator result c-bid))
  (check-true (is-unconditional-jump? c-term))
  (check-equal? (TermJump-target c-term) e-bid)

  (printf "passed~n"))

;; ============================================================
;; Test 4: No optimization - unknown condition
;; ============================================================
;; entry: if (x) goto A else goto B
;; A: if (y) goto C else goto D  <- y is unknown
;; No change expected

(define (test-no-opt-unknown)
  (printf "Test ~a: No optimization for unknown condition... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (a-bid cfg2) (cfg-create-block cfg1))
  (define-values (b-bid cfg3) (cfg-create-block cfg2))
  (define-values (c-bid cfg4) (cfg-create-block cfg3))
  (define-values (d-bid cfg5) (cfg-create-block cfg4))

  (define cfg6 (cfg-set-entry cfg5 entry-bid))

  (define v-x (VarId 'x))
  (define v-y (VarId 'y))

  (define cfg7
    (cfg-block-set-terminator cfg6 entry-bid
      (TermBranch v-x a-bid b-bid)))

  ;; A: if (y) - y is unknown
  (define cfg8
    (cfg-block-set-terminator cfg7 a-bid
      (TermBranch v-y c-bid d-bid)))

  (define cfg9
    (cfg-block-set-terminator cfg8 b-bid
      (TermReturn '())))
  (define cfg10
    (cfg-block-set-terminator cfg9 c-bid
      (TermReturn '())))
  (define cfg11
    (cfg-block-set-terminator cfg10 d-bid
      (TermReturn '())))

  (define result (cfg-dom-opt cfg11))

  ;; A should still be conditional
  (define a-term (get-terminator result a-bid))
  (check-true (is-conditional-branch? a-term))

  (printf "passed~n"))

;; ============================================================
;; Test 5: With statistics
;; ============================================================

(define (test-with-stats)
  (printf "Test ~a: With statistics... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (a-bid cfg2) (cfg-create-block cfg1))
  (define-values (b-bid cfg3) (cfg-create-block cfg2))
  (define-values (c-bid cfg4) (cfg-create-block cfg3))
  (define-values (d-bid cfg5) (cfg-create-block cfg4))

  (define cfg6 (cfg-set-entry cfg5 entry-bid))

  (define v-x (VarId 'x))

  (define cfg7
    (cfg-block-set-terminator cfg6 entry-bid
      (TermBranch v-x a-bid b-bid)))

  (define cfg8
    (cfg-block-set-terminator cfg7 a-bid
      (TermBranch v-x c-bid d-bid)))

  (define cfg9
    (cfg-block-set-terminator cfg8 b-bid
      (TermReturn '())))
  (define cfg10
    (cfg-block-set-terminator cfg9 c-bid
      (TermReturn '())))
  (define cfg11
    (cfg-block-set-terminator cfg10 d-bid
      (TermReturn '())))

  (define-values (result stats) (cfg-dom-opt-with-stats cfg11))
  (check-not-false (assq 'simplifications stats))
  (check-true (>= (cdr (assq 'simplifications stats)) 1))

  (printf "passed~n"))

;; ============================================================
;; Test 6: Multiple conditions
;; ============================================================
;; entry: if (x) goto A else goto B
;; A: if (y) goto C else goto D
;; C: if (x) goto E else goto F  <- x=true
;; C: if (y) goto G else goto H  <- y=true (after the above)
;; Note: can only simplify x in C, as y's value depends on path

(define (test-multiple-conditions)
  (printf "Test ~a: Multiple conditions... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (a-bid cfg2) (cfg-create-block cfg1))
  (define-values (b-bid cfg3) (cfg-create-block cfg2))
  (define-values (c-bid cfg4) (cfg-create-block cfg3))
  (define-values (d-bid cfg5) (cfg-create-block cfg4))

  (define cfg6 (cfg-set-entry cfg5 entry-bid))

  (define v-x (VarId 'x))
  (define v-y (VarId 'y))

  (define cfg7
    (cfg-block-set-terminator cfg6 entry-bid
      (TermBranch v-x a-bid b-bid)))

  ;; In A, x is known true
  ;; A: if (y) goto C else goto D
  (define cfg8
    (cfg-block-set-terminator cfg7 a-bid
      (TermBranch v-y c-bid d-bid)))

  ;; In C, x is true AND y is true
  ;; C: if (x) -> should be simplified since x=true
  (define cfg9
    (cfg-block-set-terminator cfg8 c-bid
      (TermBranch v-x d-bid b-bid)))  ; if (x) goto D else goto B

  (define cfg10
    (cfg-block-set-terminator cfg9 b-bid
      (TermReturn '())))
  (define cfg11
    (cfg-block-set-terminator cfg10 d-bid
      (TermReturn '())))

  (define result (cfg-dom-opt cfg11))

  ;; C should be simplified (x is known true)
  (define c-term (get-terminator result c-bid))
  (check-true (is-unconditional-jump? c-term))
  (check-equal? (TermJump-target c-term) d-bid)

  (printf "passed~n"))

;; ============================================================
;; Test 7: Entry block - no optimization
;; ============================================================

(define (test-entry-no-opt)
  (printf "Test ~a: Entry block - no optimization... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (a-bid cfg2) (cfg-create-block cfg1))
  (define-values (b-bid cfg3) (cfg-create-block cfg2))

  (define cfg4 (cfg-set-entry cfg3 entry-bid))

  (define v-x (VarId 'x))

  ;; entry: if (x) - x is unknown at entry
  (define cfg5
    (cfg-block-set-terminator cfg4 entry-bid
      (TermBranch v-x a-bid b-bid)))

  (define cfg6
    (cfg-block-set-terminator cfg5 a-bid
      (TermReturn '())))
  (define cfg7
    (cfg-block-set-terminator cfg6 b-bid
      (TermReturn '())))

  (define result (cfg-dom-opt cfg7))

  ;; Entry should remain conditional
  (define entry-term (get-terminator result entry-bid))
  (check-true (is-conditional-branch? entry-term))

  (printf "passed~n"))

;; ============================================================
;; Test 8: Chain of same condition
;; ============================================================
;; entry: if (x) goto A else goto B
;; A: if (x) goto C else goto D  <- simplified
;; C: if (x) goto E else goto F  <- simplified
;; All should become unconditional

(define (test-chain-same-condition)
  (printf "Test ~a: Chain of same condition... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (a-bid cfg2) (cfg-create-block cfg1))
  (define-values (b-bid cfg3) (cfg-create-block cfg2))
  (define-values (c-bid cfg4) (cfg-create-block cfg3))
  (define-values (d-bid cfg5) (cfg-create-block cfg4))
  (define-values (e-bid cfg6) (cfg-create-block cfg5))
  (define-values (f-bid cfg7) (cfg-create-block cfg6))

  (define cfg8 (cfg-set-entry cfg7 entry-bid))

  (define v-x (VarId 'x))

  (define cfg9
    (cfg-block-set-terminator cfg8 entry-bid
      (TermBranch v-x a-bid b-bid)))

  (define cfg10
    (cfg-block-set-terminator cfg9 a-bid
      (TermBranch v-x c-bid d-bid)))

  (define cfg11
    (cfg-block-set-terminator cfg10 c-bid
      (TermBranch v-x e-bid f-bid)))

  (define cfg12
    (cfg-block-set-terminator cfg11 b-bid
      (TermReturn '())))
  (define cfg13
    (cfg-block-set-terminator cfg12 d-bid
      (TermReturn '())))
  (define cfg14
    (cfg-block-set-terminator cfg13 e-bid
      (TermReturn '())))
  (define cfg15
    (cfg-block-set-terminator cfg14 f-bid
      (TermReturn '())))

  (define result (cfg-dom-opt cfg15))

  ;; A and C should be unconditional jumps
  (check-true (is-unconditional-jump? (get-terminator result a-bid)))
  (check-true (is-unconditional-jump? (get-terminator result c-bid)))

  (printf "passed~n"))

;; ============================================================
;; Test 9: Linear CFG - no optimization
;; ============================================================

(define (test-linear-no-opt)
  (printf "Test ~a: Linear CFG - no optimization... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (a-bid cfg2) (cfg-create-block cfg1))
  (define-values (b-bid cfg3) (cfg-create-block cfg2))

  (define cfg4 (cfg-set-entry cfg3 entry-bid))

  (define cfg5
    (cfg-block-set-terminator cfg4 entry-bid
      (TermJump a-bid)))

  (define cfg6
    (cfg-block-set-terminator cfg5 a-bid
      (TermJump b-bid)))

  (define cfg7
    (cfg-block-set-terminator cfg6 b-bid
      (TermReturn '())))

  (define result (cfg-dom-opt cfg7))

  ;; No changes expected
  (check-true (is-unconditional-jump? (get-terminator result entry-bid)))
  (check-true (is-unconditional-jump? (get-terminator result a-bid)))

  (printf "passed~n"))

;; ============================================================
;; Test 10: Both branches have redundant checks
;; ============================================================
;; entry: if (x) goto A else goto B
;; A: if (x) goto C else goto D  <- x=true, simplified
;; B: if (x) goto E else goto F  <- x=false, simplified

(define (test-both-branches)
  (printf "Test ~a: Both branches have redundant checks... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (a-bid cfg2) (cfg-create-block cfg1))
  (define-values (b-bid cfg3) (cfg-create-block cfg2))
  (define-values (c-bid cfg4) (cfg-create-block cfg3))
  (define-values (d-bid cfg5) (cfg-create-block cfg4))
  (define-values (e-bid cfg6) (cfg-create-block cfg5))
  (define-values (f-bid cfg7) (cfg-create-block cfg6))

  (define cfg8 (cfg-set-entry cfg7 entry-bid))

  (define v-x (VarId 'x))

  (define cfg9
    (cfg-block-set-terminator cfg8 entry-bid
      (TermBranch v-x a-bid b-bid)))

  ;; A: x is true
  (define cfg10
    (cfg-block-set-terminator cfg9 a-bid
      (TermBranch v-x c-bid d-bid)))

  ;; B: x is false
  (define cfg11
    (cfg-block-set-terminator cfg10 b-bid
      (TermBranch v-x e-bid f-bid)))

  (define cfg12
    (cfg-block-set-terminator cfg11 c-bid
      (TermReturn '())))
  (define cfg13
    (cfg-block-set-terminator cfg12 d-bid
      (TermReturn '())))
  (define cfg14
    (cfg-block-set-terminator cfg13 e-bid
      (TermReturn '())))
  (define cfg15
    (cfg-block-set-terminator cfg14 f-bid
      (TermReturn '())))

  (define result (cfg-dom-opt cfg15))

  ;; A should jump to C (x=true, so then branch)
  (define a-term (get-terminator result a-bid))
  (check-true (is-unconditional-jump? a-term))
  (check-equal? (TermJump-target a-term) c-bid)

  ;; B should jump to F (x=false, so else branch)
  (define b-term (get-terminator result b-bid))
  (check-true (is-unconditional-jump? b-term))
  (check-equal? (TermJump-target b-term) f-bid)

  (printf "passed~n"))

;; ============================================================
;; Run all tests
;; ============================================================

(define (run-all-tests)
  (printf "~n=== Dominator-based Optimization Tests ===~n~n")
  (set! test-count 0)

  (test-nested-condition)
  (test-false-branch)
  (test-deep-nesting)
  (test-no-opt-unknown)
  (test-with-stats)
  (test-multiple-conditions)
  (test-entry-no-opt)
  (test-chain-same-condition)
  (test-linear-no-opt)
  (test-both-branches)

  (printf "~nAll ~a tests passed!~n" test-count))

(run-all-tests)
