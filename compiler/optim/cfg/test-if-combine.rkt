#lang racket/base

;; ============================================================
;; Tests for If Combining Optimization
;; ============================================================

(require rackunit racket/match racket/list)
(require "if-combine.rkt")
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

(define (get-insns cfg bid)
  (define block (cfg-get-block cfg bid))
  (and block (CfgBlock-insns block)))

(define (count-insns cfg bid)
  (length (or (get-insns cfg bid) '())))

;; ============================================================
;; Test 1: Basic AND pattern
;; ============================================================
;; entry: if (a) goto inner else goto else
;; inner: if (b) goto then else goto else
;; => entry: if (a && b) goto then else goto else

(define (test-basic-and)
  (printf "Test ~a: Basic AND pattern... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  ;; Create blocks
  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (inner-bid cfg2) (cfg-create-block cfg1))
  (define-values (then-bid cfg3) (cfg-create-block cfg2))
  (define-values (else-bid cfg4) (cfg-create-block cfg3))

  (define cfg5 (cfg-set-entry cfg4 entry-bid))

  (define v-a (VarId 'a))
  (define v-b (VarId 'b))

  ;; entry: if (a) goto inner else goto else
  (define cfg6
    (cfg-block-set-terminator cfg5 entry-bid
      (TermBranch v-a inner-bid else-bid)))

  ;; inner: if (b) goto then else goto else
  (define cfg7
    (cfg-block-set-terminator cfg6 inner-bid
      (TermBranch v-b then-bid else-bid)))

  ;; then/else: return
  (define cfg8
    (cfg-block-set-terminator cfg7 then-bid
      (TermReturn '())))
  (define cfg9
    (cfg-block-set-terminator cfg8 else-bid
      (TermReturn '())))

  ;; Run optimization
  (define result (cfg-if-combine cfg9))

  ;; Entry should now have AND instruction
  (define entry-insns (get-insns result entry-bid))
  (check-true (and entry-insns (> (length entry-insns) 0))
              "Entry should have AND instruction")

  (printf "passed~n"))

;; ============================================================
;; Test 2: Basic OR pattern
;; ============================================================
;; entry: if (a) goto then else goto inner
;; inner: if (b) goto then else goto else
;; => entry: if (a || b) goto then else goto else

(define (test-basic-or)
  (printf "Test ~a: Basic OR pattern... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (inner-bid cfg2) (cfg-create-block cfg1))
  (define-values (then-bid cfg3) (cfg-create-block cfg2))
  (define-values (else-bid cfg4) (cfg-create-block cfg3))

  (define cfg5 (cfg-set-entry cfg4 entry-bid))

  (define v-a (VarId 'a))
  (define v-b (VarId 'b))

  ;; entry: if (a) goto then else goto inner
  (define cfg6
    (cfg-block-set-terminator cfg5 entry-bid
      (TermBranch v-a then-bid inner-bid)))

  ;; inner: if (b) goto then else goto else
  (define cfg7
    (cfg-block-set-terminator cfg6 inner-bid
      (TermBranch v-b then-bid else-bid)))

  (define cfg8
    (cfg-block-set-terminator cfg7 then-bid
      (TermReturn '())))
  (define cfg9
    (cfg-block-set-terminator cfg8 else-bid
      (TermReturn '())))

  (define result (cfg-if-combine cfg9))

  (define entry-insns (get-insns result entry-bid))
  (check-true (and entry-insns (> (length entry-insns) 0))
              "Entry should have OR instruction")

  (printf "passed~n"))

;; ============================================================
;; Test 3: Negated AND pattern
;; ============================================================
;; entry: if (a) goto else else goto inner
;; inner: if (b) goto then else goto else
;; => entry: if (!a && b) goto then else goto else

(define (test-negated-and)
  (printf "Test ~a: Negated AND pattern... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (inner-bid cfg2) (cfg-create-block cfg1))
  (define-values (then-bid cfg3) (cfg-create-block cfg2))
  (define-values (else-bid cfg4) (cfg-create-block cfg3))

  (define cfg5 (cfg-set-entry cfg4 entry-bid))

  (define v-a (VarId 'a))
  (define v-b (VarId 'b))

  ;; entry: if (a) goto else else goto inner (inverted)
  (define cfg6
    (cfg-block-set-terminator cfg5 entry-bid
      (TermBranch v-a else-bid inner-bid)))

  ;; inner: if (b) goto then else goto else
  (define cfg7
    (cfg-block-set-terminator cfg6 inner-bid
      (TermBranch v-b then-bid else-bid)))

  (define cfg8
    (cfg-block-set-terminator cfg7 then-bid
      (TermReturn '())))
  (define cfg9
    (cfg-block-set-terminator cfg8 else-bid
      (TermReturn '())))

  (define result (cfg-if-combine cfg9))

  (define entry-insns (get-insns result entry-bid))
  (check-true (and entry-insns (>= (length entry-insns) 2))
              "Entry should have NOT and AND instructions")

  (printf "passed~n"))

;; ============================================================
;; Test 4: Negated OR pattern
;; ============================================================
;; entry: if (a) goto inner else goto then
;; inner: if (b) goto then else goto else
;; => entry: if (!a || b) goto then else goto else

(define (test-negated-or)
  (printf "Test ~a: Negated OR pattern... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (inner-bid cfg2) (cfg-create-block cfg1))
  (define-values (then-bid cfg3) (cfg-create-block cfg2))
  (define-values (else-bid cfg4) (cfg-create-block cfg3))

  (define cfg5 (cfg-set-entry cfg4 entry-bid))

  (define v-a (VarId 'a))
  (define v-b (VarId 'b))

  ;; entry: if (a) goto inner else goto then
  (define cfg6
    (cfg-block-set-terminator cfg5 entry-bid
      (TermBranch v-a inner-bid then-bid)))

  ;; inner: if (b) goto then else goto else
  (define cfg7
    (cfg-block-set-terminator cfg6 inner-bid
      (TermBranch v-b then-bid else-bid)))

  (define cfg8
    (cfg-block-set-terminator cfg7 then-bid
      (TermReturn '())))
  (define cfg9
    (cfg-block-set-terminator cfg8 else-bid
      (TermReturn '())))

  (define result (cfg-if-combine cfg9))

  (define entry-insns (get-insns result entry-bid))
  (check-true (and entry-insns (>= (length entry-insns) 2))
              "Entry should have NOT and OR instructions")

  (printf "passed~n"))

;; ============================================================
;; Test 5: No combining when inner has side effects
;; ============================================================

(define (test-no-combine-side-effects)
  (printf "Test ~a: No combining with side effects... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (inner-bid cfg2) (cfg-create-block cfg1))
  (define-values (then-bid cfg3) (cfg-create-block cfg2))
  (define-values (else-bid cfg4) (cfg-create-block cfg3))

  (define cfg5 (cfg-set-entry cfg4 entry-bid))

  (define v-a (VarId 'a))
  (define v-b (VarId 'b))

  (define cfg6
    (cfg-block-set-terminator cfg5 entry-bid
      (TermBranch v-a inner-bid else-bid)))

  ;; Add side-effect instruction to inner
  (define cfg7
    (cfg-block-append-insn cfg6 inner-bid
      (VfInsn 'call '(some_func) (list (VarId 'tmp)) #f #f)))

  (define cfg8
    (cfg-block-set-terminator cfg7 inner-bid
      (TermBranch v-b then-bid else-bid)))

  (define cfg9
    (cfg-block-set-terminator cfg8 then-bid
      (TermReturn '())))
  (define cfg10
    (cfg-block-set-terminator cfg9 else-bid
      (TermReturn '())))

  (define result (cfg-if-combine cfg10))

  ;; Entry should remain unchanged
  (check-equal? (count-insns result entry-bid) 0
                "Entry should not have new instructions")

  (printf "passed~n"))

;; ============================================================
;; Test 6: No combining when inner has PHI nodes
;; ============================================================

(define (test-no-combine-phi)
  (printf "Test ~a: No combining with PHI nodes... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (inner-bid cfg2) (cfg-create-block cfg1))
  (define-values (then-bid cfg3) (cfg-create-block cfg2))
  (define-values (else-bid cfg4) (cfg-create-block cfg3))

  (define cfg5 (cfg-set-entry cfg4 entry-bid))

  (define v-a (VarId 'a))
  (define v-b (VarId 'b))

  (define cfg6
    (cfg-block-set-terminator cfg5 entry-bid
      (TermBranch v-a inner-bid else-bid)))

  ;; Add PHI to inner block
  (define cfg7
    (cfg-block-add-phi cfg6 inner-bid
      (PhiInsn (VarId 'x) (list (cons entry-bid (VarId 'y))))))

  (define cfg8
    (cfg-block-set-terminator cfg7 inner-bid
      (TermBranch v-b then-bid else-bid)))

  (define cfg9
    (cfg-block-set-terminator cfg8 then-bid
      (TermReturn '())))
  (define cfg10
    (cfg-block-set-terminator cfg9 else-bid
      (TermReturn '())))

  (define result (cfg-if-combine cfg10))

  (check-equal? (count-insns result entry-bid) 0)

  (printf "passed~n"))

;; ============================================================
;; Test 7: No combining when inner has multiple predecessors
;; ============================================================

(define (test-no-combine-multi-pred)
  (printf "Test ~a: No combining with multiple predecessors... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (entry2-bid cfg2) (cfg-create-block cfg1))
  (define-values (inner-bid cfg3) (cfg-create-block cfg2))
  (define-values (then-bid cfg4) (cfg-create-block cfg3))
  (define-values (else-bid cfg5) (cfg-create-block cfg4))

  (define cfg6 (cfg-set-entry cfg5 entry-bid))

  (define v-a (VarId 'a))
  (define v-b (VarId 'b))

  ;; Two blocks jump to inner
  (define cfg7
    (cfg-block-set-terminator cfg6 entry-bid
      (TermBranch v-a inner-bid else-bid)))

  (define cfg8
    (cfg-block-set-terminator cfg7 entry2-bid
      (TermJump inner-bid)))

  (define cfg9
    (cfg-block-set-terminator cfg8 inner-bid
      (TermBranch v-b then-bid else-bid)))

  (define cfg10
    (cfg-block-set-terminator cfg9 then-bid
      (TermReturn '())))
  (define cfg11
    (cfg-block-set-terminator cfg10 else-bid
      (TermReturn '())))

  (define result (cfg-if-combine cfg11))

  (check-equal? (count-insns result entry-bid) 0)

  (printf "passed~n"))

;; ============================================================
;; Test 8: With statistics
;; ============================================================

(define (test-with-stats)
  (printf "Test ~a: With statistics... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (inner-bid cfg2) (cfg-create-block cfg1))
  (define-values (then-bid cfg3) (cfg-create-block cfg2))
  (define-values (else-bid cfg4) (cfg-create-block cfg3))

  (define cfg5 (cfg-set-entry cfg4 entry-bid))

  (define v-a (VarId 'a))
  (define v-b (VarId 'b))

  (define cfg6
    (cfg-block-set-terminator cfg5 entry-bid
      (TermBranch v-a inner-bid else-bid)))

  (define cfg7
    (cfg-block-set-terminator cfg6 inner-bid
      (TermBranch v-b then-bid else-bid)))

  (define cfg8
    (cfg-block-set-terminator cfg7 then-bid
      (TermReturn '())))
  (define cfg9
    (cfg-block-set-terminator cfg8 else-bid
      (TermReturn '())))

  (define-values (result stats) (cfg-if-combine-with-stats cfg9))
  (check-not-false (assq 'combines stats))
  (check-true (>= (cdr (assq 'combines stats)) 1))

  (printf "passed~n"))

;; ============================================================
;; Test 9: Chain of AND conditions
;; ============================================================

(define (test-chain-and)
  (printf "Test ~a: Chain of AND conditions... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (b1-bid cfg2) (cfg-create-block cfg1))
  (define-values (b2-bid cfg3) (cfg-create-block cfg2))
  (define-values (then-bid cfg4) (cfg-create-block cfg3))
  (define-values (else-bid cfg5) (cfg-create-block cfg4))

  (define cfg6 (cfg-set-entry cfg5 entry-bid))

  (define v-a (VarId 'a))
  (define v-b (VarId 'b))
  (define v-c (VarId 'c))

  ;; entry: if (a) goto b1 else goto else
  (define cfg7
    (cfg-block-set-terminator cfg6 entry-bid
      (TermBranch v-a b1-bid else-bid)))

  ;; b1: if (b) goto b2 else goto else
  (define cfg8
    (cfg-block-set-terminator cfg7 b1-bid
      (TermBranch v-b b2-bid else-bid)))

  ;; b2: if (c) goto then else goto else
  (define cfg9
    (cfg-block-set-terminator cfg8 b2-bid
      (TermBranch v-c then-bid else-bid)))

  (define cfg10
    (cfg-block-set-terminator cfg9 then-bid
      (TermReturn '())))
  (define cfg11
    (cfg-block-set-terminator cfg10 else-bid
      (TermReturn '())))

  (define result (cfg-if-combine cfg11))

  ;; Entry should have combined conditions
  (define entry-insns (get-insns result entry-bid))
  (check-true (and entry-insns (>= (length entry-insns) 2))
              "Should have multiple AND instructions")

  (printf "passed~n"))

;; ============================================================
;; Test 10: No combining with different else blocks
;; ============================================================

(define (test-no-combine-different-else)
  (printf "Test ~a: No combining with different else blocks... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (inner-bid cfg2) (cfg-create-block cfg1))
  (define-values (then-bid cfg3) (cfg-create-block cfg2))
  (define-values (else1-bid cfg4) (cfg-create-block cfg3))
  (define-values (else2-bid cfg5) (cfg-create-block cfg4))

  (define cfg6 (cfg-set-entry cfg5 entry-bid))

  (define v-a (VarId 'a))
  (define v-b (VarId 'b))

  ;; Different else blocks
  (define cfg7
    (cfg-block-set-terminator cfg6 entry-bid
      (TermBranch v-a inner-bid else1-bid)))

  (define cfg8
    (cfg-block-set-terminator cfg7 inner-bid
      (TermBranch v-b then-bid else2-bid)))  ; Different else!

  (define cfg9
    (cfg-block-set-terminator cfg8 then-bid
      (TermReturn '())))
  (define cfg10
    (cfg-block-set-terminator cfg9 else1-bid
      (TermReturn '())))
  (define cfg11
    (cfg-block-set-terminator cfg10 else2-bid
      (TermReturn '())))

  (define result (cfg-if-combine cfg11))

  (check-equal? (count-insns result entry-bid) 0)

  (printf "passed~n"))

;; ============================================================
;; Run all tests
;; ============================================================

(define (run-all-tests)
  (printf "~n=== If Combining Tests ===~n~n")
  (set! test-count 0)

  (test-basic-and)
  (test-basic-or)
  (test-negated-and)
  (test-negated-or)
  (test-no-combine-side-effects)
  (test-no-combine-phi)
  (test-no-combine-multi-pred)
  (test-with-stats)
  (test-chain-and)
  (test-no-combine-different-else)

  (printf "~nAll ~a tests passed!~n" test-count))

(run-all-tests)
