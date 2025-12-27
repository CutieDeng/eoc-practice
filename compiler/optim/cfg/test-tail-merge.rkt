#lang racket/base

;; ============================================================
;; Tests for Tail Merging Optimization
;; ============================================================

(require rackunit racket/match racket/list)
(require "tail-merge.rkt")
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

(define (block-exists? cfg bid)
  (not (not (cfg-get-block cfg bid))))

;; 获取块的分支目标
(define (get-branch-targets cfg bid)
  (define term (get-terminator cfg bid))
  (match term
    [(TermBranch _ then-bid else-bid) (list then-bid else-bid)]
    [(TermJump target) (list target)]
    [_ '()]))

;; ============================================================
;; Test 1: Basic tail merge - two identical blocks
;; ============================================================
;; entry: if (x) goto B1 else goto B2
;; B1: return 1
;; B2: return 1  (identical to B1)
;; After merge: B2's predecessors redirected to B1

(define (test-basic-merge)
  (printf "Test ~a: Basic tail merge... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (b1-bid cfg2) (cfg-create-block cfg1))
  (define-values (b2-bid cfg3) (cfg-create-block cfg2))

  (define cfg4 (cfg-set-entry cfg3 entry-bid))

  (define v-x (VarId 'x))
  (define v-result (VarId 'result))

  ;; entry: if (x) goto B1 else goto B2
  (define cfg5
    (cfg-block-set-terminator cfg4 entry-bid
      (TermBranch v-x b1-bid b2-bid)))

  ;; B1: return 1
  (define cfg6
    (cfg-block-append-insn cfg5 b1-bid
      (VfInsn 'const '(1) (list v-result) #f #f)))
  (define cfg7
    (cfg-block-set-terminator cfg6 b1-bid
      (TermReturn (list v-result))))

  ;; B2: return 1 (identical)
  (define cfg8
    (cfg-block-append-insn cfg7 b2-bid
      (VfInsn 'const '(1) (list v-result) #f #f)))
  (define cfg9
    (cfg-block-set-terminator cfg8 b2-bid
      (TermReturn (list v-result))))

  (define result (cfg-tail-merge cfg9))

  ;; Entry should now branch to same block for both paths
  (define targets (get-branch-targets result entry-bid))
  (check-equal? (first targets) (second targets)
                "Both branches should go to same block after merge")

  (printf "passed~n"))

;; ============================================================
;; Test 2: Three identical blocks
;; ============================================================

(define (test-three-identical)
  (printf "Test ~a: Three identical blocks... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (mid-bid cfg2) (cfg-create-block cfg1))
  (define-values (b1-bid cfg3) (cfg-create-block cfg2))
  (define-values (b2-bid cfg4) (cfg-create-block cfg3))
  (define-values (b3-bid cfg5) (cfg-create-block cfg4))

  (define cfg6 (cfg-set-entry cfg5 entry-bid))

  (define v-x (VarId 'x))
  (define v-y (VarId 'y))
  (define v-result (VarId 'result))

  ;; entry: if (x) goto b1 else goto mid
  (define cfg7
    (cfg-block-set-terminator cfg6 entry-bid
      (TermBranch v-x b1-bid mid-bid)))

  ;; mid: if (y) goto b2 else goto b3
  (define cfg8
    (cfg-block-set-terminator cfg7 mid-bid
      (TermBranch v-y b2-bid b3-bid)))

  ;; B1, B2, B3: all return 42
  (define (add-return-42 cfg bid)
    (define cfg^
      (cfg-block-append-insn cfg bid
        (VfInsn 'const '(42) (list v-result) #f #f)))
    (cfg-block-set-terminator cfg^ bid
      (TermReturn (list v-result))))

  (define cfg9 (add-return-42 cfg8 b1-bid))
  (define cfg10 (add-return-42 cfg9 b2-bid))
  (define cfg11 (add-return-42 cfg10 b3-bid))

  (define result (cfg-tail-merge cfg11))

  ;; After merge, mid's branches should point to same block as entry's
  ;; or all three should be merged
  (define entry-targets (get-branch-targets result entry-bid))
  (define mid-targets (get-branch-targets result mid-bid))

  ;; At minimum, b2 and b3 should be merged (same successors in mid)
  (check-equal? (first mid-targets) (second mid-targets))

  (printf "passed~n"))

;; ============================================================
;; Test 3: No merge - different instructions
;; ============================================================

(define (test-no-merge-different-insns)
  (printf "Test ~a: No merge with different instructions... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (b1-bid cfg2) (cfg-create-block cfg1))
  (define-values (b2-bid cfg3) (cfg-create-block cfg2))

  (define cfg4 (cfg-set-entry cfg3 entry-bid))

  (define v-x (VarId 'x))
  (define v-result (VarId 'result))

  (define cfg5
    (cfg-block-set-terminator cfg4 entry-bid
      (TermBranch v-x b1-bid b2-bid)))

  ;; B1: return 1
  (define cfg6
    (cfg-block-append-insn cfg5 b1-bid
      (VfInsn 'const '(1) (list v-result) #f #f)))
  (define cfg7
    (cfg-block-set-terminator cfg6 b1-bid
      (TermReturn (list v-result))))

  ;; B2: return 2 (different value)
  (define cfg8
    (cfg-block-append-insn cfg7 b2-bid
      (VfInsn 'const '(2) (list v-result) #f #f)))
  (define cfg9
    (cfg-block-set-terminator cfg8 b2-bid
      (TermReturn (list v-result))))

  (define result (cfg-tail-merge cfg9))

  ;; Should not be merged
  (define targets (get-branch-targets result entry-bid))
  (check-not-equal? (first targets) (second targets)
                    "Different blocks should not be merged")

  (printf "passed~n"))

;; ============================================================
;; Test 4: No merge - different successors
;; ============================================================

(define (test-no-merge-different-succs)
  (printf "Test ~a: No merge with different successors... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (b1-bid cfg2) (cfg-create-block cfg1))
  (define-values (b2-bid cfg3) (cfg-create-block cfg2))
  (define-values (exit1-bid cfg4) (cfg-create-block cfg3))
  (define-values (exit2-bid cfg5) (cfg-create-block cfg4))

  (define cfg6 (cfg-set-entry cfg5 entry-bid))

  (define v-x (VarId 'x))
  (define v-result1 (VarId 'result1))
  (define v-result2 (VarId 'result2))

  (define cfg7
    (cfg-block-set-terminator cfg6 entry-bid
      (TermBranch v-x b1-bid b2-bid)))

  ;; B1: goto exit1
  (define cfg8
    (cfg-block-set-terminator cfg7 b1-bid
      (TermJump exit1-bid)))

  ;; B2: goto exit2 (different successor)
  (define cfg9
    (cfg-block-set-terminator cfg8 b2-bid
      (TermJump exit2-bid)))

  ;; Make exit blocks different so they can't be merged
  (define cfg10
    (cfg-block-append-insn cfg9 exit1-bid
      (VfInsn 'const '(1) (list v-result1) #f #f)))
  (define cfg11
    (cfg-block-set-terminator cfg10 exit1-bid
      (TermReturn (list v-result1))))

  (define cfg12
    (cfg-block-append-insn cfg11 exit2-bid
      (VfInsn 'const '(2) (list v-result2) #f #f)))  ; Different constant!
  (define cfg13
    (cfg-block-set-terminator cfg12 exit2-bid
      (TermReturn (list v-result2))))

  (define result (cfg-tail-merge cfg13))

  (define targets (get-branch-targets result entry-bid))
  (check-not-equal? (first targets) (second targets))

  (printf "passed~n"))

;; ============================================================
;; Test 5: Merge with unconditional jumps
;; ============================================================

(define (test-merge-unconditional)
  (printf "Test ~a: Merge with unconditional jumps... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (b1-bid cfg2) (cfg-create-block cfg1))
  (define-values (b2-bid cfg3) (cfg-create-block cfg2))
  (define-values (exit-bid cfg4) (cfg-create-block cfg3))

  (define cfg5 (cfg-set-entry cfg4 entry-bid))

  (define v-x (VarId 'x))

  (define cfg6
    (cfg-block-set-terminator cfg5 entry-bid
      (TermBranch v-x b1-bid b2-bid)))

  ;; B1, B2: both jump to exit (empty blocks)
  (define cfg7
    (cfg-block-set-terminator cfg6 b1-bid
      (TermJump exit-bid)))

  (define cfg8
    (cfg-block-set-terminator cfg7 b2-bid
      (TermJump exit-bid)))

  (define cfg9
    (cfg-block-set-terminator cfg8 exit-bid
      (TermReturn '())))

  (define result (cfg-tail-merge cfg9))

  ;; Both empty blocks should be merged
  (define targets (get-branch-targets result entry-bid))
  (check-equal? (first targets) (second targets))

  (printf "passed~n"))

;; ============================================================
;; Test 6: With statistics
;; ============================================================

(define (test-with-stats)
  (printf "Test ~a: With statistics... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (b1-bid cfg2) (cfg-create-block cfg1))
  (define-values (b2-bid cfg3) (cfg-create-block cfg2))

  (define cfg4 (cfg-set-entry cfg3 entry-bid))

  (define v-x (VarId 'x))
  (define v-result (VarId 'result))

  (define cfg5
    (cfg-block-set-terminator cfg4 entry-bid
      (TermBranch v-x b1-bid b2-bid)))

  ;; Both return 1
  (define cfg6
    (cfg-block-append-insn cfg5 b1-bid
      (VfInsn 'const '(1) (list v-result) #f #f)))
  (define cfg7
    (cfg-block-set-terminator cfg6 b1-bid
      (TermReturn (list v-result))))
  (define cfg8
    (cfg-block-append-insn cfg7 b2-bid
      (VfInsn 'const '(1) (list v-result) #f #f)))
  (define cfg9
    (cfg-block-set-terminator cfg8 b2-bid
      (TermReturn (list v-result))))

  (define-values (result stats) (cfg-tail-merge-with-stats cfg9))
  (check-not-false (assq 'merges stats))
  (check-true (>= (cdr (assq 'merges stats)) 1))

  (printf "passed~n"))

;; ============================================================
;; Test 7: No merge with PHI nodes
;; ============================================================

(define (test-no-merge-phi)
  (printf "Test ~a: No merge with PHI nodes... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (b1-bid cfg2) (cfg-create-block cfg1))
  (define-values (b2-bid cfg3) (cfg-create-block cfg2))

  (define cfg4 (cfg-set-entry cfg3 entry-bid))

  (define v-x (VarId 'x))
  (define v-result (VarId 'result))

  (define cfg5
    (cfg-block-set-terminator cfg4 entry-bid
      (TermBranch v-x b1-bid b2-bid)))

  ;; B1: has phi, return 1
  (define cfg6
    (cfg-block-add-phi cfg5 b1-bid
      (PhiInsn (VarId 'phi1) (list (cons entry-bid (VarId 'val1))))))
  (define cfg7
    (cfg-block-append-insn cfg6 b1-bid
      (VfInsn 'const '(1) (list v-result) #f #f)))
  (define cfg8
    (cfg-block-set-terminator cfg7 b1-bid
      (TermReturn (list v-result))))

  ;; B2: return 1 (same but no phi)
  (define cfg9
    (cfg-block-append-insn cfg8 b2-bid
      (VfInsn 'const '(1) (list v-result) #f #f)))
  (define cfg10
    (cfg-block-set-terminator cfg9 b2-bid
      (TermReturn (list v-result))))

  (define result (cfg-tail-merge cfg10))

  ;; Should not merge because B1 has PHI
  (define targets (get-branch-targets result entry-bid))
  (check-not-equal? (first targets) (second targets))

  (printf "passed~n"))

;; ============================================================
;; Test 8: Chain merge
;; ============================================================

(define (test-chain-merge)
  (printf "Test ~a: Chain merge... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (b1-bid cfg2) (cfg-create-block cfg1))
  (define-values (b2-bid cfg3) (cfg-create-block cfg2))
  (define-values (c1-bid cfg4) (cfg-create-block cfg3))
  (define-values (c2-bid cfg5) (cfg-create-block cfg4))

  (define cfg6 (cfg-set-entry cfg5 entry-bid))

  (define v-x (VarId 'x))
  (define v-y (VarId 'y))

  ;; entry: if (x) goto b1 else goto b2
  (define cfg7
    (cfg-block-set-terminator cfg6 entry-bid
      (TermBranch v-x b1-bid b2-bid)))

  ;; b1: if (y) goto c1 else goto c2
  (define cfg8
    (cfg-block-set-terminator cfg7 b1-bid
      (TermBranch v-y c1-bid c2-bid)))

  ;; b2: if (y) goto c1 else goto c2 (same as b1!)
  (define cfg9
    (cfg-block-set-terminator cfg8 b2-bid
      (TermBranch v-y c1-bid c2-bid)))

  ;; c1, c2: return
  (define cfg10
    (cfg-block-set-terminator cfg9 c1-bid
      (TermReturn '())))
  (define cfg11
    (cfg-block-set-terminator cfg10 c2-bid
      (TermReturn '())))

  (define result (cfg-tail-merge cfg11))

  ;; b1 and b2 should be merged
  (define targets (get-branch-targets result entry-bid))
  (check-equal? (first targets) (second targets))

  (printf "passed~n"))

;; ============================================================
;; Test 9: Different operations count
;; ============================================================

(define (test-different-op-count)
  (printf "Test ~a: Different operation count... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (b1-bid cfg2) (cfg-create-block cfg1))
  (define-values (b2-bid cfg3) (cfg-create-block cfg2))

  (define cfg4 (cfg-set-entry cfg3 entry-bid))

  (define v-x (VarId 'x))
  (define v-result (VarId 'result))
  (define v-tmp (VarId 'tmp))

  (define cfg5
    (cfg-block-set-terminator cfg4 entry-bid
      (TermBranch v-x b1-bid b2-bid)))

  ;; B1: one instruction
  (define cfg6
    (cfg-block-append-insn cfg5 b1-bid
      (VfInsn 'const '(1) (list v-result) #f #f)))
  (define cfg7
    (cfg-block-set-terminator cfg6 b1-bid
      (TermReturn (list v-result))))

  ;; B2: two instructions
  (define cfg8
    (cfg-block-append-insn cfg7 b2-bid
      (VfInsn 'const '(1) (list v-tmp) #f #f)))
  (define cfg9
    (cfg-block-append-insn cfg8 b2-bid
      (VfInsn 'const '(1) (list v-result) #f #f)))
  (define cfg10
    (cfg-block-set-terminator cfg9 b2-bid
      (TermReturn (list v-result))))

  (define result (cfg-tail-merge cfg10))

  ;; Should not merge - different instruction counts
  (define targets (get-branch-targets result entry-bid))
  (check-not-equal? (first targets) (second targets))

  (printf "passed~n"))

;; ============================================================
;; Test 10: Empty blocks merge
;; ============================================================

(define (test-empty-blocks)
  (printf "Test ~a: Empty blocks merge... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (b1-bid cfg2) (cfg-create-block cfg1))
  (define-values (b2-bid cfg3) (cfg-create-block cfg2))

  (define cfg4 (cfg-set-entry cfg3 entry-bid))

  (define v-x (VarId 'x))

  (define cfg5
    (cfg-block-set-terminator cfg4 entry-bid
      (TermBranch v-x b1-bid b2-bid)))

  ;; B1, B2: both empty, just return
  (define cfg6
    (cfg-block-set-terminator cfg5 b1-bid
      (TermReturn '())))
  (define cfg7
    (cfg-block-set-terminator cfg6 b2-bid
      (TermReturn '())))

  (define result (cfg-tail-merge cfg7))

  ;; Empty blocks with same return should merge
  (define targets (get-branch-targets result entry-bid))
  (check-equal? (first targets) (second targets))

  (printf "passed~n"))

;; ============================================================
;; Run all tests
;; ============================================================

(define (run-all-tests)
  (printf "~n=== Tail Merging Tests ===~n~n")
  (set! test-count 0)

  (test-basic-merge)
  (test-three-identical)
  (test-no-merge-different-insns)
  (test-no-merge-different-succs)
  (test-merge-unconditional)
  (test-with-stats)
  (test-no-merge-phi)
  (test-chain-merge)
  (test-different-op-count)
  (test-empty-blocks)

  (printf "~nAll ~a tests passed!~n" test-count))

(run-all-tests)
