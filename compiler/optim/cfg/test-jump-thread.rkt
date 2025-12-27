#lang racket/base

;; ============================================================
;; Jump Threading Tests
;; ============================================================

(require rackunit racket/match racket/list racket/set)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "loop-analysis.rkt")
(require "jump-thread.rkt")

;; ============================================================
;; 辅助函数
;; ============================================================

(define test-count 0)

(define (count-blocks cfg)
  (length (cfg-all-block-ids cfg)))

(define (get-terminator cfg bid)
  (define block (cfg-get-block cfg bid))
  (and block (CfgBlock-terminator block)))

;; 获取块的分支目标
(define (get-branch-targets cfg bid)
  (define term (get-terminator cfg bid))
  (match term
    [(TermBranch _ then-bid else-bid) (list then-bid else-bid)]
    [(TermJump target) (list target)]
    [_ '()]))

;; ============================================================
;; 测试 1: 基本跳转线程
;; ============================================================
;; A: if (x) goto B else goto C
;; B: if (x) goto D else goto E
;; → A: if (x) goto D else goto C

(define (test-basic-threading)
  (printf "Test ~a: Basic jump threading... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  ;; 创建块 A, B, C, D, E
  (define-values (a-bid cfg1) (cfg-create-block cfg0))
  (define-values (b-bid cfg2) (cfg-create-block cfg1))
  (define-values (c-bid cfg3) (cfg-create-block cfg2))
  (define-values (d-bid cfg4) (cfg-create-block cfg3))
  (define-values (e-bid cfg5) (cfg-create-block cfg4))

  (define cfg6 (cfg-set-entry cfg5 a-bid))

  (define v-x (VarId 0))

  ;; A: if (x) goto B else goto C
  (define cfg7
    (cfg-block-append-insn cfg6 a-bid
      (VfInsn 'const '(1) (list v-x) #f #f)))  ; x = 1 (arbitrary)
  (define cfg8
    (cfg-block-set-terminator cfg7 a-bid
      (TermBranch v-x b-bid c-bid)))

  ;; B: if (x) goto D else goto E
  (define cfg9
    (cfg-block-set-terminator cfg8 b-bid
      (TermBranch v-x d-bid e-bid)))

  ;; C, D, E: return
  (define cfg10
    (cfg-block-set-terminator cfg9 c-bid
      (TermReturn '())))
  (define cfg11
    (cfg-block-set-terminator cfg10 d-bid
      (TermReturn '())))
  (define cfg12
    (cfg-block-set-terminator cfg11 e-bid
      (TermReturn '())))

  ;; 执行跳转线程化
  (define cfg^ (cfg-jump-thread cfg12))

  ;; 检查 A 的分支目标
  (define targets (get-branch-targets cfg^ a-bid))

  ;; A 应该现在跳转到 D（而不是 B）因为 x 为真时去 B，B 中 x 也为真所以去 D
  (check-equal? (car targets) d-bid "Then branch should go directly to D")

  (printf "PASS~n"))

;; ============================================================
;; 测试 2: 反向条件线程
;; ============================================================
;; A: if (x) goto B else goto C
;; C: if (x) goto D else goto E
;; → A: if (x) goto B else goto E

(define (test-false-branch-threading)
  (printf "Test ~a: False branch threading... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (a-bid cfg1) (cfg-create-block cfg0))
  (define-values (b-bid cfg2) (cfg-create-block cfg1))
  (define-values (c-bid cfg3) (cfg-create-block cfg2))
  (define-values (d-bid cfg4) (cfg-create-block cfg3))
  (define-values (e-bid cfg5) (cfg-create-block cfg4))

  (define cfg6 (cfg-set-entry cfg5 a-bid))
  (define v-x (VarId 0))

  ;; A: if (x) goto B else goto C
  (define cfg7
    (cfg-block-append-insn cfg6 a-bid
      (VfInsn 'const '(1) (list v-x) #f #f)))
  (define cfg8
    (cfg-block-set-terminator cfg7 a-bid
      (TermBranch v-x b-bid c-bid)))

  ;; B: return
  (define cfg9
    (cfg-block-set-terminator cfg8 b-bid
      (TermReturn '())))

  ;; C: if (x) goto D else goto E
  (define cfg10
    (cfg-block-set-terminator cfg9 c-bid
      (TermBranch v-x d-bid e-bid)))

  ;; D, E: return
  (define cfg11
    (cfg-block-set-terminator cfg10 d-bid
      (TermReturn '())))
  (define cfg12
    (cfg-block-set-terminator cfg11 e-bid
      (TermReturn '())))

  (define cfg^ (cfg-jump-thread cfg12))

  ;; A 的 else 分支应该直接去 E（因为 x 为假时去 C，C 中 x 也为假所以去 E）
  (define targets (get-branch-targets cfg^ a-bid))
  (check-equal? (cadr targets) e-bid "Else branch should go directly to E")

  (printf "PASS~n"))

;; ============================================================
;; 测试 3: 无法线程化的情况
;; ============================================================
;; A: if (x) goto B else goto C
;; B: if (y) goto D else goto E  (不同条件)

(define (test-no-threading-different-cond)
  (printf "Test ~a: No threading (different condition)... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (a-bid cfg1) (cfg-create-block cfg0))
  (define-values (b-bid cfg2) (cfg-create-block cfg1))
  (define-values (c-bid cfg3) (cfg-create-block cfg2))
  (define-values (d-bid cfg4) (cfg-create-block cfg3))
  (define-values (e-bid cfg5) (cfg-create-block cfg4))

  (define cfg6 (cfg-set-entry cfg5 a-bid))
  (define v-x (VarId 0))
  (define v-y (VarId 1))

  ;; A: if (x) goto B else goto C
  (define cfg7
    (cfg-block-append-insn cfg6 a-bid
      (VfInsn 'const '(1) (list v-x) #f #f)))
  (define cfg8
    (cfg-block-set-terminator cfg7 a-bid
      (TermBranch v-x b-bid c-bid)))

  ;; B: if (y) goto D else goto E  (不同的条件变量)
  (define cfg9
    (cfg-block-append-insn cfg8 b-bid
      (VfInsn 'const '(0) (list v-y) #f #f)))
  (define cfg10
    (cfg-block-set-terminator cfg9 b-bid
      (TermBranch v-y d-bid e-bid)))

  ;; C, D, E: return
  (define cfg11 (cfg-block-set-terminator cfg10 c-bid (TermReturn '())))
  (define cfg12 (cfg-block-set-terminator cfg11 d-bid (TermReturn '())))
  (define cfg13 (cfg-block-set-terminator cfg12 e-bid (TermReturn '())))

  (define cfg^ (cfg-jump-thread cfg13))

  ;; A 的分支目标应该保持不变
  (define targets (get-branch-targets cfg^ a-bid))
  (check-equal? (car targets) b-bid "Then branch should still go to B")
  (check-equal? (cadr targets) c-bid "Else branch should still go to C")

  (printf "PASS~n"))

;; ============================================================
;; 测试 4: 无条件跳转（不进行线程化）
;; ============================================================

(define (test-unconditional-jump)
  (printf "Test ~a: Unconditional jump... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (a-bid cfg1) (cfg-create-block cfg0))
  (define-values (b-bid cfg2) (cfg-create-block cfg1))

  (define cfg3 (cfg-set-entry cfg2 a-bid))

  ;; A: goto B
  (define cfg4
    (cfg-block-set-terminator cfg3 a-bid
      (TermJump b-bid)))

  ;; B: return
  (define cfg5
    (cfg-block-set-terminator cfg4 b-bid
      (TermReturn '())))

  (define cfg^ (cfg-jump-thread cfg5))

  ;; 应该保持不变
  (define targets (get-branch-targets cfg^ a-bid))
  (check-equal? targets (list b-bid) "Jump target should be unchanged")

  (printf "PASS~n"))

;; ============================================================
;; 测试 5: 带统计版本
;; ============================================================

(define (test-with-stats)
  (printf "Test ~a: With stats version... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (a-bid cfg1) (cfg-create-block cfg0))
  (define-values (b-bid cfg2) (cfg-create-block cfg1))
  (define-values (c-bid cfg3) (cfg-create-block cfg2))
  (define-values (d-bid cfg4) (cfg-create-block cfg3))

  (define cfg5 (cfg-set-entry cfg4 a-bid))
  (define v-x (VarId 0))

  (define cfg6
    (cfg-block-append-insn cfg5 a-bid
      (VfInsn 'const '(1) (list v-x) #f #f)))
  (define cfg7
    (cfg-block-set-terminator cfg6 a-bid
      (TermBranch v-x b-bid c-bid)))
  (define cfg8
    (cfg-block-set-terminator cfg7 b-bid
      (TermBranch v-x d-bid c-bid)))
  (define cfg9
    (cfg-block-set-terminator cfg8 c-bid
      (TermReturn '())))
  (define cfg10
    (cfg-block-set-terminator cfg9 d-bid
      (TermReturn '())))

  (define-values (cfg^ stats) (cfg-jump-thread-with-stats cfg10))

  (check-true (list? stats) "Stats should be a list")
  (check-not-false (assq 'threads stats) "Should have threads stat")

  (printf "PASS~n"))

;; ============================================================
;; 测试 6: 空 CFG
;; ============================================================

(define (test-empty-cfg)
  (printf "Test ~a: Empty CFG... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))
  (define-values (bid cfg1) (cfg-create-block cfg0))
  (define cfg2 (cfg-set-entry cfg1 bid))
  (define cfg3
    (cfg-block-set-terminator cfg2 bid
      (TermReturn '())))

  (define cfg^ (cfg-jump-thread cfg3))

  (check-equal? (count-blocks cfg^) 1 "Should still have 1 block")

  (printf "PASS~n"))

;; ============================================================
;; 测试 7: 链式条件
;; ============================================================
;; A: if (x) goto B else goto E
;; B: if (x) goto C else goto E
;; C: if (x) goto D else goto E
;; → A: if (x) goto D else goto E

(define (test-chain-threading)
  (printf "Test ~a: Chain threading... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (a-bid cfg1) (cfg-create-block cfg0))
  (define-values (b-bid cfg2) (cfg-create-block cfg1))
  (define-values (c-bid cfg3) (cfg-create-block cfg2))
  (define-values (d-bid cfg4) (cfg-create-block cfg3))
  (define-values (e-bid cfg5) (cfg-create-block cfg4))

  (define cfg6 (cfg-set-entry cfg5 a-bid))
  (define v-x (VarId 0))

  (define cfg7
    (cfg-block-append-insn cfg6 a-bid
      (VfInsn 'const '(1) (list v-x) #f #f)))
  (define cfg8
    (cfg-block-set-terminator cfg7 a-bid
      (TermBranch v-x b-bid e-bid)))
  (define cfg9
    (cfg-block-set-terminator cfg8 b-bid
      (TermBranch v-x c-bid e-bid)))
  (define cfg10
    (cfg-block-set-terminator cfg9 c-bid
      (TermBranch v-x d-bid e-bid)))
  (define cfg11
    (cfg-block-set-terminator cfg10 d-bid
      (TermReturn '())))
  (define cfg12
    (cfg-block-set-terminator cfg11 e-bid
      (TermReturn '())))

  (define cfg^ (cfg-jump-thread cfg12))

  ;; 由于迭代，A 应该最终跳到 D
  (define targets (get-branch-targets cfg^ a-bid))
  (check-equal? (car targets) d-bid "Chain should thread to D")

  (printf "PASS~n"))

;; ============================================================
;; 测试 8: 带返回值的块
;; ============================================================

(define (test-with-return-value)
  (printf "Test ~a: With return value... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (a-bid cfg1) (cfg-create-block cfg0))
  (define-values (b-bid cfg2) (cfg-create-block cfg1))
  (define-values (c-bid cfg3) (cfg-create-block cfg2))
  (define-values (d-bid cfg4) (cfg-create-block cfg3))

  (define cfg5 (cfg-set-entry cfg4 a-bid))
  (define v-x (VarId 0))
  (define v-result (VarId 1))

  (define cfg6
    (cfg-block-append-insn cfg5 a-bid
      (VfInsn 'const '(1) (list v-x) #f #f)))
  (define cfg7
    (cfg-block-set-terminator cfg6 a-bid
      (TermBranch v-x b-bid c-bid)))

  (define cfg8
    (cfg-block-set-terminator cfg7 b-bid
      (TermBranch v-x d-bid c-bid)))

  (define cfg9
    (cfg-block-append-insn cfg8 c-bid
      (VfInsn 'const '(0) (list v-result) #f #f)))
  (define cfg10
    (cfg-block-set-terminator cfg9 c-bid
      (TermReturn (list v-result))))

  (define cfg11
    (cfg-block-append-insn cfg10 d-bid
      (VfInsn 'const '(1) (list v-result) #f #f)))
  (define cfg12
    (cfg-block-set-terminator cfg11 d-bid
      (TermReturn (list v-result))))

  (define cfg^ (cfg-jump-thread cfg12))

  ;; A 应该线程化到 D
  (define targets (get-branch-targets cfg^ a-bid))
  (check-equal? (car targets) d-bid "Should thread to D")

  (printf "PASS~n"))

;; ============================================================
;; 测试 9: 多次迭代收敛
;; ============================================================

(define (test-iteration-convergence)
  (printf "Test ~a: Iteration convergence... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  ;; 创建需要多次迭代的情况
  (define cfg0 (cfg-empty))

  (define-values (a-bid cfg1) (cfg-create-block cfg0))
  (define-values (b-bid cfg2) (cfg-create-block cfg1))
  (define-values (c-bid cfg3) (cfg-create-block cfg2))

  (define cfg4 (cfg-set-entry cfg3 a-bid))
  (define v-x (VarId 0))

  (define cfg5
    (cfg-block-append-insn cfg4 a-bid
      (VfInsn 'const '(1) (list v-x) #f #f)))
  (define cfg6
    (cfg-block-set-terminator cfg5 a-bid
      (TermBranch v-x b-bid c-bid)))
  (define cfg7
    (cfg-block-set-terminator cfg6 b-bid
      (TermBranch v-x c-bid c-bid)))
  (define cfg8
    (cfg-block-set-terminator cfg7 c-bid
      (TermReturn '())))

  (define cfg^ (cfg-jump-thread cfg8))

  ;; 优化应该收敛
  (check-not-false cfg^ "Should converge")

  (printf "PASS~n"))

;; ============================================================
;; 测试 10: 无分支 CFG
;; ============================================================

(define (test-no-branches)
  (printf "Test ~a: No branches... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))

  (define-values (a-bid cfg1) (cfg-create-block cfg0))
  (define-values (b-bid cfg2) (cfg-create-block cfg1))
  (define-values (c-bid cfg3) (cfg-create-block cfg2))

  (define cfg4 (cfg-set-entry cfg3 a-bid))

  (define cfg5
    (cfg-block-set-terminator cfg4 a-bid
      (TermJump b-bid)))
  (define cfg6
    (cfg-block-set-terminator cfg5 b-bid
      (TermJump c-bid)))
  (define cfg7
    (cfg-block-set-terminator cfg6 c-bid
      (TermReturn '())))

  (define cfg^ (cfg-jump-thread cfg7))

  ;; 无分支，不应改变
  (check-equal? (get-branch-targets cfg^ a-bid) (list b-bid)
                "A should still jump to B")

  (printf "PASS~n"))

;; ============================================================
;; 运行所有测试
;; ============================================================

(printf "~n")
(printf "╔══════════════════════════════════════════════════════════╗~n")
(printf "║              Jump Threading Tests                        ║~n")
(printf "╚══════════════════════════════════════════════════════════╝~n")
(printf "~n")

(test-basic-threading)
(test-false-branch-threading)
(test-no-threading-different-cond)
(test-unconditional-jump)
(test-with-stats)
(test-empty-cfg)
(test-chain-threading)
(test-with-return-value)
(test-iteration-convergence)
(test-no-branches)

(printf "~n~a tests completed.~n" test-count)
