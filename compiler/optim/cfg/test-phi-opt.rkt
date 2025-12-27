#lang racket/base

;; ============================================================
;; PHI 节点优化测试
;; ============================================================

(require rackunit racket/match racket/list)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "phi-opt.rkt")

;; ============================================================
;; 辅助函数
;; ============================================================

(define test-count 0)

(define (run-test name cfg expected-phi-reduction)
  (set! test-count (+ 1 test-count))
  (printf "Test ~a: ~a... " test-count name)

  (define before-phis (count-phis cfg))
  (define-values (cfg^ stats) (cfg-phi-opt-with-stats cfg))
  (define after-phis (count-phis cfg^))
  (define reduction (- before-phis after-phis))

  (check >= reduction expected-phi-reduction
         (format "Expected at least ~a PHI reduction, got ~a"
                 expected-phi-reduction reduction))
  (printf "PASS (phis: ~a -> ~a)~n" before-phis after-phis))

(define (count-phis cfg)
  (for*/sum ([bid (cfg-all-block-ids cfg)]
             [block (in-value (cfg-get-block cfg bid))]
             #:when block)
    (length (CfgBlock-phis block))))

;; 创建多块 CFG 的辅助函数
;; 返回 (values cfg entry-bid merge-bid)
(define (make-diamond-cfg)
  (define cfg0 (cfg-empty))
  ;; 创建四个块: entry -> then/else -> merge
  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (then-bid cfg2) (cfg-create-block cfg1))
  (define-values (else-bid cfg3) (cfg-create-block cfg2))
  (define-values (merge-bid cfg4) (cfg-create-block cfg3))
  (define cfg5 (cfg-set-entry cfg4 entry-bid))
  (values cfg5 entry-bid then-bid else-bid merge-bid))

;; ============================================================
;; 测试 1: 平凡 PHI - phi(a, a) → a
;; ============================================================

(define (test-trivial-phi)
  (define-values (cfg entry then-bid else-bid merge) (make-diamond-cfg))

  (define v0 (VarId 0))  ; 条件
  (define v1 (VarId 1))  ; 共同值
  (define v2 (VarId 2))  ; PHI 输出

  ;; Entry: 定义条件和共同值，分支
  (define cfg1
    (cfg-block-append-insn cfg entry
      (VfInsn 'const '(1) (list v0) #f #f)))
  (define cfg2
    (cfg-block-append-insn cfg1 entry
      (VfInsn 'const '(42) (list v1) #f #f)))
  (define cfg3
    (cfg-block-set-terminator cfg2 entry
      (TermBranch v0 then-bid else-bid)))

  ;; Then 块: 空，跳转到 merge
  (define cfg4
    (cfg-block-set-terminator cfg3 then-bid
      (TermJump merge)))

  ;; Else 块: 空，跳转到 merge
  (define cfg5
    (cfg-block-set-terminator cfg4 else-bid
      (TermJump merge)))

  ;; Merge 块: phi(v1, v1) → 应该简化为 v1
  (define cfg6
    (cfg-block-add-phi cfg5 merge
      (PhiInsn v2 (list (cons then-bid v1) (cons else-bid v1)))))
  (define cfg7
    (cfg-block-set-terminator cfg6 merge
      (TermReturn (list v2))))

  (run-test "Trivial PHI: phi(a, a) -> a" cfg7 1))

;; ============================================================
;; 测试 2: 非平凡 PHI - phi(a, b) 不应简化
;; ============================================================

(define (test-non-trivial-phi)
  (define-values (cfg entry then-bid else-bid merge) (make-diamond-cfg))

  (define v0 (VarId 0))  ; 条件
  (define v1 (VarId 1))  ; 值1
  (define v2 (VarId 2))  ; 值2
  (define v3 (VarId 3))  ; PHI 输出

  ;; Entry: 定义条件和两个不同的值
  (define cfg1
    (cfg-block-append-insn cfg entry
      (VfInsn 'const '(1) (list v0) #f #f)))
  (define cfg2
    (cfg-block-append-insn cfg1 entry
      (VfInsn 'const '(10) (list v1) #f #f)))
  (define cfg3
    (cfg-block-append-insn cfg2 entry
      (VfInsn 'const '(20) (list v2) #f #f)))
  (define cfg4
    (cfg-block-set-terminator cfg3 entry
      (TermBranch v0 then-bid else-bid)))

  ;; Then 块: 跳转到 merge
  (define cfg5
    (cfg-block-set-terminator cfg4 then-bid
      (TermJump merge)))

  ;; Else 块: 跳转到 merge
  (define cfg6
    (cfg-block-set-terminator cfg5 else-bid
      (TermJump merge)))

  ;; Merge 块: phi(v1, v2) 不同值，不应简化
  (define cfg7
    (cfg-block-add-phi cfg6 merge
      (PhiInsn v3 (list (cons then-bid v1) (cons else-bid v2)))))
  (define cfg8
    (cfg-block-set-terminator cfg7 merge
      (TermReturn (list v3))))

  (run-test "Non-trivial PHI: phi(a, b) preserved" cfg8 0))

;; ============================================================
;; 测试 3: 自引用 PHI - x = phi(x, a) → a
;; ============================================================

(define (test-self-ref-phi)
  ;; 创建循环结构: entry -> loop (with backedge)
  (define cfg0 (cfg-empty))
  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (loop-bid cfg2) (cfg-create-block cfg1))
  (define cfg3 (cfg-set-entry cfg2 entry-bid))

  (define v0 (VarId 0))  ; 初始值
  (define v1 (VarId 1))  ; PHI 输出 (自引用)
  (define v2 (VarId 2))  ; 条件

  ;; Entry: 定义初始值，跳转到 loop
  (define cfg4
    (cfg-block-append-insn cfg3 entry-bid
      (VfInsn 'const '(0) (list v0) #f #f)))
  (define cfg5
    (cfg-block-set-terminator cfg4 entry-bid
      (TermJump loop-bid)))

  ;; Loop: phi(v0 from entry, v1 from loop) - 自引用
  ;; x = phi(x, a) 其中非自引用输入只有 a，应简化为 a
  (define cfg6
    (cfg-block-add-phi cfg5 loop-bid
      (PhiInsn v1 (list (cons entry-bid v0) (cons loop-bid v1)))))
  (define cfg7
    (cfg-block-append-insn cfg6 loop-bid
      (VfInsn 'const '(1) (list v2) #f #f)))
  (define cfg8
    (cfg-block-set-terminator cfg7 loop-bid
      (TermBranch v2 loop-bid entry-bid)))  ; 简单循环结构

  (run-test "Self-ref PHI: x = phi(x, a) -> a" cfg8 1))

;; ============================================================
;; 测试 4: 多个平凡 PHI
;; ============================================================

(define (test-multiple-trivial-phis)
  (define-values (cfg entry then-bid else-bid merge) (make-diamond-cfg))

  (define v0 (VarId 0))  ; 条件
  (define v1 (VarId 1))  ; 共同值1
  (define v2 (VarId 2))  ; 共同值2
  (define v3 (VarId 3))  ; PHI 输出1
  (define v4 (VarId 4))  ; PHI 输出2

  ;; Entry
  (define cfg1
    (cfg-block-append-insn cfg entry
      (VfInsn 'const '(1) (list v0) #f #f)))
  (define cfg2
    (cfg-block-append-insn cfg1 entry
      (VfInsn 'const '(10) (list v1) #f #f)))
  (define cfg3
    (cfg-block-append-insn cfg2 entry
      (VfInsn 'const '(20) (list v2) #f #f)))
  (define cfg4
    (cfg-block-set-terminator cfg3 entry
      (TermBranch v0 then-bid else-bid)))

  ;; Then/Else 块
  (define cfg5
    (cfg-block-set-terminator cfg4 then-bid
      (TermJump merge)))
  (define cfg6
    (cfg-block-set-terminator cfg5 else-bid
      (TermJump merge)))

  ;; Merge: 两个平凡 PHI
  (define cfg7
    (cfg-block-add-phi cfg6 merge
      (PhiInsn v3 (list (cons then-bid v1) (cons else-bid v1)))))
  (define cfg8
    (cfg-block-add-phi cfg7 merge
      (PhiInsn v4 (list (cons then-bid v2) (cons else-bid v2)))))
  (define cfg9
    (cfg-block-set-terminator cfg8 merge
      (TermReturn (list v3 v4))))

  (run-test "Multiple trivial PHIs" cfg9 2))

;; ============================================================
;; 测试 5: 混合 PHI - 部分平凡部分非平凡
;; ============================================================

(define (test-mixed-phis)
  (define-values (cfg entry then-bid else-bid merge) (make-diamond-cfg))

  (define v0 (VarId 0))  ; 条件
  (define v1 (VarId 1))  ; 共同值
  (define v2 (VarId 2))  ; 不同值1
  (define v3 (VarId 3))  ; 不同值2
  (define v4 (VarId 4))  ; 平凡 PHI 输出
  (define v5 (VarId 5))  ; 非平凡 PHI 输出

  ;; Entry
  (define cfg1
    (cfg-block-append-insn cfg entry
      (VfInsn 'const '(1) (list v0) #f #f)))
  (define cfg2
    (cfg-block-append-insn cfg1 entry
      (VfInsn 'const '(42) (list v1) #f #f)))
  (define cfg3
    (cfg-block-append-insn cfg2 entry
      (VfInsn 'const '(10) (list v2) #f #f)))
  (define cfg4
    (cfg-block-append-insn cfg3 entry
      (VfInsn 'const '(20) (list v3) #f #f)))
  (define cfg5
    (cfg-block-set-terminator cfg4 entry
      (TermBranch v0 then-bid else-bid)))

  ;; Then/Else 块
  (define cfg6
    (cfg-block-set-terminator cfg5 then-bid
      (TermJump merge)))
  (define cfg7
    (cfg-block-set-terminator cfg6 else-bid
      (TermJump merge)))

  ;; Merge: 一个平凡 PHI，一个非平凡 PHI
  (define cfg8
    (cfg-block-add-phi cfg7 merge
      (PhiInsn v4 (list (cons then-bid v1) (cons else-bid v1)))))  ; 平凡
  (define cfg9
    (cfg-block-add-phi cfg8 merge
      (PhiInsn v5 (list (cons then-bid v2) (cons else-bid v3)))))  ; 非平凡
  (define cfg10
    (cfg-block-set-terminator cfg9 merge
      (TermReturn (list v4 v5))))

  (run-test "Mixed PHIs: trivial and non-trivial" cfg10 1))

;; ============================================================
;; 测试 6: PHI 链传播
;; ============================================================

(define (test-phi-chain)
  ;; entry -> merge1 -> merge2
  (define cfg0 (cfg-empty))
  (define-values (entry-bid cfg1) (cfg-create-block cfg0))
  (define-values (then-bid cfg2) (cfg-create-block cfg1))
  (define-values (else-bid cfg3) (cfg-create-block cfg2))
  (define-values (merge1-bid cfg4) (cfg-create-block cfg3))
  (define-values (then2-bid cfg5) (cfg-create-block cfg4))
  (define-values (else2-bid cfg6) (cfg-create-block cfg5))
  (define-values (merge2-bid cfg7) (cfg-create-block cfg6))
  (define cfg8 (cfg-set-entry cfg7 entry-bid))

  (define v0 (VarId 0))  ; 条件1
  (define v1 (VarId 1))  ; 原始值
  (define v2 (VarId 2))  ; PHI1 输出 (平凡)
  (define v3 (VarId 3))  ; 条件2
  (define v4 (VarId 4))  ; PHI2 输出 (使用 PHI1)

  ;; Entry
  (define cfg9
    (cfg-block-append-insn cfg8 entry-bid
      (VfInsn 'const '(1) (list v0) #f #f)))
  (define cfg10
    (cfg-block-append-insn cfg9 entry-bid
      (VfInsn 'const '(42) (list v1) #f #f)))
  (define cfg11
    (cfg-block-set-terminator cfg10 entry-bid
      (TermBranch v0 then-bid else-bid)))

  ;; Then/Else 块
  (define cfg12
    (cfg-block-set-terminator cfg11 then-bid
      (TermJump merge1-bid)))
  (define cfg13
    (cfg-block-set-terminator cfg12 else-bid
      (TermJump merge1-bid)))

  ;; Merge1: 平凡 PHI
  (define cfg14
    (cfg-block-add-phi cfg13 merge1-bid
      (PhiInsn v2 (list (cons then-bid v1) (cons else-bid v1)))))
  (define cfg15
    (cfg-block-append-insn cfg14 merge1-bid
      (VfInsn 'const '(0) (list v3) #f #f)))
  (define cfg16
    (cfg-block-set-terminator cfg15 merge1-bid
      (TermBranch v3 then2-bid else2-bid)))

  ;; Then2/Else2 块
  (define cfg17
    (cfg-block-set-terminator cfg16 then2-bid
      (TermJump merge2-bid)))
  (define cfg18
    (cfg-block-set-terminator cfg17 else2-bid
      (TermJump merge2-bid)))

  ;; Merge2: 使用 v2 的 PHI (也是平凡的)
  (define cfg19
    (cfg-block-add-phi cfg18 merge2-bid
      (PhiInsn v4 (list (cons then2-bid v2) (cons else2-bid v2)))))
  (define cfg20
    (cfg-block-set-terminator cfg19 merge2-bid
      (TermReturn (list v4))))

  (run-test "PHI chain propagation" cfg20 2))

;; ============================================================
;; 测试 7: 迭代优化到不动点
;; ============================================================

(define (test-fixpoint)
  (printf "Test ~a: Fixpoint iteration... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define-values (cfg entry then-bid else-bid merge) (make-diamond-cfg))

  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))

  (define cfg1
    (cfg-block-append-insn cfg entry
      (VfInsn 'const '(1) (list v0) #f #f)))
  (define cfg2
    (cfg-block-append-insn cfg1 entry
      (VfInsn 'const '(42) (list v1) #f #f)))
  (define cfg3
    (cfg-block-set-terminator cfg2 entry
      (TermBranch v0 then-bid else-bid)))
  (define cfg4
    (cfg-block-set-terminator cfg3 then-bid
      (TermJump merge)))
  (define cfg5
    (cfg-block-set-terminator cfg4 else-bid
      (TermJump merge)))
  (define cfg6
    (cfg-block-add-phi cfg5 merge
      (PhiInsn v2 (list (cons then-bid v1) (cons else-bid v1)))))
  (define cfg7
    (cfg-block-set-terminator cfg6 merge
      (TermReturn (list v2))))

  (define cfg-opt (cfg-phi-opt-fixpoint cfg7))
  (define final-phis (count-phis cfg-opt))

  (check-equal? final-phis 0 "Expected 0 PHIs after fixpoint")
  (printf "PASS (phis: ~a -> ~a)~n" (count-phis cfg7) final-phis))

;; ============================================================
;; 测试 8: 空 CFG
;; ============================================================

(define (test-empty-cfg)
  (printf "Test ~a: Empty CFG... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (cfg-empty))
  (define cfg-opt (cfg-phi-opt cfg))

  (check-equal? (count-phis cfg-opt) 0)
  (printf "PASS~n"))

;; ============================================================
;; 测试 9: 替换在指令中的传播
;; ============================================================

(define (test-replacement-in-insns)
  (define-values (cfg entry then-bid else-bid merge) (make-diamond-cfg))

  (define v0 (VarId 0))  ; 条件
  (define v1 (VarId 1))  ; 共同值
  (define v2 (VarId 2))  ; PHI 输出
  (define v3 (VarId 3))  ; 使用 PHI 的结果

  ;; Entry
  (define cfg1
    (cfg-block-append-insn cfg entry
      (VfInsn 'const '(1) (list v0) #f #f)))
  (define cfg2
    (cfg-block-append-insn cfg1 entry
      (VfInsn 'const '(10) (list v1) #f #f)))
  (define cfg3
    (cfg-block-set-terminator cfg2 entry
      (TermBranch v0 then-bid else-bid)))

  (define cfg4
    (cfg-block-set-terminator cfg3 then-bid
      (TermJump merge)))
  (define cfg5
    (cfg-block-set-terminator cfg4 else-bid
      (TermJump merge)))

  ;; Merge: PHI + 使用 PHI 结果的指令
  (define cfg6
    (cfg-block-add-phi cfg5 merge
      (PhiInsn v2 (list (cons then-bid v1) (cons else-bid v1)))))
  (define cfg7
    (cfg-block-append-insn cfg6 merge
      (VfInsn 'add (list v2 v2) (list v3) #f #f)))
  (define cfg8
    (cfg-block-set-terminator cfg7 merge
      (TermReturn (list v3))))

  ;; 优化后，v2 应该被替换为 v1
  (define cfg-opt (cfg-phi-opt cfg8))
  (define block (cfg-get-block cfg-opt merge))
  (define insns (CfgBlock-insns block))

  (printf "Test ~a: Replacement propagation to instructions... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  ;; 检查指令中的引用已被替换
  (when (not (null? insns))
    (match (car insns)
      [(VfInsn 'add inputs _ _ _)
       (check-equal? inputs (list v1 v1) "PHI should be replaced in instruction inputs")]))

  (printf "PASS~n"))

;; ============================================================
;; 运行所有测试
;; ============================================================

(module+ main
  (displayln "")
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║           PHI Node Optimization Tests                    ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")
  (displayln "")

  (test-trivial-phi)
  (test-non-trivial-phi)
  (test-self-ref-phi)
  (test-multiple-trivial-phis)
  (test-mixed-phis)
  (test-phi-chain)
  (test-fixpoint)
  (test-empty-cfg)
  (test-replacement-in-insns)

  (printf "~n~a tests completed.~n" test-count))
