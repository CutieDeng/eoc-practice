#lang racket/base

;; ============================================================
;; 死代码消除测试
;; ============================================================

(require rackunit)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "dce.rkt")
(require "const-fold.rkt")

;; ============================================================
;; 辅助函数
;; ============================================================

(define (make-test-cfg insns terminator)
  (define cfg0 (cfg-empty))
  (define-values (bid cfg1) (cfg-create-block cfg0))
  (define cfg2 (cfg-set-entry cfg1 bid))
  (define cfg3
    (for/fold ([cfg cfg2])
              ([insn insns])
      (cfg-block-append-insn cfg bid insn)))
  (cfg-block-set-terminator cfg3 bid terminator))

(define (get-first-block-insns cfg)
  (define block (cfg-get-block cfg (cfg-get-entry cfg)))
  (and block (CfgBlock-insns block)))

(define (insn-count cfg)
  (for/sum ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if block (length (CfgBlock-insns block)) 0)))

;; ============================================================
;; 测试用例
;; ============================================================

(define dce-tests
  (test-suite "Dead Code Elimination Tests"

    ;; 测试 1: 未使用的常量被删除
    (test-case "删除未使用的常量"
      (define v0 (VarId 0))
      (define v1 (VarId 1))  ; 未使用
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(1) (list v0) #f #f)
            (VfInsn 'const '(2) (list v1) #f #f))  ; 死代码
          (TermReturn (list v0))))  ; 只返回 v0

      (define cfg^ (cfg-dce cfg))
      (check-equal? (insn-count cfg^) 1)  ; 只剩 1 条
      (define insns (get-first-block-insns cfg^))
      (check-equal? (VfInsn-inputs (car insns)) '(1)))

    ;; 测试 2: 链式依赖保留
    (test-case "保留有依赖链的指令"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(1) (list v0) #f #f)
            (VfInsn 'const '(2) (list v1) #f #f)
            (VfInsn 'add (list v0 v1) (list v2) #f #f))
          (TermReturn (list v2))))  ; 返回 v2

      (define cfg^ (cfg-dce cfg))
      ;; 所有 3 条指令都应保留（都被 v2 依赖）
      (check-equal? (insn-count cfg^) 3))

    ;; 测试 3: 部分死代码
    (test-case "删除部分死代码"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))  ; 未使用
      (define v3 (VarId 3))  ; 未使用
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(1) (list v0) #f #f)
            (VfInsn 'const '(2) (list v1) #f #f)
            (VfInsn 'add (list v0 v1) (list v2) #f #f)   ; 死代码
            (VfInsn 'mul (list v0 v1) (list v3) #f #f))  ; 死代码
          (TermReturn (list v0 v1))))  ; 只返回 v0, v1

      (define cfg^ (cfg-dce cfg))
      (check-equal? (insn-count cfg^) 2))  ; 只剩 2 条常量

    ;; 测试 4: 有副作用的指令保留
    (test-case "保留有副作用的指令"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(42) (list v0) #f #f)
            (VfInsn 'store-local (list v0 0) '() #f #f))  ; 副作用
          (TermReturn '())))

      (define cfg^ (cfg-dce cfg))
      ;; store-local 有副作用，必须保留；v0 被 store 使用，也保留
      (check-equal? (insn-count cfg^) 2))

    ;; 测试 5: 条件分支使用的变量保留
    (test-case "保留分支条件使用的变量"
      (define v0 (VarId 0))
      (define v1 (VarId 1))  ; 未使用
      (define v2 (VarId 2))
      (define cfg0 (cfg-empty))
      (define-values (bid1 cfg1) (cfg-create-block cfg0))
      (define-values (bid2 cfg2) (cfg-create-block cfg1))
      (define-values (bid3 cfg3) (cfg-create-block cfg2))
      (define cfg4 (cfg-set-entry cfg3 bid1))

      ;; 块1：计算条件
      (define cfg5
        (cfg-block-append-insn
          (cfg-block-append-insn
            (cfg-block-append-insn cfg4 bid1
              (VfInsn 'const '(1) (list v0) #f #f))
            bid1
            (VfInsn 'const '(99) (list v1) #f #f))  ; 死代码
          bid1
          (VfInsn 'eq0 (list v0) (list v2) #f #f)))
      (define cfg6
        (cfg-block-set-terminator cfg5 bid1
          (TermBranch v2 bid2 bid3)))

      (define cfg^ (cfg-dce cfg6))
      (define insns (get-first-block-insns cfg^))
      ;; v1 未使用应被删除，v0 和 v2 应保留
      (check-equal? (length insns) 2))

    ;; 测试 6: 空程序
    (test-case "空程序不变"
      (define cfg (make-test-cfg '() (TermReturn '())))
      (define cfg^ (cfg-dce cfg))
      (check-equal? (insn-count cfg^) 0))

    ;; 测试 7: 全部是死代码
    (test-case "全部死代码被删除"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(1) (list v0) #f #f)
            (VfInsn 'const '(2) (list v1) #f #f)
            (VfInsn 'add (list v0 v1) (list v2) #f #f))
          (TermReturn '())))  ; 无返回值

      (define cfg^ (cfg-dce cfg))
      (check-equal? (insn-count cfg^) 0))

    ;; 测试 8: invoke 调用保留
    (test-case "方法调用保留"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(42) (list v0) #f #f)
            (VfInsn 'invoke
              (list 'INVOKEVIRTUAL "Foo" "bar" "()V" (list v0))
              (list v1) #f #f))  ; 调用有副作用
          (TermReturn '())))

      (define cfg^ (cfg-dce cfg))
      ;; invoke 有副作用保留，v0 被调用使用保留
      (check-equal? (insn-count cfg^) 2))

    ;; 测试 9: 常量折叠后的 DCE
    (test-case "配合常量折叠"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define v3 (VarId 3))  ; 未使用
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(1) (list v0) #f #f)
            (VfInsn 'const '(2) (list v1) #f #f)
            (VfInsn 'add (list v0 v1) (list v2) #f #f)
            (VfInsn 'const '(999) (list v3) #f #f))  ; 死代码
          (TermReturn (list v2))))

      ;; 先常量折叠
      (define cfg1 (cfg-const-fold cfg))
      ;; 再 DCE
      (define cfg2 (cfg-dce cfg1))

      ;; 折叠后 v2=3 是常量，v0, v1 变成死代码
      ;; 最终只剩 1 条指令
      (check-equal? (insn-count cfg2) 1)
      (define insns (get-first-block-insns cfg2))
      (check-equal? (VfInsn-op (car insns)) 'const)
      (check-equal? (VfInsn-inputs (car insns)) '(3)))
))

;; ============================================================
;; 运行测试
;; ============================================================

(module+ test
  (require rackunit/text-ui)
  (run-tests dce-tests))

(module+ main
  (require rackunit/text-ui)
  (displayln "=== Dead Code Elimination Tests ===\n")
  (run-tests dce-tests)
  (displayln "\nAll tests completed!"))
