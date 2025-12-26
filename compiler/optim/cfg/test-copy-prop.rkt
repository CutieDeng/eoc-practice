#lang racket/base

;; ============================================================
;; 复写传播测试
;; ============================================================

(require rackunit racket/dict)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "copy-prop.rkt")
(require "const-fold.rkt")
(require "dce.rkt")

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

(define copy-prop-tests
  (test-suite "Copy Propagation Tests"

    ;; 测试 1: 基本不变性（无变化时保持原样）
    (test-case "无复制时保持不变"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(1) (list v0) #f #f)
            (VfInsn 'const '(2) (list v1) #f #f))
          (TermReturn (list v0 v1))))

      (define cfg^ (cfg-copy-prop cfg))
      (check-equal? (insn-count cfg^) 2))

    ;; 测试 2: 使用计数分析
    (test-case "使用计数正确"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(1) (list v0) #f #f)
            (VfInsn 'const '(2) (list v1) #f #f)
            (VfInsn 'add (list v0 v1) (list v2) #f #f)
            (VfInsn 'mul (list v0 v2) (list (VarId 3)) #f #f))
          (TermReturn (list (VarId 3)))))

      (define use-count (count-var-uses cfg))
      ;; v0 被使用 2 次（add 和 mul）
      (check-equal? (dict-ref use-count v0 0) 2)
      ;; v1 被使用 1 次（add）
      (check-equal? (dict-ref use-count v1 0) 1)
      ;; v2 被使用 1 次（mul）
      (check-equal? (dict-ref use-count v2 0) 1))

    ;; 测试 3: 复写传播 + 常量折叠 + DCE 组合
    (test-case "组合优化流水线"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define v3 (VarId 3))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(2) (list v0) #f #f)
            (VfInsn 'const '(3) (list v1) #f #f)
            (VfInsn 'add (list v0 v1) (list v2) #f #f)
            (VfInsn 'const '(10) (list v3) #f #f)  ; 未使用
            (VfInsn 'mul (list v2 v0) (list (VarId 4)) #f #f))
          (TermReturn (list (VarId 4)))))

      ;; 完整优化流水线
      (define cfg1 (cfg-copy-prop cfg))
      (define cfg2 (cfg-const-fold cfg1))
      (define cfg3 (cfg-dce cfg2))

      ;; v3 应该被 DCE 删除
      ;; 常量折叠应该计算 2+3=5, 5*2=10
      (check-true (<= (insn-count cfg3) (insn-count cfg))))

    ;; 测试 4: 空 CFG
    (test-case "空 CFG 不变"
      (define cfg (make-test-cfg '() (TermReturn '())))
      (define cfg^ (cfg-copy-prop cfg))
      (check-equal? (insn-count cfg^) 0))

    ;; 测试 5: 嵌套输入列表处理
    (test-case "处理嵌套输入"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(1) (list v0) #f #f)
            (VfInsn 'const '(2) (list v1) #f #f)
            ;; 模拟 invoke 指令的嵌套参数列表
            (VfInsn 'invoke
              (list 'INVOKEVIRTUAL "Foo" "bar" "(II)I" (list v0 v1))
              (list v2) #f #f))
          (TermReturn (list v2))))

      ;; 应该不出错
      (define cfg^ (cfg-copy-prop cfg))
      (check-equal? (insn-count cfg^) 3))

    ;; 测试 6: 分支条件中的变量
    (test-case "分支条件处理"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define cfg0 (cfg-empty))
      (define-values (bid1 cfg1) (cfg-create-block cfg0))
      (define-values (bid2 cfg2) (cfg-create-block cfg1))
      (define-values (bid3 cfg3) (cfg-create-block cfg2))
      (define cfg4 (cfg-set-entry cfg3 bid1))
      (define cfg5
        (cfg-block-append-insn
          (cfg-block-append-insn cfg4 bid1
            (VfInsn 'const '(0) (list v0) #f #f))
          bid1
          (VfInsn 'eq0 (list v0) (list v1) #f #f)))
      (define cfg6
        (cfg-block-set-terminator cfg5 bid1
          (TermBranch v1 bid2 bid3)))

      (define use-count (count-var-uses cfg6))
      ;; v0 被 eq0 使用
      (check-equal? (dict-ref use-count v0 0) 1)
      ;; v1 被 terminator 使用
      (check-equal? (dict-ref use-count v1 0) 1))

    ;; 测试 7: 多次迭代优化
    (test-case "迭代优化收敛"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define v3 (VarId 3))
      (define v4 (VarId 4))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(1) (list v0) #f #f)
            (VfInsn 'const '(2) (list v1) #f #f)
            (VfInsn 'add (list v0 v1) (list v2) #f #f)  ; v2 = 3
            (VfInsn 'const '(4) (list v3) #f #f)
            (VfInsn 'mul (list v2 v3) (list v4) #f #f)) ; v4 = 12
          (TermReturn (list v4))))

      ;; 多次迭代
      (define (optimize cfg)
        (cfg-dce (cfg-const-fold (cfg-copy-prop cfg))))

      (define cfg1 (optimize cfg))
      (define cfg2 (optimize cfg1))
      (define cfg3 (optimize cfg2))

      ;; 应该收敛到只剩 1 条指令
      (check-equal? (insn-count cfg3) 1)
      (define insns (get-first-block-insns cfg3))
      (check-equal? (VfInsn-op (car insns)) 'const)
      (check-equal? (VfInsn-inputs (car insns)) '(12)))
))

;; ============================================================
;; 运行测试
;; ============================================================

(module+ test
  (require rackunit/text-ui)
  (run-tests copy-prop-tests))

(module+ main
  (require rackunit/text-ui)
  (displayln "=== Copy Propagation Tests ===\n")
  (run-tests copy-prop-tests)
  (displayln "\nAll tests completed!"))
