#lang racket/base

;; ============================================================
;; 常量折叠优化测试
;; ============================================================

(require rackunit)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "const-fold.rkt")

;; ============================================================
;; 辅助函数：构建测试 CFG
;; ============================================================

;; 创建简单的测试 CFG
(define (make-test-cfg insns)
  (define cfg0 (cfg-empty))
  (define-values (bid cfg1) (cfg-create-block cfg0))
  (define cfg2 (cfg-set-entry cfg1 bid))
  ;; 添加指令
  (for/fold ([cfg cfg2])
            ([insn insns])
    (cfg-block-append-insn cfg bid insn)))

;; 获取块中的指令列表
(define (get-block-insns cfg bid)
  (define block (cfg-get-block cfg bid))
  (and block (CfgBlock-insns block)))

;; 获取第一个块的指令
(define (get-first-block-insns cfg)
  (get-block-insns cfg (cfg-get-entry cfg)))

;; ============================================================
;; 测试用例
;; ============================================================

(define const-fold-tests
  (test-suite "Constant Folding Tests"

    ;; 测试 1: 简单加法折叠
    (test-case "加法常量折叠: 1 + 2 = 3"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(1) (list v0) #f #f)
            (VfInsn 'const '(2) (list v1) #f #f)
            (VfInsn 'add (list v0 v1) (list v2) #f #f))))

      (define cfg^ (cfg-const-fold cfg))
      (define insns (get-first-block-insns cfg^))

      ;; 应该有 3 条指令，最后一条被折叠为 const 3
      (check-equal? (length insns) 3)
      (define last-insn (list-ref insns 2))
      (check-equal? (VfInsn-op last-insn) 'const)
      (check-equal? (VfInsn-inputs last-insn) '(3)))

    ;; 测试 2: 乘法折叠
    (test-case "乘法常量折叠: 3 * 4 = 12"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(3) (list v0) #f #f)
            (VfInsn 'const '(4) (list v1) #f #f)
            (VfInsn 'mul (list v0 v1) (list v2) #f #f))))

      (define cfg^ (cfg-const-fold cfg))
      (define insns (get-first-block-insns cfg^))
      (define last-insn (list-ref insns 2))
      (check-equal? (VfInsn-op last-insn) 'const)
      (check-equal? (VfInsn-inputs last-insn) '(12)))

    ;; 测试 3: 链式折叠 (1 + 2) * 3
    (test-case "链式折叠: (1 + 2) * 3 = 9"
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
            (VfInsn 'const '(3) (list v3) #f #f)
            (VfInsn 'mul (list v2 v3) (list v4) #f #f)))) ; v4 = 9

      (define cfg^ (cfg-const-fold cfg))
      (define insns (get-first-block-insns cfg^))
      (define last-insn (list-ref insns 4))
      (check-equal? (VfInsn-op last-insn) 'const)
      (check-equal? (VfInsn-inputs last-insn) '(9)))

    ;; 测试 4: 位运算折叠
    (test-case "位运算折叠: 5 & 3 = 1"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(5) (list v0) #f #f)
            (VfInsn 'const '(3) (list v1) #f #f)
            (VfInsn 'and (list v0 v1) (list v2) #f #f))))

      (define cfg^ (cfg-const-fold cfg))
      (define insns (get-first-block-insns cfg^))
      (define last-insn (list-ref insns 2))
      (check-equal? (VfInsn-op last-insn) 'const)
      (check-equal? (VfInsn-inputs last-insn) '(1)))

    ;; 测试 5: 一元运算折叠
    (test-case "一元运算折叠: -5 = -5"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(5) (list v0) #f #f)
            (VfInsn 'neg (list v0) (list v1) #f #f))))

      (define cfg^ (cfg-const-fold cfg))
      (define insns (get-first-block-insns cfg^))
      (define last-insn (list-ref insns 1))
      (check-equal? (VfInsn-op last-insn) 'const)
      (check-equal? (VfInsn-inputs last-insn) '(-5)))

    ;; 测试 6: 比较运算折叠
    (test-case "比较折叠: 5 > 3 = 1"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(5) (list v0) #f #f)
            (VfInsn 'const '(3) (list v1) #f #f)
            (VfInsn 'gt (list v0 v1) (list v2) #f #f))))

      (define cfg^ (cfg-const-fold cfg))
      (define insns (get-first-block-insns cfg^))
      (define last-insn (list-ref insns 2))
      (check-equal? (VfInsn-op last-insn) 'const)
      (check-equal? (VfInsn-inputs last-insn) '(1)))

    ;; 测试 7: 与零比较折叠
    (test-case "与零比较折叠: 0 == 0 -> 1"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(0) (list v0) #f #f)
            (VfInsn 'eq0 (list v0) (list v1) #f #f))))

      (define cfg^ (cfg-const-fold cfg))
      (define insns (get-first-block-insns cfg^))
      (define last-insn (list-ref insns 1))
      (check-equal? (VfInsn-op last-insn) 'const)
      (check-equal? (VfInsn-inputs last-insn) '(1)))

    ;; 测试 8: 无法折叠（非常量输入）
    (test-case "无法折叠：非常量输入保持不变"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'load-local '(0) (list v0) #f #f)  ; 非常量
            (VfInsn 'const '(2) (list v1) #f #f)
            (VfInsn 'add (list v0 v1) (list v2) #f #f))))

      (define cfg^ (cfg-const-fold cfg))
      (define insns (get-first-block-insns cfg^))
      (define last-insn (list-ref insns 2))
      ;; add 指令应该保持不变
      (check-equal? (VfInsn-op last-insn) 'add))

    ;; 测试 9: 移位运算折叠
    (test-case "移位折叠: 8 >> 2 = 2"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(8) (list v0) #f #f)
            (VfInsn 'const '(2) (list v1) #f #f)
            (VfInsn 'shr (list v0 v1) (list v2) #f #f))))

      (define cfg^ (cfg-const-fold cfg))
      (define insns (get-first-block-insns cfg^))
      (define last-insn (list-ref insns 2))
      (check-equal? (VfInsn-op last-insn) 'const)
      (check-equal? (VfInsn-inputs last-insn) '(2)))

    ;; 测试 10: 除零保护
    (test-case "除零不折叠"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(10) (list v0) #f #f)
            (VfInsn 'const '(0) (list v1) #f #f)
            (VfInsn 'div (list v0 v1) (list v2) #f #f))))

      (define cfg^ (cfg-const-fold cfg))
      (define insns (get-first-block-insns cfg^))
      (define last-insn (list-ref insns 2))
      ;; div 指令应该保持不变（避免除零）
      (check-equal? (VfInsn-op last-insn) 'div))
))

;; ============================================================
;; 运行测试
;; ============================================================

(module+ test
  (require rackunit/text-ui)
  (run-tests const-fold-tests))

(module+ main
  (require rackunit/text-ui)
  (displayln "=== Constant Folding Tests ===\n")
  (run-tests const-fold-tests)
  (displayln "\nAll tests completed!"))
