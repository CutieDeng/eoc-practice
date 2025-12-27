#lang racket/base

;; ============================================================
;; 强度削减测试
;; ============================================================

(require rackunit)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "strength-reduce.rkt")
(require "sccp.rkt")
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

(define (find-insn-by-op cfg op)
  (for*/first ([bid (cfg-all-block-ids cfg)]
               [block (in-value (cfg-get-block cfg bid))]
               #:when block
               [insn (CfgBlock-insns block)]
               #:when (and (VfInsn? insn) (eq? (VfInsn-op insn) op)))
    insn))

(define (count-op cfg op)
  (for*/sum ([bid (cfg-all-block-ids cfg)]
             [block (in-value (cfg-get-block cfg bid))]
             #:when block
             [insn (CfgBlock-insns block)]
             #:when (and (VfInsn? insn) (eq? (VfInsn-op insn) op)))
    1))

;; ============================================================
;; 测试用例
;; ============================================================

(define strength-reduce-tests
  (test-suite "Strength Reduction Tests"

    ;; 测试 1: 乘以 2 的幂 → 左移
    (test-case "乘以 2 的幂转左移"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(10) (list v0) #f #f)
            (VfInsn 'mul (list v0 8) (list v1) #f #f))  ; x * 8 → x << 3
          (TermReturn (list v1))))

      (define cfg^ (cfg-strength-reduce cfg))
      (define shl-insn (find-insn-by-op cfg^ 'shl))
      (check-true (VfInsn? shl-insn))
      (check-equal? (cadr (VfInsn-inputs shl-insn)) 3))  ; shift by 3

    ;; 测试 2: 乘以 3 → 移位 + 加法
    (test-case "乘以 3 转移位加法"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(10) (list v0) #f #f)
            (VfInsn 'mul (list v0 3) (list v1) #f #f))  ; x * 3 → (x << 1) + x
          (TermReturn (list v1))))

      (define cfg^ (cfg-strength-reduce cfg))
      ;; 应该有 shl 和 add
      (check-equal? (count-op cfg^ 'shl) 1)
      (check-equal? (count-op cfg^ 'add) 1)
      (check-equal? (count-op cfg^ 'mul) 0))

    ;; 测试 3: 乘以 5 → 移位 + 加法
    (test-case "乘以 5 转移位加法"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(10) (list v0) #f #f)
            (VfInsn 'mul (list v0 5) (list v1) #f #f))  ; x * 5 → (x << 2) + x
          (TermReturn (list v1))))

      (define cfg^ (cfg-strength-reduce cfg))
      (check-equal? (count-op cfg^ 'shl) 1)
      (check-equal? (count-op cfg^ 'add) 1)
      (check-equal? (count-op cfg^ 'mul) 0))

    ;; 测试 4: 乘以 7 → 移位 - 减法
    (test-case "乘以 7 转移位减法"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(10) (list v0) #f #f)
            (VfInsn 'mul (list v0 7) (list v1) #f #f))  ; x * 7 → (x << 3) - x
          (TermReturn (list v1))))

      (define cfg^ (cfg-strength-reduce cfg))
      (check-equal? (count-op cfg^ 'shl) 1)
      (check-equal? (count-op cfg^ 'sub) 1)
      (check-equal? (count-op cfg^ 'mul) 0))

    ;; 测试 5: 乘以 0 → 常量 0
    (test-case "乘以 0 转常量"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(10) (list v0) #f #f)
            (VfInsn 'mul (list v0 0) (list v1) #f #f))
          (TermReturn (list v1))))

      (define cfg^ (cfg-strength-reduce cfg))
      (check-equal? (count-op cfg^ 'mul) 0)
      ;; 应该变成 const
      (define insns (get-first-block-insns cfg^))
      (define last-insn (cadr insns))
      (check-equal? (VfInsn-op last-insn) 'const)
      (check-equal? (VfInsn-inputs last-insn) '(0)))

    ;; 测试 6: 无符号除以 2 的幂 → 右移
    (test-case "无符号除以 2 的幂转右移"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(100) (list v0) #f #f)
            (VfInsn 'udiv (list v0 4) (list v1) #f #f))  ; x / 4 → x >>> 2
          (TermReturn (list v1))))

      (define cfg^ (cfg-strength-reduce cfg))
      (check-equal? (count-op cfg^ 'ushr) 1)
      (check-equal? (count-op cfg^ 'udiv) 0))

    ;; 测试 7: 无符号模 2 的幂 → 按位与
    (test-case "无符号模 2 的幂转按位与"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(100) (list v0) #f #f)
            (VfInsn 'urem (list v0 16) (list v1) #f #f))  ; x % 16 → x & 15
          (TermReturn (list v1))))

      (define cfg^ (cfg-strength-reduce cfg))
      (check-equal? (count-op cfg^ 'and) 1)
      (check-equal? (count-op cfg^ 'urem) 0)
      (define and-insn (find-insn-by-op cfg^ 'and))
      (check-equal? (cadr (VfInsn-inputs and-insn)) 15))

    ;; 测试 8: 代数简化 x - x = 0
    (test-case "x - x = 0"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(10) (list v0) #f #f)
            (VfInsn 'sub (list v0 v0) (list v1) #f #f))
          (TermReturn (list v1))))

      (define cfg^ (cfg-algebraic-simplify cfg))
      (define insns (get-first-block-insns cfg^))
      (define last-insn (cadr insns))
      (check-equal? (VfInsn-op last-insn) 'const)
      (check-equal? (VfInsn-inputs last-insn) '(0)))

    ;; 测试 9: 代数简化 x ^ x = 0
    (test-case "x ^ x = 0"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(10) (list v0) #f #f)
            (VfInsn 'xor (list v0 v0) (list v1) #f #f))
          (TermReturn (list v1))))

      (define cfg^ (cfg-algebraic-simplify cfg))
      (define insns (get-first-block-insns cfg^))
      (define last-insn (cadr insns))
      (check-equal? (VfInsn-op last-insn) 'const)
      (check-equal? (VfInsn-inputs last-insn) '(0)))

    ;; 测试 10: 代数简化 x & 0 = 0
    (test-case "x & 0 = 0"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(10) (list v0) #f #f)
            (VfInsn 'and (list v0 0) (list v1) #f #f))
          (TermReturn (list v1))))

      (define cfg^ (cfg-algebraic-simplify cfg))
      (define insns (get-first-block-insns cfg^))
      (define last-insn (cadr insns))
      (check-equal? (VfInsn-op last-insn) 'const)
      (check-equal? (VfInsn-inputs last-insn) '(0)))

    ;; 测试 11: 组合优化
    (test-case "组合强度削减和常量传播"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(10) (list v0) #f #f)
            (VfInsn 'mul (list v0 8) (list v1) #f #f)   ; 10 * 8 = 80
            (VfInsn 'mul (list v1 2) (list v2) #f #f))  ; 80 * 2 = 160
          (TermReturn (list v2))))

      ;; 先强度削减
      (define cfg1 (cfg-strength-reduce cfg))
      ;; 再 SCCP
      (define cfg2 (cfg-sccp cfg1))
      ;; 再 DCE
      (define cfg3 (cfg-dce cfg2))

      ;; 最终应该只剩 1 条 const
      (check-equal? (insn-count cfg3) 1)
      (define insns (get-first-block-insns cfg3))
      (check-equal? (VfInsn-inputs (car insns)) '(160)))

    ;; 测试 12: 不优化非 2 的幂
    (test-case "非 2 的幂的除法不优化"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(100) (list v0) #f #f)
            (VfInsn 'udiv (list v0 3) (list v1) #f #f))  ; 3 不是 2 的幂
          (TermReturn (list v1))))

      (define cfg^ (cfg-strength-reduce cfg))
      ;; udiv 应该保留
      (check-equal? (count-op cfg^ 'udiv) 1))))

;; ============================================================
;; 运行测试
;; ============================================================

(module+ test
  (require rackunit/text-ui)
  (run-tests strength-reduce-tests))

(module+ main
  (require rackunit/text-ui)
  (displayln "=== Strength Reduction Tests ===\n")
  (run-tests strength-reduce-tests)
  (displayln "\nAll tests completed!"))
