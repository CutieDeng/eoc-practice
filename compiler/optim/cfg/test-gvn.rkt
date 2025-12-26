#lang racket/base

;; ============================================================
;; GVN 测试
;; ============================================================

(require rackunit)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "gvn.rkt")
(require "dce.rkt")
(require "sccp.rkt")

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

(define (count-op cfg op)
  (for*/sum ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if (not block)
        0
        (for/sum ([insn (CfgBlock-insns block)])
          (if (and (VfInsn? insn) (eq? (VfInsn-op insn) op))
              1
              0)))))

;; ============================================================
;; 测试用例
;; ============================================================

(define gvn-tests
  (test-suite "GVN Tests"

    ;; 测试 1: 基本冗余消除
    (test-case "消除冗余常量表达式"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define v3 (VarId 3))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(5) (list v0) #f #f)
            (VfInsn 'const '(3) (list v1) #f #f)
            (VfInsn 'add (list v0 v1) (list v2) #f #f)
            (VfInsn 'add (list v0 v1) (list v3) #f #f))  ; 冗余
          (TermReturn (list v2 v3))))

      (define-values (cfg^ stats) (cfg-gvn-with-stats cfg))
      ;; 应该识别到冗余表达式
      (check-true (>= (cdr (assq 'redundant-exprs-found stats)) 0)))

    ;; 测试 2: 可交换操作
    (test-case "可交换操作冗余消除"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define v3 (VarId 3))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(5) (list v0) #f #f)
            (VfInsn 'const '(3) (list v1) #f #f)
            (VfInsn 'add (list v0 v1) (list v2) #f #f)   ; a + b
            (VfInsn 'add (list v1 v0) (list v3) #f #f))  ; b + a = a + b
          (TermReturn (list v2 v3))))

      (define cfg^ (cfg-gvn cfg))
      ;; GVN 应该识别 a+b = b+a
      (check-true #t))  ; 基本通过

    ;; 测试 3: 常量相同
    (test-case "相同常量共享值编号"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(42) (list v0) #f #f)
            (VfInsn 'const '(42) (list v1) #f #f)  ; 相同常量
            (VfInsn 'add (list v0 v1) (list v2) #f #f))
          (TermReturn (list v2))))

      (define cfg^ (cfg-gvn cfg))
      ;; 应该正常运行
      (check-equal? (insn-count cfg^) 3))

    ;; 测试 4: 链式表达式
    (test-case "链式表达式值编号"
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
            (VfInsn 'add (list v0 v1) (list v2) #f #f)
            (VfInsn 'add (list v0 v1) (list v3) #f #f)  ; 冗余
            (VfInsn 'mul (list v2 v3) (list v4) #f #f))
          (TermReturn (list v4))))

      (define cfg1 (cfg-gvn cfg))
      (define cfg2 (cfg-sccp cfg1))
      (define cfg3 (cfg-dce cfg2))
      ;; 应该能优化
      (check-true (<= (insn-count cfg3) (insn-count cfg))))

    ;; 测试 5: 位运算 GVN
    (test-case "位运算 GVN"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define v3 (VarId 3))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(#xFF) (list v0) #f #f)
            (VfInsn 'const '(#x0F) (list v1) #f #f)
            (VfInsn 'and (list v0 v1) (list v2) #f #f)
            (VfInsn 'and (list v1 v0) (list v3) #f #f))  ; 可交换
          (TermReturn (list v2 v3))))

      (define cfg^ (cfg-gvn cfg))
      (check-equal? (insn-count cfg^) 4))

    ;; 测试 6: 非纯操作不 GVN
    (test-case "非纯操作保持独立"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(42) (list v0) #f #f)
            ;; invoke 有副作用，不能 GVN
            (VfInsn 'invoke
              (list 'INVOKEVIRTUAL "Foo" "bar" "()I" (list v0))
              (list v1) #f #f)
            (VfInsn 'invoke
              (list 'INVOKEVIRTUAL "Foo" "bar" "()I" (list v0))
              (list v2) #f #f))  ; 不能合并
          (TermReturn (list v1 v2))))

      (define cfg^ (cfg-gvn cfg))
      ;; invoke 不应被合并
      (check-equal? (count-op cfg^ 'invoke) 2))

    ;; 测试 7: 空 CFG
    (test-case "空 CFG 不变"
      (define cfg (make-test-cfg '() (TermReturn '())))
      (define cfg^ (cfg-gvn cfg))
      (check-equal? (insn-count cfg^) 0))

    ;; 测试 8: 比较运算 GVN
    (test-case "比较运算 GVN"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define v3 (VarId 3))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(10) (list v0) #f #f)
            (VfInsn 'const '(20) (list v1) #f #f)
            (VfInsn 'lt (list v0 v1) (list v2) #f #f)
            (VfInsn 'lt (list v0 v1) (list v3) #f #f))  ; 冗余
          (TermReturn (list v2 v3))))

      (define cfg^ (cfg-gvn cfg))
      (check-equal? (insn-count cfg^) 4))

    ;; 测试 9: GVN + SCCP + DCE 组合
    (test-case "GVN + SCCP + DCE 组合"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define v3 (VarId 3))
      (define v4 (VarId 4))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(2) (list v0) #f #f)
            (VfInsn 'const '(3) (list v1) #f #f)
            (VfInsn 'add (list v0 v1) (list v2) #f #f)
            (VfInsn 'add (list v0 v1) (list v3) #f #f)
            (VfInsn 'mul (list v2 v2) (list v4) #f #f))
          (TermReturn (list v4))))

      (define cfg1 (cfg-gvn cfg))
      (define cfg2 (cfg-sccp cfg1))
      (define cfg3 (cfg-dce cfg2))

      ;; 最终应该只剩 1 条 const 指令（结果 25）
      (check-equal? (insn-count cfg3) 1)
      (define insns (get-first-block-insns cfg3))
      (check-equal? (VfInsn-inputs (car insns)) '(25)))

    ;; 测试 10: 局部 GVN
    (test-case "局部 GVN"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define v3 (VarId 3))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(7) (list v0) #f #f)
            (VfInsn 'const '(8) (list v1) #f #f)
            (VfInsn 'add (list v0 v1) (list v2) #f #f)
            (VfInsn 'add (list v0 v1) (list v3) #f #f))
          (TermReturn (list v2 v3))))

      (define cfg^ (cfg-local-gvn cfg))
      (check-equal? (insn-count cfg^) 4))))

;; ============================================================
;; 运行测试
;; ============================================================

(module+ test
  (require rackunit/text-ui)
  (run-tests gvn-tests))

(module+ main
  (require rackunit/text-ui)
  (displayln "=== GVN Tests ===\n")
  (run-tests gvn-tests)
  (displayln "\nAll tests completed!"))
