#lang racket/base

;; ============================================================
;; SCCP 测试
;; ============================================================

(require rackunit)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
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

;; ============================================================
;; 格运算测试
;; ============================================================

(define lattice-tests
  (test-suite "Lattice Tests"

    (test-case "TOP meet x = x"
      (check-equal? (lattice-meet TOP (LatticeConst 5)) (LatticeConst 5))
      (check-equal? (lattice-meet TOP BOTTOM) BOTTOM)
      (check-equal? (lattice-meet TOP TOP) TOP))

    (test-case "x meet TOP = x"
      (check-equal? (lattice-meet (LatticeConst 5) TOP) (LatticeConst 5))
      (check-equal? (lattice-meet BOTTOM TOP) BOTTOM))

    (test-case "BOTTOM meet x = BOTTOM"
      (check-equal? (lattice-meet BOTTOM (LatticeConst 5)) BOTTOM)
      (check-equal? (lattice-meet BOTTOM TOP) BOTTOM))

    (test-case "const meet const"
      (check-equal? (lattice-meet (LatticeConst 5) (LatticeConst 5))
                    (LatticeConst 5))
      (check-equal? (lattice-meet (LatticeConst 5) (LatticeConst 3))
                    BOTTOM))))

;; ============================================================
;; SCCP 基本测试
;; ============================================================

(define sccp-tests
  (test-suite "SCCP Tests"

    ;; 测试 1: 基本常量传播
    (test-case "基本常量传播"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(2) (list v0) #f #f)
            (VfInsn 'const '(3) (list v1) #f #f)
            (VfInsn 'add (list v0 v1) (list v2) #f #f))
          (TermReturn (list v2))))

      (define cfg^ (cfg-sccp cfg))
      (define insns (get-first-block-insns cfg^))
      ;; v2 应该被折叠为常量 5
      (define last-insn (list-ref insns 2))
      (check-equal? (VfInsn-op last-insn) 'const)
      (check-equal? (VfInsn-inputs last-insn) '(5)))

    ;; 测试 2: 链式常量传播
    (test-case "链式常量传播"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define v3 (VarId 3))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(2) (list v0) #f #f)
            (VfInsn 'const '(3) (list v1) #f #f)
            (VfInsn 'add (list v0 v1) (list v2) #f #f)    ; v2 = 5
            (VfInsn 'mul (list v2 v0) (list v3) #f #f))   ; v3 = 10
          (TermReturn (list v3))))

      (define cfg^ (cfg-sccp cfg))
      (define insns (get-first-block-insns cfg^))
      (define last-insn (list-ref insns 3))
      (check-equal? (VfInsn-op last-insn) 'const)
      (check-equal? (VfInsn-inputs last-insn) '(10)))

    ;; 测试 3: 条件常量 - 真分支
    (test-case "条件常量折叠 - 真分支"
      (define v0 (VarId 0))  ; 条件 = 1 (真)
      (define cfg0 (cfg-empty))
      (define-values (bid1 cfg1) (cfg-create-block cfg0))
      (define-values (bid2 cfg2) (cfg-create-block cfg1))
      (define-values (bid3 cfg3) (cfg-create-block cfg2))
      (define cfg4 (cfg-set-entry cfg3 bid1))

      ;; 入口块
      (define cfg5
        (cfg-block-append-insn cfg4 bid1
          (VfInsn 'const '(1) (list v0) #f #f)))
      (define cfg6
        (cfg-block-set-terminator cfg5 bid1
          (TermBranch v0 bid2 bid3)))

      ;; 设置其他块的 terminator
      (define cfg7
        (cfg-block-set-terminator cfg6 bid2 (TermReturn '())))
      (define cfg8
        (cfg-block-set-terminator cfg7 bid3 (TermReturn '())))

      (define-values (cfg^ stats) (cfg-sccp-with-stats cfg8))
      ;; 只有 bid1 和 bid2 应该可达
      (check-equal? (cdr (assq 'reachable-blocks stats)) 2))

    ;; 测试 4: 条件常量 - 假分支
    (test-case "条件常量折叠 - 假分支"
      (define v0 (VarId 0))  ; 条件 = 0 (假)
      (define cfg0 (cfg-empty))
      (define-values (bid1 cfg1) (cfg-create-block cfg0))
      (define-values (bid2 cfg2) (cfg-create-block cfg1))
      (define-values (bid3 cfg3) (cfg-create-block cfg2))
      (define cfg4 (cfg-set-entry cfg3 bid1))

      (define cfg5
        (cfg-block-append-insn cfg4 bid1
          (VfInsn 'const '(0) (list v0) #f #f)))
      (define cfg6
        (cfg-block-set-terminator cfg5 bid1
          (TermBranch v0 bid2 bid3)))

      (define cfg7
        (cfg-block-set-terminator cfg6 bid2 (TermReturn '())))
      (define cfg8
        (cfg-block-set-terminator cfg7 bid3 (TermReturn '())))

      (define-values (cfg^ stats) (cfg-sccp-with-stats cfg8))
      ;; 只有 bid1 和 bid3 应该可达
      (check-equal? (cdr (assq 'reachable-blocks stats)) 2))

    ;; 测试 5: 比较运算常量折叠
    (test-case "比较运算常量折叠"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(5) (list v0) #f #f)
            (VfInsn 'const '(3) (list v1) #f #f)
            (VfInsn 'gt (list v0 v1) (list v2) #f #f))  ; 5 > 3 = 1
          (TermReturn (list v2))))

      (define cfg^ (cfg-sccp cfg))
      (define insns (get-first-block-insns cfg^))
      (define last-insn (list-ref insns 2))
      (check-equal? (VfInsn-op last-insn) 'const)
      (check-equal? (VfInsn-inputs last-insn) '(1)))

    ;; 测试 6: 位运算常量折叠
    (test-case "位运算常量折叠"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(#b1010) (list v0) #f #f)
            (VfInsn 'const '(#b1100) (list v1) #f #f)
            (VfInsn 'and (list v0 v1) (list v2) #f #f))  ; 1010 & 1100 = 1000
          (TermReturn (list v2))))

      (define cfg^ (cfg-sccp cfg))
      (define insns (get-first-block-insns cfg^))
      (define last-insn (list-ref insns 2))
      (check-equal? (VfInsn-op last-insn) 'const)
      (check-equal? (VfInsn-inputs last-insn) '(8)))  ; #b1000 = 8

    ;; 测试 7: SCCP + DCE 组合
    (test-case "SCCP + DCE 组合优化"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define v3 (VarId 3))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(10) (list v0) #f #f)
            (VfInsn 'const '(20) (list v1) #f #f)
            (VfInsn 'add (list v0 v1) (list v2) #f #f)
            (VfInsn 'const '(99) (list v3) #f #f))  ; 未使用
          (TermReturn (list v2))))

      (define cfg1 (cfg-sccp cfg))
      (define cfg2 (cfg-dce cfg1))
      ;; 应该只剩 1 条 const 指令
      (check-equal? (insn-count cfg2) 1)
      (define insns (get-first-block-insns cfg2))
      (check-equal? (VfInsn-inputs (car insns)) '(30)))

    ;; 测试 8: 空 CFG
    (test-case "空 CFG 不变"
      (define cfg (make-test-cfg '() (TermReturn '())))
      (define cfg^ (cfg-sccp cfg))
      (check-equal? (insn-count cfg^) 0))

    ;; 测试 9: 除零保护
    (test-case "除零不折叠"
      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define cfg
        (make-test-cfg
          (list
            (VfInsn 'const '(10) (list v0) #f #f)
            (VfInsn 'const '(0) (list v1) #f #f)
            (VfInsn 'div (list v0 v1) (list v2) #f #f))
          (TermReturn (list v2))))

      (define cfg^ (cfg-sccp cfg))
      (define insns (get-first-block-insns cfg^))
      ;; div 不应被折叠（除零）
      (define last-insn (list-ref insns 2))
      (check-equal? (VfInsn-op last-insn) 'div))))

;; ============================================================
;; 运行测试
;; ============================================================

(module+ test
  (require rackunit/text-ui)
  (run-tests lattice-tests)
  (run-tests sccp-tests))

(module+ main
  (require rackunit/text-ui)
  (displayln "=== SCCP Tests ===\n")
  (displayln "--- Lattice Tests ---")
  (run-tests lattice-tests)
  (displayln "\n--- SCCP Tests ---")
  (run-tests sccp-tests)
  (displayln "\nAll tests completed!"))
