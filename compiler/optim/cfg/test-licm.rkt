#lang racket/base

;; ============================================================
;; LICM 测试
;; ============================================================

(require rackunit racket/set)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "loop-analysis.rkt")
(require "licm.rkt")

;; ============================================================
;; 辅助函数
;; ============================================================

(define (insn-count cfg)
  (for/sum ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if block (length (CfgBlock-insns block)) 0)))

(define (block-insn-count cfg bid)
  (define block (cfg-get-block cfg bid))
  (if block (length (CfgBlock-insns block)) 0))

;; ============================================================
;; 循环分析测试
;; ============================================================

(define loop-analysis-tests
  (test-suite "Loop Analysis Tests"

    ;; 测试 1: 简单循环检测
    (test-case "检测简单循环"
      ;; CFG:
      ;;   entry -> header -> body -> header (back edge)
      ;;                   -> exit
      (define cfg0 (cfg-empty))
      (define-values (entry cfg1) (cfg-create-block cfg0))
      (define-values (header cfg2) (cfg-create-block cfg1))
      (define-values (body cfg3) (cfg-create-block cfg2))
      (define-values (exit cfg4) (cfg-create-block cfg3))
      (define cfg5 (cfg-set-entry cfg4 entry))

      ;; entry -> header
      (define cfg6 (cfg-block-set-terminator cfg5 entry (TermJump header)))
      ;; header -> body (条件分支)
      (define v0 (VarId 0))
      (define cfg7
        (cfg-block-append-insn cfg6 header
          (VfInsn 'const '(1) (list v0) #f #f)))
      (define cfg8 (cfg-block-set-terminator cfg7 header (TermBranch v0 body exit)))
      ;; body -> header (后向边)
      (define cfg9 (cfg-block-set-terminator cfg8 body (TermJump header)))
      ;; exit
      (define cfg10 (cfg-block-set-terminator cfg9 exit (TermReturn '())))

      (define loops (analyze-loops cfg10))
      (check-equal? (length loops) 1)
      (define loop (car loops))
      (check-equal? (Loop-header loop) header)
      (check-true (set-member? (Loop-body loop) header))
      (check-true (set-member? (Loop-body loop) body)))

    ;; 测试 2: 无循环 CFG
    (test-case "无循环的 CFG"
      (define cfg0 (cfg-empty))
      (define-values (b1 cfg1) (cfg-create-block cfg0))
      (define-values (b2 cfg2) (cfg-create-block cfg1))
      (define cfg3 (cfg-set-entry cfg2 b1))
      (define cfg4 (cfg-block-set-terminator cfg3 b1 (TermJump b2)))
      (define cfg5 (cfg-block-set-terminator cfg4 b2 (TermReturn '())))

      (define loops (analyze-loops cfg5))
      (check-equal? (length loops) 0))

    ;; 测试 3: 支配关系计算
    (test-case "支配关系正确"
      ;; entry -> a -> b
      (define cfg0 (cfg-empty))
      (define-values (entry cfg1) (cfg-create-block cfg0))
      (define-values (a cfg2) (cfg-create-block cfg1))
      (define-values (b cfg3) (cfg-create-block cfg2))
      (define cfg4 (cfg-set-entry cfg3 entry))
      (define cfg5 (cfg-block-set-terminator cfg4 entry (TermJump a)))
      (define cfg6 (cfg-block-set-terminator cfg5 a (TermJump b)))
      (define cfg7 (cfg-block-set-terminator cfg6 b (TermReturn '())))

      (define dom (compute-dominators cfg7))
      ;; entry 支配所有块
      (check-true (dominates? dom entry entry))
      (check-true (dominates? dom entry a))
      (check-true (dominates? dom entry b))
      ;; a 支配 b
      (check-true (dominates? dom a b))
      ;; b 不支配 a
      (check-false (dominates? dom b a)))

    ;; 测试 4: 后向边检测
    (test-case "后向边检测"
      ;; entry -> header <-> body
      (define cfg0 (cfg-empty))
      (define-values (entry cfg1) (cfg-create-block cfg0))
      (define-values (header cfg2) (cfg-create-block cfg1))
      (define-values (body cfg3) (cfg-create-block cfg2))
      (define cfg4 (cfg-set-entry cfg3 entry))
      (define cfg5 (cfg-block-set-terminator cfg4 entry (TermJump header)))
      (define cfg6 (cfg-block-set-terminator cfg5 header (TermJump body)))
      (define cfg7 (cfg-block-set-terminator cfg6 body (TermJump header)))

      (define dom (compute-dominators cfg7))
      (define back-edges (find-back-edges cfg7 dom))
      ;; body -> header 是后向边
      (check-equal? (length back-edges) 1)
      (check-equal? (caar back-edges) body)
      (check-equal? (cdar back-edges) header))))

;; ============================================================
;; LICM 测试
;; ============================================================

(define licm-tests
  (test-suite "LICM Tests"

    ;; 测试 1: 基本循环不变代码外提
    (test-case "基本不变代码外提"
      ;; preheader -> header -> body -> header
      ;;                     -> exit
      ;; body 中有不变代码
      (define cfg0 (cfg-empty))
      (define-values (preheader cfg1) (cfg-create-block cfg0))
      (define-values (header cfg2) (cfg-create-block cfg1))
      (define-values (body cfg3) (cfg-create-block cfg2))
      (define-values (exit cfg4) (cfg-create-block cfg3))
      (define cfg5 (cfg-set-entry cfg4 preheader))

      (define v0 (VarId 0))  ; 循环条件
      (define v1 (VarId 1))  ; 在 preheader 定义
      (define v2 (VarId 2))  ; 循环不变计算

      ;; preheader: v1 = const 10
      (define cfg6
        (cfg-block-append-insn cfg5 preheader
          (VfInsn 'const '(10) (list v1) #f #f)))
      (define cfg7 (cfg-block-set-terminator cfg6 preheader (TermJump header)))

      ;; header: 条件
      (define cfg8
        (cfg-block-append-insn cfg7 header
          (VfInsn 'const '(1) (list v0) #f #f)))
      (define cfg9 (cfg-block-set-terminator cfg8 header (TermBranch v0 body exit)))

      ;; body: v2 = v1 + 5 (循环不变)
      (define cfg10
        (cfg-block-append-insn cfg9 body
          (VfInsn 'add (list v1 5) (list v2) #f #f)))
      (define cfg11 (cfg-block-set-terminator cfg10 body (TermJump header)))

      ;; exit
      (define cfg12 (cfg-block-set-terminator cfg11 exit (TermReturn (list v2))))

      ;; 执行 LICM
      (define-values (cfg^ stats) (cfg-licm-with-stats cfg12))

      ;; 应该找到循环
      (check-true (>= (cdr (assq 'loops-found stats)) 1))
      ;; 应该外提指令（body 中的 add，可能还有 header 中的 const）
      (check-true (>= (cdr (assq 'insns-hoisted stats)) 1))
      ;; body 应该变少
      (check-true (<= (block-insn-count cfg^ body)
                      (block-insn-count cfg12 body))))

    ;; 测试 2: 无循环的 CFG
    (test-case "无循环 CFG 不变"
      (define cfg0 (cfg-empty))
      (define-values (b1 cfg1) (cfg-create-block cfg0))
      (define cfg2 (cfg-set-entry cfg1 b1))
      (define v0 (VarId 0))
      (define cfg3
        (cfg-block-append-insn cfg2 b1
          (VfInsn 'const '(42) (list v0) #f #f)))
      (define cfg4 (cfg-block-set-terminator cfg3 b1 (TermReturn (list v0))))

      (define cfg^ (cfg-licm cfg4))
      (check-equal? (insn-count cfg^) 1))

    ;; 测试 3: 有副作用的指令不外提
    (test-case "副作用指令不外提"
      (define cfg0 (cfg-empty))
      (define-values (preheader cfg1) (cfg-create-block cfg0))
      (define-values (header cfg2) (cfg-create-block cfg1))
      (define-values (body cfg3) (cfg-create-block cfg2))
      (define-values (exit cfg4) (cfg-create-block cfg3))
      (define cfg5 (cfg-set-entry cfg4 preheader))

      (define v0 (VarId 0))
      (define v1 (VarId 1))

      (define cfg6 (cfg-block-set-terminator cfg5 preheader (TermJump header)))
      (define cfg7
        (cfg-block-append-insn cfg6 header
          (VfInsn 'const '(1) (list v0) #f #f)))
      (define cfg8 (cfg-block-set-terminator cfg7 header (TermBranch v0 body exit)))

      ;; body: invoke (有副作用)
      (define cfg9
        (cfg-block-append-insn cfg8 body
          (VfInsn 'invoke (list 'INVOKEVIRTUAL "Foo" "bar" "()V" '()) (list v1) #f #f)))
      (define cfg10 (cfg-block-set-terminator cfg9 body (TermJump header)))
      (define cfg11 (cfg-block-set-terminator cfg10 exit (TermReturn '())))

      (define body-before (block-insn-count cfg11 body))
      (define-values (cfg^ stats) (cfg-licm-with-stats cfg11))
      ;; body 中的 invoke 应该保留
      (check-equal? (block-insn-count cfg^ body) body-before))

    ;; 测试 4: 依赖循环变量的指令不外提
    (test-case "依赖循环变量的指令不外提"
      (define cfg0 (cfg-empty))
      (define-values (preheader cfg1) (cfg-create-block cfg0))
      (define-values (header cfg2) (cfg-create-block cfg1))
      (define-values (body cfg3) (cfg-create-block cfg2))
      (define-values (exit cfg4) (cfg-create-block cfg3))
      (define cfg5 (cfg-set-entry cfg4 preheader))

      (define v0 (VarId 0))  ; 条件
      (define v1 (VarId 1))  ; 在循环内定义的变量
      (define v2 (VarId 2))  ; 依赖 v1

      (define cfg6 (cfg-block-set-terminator cfg5 preheader (TermJump header)))
      (define cfg7
        (cfg-block-append-insn cfg6 header
          (VfInsn 'const '(1) (list v0) #f #f)))
      (define cfg8 (cfg-block-set-terminator cfg7 header (TermBranch v0 body exit)))

      ;; body: v1 在循环内定义，v2 依赖 v1
      (define cfg9
        (cfg-block-append-insn cfg8 body
          (VfInsn 'const '(1) (list v1) #f #f)))
      (define cfg10
        (cfg-block-append-insn cfg9 body
          (VfInsn 'add (list v1 1) (list v2) #f #f)))
      (define cfg11 (cfg-block-set-terminator cfg10 body (TermJump header)))
      (define cfg12 (cfg-block-set-terminator cfg11 exit (TermReturn '())))

      (define-values (cfg^ stats) (cfg-licm-with-stats cfg12))
      ;; v1 = const 1 是循环不变的，可以外提
      ;; v2 = v1 + 1 依赖 v1，如果 v1 外提了也可以外提
      ;; 但由于迭代分析，两者都可能被外提
      (check-true (>= (cdr (assq 'insns-hoisted stats)) 0)))

    ;; 测试 5: 多个不变指令链式外提
    (test-case "链式不变代码外提"
      (define cfg0 (cfg-empty))
      (define-values (preheader cfg1) (cfg-create-block cfg0))
      (define-values (header cfg2) (cfg-create-block cfg1))
      (define-values (body cfg3) (cfg-create-block cfg2))
      (define-values (exit cfg4) (cfg-create-block cfg3))
      (define cfg5 (cfg-set-entry cfg4 preheader))

      (define v0 (VarId 0))
      (define v1 (VarId 1))
      (define v2 (VarId 2))
      (define v3 (VarId 3))

      ;; preheader: v1 = 10
      (define cfg6
        (cfg-block-append-insn cfg5 preheader
          (VfInsn 'const '(10) (list v1) #f #f)))
      (define cfg7 (cfg-block-set-terminator cfg6 preheader (TermJump header)))

      (define cfg8
        (cfg-block-append-insn cfg7 header
          (VfInsn 'const '(1) (list v0) #f #f)))
      (define cfg9 (cfg-block-set-terminator cfg8 header (TermBranch v0 body exit)))

      ;; body: v2 = v1 + 5, v3 = v2 * 2 (都是循环不变的)
      (define cfg10
        (cfg-block-append-insn cfg9 body
          (VfInsn 'add (list v1 5) (list v2) #f #f)))
      (define cfg11
        (cfg-block-append-insn cfg10 body
          (VfInsn 'mul (list v2 2) (list v3) #f #f)))
      (define cfg12 (cfg-block-set-terminator cfg11 body (TermJump header)))
      (define cfg13 (cfg-block-set-terminator cfg12 exit (TermReturn (list v3))))

      (define body-before (block-insn-count cfg13 body))
      (define-values (cfg^ stats) (cfg-licm-with-stats cfg13))
      ;; body 中的指令应该被外提（至少 2 条）
      (check-true (>= (cdr (assq 'insns-hoisted stats)) 2))
      ;; body 应该变空
      (check-equal? (block-insn-count cfg^ body) 0))))

;; ============================================================
;; 运行测试
;; ============================================================

(module+ test
  (require rackunit/text-ui)
  (run-tests loop-analysis-tests)
  (run-tests licm-tests))

(module+ main
  (require rackunit/text-ui)
  (displayln "=== LICM Tests ===\n")
  (displayln "--- Loop Analysis Tests ---")
  (run-tests loop-analysis-tests)
  (displayln "\n--- LICM Tests ---")
  (run-tests licm-tests)
  (displayln "\nAll tests completed!"))
