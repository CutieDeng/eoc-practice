#lang racket/base

;; ============================================================
;; End-to-End Integration Tests
;; ============================================================
;;
;; 测试完整流水线: L IR → CFG → 优化 → 解释执行
;; 验证优化前后语义不变
;; ============================================================

(require racket/match racket/format racket/dict)
(require "../ftree.rkt")
(require "../core/cfg.rkt")
(require "../core/p-types.rkt")
(require "../cfg/raw.rkt")
(require "../optim/cfg/pipeline.rkt")
(require "l-to-cfg.rkt")

;; ============================================================
;; Simple CFG Interpreter
;; ============================================================

;; 运行时值
(struct RtVal (v) #:transparent)

;; CFG 解释器状态
(struct CfgState (vars cfg) #:transparent)

;; 创建初始状态
(define (make-cfg-state cfg)
  (CfgState (make-hash) cfg))

;; 获取变量值
(define (state-get state var-id)
  (hash-ref (CfgState-vars state) var-id
            (lambda () (error 'state-get "Undefined variable: ~a" var-id))))

;; 设置变量值
(define (state-set! state var-id val)
  (hash-set! (CfgState-vars state) var-id val))

;; 解释单条指令
(define (interp-insn state insn)
  (match insn
    [(VfInsn 'const (list v) (list out) _ _)
     (state-set! state out v)]

    [(VfInsn 'copy (list src) (list out) _ _)
     (define val (if (VarId? src) (state-get state src) src))
     (state-set! state out val)]

    [(VfInsn 'add (list a b) (list out) _ _)
     (define va (if (VarId? a) (state-get state a) a))
     (define vb (if (VarId? b) (state-get state b) b))
     (state-set! state out (+ va vb))]

    [(VfInsn 'sub (list a b) (list out) _ _)
     (define va (if (VarId? a) (state-get state a) a))
     (define vb (if (VarId? b) (state-get state b) b))
     (state-set! state out (- va vb))]

    [(VfInsn 'mul (list a b) (list out) _ _)
     (define va (if (VarId? a) (state-get state a) a))
     (define vb (if (VarId? b) (state-get state b) b))
     (state-set! state out (* va vb))]

    [(VfInsn 'div (list a b) (list out) _ _)
     (define va (if (VarId? a) (state-get state a) a))
     (define vb (if (VarId? b) (state-get state b) b))
     (state-set! state out (quotient va vb))]

    [(VfInsn 'rem (list a b) (list out) _ _)
     (define va (if (VarId? a) (state-get state a) a))
     (define vb (if (VarId? b) (state-get state b) b))
     (state-set! state out (remainder va vb))]

    [(VfInsn 'lt (list a b) (list out) _ _)
     (define va (if (VarId? a) (state-get state a) a))
     (define vb (if (VarId? b) (state-get state b) b))
     (state-set! state out (< va vb))]

    [(VfInsn 'gt (list a b) (list out) _ _)
     (define va (if (VarId? a) (state-get state a) a))
     (define vb (if (VarId? b) (state-get state b) b))
     (state-set! state out (> va vb))]

    [(VfInsn 'eq (list a b) (list out) _ _)
     (define va (if (VarId? a) (state-get state a) a))
     (define vb (if (VarId? b) (state-get state b) b))
     (state-set! state out (equal? va vb))]

    [(VfInsn 'le (list a b) (list out) _ _)
     (define va (if (VarId? a) (state-get state a) a))
     (define vb (if (VarId? b) (state-get state b) b))
     (state-set! state out (<= va vb))]

    [(VfInsn 'ge (list a b) (list out) _ _)
     (define va (if (VarId? a) (state-get state a) a))
     (define vb (if (VarId? b) (state-get state b) b))
     (state-set! state out (>= va vb))]

    [(VfInsn 'not (list a) (list out) _ _)
     (define va (if (VarId? a) (state-get state a) a))
     (state-set! state out (not va))]

    [(VfInsn 'fun-ref (list name arity) (list out) _ _)
     (state-set! state out `(fun-ref ,name ,arity))]

    [(VfInsn 'global-ref (list name) (list out) _ _)
     (state-set! state out `(global ,name))]

    [_ (void)]))  ; 忽略其他指令

;; 处理 PHI 节点
(define (interp-phis state block from-block)
  (for ([phi (CfgBlock-phis block)])
    (match phi
      [(PhiInsn out sources)
       (define src-var
         (for/first ([src sources]
                     #:when (equal? (car src) from-block))
           (cdr src)))
       (when src-var
         (state-set! state out (state-get state src-var)))])))

;; 解释一个基本块
(define (interp-block state block-id [from-block #f])
  (define cfg (CfgState-cfg state))
  (define block (cfg-get-block cfg block-id))

  (unless block
    (error 'interp-block "Block not found: ~a" block-id))

  ;; 处理 PHI 节点
  (when from-block
    (interp-phis state block from-block))

  ;; 执行指令
  (for ([insn (CfgBlock-insns block)])
    (interp-insn state insn))

  ;; 处理终止器
  (match (CfgBlock-terminator block)
    [(TermJump target)
     (interp-block state target block-id)]

    [(TermBranch cond then-target else-target)
     (define cond-val (state-get state cond))
     (if cond-val
         (interp-block state then-target block-id)
         (interp-block state else-target block-id))]

    [(TermReturn vals)
     (if (null? vals)
         'void
         (state-get state (car vals)))]

    [(TermUnreachable)
     (error 'interp-block "Reached unreachable code")]

    [_ 'void]))

;; 解释整个 CFG
(define (interp-cfg cfg)
  (define state (make-cfg-state cfg))
  (define entry (cfg-get-entry cfg))
  (unless entry
    (error 'interp-cfg "CFG has no entry block"))
  (interp-block state entry))

;; ============================================================
;; Test Framework
;; ============================================================

(define tests-passed 0)
(define tests-failed 0)

;; 调试模式
(define debug-mode (make-parameter #f))

(define (debug-print-cfg cfg label)
  (when (debug-mode)
    (printf "\n  ~a:\n" label)
    (for ([bid (cfg-all-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (when block
        (printf "    Block ~a:\n" (BlockId-id bid))
        (for ([phi (CfgBlock-phis block)])
          (printf "      PHI: ~a <- ~a\n" (PhiInsn-output phi) (PhiInsn-sources phi)))
        (for ([insn (CfgBlock-insns block)])
          (printf "      ~a ~a -> ~a\n" (VfInsn-op insn) (VfInsn-inputs insn) (VfInsn-outputs insn)))
        (printf "      Term: ~a\n" (CfgBlock-terminator block))))))

(define (test name prog expected)
  (printf "Testing ~a... " name)

  (with-handlers ([exn:fail?
                   (lambda (e)
                     (set! tests-failed (+ tests-failed 1))
                     (printf "FAIL (exception: ~a)\n" (exn-message e)))])

    ;; 转换为 CFG
    (define cfg (l-program->cfg prog))
    (debug-print-cfg cfg "Before optimization")

    ;; 优化前执行
    (define result-before (interp-cfg cfg))

    ;; 优化
    (define cfg-opt (cfg-optimize cfg))
    (debug-print-cfg cfg-opt "After optimization")

    ;; 优化后执行
    (define result-after (interp-cfg cfg-opt))

    ;; 比较结果
    (cond
      [(and (equal? result-before expected)
            (equal? result-after expected))
       (set! tests-passed (+ tests-passed 1))
       (printf "PASS (before=~a, after=~a, expected=~a)\n"
               result-before result-after expected)]
      [else
       (set! tests-failed (+ tests-failed 1))
       (printf "FAIL (before=~a, after=~a, expected=~a)\n"
               result-before result-after expected)])))

;; ============================================================
;; Test Cases
;; ============================================================

(define (run-tests)
  (displayln "")
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║           End-to-End Integration Tests                   ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")
  (displayln "")

  ;; Test 1: 简单算术
  (test "simple-add"
        (Program (ordl-make-empty symbol-compare)
          (Prim '+ (list (Int 10) (Int 20))))
        30)

  ;; Test 2: 嵌套算术
  (test "nested-arithmetic"
        (Program (ordl-make-empty symbol-compare)
          (Prim '* (list
                     (Prim '+ (list (Int 2) (Int 3)))
                     (Int 4))))
        20)

  ;; Test 3: Let 绑定
  (test "let-binding"
        (Program (ordl-make-empty symbol-compare)
          (Let 0 (Int 5)
            (Let 1 (Int 3)
              (Prim '+ (list (Var 0) (Var 1))))))
        8)

  ;; Test 4: 嵌套 Let
  (test "nested-let"
        (Program (ordl-make-empty symbol-compare)
          (Let 0 (Int 10)
            (Let 1 (Prim '+ (list (Var 0) (Int 5)))
              (Prim '* (list (Var 1) (Int 2))))))
        30)

  ;; Test 5: 条件 - true 分支
  (test "if-true"
        (Program (ordl-make-empty symbol-compare)
          (If (Prim '< (list (Int 5) (Int 10)))
              (Int 1)
              (Int 2)))
        1)

  ;; Test 6: 条件 - false 分支
  (test "if-false"
        (Program (ordl-make-empty symbol-compare)
          (If (Prim '> (list (Int 5) (Int 10)))
              (Int 1)
              (Int 2)))
        2)

  ;; Test 7: 复杂条件
  (test "if-complex"
        (Program (ordl-make-empty symbol-compare)
          (Let 0 (Int 15)
            (If (Prim '> (list (Var 0) (Int 10)))
                (Prim '- (list (Var 0) (Int 10)))
                (Prim '+ (list (Var 0) (Int 10))))))
        5)

  ;; Test 8: 常量折叠验证
  (test "constant-folding"
        (Program (ordl-make-empty symbol-compare)
          (Let 0 (Int 5)
            (Let 1 (Int 3)
              (Let 2 (Prim '+ (list (Var 0) (Var 1)))   ; 8
                (Let 3 (Prim '* (list (Var 2) (Int 2)))  ; 16
                  (Prim '+ (list (Var 3) (Int 4))))))))  ; 20
        20)

  ;; Test 9: 冗余消除验证
  (test "redundancy-elimination"
        (Program (ordl-make-empty symbol-compare)
          (Let 0 (Int 7)
            (Let 1 (Int 3)
              (Let 2 (Prim '+ (list (Var 0) (Var 1)))   ; a + b
                (Let 3 (Prim '+ (list (Var 0) (Var 1)))  ; a + b (冗余)
                  (Prim '* (list (Var 2) (Var 3))))))))  ; 10 * 10
        100)

  ;; Test 10: 强度削减验证 (乘以 2 的幂)
  (test "strength-reduction"
        (Program (ordl-make-empty symbol-compare)
          (Let 0 (Int 7)
            (Prim '* (list (Var 0) (Int 8)))))  ; 7 * 8 = 56
        56)

  ;; 总结
  (displayln "")
  (printf "Results: ~a passed, ~a failed\n" tests-passed tests-failed)
  (displayln ""))

;; 仅测试转换正确性（不经过优化）
(define (run-conversion-tests)
  (displayln "")
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║         Conversion-Only Tests (No Optimization)          ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")
  (displayln "")

  (set! tests-passed 0)
  (set! tests-failed 0)

  (test-conversion "simple-add"
        (Program (ordl-make-empty symbol-compare)
          (Prim '+ (list (Int 10) (Int 20))))
        30)

  (test-conversion "let-binding"
        (Program (ordl-make-empty symbol-compare)
          (Let 0 (Int 5)
            (Let 1 (Int 3)
              (Prim '+ (list (Var 0) (Var 1))))))
        8)

  (test-conversion "if-true"
        (Program (ordl-make-empty symbol-compare)
          (If (Prim '< (list (Int 5) (Int 10)))
              (Int 1)
              (Int 2)))
        1)

  (test-conversion "if-false"
        (Program (ordl-make-empty symbol-compare)
          (If (Prim '> (list (Int 5) (Int 10)))
              (Int 1)
              (Int 2)))
        2)

  (printf "\nConversion tests: ~a passed, ~a failed\n" tests-passed tests-failed))

(define (test-conversion name prog expected)
  (printf "Testing ~a... " name)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (set! tests-failed (+ tests-failed 1))
                     (printf "FAIL (exception: ~a)\n" (exn-message e)))])
    (define cfg (l-program->cfg prog))
    (define result (interp-cfg cfg))
    (if (equal? result expected)
        (begin
          (set! tests-passed (+ tests-passed 1))
          (printf "PASS (result=~a)\n" result))
        (begin
          (set! tests-failed (+ tests-failed 1))
          (printf "FAIL (got=~a, expected=~a)\n" result expected)))))

(module+ main
  (run-conversion-tests)
  (displayln "")
  (run-tests))

(provide run-tests interp-cfg)
