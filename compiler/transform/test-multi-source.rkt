#lang racket/base

;; ============================================================
;; 多源 CFG 转换测试
;; ============================================================
;;
;; 测试 L IR、C IR、JVM IR 到统一 CFG 的转换
;; 验证转换后的 CFG 可以通过优化管道
;; ============================================================

(require racket/match racket/format racket/dict)
(require "../lib/ftree.rkt")
(require "../core/cfg.rkt")
(require "../core/core-types.rkt")
(require "../core/p-types.rkt")
(require "../cfg/raw.rkt")
(require "../optim/cfg/pipeline.rkt")
(require "l-to-cfg.rkt")
(require "c-to-cfg.rkt")

;; pvector 辅助函数
(define (pvector-single x)
  (pvector-cons-left (pvector-empty) x))

(define (list->pvector lst)
  (for/fold ([r (pvector-empty)]) ([x (reverse lst)])
    (pvector-cons-left r x)))

;; ============================================================
;; 辅助函数
;; ============================================================

(define (print-separator [char #\=] [width 60])
  (displayln (make-string width char)))

(define (count-insns cfg)
  (for/sum ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if block (length (CfgBlock-insns block)) 0)))

(define (count-blocks cfg)
  (length (cfg-all-block-ids cfg)))

(define (print-cfg-summary cfg label)
  (printf "~a: ~a blocks, ~a instructions\n"
          label (count-blocks cfg) (count-insns cfg)))

(define (print-cfg-detail cfg label)
  (printf "\n~a:\n" label)
  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (printf "  Block ~a:\n" (BlockId-id bid))
      (for ([phi (CfgBlock-phis block)])
        (printf "    PHI: ~a <- ~a\n"
                (PhiInsn-output phi)
                (PhiInsn-sources phi)))
      (for ([insn (CfgBlock-insns block)])
        (printf "    ~a ~a -> ~a\n"
                (VfInsn-op insn)
                (VfInsn-inputs insn)
                (VfInsn-outputs insn)))
      (printf "    Term: ~a\n" (CfgBlock-terminator block)))))

;; ============================================================
;; 测试用例
;; ============================================================

;; 测试 1: 简单算术 (L IR)
(define (test-l-simple-arithmetic)
  (displayln "\n=== Test 1: L IR - Simple Arithmetic ===")

  ;; (let ([x 10])
  ;;   (let ([y 20])
  ;;     (+ x y)))
  (define prog
    (Program
      (ordl-make-empty symbol-compare)
      (Let 0 (Int 10)
        (Let 1 (Int 20)
          (Prim '+ (list (Var 0) (Var 1)))))))

  (define cfg (l-program->cfg prog))
  (print-cfg-summary cfg "Before optimization")
  (print-cfg-detail cfg "CFG Detail")

  ;; 运行优化
  (define cfg-opt (cfg-optimize cfg))
  (print-cfg-summary cfg-opt "After optimization")

  (printf "Result: ~a -> ~a instructions\n"
          (count-insns cfg) (count-insns cfg-opt)))

;; 测试 2: 条件分支 (L IR)
(define (test-l-conditional)
  (displayln "\n=== Test 2: L IR - Conditional ===")

  ;; (if (< 5 10) 1 2)
  (define prog
    (Program
      (ordl-make-empty symbol-compare)
      (If (Prim '< (list (Int 5) (Int 10)))
          (Int 1)
          (Int 2))))

  (define cfg (l-program->cfg prog))
  (print-cfg-summary cfg "Before optimization")

  (define cfg-opt (cfg-optimize cfg))
  (print-cfg-summary cfg-opt "After optimization"))

;; 测试 3: While 循环 (L IR)
(define (test-l-while-loop)
  (displayln "\n=== Test 3: L IR - While Loop ===")

  ;; (let ([sum 0])
  ;;   (let ([i 0])
  ;;     (begin
  ;;       (while (< (get! i) 10)
  ;;         (begin
  ;;           (set! sum (+ (get! sum) (get! i)))
  ;;           (set! i (+ (get! i) 1))))
  ;;       (get! sum))))
  (define prog
    (Program
      (ordl-make-empty symbol-compare)
      (Let 0 (Int 0)  ; sum
        (Let 1 (Int 0)  ; i
          (Begin
            (pvector-cons-left (pvector-empty)
              (WhileLoop
                (Prim '< (list (GetBang 1) (Int 10)))
                (Begin
                  (pvector-cons-left (pvector-empty)
                    (SetBang 0 (Prim '+ (list (GetBang 0) (GetBang 1)))))
                  (SetBang 1 (Prim '+ (list (GetBang 1) (Int 1)))))))
            (GetBang 0))))))

  (define cfg (l-program->cfg prog))
  (print-cfg-summary cfg "Before optimization")

  (define cfg-opt (cfg-optimize cfg))
  (print-cfg-summary cfg-opt "After optimization"))

;; 测试 4: C IR 到 CFG (需要先有 CProgram)
(define (test-c-simple)
  (displayln "\n=== Test 4: C IR - Simple Block ===")

  ;; 手动构造一个简单的 CProgram
  ;; block 2: x = 10; y = 20; return (x + y)
  (define blocks
    (let ([b (ordl-make-empty integer-compare)])
      (dict-set b 2
        (pvector-cons-left
          (pvector-cons-left
            (pvector-cons-left
              (pvector-single (Return (Var 2)))
              (Assign (Var 2) (Prim '+ (list (Var 0) (Var 1)))))
            (Assign (Var 1) (Int 20)))
          (Assign (Var 0) (Int 10))))))

  (define cprog (CProgram (ordl-make-empty symbol-compare) blocks))

  (define cfg (c-program->cfg cprog))
  (print-cfg-summary cfg "Before optimization")
  (print-cfg-detail cfg "CFG Detail")

  (define cfg-opt (cfg-optimize cfg))
  (print-cfg-summary cfg-opt "After optimization"))

;; 测试 5: 综合优化测试
(define (test-optimization-pipeline)
  (displayln "\n=== Test 5: Optimization Pipeline ===")

  ;; 包含冗余计算的程序
  ;; (let ([a 5])
  ;;   (let ([b 3])
  ;;     (let ([c (+ a b)])     ; a + b = 8
  ;;       (let ([d (+ a b)])   ; 冗余
  ;;         (* c d)))))        ; 8 * 8 = 64
  (define prog
    (Program
      (ordl-make-empty symbol-compare)
      (Let 0 (Int 5)  ; a
        (Let 1 (Int 3)  ; b
          (Let 2 (Prim '+ (list (Var 0) (Var 1)))  ; c = a + b
            (Let 3 (Prim '+ (list (Var 0) (Var 1)))  ; d = a + b (冗余)
              (Prim '* (list (Var 2) (Var 3)))))))))

  (define cfg (l-program->cfg prog))
  (print-cfg-summary cfg "Before optimization")

  (define-values (cfg-opt stats) (cfg-optimize-with-stats cfg))
  (print-cfg-summary cfg-opt "After optimization")

  (printf "\nOptimization stats:\n")
  (for ([s stats])
    (printf "  ~a: ~a\n" (car s) (cdr s))))

;; ============================================================
;; 运行所有测试
;; ============================================================

(define (run-all-tests)
  (displayln "")
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║         Multi-Source CFG Conversion Tests                ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")

  (with-handlers ([exn:fail?
                   (lambda (e)
                     (printf "Error in test: ~a\n" (exn-message e)))])
    (test-l-simple-arithmetic))

  (with-handlers ([exn:fail?
                   (lambda (e)
                     (printf "Error in test: ~a\n" (exn-message e)))])
    (test-l-conditional))

  (with-handlers ([exn:fail?
                   (lambda (e)
                     (printf "Error in test: ~a\n" (exn-message e)))])
    (test-l-while-loop))

  (with-handlers ([exn:fail?
                   (lambda (e)
                     (printf "Error in test: ~a\n" (exn-message e)))])
    (test-c-simple))

  (with-handlers ([exn:fail?
                   (lambda (e)
                     (printf "Error in test: ~a\n" (exn-message e)))])
    (test-optimization-pipeline))

  (displayln "\n=== All tests completed ==="))

(module+ main
  (run-all-tests))

(provide run-all-tests)
