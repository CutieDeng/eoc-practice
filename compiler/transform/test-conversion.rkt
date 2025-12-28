#lang racket/base

;; ============================================================
;; CFG 转换测试 (不依赖完整优化管道)
;; ============================================================
;;
;; 测试 L IR、C IR 到统一 CFG 的转换正确性
;; ============================================================

(require racket/match racket/format racket/dict)
(require "../ftree.rkt")
(require "../core/cfg.rkt")
(require "../core/core-types.rkt")
(require "../core/p-types.rkt")
(require "../cfg/raw.rkt")
(require "l-to-cfg.rkt")
(require "c-to-cfg.rkt")

;; ral 辅助函数
(define (ral-single x)
  (ral-consl (ral-empty) x))

(define (list->ral lst)
  (for/fold ([r (ral-empty)]) ([x (reverse lst)])
    (ral-consl r x)))

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

(define (check condition msg)
  (if condition
      (printf "  ✓ ~a\n" msg)
      (printf "  ✗ FAIL: ~a\n" msg)))

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
  (print-cfg-summary cfg "Result")
  (print-cfg-detail cfg "CFG")

  (check (= (count-blocks cfg) 1) "Single block generated")
  (check (= (count-insns cfg) 3) "3 instructions: 2 const + 1 add")
  (check (cfg-get-entry cfg) "Entry block set"))

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
  (print-cfg-summary cfg "Result")

  (check (= (count-blocks cfg) 4) "4 blocks: entry, then, else, merge")
  (check (cfg-get-entry cfg) "Entry block set"))

;; 测试 3: While 循环 (L IR)
(define (test-l-while-loop)
  (displayln "\n=== Test 3: L IR - While Loop ===")

  ;; (let ([i 0])
  ;;   (while (< (get! i) 10)
  ;;     (set! i (+ (get! i) 1)))
  ;;   (get! i))
  (define prog
    (Program
      (ordl-make-empty symbol-compare)
      (Let 0 (Int 0)  ; i
        (Begin
          (ral-single
            (WhileLoop
              (Prim '< (list (GetBang 0) (Int 10)))
              (SetBang 0 (Prim '+ (list (GetBang 0) (Int 1))))))
          (GetBang 0)))))

  (define cfg (l-program->cfg prog))
  (print-cfg-summary cfg "Result")

  (check (>= (count-blocks cfg) 3) "At least 3 blocks: entry, header, body, exit")
  (check (cfg-get-entry cfg) "Entry block set"))

;; 测试 4: C IR 到 CFG
(define (test-c-simple)
  (displayln "\n=== Test 4: C IR - Simple Block ===")

  ;; 手动构造一个简单的 CProgram
  ;; block 2: x = 10; y = 20; return (x + y)
  (define blocks
    (let ([b (ordl-make-empty integer-compare)])
      (dict-set b 2
        (list->ral
          (list
            (Assign (Var 0) (Int 10))
            (Assign (Var 1) (Int 20))
            (Assign (Var 2) (Prim '+ (list (Var 0) (Var 1))))
            (Return (Var 2)))))))

  (define cprog (CProgram (ordl-make-empty symbol-compare) blocks))

  (define cfg (c-program->cfg cprog))
  (print-cfg-summary cfg "Result")
  (print-cfg-detail cfg "CFG")

  (check (= (count-blocks cfg) 1) "Single block generated")
  (check (>= (count-insns cfg) 3) "At least 3 instructions"))

;; 测试 5: 带函数调用的 L IR
(define (test-l-function-call)
  (displayln "\n=== Test 5: L IR - Function Reference ===")

  ;; (let ([f (fun-ref foo 2)])
  ;;   (+ 1 2))
  (define prog
    (Program
      (ordl-make-empty symbol-compare)
      (Let 0 (FunRef 'foo 2)
        (Prim '+ (list (Int 1) (Int 2))))))

  (define cfg (l-program->cfg prog))
  (print-cfg-summary cfg "Result")

  (check (= (count-blocks cfg) 1) "Single block generated")
  (check (>= (count-insns cfg) 4) "At least 4 instructions"))

;; 测试 6: 嵌套 Let (L IR)
(define (test-l-nested-let)
  (displayln "\n=== Test 6: L IR - Nested Let ===")

  ;; (let ([a 1])
  ;;   (let ([b 2])
  ;;     (let ([c (+ a b)])
  ;;       (* c 3))))
  (define prog
    (Program
      (ordl-make-empty symbol-compare)
      (Let 0 (Int 1)
        (Let 1 (Int 2)
          (Let 2 (Prim '+ (list (Var 0) (Var 1)))
            (Prim '* (list (Var 2) (Int 3))))))))

  (define cfg (l-program->cfg prog))
  (print-cfg-summary cfg "Result")
  (print-cfg-detail cfg "CFG")

  (check (= (count-blocks cfg) 1) "Single block generated")
  (check (= (count-insns cfg) 5) "5 instructions: 3 const + add + mul"))

;; ============================================================
;; 运行所有测试
;; ============================================================

(define (run-all-tests)
  (displayln "")
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║         L/C IR → CFG Conversion Tests                    ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")

  (with-handlers ([exn:fail?
                   (lambda (e)
                     (printf "Error: ~a\n" (exn-message e)))])
    (test-l-simple-arithmetic))

  (with-handlers ([exn:fail?
                   (lambda (e)
                     (printf "Error: ~a\n" (exn-message e)))])
    (test-l-conditional))

  (with-handlers ([exn:fail?
                   (lambda (e)
                     (printf "Error: ~a\n" (exn-message e)))])
    (test-l-while-loop))

  (with-handlers ([exn:fail?
                   (lambda (e)
                     (printf "Error: ~a\n" (exn-message e)))])
    (test-c-simple))

  (with-handlers ([exn:fail?
                   (lambda (e)
                     (printf "Error: ~a\n" (exn-message e)))])
    (test-l-function-call))

  (with-handlers ([exn:fail?
                   (lambda (e)
                     (printf "Error: ~a\n" (exn-message e)))])
    (test-l-nested-let))

  (displayln "\n=== All conversion tests completed ==="))

(module+ main
  (run-all-tests))

(provide run-all-tests)
