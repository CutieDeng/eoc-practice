#lang racket/base

;; ============================================================
;; 合成测试用例 - 测试各种优化模式
;; ============================================================

(require racket/list racket/format racket/match)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "pipeline.rkt")

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

(define (count-insns cfg)
  (for/sum ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if block (length (CfgBlock-insns block)) 0)))

(define (print-separator [char #\=] [width 60])
  (displayln (make-string width char)))

(define (print-cfg cfg label)
  (printf "\n~a (~a instructions):\n" label (count-insns cfg))
  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (for ([insn (CfgBlock-insns block)])
        (when (VfInsn? insn)
          (printf "  ~a ~a -> ~a\n"
                  (VfInsn-op insn)
                  (VfInsn-inputs insn)
                  (VfInsn-outputs insn)))))))

;; ============================================================
;; 测试场景
;; ============================================================

(define (test-case name cfg)
  (print-separator)
  (displayln name)
  (print-separator)

  (print-cfg cfg "Before")

  (define-values (cfg-opt stats) (cfg-optimize-with-stats cfg))

  (print-cfg cfg-opt "After")

  (printf "\nStats:\n")
  (for ([s stats])
    (printf "  ~a: ~a\n" (car s) (cdr s)))

  (define before (count-insns cfg))
  (define after (count-insns cfg-opt))
  (printf "\nResult: ~a -> ~a instructions (~a% reduction)\n"
          before after
          (~r (* 100.0 (/ (- before after) (max before 1))) #:precision 1))
  (newline))

;; ============================================================
;; 测试 1: 常量折叠
;; ============================================================

(define (test-constant-folding)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))
  (define v3 (VarId 3))
  (define v4 (VarId 4))

  (test-case "Test 1: Constant Folding"
    (make-test-cfg
      (list
        (VfInsn 'const '(10) (list v0) #f #f)
        (VfInsn 'const '(20) (list v1) #f #f)
        (VfInsn 'add (list v0 v1) (list v2) #f #f)  ; 10 + 20 = 30
        (VfInsn 'const '(3) (list v3) #f #f)
        (VfInsn 'mul (list v2 v3) (list v4) #f #f)) ; 30 * 3 = 90
      (TermReturn (list v4)))))

;; ============================================================
;; 测试 2: 死代码消除
;; ============================================================

(define (test-dead-code)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))
  (define v3 (VarId 3))
  (define v4 (VarId 4))

  (test-case "Test 2: Dead Code Elimination"
    (make-test-cfg
      (list
        (VfInsn 'const '(42) (list v0) #f #f)       ; 使用
        (VfInsn 'const '(100) (list v1) #f #f)      ; 死代码
        (VfInsn 'const '(200) (list v2) #f #f)      ; 死代码
        (VfInsn 'add (list v1 v2) (list v3) #f #f)  ; 死代码
        (VfInsn 'mul (list v3 2) (list v4) #f #f))  ; 死代码
      (TermReturn (list v0)))))

;; ============================================================
;; 测试 3: 强度削减
;; ============================================================

(define (test-strength-reduction)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))
  (define v3 (VarId 3))
  (define v4 (VarId 4))

  (test-case "Test 3: Strength Reduction"
    (make-test-cfg
      (list
        (VfInsn 'const '(7) (list v0) #f #f)
        (VfInsn 'mul (list v0 8) (list v1) #f #f)   ; x * 8 -> x << 3
        (VfInsn 'mul (list v0 5) (list v2) #f #f)   ; x * 5 -> (x << 2) + x
        (VfInsn 'urem (list v1 16) (list v3) #f #f) ; x % 16 -> x & 15
        (VfInsn 'add (list v2 v3) (list v4) #f #f))
      (TermReturn (list v4)))))

;; ============================================================
;; 测试 4: GVN (冗余消除)
;; ============================================================

(define (test-gvn)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))
  (define v3 (VarId 3))
  (define v4 (VarId 4))

  (test-case "Test 4: GVN (Redundancy Elimination)"
    (make-test-cfg
      (list
        (VfInsn 'const '(5) (list v0) #f #f)
        (VfInsn 'const '(3) (list v1) #f #f)
        (VfInsn 'add (list v0 v1) (list v2) #f #f)  ; a + b
        (VfInsn 'add (list v0 v1) (list v3) #f #f)  ; a + b (冗余)
        (VfInsn 'mul (list v2 v3) (list v4) #f #f))
      (TermReturn (list v4)))))

;; ============================================================
;; 测试 5: 代数简化
;; ============================================================

(define (test-algebraic)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))
  (define v3 (VarId 3))
  (define v4 (VarId 4))

  (test-case "Test 5: Algebraic Simplification"
    (make-test-cfg
      (list
        (VfInsn 'const '(42) (list v0) #f #f)
        (VfInsn 'sub (list v0 v0) (list v1) #f #f)  ; x - x = 0
        (VfInsn 'xor (list v0 v0) (list v2) #f #f)  ; x ^ x = 0
        (VfInsn 'and (list v0 0) (list v3) #f #f)   ; x & 0 = 0
        (VfInsn 'add (list v1 v2) (list v4) #f #f)) ; 0 + 0 = 0
      (TermReturn (list v4)))))

;; ============================================================
;; 测试 6: 综合优化
;; ============================================================

(define (test-combined)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))
  (define v3 (VarId 3))
  (define v4 (VarId 4))
  (define v5 (VarId 5))
  (define v6 (VarId 6))
  (define v7 (VarId 7))

  (test-case "Test 6: Combined Optimizations"
    (make-test-cfg
      (list
        ;; 常量定义
        (VfInsn 'const '(2) (list v0) #f #f)
        (VfInsn 'const '(3) (list v1) #f #f)
        ;; 可折叠的计算
        (VfInsn 'add (list v0 v1) (list v2) #f #f)     ; 2 + 3 = 5
        (VfInsn 'mul (list v2 4) (list v3) #f #f)      ; 5 * 4 = 20 (可强度削减)
        ;; 冗余计算
        (VfInsn 'add (list v0 v1) (list v4) #f #f)     ; 2 + 3 (冗余)
        ;; 死代码
        (VfInsn 'const '(999) (list v5) #f #f)         ; 未使用
        (VfInsn 'mul (list v5 8) (list v6) #f #f)      ; 未使用
        ;; 最终结果
        (VfInsn 'add (list v3 v4) (list v7) #f #f))    ; 20 + 5 = 25
      (TermReturn (list v7)))))

;; ============================================================
;; 运行所有测试
;; ============================================================

(module+ main
  (displayln "")
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║      CFG Optimization Pipeline - Synthetic Tests         ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")
  (displayln "")

  (test-constant-folding)
  (test-dead-code)
  (test-strength-reduction)
  (test-gvn)
  (test-algebraic)
  (test-combined)

  (displayln "All tests completed!"))
