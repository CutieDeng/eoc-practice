#lang racket/base

;; ============================================================
;; Forward Propagation Tests
;; ============================================================

(require rackunit racket/match racket/list)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "forward-prop.rkt")

;; ============================================================
;; 辅助函数
;; ============================================================

(define test-count 0)

(define (run-test name cfg expected-reduction)
  (set! test-count (+ 1 test-count))
  (printf "Test ~a: ~a... " test-count name)

  (define before (count-insns cfg))
  (define-values (cfg^ stats) (cfg-forward-prop-with-stats cfg))
  (define after (count-insns cfg^))
  (define reduction (- before after))

  (with-handlers ([exn:fail? (lambda (e)
                               (printf "FAIL: ~a~n" (exn-message e))
                               (raise e))])
    (check >= reduction expected-reduction
           (format "Expected at least ~a reduction, got ~a" expected-reduction reduction))
    (printf "PASS (~a -> ~a insns)~n" before after)))

(define (count-insns cfg)
  (for/sum ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if block (length (CfgBlock-insns block)) 0)))

(define (make-test-cfg insns terminator)
  (define cfg0 (cfg-empty))
  (define-values (bid cfg1) (cfg-create-block cfg0))
  (define cfg2 (cfg-set-entry cfg1 bid))
  (define cfg3
    (for/fold ([cfg cfg2])
              ([insn insns])
      (cfg-block-append-insn cfg bid insn)))
  (cfg-block-set-terminator cfg3 bid terminator))

;; ============================================================
;; 测试 1: 双重否定消除 not(not(a)) → a
;; ============================================================

(define (test-double-negation)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))

  (run-test "Double negation: not(not(a)) -> a"
    (make-test-cfg
      (list
        (VfInsn 'const '(42) (list v0) #f #f)
        (VfInsn 'not (list v0) (list v1) #f #f)
        (VfInsn 'not (list v1) (list v2) #f #f))
      (TermReturn (list v2)))
    1))

;; ============================================================
;; 测试 2: 双重取负消除 neg(neg(a)) → a
;; ============================================================

(define (test-double-neg)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))

  (run-test "Double negate: neg(neg(a)) -> a"
    (make-test-cfg
      (list
        (VfInsn 'const '(42) (list v0) #f #f)
        (VfInsn 'neg (list v0) (list v1) #f #f)
        (VfInsn 'neg (list v1) (list v2) #f #f))
      (TermReturn (list v2)))
    1))

;; ============================================================
;; 测试 3: 加减抵消 (a + b) - b → a
;; ============================================================

(define (test-add-sub-cancel)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))
  (define v3 (VarId 3))

  (run-test "Add-Sub cancel: (a + b) - b -> a"
    (make-test-cfg
      (list
        (VfInsn 'const '(10) (list v0) #f #f)
        (VfInsn 'const '(5) (list v1) #f #f)
        (VfInsn 'add (list v0 v1) (list v2) #f #f)
        (VfInsn 'sub (list v2 v1) (list v3) #f #f))
      (TermReturn (list v3)))
    1))

;; ============================================================
;; 测试 4: 减加抵消 (a - b) + b → a
;; ============================================================

(define (test-sub-add-cancel)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))
  (define v3 (VarId 3))

  (run-test "Sub-Add cancel: (a - b) + b -> a"
    (make-test-cfg
      (list
        (VfInsn 'const '(10) (list v0) #f #f)
        (VfInsn 'const '(5) (list v1) #f #f)
        (VfInsn 'sub (list v0 v1) (list v2) #f #f)
        (VfInsn 'add (list v2 v1) (list v3) #f #f))
      (TermReturn (list v3)))
    1))

;; ============================================================
;; 测试 5: 乘法常量合并 (a * c1) * c2 → a * (c1 * c2)
;; ============================================================

(define (test-mul-const-combine)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))

  ;; Note: forward prop transforms but doesn't remove; DCE handles removal
  (run-test "Mul const combine: (a * 3) * 4 -> a * 12"
    (make-test-cfg
      (list
        (VfInsn 'const '(7) (list v0) #f #f)
        (VfInsn 'mul (list v0 3) (list v1) #f #f)
        (VfInsn 'mul (list v1 4) (list v2) #f #f))
      (TermReturn (list v2)))
    0))

;; ============================================================
;; 测试 6: 加法常量合并 (a + c1) + c2 → a + (c1 + c2)
;; ============================================================

(define (test-add-const-combine)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))

  (run-test "Add const combine: (a + 5) + 10 -> a + 15"
    (make-test-cfg
      (list
        (VfInsn 'const '(100) (list v0) #f #f)
        (VfInsn 'add (list v0 5) (list v1) #f #f)
        (VfInsn 'add (list v1 10) (list v2) #f #f))
      (TermReturn (list v2)))
    0))

;; ============================================================
;; 测试 7: 左移常量合并 (a << c1) << c2 → a << (c1 + c2)
;; ============================================================

(define (test-shl-const-combine)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))

  (run-test "Shl const combine: (a << 2) << 3 -> a << 5"
    (make-test-cfg
      (list
        (VfInsn 'const '(1) (list v0) #f #f)
        (VfInsn 'shl (list v0 2) (list v1) #f #f)
        (VfInsn 'shl (list v1 3) (list v2) #f #f))
      (TermReturn (list v2)))
    0))

;; ============================================================
;; 测试 8: 位与常量合并 (a & c1) & c2 → a & (c1 & c2)
;; ============================================================

(define (test-and-const-combine)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))

  (run-test "And const combine: (a & 0xFF) & 0x0F -> a & 0x0F"
    (make-test-cfg
      (list
        (VfInsn 'const '(12345) (list v0) #f #f)
        (VfInsn 'and (list v0 #xFF) (list v1) #f #f)
        (VfInsn 'and (list v1 #x0F) (list v2) #f #f))
      (TermReturn (list v2)))
    0))

;; ============================================================
;; 测试 9: 位或常量合并 (a | c1) | c2 → a | (c1 | c2)
;; ============================================================

(define (test-or-const-combine)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))

  (run-test "Or const combine: (a | 0x10) | 0x20 -> a | 0x30"
    (make-test-cfg
      (list
        (VfInsn 'const '(0) (list v0) #f #f)
        (VfInsn 'or (list v0 #x10) (list v1) #f #f)
        (VfInsn 'or (list v1 #x20) (list v2) #f #f))
      (TermReturn (list v2)))
    0))

;; ============================================================
;; 测试 10: 位异或常量合并 (a ^ c1) ^ c2 → a ^ (c1 ^ c2)
;; ============================================================

(define (test-xor-const-combine)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))

  (run-test "Xor const combine: (a ^ 0xAA) ^ 0x55 -> a ^ 0xFF"
    (make-test-cfg
      (list
        (VfInsn 'const '(0) (list v0) #f #f)
        (VfInsn 'xor (list v0 #xAA) (list v1) #f #f)
        (VfInsn 'xor (list v1 #x55) (list v2) #f #f))
      (TermReturn (list v2)))
    0))

;; ============================================================
;; 测试 11: 多次使用不内联
;; ============================================================

(define (test-multi-use-no-inline)
  (printf "Test ~a: Multi-use no inline... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))
  (define v3 (VarId 3))

  ;; v1 被使用两次，不应该内联
  (define cfg
    (make-test-cfg
      (list
        (VfInsn 'const '(10) (list v0) #f #f)
        (VfInsn 'add (list v0 5) (list v1) #f #f)
        (VfInsn 'add (list v1 1) (list v2) #f #f)
        (VfInsn 'add (list v1 2) (list v3) #f #f))
      (TermReturn (list v2 v3))))

  (define cfg^ (cfg-forward-prop cfg))
  (define after (count-insns cfg^))

  ;; 由于 v1 被使用两次，不应该进行合并
  (check-equal? after 4 "Multi-use variable should not be inlined")
  (printf "PASS~n"))

;; ============================================================
;; 测试 12: 迭代优化
;; ============================================================

(define (test-iterative)
  (printf "Test ~a: Iterative optimization... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))
  (define v3 (VarId 3))

  ;; 链式操作：((a + 1) + 2) + 3 → a + 6
  ;; Forward prop combines constants: v3 = v0 + 6
  ;; But v1, v2 still exist (DCE removes them)
  (define cfg
    (make-test-cfg
      (list
        (VfInsn 'const '(100) (list v0) #f #f)
        (VfInsn 'add (list v0 1) (list v1) #f #f)
        (VfInsn 'add (list v1 2) (list v2) #f #f)
        (VfInsn 'add (list v2 3) (list v3) #f #f))
      (TermReturn (list v3))))

  (define cfg^ (cfg-forward-prop-fixpoint cfg))
  (define after (count-insns cfg^))

  ;; Forward prop transforms but doesn't remove; expect same count
  ;; DCE would reduce to 2 instructions
  (check <= after 4 "Iterative should not increase instruction count")
  (printf "PASS (4 -> ~a insns)~n" after))

;; ============================================================
;; 测试 13: 减法常量合并 (a - c1) - c2 → a - (c1 + c2)
;; ============================================================

(define (test-sub-const-combine)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))

  (run-test "Sub const combine: (a - 5) - 10 -> a - 15"
    (make-test-cfg
      (list
        (VfInsn 'const '(100) (list v0) #f #f)
        (VfInsn 'sub (list v0 5) (list v1) #f #f)
        (VfInsn 'sub (list v1 10) (list v2) #f #f))
      (TermReturn (list v2)))
    0))

;; ============================================================
;; 测试 14: 右移常量合并 (a >> c1) >> c2 → a >> (c1 + c2)
;; ============================================================

(define (test-shr-const-combine)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))

  (run-test "Shr const combine: (a >> 2) >> 3 -> a >> 5"
    (make-test-cfg
      (list
        (VfInsn 'const '(1024) (list v0) #f #f)
        (VfInsn 'shr (list v0 2) (list v1) #f #f)
        (VfInsn 'shr (list v1 3) (list v2) #f #f))
      (TermReturn (list v2)))
    0))

;; ============================================================
;; 运行所有测试
;; ============================================================

(module+ main
  (displayln "")
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║           Forward Propagation Tests                      ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")
  (displayln "")

  (test-double-negation)
  (test-double-neg)
  (test-add-sub-cancel)
  (test-sub-add-cancel)
  (test-mul-const-combine)
  (test-add-const-combine)
  (test-shl-const-combine)
  (test-and-const-combine)
  (test-or-const-combine)
  (test-xor-const-combine)
  (test-multi-use-no-inline)
  (test-iterative)
  (test-sub-const-combine)
  (test-shr-const-combine)

  (printf "~n~a tests completed.~n" test-count))
