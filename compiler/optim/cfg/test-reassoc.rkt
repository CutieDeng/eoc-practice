#lang racket/base

;; ============================================================
;; Expression Reassociation Tests
;; ============================================================

(require rackunit racket/match racket/list)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "reassoc.rkt")

;; ============================================================
;; 辅助函数
;; ============================================================

(define test-count 0)

(define (run-test name cfg check-fn)
  (set! test-count (+ 1 test-count))
  (printf "Test ~a: ~a... " test-count name)

  (define cfg^ (cfg-reassoc cfg))
  (check-fn cfg^)
  (printf "PASS~n"))

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

(define (get-first-block-insns cfg)
  (define bid (cfg-get-entry cfg))
  (define block (cfg-get-block cfg bid))
  (if block (CfgBlock-insns block) '()))

;; ============================================================
;; 测试 1: 加法常量聚合 (a + 1) + 2 → a + 3
;; ============================================================

(define (test-add-const-aggregate)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))

  (define cfg
    (make-test-cfg
      (list
        (VfInsn 'const '(100) (list v0) #f #f)
        (VfInsn 'add (list v0 1) (list v1) #f #f)
        (VfInsn 'add (list v1 2) (list v2) #f #f))
      (TermReturn (list v2))))

  (run-test "Add const aggregate: (a + 1) + 2 -> a + 3"
            cfg
            (λ (cfg^)
              ;; 检查最后一条 add 指令使用 v0 和常量 3
              (define insns (get-first-block-insns cfg^))
              (define last-add
                (for/last ([insn insns]
                           #:when (and (VfInsn? insn)
                                       (eq? (VfInsn-op insn) 'add)))
                  insn))
              (check-true (and last-add
                               (match (VfInsn-inputs last-add)
                                 [(list (== v0) 3) #t]
                                 [(list 3 (== v0)) #t]
                                 [_ #f]))
                          "Should combine constants to 3"))))

;; ============================================================
;; 测试 2: 乘法常量聚合 (a * 2) * 3 → a * 6
;; ============================================================

(define (test-mul-const-aggregate)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))

  (define cfg
    (make-test-cfg
      (list
        (VfInsn 'const '(10) (list v0) #f #f)
        (VfInsn 'mul (list v0 2) (list v1) #f #f)
        (VfInsn 'mul (list v1 3) (list v2) #f #f))
      (TermReturn (list v2))))

  (run-test "Mul const aggregate: (a * 2) * 3 -> a * 6"
            cfg
            (λ (cfg^)
              (define insns (get-first-block-insns cfg^))
              (define last-mul
                (for/last ([insn insns]
                           #:when (and (VfInsn? insn)
                                       (eq? (VfInsn-op insn) 'mul)))
                  insn))
              (check-true (and last-mul
                               (match (VfInsn-inputs last-mul)
                                 [(list (== v0) 6) #t]
                                 [(list 6 (== v0)) #t]
                                 [_ #f]))
                          "Should combine constants to 6"))))

;; ============================================================
;; 测试 3: 位与常量聚合 (a & 0xFF) & 0x0F → a & 0x0F
;; ============================================================

(define (test-and-const-aggregate)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))

  (define cfg
    (make-test-cfg
      (list
        (VfInsn 'const '(12345) (list v0) #f #f)
        (VfInsn 'and (list v0 #xFF) (list v1) #f #f)
        (VfInsn 'and (list v1 #x0F) (list v2) #f #f))
      (TermReturn (list v2))))

  (run-test "And const aggregate: (a & 0xFF) & 0x0F -> a & 0x0F"
            cfg
            (λ (cfg^)
              (define insns (get-first-block-insns cfg^))
              (define last-and
                (for/last ([insn insns]
                           #:when (and (VfInsn? insn)
                                       (eq? (VfInsn-op insn) 'and)))
                  insn))
              (check-true (and last-and
                               (match (VfInsn-inputs last-and)
                                 [(list (== v0) #x0F) #t]
                                 [(list #x0F (== v0)) #t]
                                 [_ #f]))
                          "Should combine to 0x0F"))))

;; ============================================================
;; 测试 4: 位或常量聚合 (a | 0x10) | 0x20 → a | 0x30
;; ============================================================

(define (test-or-const-aggregate)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))

  (define cfg
    (make-test-cfg
      (list
        (VfInsn 'const '(0) (list v0) #f #f)
        (VfInsn 'or (list v0 #x10) (list v1) #f #f)
        (VfInsn 'or (list v1 #x20) (list v2) #f #f))
      (TermReturn (list v2))))

  (run-test "Or const aggregate: (a | 0x10) | 0x20 -> a | 0x30"
            cfg
            (λ (cfg^)
              (define insns (get-first-block-insns cfg^))
              (define last-or
                (for/last ([insn insns]
                           #:when (and (VfInsn? insn)
                                       (eq? (VfInsn-op insn) 'or)))
                  insn))
              (check-true (and last-or
                               (match (VfInsn-inputs last-or)
                                 [(list (== v0) #x30) #t]
                                 [(list #x30 (== v0)) #t]
                                 [_ #f]))
                          "Should combine to 0x30"))))

;; ============================================================
;; 测试 5: 位异或常量聚合 (a ^ 0xAA) ^ 0x55 → a ^ 0xFF
;; ============================================================

(define (test-xor-const-aggregate)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))

  (define cfg
    (make-test-cfg
      (list
        (VfInsn 'const '(0) (list v0) #f #f)
        (VfInsn 'xor (list v0 #xAA) (list v1) #f #f)
        (VfInsn 'xor (list v1 #x55) (list v2) #f #f))
      (TermReturn (list v2))))

  (run-test "Xor const aggregate: (a ^ 0xAA) ^ 0x55 -> a ^ 0xFF"
            cfg
            (λ (cfg^)
              (define insns (get-first-block-insns cfg^))
              (define last-xor
                (for/last ([insn insns]
                           #:when (and (VfInsn? insn)
                                       (eq? (VfInsn-op insn) 'xor)))
                  insn))
              (check-true (and last-xor
                               (match (VfInsn-inputs last-xor)
                                 [(list (== v0) #xFF) #t]
                                 [(list #xFF (== v0)) #t]
                                 [_ #f]))
                          "Should combine to 0xFF"))))

;; ============================================================
;; 测试 6: 三常量加法链 ((a + 1) + 2) + 3 → a + 6
;; ============================================================

(define (test-triple-add-const)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))
  (define v3 (VarId 3))

  (define cfg
    (make-test-cfg
      (list
        (VfInsn 'const '(100) (list v0) #f #f)
        (VfInsn 'add (list v0 1) (list v1) #f #f)
        (VfInsn 'add (list v1 2) (list v2) #f #f)
        (VfInsn 'add (list v2 3) (list v3) #f #f))
      (TermReturn (list v3))))

  (run-test "Triple add const: ((a + 1) + 2) + 3 -> a + 6"
            cfg
            (λ (cfg^)
              (define insns (get-first-block-insns cfg^))
              (define last-add
                (for/last ([insn insns]
                           #:when (and (VfInsn? insn)
                                       (eq? (VfInsn-op insn) 'add)))
                  insn))
              (check-true (and last-add
                               (match (VfInsn-inputs last-add)
                                 [(list (== v0) 6) #t]
                                 [(list 6 (== v0)) #t]
                                 [_ #f]))
                          "Should combine constants to 6"))))

;; ============================================================
;; 测试 7: 常量链折叠
;; ============================================================

(define (test-const-chain-fold)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))

  (define cfg
    (make-test-cfg
      (list
        (VfInsn 'const '(1) (list v0) #f #f)
        (VfInsn 'add (list v0 2) (list v1) #f #f)
        (VfInsn 'add (list v1 3) (list v2) #f #f))
      (TermReturn (list v2))))

  ;; 重结合后应该合并常量
  (run-test "Const chain fold: const 1; (v0 + 2) + 3 -> v0 + 5"
            cfg
            (λ (cfg^)
              (define insns (get-first-block-insns cfg^))
              (define last-add
                (for/last ([insn insns]
                           #:when (and (VfInsn? insn)
                                       (eq? (VfInsn-op insn) 'add)))
                  insn))
              (check-true (and last-add
                               (match (VfInsn-inputs last-add)
                                 [(list (== v0) 5) #t]
                                 [(list 5 (== v0)) #t]
                                 [_ #f]))
                          "Should combine 2 and 3 to 5"))))

;; ============================================================
;; 测试 8: 多次使用不扁平化
;; ============================================================

(define (test-multi-use-no-flatten)
  (printf "Test ~a: Multi-use no flatten... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))
  (define v3 (VarId 3))

  ;; v1 被使用两次，不应该扁平化
  (define cfg
    (make-test-cfg
      (list
        (VfInsn 'const '(10) (list v0) #f #f)
        (VfInsn 'add (list v0 1) (list v1) #f #f)
        (VfInsn 'add (list v1 2) (list v2) #f #f)
        (VfInsn 'add (list v1 3) (list v3) #f #f))
      (TermReturn (list v2 v3))))

  (define cfg^ (cfg-reassoc cfg))
  (define after (count-insns cfg^))

  ;; 由于 v1 被使用两次，不应该进行扁平化
  (check-equal? after 4 "Multi-use variable should not be flattened")
  (printf "PASS~n"))

;; ============================================================
;; 测试 9: 加法单位元消除 a + 0 → a
;; ============================================================

(define (test-add-identity)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))

  (define cfg
    (make-test-cfg
      (list
        (VfInsn 'const '(42) (list v0) #f #f)
        (VfInsn 'add (list v0 0) (list v1) #f #f)
        (VfInsn 'add (list v1 0) (list v2) #f #f))
      (TermReturn (list v2))))

  (run-test "Add identity: (a + 0) + 0 -> a + 0 (identity preserved)"
            cfg
            (λ (cfg^)
              (define insns (get-first-block-insns cfg^))
              (define last-add
                (for/last ([insn insns]
                           #:when (and (VfInsn? insn)
                                       (eq? (VfInsn-op insn) 'add)))
                  insn))
              ;; 0 + 0 = 0，仍然是单位元
              (check-true (or (not last-add)
                              (match (VfInsn-inputs last-add)
                                [(list (== v0) 0) #t]
                                [(list 0 (== v0)) #t]
                                [_ #f]))
                          "Should have identity element"))))

;; ============================================================
;; 测试 10: 乘法单位元 a * 1 → a
;; ============================================================

(define (test-mul-identity)
  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))

  (define cfg
    (make-test-cfg
      (list
        (VfInsn 'const '(42) (list v0) #f #f)
        (VfInsn 'mul (list v0 1) (list v1) #f #f)
        (VfInsn 'mul (list v1 1) (list v2) #f #f))
      (TermReturn (list v2))))

  (run-test "Mul identity: (a * 1) * 1 -> a * 1 (identity preserved)"
            cfg
            (λ (cfg^)
              (define insns (get-first-block-insns cfg^))
              (define last-mul
                (for/last ([insn insns]
                           #:when (and (VfInsn? insn)
                                       (eq? (VfInsn-op insn) 'mul)))
                  insn))
              ;; 1 * 1 = 1，仍然是单位元
              (check-true (or (not last-mul)
                              (match (VfInsn-inputs last-mul)
                                [(list (== v0) 1) #t]
                                [(list 1 (== v0)) #t]
                                [_ #f]))
                          "Should have identity element"))))

;; ============================================================
;; 测试 11: 混合变量和常量 (a + b) + 1 保持结构
;; ============================================================

(define (test-mixed-vars-consts)
  (printf "Test ~a: Mixed vars and consts... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))
  (define v3 (VarId 3))

  ;; (a + b) + 1：有两个变量和一个常量
  (define cfg
    (make-test-cfg
      (list
        (VfInsn 'const '(10) (list v0) #f #f)
        (VfInsn 'const '(20) (list v1) #f #f)
        (VfInsn 'add (list v0 v1) (list v2) #f #f)
        (VfInsn 'add (list v2 1) (list v3) #f #f))
      (TermReturn (list v3))))

  (define cfg^ (cfg-reassoc cfg))
  ;; 不应该减少指令（没有多个常量可合并）
  (check-equal? (count-insns cfg^) 4 "Should not reduce with mixed vars")
  (printf "PASS~n"))

;; ============================================================
;; 测试 12: 迭代重结合
;; ============================================================

(define (test-iterative-reassoc)
  (printf "Test ~a: Iterative reassociation... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define v0 (VarId 0))
  (define v1 (VarId 1))
  (define v2 (VarId 2))
  (define v3 (VarId 3))
  (define v4 (VarId 4))

  ;; (((a + 1) + 2) + 3) + 4 → a + 10
  (define cfg
    (make-test-cfg
      (list
        (VfInsn 'const '(100) (list v0) #f #f)
        (VfInsn 'add (list v0 1) (list v1) #f #f)
        (VfInsn 'add (list v1 2) (list v2) #f #f)
        (VfInsn 'add (list v2 3) (list v3) #f #f)
        (VfInsn 'add (list v3 4) (list v4) #f #f))
      (TermReturn (list v4))))

  (define cfg^ (cfg-reassoc-fixpoint cfg))
  (define insns (get-first-block-insns cfg^))
  (define last-add
    (for/last ([insn insns]
               #:when (and (VfInsn? insn)
                           (eq? (VfInsn-op insn) 'add)))
      insn))

  (check-true (and last-add
                   (match (VfInsn-inputs last-add)
                     [(list (== v0) 10) #t]
                     [(list 10 (== v0)) #t]
                     [_ #f]))
              "Should combine all constants to 10")
  (printf "PASS~n"))

;; ============================================================
;; 运行所有测试
;; ============================================================

(module+ main
  (displayln "")
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║           Expression Reassociation Tests                 ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")
  (displayln "")

  (test-add-const-aggregate)
  (test-mul-const-aggregate)
  (test-and-const-aggregate)
  (test-or-const-aggregate)
  (test-xor-const-aggregate)
  (test-triple-add-const)
  (test-const-chain-fold)
  (test-multi-use-no-flatten)
  (test-add-identity)
  (test-mul-identity)
  (test-mixed-vars-consts)
  (test-iterative-reassoc)

  (printf "~n~a tests completed.~n" test-count))
