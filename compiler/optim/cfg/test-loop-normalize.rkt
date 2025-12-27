#lang racket/base

;; ============================================================
;; Loop Normalization Tests
;; ============================================================

(require rackunit racket/match racket/list racket/set)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "loop-analysis.rkt")
(require "loop-normalize.rkt")

;; ============================================================
;; 辅助函数
;; ============================================================

(define test-count 0)

(define (count-insns cfg)
  (for/sum ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if block (length (CfgBlock-insns block)) 0)))

(define (count-phis cfg)
  (for*/sum ([bid (cfg-all-block-ids cfg)]
             [block (in-value (cfg-get-block cfg bid))]
             #:when block)
    (length (CfgBlock-phis block))))

;; ============================================================
;; 创建测试循环的辅助函数
;; ============================================================

;; 创建一个从 init 到 bound 步长为 step 的循环
;; for (i = init; i < bound; i += step) { body }
(define (make-counting-loop init bound step)
  (define cfg0 (cfg-empty))

  (define-values (preheader-bid cfg1) (cfg-create-block cfg0))
  (define-values (header-bid cfg2) (cfg-create-block cfg1))
  (define-values (body-bid cfg3) (cfg-create-block cfg2))
  (define-values (exit-bid cfg4) (cfg-create-block cfg3))

  (define cfg5 (cfg-set-entry cfg4 preheader-bid))

  (define v-init (VarId 0))
  (define v-i (VarId 1))
  (define v-i-next (VarId 2))
  (define v-cmp (VarId 3))
  (define v-bound (VarId 4))

  ;; Preheader
  (define cfg6
    (cfg-block-append-insn cfg5 preheader-bid
      (VfInsn 'const (list init) (list v-init) #f #f)))
  (define cfg7
    (cfg-block-append-insn cfg6 preheader-bid
      (VfInsn 'const (list bound) (list v-bound) #f #f)))
  (define cfg8
    (cfg-block-set-terminator cfg7 preheader-bid
      (TermJump header-bid)))

  ;; Header
  (define cfg9
    (cfg-block-add-phi cfg8 header-bid
      (PhiInsn v-i (list (cons preheader-bid v-init)
                         (cons body-bid v-i-next)))))
  (define cfg10
    (cfg-block-append-insn cfg9 header-bid
      (VfInsn 'lt (list v-i v-bound) (list v-cmp) #f #f)))
  (define cfg11
    (cfg-block-set-terminator cfg10 header-bid
      (TermBranch v-cmp body-bid exit-bid)))

  ;; Body
  (define cfg12
    (cfg-block-append-insn cfg11 body-bid
      (VfInsn 'add (list v-i step) (list v-i-next) #f #f)))
  (define cfg13
    (cfg-block-set-terminator cfg12 body-bid
      (TermJump header-bid)))

  ;; Exit
  (define cfg14
    (cfg-block-set-terminator cfg13 exit-bid
      (TermReturn (list v-i))))

  cfg14)

;; 创建规范形式的循环 (i = 0; i < N; i++)
(define (make-canonical-loop N)
  (make-counting-loop 0 N 1))

;; ============================================================
;; 测试 1: 检测规范循环
;; ============================================================

(define (test-detect-canonical)
  (printf "Test ~a: Detect canonical loop... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-canonical-loop 10))
  (define loops (analyze-loops cfg))

  (check-equal? (length loops) 1 "Should detect one loop")

  (define loop (car loops))
  (define loop-info (find-norm-induction-variable cfg loop))

  (check-not-false loop-info "Should find induction variable")
  (check-true (is-canonical? loop-info) "Should be canonical (0 to N, step 1)")
  (check-false (can-normalize? loop-info) "Already canonical, cannot normalize")

  (printf "PASS~n"))

;; ============================================================
;; 测试 2: 检测非规范循环 (非零起始)
;; ============================================================

(define (test-detect-non-canonical-start)
  (printf "Test ~a: Detect non-canonical loop (non-zero start)... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-counting-loop 5 15 1))  ; for (i = 5; i < 15; i++)
  (define loops (analyze-loops cfg))

  (check-equal? (length loops) 1 "Should detect one loop")

  (define loop (car loops))
  (define loop-info (find-norm-induction-variable cfg loop))

  (check-not-false loop-info "Should find induction variable")
  (check-equal? (NormLoopInfo-init-value loop-info) 5 "Init should be 5")
  (check-equal? (NormLoopInfo-trip-count loop-info) 10 "Trip count should be 10")
  (check-false (is-canonical? loop-info) "Should not be canonical")
  (check-true (can-normalize? loop-info) "Should be normalizable")

  (printf "PASS~n"))

;; ============================================================
;; 测试 3: 检测非规范循环 (步长 != 1)
;; ============================================================

(define (test-detect-non-canonical-step)
  (printf "Test ~a: Detect non-canonical loop (step != 1)... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-counting-loop 0 10 2))  ; for (i = 0; i < 10; i += 2)
  (define loops (analyze-loops cfg))

  (check-equal? (length loops) 1 "Should detect one loop")

  (define loop (car loops))
  (define loop-info (find-norm-induction-variable cfg loop))

  (check-not-false loop-info "Should find induction variable")
  (check-equal? (NormLoopInfo-step loop-info) 2 "Step should be 2")
  (check-equal? (NormLoopInfo-trip-count loop-info) 5 "Trip count should be 5")
  (check-false (is-canonical? loop-info) "Should not be canonical")
  (check-true (can-normalize? loop-info) "Should be normalizable")

  (printf "PASS~n"))

;; ============================================================
;; 测试 4: 规范化非零起始循环
;; ============================================================

(define (test-normalize-non-zero-start)
  (printf "Test ~a: Normalize non-zero start loop... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-counting-loop 10 20 1))  ; for (i = 10; i < 20; i++)
  (define cfg^ (cfg-loop-normalize cfg))

  ;; 检查规范化后的循环
  (define loops (analyze-loops cfg^))

  ;; 循环应该还存在（或被完全规范化）
  (check-not-false cfg^ "Normalization should return a CFG")

  ;; 检查是否添加了新的 PHI 和指令
  (define phis-before (count-phis cfg))
  (define phis-after (count-phis cfg^))

  (check >= phis-after phis-before "Should have same or more PHIs after normalization")

  (printf "PASS~n"))

;; ============================================================
;; 测试 5: 规范化步长 != 1 的循环
;; ============================================================

(define (test-normalize-non-unit-step)
  (printf "Test ~a: Normalize non-unit step loop... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-counting-loop 0 10 2))  ; for (i = 0; i < 10; i += 2)
  (define cfg^ (cfg-loop-normalize cfg))

  (check-not-false cfg^ "Normalization should return a CFG")

  ;; 应该添加了计算旧 IV 的指令
  (define insns-before (count-insns cfg))
  (define insns-after (count-insns cfg^))

  (check > insns-after insns-before "Should have more instructions after normalization")

  (printf "PASS~n"))

;; ============================================================
;; 测试 6: 规范化保持循环结构
;; ============================================================

(define (test-normalize-preserves-structure)
  (printf "Test ~a: Normalization preserves loop structure... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-counting-loop 5 25 4))  ; for (i = 5; i < 25; i += 4)
  (define loops-before (analyze-loops cfg))

  (define cfg^ (cfg-loop-normalize cfg))
  (define loops-after (analyze-loops cfg^))

  ;; 规范化后循环数应该相同
  (check-equal? (length loops-after) (length loops-before)
                "Loop count should be preserved")

  (printf "PASS~n"))

;; ============================================================
;; 测试 7: 已规范循环不变
;; ============================================================

(define (test-canonical-unchanged)
  (printf "Test ~a: Canonical loop unchanged... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-canonical-loop 10))
  (define insns-before (count-insns cfg))
  (define phis-before (count-phis cfg))

  (define cfg^ (cfg-loop-normalize cfg))
  (define insns-after (count-insns cfg^))
  (define phis-after (count-phis cfg^))

  (check-equal? insns-after insns-before
                "Canonical loop should not add instructions")
  (check-equal? phis-after phis-before
                "Canonical loop should not add PHIs")

  (printf "PASS~n"))

;; ============================================================
;; 测试 8: 统计版本
;; ============================================================

(define (test-with-stats)
  (printf "Test ~a: With stats version... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-counting-loop 10 30 2))
  (define-values (cfg^ stats) (cfg-loop-normalize-with-stats cfg))

  (check-true (list? stats) "Stats should be a list")
  (check-not-false (assq 'loops-found stats) "Should have loops-found stat")
  (check-not-false (assq 'loops-normalizable stats) "Should have loops-normalizable stat")

  (define normalizable (cdr (assq 'loops-normalizable stats)))
  (check-equal? normalizable 1 "Should have 1 normalizable loop")

  (printf "PASS~n"))

;; ============================================================
;; 测试 9: 无循环 CFG
;; ============================================================

(define (test-no-loop)
  (printf "Test ~a: No loop CFG... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg0 (cfg-empty))
  (define-values (bid cfg1) (cfg-create-block cfg0))
  (define cfg2 (cfg-set-entry cfg1 bid))
  (define cfg3
    (cfg-block-append-insn cfg2 bid
      (VfInsn 'const '(42) (list (VarId 0)) #f #f)))
  (define cfg4
    (cfg-block-set-terminator cfg3 bid
      (TermReturn (list (VarId 0)))))

  (define cfg^ (cfg-loop-normalize cfg4))

  ;; 无循环时应该不变
  (check-equal? (count-insns cfg^) (count-insns cfg4)
                "No loop CFG should be unchanged")

  (printf "PASS~n"))

;; ============================================================
;; 测试 10: 复杂初始值和边界
;; ============================================================

(define (test-complex-bounds)
  (printf "Test ~a: Complex bounds... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  ;; for (i = 100; i < 200; i += 5)
  (define cfg (make-counting-loop 100 200 5))
  (define loops (analyze-loops cfg))
  (define loop (car loops))
  (define loop-info (find-norm-induction-variable cfg loop))

  (check-not-false loop-info "Should detect loop info")
  (check-equal? (NormLoopInfo-init-value loop-info) 100 "Init should be 100")
  (check-equal? (NormLoopInfo-bound loop-info) 200 "Bound should be 200")
  (check-equal? (NormLoopInfo-step loop-info) 5 "Step should be 5")
  (check-equal? (NormLoopInfo-trip-count loop-info) 20 "Trip count should be 20")

  (define cfg^ (cfg-loop-normalize cfg))
  (check-not-false cfg^ "Normalization should succeed")

  (printf "PASS~n"))

;; ============================================================
;; 运行所有测试
;; ============================================================

(printf "~n")
(printf "╔══════════════════════════════════════════════════════════╗~n")
(printf "║              Loop Normalization Tests                    ║~n")
(printf "╚══════════════════════════════════════════════════════════╝~n")
(printf "~n")

(test-detect-canonical)
(test-detect-non-canonical-start)
(test-detect-non-canonical-step)
(test-normalize-non-zero-start)
(test-normalize-non-unit-step)
(test-normalize-preserves-structure)
(test-canonical-unchanged)
(test-with-stats)
(test-no-loop)
(test-complex-bounds)

(printf "~n~a tests completed.~n" test-count)
