#lang racket/base

;; ============================================================
;; Loop Unrolling Tests
;; ============================================================

(require rackunit racket/match racket/list racket/set)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "loop-analysis.rkt")
(require "loop-unroll.rkt")

;; ============================================================
;; 辅助函数
;; ============================================================

(define test-count 0)

(define (count-insns cfg)
  (for/sum ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if block (length (CfgBlock-insns block)) 0)))

(define (count-blocks cfg)
  (length (cfg-all-block-ids cfg)))

;; ============================================================
;; 创建简单循环的辅助函数
;; ============================================================

;; 创建一个简单的计数循环:
;; preheader: init = 0; goto header
;; header: i = phi(init, i'); if i < N goto body else exit
;; body: i' = i + 1; goto header
;; exit: return i
(define (make-simple-counting-loop N)
  (define cfg0 (cfg-empty))

  ;; 创建四个块
  (define-values (preheader-bid cfg1) (cfg-create-block cfg0))
  (define-values (header-bid cfg2) (cfg-create-block cfg1))
  (define-values (body-bid cfg3) (cfg-create-block cfg2))
  (define-values (exit-bid cfg4) (cfg-create-block cfg3))

  (define cfg5 (cfg-set-entry cfg4 preheader-bid))

  ;; 变量
  (define v-init (VarId 0))   ; 初始值 0
  (define v-i (VarId 1))      ; 归纳变量 (PHI 输出)
  (define v-i-next (VarId 2)) ; i + 1
  (define v-cmp (VarId 3))    ; 比较结果
  (define v-N (VarId 4))      ; 边界 N

  ;; Preheader: init = 0; N = <N>; goto header
  (define cfg6
    (cfg-block-append-insn cfg5 preheader-bid
      (VfInsn 'const '(0) (list v-init) #f #f)))
  (define cfg7
    (cfg-block-append-insn cfg6 preheader-bid
      (VfInsn 'const (list N) (list v-N) #f #f)))
  (define cfg8
    (cfg-block-set-terminator cfg7 preheader-bid
      (TermJump header-bid)))

  ;; Header: i = phi(init from preheader, i' from body)
  ;;         cmp = i < N
  ;;         if cmp goto body else exit
  (define cfg9
    (cfg-block-add-phi cfg8 header-bid
      (PhiInsn v-i (list (cons preheader-bid v-init)
                         (cons body-bid v-i-next)))))
  (define cfg10
    (cfg-block-append-insn cfg9 header-bid
      (VfInsn 'lt (list v-i v-N) (list v-cmp) #f #f)))
  (define cfg11
    (cfg-block-set-terminator cfg10 header-bid
      (TermBranch v-cmp body-bid exit-bid)))

  ;; Body: i' = i + 1; goto header
  (define cfg12
    (cfg-block-append-insn cfg11 body-bid
      (VfInsn 'add (list v-i 1) (list v-i-next) #f #f)))
  (define cfg13
    (cfg-block-set-terminator cfg12 body-bid
      (TermJump header-bid)))

  ;; Exit: return i
  (define cfg14
    (cfg-block-set-terminator cfg13 exit-bid
      (TermReturn (list v-i))))

  cfg14)

;; 创建一个带累加的循环:
;; sum = 0; for (i = 0; i < N; i++) { sum += i; } return sum;
(define (make-sum-loop N)
  (define cfg0 (cfg-empty))

  (define-values (preheader-bid cfg1) (cfg-create-block cfg0))
  (define-values (header-bid cfg2) (cfg-create-block cfg1))
  (define-values (body-bid cfg3) (cfg-create-block cfg2))
  (define-values (exit-bid cfg4) (cfg-create-block cfg3))

  (define cfg5 (cfg-set-entry cfg4 preheader-bid))

  ;; 变量
  (define v-init-i (VarId 0))
  (define v-init-sum (VarId 1))
  (define v-i (VarId 2))
  (define v-sum (VarId 3))
  (define v-i-next (VarId 4))
  (define v-sum-next (VarId 5))
  (define v-cmp (VarId 6))
  (define v-N (VarId 7))

  ;; Preheader
  (define cfg6
    (cfg-block-append-insn cfg5 preheader-bid
      (VfInsn 'const '(0) (list v-init-i) #f #f)))
  (define cfg7
    (cfg-block-append-insn cfg6 preheader-bid
      (VfInsn 'const '(0) (list v-init-sum) #f #f)))
  (define cfg8
    (cfg-block-append-insn cfg7 preheader-bid
      (VfInsn 'const (list N) (list v-N) #f #f)))
  (define cfg9
    (cfg-block-set-terminator cfg8 preheader-bid
      (TermJump header-bid)))

  ;; Header
  (define cfg10
    (cfg-block-add-phi cfg9 header-bid
      (PhiInsn v-i (list (cons preheader-bid v-init-i)
                         (cons body-bid v-i-next)))))
  (define cfg11
    (cfg-block-add-phi cfg10 header-bid
      (PhiInsn v-sum (list (cons preheader-bid v-init-sum)
                           (cons body-bid v-sum-next)))))
  (define cfg12
    (cfg-block-append-insn cfg11 header-bid
      (VfInsn 'lt (list v-i v-N) (list v-cmp) #f #f)))
  (define cfg13
    (cfg-block-set-terminator cfg12 header-bid
      (TermBranch v-cmp body-bid exit-bid)))

  ;; Body
  (define cfg14
    (cfg-block-append-insn cfg13 body-bid
      (VfInsn 'add (list v-sum v-i) (list v-sum-next) #f #f)))
  (define cfg15
    (cfg-block-append-insn cfg14 body-bid
      (VfInsn 'add (list v-i 1) (list v-i-next) #f #f)))
  (define cfg16
    (cfg-block-set-terminator cfg15 body-bid
      (TermJump header-bid)))

  ;; Exit
  (define cfg17
    (cfg-block-set-terminator cfg16 exit-bid
      (TermReturn (list v-sum))))

  cfg17)

;; ============================================================
;; 测试 1: 循环检测
;; ============================================================

(define (test-loop-detection)
  (printf "Test ~a: Loop detection... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-simple-counting-loop 10))
  (define loops (analyze-loops cfg))

  (check-equal? (length loops) 1 "Should detect exactly 1 loop")
  (define loop (car loops))
  (check-equal? (set-count (Loop-body loop)) 2
                "Loop body should have 2 blocks (header + body)")
  (printf "PASS~n"))

;; ============================================================
;; 测试 2: 归纳变量识别
;; ============================================================

(define (test-induction-variable)
  (printf "Test ~a: Induction variable detection... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-simple-counting-loop 10))
  (define loops (analyze-loops cfg))
  (define loop (car loops))

  (define loop-info (find-induction-variable cfg loop))

  (check-true (LoopInfo? loop-info) "Should find induction variable")
  (when (LoopInfo? loop-info)
    (check-equal? (LoopInfo-init-value loop-info) 0 "Init should be 0")
    (check-equal? (LoopInfo-step loop-info) 1 "Step should be 1"))
  (printf "PASS~n"))

;; ============================================================
;; 测试 3: 迭代次数计算
;; ============================================================

(define (test-trip-count)
  (printf "Test ~a: Trip count calculation... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-simple-counting-loop 10))
  (define loops (analyze-loops cfg))
  (define loop (car loops))
  (define loop-info (find-induction-variable cfg loop))

  (check-true (LoopInfo? loop-info) "Should find loop info")
  (when (LoopInfo? loop-info)
    (check-equal? (LoopInfo-trip-count loop-info) 10
                  "Trip count should be 10"))
  (printf "PASS~n"))

;; ============================================================
;; 测试 4: 完全展开判断
;; ============================================================

(define (test-should-unroll)
  (printf "Test ~a: Should unroll decision... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  ;; 小循环应该展开
  (define cfg-small (make-simple-counting-loop 4))
  (define loops-small (analyze-loops cfg-small))
  (define loop-small (car loops-small))
  (define info-small (find-induction-variable cfg-small loop-small))

  (check-true (and info-small
                   (should-complete-unroll? info-small loop-small cfg-small))
              "Small loop should be unrolled")

  ;; 大循环不应该展开
  (define cfg-large (make-simple-counting-loop 100))
  (define loops-large (analyze-loops cfg-large))
  (define loop-large (car loops-large))
  (define info-large (find-induction-variable cfg-large loop-large))

  (check-false (and info-large
                    (should-complete-unroll? info-large loop-large cfg-large))
               "Large loop should not be unrolled")
  (printf "PASS~n"))

;; ============================================================
;; 测试 5: 累加循环检测
;; ============================================================

(define (test-sum-loop-detection)
  (printf "Test ~a: Sum loop detection... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-sum-loop 5))
  (define loops (analyze-loops cfg))

  (check-equal? (length loops) 1 "Should detect 1 loop")
  (define loop (car loops))
  (define loop-info (find-induction-variable cfg loop))

  (check-true (LoopInfo? loop-info) "Should find induction variable")
  (printf "PASS~n"))

;; ============================================================
;; 测试 6: 无循环 CFG
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

  (define loops (analyze-loops cfg4))
  (check-equal? (length loops) 0 "Should detect no loops")

  (define cfg^ (cfg-loop-unroll cfg4))
  (check-equal? (count-blocks cfg^) (count-blocks cfg4)
                "No change for no-loop CFG")
  (printf "PASS~n"))

;; ============================================================
;; 测试 7: 零迭代循环
;; ============================================================

(define (test-zero-trip-loop)
  (printf "Test ~a: Zero trip loop... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-simple-counting-loop 0))
  (define loops (analyze-loops cfg))

  (when (not (null? loops))
    (define loop (car loops))
    (define loop-info (find-induction-variable cfg loop))

    (when (LoopInfo? loop-info)
      (check-equal? (LoopInfo-trip-count loop-info) 0
                    "Trip count should be 0")))
  (printf "PASS~n"))

;; ============================================================
;; 测试 8: 展开不应增加循环数
;; ============================================================

(define (test-unroll-loop-count)
  (printf "Test ~a: Unroll loop count... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-simple-counting-loop 4))
  (define loops-before (analyze-loops cfg))

  (define cfg^ (cfg-loop-unroll cfg))
  (define loops-after (analyze-loops cfg^))

  (check <= (length loops-after) (length loops-before)
         "Unrolling should not increase loop count")
  (printf "PASS~n"))

;; ============================================================
;; 测试 9: 带统计版本
;; ============================================================

(define (test-with-stats)
  (printf "Test ~a: With stats version... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-simple-counting-loop 4))
  (define-values (cfg^ stats) (cfg-loop-unroll-with-stats cfg))

  (check-true (list? stats) "Stats should be a list")
  (check-not-false (assq 'loops-before stats) "Should have loops-before stat")
  (check-not-false (assq 'loops-after stats) "Should have loops-after stat")
  (printf "PASS~n"))

;; ============================================================
;; 测试 10: 嵌套循环
;; ============================================================

(define (test-nested-loops)
  (printf "Test ~a: Nested loops... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  ;; 创建简单的嵌套循环结构
  ;; outer: for i in 0..2
  ;;   inner: for j in 0..3
  (define cfg0 (cfg-empty))

  (define-values (pre-bid cfg1) (cfg-create-block cfg0))
  (define-values (outer-h-bid cfg2) (cfg-create-block cfg1))
  (define-values (inner-h-bid cfg3) (cfg-create-block cfg2))
  (define-values (inner-b-bid cfg4) (cfg-create-block cfg3))
  (define-values (outer-inc-bid cfg5) (cfg-create-block cfg4))
  (define-values (exit-bid cfg6) (cfg-create-block cfg5))

  (define cfg7 (cfg-set-entry cfg6 pre-bid))

  ;; 简化：只设置基本的控制流来创建嵌套结构
  (define v-i (VarId 0))
  (define v-j (VarId 1))
  (define v-cmp-i (VarId 2))
  (define v-cmp-j (VarId 3))
  (define v-i-init (VarId 4))
  (define v-j-init (VarId 5))
  (define v-i-next (VarId 6))
  (define v-j-next (VarId 7))

  ;; Pre
  (define cfg8
    (cfg-block-append-insn cfg7 pre-bid
      (VfInsn 'const '(0) (list v-i-init) #f #f)))
  (define cfg9
    (cfg-block-set-terminator cfg8 pre-bid
      (TermJump outer-h-bid)))

  ;; Outer header
  (define cfg10
    (cfg-block-add-phi cfg9 outer-h-bid
      (PhiInsn v-i (list (cons pre-bid v-i-init)
                         (cons outer-inc-bid v-i-next)))))
  (define cfg11
    (cfg-block-append-insn cfg10 outer-h-bid
      (VfInsn 'lt (list v-i 2) (list v-cmp-i) #f #f)))
  (define cfg12
    (cfg-block-append-insn cfg11 outer-h-bid
      (VfInsn 'const '(0) (list v-j-init) #f #f)))
  (define cfg13
    (cfg-block-set-terminator cfg12 outer-h-bid
      (TermBranch v-cmp-i inner-h-bid exit-bid)))

  ;; Inner header
  (define cfg14
    (cfg-block-add-phi cfg13 inner-h-bid
      (PhiInsn v-j (list (cons outer-h-bid v-j-init)
                         (cons inner-b-bid v-j-next)))))
  (define cfg15
    (cfg-block-append-insn cfg14 inner-h-bid
      (VfInsn 'lt (list v-j 3) (list v-cmp-j) #f #f)))
  (define cfg16
    (cfg-block-set-terminator cfg15 inner-h-bid
      (TermBranch v-cmp-j inner-b-bid outer-inc-bid)))

  ;; Inner body
  (define cfg17
    (cfg-block-append-insn cfg16 inner-b-bid
      (VfInsn 'add (list v-j 1) (list v-j-next) #f #f)))
  (define cfg18
    (cfg-block-set-terminator cfg17 inner-b-bid
      (TermJump inner-h-bid)))

  ;; Outer increment
  (define cfg19
    (cfg-block-append-insn cfg18 outer-inc-bid
      (VfInsn 'add (list v-i 1) (list v-i-next) #f #f)))
  (define cfg20
    (cfg-block-set-terminator cfg19 outer-inc-bid
      (TermJump outer-h-bid)))

  ;; Exit
  (define cfg21
    (cfg-block-set-terminator cfg20 exit-bid
      (TermReturn (list v-i))))

  (define loops (analyze-loops cfg21))
  (check-equal? (length loops) 2 "Should detect 2 loops")

  ;; 不应该崩溃
  (define cfg^ (cfg-loop-unroll cfg21))
  (check-true (Cfg? cfg^) "Should return valid CFG")
  (printf "PASS~n"))

;; ============================================================
;; 运行所有测试
;; ============================================================

(module+ main
  (displayln "")
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║              Loop Unrolling Tests                        ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")
  (displayln "")

  (test-loop-detection)
  (test-induction-variable)
  (test-trip-count)
  (test-should-unroll)
  (test-sum-loop-detection)
  (test-no-loop)
  (test-zero-trip-loop)
  (test-unroll-loop-count)
  (test-with-stats)
  (test-nested-loops)

  (printf "~n~a tests completed.~n" test-count))
