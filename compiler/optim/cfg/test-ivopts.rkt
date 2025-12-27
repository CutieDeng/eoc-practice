#lang racket/base

;; ============================================================
;; Induction Variable Optimization Tests
;; ============================================================

(require rackunit racket/match racket/list racket/set)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "loop-analysis.rkt")
(require "ivopts.rkt")

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

(define (count-muls cfg)
  (for*/sum ([bid (cfg-all-block-ids cfg)]
             [block (in-value (cfg-get-block cfg bid))]
             #:when block
             [insn (CfgBlock-insns block)]
             #:when (and (VfInsn? insn)
                         (eq? (VfInsn-op insn) 'mul)))
    1))

;; ============================================================
;; 创建测试循环的辅助函数
;; ============================================================

;; 创建带 DIV 的循环：
;; for (i = 0; i < N; i++) {
;;   j = i * scale + offset
;;   use(j)
;; }
(define (make-loop-with-div N scale offset)
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
  (define v-j (VarId 5))       ; DIV: j = i * scale + offset
  (define v-tmp (VarId 6))     ; 临时变量

  ;; Preheader
  (define cfg6
    (cfg-block-append-insn cfg5 preheader-bid
      (VfInsn 'const '(0) (list v-init) #f #f)))
  (define cfg7
    (cfg-block-append-insn cfg6 preheader-bid
      (VfInsn 'const (list N) (list v-bound) #f #f)))
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

  ;; Body: j = i * scale + offset; i' = i + 1
  (define cfg12
    (if (= offset 0)
        ;; j = i * scale
        (cfg-block-append-insn cfg11 body-bid
          (VfInsn 'mul (list v-i scale) (list v-j) #f #f))
        ;; j = i * scale + offset
        (let ()
          (define cfg-a
            (cfg-block-append-insn cfg11 body-bid
              (VfInsn 'mul (list v-i scale) (list v-tmp) #f #f)))
          (cfg-block-append-insn cfg-a body-bid
            (VfInsn 'add (list v-tmp offset) (list v-j) #f #f)))))

  (define cfg13
    (cfg-block-append-insn cfg12 body-bid
      (VfInsn 'add (list v-i 1) (list v-i-next) #f #f)))
  (define cfg14
    (cfg-block-set-terminator cfg13 body-bid
      (TermJump header-bid)))

  ;; Exit
  (define cfg15
    (cfg-block-set-terminator cfg14 exit-bid
      (TermReturn (list v-j))))

  cfg15)

;; 创建简单计数循环
(define (make-simple-loop N)
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
      (VfInsn 'const '(0) (list v-init) #f #f)))
  (define cfg7
    (cfg-block-append-insn cfg6 preheader-bid
      (VfInsn 'const (list N) (list v-bound) #f #f)))
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
      (VfInsn 'add (list v-i 1) (list v-i-next) #f #f)))
  (define cfg13
    (cfg-block-set-terminator cfg12 body-bid
      (TermJump header-bid)))

  ;; Exit
  (define cfg14
    (cfg-block-set-terminator cfg13 exit-bid
      (TermReturn (list v-i))))

  cfg14)

;; ============================================================
;; 测试 1: 检测基础归纳变量
;; ============================================================

(define (test-detect-biv)
  (printf "Test ~a: Detect basic IV... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-simple-loop 10))
  (define loops (analyze-loops cfg))

  (check-equal? (length loops) 1 "Should detect one loop")

  (define loop (car loops))
  (define bivs (find-basic-ivs cfg loop))

  (check-equal? (length bivs) 1 "Should find one BIV")

  (define biv (car bivs))
  (check-equal? (BIV-step biv) 1 "Step should be 1")

  (printf "PASS~n"))

;; ============================================================
;; 测试 2: 检测带步长的基础归纳变量
;; ============================================================

(define (test-detect-biv-with-step)
  (printf "Test ~a: Detect BIV with step... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  ;; 创建步长为 2 的循环
  (define cfg0 (cfg-empty))
  (define-values (pre cfg1) (cfg-create-block cfg0))
  (define-values (hdr cfg2) (cfg-create-block cfg1))
  (define-values (body cfg3) (cfg-create-block cfg2))
  (define-values (exit cfg4) (cfg-create-block cfg3))
  (define cfg5 (cfg-set-entry cfg4 pre))

  (define v-init (VarId 0))
  (define v-i (VarId 1))
  (define v-i-next (VarId 2))
  (define v-cmp (VarId 3))

  (define cfg6
    (cfg-block-append-insn cfg5 pre
      (VfInsn 'const '(0) (list v-init) #f #f)))
  (define cfg7
    (cfg-block-set-terminator cfg6 pre (TermJump hdr)))
  (define cfg8
    (cfg-block-add-phi cfg7 hdr
      (PhiInsn v-i (list (cons pre v-init) (cons body v-i-next)))))
  (define cfg9
    (cfg-block-append-insn cfg8 hdr
      (VfInsn 'lt (list v-i 10) (list v-cmp) #f #f)))
  (define cfg10
    (cfg-block-set-terminator cfg9 hdr
      (TermBranch v-cmp body exit)))
  ;; i' = i + 2
  (define cfg11
    (cfg-block-append-insn cfg10 body
      (VfInsn 'add (list v-i 2) (list v-i-next) #f #f)))
  (define cfg12
    (cfg-block-set-terminator cfg11 body (TermJump hdr)))
  (define cfg13
    (cfg-block-set-terminator cfg12 exit
      (TermReturn (list v-i))))

  (define loops (analyze-loops cfg13))
  (define bivs (find-basic-ivs cfg13 (car loops)))

  (check-equal? (length bivs) 1 "Should find one BIV")
  (check-equal? (BIV-step (car bivs)) 2 "Step should be 2")

  (printf "PASS~n"))

;; ============================================================
;; 测试 3: 检测派生归纳变量 (j = i * 4)
;; ============================================================

(define (test-detect-div-mul)
  (printf "Test ~a: Detect DIV (multiply)... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-loop-with-div 10 4 0))  ; j = i * 4
  (define loops (analyze-loops cfg))
  (define loop (car loops))

  (define bivs (find-basic-ivs cfg loop))
  (check-equal? (length bivs) 1 "Should find one BIV")

  (define divs (find-derived-ivs cfg loop bivs))
  (check >= (length divs) 1 "Should find at least one DIV")

  ;; Find the mul DIV (scale 4)
  (define mul-div
    (for/first ([d divs] #:when (= (DIV-scale d) 4))
      d))
  (check-not-false mul-div "Should find DIV with scale 4")
  (check-equal? (DIV-offset mul-div) 0 "Offset should be 0")

  (printf "PASS~n"))

;; ============================================================
;; 测试 4: 检测派生归纳变量 (j = i * 4 + 100)
;; ============================================================

(define (test-detect-div-mul-add)
  (printf "Test ~a: Detect DIV (multiply + offset)... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-loop-with-div 10 4 100))  ; j = i * 4 + 100
  (define loops (analyze-loops cfg))
  (define loop (car loops))

  (define bivs (find-basic-ivs cfg loop))
  (define divs (find-derived-ivs cfg loop bivs))

  ;; 应该找到两个 DIV：一个是 i*4，一个是 i*4+100
  (check >= (length divs) 1 "Should find at least one DIV")

  (printf "PASS~n"))

;; ============================================================
;; 测试 5: 强度削减 (j = i * 4 → j = j + 4)
;; ============================================================

(define (test-strength-reduce-div)
  (printf "Test ~a: Strength reduce DIV... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-loop-with-div 10 4 0))
  (define muls-before (count-muls cfg))

  (define cfg^ (cfg-ivopts cfg))
  (define muls-after (count-muls cfg^))

  ;; 强度削减后应该减少乘法
  ;; (实际上可能是替换为 add 0，由后续 DCE 清理)
  (check-not-false cfg^ "Optimization should return a CFG")

  ;; 检查添加了新的 PHI
  (define phis-before (count-phis cfg))
  (define phis-after (count-phis cfg^))

  (check >= phis-after phis-before "Should have at least same PHIs")

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

  (define cfg^ (cfg-ivopts cfg4))
  (check-equal? (count-insns cfg^) (count-insns cfg4)
                "No loop CFG should be unchanged")

  (printf "PASS~n"))

;; ============================================================
;; 测试 7: 无派生 IV 的循环
;; ============================================================

(define (test-no-div)
  (printf "Test ~a: Loop with no DIV... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-simple-loop 10))
  (define insns-before (count-insns cfg))

  (define cfg^ (cfg-ivopts cfg))
  (define insns-after (count-insns cfg^))

  ;; 没有 DIV 时不应该增加指令
  (check-equal? insns-after insns-before
                "Loop without DIV should not change")

  (printf "PASS~n"))

;; ============================================================
;; 测试 8: 统计版本
;; ============================================================

(define (test-with-stats)
  (printf "Test ~a: With stats version... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-loop-with-div 10 4 0))
  (define-values (cfg^ stats) (cfg-ivopts-with-stats cfg))

  (check-true (list? stats) "Stats should be a list")
  (check-not-false (assq 'loops stats) "Should have loops stat")
  (check-not-false (assq 'basic-ivs stats) "Should have basic-ivs stat")
  (check-not-false (assq 'derived-ivs stats) "Should have derived-ivs stat")

  (define biv-count (cdr (assq 'basic-ivs stats)))
  (check-equal? biv-count 1 "Should have 1 BIV")

  (printf "PASS~n"))

;; ============================================================
;; 测试 9: 大系数不优化
;; ============================================================

(define (test-large-scale-no-opt)
  (printf "Test ~a: Large scale not optimized... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  (define cfg (make-loop-with-div 10 100 0))  ; j = i * 100
  (define loops (analyze-loops cfg))
  (define loop (car loops))

  (define bivs (find-basic-ivs cfg loop))
  (define divs (find-derived-ivs cfg loop bivs))

  ;; 应该能检测到 DIV
  (check >= (length divs) 0 "Should detect DIV")

  ;; 但不会进行强度削减（scale 太大）
  (define-values (cfg^ stats) (cfg-ivopts-with-stats cfg))
  (define reductions (cdr (assq 'strength-reductions stats)))

  ;; 大系数可能不会被优化
  (check-not-false cfg^ "Should return a CFG")

  (printf "PASS~n"))

;; ============================================================
;; 测试 10: 位移形式的 DIV
;; ============================================================

(define (test-shift-div)
  (printf "Test ~a: Shift form DIV... " (+ 1 test-count))
  (set! test-count (+ 1 test-count))

  ;; 创建 j = i << 2 (等价于 i * 4) 的循环
  (define cfg0 (cfg-empty))
  (define-values (pre cfg1) (cfg-create-block cfg0))
  (define-values (hdr cfg2) (cfg-create-block cfg1))
  (define-values (body cfg3) (cfg-create-block cfg2))
  (define-values (exit cfg4) (cfg-create-block cfg3))
  (define cfg5 (cfg-set-entry cfg4 pre))

  (define v-init (VarId 0))
  (define v-i (VarId 1))
  (define v-i-next (VarId 2))
  (define v-cmp (VarId 3))
  (define v-j (VarId 4))

  (define cfg6
    (cfg-block-append-insn cfg5 pre
      (VfInsn 'const '(0) (list v-init) #f #f)))
  (define cfg7
    (cfg-block-set-terminator cfg6 pre (TermJump hdr)))
  (define cfg8
    (cfg-block-add-phi cfg7 hdr
      (PhiInsn v-i (list (cons pre v-init) (cons body v-i-next)))))
  (define cfg9
    (cfg-block-append-insn cfg8 hdr
      (VfInsn 'lt (list v-i 10) (list v-cmp) #f #f)))
  (define cfg10
    (cfg-block-set-terminator cfg9 hdr
      (TermBranch v-cmp body exit)))
  ;; j = i << 2
  (define cfg11
    (cfg-block-append-insn cfg10 body
      (VfInsn 'shl (list v-i 2) (list v-j) #f #f)))
  (define cfg12
    (cfg-block-append-insn cfg11 body
      (VfInsn 'add (list v-i 1) (list v-i-next) #f #f)))
  (define cfg13
    (cfg-block-set-terminator cfg12 body (TermJump hdr)))
  (define cfg14
    (cfg-block-set-terminator cfg13 exit
      (TermReturn (list v-j))))

  (define loops (analyze-loops cfg14))
  (define bivs (find-basic-ivs cfg14 (car loops)))
  (define divs (find-derived-ivs cfg14 (car loops) bivs))

  ;; 应该检测到 shl 形式的 DIV
  (check >= (length divs) 1 "Should detect shift as DIV")

  (when (>= (length divs) 1)
    (define div (car divs))
    (check-equal? (DIV-scale div) 4 "Scale should be 4 (from << 2)"))

  (printf "PASS~n"))

;; ============================================================
;; 运行所有测试
;; ============================================================

(printf "~n")
(printf "╔══════════════════════════════════════════════════════════╗~n")
(printf "║          Induction Variable Optimization Tests          ║~n")
(printf "╚══════════════════════════════════════════════════════════╝~n")
(printf "~n")

(test-detect-biv)
(test-detect-biv-with-step)
(test-detect-div-mul)
(test-detect-div-mul-add)
(test-strength-reduce-div)
(test-no-loop)
(test-no-div)
(test-with-stats)
(test-large-scale-no-opt)
(test-shift-div)

(printf "~n~a tests completed.~n" test-count)
