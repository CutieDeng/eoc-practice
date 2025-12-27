#lang racket/base

;; ============================================================
;; CFG Optimization: Strength Reduction
;; ============================================================
;;
;; 强度削减优化
;;
;; 用更便宜的操作替换昂贵的操作：
;;   1. 乘以 2 的幂 → 左移
;;   2. 除以 2 的幂 → 右移（无符号或已知正数）
;;   3. 模 2 的幂 → 按位与
;;   4. 乘以小常数 → 加法/移位组合
;;   5. 循环内归纳变量强度削减
;;
;; 参考：GCC tree-ssa-loop-ivopts.cc, fold-const.cc
;; ============================================================

(require racket/match racket/list racket/dict)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")

;; ============================================================
;; 辅助函数
;; ============================================================

;; 判断是否为 2 的幂
(define (power-of-2? n)
  (and (integer? n)
       (positive? n)
       (zero? (bitwise-and n (- n 1)))))

;; 计算 log2（仅对 2 的幂）
(define (log2 n)
  (if (= n 1)
      0
      (+ 1 (log2 (quotient n 2)))))

;; 判断是否可以用移位和加法高效实现乘法
;; 返回 (list (shift . add?) ...) 或 #f
;; 例如：x*5 = x*4 + x = (x<<2) + x → '((2 . #t))
(define (decompose-multiply n)
  (cond
    [(not (positive? n)) #f]
    [(power-of-2? n) (list (cons (log2 n) #f))]
    ;; x * 3 = x * 2 + x
    [(= n 3) '((1 . #t))]
    ;; x * 5 = x * 4 + x
    [(= n 5) '((2 . #t))]
    ;; x * 6 = x * 4 + x * 2
    [(= n 6) '((2 . #f) (1 . #t))]
    ;; x * 7 = x * 8 - x
    [(= n 7) '((3 . sub))]
    ;; x * 9 = x * 8 + x
    [(= n 9) '((3 . #t))]
    ;; x * 10 = x * 8 + x * 2
    [(= n 10) '((3 . #f) (1 . #t))]
    ;; x * 12 = x * 8 + x * 4
    [(= n 12) '((3 . #f) (2 . #t))]
    ;; x * 15 = x * 16 - x
    [(= n 15) '((4 . sub))]
    ;; x * 17 = x * 16 + x
    [(= n 17) '((4 . #t))]
    ;; 其他情况不优化
    [else #f]))

;; ============================================================
;; 强度削减转换
;; ============================================================

;; 尝试对单条指令进行强度削减
;; 返回 (values new-insns changed?)
;; new-insns 可能是多条指令（用于分解乘法）
(define (reduce-insn insn)
  (match insn
    ;; 乘以 2 的幂 → 左移
    [(VfInsn 'mul (list var (? power-of-2? n)) outputs info id)
     #:when (VarId? var)
     (values (list (VfInsn 'shl (list var (log2 n)) outputs info id))
             #t)]

    [(VfInsn 'mul (list (? power-of-2? n) var) outputs info id)
     #:when (VarId? var)
     (values (list (VfInsn 'shl (list var (log2 n)) outputs info id))
             #t)]

    ;; 乘以小常数 → 移位 + 加法组合
    [(VfInsn 'mul (list var (? integer? n)) (list out) info id)
     #:when (and (VarId? var) (decompose-multiply n))
     (define decomp (decompose-multiply n))
     (cond
       ;; 简单情况：2 的幂
       [(and (= (length decomp) 1) (not (cdar decomp)))
        (values (list (VfInsn 'shl (list var (caar decomp)) (list out) info id))
                #t)]
       ;; x * 3 = (x << 1) + x
       [(equal? decomp '((1 . #t)))
        (define tmp (VarId (+ (VarId-id out) 1000)))
        (values (list
                 (VfInsn 'shl (list var 1) (list tmp) #f #f)
                 (VfInsn 'add (list tmp var) (list out) info id))
                #t)]
       ;; x * 5 = (x << 2) + x
       [(equal? decomp '((2 . #t)))
        (define tmp (VarId (+ (VarId-id out) 1000)))
        (values (list
                 (VfInsn 'shl (list var 2) (list tmp) #f #f)
                 (VfInsn 'add (list tmp var) (list out) info id))
                #t)]
       ;; x * 7 = (x << 3) - x
       [(equal? decomp '((3 . sub)))
        (define tmp (VarId (+ (VarId-id out) 1000)))
        (values (list
                 (VfInsn 'shl (list var 3) (list tmp) #f #f)
                 (VfInsn 'sub (list tmp var) (list out) info id))
                #t)]
       ;; x * 9 = (x << 3) + x
       [(equal? decomp '((3 . #t)))
        (define tmp (VarId (+ (VarId-id out) 1000)))
        (values (list
                 (VfInsn 'shl (list var 3) (list tmp) #f #f)
                 (VfInsn 'add (list tmp var) (list out) info id))
                #t)]
       ;; x * 15 = (x << 4) - x
       [(equal? decomp '((4 . sub)))
        (define tmp (VarId (+ (VarId-id out) 1000)))
        (values (list
                 (VfInsn 'shl (list var 4) (list tmp) #f #f)
                 (VfInsn 'sub (list tmp var) (list out) info id))
                #t)]
       ;; x * 17 = (x << 4) + x
       [(equal? decomp '((4 . #t)))
        (define tmp (VarId (+ (VarId-id out) 1000)))
        (values (list
                 (VfInsn 'shl (list var 4) (list tmp) #f #f)
                 (VfInsn 'add (list tmp var) (list out) info id))
                #t)]
       [else (values (list insn) #f)])]

    [(VfInsn 'mul (list (? integer? n) var) (list out) info id)
     #:when (and (VarId? var) (decompose-multiply n))
     ;; 交换操作数后重新处理
     (reduce-insn (VfInsn 'mul (list var n) (list out) info id))]

    ;; 除以 2 的幂 → 右移（仅对无符号安全）
    ;; 注意：对有符号数，需要特殊处理负数
    ;; 这里保守处理，只对已知的无符号类型优化
    [(VfInsn 'udiv (list var (? power-of-2? n)) outputs info id)
     #:when (VarId? var)
     (values (list (VfInsn 'ushr (list var (log2 n)) outputs info id))
             #t)]

    ;; 模 2 的幂 → 按位与
    [(VfInsn 'urem (list var (? power-of-2? n)) outputs info id)
     #:when (VarId? var)
     (values (list (VfInsn 'and (list var (- n 1)) outputs info id))
             #t)]

    ;; 乘以 0 → 常量 0
    [(VfInsn 'mul (list _ 0) outputs info id)
     (values (list (VfInsn 'const '(0) outputs info id)) #t)]

    [(VfInsn 'mul (list 0 _) outputs info id)
     (values (list (VfInsn 'const '(0) outputs info id)) #t)]

    ;; 乘以 1 → 复制（保持原样，让 copy-prop 处理）
    [(VfInsn 'mul (list var 1) outputs info id)
     #:when (VarId? var)
     (values (list insn) #f)]

    [(VfInsn 'mul (list 1 var) outputs info id)
     #:when (VarId? var)
     (values (list insn) #f)]

    ;; 加 0 → 复制
    [(VfInsn 'add (list var 0) outputs info id)
     #:when (VarId? var)
     (values (list insn) #f)]

    [(VfInsn 'add (list 0 var) outputs info id)
     #:when (VarId? var)
     (values (list insn) #f)]

    ;; 左移 0 → 复制
    [(VfInsn 'shl (list var 0) outputs info id)
     (values (list insn) #f)]

    ;; 右移 0 → 复制
    [(VfInsn 'shr (list var 0) outputs info id)
     (values (list insn) #f)]

    [(VfInsn 'ushr (list var 0) outputs info id)
     (values (list insn) #f)]

    ;; 其他情况不优化
    [_ (values (list insn) #f)]))

;; ============================================================
;; 强度削减主体
;; ============================================================

;; 对单个块执行强度削减
(define (reduce-block block)
  (define-values (new-insns any-changed?)
    (for/fold ([acc '()] [changed #f])
              ([insn (CfgBlock-insns block)])
      (define-values (reduced insn-changed?) (reduce-insn insn))
      (values (append acc reduced) (or changed insn-changed?))))

  (values (struct-copy CfgBlock block [insns new-insns])
          any-changed?))

;; 强度削减主入口
(define (cfg-strength-reduce cfg)
  (define-values (cfg^ any-changed?)
    (for/fold ([cfg cfg] [changed #f])
              ([bid (cfg-all-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (if (not block)
          (values cfg changed)
          (let-values ([(new-block block-changed?)
                        (reduce-block block)])
            (values (if block-changed?
                       (cfg-set-block cfg new-block)
                       cfg)
                    (or changed block-changed?))))))
  cfg^)

(provide cfg-strength-reduce)

;; ============================================================
;; 带统计版本
;; ============================================================

(define (cfg-strength-reduce-with-stats cfg)
  (define reduction-count 0)

  (define (reduce-with-count insn)
    (define-values (reduced changed?) (reduce-insn insn))
    (when changed?
      (set! reduction-count (+ reduction-count 1)))
    reduced)

  (define cfg^
    (for/fold ([cfg cfg])
              ([bid (cfg-all-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (if (not block)
          cfg
          (let ([new-insns (append-map reduce-with-count (CfgBlock-insns block))])
            (cfg-set-block cfg
              (struct-copy CfgBlock block [insns new-insns]))))))

  (values cfg^
          `((reductions . ,reduction-count))))

(provide cfg-strength-reduce-with-stats)

;; ============================================================
;; 高级：循环归纳变量强度削减
;; ============================================================

;; 在循环中，表达式如 i * k 可以转换为累加器
;; 例如：
;;   for (i = 0; i < n; i++)
;;     arr[i * 4] = ...
;; 转换为：
;;   for (i = 0, p = 0; i < n; i++, p += 4)
;;     arr[p] = ...
;;
;; 这需要循环分析支持，暂不实现

;; ============================================================
;; 代数简化（额外优化）
;; ============================================================

;; x - x = 0
;; x ^ x = 0
;; x & x = x
;; x | x = x
;; x + (-x) = 0
;; 这些简化可以在这里或单独的 pass 中处理

(define (algebraic-simplify insn)
  (match insn
    ;; x - x = 0
    [(VfInsn 'sub (list v1 v2) outputs info id)
     #:when (and (VarId? v1) (VarId? v2) (equal? v1 v2))
     (values (VfInsn 'const '(0) outputs info id) #t)]

    ;; x ^ x = 0
    [(VfInsn 'xor (list v1 v2) outputs info id)
     #:when (and (VarId? v1) (VarId? v2) (equal? v1 v2))
     (values (VfInsn 'const '(0) outputs info id) #t)]

    ;; x & 0 = 0
    [(VfInsn 'and (list _ 0) outputs info id)
     (values (VfInsn 'const '(0) outputs info id) #t)]

    [(VfInsn 'and (list 0 _) outputs info id)
     (values (VfInsn 'const '(0) outputs info id) #t)]

    ;; x | 0 = x (保持原样)
    [(VfInsn 'or (list var 0) outputs info id)
     #:when (VarId? var)
     (values insn #f)]

    [(VfInsn 'or (list 0 var) outputs info id)
     #:when (VarId? var)
     (values insn #f)]

    ;; x & -1 = x (保持原样)
    [(VfInsn 'and (list var -1) outputs info id)
     #:when (VarId? var)
     (values insn #f)]

    ;; x | -1 = -1
    [(VfInsn 'or (list _ -1) outputs info id)
     (values (VfInsn 'const '(-1) outputs info id) #t)]

    [(VfInsn 'or (list -1 _) outputs info id)
     (values (VfInsn 'const '(-1) outputs info id) #t)]

    [_ (values insn #f)]))

;; 代数简化 pass
(define (cfg-algebraic-simplify cfg)
  (for/fold ([cfg cfg])
            ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if (not block)
        cfg
        (let ()
          (define-values (new-insns _)
            (for/fold ([acc '()] [changed #f])
                      ([insn (CfgBlock-insns block)])
              (define-values (new-insn insn-changed?)
                (algebraic-simplify insn))
              (values (cons new-insn acc) (or changed insn-changed?))))
          (cfg-set-block cfg
            (struct-copy CfgBlock block
                         [insns (reverse new-insns)]))))))

(provide cfg-algebraic-simplify)
