#lang racket/base

;; ============================================================
;; CFG Optimization: Constant Folding
;; ============================================================
;;
;; 常量折叠优化
;;
;; 对于所有输入均为常量的纯运算，在编译时求值
;; 将运算指令替换为常量指令
;;
;; 支持的操作：
;;   - 算术：add, sub, mul, div, rem, neg
;;   - 位运算：shl, shr, ushr, and, or, xor
;;   - 比较：eq, ne, lt, le, gt, ge, eq0, ne0, lt0, le0, gt0, ge0
;;   - 类型转换：i2l, i2f, etc.
;; ============================================================

(require racket/match racket/list racket/dict)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")

;; ============================================================
;; 常量值提取
;; ============================================================

;; 从 VfInsn 提取常量值（如果是 const 指令）
;; 返回 (cons var-id value) 或 #f
(define (extract-const-def insn)
  (match insn
    [(VfInsn 'const (list value) (list out-var) _ _)
     (cons out-var value)]
    [_ #f]))

;; 构建 VarId → 常量值 的映射
(define (build-const-map cfg)
  (for*/fold ([const-map (ordl-make-empty var-id-compare)])
             ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if (not block)
        const-map
        (for/fold ([m const-map])
                  ([insn (CfgBlock-insns block)])
          (define def (extract-const-def insn))
          (if def
              (dict-set m (car def) (cdr def))
              m)))))

;; 查询变量是否为常量，返回值或 #f
(define (lookup-const const-map var-id)
  (and (VarId? var-id)
       (dict-ref const-map var-id #f)))

;; ============================================================
;; 可折叠操作定义
;; ============================================================

;; 二元算术运算
(define (fold-binary-arith op v1 v2)
  (case op
    [(add) (+ v1 v2)]
    [(sub) (- v1 v2)]
    [(mul) (* v1 v2)]
    [(div) (if (zero? v2) #f (quotient v1 v2))]  ; 避免除零
    [(rem) (if (zero? v2) #f (remainder v1 v2))]
    [else #f]))

;; 二元位运算
(define (fold-binary-bitwise op v1 v2)
  (case op
    [(shl) (arithmetic-shift v1 v2)]
    [(shr) (arithmetic-shift v1 (- v2))]
    [(ushr) (arithmetic-shift v1 (- v2))]  ; 简化处理，实际需要无符号
    [(and) (bitwise-and v1 v2)]
    [(or) (bitwise-ior v1 v2)]
    [(xor) (bitwise-xor v1 v2)]
    [else #f]))

;; 二元比较运算
(define (fold-binary-compare op v1 v2)
  (case op
    [(eq) (if (= v1 v2) 1 0)]
    [(ne) (if (not (= v1 v2)) 1 0)]
    [(lt) (if (< v1 v2) 1 0)]
    [(le) (if (<= v1 v2) 1 0)]
    [(gt) (if (> v1 v2) 1 0)]
    [(ge) (if (>= v1 v2) 1 0)]
    [(lcmp) (cond [(< v1 v2) -1] [(= v1 v2) 0] [else 1])]
    [(cmpl cmpg) (cond [(< v1 v2) -1] [(= v1 v2) 0] [else 1])]
    [else #f]))

;; 一元运算
(define (fold-unary op v)
  (case op
    [(neg) (- v)]
    ;; 与零比较
    [(eq0) (if (zero? v) 1 0)]
    [(ne0) (if (not (zero? v)) 1 0)]
    [(lt0) (if (< v 0) 1 0)]
    [(le0) (if (<= v 0) 1 0)]
    [(gt0) (if (> v 0) 1 0)]
    [(ge0) (if (>= v 0) 1 0)]
    ;; 类型转换（简化处理）
    [(i2l i2f i2d) (exact->inexact v)]
    [(l2i f2i d2i) (inexact->exact (truncate v))]
    [(l2f l2d f2d) (exact->inexact v)]
    [(d2f) v]
    [(f2l d2l) (inexact->exact (truncate v))]
    [(i2b) (bitwise-and v #xFF)]
    [(i2c) (bitwise-and v #xFFFF)]
    [(i2s) (let ([masked (bitwise-and v #xFFFF)])
             (if (> masked #x7FFF)
                 (- masked #x10000)
                 masked))]
    [else #f]))

;; ============================================================
;; 指令折叠
;; ============================================================

;; 尝试折叠单条指令
;; 返回新指令或 #f（无法折叠）
(define (try-fold-insn insn const-map)
  (match insn
    ;; 已经是常量，不需要折叠
    [(VfInsn 'const _ _ _ _) #f]

    ;; 二元运算：检查两个输入是否都是常量
    [(VfInsn op (list v1 v2) (list out) info id)
     #:when (and (VarId? v1) (VarId? v2))
     (define c1 (lookup-const const-map v1))
     (define c2 (lookup-const const-map v2))
     (and c1 c2
          (let ([result (or (fold-binary-arith op c1 c2)
                           (fold-binary-bitwise op c1 c2)
                           (fold-binary-compare op c1 c2))])
            (and result
                 (VfInsn 'const (list result) (list out) info id))))]

    ;; 一元运算：检查输入是否为常量
    [(VfInsn op (list v) (list out) info id)
     #:when (VarId? v)
     (define c (lookup-const const-map v))
     (and c
          (let ([result (fold-unary op c)])
            (and result
                 (VfInsn 'const (list result) (list out) info id))))]

    ;; 其他情况不折叠
    [_ #f]))

;; ============================================================
;; Pass 主体
;; ============================================================

;; 对单个基本块执行常量折叠
;; 返回 (values new-block changed?)
(define (fold-block block const-map)
  (define-values (new-insns changed?)
    (for/fold ([acc '()] [changed #f])
              ([insn (CfgBlock-insns block)])
      (define folded (try-fold-insn insn const-map))
      (if folded
          (values (cons folded acc) #t)
          (values (cons insn acc) changed))))
  (values (struct-copy CfgBlock block
                       [insns (reverse new-insns)])
          changed?))

;; 常量折叠主入口
;; 迭代执行直到不再有变化
(define (cfg-const-fold cfg)
  (let loop ([cfg cfg] [iterations 0])
    (when (> iterations 100)
      (error 'cfg-const-fold "Too many iterations, possible infinite loop"))

    ;; 重新构建常量映射
    (define const-map (build-const-map cfg))

    ;; 对所有块执行折叠
    (define-values (cfg^ any-changed?)
      (for/fold ([cfg cfg] [changed #f])
                ([bid (cfg-all-block-ids cfg)])
        (define block (cfg-get-block cfg bid))
        (if (not block)
            (values cfg changed)
            (let-values ([(new-block block-changed?) (fold-block block const-map)])
              (values (if block-changed?
                         (cfg-set-block cfg new-block)
                         cfg)
                      (or changed block-changed?))))))

    ;; 如果有变化，继续迭代
    (if any-changed?
        (loop cfg^ (+ iterations 1))
        cfg^)))

(provide cfg-const-fold)
(provide build-const-map lookup-const)  ; 导出供其他优化使用

;; ============================================================
;; 统计信息（可选）
;; ============================================================

;; 统计折叠了多少条指令
(define (cfg-const-fold-with-stats cfg)
  (define before-count (cfg-insn-count cfg))
  (define cfg^ (cfg-const-fold cfg))
  (define after-const-count (count-const-insns cfg^))
  (values cfg^
          (list (cons 'folded-ops (- after-const-count (count-const-insns cfg))))))

(define (cfg-insn-count cfg)
  (for/sum ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if block (length (CfgBlock-insns block)) 0)))

(define (count-const-insns cfg)
  (for/sum ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if block
        (count (λ (insn)
                 (and (VfInsn? insn)
                      (eq? (VfInsn-op insn) 'const)))
               (CfgBlock-insns block))
        0)))

(provide cfg-const-fold-with-stats)
