#lang racket/base

;; ============================================================
;; CFG Optimization: Global Value Numbering (GVN)
;; ============================================================
;;
;; 全局值编号优化
;;
;; 通过给每个计算值分配唯一编号来消除冗余计算：
;;   1. 相同操作符 + 相同操作数值编号 → 相同结果值编号
;;   2. 发现冗余表达式时，替换为对先前计算的引用
;;
;; 示例：
;;   v1 = a + b
;;   v2 = a + b    ; 冗余，替换为 v2 = v1
;;   v3 = v1 + v2  ; 使用 v1 而不是重新计算
;;
;; 注意：
;;   - 只处理纯操作（无副作用）
;;   - 需要与 DCE 配合消除死代码
;;
;; 参考：GCC tree-ssa-sccvn.cc
;; ============================================================

(require racket/match racket/list racket/dict racket/set)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")

;; ============================================================
;; 值编号表示
;; ============================================================

;; 值编号（用整数表示）
(define next-vn 0)
(define (fresh-value-number!)
  (begin0 next-vn
    (set! next-vn (+ next-vn 1))))

(define (reset-value-numbers!)
  (set! next-vn 0))

;; 表达式的规范形式（用于哈希）
;; (op vn1 vn2 ...) 或 (const value) 或 (var var-id)
(struct CanonExpr (op args) #:prefab)

;; ============================================================
;; GVN 状态
;; ============================================================

;; GVN 分析状态
(struct GVNState
  (var-to-vn      ; VarId → ValueNumber
   expr-to-vn     ; CanonExpr → ValueNumber
   vn-to-var      ; ValueNumber → VarId（第一个定义该值的变量）
   vn-to-const)   ; ValueNumber → constant（如果是常量）
  #:mutable)

(define (make-gvn-state)
  (GVNState
   (ordl-make-empty var-id-compare)
   (make-hash)  ; 使用 Racket 原生 hash 表
   (make-hash)
   (make-hash)))

;; 获取变量的值编号
(define (get-var-vn state var)
  (dict-ref (GVNState-var-to-vn state) var #f))

;; 设置变量的值编号
(define (set-var-vn! state var vn)
  (set-GVNState-var-to-vn! state
    (dict-set (GVNState-var-to-vn state) var vn))
  ;; 如果这是第一个定义该值编号的变量，记录它
  (unless (hash-has-key? (GVNState-vn-to-var state) vn)
    (hash-set! (GVNState-vn-to-var state) vn var)))

;; 查找表达式的值编号
(define (lookup-expr-vn state expr)
  (hash-ref (GVNState-expr-to-vn state) expr #f))

;; 添加表达式的值编号
(define (add-expr-vn! state expr vn)
  (hash-set! (GVNState-expr-to-vn state) expr vn))

;; 获取值编号对应的代表变量
(define (get-vn-leader state vn)
  (hash-ref (GVNState-vn-to-var state) vn #f))

;; 记录常量值编号
(define (set-vn-const! state vn val)
  (hash-set! (GVNState-vn-to-const state) vn val))

;; 获取值编号的常量值
(define (get-vn-const state vn)
  (hash-ref (GVNState-vn-to-const state) vn #f))

;; ============================================================
;; 值编号计算
;; ============================================================

;; 判断操作是否为纯操作（可以进行 GVN）
(define (pure-op? op)
  (case op
    ;; 算术运算
    [(const add sub mul div rem neg) #t]
    ;; 位运算
    [(and or xor shl shr ushr) #t]
    ;; 比较运算
    [(eq ne lt le gt ge eq0 ne0) #t]
    ;; 类型转换
    [(i2l i2f i2d l2i l2f l2d f2i f2l f2d d2i d2l d2f) #t]
    [(i2b i2c i2s) #t]
    ;; 数组/字段读取（假设无别名，简化处理）
    ;; 实际需要别名分析
    [(arraylength) #t]
    ;; 其他有副作用的操作不能 GVN
    [else #f]))

;; 判断操作是否可交换
(define (commutative-op? op)
  (case op
    [(add mul and or xor eq ne) #t]
    [else #f]))

;; 规范化表达式（处理可交换操作）
(define (canonicalize-expr op arg-vns)
  (if (and (commutative-op? op) (= (length arg-vns) 2))
      ;; 对可交换操作，按值编号排序参数
      (CanonExpr op (sort arg-vns <))
      (CanonExpr op arg-vns)))

;; 为输入值获取或创建值编号
(define (get-input-vn state inp)
  (cond
    [(VarId? inp)
     (or (get-var-vn state inp)
         ;; 如果变量没有值编号，分配一个新的
         (let ([vn (fresh-value-number!)])
           (set-var-vn! state inp vn)
           vn))]
    ;; 常量
    [(number? inp)
     (define expr (CanonExpr 'const (list inp)))
     (or (lookup-expr-vn state expr)
         (let ([vn (fresh-value-number!)])
           (add-expr-vn! state expr vn)
           (set-vn-const! state vn inp)
           vn))]
    ;; 其他类型暂不处理
    [else #f]))

;; 处理指令，返回 (values new-insn changed?)
(define (process-insn-gvn insn state)
  (match insn
    [(VfInsn 'const (list value) (list out) info id)
     ;; 常量指令
     (define expr (CanonExpr 'const (list value)))
     (define existing-vn (lookup-expr-vn state expr))
     (cond
       [existing-vn
        ;; 已存在相同常量
        (set-var-vn! state out existing-vn)
        (define leader (get-vn-leader state existing-vn))
        (if (and leader (not (equal? leader out)))
            ;; 可以替换为 copy（但我们的 IR 没有 copy，保持原样让 DCE 处理）
            (values insn #f)
            (values insn #f))]
       [else
        ;; 新常量
        (define vn (fresh-value-number!))
        (add-expr-vn! state expr vn)
        (set-var-vn! state out vn)
        (set-vn-const! state vn value)
        (values insn #f)])]

    [(VfInsn op inputs (list out) info id)
     #:when (pure-op? op)
     ;; 纯操作，尝试 GVN
     (define flat-inputs (flatten inputs))
     (define input-vns
       (for/list ([inp flat-inputs])
         (get-input-vn state inp)))

     (cond
       [(ormap not input-vns)
        ;; 某些输入无法处理
        (define vn (fresh-value-number!))
        (set-var-vn! state out vn)
        (values insn #f)]

       [else
        (define expr (canonicalize-expr op input-vns))
        (define existing-vn (lookup-expr-vn state expr))

        (cond
          [existing-vn
           ;; 找到冗余表达式！
           (set-var-vn! state out existing-vn)
           (define leader (get-vn-leader state existing-vn))
           (if (and leader (not (equal? leader out)))
               ;; 替换为对 leader 的复制
               ;; 由于我们没有 copy 指令，生成一个 const 如果是常量
               ;; 或者保持原样让后续优化处理
               (let ([const-val (get-vn-const state existing-vn)])
                 (if const-val
                     (values (VfInsn 'const (list const-val) (list out) info id) #t)
                     ;; 无法直接替换，但记录了等价关系
                     (values insn #f)))
               (values insn #f))]

          [else
           ;; 新表达式
           (define vn (fresh-value-number!))
           (add-expr-vn! state expr vn)
           (set-var-vn! state out vn)
           (values insn #f)])])]

    [(VfInsn op inputs outputs info id)
     ;; 非纯操作或多输出，为每个输出分配新值编号
     (for ([out outputs])
       (define vn (fresh-value-number!))
       (set-var-vn! state out vn))
     (values insn #f)]

    [_ (values insn #f)]))

;; ============================================================
;; GVN 主体
;; ============================================================

;; 对单个块执行 GVN
(define (gvn-block block state)
  (define-values (new-insns changed?)
    (for/fold ([acc '()] [changed #f])
              ([insn (CfgBlock-insns block)])
      (define-values (new-insn insn-changed?)
        (process-insn-gvn insn state))
      (values (cons new-insn acc) (or changed insn-changed?))))

  (values (struct-copy CfgBlock block
                       [insns (reverse new-insns)])
          changed?))

;; GVN 主入口
(define (cfg-gvn cfg)
  (reset-value-numbers!)
  (define state (make-gvn-state))

  ;; 按 RPO 顺序处理块（简化版：按 ID 顺序）
  (define-values (cfg^ any-changed?)
    (for/fold ([cfg cfg] [changed #f])
              ([bid (cfg-all-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (if (not block)
          (values cfg changed)
          (let-values ([(new-block block-changed?)
                        (gvn-block block state)])
            (values (if block-changed?
                       (cfg-set-block cfg new-block)
                       cfg)
                    (or changed block-changed?))))))

  cfg^)

(provide cfg-gvn)

;; ============================================================
;; 带统计版本
;; ============================================================

(define (cfg-gvn-with-stats cfg)
  (reset-value-numbers!)
  (define state (make-gvn-state))

  (define redundant-count 0)

  ;; 自定义处理以统计
  (define (process-with-count insn)
    (define-values (new-insn changed?) (process-insn-gvn insn state))
    (when changed?
      (set! redundant-count (+ redundant-count 1)))
    new-insn)

  (define cfg^
    (for/fold ([cfg cfg])
              ([bid (cfg-all-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (if (not block)
          cfg
          (let ([new-insns (map process-with-count (CfgBlock-insns block))])
            (cfg-set-block cfg
              (struct-copy CfgBlock block [insns new-insns]))))))

  (values cfg^
          `((redundant-exprs-found . ,redundant-count)
            (unique-values . ,next-vn))))

(provide cfg-gvn-with-stats)

;; ============================================================
;; 高级 GVN：基于支配的值编号
;; ============================================================

;; 完整的 GVN 需要考虑支配关系
;; 只有在支配路径上定义的值才能安全替换
;; 这需要先计算支配树

;; 简化版本：只在同一块内进行 GVN
(define (cfg-local-gvn cfg)
  (for/fold ([cfg cfg])
            ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if (not block)
        cfg
        (let ()
          ;; 每个块使用独立的状态
          (reset-value-numbers!)
          (define state (make-gvn-state))
          (define-values (new-block _) (gvn-block block state))
          (cfg-set-block cfg new-block)))))

(provide cfg-local-gvn)
