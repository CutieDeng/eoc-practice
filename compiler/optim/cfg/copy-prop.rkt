#lang racket/base

;; ============================================================
;; CFG Optimization: Copy Propagation
;; ============================================================
;;
;; 复写传播优化
;;
;; 将变量的使用替换为其定义的值（当定义是简单的复制时）
;;
;; 优化模式：
;;   1. 常量传播：v1 = const 5, ... use v1 ... → ... use 5 ...
;;   2. 变量传播：v2 的定义如果只是传递 v1，则用 v1 替换 v2
;;
;; 注意：此优化与常量折叠协同工作
;;   - 常量折叠：计算常量表达式
;;   - 复写传播：传播常量和变量引用
;;   - DCE：删除无用的中间变量
;;
;; 参考：GCC tree-ssa-copy.cc, tree-ssa-forwprop.cc
;; ============================================================

(require racket/match racket/list racket/dict racket/set)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")

;; ============================================================
;; 值映射构建
;; ============================================================

;; 表示一个可传播的值
(struct PropValue (kind data) #:prefab)
;; kind: 'const | 'var
;; data: 常量值 | VarId

;; 从指令提取可传播的值定义
;; 返回 (cons output-var PropValue) 或 #f
(define (extract-propagatable-def insn)
  (match insn
    ;; 常量定义
    [(VfInsn 'const (list value) (list out) _ _)
     (cons out (PropValue 'const value))]

    ;; load-local 本质上是从局部变量槽读取
    ;; 我们可以追踪它，但不直接传播
    ;; （因为局部变量槽可能被多次写入）

    ;; 其他情况暂不处理
    [_ #f]))

;; 构建 VarId → PropValue 的映射
(define (build-prop-map cfg)
  (for*/fold ([prop-map (ordl-make-empty var-id-compare)])
             ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if (not block)
        prop-map
        (for/fold ([m prop-map])
                  ([insn (CfgBlock-insns block)])
          (define def (extract-propagatable-def insn))
          (if def
              (dict-set m (car def) (cdr def))
              m)))))

;; 解析 PropValue 到最终值
;; 处理传递链：v1 → v2 → v3 → const
(define (resolve-prop-value prop-map var-id [visited (set)])
  (cond
    [(set-member? visited var-id) #f]  ; 循环检测
    [else
     (define pv (dict-ref prop-map var-id #f))
     (match pv
       [(PropValue 'const val) pv]
       [(PropValue 'var other-var)
        (resolve-prop-value prop-map other-var (set-add visited var-id))]
       [#f #f])]))

;; ============================================================
;; 指令重写
;; ============================================================

;; 替换指令输入中的变量引用
;; 如果变量映射到常量，进行替换
(define (rewrite-inputs inputs prop-map)
  (define (rewrite-single v)
    (if (VarId? v)
        (let ([pv (resolve-prop-value prop-map v)])
          (match pv
            [(PropValue 'const val)
             ;; 将 VarId 替换为常量值
             ;; 注意：这需要后续指令能够处理内联常量
             ;; 为简单起见，我们只记录替换，不直接内联
             v]  ; 暂时保持原样，让 const-fold 处理
            [_ v]))
        v))

  ;; 递归处理嵌套列表
  (define (rewrite-recursive datum)
    (cond
      [(VarId? datum) (rewrite-single datum)]
      [(list? datum) (map rewrite-recursive datum)]
      [else datum]))

  (rewrite-recursive inputs))

;; 重写单条指令
;; 返回 (values new-insn changed?)
(define (rewrite-insn insn prop-map)
  (match insn
    [(VfInsn op inputs outputs info id)
     (define new-inputs (rewrite-inputs inputs prop-map))
     (if (equal? inputs new-inputs)
         (values insn #f)
         (values (VfInsn op new-inputs outputs info id) #t))]
    [_ (values insn #f)]))

;; ============================================================
;; 高级复写传播：变量替换
;; ============================================================

;; 检查指令是否是简单的变量复制
;; 返回 (cons dest-var src-var) 或 #f
(define (extract-var-copy insn)
  ;; 目前我们的 IR 没有显式的 copy 指令
  ;; 但可以识别一些等价模式
  #f)

;; 构建变量等价类
;; 返回 VarId → VarId（规范代表）的映射
(define (build-var-equiv-map cfg)
  (define copy-pairs
    (for*/list ([bid (cfg-all-block-ids cfg)]
                [block (in-value (cfg-get-block cfg bid))]
                #:when block
                [insn (CfgBlock-insns block)]
                [pair (in-value (extract-var-copy insn))]
                #:when pair)
      pair))

  ;; 构建等价类（使用 union-find 简化版）
  (define equiv-map (ordl-make-empty var-id-compare))
  (for/fold ([m equiv-map])
            ([pair copy-pairs])
    (dict-set m (car pair) (cdr pair))))

;; 找到变量的规范代表
(define (find-canonical equiv-map var-id [visited (set)])
  (cond
    [(set-member? visited var-id) var-id]  ; 循环检测
    [else
     (define rep (dict-ref equiv-map var-id #f))
     (if rep
         (find-canonical equiv-map rep (set-add visited var-id))
         var-id)]))

;; ============================================================
;; 常量传播增强版
;; ============================================================

;; 如果一个变量的所有输入都是常量，且操作可以求值，则传播结果
;; 这与 const-fold 配合，但从使用侧驱动

;; 检查表达式是否所有输入都已知
(define (all-inputs-known? inputs prop-map)
  (andmap (λ (v)
            (or (not (VarId? v))
                (resolve-prop-value prop-map v)))
          (flatten inputs)))

;; ============================================================
;; Pass 主体
;; ============================================================

;; 对单个块执行复写传播
(define (propagate-block block prop-map equiv-map)
  (define-values (new-insns changed?)
    (for/fold ([acc '()] [changed #f])
              ([insn (CfgBlock-insns block)])
      ;; 1. 变量替换（用规范代表）
      (define insn1
        (match insn
          [(VfInsn op inputs outputs info id)
           (define (subst-var v)
             (if (VarId? v)
                 (find-canonical equiv-map v)
                 v))
           (define (subst-recursive datum)
             (cond
               [(VarId? datum) (subst-var datum)]
               [(list? datum) (map subst-recursive datum)]
               [else datum]))
           (define new-inputs (subst-recursive inputs))
           (if (equal? inputs new-inputs)
               insn
               (VfInsn op new-inputs outputs info id))]
          [_ insn]))

      ;; 2. 记录是否有变化
      (define insn-changed? (not (equal? insn insn1)))
      (values (cons insn1 acc) (or changed insn-changed?))))

  (values (struct-copy CfgBlock block
                       [insns (reverse new-insns)])
          changed?))

;; 复写传播主入口
(define (cfg-copy-prop cfg)
  ;; 构建传播映射
  (define prop-map (build-prop-map cfg))
  (define equiv-map (build-var-equiv-map cfg))

  ;; 对所有块执行传播
  (define-values (cfg^ any-changed?)
    (for/fold ([cfg cfg] [changed #f])
              ([bid (cfg-all-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (if (not block)
          (values cfg changed)
          (let-values ([(new-block block-changed?)
                        (propagate-block block prop-map equiv-map)])
            (values (if block-changed?
                       (cfg-set-block cfg new-block)
                       cfg)
                    (or changed block-changed?))))))

  cfg^)

(provide cfg-copy-prop)

;; ============================================================
;; 简化后的使用计数分析
;; ============================================================

;; 统计每个变量的使用次数
(define (count-var-uses cfg)
  (define use-count (ordl-make-empty var-id-compare))

  (define (count-in-datum datum m)
    (cond
      [(VarId? datum)
       (dict-update m datum add1 0)]
      [(list? datum)
       (for/fold ([m m]) ([d datum])
         (count-in-datum d m))]
      [else m]))

  ;; 统计指令中的使用
  (for*/fold ([m use-count])
             ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if (not block)
        m
        (let ()
          ;; 指令输入
          (define m1
            (for/fold ([m m])
                      ([insn (CfgBlock-insns block)])
              (if (VfInsn? insn)
                  (count-in-datum (VfInsn-inputs insn) m)
                  m)))
          ;; terminator
          (define term (CfgBlock-terminator block))
          (count-in-datum (terminator-uses term) m1)))))

(provide count-var-uses)

;; ============================================================
;; 单次使用变量内联（激进优化）
;; ============================================================

;; 如果一个变量只被使用一次，可以考虑内联其定义
;; 这需要更复杂的分析，暂时不实现

;; ============================================================
;; 统计版本
;; ============================================================

(define (cfg-copy-prop-with-stats cfg)
  (define cfg^ (cfg-copy-prop cfg))
  (values cfg^ '()))

(provide cfg-copy-prop-with-stats)
