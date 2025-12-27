#lang racket/base

;; ============================================================
;; CFG Optimization: Expression Reassociation
;; ============================================================
;;
;; 表达式重结合优化
;;
;; 重排关联/交换运算的操作数，优化表达式求值：
;;   1. 常量聚合：将多个常量合并为一个
;;   2. 表达式扁平化：(a + b) + c → a + b + c
;;   3. 操作数排序：按 rank 排序以增加 CSE 机会
;;
;; 支持的关联运算：
;;   - add (加法)
;;   - mul (乘法)
;;   - and (位与)
;;   - or  (位或)
;;   - xor (位异或)
;;
;; 参考：GCC tree-ssa-reassoc.cc
;; ============================================================

(require racket/match racket/list racket/dict racket/set)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")

;; ============================================================
;; 关联运算识别
;; ============================================================

;; 检查操作是否关联且可交换
(define (associative-op? op)
  (memq op '(add mul and or xor)))

;; 获取操作的单位元
(define (identity-element op)
  (case op
    [(add) 0]
    [(mul) 1]
    [(and) -1]  ; 全1
    [(or)  0]
    [(xor) 0]
    [else #f]))

;; 折叠两个常量
(define (fold-constants op c1 c2)
  (case op
    [(add) (+ c1 c2)]
    [(mul) (* c1 c2)]
    [(and) (bitwise-and c1 c2)]
    [(or)  (bitwise-ior c1 c2)]
    [(xor) (bitwise-xor c1 c2)]
    [else #f]))

;; ============================================================
;; 使用计数和定义映射
;; ============================================================

;; 统计每个变量的使用次数
(define (count-uses cfg)
  (define use-count (ordl-make-empty var-id-compare))

  (define (count-in-datum datum m)
    (cond
      [(VarId? datum)
       (dict-update m datum add1 0)]
      [(list? datum)
       (for/fold ([m m]) ([d datum])
         (count-in-datum d m))]
      [else m]))

  (for*/fold ([m use-count])
             ([bid (cfg-all-block-ids cfg)]
              [block (in-value (cfg-get-block cfg bid))]
              #:when block)
    (define m1
      (for/fold ([m m])
                ([insn (CfgBlock-insns block)])
        (if (VfInsn? insn)
            (count-in-datum (VfInsn-inputs insn) m)
            m)))
    (define m2
      (for/fold ([m m1])
                ([phi (CfgBlock-phis block)])
        (for/fold ([m m])
                  ([src (PhiInsn-sources phi)])
          (count-in-datum (cdr src) m))))
    (count-in-datum (terminator-uses (CfgBlock-terminator block)) m2)))

;; 检查变量是否只被使用一次
(define (single-use? use-count var)
  (= 1 (dict-ref use-count var 0)))

;; 构建定义映射
(define (build-def-map cfg)
  (for*/fold ([def-map (ordl-make-empty var-id-compare)])
             ([bid (cfg-all-block-ids cfg)]
              [block (in-value (cfg-get-block cfg bid))]
              #:when block
              [insn (CfgBlock-insns block)]
              #:when (VfInsn? insn)
              [out (VfInsn-outputs insn)])
    (dict-set def-map out insn)))

;; ============================================================
;; 表达式树扁平化
;; ============================================================

;; 收集关联操作链中的所有操作数
;; 返回 (list-of (or VarId integer))
(define (collect-operands op var def-map use-count collected)
  (cond
    ;; 已经收集过
    [(set-member? collected var)
     (values (list var) collected)]

    ;; 检查变量的定义
    [else
     (define insn (dict-ref def-map var #f))
     (cond
       ;; 没有定义（参数）或非 VfInsn
       [(not insn)
        (values (list var) (set-add collected var))]

       ;; 定义不是同类型的关联操作
       [(not (and (VfInsn? insn)
                  (eq? (VfInsn-op insn) op)))
        (values (list var) (set-add collected var))]

       ;; 被多次使用，不能内联
       [(not (single-use? use-count var))
        (values (list var) (set-add collected var))]

       ;; 递归收集操作数
       [else
        (match (VfInsn-inputs insn)
          [(list a b)
           (define collected1 (set-add collected var))
           (define-values (ops-a collected2)
             (if (VarId? a)
                 (collect-operands op a def-map use-count collected1)
                 (values (list a) collected1)))
           (define-values (ops-b collected3)
             (if (VarId? b)
                 (collect-operands op b def-map use-count collected2)
                 (values (list b) collected2)))
           (values (append ops-a ops-b) collected3)]
          [_
           (values (list var) (set-add collected var))])])]))

;; ============================================================
;; 重结合优化
;; ============================================================

;; 对操作数列表进行优化
;; 返回 (values optimized-operands constant-result)
(define (optimize-operands op operands)
  ;; 分离常量和变量
  (define-values (constants vars)
    (partition integer? operands))

  ;; 折叠所有常量
  (define identity (identity-element op))
  (define folded-const
    (if (null? constants)
        identity
        (foldl (λ (c acc) (fold-constants op c acc))
               (car constants)
               (cdr constants))))

  ;; 构建结果
  (cond
    ;; 只有常量
    [(null? vars)
     (values '() folded-const)]

    ;; 常量是单位元，忽略
    [(= folded-const identity)
     (values vars #f)]

    ;; 有常量和变量
    [else
     (values vars folded-const)]))

;; 尝试对单条指令进行重结合
;; 返回 (values new-insns changed?)
(define (try-reassoc-insn insn def-map use-count)
  (match insn
    [(VfInsn op inputs (list output) info id)
     #:when (and (associative-op? op)
                 (= 2 (length inputs)))

     ;; 收集所有操作数
     (define-values (all-operands _)
       (let loop ([inputs inputs] [collected (set)])
         (if (null? inputs)
             (values '() collected)
             (let* ([input (car inputs)]
                    [rest (cdr inputs)])
               (if (VarId? input)
                   (let-values ([(ops col) (collect-operands op input def-map use-count collected)])
                     (let-values ([(rest-ops rest-col) (loop rest col)])
                       (values (append ops rest-ops) rest-col)))
                   (let-values ([(rest-ops rest-col) (loop rest collected)])
                     (values (cons input rest-ops) rest-col)))))))

     ;; 如果没有扁平化，跳过
     (if (<= (length all-operands) 2)
         (values (list insn) #f)

         ;; 优化操作数
         (let-values ([(vars const) (optimize-operands op all-operands)])
           (cond
             ;; 只有常量结果
             [(null? vars)
              (values (list (VfInsn 'const (list const) (list output) info id))
                      #t)]

             ;; 一个变量 + 常量
             [(and (= 1 (length vars)) const)
              (values (list (VfInsn op (list (car vars) const) (list output) info id))
                      #t)]

             ;; 一个变量，没有常量（identity element case）
             ;; This means we had (a op identity op identity...) -> a
             ;; We need to keep at least a binary operation or use copy
             [(and (= 1 (length vars)) (not const))
              ;; Keep the identity element to maintain binary operation
              (define identity (identity-element op))
              (values (list (VfInsn op (list (car vars) identity) (list output) info id))
                      #t)]

             ;; 多个变量
             [else
              ;; 重建表达式树（保持变量顺序）
              ;; 如果有常量，放在最后
              (define final-ops (if const (append vars (list const)) vars))

              ;; 只有当真正优化了才改变
              (if (equal? final-ops inputs)
                  (values (list insn) #f)
                  ;; 生成新指令
                  (values (list (VfInsn op
                                        (list (car final-ops) (cadr final-ops))
                                        (list output) info id))
                          #t))])))]

    [_ (values (list insn) #f)]))

;; ============================================================
;; Pass 主体
;; ============================================================

;; 对单个块执行重结合
(define (reassoc-block block def-map use-count)
  (define-values (new-insns changed?)
    (for/fold ([acc '()] [changed #f])
              ([insn (CfgBlock-insns block)])
      (define-values (result-insns insn-changed?)
        (try-reassoc-insn insn def-map use-count))
      (values (append acc result-insns) (or changed insn-changed?))))

  (values (struct-copy CfgBlock block [insns new-insns])
          changed?))

;; 重结合主入口
(define (cfg-reassoc cfg)
  ;; 构建分析信息
  (define use-count (count-uses cfg))
  (define def-map (build-def-map cfg))

  ;; 对所有块执行重结合
  (define-values (cfg^ any-changed?)
    (for/fold ([cfg cfg] [changed #f])
              ([bid (cfg-all-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (if (not block)
          (values cfg changed)
          (let-values ([(new-block block-changed?)
                        (reassoc-block block def-map use-count)])
            (values (if block-changed?
                       (cfg-set-block cfg new-block)
                       cfg)
                    (or changed block-changed?))))))

  cfg^)

(provide cfg-reassoc)

;; ============================================================
;; 迭代到不动点
;; ============================================================

(define (cfg-reassoc-fixpoint cfg [max-iter 10])
  (let loop ([cfg cfg] [i 0])
    (when (>= i max-iter)
      (error 'cfg-reassoc-fixpoint "Did not converge in ~a iterations" max-iter))
    (define use-count (count-uses cfg))
    (define def-map (build-def-map cfg))
    (define-values (cfg^ changed?)
      (for/fold ([cfg cfg] [changed #f])
                ([bid (cfg-all-block-ids cfg)])
        (define block (cfg-get-block cfg bid))
        (if (not block)
            (values cfg changed)
            (let-values ([(new-block block-changed?)
                          (reassoc-block block def-map use-count)])
              (values (if block-changed?
                         (cfg-set-block cfg new-block)
                         cfg)
                      (or changed block-changed?))))))
    (if changed?
        (loop cfg^ (+ i 1))
        cfg^)))

(provide cfg-reassoc-fixpoint)

;; ============================================================
;; 带统计版本
;; ============================================================

(define (cfg-reassoc-with-stats cfg)
  (define (count-insns c)
    (for/sum ([bid (cfg-all-block-ids c)])
      (define block (cfg-get-block c bid))
      (if block (length (CfgBlock-insns block)) 0)))

  (define before (count-insns cfg))
  (define cfg^ (cfg-reassoc cfg))
  (define after (count-insns cfg^))

  (values cfg^
          `((before . ,before)
            (after . ,after)
            (reassociated . ,(- before after)))))

(provide cfg-reassoc-with-stats)
