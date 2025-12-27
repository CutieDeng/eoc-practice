#lang racket/base

;; ============================================================
;; CFG Optimization: Jump Threading
;; ============================================================
;;
;; 跳转线程化优化
;;
;; 当一个条件分支的目标块再次检查相同的条件时，
;; 可以直接跳转到最终目标，消除冗余的条件检查。
;;
;; 例如:
;;   A: if (x) goto B else goto C
;;   B: if (x) goto D else goto E
;;
;; 由于从 A 到 B 时 x 必为真，B 的分支必定去 D，所以可优化为:
;;   A: if (x) goto D else goto C
;;   B: if (x) goto D else goto E  ; 可能变成死代码
;;
;; 参考：GCC tree-ssa-threadedge.cc
;; ============================================================

(require racket/match racket/list racket/dict racket/set)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "loop-analysis.rkt")

;; ============================================================
;; 条件值追踪
;; ============================================================

;; 已知条件值: (VarId -> boolean)
;; 表示在当前路径上某变量的已知真值
(struct KnownConds (map) #:prefab)

(define (known-conds-empty)
  (KnownConds (hash)))

(define (known-conds-add kc var val)
  (KnownConds (hash-set (KnownConds-map kc) var val)))

(define (known-conds-get kc var)
  (hash-ref (KnownConds-map kc) var #f))

;; ============================================================
;; 跳转线程分析
;; ============================================================

;; 分析块的终止条件，返回 (values cond-var then-bid else-bid) 或 #f
(define (analyze-branch-terminator cfg bid)
  (define block (cfg-get-block cfg bid))
  (and block
       (match (CfgBlock-terminator block)
         [(TermBranch cond then-bid else-bid)
          (and (VarId? cond)
               (values cond then-bid else-bid))]
         [_ #f])))

;; 检查块是否为"简单"块（可以安全线程化）
;; 简单块：只有一个前驱，没有副作用，以条件分支结束
(define (simple-threading-candidate? cfg bid preds)
  (define pred-list (hash-ref preds bid '()))
  (define block (cfg-get-block cfg bid))

  (and block
       ;; 有前驱
       (not (null? pred-list))
       ;; 以条件分支结束
       (TermBranch? (CfgBlock-terminator block))
       ;; 指令没有副作用（保守：检查是否为空或只有纯计算）
       (all-pure-insns? (CfgBlock-insns block))
       ;; 没有 PHI 节点或 PHI 可以被简化
       (or (null? (CfgBlock-phis block))
           (single-predecessor-phis? block pred-list))))

;; 检查所有指令是否纯（无副作用）
(define (all-pure-insns? insns)
  (for/and ([insn insns])
    (pure-insn? insn)))

;; 检查指令是否纯
(define (pure-insn? insn)
  (and (VfInsn? insn)
       (memq (VfInsn-op insn)
             '(const add sub mul div udiv rem urem
               and or xor not neg
               shl shr ushr
               lt le gt ge eq ne
               icmp_slt icmp_sle icmp_sgt icmp_sge
               icmp_ult icmp_ule icmp_ugt icmp_uge))))

;; 检查 PHI 是否只有单个有效前驱
(define (single-predecessor-phis? block pred-list)
  (= (length pred-list) 1))

;; ============================================================
;; 跳转线程变换
;; ============================================================

;; 尝试对单个边进行线程化
;; 从 src-bid 到 dst-bid 的边，如果 dst-bid 再次检查相同条件
;; known-val 是条件在到达 dst 时的已知值
(define (try-thread-edge cfg src-bid dst-bid cond-var known-val preds)
  (define dst-block (cfg-get-block cfg dst-bid))

  (cond
    [(not dst-block) (values cfg #f)]

    [else
     (match (CfgBlock-terminator dst-block)
       [(TermBranch dst-cond then-bid else-bid)
        (cond
          ;; 如果 dst 的条件与已知条件相同
          [(equal? dst-cond cond-var)
           ;; 根据已知值确定最终目标
           (define final-target (if known-val then-bid else-bid))
           ;; 更新 src 的 terminator
           (define src-block (cfg-get-block cfg src-bid))
           (if src-block
               (let ()
                 (define new-term
                   (update-terminator-target
                    (CfgBlock-terminator src-block)
                    dst-bid final-target))
                 (define new-src-block
                   (struct-copy CfgBlock src-block [terminator new-term]))
                 (values (cfg-set-block cfg new-src-block) #t))
               (values cfg #f))]

          ;; 检查是否是等价条件（通过定义追踪）
          [(and (VarId? dst-cond)
                (equivalent-condition? cfg cond-var dst-cond known-val))
           (define final-target (if known-val then-bid else-bid))
           (define src-block (cfg-get-block cfg src-bid))
           (if src-block
               (let ()
                 (define new-term
                   (update-terminator-target
                    (CfgBlock-terminator src-block)
                    dst-bid final-target))
                 (define new-src-block
                   (struct-copy CfgBlock src-block [terminator new-term]))
                 (values (cfg-set-block cfg new-src-block) #t))
               (values cfg #f))]

          [else (values cfg #f)])]

       ;; dst 是无条件跳转，可以直接传递
       [(TermJump target)
        ;; 如果 dst 只是跳转，继续追踪
        (values cfg #f)]  ; 暂不处理链式跳转

       [_ (values cfg #f)])]))

;; 更新 terminator 中的目标
(define (update-terminator-target term old-target new-target)
  (match term
    [(TermBranch cond then-bid else-bid)
     (TermBranch cond
                 (if (equal? then-bid old-target) new-target then-bid)
                 (if (equal? else-bid old-target) new-target else-bid))]
    [(TermJump target)
     (if (equal? target old-target)
         (TermJump new-target)
         term)]
    [_ term]))

;; 检查两个条件变量是否等价
;; 例如：y = x, 则 y 和 x 在条件中等价
(define (equivalent-condition? cfg cond1 cond2 known-val)
  ;; 简化实现：检查 cond2 是否直接复制自 cond1
  (for*/or ([bid (cfg-all-block-ids cfg)]
            [block (in-value (cfg-get-block cfg bid))]
            #:when block
            [insn (CfgBlock-insns block)]
            #:when (VfInsn? insn))
    (match insn
      ;; cond2 = cond1 (复制)
      [(VfInsn 'add (list v 0) (list out) _ _)
       (and (equal? out cond2) (equal? v cond1))]
      ;; cond2 = not cond1 (取反) - 这种情况需要反转 known-val
      ;; 暂不处理
      [_ #f])))

;; ============================================================
;; 主算法
;; ============================================================

;; 对整个 CFG 执行跳转线程化
(define (cfg-jump-thread cfg)
  (define preds (compute-predecessors cfg))

  (let loop ([cfg cfg] [changed #t] [iterations 0])
    (cond
      [(not changed) cfg]
      [(>= iterations 10) cfg]  ; 防止无限循环
      [else
       (define-values (cfg^ any-changed?)
         (thread-pass cfg preds))
       (loop cfg^ any-changed? (+ iterations 1))])))

;; 单次线程化遍历
(define (thread-pass cfg preds)
  (for/fold ([cfg cfg] [changed #f])
            ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if (not block)
        (values cfg changed)
        (match (CfgBlock-terminator block)
          [(TermBranch cond then-bid else-bid)
           #:when (VarId? cond)
           ;; 尝试线程化 then 分支（已知 cond = true）
           (define-values (cfg1 changed1?)
             (try-thread-edge cfg bid then-bid cond #t preds))
           ;; 尝试线程化 else 分支（已知 cond = false）
           (define-values (cfg2 changed2?)
             (try-thread-edge cfg1 bid else-bid cond #f preds))
           (values cfg2 (or changed changed1? changed2?))]
          [_ (values cfg changed)]))))

(provide cfg-jump-thread)

;; ============================================================
;; 带统计版本
;; ============================================================

(define (cfg-jump-thread-with-stats cfg)
  (define preds (compute-predecessors cfg))
  (define thread-count 0)

  (define (thread-pass-with-count cfg)
    (for/fold ([cfg cfg] [changed #f])
              ([bid (cfg-all-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (if (not block)
          (values cfg changed)
          (match (CfgBlock-terminator block)
            [(TermBranch cond then-bid else-bid)
             #:when (VarId? cond)
             (define-values (cfg1 changed1?)
               (try-thread-edge cfg bid then-bid cond #t preds))
             (when changed1? (set! thread-count (+ thread-count 1)))
             (define-values (cfg2 changed2?)
               (try-thread-edge cfg1 bid else-bid cond #f preds))
             (when changed2? (set! thread-count (+ thread-count 1)))
             (values cfg2 (or changed changed1? changed2?))]
            [_ (values cfg changed)]))))

  (define cfg^
    (let loop ([cfg cfg] [changed #t] [iterations 0])
      (cond
        [(not changed) cfg]
        [(>= iterations 10) cfg]
        [else
         (define-values (cfg^ any-changed?)
           (thread-pass-with-count cfg))
         (loop cfg^ any-changed? (+ iterations 1))])))

  (values cfg^
          `((threads . ,thread-count))))

(provide cfg-jump-thread-with-stats)

;; ============================================================
;; 高级：向后跳转线程化
;; ============================================================

;; 向后线程化：当一个 PHI 的值在某些路径上是常量时，
;; 可以为这些路径创建专门的副本
;;
;; 例如:
;;   A: x = 1; goto C
;;   B: x = 0; goto C
;;   C: y = phi(x from A, x from B)
;;      if (y) goto D else goto E
;;
;; 可以优化为:
;;   A: x = 1; goto D  (直接去 D)
;;   B: x = 0; goto E  (直接去 E)
;;
;; 这需要更复杂的分析，暂不实现

;; ============================================================
;; 导出
;; ============================================================

(provide try-thread-edge
         analyze-branch-terminator)
