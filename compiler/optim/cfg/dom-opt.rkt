#lang racket/base

;; ============================================================
;; CFG Optimization: Dominator-based Optimization
;; ============================================================
;;
;; 基于支配树的优化
;;
;; 1. 边等价记录：
;;    当取分支 if (x) goto A，在 A 中 x 已知为真
;;    当取分支 if (x) goto B (else)，在 B 中 x 已知为假
;;
;; 2. 利用已知值简化：
;;    - 替换条件表达式中的已知值
;;    - 简化嵌套条件
;;
;; 参考：GCC tree-ssa-dom.cc
;; ============================================================

(require racket/match racket/list racket/set racket/hash)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "loop-analysis.rkt")

;; ============================================================
;; Dominator Tree Computation
;; ============================================================

;; 计算支配树（简化版本：使用 Cooper 等人的算法）
;; 返回 Hash[BlockId -> BlockId] 表示 idom（直接支配者）
(define (compute-dominators cfg)
  (define entry (Cfg-entry cfg))
  (define all-bids (cfg-all-block-ids cfg))
  (define succs (compute-successors cfg))
  (define preds (compute-predecessors cfg))

  ;; 初始化 doms
  (define doms (make-hash))
  (for ([bid all-bids])
    (hash-set! doms bid #f))
  (hash-set! doms entry entry)

  ;; 计算 RPO（逆后序）
  (define rpo (compute-rpo cfg entry succs))
  (define rpo-num (make-hash))
  (for ([bid rpo] [i (in-naturals)])
    (hash-set! rpo-num bid i))

  ;; 迭代计算支配者
  (let loop ([changed #t])
    (when changed
      (define any-changed #f)
      (for ([bid rpo]
            #:when (not (equal? bid entry)))
        (define pred-list (hash-ref preds bid '()))
        (define new-idom
          (for/fold ([idom #f])
                    ([p pred-list]
                     #:when (hash-ref doms p #f))  ; p 已被处理
            (if (not idom)
                p
                (intersect-doms doms rpo-num idom p))))
        (when (and new-idom (not (equal? new-idom (hash-ref doms bid #f))))
          (hash-set! doms bid new-idom)
          (set! any-changed #t)))
      (loop any-changed)))

  doms)

;; 计算后继
(define (compute-successors cfg)
  (define succs (make-hash))
  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (hash-set! succs bid (get-terminator-succs (CfgBlock-terminator block)))))
  succs)

;; 获取终止器的后继
(define (get-terminator-succs term)
  (match term
    [(TermJump target) (list target)]
    [(TermBranch _ then-bid else-bid) (list then-bid else-bid)]
    [(TermSwitch _ cases default) (cons default (map cdr cases))]
    [_ '()]))

;; 计算 RPO（逆后序）
(define (compute-rpo cfg entry succs)
  (define visited (mutable-set))
  (define result '())

  (define (dfs bid)
    (unless (set-member? visited bid)
      (set-add! visited bid)
      (for ([succ (hash-ref succs bid '())])
        (dfs succ))
      (set! result (cons bid result))))

  (dfs entry)
  result)

;; 计算两个节点的最近公共支配者
(define (intersect-doms doms rpo-num b1 b2)
  (define (rpo b) (hash-ref rpo-num b +inf.0))
  (let loop ([f1 b1] [f2 b2])
    (cond
      [(equal? f1 f2) f1]
      [(< (rpo f1) (rpo f2))
       (loop f1 (hash-ref doms f2 f2))]
      [else
       (loop (hash-ref doms f1 f1) f2)])))

;; ============================================================
;; Edge Equivalences
;; ============================================================

;; 边等价：在某条边上，某变量的已知值
;; (Hash (cons BlockId BlockId) -> (Hash VarId -> Boolean))
(define (compute-edge-equivalences cfg)
  (define equivs (make-hash))

  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (match (CfgBlock-terminator block)
        [(TermBranch cond then-bid else-bid)
         (when (VarId? cond)
           ;; 在 then 边上，cond = true
           (hash-set! equivs (cons bid then-bid)
                      (hash cond #t))
           ;; 在 else 边上，cond = false
           (hash-set! equivs (cons bid else-bid)
                      (hash cond #f)))]
        [_ (void)])))

  equivs)

;; ============================================================
;; Value Propagation
;; ============================================================

;; 计算在每个块中已知为真/假的变量
;; 返回 Hash[BlockId -> Hash[VarId -> Boolean]]
(define (compute-known-values cfg doms equivs)
  (define entry (Cfg-entry cfg))
  (define known (make-hash))
  (hash-set! known entry (hash))

  ;; 按支配顺序遍历
  (define rpo (compute-rpo cfg entry (compute-successors cfg)))

  (for ([bid rpo])
    (unless (hash-has-key? known bid)
      (hash-set! known bid (hash)))
    (define idom (hash-ref doms bid #f))
    (when (and idom (not (equal? idom bid)))
      ;; 继承支配者的已知值
      (define parent-known (hash-ref known idom (hash)))
      ;; 加上从 idom 到 bid 的边等价（如果存在）
      ;; 边等价优先（更局部的信息）
      (define edge-equiv (hash-ref equivs (cons idom bid) (hash)))
      ;; 使用自定义合并，边等价优先级更高
      (define merged
        (for/fold ([h parent-known])
                  ([(k v) (in-hash edge-equiv)])
          (hash-set h k v)))
      (hash-set! known bid merged)))

  known)

;; ============================================================
;; Apply Optimizations
;; ============================================================

;; 使用已知值简化块
(define (simplify-block cfg bid known-vals)
  (define block (cfg-get-block cfg bid))
  (if (not block)
      cfg
      (match (CfgBlock-terminator block)
        [(TermBranch cond then-bid else-bid)
         (if (and (VarId? cond) (hash-has-key? known-vals cond))
             (let* ([val (hash-ref known-vals cond)]
                    [target (if val then-bid else-bid)]
                    [new-block (struct-copy CfgBlock block
                                            [terminator (TermJump target)])])
               (cfg-set-block cfg new-block))
             cfg)]
        [_ cfg])))

;; ============================================================
;; Main Algorithm
;; ============================================================

;; 对整个 CFG 执行支配树优化
(define (cfg-dom-opt cfg)
  (define doms (compute-dominators cfg))
  (define equivs (compute-edge-equivalences cfg))
  (define known (compute-known-values cfg doms equivs))

  ;; 应用简化
  (for/fold ([cfg cfg])
            ([bid (cfg-all-block-ids cfg)])
    (define block-known (hash-ref known bid (hash)))
    (simplify-block cfg bid block-known)))

(provide cfg-dom-opt)

;; ============================================================
;; With Statistics
;; ============================================================

(define (cfg-dom-opt-with-stats cfg)
  (define doms (compute-dominators cfg))
  (define equivs (compute-edge-equivalences cfg))
  (define known (compute-known-values cfg doms equivs))
  (define simplify-count 0)

  (define (simplify-block-with-count cfg bid known-vals)
    (define block (cfg-get-block cfg bid))
    (if (not block)
        cfg
        (match (CfgBlock-terminator block)
          [(TermBranch cond then-bid else-bid)
           (if (and (VarId? cond) (hash-has-key? known-vals cond))
               (let* ([val (hash-ref known-vals cond)]
                      [target (if val then-bid else-bid)]
                      [new-block (struct-copy CfgBlock block
                                              [terminator (TermJump target)])])
                 (set! simplify-count (+ simplify-count 1))
                 (cfg-set-block cfg new-block))
               cfg)]
          [_ cfg])))

  (define cfg^
    (for/fold ([cfg cfg])
              ([bid (cfg-all-block-ids cfg)])
      (define block-known (hash-ref known bid (hash)))
      (simplify-block-with-count cfg bid block-known)))

  (values cfg^
          `((simplifications . ,simplify-count))))

(provide cfg-dom-opt-with-stats)
