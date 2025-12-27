#lang racket/base

;; ============================================================
;; CFG Optimization: Tail Merging (Cross-Jumping)
;; ============================================================
;;
;; 合并具有相同尾部代码的基本块
;;
;; 情况 1: 相同指令、相同后继
;;   B1: insns...; goto C
;;   B2: insns...; goto C   (相同指令序列)
;; 合并后:
;;   所有到 B2 的边重定向到 B1
;;
;; 情况 2: 相同终止器，可以合并部分尾部
;;   (当前只实现情况 1)
;;
;; 参考：GCC tree-ssa-tail-merge.cc
;; ============================================================

(require racket/match racket/list racket/set racket/hash)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "loop-analysis.rkt")

;; ============================================================
;; Block Equivalence
;; ============================================================

;; 检查两个块是否可以合并
;; 条件：相同指令序列、相同 PHI 节点、相同终止器
(define (blocks-equivalent? cfg bid1 bid2)
  (and (not (equal? bid1 bid2))
       (let ([b1 (cfg-get-block cfg bid1)]
             [b2 (cfg-get-block cfg bid2)])
         (and b1 b2
              (insns-equivalent? (CfgBlock-insns b1) (CfgBlock-insns b2))
              (null? (CfgBlock-phis b1))  ; 简化：暂不处理有 PHI 的块
              (null? (CfgBlock-phis b2))
              (terminators-equivalent? (CfgBlock-terminator b1)
                                       (CfgBlock-terminator b2))))))

;; 检查指令序列是否等价
(define (insns-equivalent? insns1 insns2)
  (and (= (length insns1) (length insns2))
       (for/and ([i1 insns1] [i2 insns2])
         (insn-equivalent? i1 i2))))

;; 检查单条指令是否等价
(define (insn-equivalent? i1 i2)
  (match* (i1 i2)
    [((VfInsn op1 args1 _ ty1 attrs1)
      (VfInsn op2 args2 _ ty2 attrs2))
     ;; 操作符和参数必须相同，输出可以不同（会重命名）
     (and (equal? op1 op2)
          (equal? args1 args2)
          (equal? ty1 ty2))]
    [(_ _) #f]))

;; 检查终止器是否等价
(define (terminators-equivalent? t1 t2)
  (match* (t1 t2)
    [((TermJump target1) (TermJump target2))
     (equal? target1 target2)]
    [((TermBranch cond1 then1 else1) (TermBranch cond2 then2 else2))
     (and (equal? cond1 cond2)
          (equal? then1 then2)
          (equal? else1 else2))]
    [((TermReturn vals1) (TermReturn vals2))
     (equal? vals1 vals2)]
    [(_ _) #f]))

;; ============================================================
;; Successor Grouping
;; ============================================================

;; 按后继块分组
;; 返回 (Hash (Listof BlockId) -> (Listof BlockId))
;; 即：相同后继块的块列表
(define (group-by-successors cfg)
  (define groups (make-hash))
  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (define succs (get-successors (CfgBlock-terminator block)))
      (hash-update! groups succs (λ (lst) (cons bid lst)) '())))
  groups)

;; 获取终止器的后继块
(define (get-successors term)
  (match term
    [(TermJump target) (list target)]
    [(TermBranch _ then-bid else-bid)
     (sort (list then-bid else-bid) block-id<?)]
    [(TermReturn _) '()]
    [(TermSwitch _ cases default)
     (sort (cons default (map cdr cases)) block-id<?)]
    [_ '()]))

;; BlockId 比较
(define (block-id<? a b)
  (< (BlockId-id a) (BlockId-id b)))

;; ============================================================
;; Merge Algorithm
;; ============================================================

;; 在一组具有相同后继的块中找等价对
(define (find-equivalent-pair cfg bids preds entry-bid)
  ;; 过滤掉没有前驱的块（除了入口块），因为它们是死代码
  (define live-bids
    (filter (λ (bid)
              (or (equal? bid entry-bid)
                  (not (null? (hash-ref preds bid '())))))
            bids))
  (for*/first ([i (in-range (length live-bids))]
               [j (in-range (+ i 1) (length live-bids))]
               #:when (blocks-equivalent? cfg
                        (list-ref live-bids i)
                        (list-ref live-bids j)))
    (cons (list-ref live-bids i) (list-ref live-bids j))))

;; 合并两个等价块：保留 keep，删除 remove
;; 将所有到 remove 的边重定向到 keep
(define (merge-blocks cfg keep-bid remove-bid)
  (define preds (compute-predecessors cfg))
  (define pred-list (hash-ref preds remove-bid '()))

  ;; 更新每个前驱的终止器，将 remove-bid 替换为 keep-bid
  (for/fold ([cfg cfg])
            ([pred-bid pred-list])
    (define pred-block (cfg-get-block cfg pred-bid))
    (if pred-block
        (let* ([old-term (CfgBlock-terminator pred-block)]
               [new-term (redirect-terminator old-term remove-bid keep-bid)]
               [new-block (struct-copy CfgBlock pred-block [terminator new-term])])
          (cfg-set-block cfg new-block))
        cfg)))

;; 重定向终止器中的目标
(define (redirect-terminator term old-target new-target)
  (match term
    [(TermJump target)
     (if (equal? target old-target)
         (TermJump new-target)
         term)]
    [(TermBranch cond then-bid else-bid)
     (TermBranch cond
                 (if (equal? then-bid old-target) new-target then-bid)
                 (if (equal? else-bid old-target) new-target else-bid))]
    [(TermSwitch value cases default)
     (TermSwitch value
                 (for/list ([c cases])
                   (if (equal? (cdr c) old-target)
                       (cons (car c) new-target)
                       c))
                 (if (equal? default old-target) new-target default))]
    [_ term]))

;; ============================================================
;; Main Algorithm
;; ============================================================

;; 单次合并遍历
(define (tail-merge-pass cfg)
  (define groups (group-by-successors cfg))
  (define preds (compute-predecessors cfg))
  (define entry-bid (Cfg-entry cfg))

  (for/fold ([cfg cfg] [changed #f])
            ([(succs bids) (in-hash groups)]
             #:when (>= (length bids) 2))  ; 至少需要两个块才能合并
    (define pair (find-equivalent-pair cfg bids preds entry-bid))
    (if pair
        (let ([keep-bid (car pair)]
              [remove-bid (cdr pair)])
          (values (merge-blocks cfg keep-bid remove-bid) #t))
        (values cfg changed))))

;; 对整个 CFG 执行尾部合并
(define (cfg-tail-merge cfg)
  (let loop ([cfg cfg] [changed #t] [iterations 0])
    (cond
      [(not changed) cfg]
      [(>= iterations 20) cfg]  ; 防止无限循环
      [else
       (define-values (cfg^ any-changed?)
         (tail-merge-pass cfg))
       (loop cfg^ any-changed? (+ iterations 1))])))

(provide cfg-tail-merge)

;; ============================================================
;; With Statistics
;; ============================================================

(define (cfg-tail-merge-with-stats cfg)
  (define merge-count 0)

  (define (tail-merge-pass-with-count cfg)
    (define groups (group-by-successors cfg))
    (define preds (compute-predecessors cfg))
    (define entry-bid (Cfg-entry cfg))

    (for/fold ([cfg cfg] [changed #f])
              ([(succs bids) (in-hash groups)]
               #:when (>= (length bids) 2))
      (define pair (find-equivalent-pair cfg bids preds entry-bid))
      (if pair
          (let ([keep-bid (car pair)]
                [remove-bid (cdr pair)])
            (set! merge-count (+ merge-count 1))
            (values (merge-blocks cfg keep-bid remove-bid) #t))
          (values cfg changed))))

  (define cfg^
    (let loop ([cfg cfg] [changed #t] [iterations 0])
      (cond
        [(not changed) cfg]
        [(>= iterations 20) cfg]
        [else
         (define-values (cfg^ any-changed?)
           (tail-merge-pass-with-count cfg))
         (loop cfg^ any-changed? (+ iterations 1))])))

  (values cfg^
          `((merges . ,merge-count))))

(provide cfg-tail-merge-with-stats)
