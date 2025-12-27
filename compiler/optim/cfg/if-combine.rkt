#lang racket/base

;; ============================================================
;; CFG Optimization: If Combining
;; ============================================================
;;
;; 将相邻的 if 表达式合并为逻辑运算，减少分支数
;;
;; AND 模式:
;;   outer_bb: if (p) goto inner_bb else goto else_bb
;;   inner_bb: if (q) goto then_bb else goto else_bb
;; 转换为:
;;   outer_bb: if (p && q) goto then_bb else goto else_bb
;;
;; OR 模式:
;;   outer_bb: if (p) goto then_bb else goto inner_bb
;;   inner_bb: if (q) goto then_bb else goto else_bb
;; 转换为:
;;   outer_bb: if (p || q) goto then_bb else goto else_bb
;;
;; 参考：GCC tree-ssa-ifcombine.cc
;; ============================================================

(require racket/match racket/list racket/set)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "loop-analysis.rkt")

;; ============================================================
;; Pattern Recognition
;; ============================================================

;; 检查块是否无副作用且只有一个前驱
(define (candidate-inner-block? cfg bid preds)
  (define block (cfg-get-block cfg bid))
  (define pred-list (hash-ref preds bid '()))
  (and block
       (= (length pred-list) 1)  ; 只有一个前驱
       (TermBranch? (CfgBlock-terminator block))  ; 以条件分支结束
       (all-pure-insns? (CfgBlock-insns block))  ; 无副作用
       (null? (CfgBlock-phis block))))  ; 没有 PHI 节点

;; 检查所有指令是否纯
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

;; ============================================================
;; Combine Patterns
;; ============================================================

;; 尝试识别和合并 AND 模式
;; outer_bb: if (p) goto inner_bb else goto else_bb
;; inner_bb: if (q) goto then_bb else goto else_bb
;; => combined_bb: if (p && q) goto then_bb else goto else_bb
(define (try-combine-and cfg outer-bid inner-bid preds)
  (define outer-block (cfg-get-block cfg outer-bid))
  (define inner-block (cfg-get-block cfg inner-bid))

  (and outer-block inner-block
       (match* ((CfgBlock-terminator outer-block)
                (CfgBlock-terminator inner-block))
         [((TermBranch outer-cond outer-then outer-else)
           (TermBranch inner-cond inner-then inner-else))
          ;; 检查 AND 模式:
          ;; outer: if (p) goto inner else goto else
          ;; inner: if (q) goto then else goto else
          ;; 共同的 else 块
          (cond
            ;; 模式 1: outer 的 then 是 inner, 共享 else
            [(and (equal? outer-then inner-bid)
                  (equal? outer-else inner-else)
                  (VarId? outer-cond)
                  (VarId? inner-cond))
             ;; 创建 p && q
             (define result-var (fresh-var))
             (define and-insn
               (VfInsn 'and (list outer-cond inner-cond) (list result-var) #f #f))
             ;; 新终止器: if (p && q) goto then else goto else
             (define new-term (TermBranch result-var inner-then inner-else))
             ;; 合并指令
             (define combined-insns
               (append (CfgBlock-insns inner-block)
                       (list and-insn)))
             (define new-outer-block
               (struct-copy CfgBlock outer-block
                            [insns combined-insns]
                            [terminator new-term]))
             (values (cfg-set-block cfg new-outer-block) #t)]

            ;; 模式 2: outer 的 else 是 inner (p 取反), 共享 else
            ;; outer: if (p) goto else else goto inner
            ;; inner: if (q) goto then else goto else
            ;; 等价于: if (!p && q) goto then else goto else
            [(and (equal? outer-else inner-bid)
                  (equal? outer-then inner-else)
                  (VarId? outer-cond)
                  (VarId? inner-cond))
             ;; 创建 !p && q => 使用 !(p || !q)
             ;; 更简单: 直接生成 (not p) && q
             (define not-var (fresh-var))
             (define result-var (fresh-var))
             (define not-insn
               (VfInsn 'not (list outer-cond) (list not-var) #f #f))
             (define and-insn
               (VfInsn 'and (list not-var inner-cond) (list result-var) #f #f))
             (define new-term (TermBranch result-var inner-then inner-else))
             (define combined-insns
               (append (CfgBlock-insns inner-block)
                       (list not-insn and-insn)))
             (define new-outer-block
               (struct-copy CfgBlock outer-block
                            [insns combined-insns]
                            [terminator new-term]))
             (values (cfg-set-block cfg new-outer-block) #t)]

            [else (values cfg #f)])]
         [(_ _) (values cfg #f)])))

;; 尝试识别和合并 OR 模式
;; outer_bb: if (p) goto then_bb else goto inner_bb
;; inner_bb: if (q) goto then_bb else goto else_bb
;; => combined_bb: if (p || q) goto then_bb else goto else_bb
(define (try-combine-or cfg outer-bid inner-bid preds)
  (define outer-block (cfg-get-block cfg outer-bid))
  (define inner-block (cfg-get-block cfg inner-bid))

  (and outer-block inner-block
       (match* ((CfgBlock-terminator outer-block)
                (CfgBlock-terminator inner-block))
         [((TermBranch outer-cond outer-then outer-else)
           (TermBranch inner-cond inner-then inner-else))
          (cond
            ;; 模式 1: outer 的 else 是 inner, 共享 then
            ;; outer: if (p) goto then else goto inner
            ;; inner: if (q) goto then else goto else
            ;; => if (p || q) goto then else goto else
            [(and (equal? outer-else inner-bid)
                  (equal? outer-then inner-then)
                  (VarId? outer-cond)
                  (VarId? inner-cond))
             (define result-var (fresh-var))
             (define or-insn
               (VfInsn 'or (list outer-cond inner-cond) (list result-var) #f #f))
             (define new-term (TermBranch result-var inner-then inner-else))
             (define combined-insns
               (append (CfgBlock-insns inner-block)
                       (list or-insn)))
             (define new-outer-block
               (struct-copy CfgBlock outer-block
                            [insns combined-insns]
                            [terminator new-term]))
             (values (cfg-set-block cfg new-outer-block) #t)]

            ;; 模式 2: outer 的 then 是 inner (p 取反), 共享 then
            ;; outer: if (p) goto inner else goto then
            ;; inner: if (q) goto then else goto else
            ;; 等价于: if (!p || q) goto then else goto else
            [(and (equal? outer-then inner-bid)
                  (equal? outer-else inner-then)
                  (VarId? outer-cond)
                  (VarId? inner-cond))
             (define not-var (fresh-var))
             (define result-var (fresh-var))
             (define not-insn
               (VfInsn 'not (list outer-cond) (list not-var) #f #f))
             (define or-insn
               (VfInsn 'or (list not-var inner-cond) (list result-var) #f #f))
             (define new-term (TermBranch result-var inner-then inner-else))
             (define combined-insns
               (append (CfgBlock-insns inner-block)
                       (list not-insn or-insn)))
             (define new-outer-block
               (struct-copy CfgBlock outer-block
                            [insns combined-insns]
                            [terminator new-term]))
             (values (cfg-set-block cfg new-outer-block) #t)]

            [else (values cfg #f)])]
         [(_ _) (values cfg #f)])))

;; 生成新的变量 ID
(define fresh-var-counter 0)

(define (fresh-var)
  (set! fresh-var-counter (+ fresh-var-counter 1))
  (VarId (string->symbol (format "comb~a" fresh-var-counter))))

;; ============================================================
;; Main Algorithm
;; ============================================================

;; 对整个 CFG 执行 If 合并
(define (cfg-if-combine cfg)
  (define preds (compute-predecessors cfg))

  (let loop ([cfg cfg] [changed #t] [iterations 0])
    (cond
      [(not changed) cfg]
      [(>= iterations 10) cfg]  ; 防止无限循环
      [else
       (define-values (cfg^ any-changed?)
         (if-combine-pass cfg preds))
       (loop cfg^ any-changed? (+ iterations 1))])))

;; 单次合并遍历
(define (if-combine-pass cfg preds)
  (for/fold ([cfg cfg] [changed #f])
            ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (cond
      [(not block) (values cfg changed)]
      [(not (TermBranch? (CfgBlock-terminator block)))
       (values cfg changed)]
      [else
       (match (CfgBlock-terminator block)
         [(TermBranch cond then-bid else-bid)
          ;; 尝试合并 then 分支的块 (AND 或 OR 模式)
          (define-values (cfg1 changed1?)
            (if (candidate-inner-block? cfg then-bid preds)
                (let-values ([(c1 ch1) (try-combine-and cfg bid then-bid preds)])
                  (if ch1
                      (values c1 ch1)
                      (try-combine-or cfg bid then-bid preds)))
                (values cfg #f)))
          ;; 尝试合并 else 分支的块 (AND 或 OR 模式)
          (define-values (cfg2 changed2?)
            (if (and (not changed1?)
                     (candidate-inner-block? cfg1 else-bid preds))
                (let-values ([(c2 ch2) (try-combine-and cfg1 bid else-bid preds)])
                  (if ch2
                      (values c2 ch2)
                      (try-combine-or cfg1 bid else-bid preds)))
                (values cfg1 #f)))
          (values cfg2 (or changed changed1? changed2?))]
         [_ (values cfg changed)])])))

(provide cfg-if-combine)

;; ============================================================
;; With Statistics
;; ============================================================

(define (cfg-if-combine-with-stats cfg)
  (define preds (compute-predecessors cfg))
  (define combine-count 0)

  (define (if-combine-pass-with-count cfg)
    (for/fold ([cfg cfg] [changed #f])
              ([bid (cfg-all-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (cond
        [(not block) (values cfg changed)]
        [(not (TermBranch? (CfgBlock-terminator block)))
         (values cfg changed)]
        [else
         (match (CfgBlock-terminator block)
           [(TermBranch cond then-bid else-bid)
            (define-values (cfg1 changed1?)
              (if (candidate-inner-block? cfg then-bid preds)
                  (let-values ([(c1 ch1) (try-combine-and cfg bid then-bid preds)])
                    (if ch1
                        (begin (set! combine-count (+ combine-count 1))
                               (values c1 ch1))
                        (let-values ([(c2 ch2) (try-combine-or cfg bid then-bid preds)])
                          (when ch2 (set! combine-count (+ combine-count 1)))
                          (values c2 ch2))))
                  (values cfg #f)))
            (define-values (cfg2 changed2?)
              (if (and (not changed1?)
                       (candidate-inner-block? cfg1 else-bid preds))
                  (let-values ([(c1 ch1) (try-combine-and cfg1 bid else-bid preds)])
                    (if ch1
                        (begin (set! combine-count (+ combine-count 1))
                               (values c1 ch1))
                        (let-values ([(c2 ch2) (try-combine-or cfg1 bid else-bid preds)])
                          (when ch2 (set! combine-count (+ combine-count 1)))
                          (values c2 ch2))))
                  (values cfg1 #f)))
            (values cfg2 (or changed changed1? changed2?))]
           [_ (values cfg changed)])])))

  (define cfg^
    (let loop ([cfg cfg] [changed #t] [iterations 0])
      (cond
        [(not changed) cfg]
        [(>= iterations 10) cfg]
        [else
         (define-values (cfg^ any-changed?)
           (if-combine-pass-with-count cfg))
         (loop cfg^ any-changed? (+ iterations 1))])))

  (values cfg^
          `((combines . ,combine-count))))

(provide cfg-if-combine-with-stats)
