#lang racket/base

;; ============================================================
;; CFG Optimization: Partial Redundancy Elimination (PRE)
;; ============================================================
;;
;; 部分冗余消除：通过代码移动消除冗余计算
;;
;; PRE 处理的情况：
;; 1. 完全冗余 (Full Redundancy): 表达式在所有路径上都已计算
;; 2. 部分冗余 (Partial Redundancy): 表达式在某些路径上已计算
;;
;; 算法基于 Lazy Code Motion (LCM):
;; 1. 计算 ANTIC (Anticipated): 从此点出发，所有路径都会计算的表达式
;; 2. 计算 AVAIL (Available): 到达此点，所有路径都已计算的表达式
;; 3. 确定插入点和删除点
;;
;; 参考：
;; - GCC tree-ssa-pre.cc
;; - Knoop, Rüthing, Steffen: "Lazy Code Motion"
;; ============================================================

(require racket/match racket/list racket/set racket/hash)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "loop-analysis.rkt")

;; ============================================================
;; CFG Successors Computation
;; ============================================================

;; 计算每个块的后继
(define (compute-successors cfg)
  (define succs (make-hash))

  (for ([bid (cfg-all-block-ids cfg)])
    (hash-set! succs bid '()))

  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (define term (CfgBlock-terminator block))
      (define targets (terminator-targets term))
      (hash-set! succs bid targets)))

  succs)

;; 获取终止器的目标块
(define (terminator-targets term)
  (match term
    [(TermJump target) (list target)]
    [(TermBranch _ then-target else-target) (list then-target else-target)]
    [(TermSwitch _ cases default) (cons default (map cdr cases))]
    [(TermReturn _) '()]
    [(TermThrow _) '()]
    [(TermUnreachable) '()]
    [_ '()]))

;; ============================================================
;; Expression Representation
;; ============================================================

;; 表达式的规范化表示
;; 用于识别等价的计算
(struct Expr (
  op        ; Symbol - 操作符
  operands  ; (Listof Any) - 操作数（规范化后）
) #:prefab)

;; 从指令创建表达式
(define (insn->expr insn)
  (match insn
    [(VfInsn op inputs outputs _ _)
     #:when (pure-op? op)
     (Expr op (normalize-operands op inputs))]
    [_ #f]))

;; 检查是否为纯操作（无副作用，确定性）
(define (pure-op? op)
  (and (memq op '(add sub mul div udiv rem urem
                  and or xor not neg
                  shl shr ushr
                  lt le gt ge eq ne
                  icmp_slt icmp_sle icmp_sgt icmp_sge
                  icmp_ult icmp_ule icmp_ugt icmp_uge))
       #t))

;; 规范化操作数（交换律排序等）
(define (normalize-operands op inputs)
  (if (commutative-op? op)
      (sort inputs operand<?)
      inputs))

;; 检查是否为交换操作
(define (commutative-op? op)
  (and (memq op '(add mul and or xor eq ne)) #t))

;; 操作数比较（用于排序）
;; 支持 VarId-id 为整数或符号
(define (operand<? a b)
  (cond
    [(and (VarId? a) (VarId? b))
     (let ([id-a (VarId-id a)]
           [id-b (VarId-id b)])
       (cond
         [(and (number? id-a) (number? id-b)) (< id-a id-b)]
         [(and (symbol? id-a) (symbol? id-b)) (symbol<? id-a id-b)]
         [(number? id-a) #t]  ; 数字在符号前
         [else #f]))]
    [(and (number? a) (number? b))
     (< a b)]
    [(VarId? a) #t]  ; VarId 在数字前
    [(VarId? b) #f]
    [else #f]))

(provide (struct-out Expr) insn->expr pure-op?)

;; ============================================================
;; Expression Sets per Block
;; ============================================================

;; 计算每个块中定义的表达式
(define (compute-expr-gen cfg)
  (define gen-map (make-hash))

  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (define exprs (mutable-set))
      (for ([insn (CfgBlock-insns block)])
        (define expr (insn->expr insn))
        (when expr
          (set-add! exprs expr)))
      (hash-set! gen-map bid (set-copy exprs))))

  gen-map)

;; 计算每个块中杀死的表达式
;; 如果一个变量被重新定义，使用该变量的表达式被杀死
(define (compute-expr-kill cfg)
  (define kill-map (make-hash))

  ;; 首先收集所有表达式
  (define all-exprs (mutable-set))
  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (for ([insn (CfgBlock-insns block)])
        (define expr (insn->expr insn))
        (when expr
          (set-add! all-exprs expr)))))

  ;; 对每个块，找出被重定义的变量
  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (define killed (mutable-set))
      (define defined-vars (mutable-set))

      ;; 收集所有定义的变量
      (for ([insn (CfgBlock-insns block)])
        (when (VfInsn? insn)
          (for ([out (VfInsn-outputs insn)])
            (set-add! defined-vars out))))

      ;; PHI 节点也定义变量
      (for ([phi (CfgBlock-phis block)])
        (when (PhiInsn? phi)
          (set-add! defined-vars (PhiInsn-output phi))))

      ;; 找出使用这些变量的表达式
      (for ([expr (in-set all-exprs)])
        (when (for/or ([operand (Expr-operands expr)])
                (and (VarId? operand)
                     (set-member? defined-vars operand)))
          (set-add! killed expr)))

      (hash-set! kill-map bid (set-copy killed))))

  kill-map)

;; ============================================================
;; Availability Analysis (Forward)
;; ============================================================

;; 可用性分析：表达式在到达某点时是否已计算
;; AVAIL_in[B] = ∩ AVAIL_out[P] for all predecessors P
;; AVAIL_out[B] = (AVAIL_in[B] - KILL[B]) ∪ GEN[B]

(define (compute-availability cfg gen-map kill-map)
  (define preds (compute-predecessors cfg))
  (define succs (compute-successors cfg))
  (define entry (Cfg-entry cfg))
  (define all-bids (cfg-all-block-ids cfg))

  ;; 初始化
  (define avail-in (make-hash))
  (define avail-out (make-hash))

  ;; 收集所有表达式作为全集
  (define all-exprs
    (for/fold ([s (set)])
              ([bid all-bids])
      (set-union s (hash-ref gen-map bid (set)))))

  ;; 入口块的 AVAIL_in 为空
  (for ([bid all-bids])
    (hash-set! avail-in bid (if (equal? bid entry) (set) all-exprs))
    (hash-set! avail-out bid (set)))

  ;; 迭代直到不动点
  (let loop ([changed #t])
    (when changed
      (define any-changed #f)
      (for ([bid all-bids])
        (define pred-list (hash-ref preds bid '()))

        ;; AVAIL_in = ∩ AVAIL_out[P]
        (define new-in
          (if (null? pred-list)
              (set)  ; 入口块
              (for/fold ([s all-exprs])
                        ([p pred-list])
                (set-intersect s (hash-ref avail-out p (set))))))

        (when (not (equal? new-in (hash-ref avail-in bid)))
          (set! any-changed #t)
          (hash-set! avail-in bid new-in))

        ;; AVAIL_out = (AVAIL_in - KILL) ∪ GEN
        (define gen (hash-ref gen-map bid (set)))
        (define kill (hash-ref kill-map bid (set)))
        (define new-out
          (set-union (set-subtract (hash-ref avail-in bid) kill) gen))

        (when (not (equal? new-out (hash-ref avail-out bid)))
          (set! any-changed #t)
          (hash-set! avail-out bid new-out)))

      (loop any-changed)))

  (values avail-in avail-out))

;; ============================================================
;; Anticipability Analysis (Backward)
;; ============================================================

;; 可预期性分析：从某点出发，表达式是否会被计算
;; ANTIC_out[B] = ∩ ANTIC_in[S] for all successors S
;; ANTIC_in[B] = (ANTIC_out[B] - KILL[B]) ∪ GEN[B]

(define (compute-anticipability cfg gen-map kill-map)
  (define preds (compute-predecessors cfg))
  (define succs (compute-successors cfg))
  (define all-bids (cfg-all-block-ids cfg))

  ;; 初始化
  (define antic-in (make-hash))
  (define antic-out (make-hash))

  ;; 收集所有表达式
  (define all-exprs
    (for/fold ([s (set)])
              ([bid all-bids])
      (set-union s (hash-ref gen-map bid (set)))))

  ;; 出口块的 ANTIC_out 为空
  (for ([bid all-bids])
    (define succ-list (hash-ref succs bid '()))
    (hash-set! antic-out bid (if (null? succ-list) (set) all-exprs))
    (hash-set! antic-in bid (set)))

  ;; 迭代直到不动点
  (let loop ([changed #t])
    (when changed
      (define any-changed #f)
      (for ([bid all-bids])
        (define succ-list (hash-ref succs bid '()))

        ;; ANTIC_out = ∩ ANTIC_in[S]
        (define new-out
          (if (null? succ-list)
              (set)  ; 出口块
              (for/fold ([s all-exprs])
                        ([s-bid succ-list])
                (set-intersect s (hash-ref antic-in s-bid (set))))))

        (when (not (equal? new-out (hash-ref antic-out bid)))
          (set! any-changed #t)
          (hash-set! antic-out bid new-out))

        ;; ANTIC_in = (ANTIC_out - KILL) ∪ GEN
        (define gen (hash-ref gen-map bid (set)))
        (define kill (hash-ref kill-map bid (set)))
        (define new-in
          (set-union (set-subtract (hash-ref antic-out bid) kill) gen))

        (when (not (equal? new-in (hash-ref antic-in bid)))
          (set! any-changed #t)
          (hash-set! antic-in bid new-in)))

      (loop any-changed)))

  (values antic-in antic-out))

;; ============================================================
;; Find Redundant Expressions
;; ============================================================

;; 在块中找到冗余的表达式
;; 如果表达式在 AVAIL_in 中，则它是冗余的
(define (find-redundant-in-block cfg bid avail-in)
  (define block (cfg-get-block cfg bid))
  (define available (hash-ref avail-in bid (set)))
  (define redundant '())

  (when block
    ;; 追踪块内的局部可用性
    (define local-avail (set->mutable-set available))

    (for ([insn (CfgBlock-insns block)])
      (define expr (insn->expr insn))
      (when expr
        (if (set-member? local-avail expr)
            ;; 冗余表达式
            (set! redundant (cons insn redundant))
            ;; 添加到局部可用
            (set-add! local-avail expr)))))

  (reverse redundant))

;; ============================================================
;; Expression Value Numbering
;; ============================================================

;; 为表达式分配唯一的值编号
;; 返回 expr -> VarId 的映射
(define (number-expressions cfg avail-out)
  (define expr->var (make-hash))
  (define counter 0)

  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (for ([insn (CfgBlock-insns block)])
        (define expr (insn->expr insn))
        (when (and expr (not (hash-has-key? expr->var expr)))
          (set! counter (+ counter 1))
          (define var (VarId (string->symbol (format "pre~a" counter))))
          (hash-set! expr->var expr var)))))

  expr->var)

;; ============================================================
;; Main PRE Pass
;; ============================================================

;; 对整个 CFG 执行 PRE
(define (cfg-pre cfg)
  ;; 步骤 1: 计算 GEN 和 KILL
  (define gen-map (compute-expr-gen cfg))
  (define kill-map (compute-expr-kill cfg))

  ;; 步骤 2: 可用性分析
  (define-values (avail-in avail-out)
    (compute-availability cfg gen-map kill-map))

  ;; 步骤 3: 可预期性分析
  (define-values (antic-in antic-out)
    (compute-anticipability cfg gen-map kill-map))

  ;; 步骤 4: 找到并消除冗余表达式
  (define eliminated-count 0)

  (define cfg^
    (for/fold ([cfg cfg])
              ([bid (cfg-all-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (cond
        [(not block) cfg]
        [else
         ;; 找到冗余表达式
         (define redundant-insns
           (find-redundant-in-block cfg bid avail-in))

         (if (null? redundant-insns)
             cfg
             ;; 移除冗余指令，用已有值替换
             (let ()
               (set! eliminated-count
                     (+ eliminated-count (length redundant-insns)))
               (eliminate-redundant cfg bid redundant-insns avail-in)))])))

  cfg^)

(provide cfg-pre)

;; ============================================================
;; Eliminate Redundant Expressions
;; ============================================================

;; 在块中消除冗余表达式
;; 需要找到提供相同值的之前计算
(define (eliminate-redundant cfg bid redundant-insns avail-in)
  (define block (cfg-get-block cfg bid))
  (define redundant-set (list->set redundant-insns))

  ;; 构建表达式到变量的映射（从前驱块）
  (define expr->var (build-available-values cfg bid avail-in))

  ;; 处理指令
  (define new-insns '())
  (for ([insn (CfgBlock-insns block)])
    (cond
      [(set-member? redundant-set insn)
       ;; 冗余指令 - 替换为复制
       (define expr (insn->expr insn))
       (define available-var (hash-ref expr->var expr #f))
       (when (and available-var (pair? (VfInsn-outputs insn)))
         (define output (car (VfInsn-outputs insn)))
         ;; 添加复制指令
         (set! new-insns
               (cons (VfInsn 'copy (list available-var) (list output) #f #f)
                     new-insns)))]
      [else
       ;; 保留指令，但更新 expr->var
       (define expr (insn->expr insn))
       (when (and expr (pair? (VfInsn-outputs insn)))
         (hash-set! expr->var expr (car (VfInsn-outputs insn))))
       (set! new-insns (cons insn new-insns))]))

  (cfg-set-block cfg
                 (struct-copy CfgBlock block
                              [insns (reverse new-insns)])))

;; 构建可用表达式到变量的映射
(define (build-available-values cfg bid avail-in)
  (define preds (compute-predecessors cfg))
  (define pred-list (hash-ref preds bid '()))
  (define expr->var (make-hash))

  ;; 从前驱块收集表达式定义
  (for ([pred-bid pred-list])
    (define pred-block (cfg-get-block cfg pred-bid))
    (when pred-block
      (for ([insn (CfgBlock-insns pred-block)])
        (define expr (insn->expr insn))
        (when (and expr (pair? (VfInsn-outputs insn)))
          (hash-set! expr->var expr (car (VfInsn-outputs insn)))))))

  expr->var)

;; ============================================================
;; With Statistics
;; ============================================================

(define (cfg-pre-with-stats cfg)
  (define gen-map (compute-expr-gen cfg))
  (define kill-map (compute-expr-kill cfg))
  (define-values (avail-in avail-out)
    (compute-availability cfg gen-map kill-map))
  (define-values (antic-in antic-out)
    (compute-anticipability cfg gen-map kill-map))

  (define eliminated-count 0)
  (define inserted-count 0)

  (define cfg^
    (for/fold ([cfg cfg])
              ([bid (cfg-all-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (cond
        [(not block) cfg]
        [else
         (define redundant-insns
           (find-redundant-in-block cfg bid avail-in))
         (if (null? redundant-insns)
             cfg
             (begin
               (set! eliminated-count
                     (+ eliminated-count (length redundant-insns)))
               (eliminate-redundant cfg bid redundant-insns avail-in)))])))

  (values cfg^
          `((eliminated . ,eliminated-count)
            (inserted . ,inserted-count))))

(provide cfg-pre-with-stats)

;; ============================================================
;; Helper: Mutable Set from Set
;; ============================================================

(define (set->mutable-set s)
  (define ms (mutable-set))
  (for ([e (in-set s)])
    (set-add! ms e))
  ms)
