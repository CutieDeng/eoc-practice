#lang racket/base

;; ============================================================
;; CFG Optimization: Sparse Conditional Constant Propagation (SCCP)
;; ============================================================
;;
;; SCCP 是一种更强大的常量传播算法，它同时追踪：
;;   1. 变量的常量值（使用格）
;;   2. 基本块的可达性
;;
;; 格 (Lattice):
;;   ⊤ (top)     - 未知/未初始化
;;   constant    - 已知常量值
;;   ⊥ (bottom)  - 非常量（多个可能值）
;;
;; 优势：
;;   - 如果分支条件是常量，只分析实际执行的路径
;;   - 可以发现更多的常量（死分支中的 phi 节点不参与合并）
;;
;; 参考：GCC tree-ssa-ccp.cc
;; ============================================================

(require racket/match racket/list racket/dict racket/set)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")

;; ============================================================
;; 格值定义
;; ============================================================

;; 格值表示
(struct LatticeTop () #:prefab)          ; ⊤ - 未知
(struct LatticeConst (value) #:prefab)   ; 常量
(struct LatticeBottom () #:prefab)       ; ⊥ - 非常量

(define TOP (LatticeTop))
(define BOTTOM (LatticeBottom))

;; 格值合并（meet 操作）
;; TOP ⊓ x = x
;; x ⊓ TOP = x
;; BOTTOM ⊓ x = BOTTOM
;; const(a) ⊓ const(a) = const(a)
;; const(a) ⊓ const(b) = BOTTOM (a ≠ b)
(define (lattice-meet v1 v2)
  (match* (v1 v2)
    [((LatticeTop) v) v]
    [(v (LatticeTop)) v]
    [((LatticeBottom) _) BOTTOM]
    [(_ (LatticeBottom)) BOTTOM]
    [((LatticeConst a) (LatticeConst b))
     (if (equal? a b)
         (LatticeConst a)
         BOTTOM)]))

;; 判断格值是否为常量
(define (lattice-const? v)
  (LatticeConst? v))

;; 获取常量值
(define (lattice-value v)
  (and (LatticeConst? v) (LatticeConst-value v)))

;; ============================================================
;; SCCP 状态
;; ============================================================

;; SCCP 分析状态
(struct SCCPState
  (var-lattice      ; VarId → LatticeValue
   block-reachable  ; BlockId → bool
   cfg-worklist     ; (set BlockId) - 待处理的块
   ssa-worklist)    ; (set VarId) - 待处理的变量
  #:mutable)

(define (make-sccp-state)
  (SCCPState
   (ordl-make-empty var-id-compare)
   (ordl-make-empty block-id-compare)
   (mutable-set)
   (mutable-set)))

;; 获取变量的格值
(define (get-var-lattice state var)
  (dict-ref (SCCPState-var-lattice state) var TOP))

;; 设置变量的格值，返回是否有变化
(define (set-var-lattice! state var new-val)
  (define old-val (get-var-lattice state var))
  (define merged (lattice-meet old-val new-val))
  (unless (equal? old-val merged)
    (set-SCCPState-var-lattice! state
      (dict-set (SCCPState-var-lattice state) var merged))
    #t))

;; 标记块为可达
(define (mark-block-reachable! state bid)
  (define already-reachable
    (dict-ref (SCCPState-block-reachable state) bid #f))
  (unless already-reachable
    (set-SCCPState-block-reachable! state
      (dict-set (SCCPState-block-reachable state) bid #t))
    (set-add! (SCCPState-cfg-worklist state) bid)))

;; 检查块是否可达
(define (block-reachable? state bid)
  (dict-ref (SCCPState-block-reachable state) bid #f))

;; ============================================================
;; 常量求值
;; ============================================================

;; 对指令求值，返回输出的格值列表
(define (evaluate-insn insn state)
  (match insn
    [(VfInsn 'const (list value) outputs _ _)
     (list (LatticeConst value))]

    [(VfInsn op inputs outputs _ _)
     ;; 获取输入的格值
     (define input-vals
       (for/list ([inp (flatten inputs)])
         (if (VarId? inp)
             (get-var-lattice state inp)
             (LatticeConst inp))))

     ;; 如果任何输入是 BOTTOM，结果也是 BOTTOM
     (cond
       [(ormap LatticeBottom? input-vals)
        (make-list (length outputs) BOTTOM)]
       ;; 如果任何输入是 TOP，结果是 TOP（等待更多信息）
       [(ormap LatticeTop? input-vals)
        (make-list (length outputs) TOP)]
       ;; 所有输入都是常量，尝试求值
       [else
        (define const-vals (map LatticeConst-value input-vals))
        (define result (try-evaluate-op op const-vals))
        (if result
            (list (LatticeConst result))
            (make-list (length outputs) BOTTOM))])]

    [_ (list BOTTOM)]))

;; 尝试对操作求值
(define (try-evaluate-op op vals)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (match* (op vals)
      ;; 算术运算
      [('add (list a b)) (+ a b)]
      [('sub (list a b)) (- a b)]
      [('mul (list a b)) (* a b)]
      [('div (list a b)) (and (not (zero? b)) (quotient a b))]
      [('rem (list a b)) (and (not (zero? b)) (remainder a b))]
      [('neg (list a)) (- a)]

      ;; 位运算
      [('and (list a b)) (bitwise-and a b)]
      [('or (list a b)) (bitwise-ior a b)]
      [('xor (list a b)) (bitwise-xor a b)]
      [('shl (list a b)) (arithmetic-shift a b)]
      [('shr (list a b)) (arithmetic-shift a (- b))]

      ;; 比较运算（返回 0 或 1）
      [('eq (list a b)) (if (= a b) 1 0)]
      [('ne (list a b)) (if (not (= a b)) 1 0)]
      [('lt (list a b)) (if (< a b) 1 0)]
      [('le (list a b)) (if (<= a b) 1 0)]
      [('gt (list a b)) (if (> a b) 1 0)]
      [('ge (list a b)) (if (>= a b) 1 0)]
      [('eq0 (list a)) (if (zero? a) 1 0)]
      [('ne0 (list a)) (if (not (zero? a)) 1 0)]

      ;; 其他情况无法求值
      [(_ _) #f])))

;; ============================================================
;; SCCP 主算法
;; ============================================================

;; 处理单个块
(define (process-block cfg state bid)
  (define block (cfg-get-block cfg bid))
  (when block
    ;; 处理所有指令
    (for ([insn (CfgBlock-insns block)])
      (when (VfInsn? insn)
        (define results (evaluate-insn insn state))
        (for ([out (VfInsn-outputs insn)]
              [res results])
          (when (set-var-lattice! state out res)
            ;; 值有变化，添加使用者到工作列表
            (set-add! (SCCPState-ssa-worklist state) out)))))

    ;; 处理 terminator
    (process-terminator cfg state bid (CfgBlock-terminator block))))

;; 处理 terminator
(define (process-terminator cfg state bid term)
  (match term
    [(TermJump target)
     (mark-block-reachable! state target)]

    [(TermBranch cond then-bid else-bid)
     (define cond-val
       (if (VarId? cond)
           (get-var-lattice state cond)
           (LatticeConst cond)))
     (match cond-val
       [(LatticeTop)
        ;; 条件未知，两边都可能可达
        (mark-block-reachable! state then-bid)
        (mark-block-reachable! state else-bid)]
       [(LatticeConst v)
        ;; 条件已知，只有一边可达
        (if (not (zero? v))
            (mark-block-reachable! state then-bid)
            (mark-block-reachable! state else-bid))]
       [(LatticeBottom)
        ;; 条件是变量，两边都可达
        (mark-block-reachable! state then-bid)
        (mark-block-reachable! state else-bid)])]

    [(TermSwitch value cases default-bid)
     ;; TODO: 处理 switch
     (for ([c cases])
       (mark-block-reachable! state (cdr c)))
     (mark-block-reachable! state default-bid)]

    [(TermReturn _) (void)]
    [(TermThrow _) (void)]
    [(TermUnreachable) (void)]))

;; SCCP 主循环
(define (run-sccp cfg)
  (define state (make-sccp-state))

  ;; 从入口块开始
  (define entry (cfg-get-entry cfg))
  (when entry
    (mark-block-reachable! state entry))

  ;; 迭代直到不动点
  (let outer-loop ([iterations 0])
    (when (> iterations 20)
      (error 'run-sccp "Did not converge in 20 iterations"))

    ;; 处理所有待处理的块
    (let inner-loop ()
      (unless (set-empty? (SCCPState-cfg-worklist state))
        (define bid (set-first (SCCPState-cfg-worklist state)))
        (set-remove! (SCCPState-cfg-worklist state) bid)
        (process-block cfg state bid)
        (inner-loop)))

    ;; 检查是否有变化（通过重新处理所有可达块）
    (define old-lattice (SCCPState-var-lattice state))

    ;; 重新处理所有可达块
    (for ([bid (cfg-all-block-ids cfg)]
          #:when (block-reachable? state bid))
      (define block (cfg-get-block cfg bid))
      (when block
        (for ([insn (CfgBlock-insns block)])
          (when (VfInsn? insn)
            (define results (evaluate-insn insn state))
            (for ([out (VfInsn-outputs insn)]
                  [res results])
              (set-var-lattice! state out res))))))

    ;; 检查是否收敛
    (if (equal? old-lattice (SCCPState-var-lattice state))
        state
        (outer-loop (+ iterations 1)))))

;; ============================================================
;; 应用 SCCP 结果
;; ============================================================

;; 替换常量
(define (apply-sccp-results cfg state)
  (define var-lattice (SCCPState-var-lattice state))

  (define (rewrite-input inp)
    (if (VarId? inp)
        (let ([lat (dict-ref var-lattice inp #f)])
          (if (and lat (LatticeConst? lat))
              inp  ; 保持变量，让后续优化处理
              inp))
        inp))

  (define (rewrite-inputs inputs)
    (cond
      [(VarId? inputs) (rewrite-input inputs)]
      [(list? inputs) (map rewrite-inputs inputs)]
      [else inputs]))

  ;; 重写所有块
  (for/fold ([cfg cfg])
            ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if (not block)
        cfg
        (let ()
          ;; 如果块不可达，可以删除（但保留结构）
          (define reachable? (block-reachable? state bid))

          ;; 重写指令
          (define new-insns
            (for/list ([insn (CfgBlock-insns block)])
              (if (VfInsn? insn)
                  (let* ([outputs (VfInsn-outputs insn)]
                         [first-out (and (pair? outputs) (car outputs))]
                         [lat (and first-out
                                   (dict-ref var-lattice first-out #f))])
                    (if (and lat (LatticeConst? lat))
                        ;; 替换为常量定义
                        (VfInsn 'const
                                (list (LatticeConst-value lat))
                                outputs
                                (VfInsn-info insn)
                                (VfInsn-id insn))
                        ;; 保持原样
                        insn))
                  insn)))

          (cfg-set-block cfg
            (struct-copy CfgBlock block [insns new-insns]))))))

;; ============================================================
;; SCCP 主入口
;; ============================================================

(define (cfg-sccp cfg)
  (define state (run-sccp cfg))
  (apply-sccp-results cfg state))

(provide cfg-sccp)

;; 带统计版本
(define (cfg-sccp-with-stats cfg)
  (define state (run-sccp cfg))

  ;; 统计常量数量
  (define const-count
    (for/sum ([(_ val) (in-dict (SCCPState-var-lattice state))])
      (if (LatticeConst? val) 1 0)))

  ;; 统计可达块数量
  (define reachable-count
    (for/sum ([bid (cfg-all-block-ids cfg)])
      (if (block-reachable? state bid) 1 0)))

  (define total-blocks (length (cfg-all-block-ids cfg)))

  (define cfg^ (apply-sccp-results cfg state))

  (values cfg^
          `((constants-found . ,const-count)
            (reachable-blocks . ,reachable-count)
            (total-blocks . ,total-blocks)
            (unreachable-blocks . ,(- total-blocks reachable-count)))))

(provide cfg-sccp-with-stats)

;; 导出格值类型用于测试
(provide LatticeTop LatticeConst LatticeBottom TOP BOTTOM
         lattice-meet lattice-const? lattice-value)
