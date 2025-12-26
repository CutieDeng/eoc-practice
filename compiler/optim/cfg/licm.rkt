#lang racket/base

;; ============================================================
;; CFG Optimization: Loop Invariant Code Motion (LICM)
;; ============================================================
;;
;; 循环不变代码外提
;;
;; 将循环中不依赖循环迭代的计算移到循环外：
;;   1. 识别循环不变指令（输入在循环外定义或也是不变的）
;;   2. 将不变指令移到循环预头（preheader）
;;
;; 条件：
;;   - 指令无副作用
;;   - 指令的所有输入要么在循环外定义，要么也是循环不变的
;;   - 指令支配所有循环出口（保证总会执行）
;;
;; 参考：GCC tree-ssa-loop-im.cc
;; ============================================================

(require racket/match racket/list racket/dict racket/set)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "loop-analysis.rkt")

;; ============================================================
;; 循环不变性分析
;; ============================================================

;; 判断操作是否可以外提（无副作用且可安全移动）
(define (hoistable-op? op)
  (case op
    ;; 算术运算
    [(const add sub mul neg) #t]
    ;; 位运算
    [(and or xor shl shr ushr) #t]
    ;; 比较运算
    [(eq ne lt le gt ge eq0 ne0) #t]
    ;; 类型转换
    [(i2l i2f i2d l2i l2f l2d f2i f2l f2d d2i d2l d2f) #t]
    [(i2b i2c i2s) #t]
    ;; 不能外提的操作
    ;; - div/rem 可能抛出异常
    ;; - 数组/字段访问可能抛出异常或有别名问题
    ;; - 方法调用有副作用
    [else #f]))

;; 构建变量定义位置映射
;; 返回 VarId → BlockId
(define (build-def-block-map cfg)
  (for*/fold ([m (ordl-make-empty var-id-compare)])
             ([bid (cfg-all-block-ids cfg)]
              [block (in-value (cfg-get-block cfg bid))]
              #:when block
              [insn (CfgBlock-insns block)]
              #:when (VfInsn? insn)
              [out (VfInsn-outputs insn)])
    (dict-set m out bid)))

;; 检查变量是否在循环外定义
(define (defined-outside-loop? var def-map loop-body)
  (define def-block (dict-ref def-map var #f))
  (or (not def-block)
      (not (set-member? loop-body def-block))))

;; 检查指令是否为循环不变的
;; invariant-vars: 已知的循环不变变量集合
(define (loop-invariant-insn? insn loop-body def-map invariant-vars)
  (and (VfInsn? insn)
       (hoistable-op? (VfInsn-op insn))
       ;; 所有输入要么在循环外定义，要么是循环不变的
       (for/and ([inp (flatten (VfInsn-inputs insn))])
         (or (not (VarId? inp))
             (defined-outside-loop? inp def-map loop-body)
             (set-member? invariant-vars inp)))))

;; 查找循环中所有不变指令
;; 返回 (list (cons BlockId insn) ...)
(define (find-invariant-insns cfg loop def-map)
  (define loop-body (Loop-body loop))

  ;; 迭代直到不动点
  (let outer-loop ([invariant-vars (set)]
                   [invariant-insns '()])
    (define-values (new-vars new-insns)
      (for*/fold ([vars invariant-vars]
                  [insns invariant-insns])
                 ([bid (in-set loop-body)]
                  [block (in-value (cfg-get-block cfg bid))]
                  #:when block
                  [insn (CfgBlock-insns block)]
                  #:when (VfInsn? insn))
        ;; 跳过已处理的指令
        (define outputs (VfInsn-outputs insn))
        (if (and (pair? outputs)
                 (not (set-member? vars (car outputs)))
                 (loop-invariant-insn? insn loop-body def-map vars))
            (values (set-union vars (list->set outputs))
                    (cons (cons bid insn) insns))
            (values vars insns))))

    (if (equal? new-vars invariant-vars)
        invariant-insns
        (outer-loop new-vars new-insns))))

;; ============================================================
;; 代码外提
;; ============================================================

;; 从块中移除指定指令
(define (remove-insn-from-block block insn)
  (struct-copy CfgBlock block
               [insns (filter (λ (i) (not (equal? i insn)))
                              (CfgBlock-insns block))]))

;; 在块末尾（terminator 前）插入指令
(define (append-insn-to-block block insn)
  (struct-copy CfgBlock block
               [insns (append (CfgBlock-insns block) (list insn))]))

;; 对单个循环执行 LICM
(define (licm-loop cfg loop def-map)
  (define preheader (Loop-preheader loop))

  ;; 如果没有预头，无法外提
  (if (not preheader)
      (values cfg 0)
      (let ()
        (define invariants (find-invariant-insns cfg loop def-map))

        ;; 移动每个不变指令
        (define-values (cfg^ count)
          (for/fold ([cfg cfg] [count 0])
                    ([inv invariants])
            (match-define (cons src-bid insn) inv)

            ;; 从源块移除
            (define src-block (cfg-get-block cfg src-bid))
            (define cfg1
              (cfg-set-block cfg (remove-insn-from-block src-block insn)))

            ;; 添加到预头
            (define pre-block (cfg-get-block cfg1 preheader))
            (define cfg2
              (cfg-set-block cfg1 (append-insn-to-block pre-block insn)))

            (values cfg2 (+ count 1))))

        (values cfg^ count))))

;; ============================================================
;; LICM 主入口
;; ============================================================

(define (cfg-licm cfg)
  (define loops (analyze-loops cfg))

  ;; 如果没有循环，直接返回
  (if (null? loops)
      cfg
      (let ()
        (define def-map (build-def-block-map cfg))

        ;; 按嵌套深度处理（内层优先）
        (define sorted-loops (sort-loops-by-depth loops))

        ;; 对每个循环执行 LICM
        (for/fold ([cfg cfg])
                  ([loop sorted-loops])
          (define-values (cfg^ _) (licm-loop cfg loop def-map))
          cfg^))))

(provide cfg-licm)

;; ============================================================
;; 带统计版本
;; ============================================================

(define (cfg-licm-with-stats cfg)
  (define loops (analyze-loops cfg))

  (if (null? loops)
      (values cfg '((loops-found . 0) (insns-hoisted . 0)))
      (let ()
        (define def-map (build-def-block-map cfg))
        (define sorted-loops (sort-loops-by-depth loops))

        (define-values (cfg^ total-hoisted)
          (for/fold ([cfg cfg] [total 0])
                    ([loop sorted-loops])
            (define-values (cfg1 count) (licm-loop cfg loop def-map))
            (values cfg1 (+ total count))))

        (values cfg^
                `((loops-found . ,(length loops))
                  (insns-hoisted . ,total-hoisted))))))

(provide cfg-licm-with-stats)

;; ============================================================
;; 预头创建（如果需要）
;; ============================================================

;; 为循环创建预头
;; 这会修改 CFG 结构
(define (ensure-preheader cfg loop)
  (define header (Loop-header loop))
  (define existing (Loop-preheader loop))

  (if existing
      (values cfg existing)
      ;; 需要创建预头
      (let ()
        ;; 创建新块
        (define-values (pre-bid cfg1) (cfg-create-block cfg))

        ;; 设置预头的 terminator 跳转到 header
        (define cfg2
          (cfg-block-set-terminator cfg1 pre-bid
            (TermJump header)))

        ;; 更新所有循环外前驱，让它们跳转到预头
        (define preds (compute-predecessors cfg2))
        (define loop-body (Loop-body loop))
        (define outside-preds
          (for/list ([p (hash-ref preds header '())]
                     #:when (not (set-member? loop-body p)))
            p))

        (define cfg3
          (for/fold ([cfg cfg2])
                    ([pred outside-preds])
            (define block (cfg-get-block cfg pred))
            (if (not block)
                cfg
                (let ([new-term
                       (match (CfgBlock-terminator block)
                         [(TermJump t)
                          (if (equal? t header)
                              (TermJump pre-bid)
                              (TermJump t))]
                         [(TermBranch c t e)
                          (TermBranch c
                                      (if (equal? t header) pre-bid t)
                                      (if (equal? e header) pre-bid e))]
                         [term term])])
                  (cfg-block-set-terminator cfg pred new-term)))))

        (values cfg3 pre-bid))))

(provide ensure-preheader)
