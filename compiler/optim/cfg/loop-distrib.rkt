#lang racket/base

;; ============================================================
;; CFG Optimization: Loop Distribution (Loop Fission)
;; ============================================================
;;
;; 循环分布：将单个循环拆分为多个循环
;;
;; 转换:
;;   for i = 0 to n:
;;     A[i] = B[i] + 1
;;     C[i] = D[i] * 2
;;
;; 变为:
;;   for i = 0 to n:
;;     A[i] = B[i] + 1
;;   for i = 0 to n:
;;     C[i] = D[i] * 2
;;
;; 好处：
;; 1. 改善缓存局部性
;; 2. 启用部分向量化
;; 3. 允许更多循环优化
;;
;; 参考：GCC tree-loop-distribution.cc
;; ============================================================

(require racket/match racket/list racket/set racket/hash racket/function)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "loop-analysis.rkt")
(require "alias-analysis.rkt")

;; ============================================================
;; Dependency Analysis
;; ============================================================

;; 依赖类型
(define dep-flow 'flow)      ; 读后写 (RAW)
(define dep-anti 'anti)      ; 写后读 (WAR)
(define dep-output 'output)  ; 写后写 (WAW)

;; 计算循环体内指令间的依赖
(define (compute-dependencies insns)
  (define deps '())  ; (list (cons insn1 insn2) ...)

  ;; 收集所有定义和使用
  (define def-map (make-hash))  ; var -> insn
  (define use-map (make-hash))  ; var -> (listof insn)

  (for ([insn insns])
    (when (VfInsn? insn)
      ;; 记录定义
      (for ([out (VfInsn-outputs insn)])
        (hash-set! def-map out insn))
      ;; 记录使用
      (for ([inp (VfInsn-inputs insn)])
        (when (VarId? inp)
          (hash-update! use-map inp
                        (λ (lst) (cons insn lst))
                        '())))))

  ;; 计算依赖
  (for ([insn insns])
    (when (VfInsn? insn)
      ;; Flow 依赖：当前指令使用了之前指令定义的变量
      (for ([inp (VfInsn-inputs insn)])
        (when (VarId? inp)
          (define def-insn (hash-ref def-map inp #f))
          (when (and def-insn (not (equal? def-insn insn)))
            (set! deps (cons (cons def-insn insn) deps)))))

      ;; Output 依赖：当前指令重定义了之前指令定义的变量
      (for ([out (VfInsn-outputs insn)])
        (define users (hash-ref use-map out '()))
        (for ([user users])
          (when (not (equal? user insn))
            (set! deps (cons (cons insn user) deps)))))))

  (remove-duplicates deps))

;; ============================================================
;; Statement Partitioning
;; ============================================================

;; 使用依赖图对语句进行分区
;; 返回可以分离到不同循环的语句组
(define (partition-statements insns deps)
  ;; 构建依赖图的连通分量
  (define insn-set (list->set insns))
  (define adj (make-hash))

  ;; 初始化邻接表
  (for ([insn insns])
    (hash-set! adj insn (mutable-set)))

  ;; 添加依赖边（无向，因为依赖阻止分离）
  (for ([dep deps])
    (define from (car dep))
    (define to (cdr dep))
    (when (and (set-member? insn-set from)
               (set-member? insn-set to))
      (set-add! (hash-ref adj from) to)
      (set-add! (hash-ref adj to) from)))

  ;; 使用 DFS 找连通分量
  (define visited (mutable-set))
  (define components '())

  (define (dfs insn component)
    (when (not (set-member? visited insn))
      (set-add! visited insn)
      (set-add! component insn)
      (for ([neighbor (in-set (hash-ref adj insn (set)))])
        (dfs neighbor component))))

  (for ([insn insns])
    (when (not (set-member? visited insn))
      (define component (mutable-set))
      (dfs insn component)
      (set! components (cons (set->list component) components))))

  ;; 保持原始顺序
  (for/list ([comp (reverse components)])
    (sort comp insn-order<? #:key identity)))

;; 指令顺序比较
;; 支持 VarId-id 为整数或符号
(define (insn-order<? a b)
  (cond
    [(and (VfInsn? a) (VfInsn? b)
          (pair? (VfInsn-outputs a))
          (pair? (VfInsn-outputs b)))
     (let ([id-a (VarId-id (car (VfInsn-outputs a)))]
           [id-b (VarId-id (car (VfInsn-outputs b)))])
       (cond
         [(and (number? id-a) (number? id-b)) (< id-a id-b)]
         [(and (symbol? id-a) (symbol? id-b)) (symbol<? id-a id-b)]
         [(number? id-a) #t]  ; 数字在符号前
         [else #f]))]
    [else #f]))

;; ============================================================
;; Loop Distribution Transform
;; ============================================================

;; 生成新的块 ID
(define loop-distrib-counter 0)

(define (fresh-loop-block-id prefix)
  (set! loop-distrib-counter (+ loop-distrib-counter 1))
  (BlockId (string->symbol (format "~a_dist~a" prefix loop-distrib-counter))))

;; 检查循环是否可以分布
(define (can-distribute-loop? cfg lp)
  (define header (Loop-header lp))
  (define body-blocks (set->list (Loop-body lp)))

  ;; 简化条件：单块循环体（除头外）
  (define non-header-blocks
    (filter (λ (bid) (not (equal? bid header))) body-blocks))

  (and (= (length non-header-blocks) 1)
       ;; 循环体有多条指令
       (let ()
         (define body-bid (car non-header-blocks))
         (define body-block (cfg-get-block cfg body-bid))
         (and body-block
              (>= (length (CfgBlock-insns body-block)) 2)))))

;; 对循环执行分布
(define (distribute-loop cfg lp)
  (define header (Loop-header lp))
  (define body-blocks (set->list (Loop-body lp)))
  (define non-header-blocks
    (filter (λ (bid) (not (equal? bid header))) body-blocks))
  (define body-bid (car non-header-blocks))
  (define body-block (cfg-get-block cfg body-bid))

  (when (not body-block)
    (error 'distribute-loop "Body block not found"))

  (define insns (CfgBlock-insns body-block))
  (define deps (compute-dependencies insns))
  (define partitions (partition-statements insns deps))

  ;; 只有多个分区时才分布
  (if (<= (length partitions) 1)
      cfg
      ;; TODO: 实际分布需要复制循环结构
      ;; 这里只做简化处理
      cfg))

;; ============================================================
;; Main Pass
;; ============================================================

;; 对整个 CFG 执行循环分布
(define (cfg-loop-distrib cfg)
  ;; 分析循环结构
  (define loops (analyze-loops cfg))

  ;; 尝试分布每个循环
  (for/fold ([cfg cfg])
            ([lp loops])
    (if (can-distribute-loop? cfg lp)
        (distribute-loop cfg lp)
        cfg)))

(provide cfg-loop-distrib)

;; ============================================================
;; Analysis Only
;; ============================================================

;; 分析循环并返回可分布信息
(define (analyze-loop-distribution cfg)
  (define loops (analyze-loops cfg))

  (define results '())

  (for ([lp loops])
    (define header (Loop-header lp))
    (define body-blocks (set->list (Loop-body lp)))
    (define non-header-blocks
      (filter (λ (bid) (not (equal? bid header))) body-blocks))

    (when (= (length non-header-blocks) 1)
      (define body-bid (car non-header-blocks))
      (define body-block (cfg-get-block cfg body-bid))
      (when body-block
        (define insns (CfgBlock-insns body-block))
        (define deps (compute-dependencies insns))
        (define partitions (partition-statements insns deps))

        (set! results
              (cons `((loop . ,header)
                      (insn-count . ,(length insns))
                      (dep-count . ,(length deps))
                      (partition-count . ,(length partitions))
                      (distributable . ,(> (length partitions) 1)))
                    results)))))

  (reverse results))

(provide analyze-loop-distribution)

;; ============================================================
;; With Statistics
;; ============================================================

(define (cfg-loop-distrib-with-stats cfg)
  (define analysis (analyze-loop-distribution cfg))
  (define distributable-count
    (for/sum ([info analysis])
      (if (cdr (assoc 'distributable info)) 1 0)))

  (define cfg^ (cfg-loop-distrib cfg))

  (values cfg^
          `((loops-analyzed . ,(length analysis))
            (distributable-loops . ,distributable-count))))

(provide cfg-loop-distrib-with-stats)
