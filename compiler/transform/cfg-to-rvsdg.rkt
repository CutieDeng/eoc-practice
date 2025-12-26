#lang racket/base

;; ============================================================
;; Transform: CFG → RVSDG
;; ============================================================
;;
;; 将控制流图转换为 RVSDG
;; 包含控制流结构化算法
;; ============================================================

(require racket/match racket/list racket/dict racket/set)
(require (except-in cutie-ftree symbol-compare))
(require "../core/cfg.rkt")
(require "../rvsdg/core-def.rkt")
(require "../rvsdg/raw/region-ctor.rkt")
(require "../rvsdg/safe/node.rkt")
(require "../rvsdg/safe/wire.rkt")
(require "../rvsdg/semantic/structured.rkt")
(require "../cfg/raw.rkt")

;; === 主入口 ===

;; 将 CFG 转换为 RVSDG Region
(define (cfg->rvsdg cfg)
  ;; 1. 计算支配树
  (define dom-info (compute-dominators cfg))

  ;; 2. 识别循环（回边）
  (define loops (identify-loops cfg dom-info))

  ;; 3. 结构化并生成 RVSDG
  (define region (Region-empty))
  (structurize-cfg cfg (cfg-get-entry cfg) dom-info loops region))

(provide cfg->rvsdg)

;; === 支配树计算 ===

;; 使用 Cooper, Harvey, Kennedy 的简单支配树算法
(define (compute-dominators cfg)
  (define entry (cfg-get-entry cfg))
  (define blocks (cfg-all-block-ids cfg))

  ;; 初始化：entry 支配自己，其他节点支配者未知
  (define initial-doms
    (for/fold ([doms (ordl-make-empty block-id-compare)])
              ([b blocks])
      (dict-set doms b (if (equal? b entry) b #f))))

  ;; 计算前驱
  (define preds (compute-predecessors cfg))

  ;; 后序遍历顺序
  (define post-order (reverse-postorder cfg entry))
  (define po-index
    (for/fold ([idx (ordl-make-empty block-id-compare)])
              ([b post-order]
               [i (in-naturals)])
      (dict-set idx b i)))

  ;; 迭代计算支配者
  (define (iterate doms)
    (define-values (doms^ changed)
      (for/fold ([doms doms] [changed #f])
                ([b post-order]
                 #:unless (equal? b entry))
        (define b-preds (dict-ref preds b '()))
        (define new-idom
          (for/fold ([idom #f])
                    ([p b-preds])
            (define p-idom (dict-ref doms p #f))
            (cond
              [(not p-idom) idom]
              [(not idom) p]
              [else (intersect doms po-index p idom)])))
        (if (equal? new-idom (dict-ref doms b #f))
            (values doms changed)
            (values (dict-set doms b new-idom) #t))))
    (if changed (iterate doms^) doms^))

  (iterate initial-doms))

;; 求两个节点的最近公共支配者
(define (intersect doms po-index b1 b2)
  (let loop ([f1 b1] [f2 b2])
    (cond
      [(equal? f1 f2) f1]
      [(> (dict-ref po-index f1 0) (dict-ref po-index f2 0))
       (loop (dict-ref doms f1) f2)]
      [else
       (loop f1 (dict-ref doms f2))])))

;; 计算前驱映射
(define (compute-predecessors cfg)
  (define blocks (cfg-all-block-ids cfg))
  (for/fold ([preds (ordl-make-empty block-id-compare)])
            ([b blocks])
    (define block (cfg-get-block cfg b))
    (define succs (terminator-successors (CfgBlock-terminator block)))
    (for/fold ([preds preds])
              ([s succs])
      (dict-update preds s (lambda (ps) (cons b ps)) '()))))

;; 逆后序遍历
(define (reverse-postorder cfg entry)
  (define visited (mutable-set))
  (define result '())

  (define (dfs block-id)
    (unless (set-member? visited block-id)
      (set-add! visited block-id)
      (define block (cfg-get-block cfg block-id))
      (when block
        (define succs (terminator-successors (CfgBlock-terminator block)))
        (for ([s succs]) (dfs s))
        (set! result (cons block-id result)))))

  (dfs entry)
  result)

;; === 循环识别 ===

(define (identify-loops cfg dom-info)
  ;; 回边：从 b 到 h 的边，其中 h 支配 b
  (define blocks (cfg-all-block-ids cfg))
  (define back-edges
    (for*/list ([b blocks]
                [s (terminator-successors
                     (CfgBlock-terminator (cfg-get-block cfg b)))]
                #:when (dominates? dom-info s b))
      (cons b s)))

  ;; 对每个回边，收集循环体
  (for/fold ([loops '()])
            ([edge back-edges])
    (match edge
      [(cons tail header)
       (define body (collect-loop-body cfg dom-info header tail))
       (cons (list header body tail) loops)])))

;; 检查 a 是否支配 b
(define (dominates? dom-info a b)
  (let loop ([current b])
    (cond
      [(equal? current a) #t]
      [(not (dict-ref dom-info current #f)) #f]
      [(equal? current (dict-ref dom-info current)) #f]
      [else (loop (dict-ref dom-info current))])))

;; 收集循环体
(define (collect-loop-body cfg dom-info header tail)
  (define body (mutable-set header))
  (define worklist (list tail))

  (let loop ([worklist worklist])
    (unless (null? worklist)
      (define n (car worklist))
      (unless (set-member? body n)
        (set-add! body n)
        (define preds (compute-predecessors cfg))
        (define n-preds (dict-ref preds n '()))
        (loop (append n-preds (cdr worklist))))
      (loop (cdr worklist))))

  (set->list body))

;; === 结构化转换 ===

(define (structurize-cfg cfg entry dom-info loops region)
  ;; 简化版本：线性处理块，识别基本结构

  ;; 创建变量映射：VarId → OutputId
  (define var-map (ordl-make-empty var-id-compare))

  ;; 按拓扑序处理块
  (define order (reverse-postorder cfg entry))

  (define-values (region^ var-map^)
    (for/fold ([region region]
               [var-map var-map])
              ([block-id order])
      (process-block cfg block-id region var-map dom-info loops)))

  region^)

;; 处理单个基本块
(define (process-block cfg block-id region var-map dom-info loops)
  (define block (cfg-get-block cfg block-id))
  (if (not block)
      (values region var-map)
      (let ()
        ;; 处理 φ 节点
        (define-values (region1 var-map1)
          (for/fold ([region region]
                     [var-map var-map])
                    ([phi (CfgBlock-phis block)])
            (process-phi phi region var-map)))

        ;; 处理指令
        (define-values (region2 var-map2)
          (for/fold ([region region1]
                     [var-map var-map1])
                    ([insn (CfgBlock-insns block)])
            (process-vf-insn insn region var-map)))

        ;; 处理终结器
        (define-values (region3 var-map3)
          (process-terminator cfg block-id (CfgBlock-terminator block)
                              region2 var-map2 dom-info loops))

        (values region3 var-map3))))

;; 处理 φ 节点 → Gamma 节点的输出
(define (process-phi phi region var-map)
  ;; φ 节点在 RVSDG 中体现为 Gamma 节点的输出
  ;; 简化处理：暂时跳过，在 Gamma 生成时处理
  (values region var-map))

;; 处理值流指令 → Simple 节点
(define (process-vf-insn insn region var-map)
  (match insn
    [(VfInsn op inputs outputs info)
     ;; 查找输入对应的 OutputId
     (define input-ports
       (for/list ([v inputs] #:when (VarId? v))
         (dict-ref var-map v #f)))

     ;; 创建 Simple 节点
     (define-values (node-id in-id out-id region^)
       (rvsdg/create-node-with-value
         region
         (length (filter VarId? inputs))
         (length outputs)
         (Simple op)))

     ;; 连接输入
     (define region^^
       (for/fold ([r region^])
                 ([port input-ports]
                  [i (in-naturals)]
                  #:when port)
         (define in-port (rvsdg/get-input-port r node-id i))
         (define-values (_w r^) (rvsdg/connect r port in-port))
         r^))

     ;; 更新变量映射
     (define var-map^
       (for/fold ([vm var-map])
                 ([v outputs]
                  [i (in-naturals)])
         (define out-port (rvsdg/get-output-port region^^ node-id i))
         (dict-set vm v out-port)))

     (values region^^ var-map^)]
    [_ (values region var-map)]))

;; 处理终结器
(define (process-terminator cfg block-id term region var-map dom-info loops)
  (match term
    [(TermJump target)
     ;; 无条件跳转：如果是回边，创建 Theta
     (define is-back-edge
       (for/or ([loop loops])
         (match loop
           [(list header body tail)
            (and (equal? block-id tail)
                 (equal? target header))])))
     (if is-back-edge
         (create-theta-for-loop cfg block-id target region var-map dom-info loops)
         (values region var-map))]

    [(TermBranch cond then-target else-target)
     ;; 条件分支：创建 Gamma
     (create-gamma-for-branch cfg block-id cond then-target else-target
                               region var-map dom-info loops)]

    [(TermSwitch value cases default)
     ;; Switch：创建多路 Gamma
     (values region var-map)]  ; 暂时简化

    [(TermReturn ret-vals)
     ;; 返回：连接到 Region 输出
     (values region var-map)]

    [(TermThrow exc)
     ;; 异常：创建 Kappa 或特殊处理
     (values region var-map)]

    [(TermUnreachable)
     (values region var-map)]))

;; 创建 Gamma 节点（条件分支）
(define (create-gamma-for-branch cfg block-id cond then-target else-target
                                  region var-map dom-info loops)
  ;; 找到汇合点（then 和 else 分支的公共支配者下界）
  (define join-point (find-join-point cfg then-target else-target dom-info))

  ;; 创建 then 分支的子 region
  (define then-region
    (structurize-subgraph cfg then-target join-point dom-info loops))

  ;; 创建 else 分支的子 region
  (define else-region
    (structurize-subgraph cfg else-target join-point dom-info loops))

  ;; 创建 Gamma 节点
  (define cond-output (dict-ref var-map cond #f))
  (define-values (node-id in-id out-id region^)
    (rvsdg/create-if-then-else region then-region else-region 1 0))

  ;; 连接条件输入
  (define region^^
    (if cond-output
        (let ([in-port (rvsdg/get-input-port region^ node-id 0)])
          (define-values (_w r) (rvsdg/connect region^ cond-output in-port))
          r)
        region^))

  (values region^^ var-map))

;; 创建 Theta 节点（循环）
(define (create-theta-for-loop cfg tail header region var-map dom-info loops)
  ;; 找到循环体
  (define loop-body
    (for/first ([loop loops]
                #:when (match loop
                         [(list h body t) (and (equal? h header) (equal? t tail))]
                         [_ #f]))
      (match loop [(list _ body _) body])))

  (if (not loop-body)
      (values region var-map)
      (let ()
        ;; 创建循环体的子 region
        (define body-region
          (structurize-loop-body cfg header loop-body dom-info loops))

        ;; 创建 Theta 节点
        (define-values (node-id in-id out-id region^)
          (rvsdg/create-theta region 0 0 body-region))

        (values region^ var-map))))

;; 结构化子图
(define (structurize-subgraph cfg entry exit dom-info loops)
  ;; 收集从 entry 到 exit 的所有块
  (define region (Region-empty))
  ;; 简化：返回空 region
  region)

;; 结构化循环体
(define (structurize-loop-body cfg header body-blocks dom-info loops)
  (define region (Region-empty))
  ;; 简化：返回空 region
  region)

;; 找到两个分支的汇合点
(define (find-join-point cfg block1 block2 dom-info)
  ;; 简化：返回 #f 表示没有明确汇合点
  #f)

;; === 辅助 ===

(define (var-id-compare a b)
  (integer-compare (VarId-id a) (VarId-id b)))

(define (dict-update d k f default)
  (dict-set d k (f (dict-ref d k default))))
