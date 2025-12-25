#lang racket/base

;; ============================================================
;; Analysis Layer: 分析框架
;; ============================================================
;;
;; 提供可扩展的程序分析框架：
;; - 数据流分析基础设施
;; - 格 (Lattice) 抽象
;; - 不动点迭代
;; ============================================================

(require racket/match racket/dict racket/set racket/list racket/class racket/contract)
(require "../core-def.rkt")
(require "../semantic/traverse.rkt")

;; === 格 (Lattice) 接口 ===

;; 格定义
(struct Lattice (
  bottom      ; 最小元素
  top         ; 最大元素（可选，#f 表示无限格）
  join        ; (-> Elem Elem Elem) 并操作
  meet        ; (-> Elem Elem Elem) 交操作（可选）
  leq?        ; (-> Elem Elem Boolean) 偏序关系
) #:transparent)

(provide (struct-out Lattice))

;; 常用格构造器

;; 集合格（幂集格）
(define (make-set-lattice universe)
  (Lattice
    (set)                           ; bottom = 空集
    (if universe (list->set universe) #f)  ; top = 全集
    set-union                       ; join = 并集
    set-intersect                   ; meet = 交集
    subset?))                       ; leq = 子集

(provide make-set-lattice)

;; 常量传播格
(define (make-constant-lattice)
  (Lattice
    'bottom
    'top
    (lambda (a b)
      (cond [(eq? a 'bottom) b]
            [(eq? b 'bottom) a]
            [(eq? a 'top) 'top]
            [(eq? b 'top) 'top]
            [(equal? a b) a]
            [else 'top]))
    (lambda (a b)
      (cond [(eq? a 'top) b]
            [(eq? b 'top) a]
            [(eq? a 'bottom) 'bottom]
            [(eq? b 'bottom) 'bottom]
            [(equal? a b) a]
            [else 'bottom]))
    (lambda (a b)
      (or (eq? a 'bottom)
          (eq? b 'top)
          (equal? a b)))))

(provide make-constant-lattice)

;; === 数据流分析框架 ===

;; 分析方向
(define FORWARD 'forward)
(define BACKWARD 'backward)

(provide FORWARD BACKWARD)

;; 数据流分析定义
(struct DataflowAnalysis (
  direction   ; 'forward | 'backward
  lattice     ; Lattice
  transfer    ; (-> NodeId AnalysisState Region AnalysisState)
  init        ; (-> NodeId Region LatticeElem) 初始值
) #:transparent)

(provide (struct-out DataflowAnalysis))

;; 分析状态（每个节点的格元素）
(define (make-analysis-state)
  (hash))

(define (analysis-state-get state node-id default)
  (hash-ref state node-id default))

(define (analysis-state-set state node-id value)
  (hash-set state node-id value))

(provide make-analysis-state analysis-state-get analysis-state-set)

;; === 不动点迭代 ===

;; 运行数据流分析
(define (run-dataflow-analysis region analysis)
  (define lattice (DataflowAnalysis-lattice analysis))
  (define direction (DataflowAnalysis-direction analysis))
  (define transfer (DataflowAnalysis-transfer analysis))
  (define init-fn (DataflowAnalysis-init analysis))
  (define join (Lattice-join lattice))
  (define bottom (Lattice-bottom lattice))
  (define leq? (Lattice-leq? lattice))

  ;; 获取节点顺序
  (define nodes
    (if (eq? direction FORWARD)
        (rvsdg/topological-order region)
        (rvsdg/reverse-topological-order region)))

  ;; 初始化状态
  (define initial-state
    (for/fold ([state (make-analysis-state)]) ([node-id (in-list nodes)])
      (analysis-state-set state node-id (init-fn node-id region))))

  ;; 获取前驱/后继
  (define get-deps
    (if (eq? direction FORWARD)
        rvsdg/node-predecessors
        rvsdg/node-successors))

  ;; 不动点迭代
  (let loop ([state initial-state]
             [changed? #t]
             [iterations 0])
    (if (or (not changed?) (> iterations 1000))
        state
        (let-values ([(new-state any-changed?)
                      (for/fold ([s state] [changed #f])
                                ([node-id (in-list nodes)])
                        ;; 收集依赖节点的值
                        (define deps (set->list (get-deps region node-id)))
                        (define in-value
                          (if (null? deps)
                              bottom
                              (foldl (lambda (dep acc)
                                       (join acc (analysis-state-get s dep bottom)))
                                     bottom
                                     deps)))
                        ;; 应用传递函数
                        (define out-value (transfer node-id in-value region))
                        ;; 检查是否变化
                        (define old-value (analysis-state-get s node-id bottom))
                        (if (leq? out-value old-value)
                            (values s changed)
                            (values (analysis-state-set s node-id out-value) #t)))])
          (loop new-state any-changed? (+ iterations 1))))))

(provide run-dataflow-analysis)

;; === 内置分析 ===

;; 活跃变量分析
(define (make-liveness-analysis)
  (DataflowAnalysis
    BACKWARD
    (make-set-lattice #f)  ; 无限集合格
    ;; 传递函数: live-out = (live-in - defs) ∪ uses
    (lambda (node-id live-out region)
      ;; 简化实现：每个节点的输出是定义，输入是使用
      (define-values (in-id in-cnt)
        (let ([inputs (dict-ref (Region-node->input region) node-id #f)])
          (if inputs (values (car inputs) (cdr inputs)) (values #f 0))))
      (define-values (out-id out-cnt)
        (let ([outputs (dict-ref (Region-node->output region) node-id #f)])
          (if outputs (values (car outputs) (cdr outputs)) (values #f 0))))
      ;; 定义: 输出端口
      (define defs
        (if out-id
            (for/set ([i (in-range out-cnt)])
              (OutputId (+ (OutputId-id out-id) i)))
            (set)))
      ;; 使用: 输入连接的源输出
      (define uses
        (if in-id
            (for/fold ([s (set)]) ([i (in-range in-cnt)])
              (define port (InputId (+ (InputId-id in-id) i)))
              (define wire (dict-ref (Region-input->wire region) port #f))
              (if wire
                  (let ([src (dict-ref (Region-wire->output region) wire #f)])
                    (if src (set-add s src) s))
                  s))
            (set)))
      (set-union (set-subtract live-out defs) uses))
    ;; 初始值: 空集
    (lambda (node-id region) (set))))

(provide make-liveness-analysis)

;; 可达定义分析
(define (make-reaching-definitions-analysis)
  (DataflowAnalysis
    FORWARD
    (make-set-lattice #f)
    ;; 传递函数: reach-out = (reach-in - kills) ∪ gens
    (lambda (node-id reach-in region)
      ;; 每个节点的输出是一个新定义
      (set-add reach-in node-id))
    ;; 初始值: 空集
    (lambda (node-id region) (set))))

(provide make-reaching-definitions-analysis)

;; === 用户扩展接口 ===

;; 分析接口（面向对象风格）
(define analysis<%>
  (interface ()
    [name (->m symbol?)]
    [run (->m any/c hash?)]
    [get-result (->m any/c any/c any/c)]))

(provide analysis<%>)

;; 分析管理器
(define analysis-manager%
  (class object%
    (init-field region)
    (super-new)

    (define analyses (hash))
    (define cache (hash))

    (define/public (register name analysis)
      (set! analyses (hash-set analyses name analysis)))

    (define/public (run-analysis name)
      (unless (hash-has-key? cache name)
        (define analysis (hash-ref analyses name))
        (define result (run-dataflow-analysis region analysis))
        (set! cache (hash-set cache name result)))
      (hash-ref cache name))

    (define/public (get-result name node-id)
      (define results (run-analysis name))
      (analysis-state-get results node-id #f))

    (define/public (invalidate [name #f])
      (if name
          (set! cache (hash-remove cache name))
          (set! cache (hash))))))

(provide analysis-manager%)
