#lang racket/base

;; ============================================================
;; Semantic Layer: 遍历操作
;; ============================================================
;;
;; 提供 Region 的各种遍历方式：
;; - 节点遍历
;; - 拓扑排序
;; - 前驱/后继查询
;; ============================================================

(require racket/match racket/dict racket/set racket/list)
(require "../core-def.rkt")
(require "../raw/query.rkt")
(require "../raw/node-ctor.rkt")
(require "../raw/node-value.rkt")
(require "../safe/node.rkt")
(require "../safe/wire.rkt")

;; === 节点遍历 ===

;; 获取所有节点 ID（列表形式）
(define (rvsdg/all-nodes region)
  (rvsdg-raw/all-node-ids region))

(provide rvsdg/all-nodes)

;; 过滤节点
(define (rvsdg/filter-nodes region pred?)
  (filter (lambda (node-id)
            (define value (rvsdg-raw/get-node-value region node-id))
            (and value (pred? value)))
          (rvsdg/all-nodes region)))

(provide rvsdg/filter-nodes)

;; 按节点类型查找
(define (rvsdg/find-nodes-by-type region type-pred?)
  (rvsdg/filter-nodes region type-pred?))

(provide rvsdg/find-nodes-by-type)

;; 查找特定操作符的 Simple 节点
(define (rvsdg/find-simple-nodes region op)
  (rvsdg/filter-nodes region
    (lambda (v) (and (Simple? v) (equal? (Simple-op v) op)))))

(provide rvsdg/find-simple-nodes)

;; === 前驱/后继查询 ===

;; 获取节点的直接前驱（数据流上游）
(define (rvsdg/node-predecessors region node-id)
  (define inputs (rvsdg-raw/node-inputs region node-id))
  (if (not inputs)
      (set)
      (match-let ([(cons input-id input-cnt) inputs])
        (for/fold ([preds (set)]) ([i (in-range input-cnt)])
          (define port (rvsdg-raw/input-offset input-id i))
          (define source (rvsdg/get-input-source region port))
          (if source
              (let ([src-node (rvsdg-raw/output-node region source)])
                (if src-node (set-add preds src-node) preds))
              preds)))))

(provide rvsdg/node-predecessors)

;; 获取节点的直接后继（数据流下游）
(define (rvsdg/node-successors region node-id)
  (define outputs (rvsdg-raw/node-outputs region node-id))
  (if (not outputs)
      (set)
      (match-let ([(cons output-id output-cnt) outputs])
        (for/fold ([succs (set)]) ([i (in-range output-cnt)])
          (define port (rvsdg-raw/output-offset output-id i))
          (define target (rvsdg/get-output-target region port))
          (if target
              (let ([tgt-node (rvsdg-raw/input-node region target)])
                (if tgt-node (set-add succs tgt-node) succs))
              succs)))))

(provide rvsdg/node-successors)

;; === 拓扑排序 ===

;; 计算入度
(define (compute-in-degrees region nodes)
  (for/fold ([degrees (hash)]) ([node-id (in-list nodes)])
    (hash-set degrees node-id (set-count (rvsdg/node-predecessors region node-id)))))

;; Kahn 算法实现拓扑排序
(define (rvsdg/topological-order region)
  (define nodes (rvsdg/all-nodes region))
  (define in-degrees (compute-in-degrees region nodes))

  ;; 找出入度为 0 的节点
  (define (zero-in-degree-nodes degrees)
    (for/list ([(node deg) (in-hash degrees)] #:when (= deg 0))
      node))

  ;; Kahn 算法
  (let loop ([result '()]
             [degrees in-degrees]
             [queue (zero-in-degree-nodes in-degrees)])
    (if (null? queue)
        (reverse result)
        (let* ([node (car queue)]
               [rest-queue (cdr queue)]
               [successors (set->list (rvsdg/node-successors region node))]
               ;; 更新后继节点的入度
               [new-degrees
                (for/fold ([d (hash-remove degrees node)]) ([succ (in-list successors)])
                  (if (hash-has-key? d succ)
                      (hash-update d succ sub1)
                      d))]
               ;; 新变为入度 0 的节点
               [new-zeros
                (filter (lambda (s) (and (hash-has-key? new-degrees s)
                                          (= (hash-ref new-degrees s) 0)))
                        successors)])
          (loop (cons node result)
                new-degrees
                (append rest-queue new-zeros))))))

(provide rvsdg/topological-order)

;; 逆拓扑排序
(define (rvsdg/reverse-topological-order region)
  (reverse (rvsdg/topological-order region)))

(provide rvsdg/reverse-topological-order)

;; === 可达性分析 ===

;; 从给定节点向前（上游）的传递闭包
(define (rvsdg/transitive-predecessors region node-id)
  (let loop ([visited (set)]
             [worklist (list node-id)])
    (if (null? worklist)
        (set-remove visited node-id)  ; 不包含自身
        (let ([current (car worklist)]
              [rest (cdr worklist)])
          (if (set-member? visited current)
              (loop visited rest)
              (let ([preds (set->list (rvsdg/node-predecessors region current))])
                (loop (set-add visited current)
                      (append preds rest))))))))

(provide rvsdg/transitive-predecessors)

;; 从给定节点向后（下游）的传递闭包
(define (rvsdg/transitive-successors region node-id)
  (let loop ([visited (set)]
             [worklist (list node-id)])
    (if (null? worklist)
        (set-remove visited node-id)
        (let ([current (car worklist)]
              [rest (cdr worklist)])
          (if (set-member? visited current)
              (loop visited rest)
              (let ([succs (set->list (rvsdg/node-successors region current))])
                (loop (set-add visited current)
                      (append succs rest))))))))

(provide rvsdg/transitive-successors)

;; === 连通性 ===

;; 检查两个节点是否连通（node-a 可达 node-b）
(define (rvsdg/reachable? region from-node to-node)
  (set-member? (rvsdg/transitive-successors region from-node) to-node))

(provide rvsdg/reachable?)

;; 获取从 roots 可达的所有节点
(define (rvsdg/reachable-from region roots)
  (let loop ([visited (set)]
             [worklist (if (list? roots) roots (list roots))])
    (if (null? worklist)
        visited
        (let ([current (car worklist)]
              [rest (cdr worklist)])
          (if (set-member? visited current)
              (loop visited rest)
              (let ([succs (set->list (rvsdg/node-successors region current))])
                (loop (set-add visited current)
                      (append succs rest))))))))

(provide rvsdg/reachable-from)

;; === 边遍历 ===

;; 获取所有边 (source-node . target-node) 对
(define (rvsdg/all-edges region)
  (define wires (rvsdg-raw/all-wire-ids region))
  (for/list ([wire-id (in-list wires)])
    (define input-id (rvsdg-raw/wire-input region wire-id))
    (define output-id (rvsdg-raw/wire-output region wire-id))
    (define src-node (and output-id (rvsdg-raw/output-node region output-id)))
    (define tgt-node (and input-id (rvsdg-raw/input-node region input-id)))
    (cons src-node tgt-node)))

(provide rvsdg/all-edges)

;; 统计节点数和边数
(define (rvsdg/graph-size region)
  (values (length (rvsdg/all-nodes region))
          (length (rvsdg-raw/all-wire-ids region))))

(provide rvsdg/graph-size)
