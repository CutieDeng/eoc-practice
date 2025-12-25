#lang racket/base

;; ============================================================
;; Semantic Layer: 图变换操作
;; ============================================================
;;
;; 提供 Region 的各种变换：
;; - 节点克隆
;; - 节点替换
;; - 子图操作
;; ============================================================

(require racket/match racket/dict racket/set racket/list)
(require "../core-def.rkt")
(require "../raw/query.rkt")
(require "../raw/node-ctor.rkt")
(require "../raw/node-value.rkt")
(require "../safe/node.rkt")
(require "../safe/wire.rkt")
(require "traverse.rkt")

;; === 节点克隆 ===

;; 克隆单个节点（不复制连接）
;; 返回: (Values new-node-id Region)
(define (rvsdg/clone-node region node-id)
  (unless (rvsdg/node-exists? region node-id)
    (error 'rvsdg/clone-node "Node does not exist: ~a" node-id))

  ;; 获取原节点信息
  (define-values (input-id input-cnt) (rvsdg/get-node-inputs region node-id))
  (define-values (output-id output-cnt) (rvsdg/get-node-outputs region node-id))
  (define value (rvsdg-raw/get-node-value region node-id))

  ;; 创建新节点
  (define-values (new-node-id new-input-id new-output-id region^)
    (rvsdg/create-node region input-cnt output-cnt))

  ;; 复制节点值
  (define region^^
    (if value
        (rvsdg-raw/set-node-value region^ new-node-id value)
        region^))

  (values new-node-id region^^))

(provide rvsdg/clone-node)

;; 克隆多个节点，返回 ID 映射
;; 返回: (Values (HashTable old-id new-id) Region)
(define (rvsdg/clone-nodes region node-ids)
  (for/fold ([mapping (hash)]
             [r region])
            ([node-id (in-list node-ids)])
    (if (rvsdg/node-exists? r node-id)
        (let-values ([(new-id r^) (rvsdg/clone-node r node-id)])
          (values (hash-set mapping node-id new-id) r^))
        (values mapping r))))

(provide rvsdg/clone-nodes)

;; === 节点替换 ===

;; 替换节点：将所有指向 old-node 输出的边改为指向 new-node 的对应输出
;; 前置条件：两个节点的输出端口数量相同
(define (rvsdg/replace-node-outputs region old-node-id new-node-id)
  (unless (rvsdg/node-exists? region old-node-id)
    (error 'rvsdg/replace-node-outputs "Old node does not exist: ~a" old-node-id))
  (unless (rvsdg/node-exists? region new-node-id)
    (error 'rvsdg/replace-node-outputs "New node does not exist: ~a" new-node-id))

  (define-values (old-out-id old-out-cnt) (rvsdg/get-node-outputs region old-node-id))
  (define-values (new-out-id new-out-cnt) (rvsdg/get-node-outputs region new-node-id))

  (unless (= old-out-cnt new-out-cnt)
    (error 'rvsdg/replace-node-outputs
           "Output count mismatch: ~a vs ~a" old-out-cnt new-out-cnt))

  ;; 重连每个输出
  (for/fold ([r region]) ([i (in-range old-out-cnt)])
    (define old-port (rvsdg-raw/output-offset old-out-id i))
    (define new-port (rvsdg-raw/output-offset new-out-id i))
    (define target (rvsdg/get-output-target r old-port))
    (if target
        (rvsdg/reconnect-input r target new-port)
        r)))

(provide rvsdg/replace-node-outputs)

;; === 子图克隆 ===

;; 克隆子图：克隆一组节点及其内部连接
;; 返回: (Values (HashTable old-id new-id) Region)
(define (rvsdg/clone-subgraph region node-ids)
  ;; 第一步：克隆所有节点
  (define-values (id-mapping region^)
    (rvsdg/clone-nodes region node-ids))

  ;; 第二步：复制内部连接
  (define node-set (list->set node-ids))
  (define region^^
    (for/fold ([r region^]) ([old-node-id (in-list node-ids)])
      (define new-node-id (hash-ref id-mapping old-node-id))
      (define-values (old-in-id old-in-cnt) (rvsdg/get-node-inputs region old-node-id))
      (define-values (new-in-id _) (rvsdg/get-node-inputs r new-node-id))

      ;; 对每个输入端口
      (for/fold ([r2 r]) ([i (in-range old-in-cnt)])
        (define old-input-port (rvsdg-raw/input-offset old-in-id i))
        (define new-input-port (rvsdg-raw/input-offset new-in-id i))
        (define source (rvsdg/get-input-source region old-input-port))
        (if source
            (let ([src-node (rvsdg-raw/output-node region source)])
              ;; 只复制子图内部的连接
              (if (set-member? node-set src-node)
                  (let* ([new-src-node (hash-ref id-mapping src-node)]
                         [old-src-outputs (rvsdg-raw/node-outputs region src-node)]
                         [new-src-outputs (rvsdg-raw/node-outputs r2 new-src-node)]
                         ;; 计算输出端口索引
                         [src-out-id (car old-src-outputs)]
                         [port-index (- (OutputId-id source) (OutputId-id src-out-id))]
                         [new-source (rvsdg-raw/output-offset (car new-src-outputs) port-index)])
                    (let-values ([(_ r3) (rvsdg/connect r2 new-source new-input-port)])
                      r3))
                  r2))
            r2))))

  (values id-mapping region^^))

(provide rvsdg/clone-subgraph)

;; === 死代码消除 ===

;; 删除未连接的节点（没有输出被使用的节点）
(define (rvsdg/eliminate-dead-nodes region live-nodes)
  (define live-set (if (set? live-nodes) live-nodes (list->set live-nodes)))
  (define all-nodes (rvsdg/all-nodes region))
  (define dead-nodes
    (filter (lambda (n) (not (set-member? live-set n))) all-nodes))
  (rvsdg/delete-nodes region dead-nodes))

(provide rvsdg/eliminate-dead-nodes)

;; 计算活跃节点（从给定的根节点反向可达）
(define (rvsdg/compute-live-nodes region root-nodes)
  (define roots (if (list? root-nodes) root-nodes (list root-nodes)))
  (let loop ([visited (set)]
             [worklist roots])
    (if (null? worklist)
        visited
        (let ([current (car worklist)]
              [rest (cdr worklist)])
          (if (or (not (rvsdg/node-exists? region current))
                  (set-member? visited current))
              (loop visited rest)
              (let ([preds (set->list (rvsdg/node-predecessors region current))])
                (loop (set-add visited current)
                      (append preds rest))))))))

(provide rvsdg/compute-live-nodes)

;; === 节点内联 ===

;; 内联简单节点：如果节点只有一个输入和一个输出，且是恒等操作
;; 则可以移除该节点，直接连接其输入源和输出目标
(define (rvsdg/inline-identity-node region node-id)
  (define-values (in-id in-cnt) (rvsdg/get-node-inputs region node-id))
  (define-values (out-id out-cnt) (rvsdg/get-node-outputs region node-id))

  (unless (and (= in-cnt 1) (= out-cnt 1))
    (error 'rvsdg/inline-identity-node
           "Can only inline nodes with 1 input and 1 output"))

  (define input-port in-id)
  (define output-port out-id)
  (define source (rvsdg/get-input-source region input-port))
  (define target (rvsdg/get-output-target region output-port))

  (cond
    [(and source target)
     ;; 有输入也有输出：重连
     (define region^ (rvsdg/reconnect-input region target source))
     (rvsdg/delete-node region^ node-id)]
    [target
     ;; 只有输出：断开输出，删除节点
     (define region^ (rvsdg/disconnect-input region target))
     (rvsdg/delete-node region^ node-id)]
    [else
     ;; 没有输出：直接删除
     (rvsdg/delete-node region node-id)]))

(provide rvsdg/inline-identity-node)

;; === 插入节点 ===

;; 在边上插入节点：将 src -> tgt 变为 src -> new -> tgt
;; 返回: (Values new-node-id Region)
(define (rvsdg/insert-node-on-edge region src-output tgt-input value)
  ;; 创建新节点（1 输入，1 输出）
  (define-values (new-node-id new-in-id new-out-id region^)
    (rvsdg/create-node-with-value region 1 1 value))

  ;; 重连：tgt-input 现在从 new-out 获取输入
  (define region^^ (rvsdg/reconnect-input region^ tgt-input new-out-id))

  ;; 连接：new-in 从 src-output 获取输入
  (define-values (_ region^^^) (rvsdg/connect region^^ src-output new-in-id))

  (values new-node-id region^^^))

(provide rvsdg/insert-node-on-edge)
