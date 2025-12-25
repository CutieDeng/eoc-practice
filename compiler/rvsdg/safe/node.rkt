#lang racket/base

;; ============================================================
;; Safe Layer: 带检查的节点操作
;; ============================================================
;;
;; 提供完整的节点生命周期管理，包括：
;; - 节点创建（分配 ID + 端口 + 设置映射）
;; - 节点删除（自动断开连接）
;; - 节点检查
;; ============================================================

(require racket/match racket/dict)
(require "../core-def.rkt")
(require "../raw/region-ctor.rkt")
(require "../raw/node-ctor.rkt")
(require "../raw/node-ctor2.rkt")
(require "../raw/node-value.rkt")
(require "../raw/node-dtor.rkt")
(require "../raw/query.rkt")
(require "../raw/connect.rkt")

;; === 节点创建 ===

;; 创建节点（完整流程）
;; 返回: (Values NodeId InputId OutputId Region)
(define (rvsdg/create-node region input-cnt output-cnt)
  ;; 分配节点 ID
  (define-values (node-id region^) (rvsdg-raw/alloc-node-ids region 1))
  ;; 分配输入/输出端口 ID
  (define-values (input-id output-id region^^)
    (rvsdg-raw/alloc-input-output-ids region^ input-cnt output-cnt))
  ;; 设置节点的端口映射
  (define region^^^
    (rvsdg-raw/node-input-output-set region^^ node-id input-id input-cnt output-id output-cnt))
  (values node-id input-id output-id region^^^))

(provide rvsdg/create-node)

;; 创建节点并设置值
;; 返回: (Values NodeId InputId OutputId Region)
(define (rvsdg/create-node-with-value region input-cnt output-cnt value)
  (define-values (node-id input-id output-id region^)
    (rvsdg/create-node region input-cnt output-cnt))
  (define region^^ (rvsdg-raw/set-node-value region^ node-id value))
  (values node-id input-id output-id region^^))

(provide rvsdg/create-node-with-value)

;; === 节点存在性检查 ===

(define (rvsdg/node-exists? region node-id)
  (dict-has-key? (Region-node->input region) node-id))

(provide rvsdg/node-exists?)

;; === 节点删除 ===

;; 断开节点的所有输入连接
(define (disconnect-all-inputs region node-id)
  (define inputs (rvsdg-raw/node-inputs region node-id))
  (if (not inputs)
      region
      (match-let ([(cons input-id input-cnt) inputs])
        (for/fold ([r region]) ([i (in-range input-cnt)])
          (define current-input (rvsdg-raw/input-offset input-id i))
          (define wire-id (rvsdg-raw/input-wire r current-input))
          (if wire-id
              (let ([output-id (rvsdg-raw/wire-output r wire-id)])
                (if output-id
                    (rvsdg-raw/wire-input-output-disconnect r wire-id current-input output-id)
                    r))
              r)))))

;; 断开节点的所有输出连接
(define (disconnect-all-outputs region node-id)
  (define outputs (rvsdg-raw/node-outputs region node-id))
  (if (not outputs)
      region
      (match-let ([(cons output-id output-cnt) outputs])
        (for/fold ([r region]) ([i (in-range output-cnt)])
          (define current-output (rvsdg-raw/output-offset output-id i))
          (define wire-id (rvsdg-raw/output-wire r current-output))
          (if wire-id
              (let ([input-id (rvsdg-raw/wire-input r wire-id)])
                (if input-id
                    (rvsdg-raw/wire-input-output-disconnect r wire-id input-id current-output)
                    r))
              r)))))

;; 删除节点（自动断开所有连接）
;; 前置条件：node-id 存在
(define (rvsdg/delete-node region node-id)
  (unless (rvsdg/node-exists? region node-id)
    (error 'rvsdg/delete-node "Node does not exist: ~a" node-id))
  ;; 断开所有连接
  (define region^ (disconnect-all-inputs region node-id))
  (define region^^ (disconnect-all-outputs region^ node-id))
  ;; 释放节点
  (rvsdg-raw/free-node region^^ node-id))

(provide rvsdg/delete-node)

;; 批量删除节点
(define (rvsdg/delete-nodes region node-ids)
  (for/fold ([r region]) ([node-id (in-list node-ids)])
    (if (rvsdg/node-exists? r node-id)
        (rvsdg/delete-node r node-id)
        r)))

(provide rvsdg/delete-nodes)

;; === 节点查询 ===

;; 获取节点值（带检查）
(define (rvsdg/get-node-value region node-id)
  (unless (rvsdg/node-exists? region node-id)
    (error 'rvsdg/get-node-value "Node does not exist: ~a" node-id))
  (rvsdg-raw/get-node-value region node-id))

(provide rvsdg/get-node-value)

;; 获取节点的输入端口信息
;; 返回: (Values InputId Count)
(define (rvsdg/get-node-inputs region node-id)
  (unless (rvsdg/node-exists? region node-id)
    (error 'rvsdg/get-node-inputs "Node does not exist: ~a" node-id))
  (match-define (cons input-id cnt) (rvsdg-raw/node-inputs region node-id))
  (values input-id cnt))

(provide rvsdg/get-node-inputs)

;; 获取节点的输出端口信息
;; 返回: (Values OutputId Count)
(define (rvsdg/get-node-outputs region node-id)
  (unless (rvsdg/node-exists? region node-id)
    (error 'rvsdg/get-node-outputs "Node does not exist: ~a" node-id))
  (match-define (cons output-id cnt) (rvsdg-raw/node-outputs region node-id))
  (values output-id cnt))

(provide rvsdg/get-node-outputs)

;; 获取节点的第 i 个输入端口
(define (rvsdg/get-input-port region node-id i)
  (define-values (input-id cnt) (rvsdg/get-node-inputs region node-id))
  (unless (< i cnt)
    (error 'rvsdg/get-input-port "Input index out of range: ~a >= ~a" i cnt))
  (rvsdg-raw/input-offset input-id i))

(provide rvsdg/get-input-port)

;; 获取节点的第 i 个输出端口
(define (rvsdg/get-output-port region node-id i)
  (define-values (output-id cnt) (rvsdg/get-node-outputs region node-id))
  (unless (< i cnt)
    (error 'rvsdg/get-output-port "Output index out of range: ~a >= ~a" i cnt))
  (rvsdg-raw/output-offset output-id i))

(provide rvsdg/get-output-port)
