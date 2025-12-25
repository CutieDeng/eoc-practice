#lang racket/base

;; ============================================================
;; Raw Layer: 节点 ID 分配与端口映射
;; ============================================================

(require racket/match racket/dict)
(require "../core-def.rkt")

;; === ID 偏移计算 ===

(define (rvsdg-raw/node-offset id index)
  (match id [(NodeId x) (NodeId (+ x index))]))

(define (rvsdg-raw/input-offset id index)
  (match id [(InputId x) (InputId (+ x index))]))

(define (rvsdg-raw/output-offset id index)
  (match id [(OutputId x) (OutputId (+ x index))]))

(provide rvsdg-raw/node-offset)
(provide rvsdg-raw/input-offset)
(provide rvsdg-raw/output-offset)

;; === 节点 ID 分配 ===

(define (rvsdg-raw/alloc-node-ids region [cnt 1])
  (define current-cnt (Region-node-cnt region))
  (define node-id (NodeId current-cnt))
  (define region^ (struct-copy Region region [node-cnt (+ current-cnt cnt)]))
  (values node-id region^))

(provide rvsdg-raw/alloc-node-ids)

;; === 节点端口映射设置 ===

;; 设置节点的输入端口映射
;; 注意：这里只设置第一个 input-id 到 node-id 的映射
;; 对于多个输入端口，调用者需要使用 input-offset 并分别设置
(define (rvsdg-raw/node-input-set region node-id input-id input-cnt)
  (define node->input (Region-node->input region))
  (define input->node (Region-input->node region))
  (define node->input^ (dict-set node->input node-id (cons input-id input-cnt)))
  ;; 为所有输入端口设置反向映射
  (define input->node^
    (for/fold ([m input->node]) ([i (in-range input-cnt)])
      (dict-set m (rvsdg-raw/input-offset input-id i) node-id)))
  (struct-copy Region region
    [node->input node->input^]
    [input->node input->node^]))

(provide rvsdg-raw/node-input-set)

;; 设置节点的输出端口映射
(define (rvsdg-raw/node-output-set region node-id output-id output-cnt)
  (define node->output (Region-node->output region))
  (define output->node (Region-output->node region))
  (define node->output^ (dict-set node->output node-id (cons output-id output-cnt)))
  ;; 为所有输出端口设置反向映射
  (define output->node^
    (for/fold ([m output->node]) ([i (in-range output-cnt)])
      (dict-set m (rvsdg-raw/output-offset output-id i) node-id)))
  (struct-copy Region region
    [node->output node->output^]
    [output->node output->node^]))

(provide rvsdg-raw/node-output-set)

;; 同时设置输入和输出端口
(define (rvsdg-raw/node-input-output-set region node-id input-id input-cnt output-id output-cnt)
  (define region^ (rvsdg-raw/node-input-set region node-id input-id input-cnt))
  (rvsdg-raw/node-output-set region^ node-id output-id output-cnt))

(provide rvsdg-raw/node-input-output-set)
