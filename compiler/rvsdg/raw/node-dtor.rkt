#lang racket/base

;; ============================================================
;; Raw Layer: 节点释放
;; ============================================================
;;
;; 注意：这是 raw 层操作，只清理映射关系
;; 不会自动断开连接，调用者需要先断开所有边
;; ============================================================

(require racket/match racket/dict)
(require "../core-def.rkt")
(require "node-ctor.rkt")  ; for rvsdg-raw/input-offset, rvsdg-raw/output-offset

;; === 节点释放 ===

;; 释放节点：清理所有相关映射
;; 前置条件：节点的所有边应该已经断开
(define (rvsdg-raw/free-node region node-id)
  ;; 获取节点的输入端口信息
  (match-define (cons input-id input-cnt)
    (dict-ref (Region-node->input region) node-id))
  ;; 获取节点的输出端口信息
  (match-define (cons output-id output-cnt)
    (dict-ref (Region-node->output region) node-id))

  ;; 从 node->input, node->output, node->value 中移除节点
  (define node->input^ (dict-remove (Region-node->input region) node-id))
  (define node->output^ (dict-remove (Region-node->output region) node-id))
  (define node->value^ (dict-remove (Region-node->value region) node-id))

  ;; 从 input->node 中移除所有输入端口
  (define input->node^
    (for/fold ([m (Region-input->node region)]) ([i (in-range input-cnt)])
      (dict-remove m (rvsdg-raw/input-offset input-id i))))

  ;; 从 output->node 中移除所有输出端口
  (define output->node^
    (for/fold ([m (Region-output->node region)]) ([i (in-range output-cnt)])
      (dict-remove m (rvsdg-raw/output-offset output-id i))))

  (struct-copy Region region
    [node->input node->input^]
    [node->output node->output^]
    [node->value node->value^]
    [input->node input->node^]
    [output->node output->node^]))

(provide rvsdg-raw/free-node)
