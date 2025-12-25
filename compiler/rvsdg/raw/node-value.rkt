#lang racket/base

;; ============================================================
;; Raw Layer: 节点值操作
;; ============================================================

(require racket/dict)
(require "../core-def.rkt")

;; === 节点值设置 ===

(define (rvsdg-raw/set-node-value region node-id value)
  (define node->value (Region-node->value region))
  (define node->value^ (dict-set node->value node-id value))
  (struct-copy Region region [node->value node->value^]))

(provide rvsdg-raw/set-node-value)

;; === 节点值获取 ===

(define (rvsdg-raw/get-node-value region node-id)
  (dict-ref (Region-node->value region) node-id #f))

(provide rvsdg-raw/get-node-value)

;; === 节点值移除 ===

(define (rvsdg-raw/remove-node-value region node-id)
  (define node->value (Region-node->value region))
  (define node->value^ (dict-remove node->value node-id))
  (struct-copy Region region [node->value node->value^]))

(provide rvsdg-raw/remove-node-value)
