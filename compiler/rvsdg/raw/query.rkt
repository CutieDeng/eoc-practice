#lang racket/base

;; ============================================================
;; Raw Layer: 原生查询操作
;; ============================================================
;;
;; 提供对 Region 内部映射的直接访问
;; 这些是不安全的操作，不进行任何检查
;; ============================================================

(require racket/dict)
(require "../core-def.rkt")
(require "node-ctor.rkt")

;; === 直接访问映射 ===

;; 获取各映射的引用（返回 ordl）
(define (rvsdg-raw/wire->input-map region) (Region-wire->input region))
(define (rvsdg-raw/wire->output-map region) (Region-wire->output region))
(define (rvsdg-raw/input->wire-map region) (Region-input->wire region))
(define (rvsdg-raw/input->node-map region) (Region-input->node region))
(define (rvsdg-raw/output->wire-map region) (Region-output->wire region))
(define (rvsdg-raw/output->node-map region) (Region-output->node region))
(define (rvsdg-raw/node->input-map region) (Region-node->input region))
(define (rvsdg-raw/node->output-map region) (Region-node->output region))
(define (rvsdg-raw/node->value-map region) (Region-node->value region))

(provide rvsdg-raw/wire->input-map)
(provide rvsdg-raw/wire->output-map)
(provide rvsdg-raw/input->wire-map)
(provide rvsdg-raw/input->node-map)
(provide rvsdg-raw/output->wire-map)
(provide rvsdg-raw/output->node-map)
(provide rvsdg-raw/node->input-map)
(provide rvsdg-raw/node->output-map)
(provide rvsdg-raw/node->value-map)

;; === 单项查询 ===

;; 查询边的连接
(define (rvsdg-raw/wire-input region wire-id)
  (dict-ref (Region-wire->input region) wire-id #f))

(define (rvsdg-raw/wire-output region wire-id)
  (dict-ref (Region-wire->output region) wire-id #f))

;; 查询端口的连接
(define (rvsdg-raw/input-wire region input-id)
  (dict-ref (Region-input->wire region) input-id #f))

(define (rvsdg-raw/output-wire region output-id)
  (dict-ref (Region-output->wire region) output-id #f))

;; 查询端口的所属节点
(define (rvsdg-raw/input-node region input-id)
  (dict-ref (Region-input->node region) input-id #f))

(define (rvsdg-raw/output-node region output-id)
  (dict-ref (Region-output->node region) output-id #f))

;; 查询节点的端口
(define (rvsdg-raw/node-inputs region node-id)
  (dict-ref (Region-node->input region) node-id #f))

(define (rvsdg-raw/node-outputs region node-id)
  (dict-ref (Region-node->output region) node-id #f))

(provide rvsdg-raw/wire-input)
(provide rvsdg-raw/wire-output)
(provide rvsdg-raw/input-wire)
(provide rvsdg-raw/output-wire)
(provide rvsdg-raw/input-node)
(provide rvsdg-raw/output-node)
(provide rvsdg-raw/node-inputs)
(provide rvsdg-raw/node-outputs)

;; === 计数器查询 ===

(define (rvsdg-raw/wire-count region) (Region-wire-cnt region))
(define (rvsdg-raw/input-count region) (Region-input-cnt region))
(define (rvsdg-raw/output-count region) (Region-output-cnt region))
(define (rvsdg-raw/node-count region) (Region-node-cnt region))

(provide rvsdg-raw/wire-count)
(provide rvsdg-raw/input-count)
(provide rvsdg-raw/output-count)
(provide rvsdg-raw/node-count)

;; === 遍历获取所有 ID ===

;; 获取所有节点 ID（通过遍历 node->value 映射）
(define (rvsdg-raw/all-node-ids region)
  (dict-keys (Region-node->value region)))

;; 获取所有 Wire ID
(define (rvsdg-raw/all-wire-ids region)
  (dict-keys (Region-wire->input region)))

(provide rvsdg-raw/all-node-ids)
(provide rvsdg-raw/all-wire-ids)
