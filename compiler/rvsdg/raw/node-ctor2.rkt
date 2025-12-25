#lang racket/base

;; ============================================================
;; Raw Layer: 输入/输出端口 ID 分配
;; ============================================================

(require racket/match racket/dict)
(require "../core-def.rkt")

;; === 输入端口 ID 分配 ===

(define (rvsdg-raw/alloc-input-ids region [cnt 1])
  (define current-cnt (Region-input-cnt region))
  (define input-id (InputId current-cnt))
  (define region^ (struct-copy Region region [input-cnt (+ current-cnt cnt)]))
  (values input-id region^))

(provide rvsdg-raw/alloc-input-ids)

;; === 输出端口 ID 分配 ===

(define (rvsdg-raw/alloc-output-ids region [cnt 1])
  (define current-cnt (Region-output-cnt region))
  (define output-id (OutputId current-cnt))
  (define region^ (struct-copy Region region [output-cnt (+ current-cnt cnt)]))
  (values output-id region^))

(provide rvsdg-raw/alloc-output-ids)

;; === 同时分配输入和输出端口 ID ===

(define (rvsdg-raw/alloc-input-output-ids region input-cnt output-cnt)
  (define-values (input-id region^) (rvsdg-raw/alloc-input-ids region input-cnt))
  (define-values (output-id region^^) (rvsdg-raw/alloc-output-ids region^ output-cnt))
  (values input-id output-id region^^))

(provide rvsdg-raw/alloc-input-output-ids)
