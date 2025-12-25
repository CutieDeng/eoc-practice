#lang racket/base

;; ============================================================
;; Raw Layer: Wire ID 分配
;; ============================================================

(require racket/match racket/dict)
(require "../core-def.rkt")

;; === Wire ID 偏移计算 ===

(define (rvsdg-raw/wire-offset id index)
  (match id [(WireId x) (WireId (+ x index))]))

(provide rvsdg-raw/wire-offset)

;; === Wire ID 分配 ===

(define (rvsdg-raw/alloc-wire-ids region [cnt 1])
  (define current-cnt (Region-wire-cnt region))
  (define wire-id (WireId current-cnt))
  (define region^ (struct-copy Region region [wire-cnt (+ current-cnt cnt)]))
  (values wire-id region^))

(provide rvsdg-raw/alloc-wire-ids)
