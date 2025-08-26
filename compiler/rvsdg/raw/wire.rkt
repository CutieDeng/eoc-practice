#lang racket/base

(require racket/match racket/dict)

(require "../core-def.rkt")

(define (rvsdg-raw/wire-offset id index) (match id [(WireId x) (WireId (+ x index))]))
(provide rvsdg-raw/wire-offset)

(define (rvsdg-raw/alloc-wire-ids region [cnt 1])
  (define wire-id (WireId (Region-wire-cnt region)))
  (define region^ (struct-copy Region region [wire-cnt (+ wire-id cnt)]))
  (values wire-id region^)
)
(provide rvsdg-raw/alloc-wire-ids)
