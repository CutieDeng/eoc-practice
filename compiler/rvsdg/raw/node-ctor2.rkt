#lang racket/base

(require racket/match racket/dict)

(require "../core-def.rkt")

(define (rvsdg-raw/alloc-input-ids region [cnt 1])
  (define input-id (InputId (Region-input-cnt region)))
  (define region^ (struct-copy Region region [input-cnt (+ input-id cnt)]))
  (values input-id region^)
)
(provide rvsdg-raw/alloc-input-ids)

(define (rvsdg-raw/alloc-output-ids region [cnt 1])
  (define output-id (OutputId (Region-output-cnt region)))
  (define region^ (struct-copy Region region [output-cnt (+ output-id cnt)]))
  (values output-id region^)
)
(provide rvsdg-raw/alloc-output-ids)

(define (rvsdg-raw/alloc-input-output-ids region input-cnt output-cnt)
  (define-values (input-id region^) (rvsdg-raw/alloc-input-ids region input-cnt))
  (define-values (output-id region^^) (rvsdg-raw/alloc-output-ids region^ output-cnt))
  (values input-id output-id region^^)
)
(provide rvsdg-raw/alloc-input-output-ids)
