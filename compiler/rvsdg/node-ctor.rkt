#lang racket/base

(require racket/match)

(require "core-def.rkt")
(require "node-ctor-raw.rkt")
(require "connect-raw.rkt")

(define (rvsdg/alloc-node region input-cnt output-cnt)
  (define-values (node-id region^) (rvsdg-raw/alloc-node-ids region 1))
  (define-values (input-id output-id region^^) (rvsdg-raw/alloc-input-output-ids region^ input-cnt output-cnt))
  (define region^^^ (rvsdg-raw/node-input-output-set region^^ node-id input-id input-cnt output-id output-cnt))
  (values node-id input-id output-id region^^^)
)
(provide rvsdg/alloc-node)
