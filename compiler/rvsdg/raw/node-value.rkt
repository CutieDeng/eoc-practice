#lang racket/base

(require "../core-def.rkt")

(require racket/dict)

(define (rvsdg-raw/set-node-value region node-id value)
  (define node->value (Region-node->value region))
  (define node->value^ (dict-set node->value node-id value))
  (struct-copy Region region [node->value node->value^])
)
(provide rvsdg-raw/set-node-value)
