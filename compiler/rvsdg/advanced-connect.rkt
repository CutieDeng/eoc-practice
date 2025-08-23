#lang racket/base

(require racket/match racket/dict)
(require cutie-ftree)

(require "core-def.rkt")
(require "ctor.rkt")
(require "connect-raw.rkt")
(require "node-ctor.rkt")
(require "input-raw.rkt")
(require "output-raw.rkt")
(require "advanced-connect-raw.rkt")

; wires: ral
(define (rvsdg/split-wires-with-node region wires)
  (define wire->input (Region-wire->input region))
  (define wire->output (Region-wire->output region))
  (define-values (inputs outputs)
    (for/fold ([inputs (ral-empty)] [outputs (ral-empty)]) ([w (in-ral0 wires)])
      (values (ral-consr inputs (dict-ref wire->input w)) (ral-consr outputs (dict-ref wire->output w)))
    ))
  (rvsdg-raw/split-wires-with-node region wires inputs outputs)
)
(provide rvsdg/split-wires-with-node)
