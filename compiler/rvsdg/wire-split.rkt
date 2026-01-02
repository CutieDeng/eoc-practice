#lang racket/base

(require racket/match racket/dict)
(require "../lib/ftree.rkt")

(require "core-def.rkt")
(require "raw/wire-split.rkt")

; wires: pvector
(define (rvsdg/split-wires-with-node region wires)
  (define wire->input (Region-wire->input region))
  (define wire->output (Region-wire->output region))
  (define-values (inputs outputs)
    (for/fold ([inputs (pvector-empty)] [outputs (pvector-empty)]) ([w (in-pvector wires)])
      (values (pvector-cons-right inputs (dict-ref wire->input w)) (pvector-cons-right outputs (dict-ref wire->output w)))
    ))
  (rvsdg-raw/split-wires-with-node region wires inputs outputs)
)
(provide rvsdg/split-wires-with-node)
