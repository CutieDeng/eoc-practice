#lang racket/base

(require racket/match racket/dict)
(require cutie-ftree)

(require "core-def.rkt")
(require "raw/node-ctor.rkt")
(require "raw/node-ctor2.rkt")
(require "raw/wire.rkt")
(require "raw/connect.rkt")

(define (rvsdg/input-disconnect region input-id)
  (define wire-id (dict-ref (Region-input->wire region) input-id))
  (define output-id (dict-ref (Region-wire->output region) wire-id))
  (rvsdg-raw/wire-input-output-disconnect region wire-id input-id output-id)
)
(provide rvsdg/input-disconnect)

(define (rvsdg/output-disconnect region output-id)
  (define wire-id (dict-ref (Region-output->wire region) output-id))
  (define input-id (dict-ref (Region-wire->input region) wire-id))
  (rvsdg-raw/wire-input-output-disconnect region wire-id input-id output-id)
)
(provide rvsdg/output-disconnect)
