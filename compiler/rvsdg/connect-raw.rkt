#lang racket/base

(require racket/match racket/dict)

(require "core-def.rkt")

(define (rvsdg-raw/wire-offset id index) (match id [(WireId x) (WireId (+ x index))]))
(provide rvsdg-raw/wire-offset)

(define (rvsdg-raw/wire-input-connect region wire-id input-id)
  (define input->wire (Region-input->wire region))
  (define wire->input (Region-wire->input region))
  (define input->wire^ (dict-set input->wire input-id wire-id))
  (define wire->input^ (dict-set wire->input wire-id input-id))
  (struct-copy Region region [input->wire input->wire^] [wire->input wire->input^])
)
(provide rvsdg-raw/wire-input-connect)

(define (rvsdg-raw/wire-output-connect region wire-id output-id)
  (define output->wire (Region-output->wire region))
  (define wire->output (Region-wire->output region))
  (define output->wire^ (dict-set output->wire output-id wire-id))
  (define wire->output^ (dict-set wire->output wire-id output-id))
  (struct-copy Region region [output->wire output->wire^] [wire->output wire->output^])
)
(provide rvsdg-raw/wire-output-connect)

(define (rvsdg-raw/wire-input-output-connect region wire-id input-id output-id)
  (define region^ (rvsdg-raw/wire-input-connect region wire-id input-id))
  (rvsdg-raw/wire-output-connect region^ wire-id output-id)
)
(provide rvsdg-raw/wire-input-output-connect)

(define (rvsdg-raw/alloc-wire-ids region [cnt 1])
  (define wire-id (WireId (Region-wire-cnt region)))
  (define region^ (struct-copy Region region [wire-cnt (+ wire-id cnt)]))
  (values wire-id region^)
)
(provide rvsdg-raw/alloc-wire-ids)

(define (rvsdg-raw/wire-input-disconnect region wire-id input-id)
  (define input->wire (Region-input->wire region))
  (define wire->input (Region-wire->input region))
  (define input->wire^ (dict-remove input->wire input-id))
  (define wire->input^ (dict-remove wire->input wire-id))
  (struct-copy Region region [input->wire input->wire^] [wire->input wire->input^])
)
(provide rvsdg-raw/wire-input-disconnect)

(define (rvsdg-raw/wire-output-disconnect region wire-id output-id)
  (define output->wire (Region-output->wire region))
  (define wire->output (Region-wire->output region))
  (define output->wire^ (dict-remove output->wire output-id))
  (define wire->output^ (dict-remove wire->output wire-id))
  (struct-copy Region region [output->wire output->wire^] [wire->output wire->output^])
)
(provide rvsdg-raw/wire-output-disconnect)

(define (rvsdg-raw/wire-input-output-disconnect region wire-id input-id output-id)
  (define region^ (rvsdg-raw/wire-input-disconnect region wire-id input-id))
  (rvsdg-raw/wire-output-disconnect region^ wire-id output-id)
)
(provide rvsdg-raw/wire-input-output-disconnect)

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

(define (rvsdg-raw/wire-input-reconnect region wire-id new-input-id old-input-id)
  (define region^ (rvsdg-raw/wire-input-disconnect region wire-id old-input-id))
  (define region^^ (rvsdg-raw/wire-input-connect region^ wire-id new-input-id))
  region^^
)
(provide rvsdg-raw/wire-input-reconnect)

(define (rvsdg-raw/wire-output-reconnect region wire-id new-output-id old-output-id)
  (define region^ (rvsdg-raw/wire-output-disconnect region wire-id old-output-id))
  (define region^^ (rvsdg-raw/wire-output-connect region^ wire-id new-output-id))
  region^^
)
(provide rvsdg-raw/wire-output-reconnect)

(define (rvsdg-raw/wire-input-output-reconnect region wire-id new-input-id old-input-id new-output-id old-output-id)
  (define region^ (rvsdg-raw/wire-input-output-disconnect region wire-id old-input-id old-output-id))
  (define region^^ (rvsdg-raw/wire-input-output-connect region^ wire-id new-input-id new-output-id))
  region^^
)
(provide rvsdg-raw/wire-input-output-reconnect)

(define (rvsdg-raw/input-wire-reconnect region input-id new-wire-id old-wire-id)
  (define region^ (rvsdg-raw/wire-input-disconnect region old-wire-id input-id))
  (define region^^ (rvsdg-raw/wire-input-connect region^ new-wire-id input-id))
  region^^
)
(provide rvsdg-raw/input-wire-reconnect)

(define (rvsdg-raw/output-wire-reconnect region output-id new-wire-id old-wire-id)
  (define region^ (rvsdg-raw/wire-output-disconnect region old-wire-id output-id))
  (define region^^ (rvsdg-raw/wire-output-connect region^ new-wire-id output-id))
  region^^
)
(provide rvsdg-raw/output-wire-reconnect)
