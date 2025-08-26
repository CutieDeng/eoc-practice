#lang racket/base

(require racket/match racket/dict)
(require cutie-ftree)

(require "core-def.rkt")
(require "raw/node-ctor.rkt")
(require "raw/node-ctor2.rkt")
(require "raw/wire.rkt")
(require "connect.rkt")
(require "raw/node-dtor.rkt")

(define (rvsdg/free-nodes region node-ids)
  (for/fold ([r region]) ([n (in-ral0 node-ids)])
    (rvsdg/free-node r n))
)
(provide rvsdg/free-nodes)

(define (rvsdg/free-node region node-id)
  (match-define (cons input-id input-cnt) (dict-ref (Region-node->input region) node-id))
  (define region^
    (for/fold ([r region]) ([i (in-range input-cnt)] #:do [(define c-input-id (rvsdg-raw/input-offset input-id i))])
      (rvsdg/input-disconnect r c-input-id)
    ))
  (match-define (cons output-id output-cnt) (dict-ref (Region-node->output region) node-id))
  (define region^^
    (for/fold ([r region^]) ([i (in-range output-cnt)] #:do [(define c-output-id (rvsdg-raw/output-offset output-id i))])
      (rvsdg/output-disconnect r c-output-id)
    ))
  (rvsdg-raw/free-node region^^ node-id)
)
(provide rvsdg/free-node)
