#lang racket/base

(require racket/match racket/dict)

(require "core-def.rkt")

(define (rvsdg-raw/node-offset id index) (match id [(NodeId x) (NodeId (+ x index))]))
(provide rvsdg-raw/node-offset)

(define (rvsdg-raw/alloc-node-ids region [cnt 1])
  (define node-id (NodeId (Region-node-cnt region)))
  (define region^ (struct-copy Region region [node-cnt (+ node-id cnt)]))
  (values node-id region^)
)
(provide rvsdg-raw/alloc-node-ids)

(define (rvsdg-raw/node-input-set region node-id input-id input-cnt)
  (define node->input (Region-node->input region))
  (define input->node (Region-input->node region))
  (define node->input^ (dict-set node->input node-id (cons input-id input-cnt)))
  (define input->node^ (dict-set input->node input-id node-id))
  (define region^ (struct-copy Region region [node->input node->input^] [input->node input->node^]))
  region^
)
(provide rvsdg-raw/node-input-set)

(define (rvsdg-raw/node-output-set region node-id output-id output-cnt)
  (define node->output (Region-node->output region))
  (define output->node (Region-output->node region))
  (define node->output^ (dict-set node->output node-id (cons output-id output-cnt)))
  (define output->node^ (dict-set output->node output-id node-id))
  (define region^ (struct-copy Region region [node->output node->output^] [output->node output->node^]))
  region^
)
(provide rvsdg-raw/node-output-set)

(define (rvsdg-raw/node-input-output-set region node-id input-id input-cnt output-id output-cnt)
  (define region^ (rvsdg-raw/node-input-set region node-id input-id input-cnt)) 
  (define region^^ (rvsdg-raw/node-output-set region^ node-id output-id output-cnt))
  region^^
)
(provide rvsdg-raw/node-input-output-set)
