#lang racket/base

(require racket/match racket/dict)

(require "../core-def.rkt")

(define (rvsdg-raw/free-node region node-id)
  (match-define (cons input-id _) (dict-ref (Region-node->input node-id)))
  (match-define (cons output-id _) (dict-ref (Region-node->output node-id)))
  (define node->input (dict-remove (Region-node->input region) node-id))
  (define node->output (dict-remove (Region-node->output region) node-id))
  (define input->node (dict-remove (Region-input->node region) input-id))
  (define output->node (dict-remove (Region-output->node region) output-id))
  (struct-copy Region region
    [node->input node->input] [node->output node->output]
    [input->node input->node] [output->node output->node])
)
(provide rvsdg-raw/free-node)
