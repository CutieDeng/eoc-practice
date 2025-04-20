#lang racket

(require "../utilities.rkt")
(require "graph-core.rkt")
(require cutie-ftree)

(require "analyze-dataflow.rkt")

(define pass-dominance
  (class object%
    (super-new)
    (define/public (pass p) (match p [(X86Program info blocks)
      (define bottom (list->set (dict-keys blocks)))
      (define ana (new analyzer))
      (define graph (dict-ref info 'graph))
      (define id2block (dict-ref info 'id2block))
      (define components (dict-ref info 'connect-component))
      (send ana analyze-dataflow
        graph
        (λ (node input) (set-add input node))
        bottom
        set-intersect
        components
        (λ (i) (dict-ref id2block i))
        (set)
      )
      (define t (get-field result ana))
      (define info^ (dict-set info 'dominanced t))
      (X86Program info^ blocks)
    ]))
  ))

(provide pass-dominance)
