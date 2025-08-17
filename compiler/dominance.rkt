#lang racket

(require "core/core-types.rkt" "core/utilities.rkt" "core/integer-set.rkt")
(require "graph-core.rkt")
(require cutie-ftree)

(require "analyze-dataflow.rkt")

(define pass-dominance
  (class object%
    (super-new)
    (define/public (pass p) (match p [(X86Program info blocks)
      (define full-bb-set (bset* (dict-keys blocks)))
      (define empty-bb-set (bset))
      (define (id-fn x) x)
      (define (transfer node input) (bset-add input node))
      (define ana (new analyzer))
      (define graph (dict-ref info 'graph))
      (define components (dict-ref info 'connect-component))
      (send ana analyze-dataflow
        graph
        transfer
        full-bb-set
        bset-and
        components
        id-fn
        empty-bb-set
      )
      (define t (get-field analyze-value ana))
      (define info^ (dict-set* info 'dominanced t))
      (X86Program info^ blocks)
    ]))
  ))

(provide pass-dominance)
