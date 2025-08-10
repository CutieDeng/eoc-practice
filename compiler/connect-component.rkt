#lang racket

(require "core/core-types.rkt")
(require "core/utilities.rkt")
(require "graph-core.rkt")
(require cutie-ftree)

(require "topology-sort.rkt")
(require "connect-component-core-2.rkt")
(require "x86-control-flow-graph.rkt")

(define pass-connect-component
  (class object%
    (super-new)
    (define/public (pass p) (match p
      [(X86Program info blocks)
        (define control-flow-graph (send (new x86-control-flow-graph) build-cfg p))
        (define cc (new connect-component))
        (send cc get-connect-component control-flow-graph)
        (define alist (topology-sort (get-field agraph cc)))
        (define group2id (get-field group2id cc))
        (define alist-connect-component (for/fold ([c (ral-empty)]) ([a (in-ral0 alist)])
          (ral-consr c (dict-ref group2id a))))
        (define info^ (dict-set* info 
          'graph control-flow-graph
          'agraph (get-field agraph cc)
          'topology-order alist
          'group2id group2id
          'connect-component alist-connect-component))
        (X86Program info^ blocks)
      ]
    ))
  )
)

(provide pass-connect-component)
