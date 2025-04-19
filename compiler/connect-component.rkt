#lang racket

(require "../utilities.rkt")
(require "graph-core.rkt")
(require cutie-ftree)

(require "topology-sort.rkt")
(require "connect-component-core.rkt")
(require "x86-cfg.rkt")

(define pass-uncover-live
  (class object%
    (super-new)
    (define/public (pass p) (match p
      [(X86Program info blocks)
        (define control-flow-graph (send (new x86-control-flow-graph) build-cfg p))
        (define cc (new connect-component))
        (send cc get-connect-component control-flow-graph)
        (define info^ (dict-set info 'graph control-flow-graph))
        (define info^^ (dict-set info^ 'connect-component (torder cc)))
        (define info^^^ (dict-set info^^ 'id2block (get-field id2block cc)))
        (X86Program info^^^ blocks)
      ]
    ))
    ; listof dict
    (define (torder cc)
      (define dag (get-field directed-acyclic-graph cc))
      (define group2id (get-field group2id cc))
      (define torders (topology-sort dag))
      (for/fold ([to (ral-empty)]) ([t (in-ral0 torders)])
        (ral-consr to (dict-ref group2id t))
      )
    )
  )
)

(provide pass-uncover-live)
