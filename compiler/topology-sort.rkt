#lang racket

(require cutie-ftree)
(require "graph-core.rkt")

(define topology
  (class object%
    (super-new)
    (field [visit #f] [rst #f])
    (define/public (order graph)
      (set! visit (mutable-set))
      (set! rst (ral-empty))
      (for ([i (in-vertices graph)])
        (find i graph)
      )
      rst
    )
    (define (find current-node graph)
      (cond
        [(set-member? visit current-node) (void)]
        [else
          (set-add! visit current-node)
          (for ([t (in-neighbors graph current-node)])
            (find t graph)
          )
          (set! rst (ral-consl rst current-node))
        ]
      )
    )
  ))

(define (topology-sort graph) (send (new topology) order graph))

(provide topology-sort)
