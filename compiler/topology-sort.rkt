#lang racket

(require cutie-ftree)
(require "graph-core.rkt")
(require "core/integer-set.rkt")

(define topology
  (class object%
    (super-new)
    (field [visit 0] [rst (ral-empty)] [graph #f])
    (define/public (order graph)
      (set-field! graph this graph)
      (for ([i (in-vertices graph)])
        (find i)
      )
      rst
    )
    (define/private (visit-add! i)
      (set! visit (bset-add visit i)) 
    )
    (define (find current-node)
      (cond
        [(bset-member? visit current-node) (void)]
        [else
          (visit-add! current-node)
          (for ([t (in-neighbors graph current-node)])
            (find t)
          )
          (set! rst (ral-consl rst current-node))
        ]
      )
    )
  ))

(define (topology-sort graph) (send (new topology) order graph))

(provide topology-sort)
