#lang racket

(require "core/utilities.rkt")
(require "core/core-types.rkt")
(require "graph-core.rkt")
(require "core/integer-set.rkt")
(require cutie-ftree)

(define analyzer
  (class object%
    (super-new)
    (field
      [analyze-value (ordl-make-empty integer-compare)]

      [change-set #f]
      [change-allowed #f]
      [worklist #f]

      [graph #f]
      [graph-t #f]
      [transfer #f]
      [bottom #f]
      [join #f]
      [fn #f]
      [default #f])
    (define/private (init-analyze-value)
      (define analyze-value^
        (for/fold ([a analyze-value]) ([i (in-vertices graph-t)])
          (dict-set a i (transfer i bottom))))
      (set! analyze-value analyze-value^)
    )
    (define/private (analyze-graph-batch:impl)
      (cond [(ral-empty? worklist) (void)]
      [else
        (define-values (bb-id worklist^) (ral-dropr worklist))
        (set! change-set (bset-remove change-set bb-id))
        (set! worklist worklist^)
        (define pred-ids (get-neighbors graph-t bb-id))
        (define bb-input (match pred-ids
          [(ral) default]
          [_
            (for/fold ([s bottom]) ([p-id (in-ral0 pred-ids)])
              (join s (dict-ref analyze-value p-id))
            )
          ]))
        (define bb-output (transfer bb-id bb-input))
        (cond
          [(equal? bb-output (dict-ref analyze-value bb-id)) (void)]
          [else
            (set! analyze-value (dict-set analyze-value bb-id bb-output))
            (for (
              [succ-id (in-neighbors graph bb-id)]
              #:when (bset-member? change-allowed succ-id)
              #:when (not (bset-member? change-set succ-id)))
              (set! change-set (bset-add change-set succ-id))
              (set! worklist (ral-consr worklist succ-id))
            )
          ])
        (analyze-graph-batch:impl)
      ])
    )
    (define/private (analyze-graph-batch batch)
      (define-values (c^ w^)
        (for/fold ([c 0] [w (ral-empty)]) ([i (in-bset batch)] #:do [(define j (fn i))])
          (values (bset-add c j) (ral-consr w j))
        ))
      (set! change-set c^)
      (set! change-allowed c^)
      (set! worklist w^)
      (analyze-graph-batch:impl)
    )
    (define/public (analyze-dataflow graph transfer bottom join batches fn default)
      (set-field! graph this graph)
      (set-field! graph-t this (transpose graph))
      (set-field! transfer this transfer)
      (set-field! bottom this bottom)
      (set-field! join this join)
      (set-field! fn this fn)
      (set-field! default this default)
      (init-analyze-value)
      (for ([b (in-ral0 batches)])
        (analyze-graph-batch b))
    )
  ))

(provide analyzer)
