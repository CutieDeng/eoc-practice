#lang racket

(require "core/utilities.rkt" "core/core-types.rkt")
(require "core/integer-set.rkt")
(require "graph-core.rkt")
(require cutie-ftree)

(define connect-component
  (class object%
    (super-new)
    (field
      [id2time (ordl-make-empty integer-compare)]
      [time2id (ordl-make-empty integer-compare)]
      [cur-time 0]
      [time2set (ordl-make-empty integer-compare)]
      [agraph (graph-make-empty)]
      [graph #f]
      [visit (bset)]
      [group2id (ordl-make-empty integer-compare)]
    )
    (define/private (cur-time!)
      (begin0 cur-time (set! cur-time (+ cur-time 1))))
    (define/private (bind-id-time id tim)
      (set! id2time (dict-set id2time id tim))
      (set! time2id (dict-set time2id tim id))
    )
    (define/private (time2set-find t)
      (define f (dict-ref time2set t #f))
      (cond [(and f (equal? f t)) f] [f (time2set-find f)]
        [else (set! time2set (dict-set time2set t t)) t])
    )
    (define/private (time2set-join:impl lhs rhs)
      (dict-set time2set rhs lhs) lhs)
    (define/private (time2set-join lhs rhs)
      (cond [(< lhs rhs) (time2set-join:impl lhs rhs)] [else (time2set-join:impl rhs lhs)]))
    (define/private (in-visit-stack? id)
      (bset-member? visit id))
    (define/private (group-cast bb-id)
      (dict-ref time2id (time2set-find (dict-ref id2time bb-id)))
    )

    (define/private (init-nodes)
      (for ([bb-id (in-vertices graph)])
        (init-node bb-id))
    )
    (define/private (flushes)
      (for ([bb-id (in-vertices graph)])
        (time2set-find (dict-ref id2time bb-id))
      )
    )
    (define/private (build-agraph)
      (define agraph^
        (for/fold ([g agraph]) ([bb-id (in-vertices graph)])
          (add-vertex g (group-cast bb-id))))
      (define agraph^^
        (for/fold ([g agraph^]) (
          [bb-u-id (in-vertices graph)]
          #:do [(define bb-u-group (group-cast bb-u-id))]
          [bb-v-id (in-neighbors graph bb-u-id)]
          #:do [(define bb-v-group (group-cast bb-v-id))])
            (add-directed-edge g bb-u-group bb-v-group)
        ))
      (set! agraph agraph^^)
    )
    (define/private (build-group2id)
      (define empty-set (bset))
      (define g2i^
        (for/fold ([g2i group2id]) ([u (in-vertices graph)])
          (define g (group-cast u))
          (define cur-g (dict-ref g2i g empty-set))
          (define cur-g^ (bset-add cur-g u))
          (dict-set g2i g cur-g^)
        ))
      (set! group2id g2i^)
    )
    ; (public agraph group2id)
    (define/public (get-connect-component graph)
      (set-field! graph this graph)
      (init-nodes)
      ; (flushes)
      (build-agraph)
      (build-group2id)
    )
    (define/private (init-node bb-id)
      (define bb-tim (dict-ref id2time bb-id #f))
      (cond
        [bb-tim (void)]
        [else
          (set! bb-tim (cur-time!))
          (bind-id-time bb-id bb-tim)
          (set! visit (bset-add visit bb-tim))
          (define cur-min-tim^
            (for/fold ([cur-min-tim bb-tim]) ([nxt-bb-id (in-neighbors graph bb-id)])
              (init-node nxt-bb-id)
              (define nxt-bb-tim (dict-ref id2time nxt-bb-id))
              (define nxt-bb-tim-p (time2set-find nxt-bb-tim))
              (cond
                [(bset-member? visit nxt-bb-tim-p) (time2set-join nxt-bb-tim-p cur-min-tim)]
                [else cur-min-tim]
              )
            ))
          (set! visit (bset-remove visit bb-tim))
        ]
      )
    )
  )
)

(provide connect-component)
