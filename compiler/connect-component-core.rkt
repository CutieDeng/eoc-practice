#lang racket

(require "../utilities.rkt")
(require "graph-core.rkt")
(require cutie-ftree)

(define connect-component
  (class object%
    (super-new)
    (field
      [block2id (ordl-make-empty symbol-compare)]
      [id2block (ordl-make-empty integer-compare)]
      [id-cnt 0]
      [id2group (ordl-make-empty integer-compare)]
      [directed-acyclic-graph (graph-make-empty-raw integer-compare)]
      [group2id (ordl-make-empty integer-compare)]
    )
    ; (public block2id id2block id-cnt id2group group2id directed-acyclic-graph)
    (define/public (get-connect-component graph)
      (init-nodes graph)
      (flushes)
      (build-directed-acyclic-graph graph)
      (get-group2id)
    )
    (define (init-nodes graph)
      (for ([n (in-vertices graph)])
        (init-node n graph (ordl-make-empty symbol-compare)))
    )
    (define (init-node node graph in-stack)
      (define node-id (dict-ref block2id node #f))
      (cond
        [node-id
          (cond
            [(dict-ref in-stack node #f) (dict-ref id2group node-id)]
            [else #f]
          )
        ]
        [else
          (define id id-cnt) (set! id-cnt (add1 id-cnt))
          (set! id2block (dict-set id2block id node))
          (set! block2id (dict-set block2id node id))
          (set! id2group (dict-set id2group id id))
          (set! in-stack (dict-set in-stack node #t))
          (define min-id (for/fold ([min-id id]) ([nxt-node (in-neighbors graph node)])
            (define nxt-node-group (init-node nxt-node graph in-stack))
            (match* (min-id nxt-node-group)
              [(_ #f) min-id]
              [(a b) (min a b)]
            )
          ))
          (cond 
            [min-id
              (set! id2group (dict-set id2group id min-id))
              min-id]
            [else id])
        ]
      )
    )
    (define (flushes)
      (for ([id (in-range id-cnt)])
        (flush id)
      )
    )
    (define (flush id)
      (define id^ (dict-ref id2group id))
      (cond
        [(equal? id id^) id]
        [else
          (define id^^ (flush id^))
          (set! id2group (dict-set id2group id id^^))
          id^^
        ]
      )
    )
    (define (build-directed-acyclic-graph graph)
      (for ([i (in-range id-cnt)]) (set! directed-acyclic-graph (add-vertex directed-acyclic-graph (dict-ref id2group i))))
      (for ([u (in-vertices graph)])
        (define u-id (dict-ref block2id u))
        (define u-group (dict-ref id2group u-id))
        (for ([v (in-neighbors graph u)])
          (define v-id (dict-ref block2id v))
          (define v-group (dict-ref id2group v-id))
          (unless (equal? u-group v-group)
            (set! directed-acyclic-graph (add-directed-edge directed-acyclic-graph u-group v-group))
          )
        )
      )
    )
    (define (get-group2id)
      (define empty-set (ordl-make-empty integer-compare))
      (define g (for/fold ([g2i (ordl-make-empty integer-compare)]) ([(i g) (in-dict id2group)])
        (define group (dict-ref g2i g empty-set))
        (set! group (dict-set group i #t))
        (dict-set g2i g group)
      ))
      (set! group2id g)
    )
  )
)

(provide connect-component)
