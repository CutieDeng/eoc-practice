#lang racket

(define (debug-dict d)
  (for ([(k v) (in-dict d)])
    (printf "~a = ~a\n" k v)
  )
  (printf "\n")
)

(provide debug-dict)

(require "graph-core.rkt")

(define (debug-graph g)
  (for ([u (in-vertices g)]) (for ([v (in-neighbors g u)])
    (printf "~a -> ~a\n" u v)
  ))
)

(provide debug-graph)

(require cutie-ftree)

(define (debug-con-seq s d)
  (printf "(\n")
  (for ([si (in-ral0 s)])
    (define n-display (for/list ([ki (in-dict-keys si)])
      (define node (dict-ref d ki))
      node
    ))
    (printf "\t~a\n" n-display)
  )
  (printf ")\n")
)

(provide debug-con-seq)

(define (debug-dag dag group2id id2block)
  (define cache (mutable-set))
  (printf "(\n")
  (define (field-content id)
    (define ids (dict-ref group2id id))
    (cond
      [(set-member? cache id)
        (match-define (cons k _) (ordl-max ids))
        (format "(~a ...)" k)
      ]
      [else
        (set-add! cache id)
        (format "~a" (map (lambda (i) (dict-ref id2block i)) (dict-keys ids)))
      ]
    )
  )
  (for ([u (in-vertices dag)]) (for ([v (in-neighbors dag u)]) 
    (printf "\t~a -> ~a\n" (field-content u) (field-content v))
  ))
  (printf ")\n")
)

(provide debug-dag)
