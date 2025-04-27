#lang racket

(require "../utilities.rkt")
(require "graph-core.rkt")
(require cutie-ftree)

(require "debug0.rkt")

(define dominance-dj-graph
  (class object%
    (super-new)
    (define (domin? u v domined)
      (define v-domed (dict-ref domined v))
      (set-member? v-domed u)
    )
    (field [dj (graph-make-empty)])
    (define/public (dj-graph graph domined)
      (for ([u (in-vertices graph)]) (for ([v (in-neighbors graph u)])
        (unless (domin? u v domined)
          (set! dj (add-directed-edge (add-vertex (add-vertex dj u) v) u v))
        )
      ))
    )
  ))

(define pass-dominance-dj-graph
  (class object%
    (super-new)
    (define/public (pass p) (match p [(X86Program info blocks)
      (define graph (dict-ref info 'graph))
      (define domined (dict-ref info 'dominanced))
      (define d (new dominance-dj-graph))
      (send d dj-graph graph domined)
      (define djgraph (get-field dj d))
      (define info^ (dict-set info 'dj-graph djgraph))
      ; (printf "=\n=\n=\n")
      ; (debug-dict domined)
      ; (printf "\t= end of dm =\n")
      ; (debug-graph graph)
      ; (printf "\t= end of graph =\n")
      ; (debug-graph djgraph)
      ; (printf "\t= end of dj graph =\n")
      (X86Program info^ blocks)
    ]))
  ))

(provide dominance-dj-graph pass-dominance-dj-graph)
