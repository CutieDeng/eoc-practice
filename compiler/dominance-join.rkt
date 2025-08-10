#lang racket

(require "core/utilities.rkt")
(require "core/core-types.rkt")
(require "core/integer-set.rkt")
(require "graph-core.rkt")
(require cutie-ftree)

(require "debug0.rkt")

(define dominance-dj-graph
  (class object%
    (super-new)
    (field [dominanced #f] [dj-graph (graph-make-empty)])
    (define (domin? u v)
      (define v-domed (dict-ref dominanced v))
      (bset-member? v-domed u)
    )
    (define/public (build-dj-graph graph dominanced)
      (set-field! dominanced this dominanced)
      (for* ([u (in-vertices graph)] [v (in-neighbors graph u)])
        (unless (domin? u v)
          (set! dj-graph (add-directed-edge (add-vertex (add-vertex dj-graph u) v) u v))
        )
      )
    )
  ))

(define pass-dominance-dj-graph
  (class object%
    (super-new)
    (define/public pass (match-lambda [(X86Program info blocks)
      (define graph (dict-ref info 'graph))
      (define dominanced (dict-ref info 'dominanced))
      (define d (new dominance-dj-graph))
      (send d build-dj-graph graph dominanced)
      (define djgraph (get-field dj-graph d))
      (define info^ (dict-set* info 'dj-graph djgraph))
      ; (printf "=\n=\n=\n")
      ; (debug-dict domined)
      ; (printf "\t= end of dm =\n")
      ; (debug-graph graph)
      ; (printf "\t= end of graph =\n")
      ; (debug-graph djgraph)
      ; (printf "\t= end of dj graph =\n")
      (debug "dj-graph" info^)
      (X86Program info^ blocks)
    ]))
  ))

(provide dominance-dj-graph pass-dominance-dj-graph)
