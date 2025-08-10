#lang racket

(require "core/core-types.rkt")
(require "graph-core.rkt")
(require "core/integer-set.rkt")
(require cutie-ftree)

(define (interference-cmp-fn lhs rhs)
  (match* (lhs rhs)
    [((Var _) (Reg _)) '>]
    [((Reg _) (Var _)) '<]
    [((Var l) (Var r)) (integer-compare l r)]
    [((Reg l) (Reg r)) (integer-compare l r)]
  )
)

(define interference
  (class object%
    (super-new)
    (field [i-graph (graph-make-empty-raw integer-compare)])
    (define/public (add-edges write-block live-block)
      (for ([i (in-range (sub1 (ral-length live-block)))])
        (define w (ral-ref write-block i))
        (define r (ral-ref live-block (+ i 1)))
        (define i-graph^ (for*/fold ([g i-graph]) ([wi (in-bset w)] [ri (in-bset (bset-subtract r w))])
          (add-edge (add-vertex (add-vertex g wi) ri) wi ri) 
        ))
        (set! i-graph i-graph^) 
      )
    )
    (define/public (add-vertexes vertexes)
      (set! i-graph (for/fold ([g i-graph]) ([v (in-bset vertexes)]) (add-vertex g v)))
    )
    (define/public (solve write-block live-block)
      (for ([i (in-range (ral-length write-block))])
        (define w (ral-ref write-block i))
        (define r (ral-ref live-block (+ i 1)))
        (define i^ (for*/fold ([i i-graph]) ([wi (in-bset w)] [ri (in-bset (bset-subtract r w))])
          (add-edge i wi ri) 
        ))
        (set! i-graph i^)
      )
    )
  ))

(provide interference interference-cmp-fn)
