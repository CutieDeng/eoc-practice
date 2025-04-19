#lang racket

(require "../utilities.rkt")
(require "graph-core.rkt")
(require cutie-ftree)

(define (interference-cmp-fn lhs rhs)
  (match* (lhs rhs)
    [((Var _) (Reg _)) 'lt]
    [((Reg _) (Var _)) 'gt]
    [((Var l) (Var r)) (symbol-compare l r)]
    [((Reg l) (Reg r)) (symbol-compare l r)]
  )
)

(define interference
  (class object%
    (super-new)
    (field [i-graph (graph-make-empty-raw interference-cmp-fn)])
    (define/public (solve-weak write-block live-block)
      (for ([i (in-range (sub1 (ral-length live-block)))])
        (define w (ral-ref write-block i))
        (define r (ral-ref live-block (+ i 1)))
        (define g (for*/fold ([i i-graph]) ([wi (in-set w)] [ri (in-set (set-subtract r w))])
          (add-edge (add-vertex (add-vertex i wi) ri) wi ri) 
        ))
        (set! i-graph g) 
      )
    )
    (define/public (add-vertexes vertexes)
      (set! i-graph (for/fold ([g i-graph]) ([v (in-set vertexes)]) (add-vertex g v)))
    )
    (define/public (solve write-block live-block)
      (for ([i (in-range (ral-length write-block))])
        (define w (ral-ref write-block i))
        (define r (ral-ref live-block (+ i 1)))
        (define i^ (for*/fold ([i i-graph]) ([wi (in-set w)] [ri (in-set (set-subtract r w))])
          (add-edge i wi ri) 
        ))
        (set! i-graph i^)
      )
    )
  ))

(provide interference interference-cmp-fn)
