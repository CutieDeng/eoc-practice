#lang racket

(require "../utilities.rkt")
(require "graph-core.rkt")
(require cutie-ftree)

(define pass-dominance
  (class object%
    (super-new)
    (field [domin (ordl-make-empty symbol-compare)])
    (define/public (pass p) (match p [(X86Program info blocks)
      (define full-set (list->set (dict-keys blocks)))
      (for ([block full-set]) (set! domin (dict-set domin block full-set)))
      (define connect-component (dict-ref info 'connect-component))
      (define graph (dict-ref info 'graph))
      (define graph-t (transpose graph))
      (for ([u (in-vertices graph-t)])
        (define v (get-neighbors graph-t u))
        (when (ral-empty? v) 
          (set! domin (dict-set domin u (set u)))
        )
      )
      (define id2block (dict-ref info 'id2block))
      (for ([component (in-ral0 connect-component)]) 
        (define (loop hint)
          (for ([blockid (in-dict-keys component)])
            (define block (dict-ref id2block blockid))
            (define dominance^
              (for/fold ([collect (dict-ref domin block)]) ([src (in-neighbors graph-t block)])
                (set-intersect collect (dict-ref domin src))
              ))
            (define dominance^^ (set-union dominance^ (set block)))
            (define dominance-old (dict-ref domin block))
            (unless (equal? dominance^^ dominance-old)
              (set! hint #t)
              (set! domin (dict-set domin block dominance^^))
            )
          )
          (when hint (loop #f))
        )
        (loop #f)
      )
      p
    ]))
  ))

(provide pass-dominance)

