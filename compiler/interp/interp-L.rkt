#lang racket
(require racket/fixnum)
(require racket/dict)
(require "../core/core-types.rkt")

(require cutie-ftree)

(define interp-L-class (class object%
  (super-new)
  (field [env (make-parameter #f)])
  (define/public (interp-exp e)
    (match e
      [(Var x) (dict-ref (env) x)]
      [(Let x e body)
        (define env^ (dict-set (env) x e))
        (parameterize ([env env^]) (interp-exp body))
      ]
      [(Int n) n]
      [(Prim 'read '())
        (define r (read))
        (cond 
          [(fixnum? r) r]
          [else (error 'interp-exp "expected an integer" r)])]
      [(Prim '- `(,e))
        (define v (interp-exp e))
        (fx- 0 v)]
      [(Prim '+ `(,e1 ,e2))
        (define v1 (interp-exp e1))
        (define v2 (interp-exp e2))
        (fx+ v1 v2)]
      [(Prim '- `(,e1 ,e2))
        (define v1 (interp-exp e1))
        (define v2 (interp-exp e2))
        (fx- v1 v2)]
      
    )
  )
  (define (interp p) (match p [(Program _ e) (interp-exp e)]))
))

(provide interp-L-class)
