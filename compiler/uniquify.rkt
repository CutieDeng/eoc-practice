#lang racket

(require "core/core-types.rkt")
(require "program-default.rkt")
(require "control-flow-graph/var-id-reassign.rkt")
(require cutie-ftree)

(define pass-uniquify
  (class (pass-var-id-reassign-mixin pass-program 16)
    (super-new)
    (inherit-field var-cnt)
    (inherit gen-var-id)
    (field [env (ordl-make-empty integer-compare)] [var-id->symbol (ordl-make-empty integer-compare)] [origin-var-id->symbol #f])
    (define/override (pass p)
      (define info (Program-info p))
      (set! origin-var-id->symbol (dict-ref info 'var-id->symbol))
      (define p^ (super pass p))
      (Program (dict-set* (Program-info p^) 'var-cnt var-cnt 'var-id->symbol var-id->symbol) (Program-body p^))
    )
    (define/override (pass-exp exp)
      (define e env)
      (match exp
        [(Var x) (Var (dict-ref e x))]
        [(Let x exp body)
          (define exp^ (pass-exp exp))
          (define x^ (gen-var-id))
          (set! env (dict-set e x x^))
          (define s^
            (dict-ref origin-var-id->symbol x^ #f))
          (cond [s^ (set! var-id->symbol (dict-set var-id->symbol x s^))])
          (define body^ (pass-exp body))
          (set! env e)
          (Let x^ exp^ body^)
        ]
        [(SetBang var rhs)
          (SetBang (dict-ref e var) (pass-exp rhs))]
        [(GetBang var)
          (GetBang (dict-ref e var))]
        [_ (super pass-exp exp)]
      )
    )
  ))

(provide pass-uniquify)
