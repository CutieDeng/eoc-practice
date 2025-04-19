#lang racket

(require "../utilities.rkt")
(require "program-default.rkt")
(require cutie-ftree)

(define pass-uniquify
  (class pass-program
    (super-new)
    (field [env (ordl-make-empty symbol-compare)])
    (define/override (pass-exp exp)
      (define e env)
      (match exp
        [(Var x) (Var (dict-ref e x))]
        [(Let x exp body)
          (define exp^ (pass-exp exp))
          (define x^ (gensym x))
          (set! env (dict-set e x x^))
          (define body^ (pass-exp body))
          (set! env e)
          (Let x^ exp^ body^)
        ]
        [(SetBang var rhs)
          (SetBang (dict-ref e var) (pass-exp rhs))]
        [_ (super pass-exp exp)]
      )
    )
  ))

(provide pass-uniquify)
