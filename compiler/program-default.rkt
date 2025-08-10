#lang racket

(require "core/core-types.rkt")
(require cutie-ftree)

(define pass-program
  (class object%
    (super-new)
    (define/public (pass program) (match program [(Program info body)
        (Program info (pass-exp body))
    ]))
    (define/public (pass-exp exp) (match exp
      [(HasType e types)
        (define e^ (pass-exp e))
        (if (eq? e e^) exp (HasType e^ types))]
      [(Prim o es)
        (define-values (change? rst) (for/fold ([change? #f] [rst '()]) ([e es]) (define e^ (pass-exp e))
          (values (or change? (not (eq? e e^))) (cons e^ rst))))
        (if change? (Prim o (reverse rst)) exp)]
      [(If cnd thn els)
        (define cnd^ (pass-exp cnd))
        (define thn^ (pass-exp thn))
        (define els^ (pass-exp els))
        (if (and (eq? cnd cnd^) (eq? thn thn^) (eq? els els^)) exp (If cnd^ thn^ els^))
      ]
      [(Begin es e)
        (define-values (change? rst) (for/fold ([change? #f] [rst (ral-empty)]) ([e (in-ral0 es)]) (define e^ (pass-exp e))
          (values (or change? (not (eq? e e^))) (ral-consr rst e^))))
        (define e^ (pass-exp e))
        (if (and (not change?) (eq? e e^)) exp (Begin rst e^))
      ]
      [(Let x rhs body)
        (define rhs^ (pass-exp rhs))
        (define body^ (pass-exp body))
        (if (and (eq? rhs rhs^) (eq? body body^)) exp (Let x rhs^ body^))
      ]
      [(SetBang var rhs) 
        (define rhs^ (pass-exp rhs))
        (if (eq? rhs rhs^) exp (SetBang var rhs^))]
      [(WhileLoop cnd body)
        (define cnd^ (pass-exp cnd))
        (define body^ (pass-exp body))
        (if (and (eq? cnd cnd^) (eq? body body^)) exp (WhileLoop cnd^ body^))]
      [(GetBang v)
        (define v^ (pass-exp v))
        (if (eq? v v^) exp (GetBang v^))
      ]
      [_ exp]
    ))
  ))

(provide pass-program)
