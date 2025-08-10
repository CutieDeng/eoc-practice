#lang racket

(require "core/core-types.rkt" "core/utilities.rkt")
(require "core/integer-set.rkt")
(require "program-default.rkt")

(define pass-uncover-get!-exp 
  (class pass-program
    (super-new)
    (field [set!-vars #f])
    (define/private (set!-var? x)
      (bset-member? set!-vars x)
    )
    (define/override (pass p) (match p [(Program info body)
      (set! set!-vars (dict-ref info 'set!))
      (super pass p)
    ]))
    (define/override (pass-exp exp) (match exp
      [(Var x) #:when (set!-var? x) (GetBang x)]
      [(SetBang var rhs)
        (define rhs^ (pass-exp rhs))
        (if (eq? rhs rhs^) exp (SetBang var rhs^))]
      [_ (super pass-exp exp)]
    ))
  ))

(provide pass-uncover-get!-exp)
