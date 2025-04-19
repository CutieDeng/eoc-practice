#lang racket

(require "../utilities.rkt")
(require "program-default.rkt")

(define pass-uncover-get!-exp 
  (class pass-program
    (super-new)
    (field [set!-vars #f])
    (define/override (pass p) (match p [(Program info body)
      (set! set!-vars (dict-ref info 'set!))
      (cond
        [set!-vars (super pass p)]
        [else (printf "Warning: set! info needed when uncover get!\n") p]
      )
    ]))
    (define/override (pass-exp exp) (match exp
      [(Var x) #:when (set-member? set!-vars x) (GetBang x)]
      [(SetBang var rhs)
        (define rhs^ (pass-exp rhs))
        (if (eq? rhs rhs^) exp (SetBang var rhs^))]
      [_ (super pass-exp exp)]
    ))
  ))

(provide pass-uncover-get!-exp)
