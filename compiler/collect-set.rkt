#lang racket

(require "../utilities.rkt")
(require "program-default.rkt")

(define pass-collect-set! 
  (class pass-program
    (super-new)
    (field [set!-env (mutable-set)])
    (define/override (pass-exp body) (match body
      [(Let _ rhs body) (pass-exp rhs) (pass-exp body)]
      [(SetBang var rhs) (set-add! set!-env var) (pass-exp rhs)]
      [(If cnd thn els) (pass-exp cnd) (pass-exp thn) (pass-exp els)]
      [(Prim _ args) (for ([a args]) (pass-exp a))]
      [(Begin es e) (for ([a es]) (pass-exp a)) (pass-exp e)]
      [(WhileLoop cnd body) (pass-exp cnd) (pass-exp body)]
      [_ (void)]
    ))
    (define/override (pass p)
      (match p [(Program info body)
        (pass-exp body)
        (Program (dict-set info 'set! set!-env) body)
      ])
    )
  ))

(provide pass-collect-set!)
