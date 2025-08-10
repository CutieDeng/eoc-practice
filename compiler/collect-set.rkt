#lang racket

(require "core/core-types.rkt")
(require "core/integer-set.rkt")
(require "program-default.rkt")
(require cutie-ftree)

(define pass-collect-set! 
  (class pass-program
    (super-new)
    (field [set!-var-set (bset)])
    (define (mut-var-set-add! var)
      (set! set!-var-set (bset-add set!-var-set var))
    )
    (define/override (pass-exp body) (match body
      [(Let _ rhs body) (pass-exp rhs) (pass-exp body)]
      [(SetBang var rhs) (mut-var-set-add! var) (pass-exp rhs)]
      [(If cnd thn els) (pass-exp cnd) (pass-exp thn) (pass-exp els)]
      [(Prim _ args) (for ([a args]) (pass-exp a))]
      [(Begin es e) (for ([a (in-ral0 es)]) (pass-exp a)) (pass-exp e)]
      [(WhileLoop cnd body) (pass-exp cnd) (pass-exp body)]
      [_ (void)]
    ))
    (define/override (pass p)
      (match p [(Program info body)
        (pass-exp body)
        (Program (dict-set info 'set! set!-var-set) body)
      ])
    )
  ))

(provide pass-collect-set!)
