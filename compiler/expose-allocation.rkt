#lang racket

(require "../utilities.rkt")
(require "program-default.rkt")

(define pass-expose-allocation
  (class pass-program
    (super-new)
    (define/public (expand-env-r body env) (match env
      [(cons (cons x v) rest) (Let x v (expand-env-r body rest))]
      ['() body]
    ))
    (define/override pass-exp (match-lambda
      [(HasType (Prim 'vector es) types)
        (define-values (es^ env^) (for/fold ([es^ '()] [env^ '()]) ([e es])
          (define tmp (gensym 'tmp))
          (values (cons tmp es^) (dict-set env^ tmp (pass-exp e)))
          ))
        (define bytes (* (+ 1 (length es)) 8))
        (define pre-collect (λ (b) (Let '_ (If 
          (Prim '< (list 
            (Prim '+ (list (GlobalValue 'free_ptr) (Int bytes)))
            (GlobalValue 'fromspace_end))) (Void) (Collect bytes)) b)))
        (define v (gensym 'tmp))
        (define inner (for/foldr ([b (Var v)]) ([e es^] [idx (in-range (length es))])
          (Let '_ (Prim 'vector-set! (list (Var v) (Int idx) (Var e))) b)
          ))
        (set! inner (expand-env-r inner env^))
        (set! inner (Let v (Allocate (length es) types) inner))
        (set! inner (pre-collect inner))
        inner
      ]
      [body (super pass-exp body)]
    ))
  ))

(provide pass-expose-allocation)
