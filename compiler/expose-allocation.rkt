#lang racket

(require "core/core-types.rkt")
(require "program-default.rkt")
(require "control-flow-graph/var-id-reassign.rkt")
(require cutie-ftree)

(define pass-expose-allocation
  (class (pass-var-id-reassign-mixin pass-program #f)
    (super-new)
    (inherit-field var-cnt)
    (inherit gen-var-id)
    (define/private (expand-envs body env) 
      (for/fold ([body body]) ([e (in-ral0 env)])
        (match-define (cons x v) e)
        (Let x v body)
      ))
    (define/private (prepare-alloc-bytes es-length)
      (* (+ 1 es-length) 8)
    )
    (define/override pass-exp (match-lambda
      [(HasType (Prim 'vector es) types)
        (define-values (es^ env^) (for/fold ([es^ (ral-empty)] [env^ (ral-empty)]) ([e es])
          (define tmp (gen-var-id))
          (values (ral-consl es^ tmp) (ral-consl env^ (cons tmp (pass-exp e))))
        ))
        (define bytes (prepare-alloc-bytes (ral-length es^)))
        (define pre-collect (λ (b) (Let (gen-var-id) (If 
          (Prim '< (list 
            (Prim '+ (list (GlobalValue 'free_ptr) (Int bytes)))
            (GlobalValue 'fromspace_end))) (Void) (Collect bytes)) b)))
        (define v (gen-var-id))
        (define inner (for/fold ([b (Var v)]) ([e (in-ral0 es^)] [idx (in-range (length es))])
          (Let (gen-var-id) (Prim 'vector-set! (list (Var v) (Int idx) (Var e))) b)
        ))
        (define inner^ (expand-envs inner env^))
        (define inner^^ (Let v (Allocate (length es) types) inner^))
        (define inner^^^ (pre-collect inner^^))
        inner^^^
      ]
      [body (super pass-exp body)]
    ))
    (define/override (pass p)
      (define info (Program-info p))
      (set! var-cnt (dict-ref info 'var-cnt))
      (define p^ (super pass p))
      (Program (dict-set* (Program-info p^) 'var-cnt var-cnt) (Program-body p^))
    )
  ))

(provide pass-expose-allocation)
