#lang racket

(require "core/core-types.rkt")
(require "control-flow-graph/var-id-reassign.rkt")
(require "program-default.rkt")
(require cutie-ftree)

(define pass-rco
  (class (pass-var-id-reassign-mixin pass-program #f)
    (super-new)
    (inherit-field var-cnt)
    (inherit gen-var-id)
    (field [rco-env (make-parameter #f)])
    (define/override (pass p)
      (set! var-cnt (dict-ref (Program-info p) 'var-cnt))
      (define p^ (super pass p))
      (Program (dict-set (Program-info p^) 'var-cnt var-cnt) (Program-body p^))
    )
    (define (lets-append exp)
      (for/fold ([exp^ exp]) ([i (in-ral0 (rco-env))])
        (match-define (cons v r) i)
        (Let v r exp^)
      )
    )
    (define/override (pass-exp p)
      (parameterize ([rco-env (ral-empty)])
        (lets-append
          (match p
            [(Prim o es) (Prim o (for/list ([e es]) (pass-atom e)))]
            [e (super pass-exp e)]
          )
        )
      )
    )
    (define (pass-atom p)
      (define p^ (pass-exp p))
      (cond
        [(atom? p^) p^]
        [else (define tmp (gen-var-id)) (rco-env (ral-consr (rco-env) (cons tmp p^))) (Var tmp)]
      )
    )
    (define atom? (match-lambda 
      [(or (Bool _) (Int _) (Var _) (Void )) #t]
      [_ #f]
    ))
  ))

(provide pass-rco)
