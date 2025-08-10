#lang racket/base

(require "core/core-types.rkt")
(require "program-default.rkt" "control-flow-graph/var-id-reassign.rkt")

(require racket/class racket/match racket/dict)
(require cutie-ftree)

(define pass-var-new-name
  (class (pass-var-id-reassign-mixin pass-program 0)
    (super-new)
    (inherit gen-var-id)
    (inherit-field var-cnt)
    (field [symbol->var-id (ordl-make-empty symbol-compare)] [var-id->symbol (ordl-make-empty integer-compare)])
    (define/private (bind-symbol-var-id sym id)
      (set! symbol->var-id (dict-set symbol->var-id sym id))
      (set! var-id->symbol (dict-set var-id->symbol id sym))
    )
    (define/private (transfer sym)
      (define s (dict-ref symbol->var-id sym #f))
      (cond
        [s s]
        [else (define s^ (gen-var-id)) (bind-symbol-var-id sym s^) s^]
      )
    )
    (define/override pass-exp (match-lambda
      [(Var:r v) (Var (transfer v))]
      [(Let x t b) (Let (transfer x) (pass-exp t) (pass-exp b))]
      [(GetBang v) (GetBang (transfer v))]
      ((SetBang v r) (SetBang (transfer v) (pass-exp r)))
      [e (super pass-exp e)]
    ))
    (define/override pass (match-lambda
      [(and p (Program _ _))
        (define p^ (super pass p))
        (define info^ (dict-set* (Program-info p^) 'var-id->symbol var-id->symbol 'var-cnt var-cnt))
        (Program info^ (Program-body p^))
      ]
    ))
  ))

(provide pass-var-new-name)
