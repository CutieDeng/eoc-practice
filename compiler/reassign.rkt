#lang racket

(require "core/core-types.rkt")
(require "program-default.rkt")
(require cutie-ftree)

(define pass-var-new-name
  (class pass-program
    (super-new)
    (field [symbol-map (ordl-make-empty symbol-compare)] [symbol-get (ordl-make-empty integer-compare)] [var-cnt 16])
    (define/private (var-id-gen)
      (begin0 var-cnt
        (set! var-cnt (+ var-cnt 1))
      )
    )
    (define/private (set-symbol-map sym id)
      (set! symbol-map (dict-set symbol-map sym id))
      (set! symbol-get (dict-set symbol-get id sym))
    )
    (define/private (transfer sym)
      (define s (dict-ref symbol-map sym #f))
      (cond
        [s s]
        [else (define s^ (var-id-gen)) (set-symbol-map sym s^) s^]
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
        ; TODO set symbol-get
        (Program (dict-set (dict-set (Program-info p^) 'symbol-map symbol-map) 'var-cnt var-cnt)(Program-body p^))
      ]
    ))
  ))

(provide pass-var-new-name)
