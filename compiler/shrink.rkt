#lang racket

(require "core/core-types.rkt")
(require "program-default.rkt")

(define pass-shrink
  (class pass-program
    (super-new)
    (define/override pass-exp (match-lambda
      [(Prim 'and (list a b)) (If (pass-exp a) (pass-exp b) (Bool #f))]
      [(Prim 'or (list a b)) (If (pass-exp a) (Bool #t) (pass-exp b))]
      [e (super pass-exp e)]
    ))
  ))

(provide pass-shrink)
