#lang racket

(require "include/rvar.rkt" "include/prim.rkt" "include/explicit-control.rkt")

(require "cvar.rkt")
; rco
(require "rvarbnf.rkt")

(define (explicit-tail e)
    (match e
        [(Var _) (Return e)]
        [(Int _) (Return e)]
        [(Let x e body)
            (define t (explicit-tail body))
            (explicit-assign e x t)]
        [(Prim _ _)
            (Return e)]
        ))

(define (explicit-assign e x cont)
    (match e
        [(Var _) (Seq (Assign x e) cont)]
        [(Int _) (Seq (Assign x e) cont)]
        [(Prim _ _) (Seq (Assign x e) cont)]
        [(Let y rhs body)
            (define lo.1 (explicit-assign body x cont))
            (explicit-assign rhs y lo.1)]
    )
)

(define (parse-explicit-control p)
    (define-values (_ exp) (uniquify-exp '() 0 p))
    (define exp2 (rco exp))
    (explicit-tail exp2)
)

(provide (all-defined-out))
