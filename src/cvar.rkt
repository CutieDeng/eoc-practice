#lang racket

; Cvar il 
(require "include/rvar.rkt" "include/prim.rkt")

(provide (all-defined-out))

(define (uniquify-exp env now-idx e) 
    (match e 
        [(Var x) (values now-idx (dict-ref env x))]
        [(Int n) (values now-idx (Int n))]
        [(Let x e body)
            (define new-idx (+ now-idx 1))
            (define-values (new-idx2 e2) (uniquify-exp env new-idx e))
            (define new-var (Var (format "~a.~a" (Var-var x) now-idx)))
            (define new-env (dict-set env (Var-var x) new-var))
            (define-values (new-idx3 body2) (uniquify-exp new-env new-idx2 body)) 
            (values new-idx3 (Let new-var e2 body2))]
        [(Prim op es) 
            (define new-idx now-idx)
            (define r
                (Prim op (for/list ([e es]) 
                    (define-values (new-idx2 rst) (uniquify-exp env new-idx e))
                    (set! new-idx new-idx2)
                    rst)))
            (values new-idx r)]
        ))


(define (parse-exp-cvar p)
    (define-values (_ e) (uniquify-exp '() 0 p))
       e)
