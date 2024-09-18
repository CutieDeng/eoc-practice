#lang racket

(struct Program (info body) #:transparent) 

(struct Int (value) #:transparent) 
(struct Prim (op args) #:transparent) 

(define (leaf? arith) 
    (match arith 
        [(Int _) #t] 
        [(Prim 'read '()) #t]
        [(Prim '+ (list _ _)) #f]
        [(Prim '- (list _)) #f])) 

(define (exp? ast) 
    (match ast 
        [(Int _) #t] 
        [(Prim 'read '()) #t] 
        [(Prim '- (list e)) (exp? e)]
        [(Prim '+ (list e1 e2)) (and (exp? e1) (exp? e2))]
        [_ #f]))

(define Rint? (λ (e) (match e [(Program _ e) (exp? e)])))

(provide (all-defined-out))
