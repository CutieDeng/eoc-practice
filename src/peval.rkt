#lang racket/base 

; 用于执行 partial evaluate

(require racket/match 
    racket/fixnum "include/prim.rkt") 

(define (pe-neg r) (match r 
    [(Int n) (Int (fx- 0 n))] 
    [else (Prim '- (list r))]))

(define (pe-add r1 r2) (match* (r1 r2) 
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))] 
    [(_ _) (Prim '+ (list r1 r2))])) 

(define (pe-exp e) (match e 
    [(Int n) e] 
    [(Prim 'read '()) e] 
    [(Prim '- (list e1)) (pe-neg (pe-exp e1))] 
    [(Prim '+ (list e1 e2)) (pe-add (pe-exp e1) (pe-exp e2))]))

(define (pe-Rint p) (match p 
    [(Program '() e) (Program '() (pe-exp e))]))

(provide (all-defined-out))