#lang racket

(require "include/rvar.rkt" "include/prim.rkt")

(define (rco-exp idx e to-add-lets)
    (match e
        [(Var _) (rco-atom idx e to-add-lets)]
        [(Int _) (rco-atom idx e to-add-lets)]
        [(Let x e body) 
            (define-values (idx2 e2 to-add-lets2) (rco-exp idx e '()))
            (define e2-2 (foldl (λ (h b) (match h [(cons v e) (Let v e b)])) e2 to-add-lets2))
            (define-values (idx3 body2 to-add-lets3) (rco-exp idx2 body '()))
            (define body2-2 (foldl (λ (h b) (match h [(cons v e) (Let v e b)])) body2 to-add-lets3))
            (values idx3 (Let x e2-2 body2-2) to-add-lets)]
        [(Prim op es) 
            (define es2
                (for/list ([e es]) 
                    (define-values (idx2 e2 to-add-lets2) (rco-atom idx e to-add-lets))
                    (set! idx idx2)
                    (set! to-add-lets to-add-lets2)
                    e2))
            (values idx (Prim op es2) to-add-lets)
        ]
    ))

(define (rco-atom idx e to-add-lets)
    (match e
        [(Var _) (values idx e to-add-lets)]
        [(Int _) (values idx e to-add-lets)]
        [(Prim '- (list e1))
            (cond 
                [(dict-has-key? to-add-lets e) (values idx (dict-ref to-add-lets e) to-add-lets)]
                [else 
                    (define-values (idx2 inner-e to-add-lets2) (rco-atom idx e1 to-add-lets))
                    (define var-idx2 (Var idx2))
                    (define new-let (dict-set to-add-lets2 var-idx2 (Prim '- (list inner-e))))
                    (values (+ idx2 1) var-idx2 new-let)]
                    )]
        [(Prim '+ (list e1 e2))
            (define-values (idx2 inner-e to-add-lets2) (rco-atom idx e1 to-add-lets))
            (define-values (idx3 inner-e2 to-add-lets3) (rco-atom idx2 e2 to-add-lets2))
            (define var-idx3 (Var idx3))
            (define new-let (dict-set to-add-lets3 var-idx3 (Prim '+ (list inner-e inner-e2))))
            (values (+ idx3 1) var-idx3 new-let)]
        [(Let x e body)
            (define-values (idx2 e2 to-add-lets2) (rco-atom idx e to-add-lets))
            (define-values (idx3 body2 to-add-lets3) (rco-exp idx2 body '()))
            (define body3 (foldl (λ (h b) (match h [(cons v e) (Let v e b)])) body2 to-add-lets3))
            (define now (Let x e2 body3))
            (values idx3 now to-add-lets2)]
    ))

(define (rco p)
    (define-values (_ e to-add-lets) (rco-exp 0 p '()))
    (define new-body (foldl (λ (h b) (match h [(cons v e) (Let v e b)])) e to-add-lets))
    new-body
)

(provide (all-defined-out))
