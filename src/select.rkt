#lang racket

(require "include/prim.rkt" "include/rvar.rkt" "include/explicit-control.rkt")
(require "include/select-instruction.rkt")

(require "explicit-control.rkt")
(require "cvar.rkt")
(require "rvarbnf.rkt")

(define (select-instructions p)
    (match p
        [(Seq assign rhs)
            (define assign-ins (match assign 
                [(Assign x (Int n)) (Imm x n)]
                [(Assign x (Prim '+ (list y z))) (Add x y z)]
                [(Assign x (Prim '- (list y))) (Sub x y)]
                [(Assign x (Var y)) (Mov x y)]
            ))
            (cons assign-ins (select-instructions rhs))]
        [(Return v)
            (list (Mov (Reg "w0") v))]))
                
(define (select p)
    ; (define p2 (parse-exp-cvar p))
    ; (define p3 (rco p2))
    (define p4 (parse-explicit-control p))
    (select-instructions p4))

(define t0
    (select
        (Let (Var "y") 
            (Let (Var "x.1") (Int 20) (Let (Var "x.2") (Int 22) (Prim '+ (list (Var "x.1") (Var "x.2"))))) (Var "y")))
)

(printf "~a\n" t0)