#lang racket/base

;; ============================================================
;; Pass: Remove Complex Operands (ANF Transformation)
;; ============================================================
;;
;; Ensure all primitive operations only have atomic operands.
;; Complex operands are bound to temporary variables.
;;
;; Input:  AST (Lvar with unique names)
;; Output: AST in A-normal form
;;
;; ============================================================

(require racket/match
         "../../../kernel/ir/ast/types.rkt")

(provide remove-complex-opera*)

;; Counter for temporary names
(define temp-counter 0)

(define (fresh-temp)
  (set! temp-counter (add1 temp-counter))
  (string->symbol (format "tmp.~a" temp-counter)))

(define (reset-temp-counter!)
  (set! temp-counter 0))

;; ============================================================
;; Main Pass
;; ============================================================

(define (remove-complex-opera* prog)
  (reset-temp-counter!)
  (match prog
    [(Program info body)
     (Program info (rco-exp body))]))

;; Is expression atomic?
(define (atomic-exp? e)
  (match e
    [(Int _) #t]
    [(Bool _) #t]
    [(Void) #t]
    [(Var:named _) #t]
    [_ #f]))

;; rco-atom: ensure expression is atomic
;; Returns (values atom bindings) where bindings is list of (var . rhs)
(define (rco-atom exp)
  (if (atomic-exp? exp)
      (values exp '())
      (let ([tmp (fresh-temp)])
        (define-values (new-exp bindings) (rco-exp/bindings exp))
        (values (Var:named tmp)
                (append bindings (list (cons tmp new-exp)))))))

;; rco-exp/bindings: returns (values exp bindings)
(define (rco-exp/bindings exp)
  (match exp
    ;; Atomic - no bindings needed
    [(Int n) (values (Int n) '())]
    [(Bool b) (values (Bool b) '())]
    [(Void) (values (Void) '())]
    [(Var:named name) (values (Var:named name) '())]

    ;; Primitive operations - make arguments atomic
    [(Prim op args)
     (define-values (new-args all-bindings)
       (for/fold ([atoms '()] [bindings '()])
                 ([arg args])
         (define-values (atom bs) (rco-atom arg))
         (values (append atoms (list atom))
                 (append bindings bs))))
     (values (Prim op new-args) all-bindings)]

    ;; Let binding
    [(Let var rhs body)
     (define-values (new-rhs rhs-bindings) (rco-exp/bindings rhs))
     (define-values (new-body body-bindings) (rco-exp/bindings body))
     (values (Let var new-rhs new-body)
             (append rhs-bindings body-bindings))]

    ;; Conditional
    [(If cond then else)
     (define-values (cond-atom cond-bindings) (rco-atom cond))
     (define new-then (rco-exp then))
     (define new-else (rco-exp else))
     (values (If cond-atom new-then new-else) cond-bindings)]

    ;; Sequence
    [(Begin exprs body)
     (define new-exprs (map rco-exp exprs))
     (define new-body (rco-exp body))
     (values (Begin new-exprs new-body) '())]

    ;; Mutation
    [(SetBang var rhs)
     (define-values (new-rhs bindings) (rco-exp/bindings rhs))
     (values (SetBang var new-rhs) bindings)]
    [(GetBang var)
     (values (GetBang var) '())]

    ;; While loop
    [(WhileLoop cond body)
     (define new-cond (rco-exp cond))
     (define new-body (rco-exp body))
     (values (WhileLoop new-cond new-body) '())]

    ;; Type annotations
    [(HasType exp type)
     (define-values (new-exp bindings) (rco-exp/bindings exp))
     (values (HasType new-exp type) bindings)]

    [_ (error 'rco-exp/bindings "unhandled expression: ~a" exp)]))

;; Wrap expression with bindings
(define (wrap-bindings bindings exp)
  (foldr (lambda (binding body)
           (Let (Var:named (car binding)) (cdr binding) body))
         exp
         bindings))

;; rco-exp: main entry point
(define (rco-exp exp)
  (define-values (new-exp bindings) (rco-exp/bindings exp))
  (wrap-bindings bindings new-exp))
