#lang racket/base

;; ============================================================
;; Pass: Uniquify
;; ============================================================
;;
;; Make all variable names unique by appending a counter.
;; This simplifies later passes by ensuring no shadowing.
;;
;; Input:  AST (Lvar)
;; Output: AST with unique variable names
;;
;; ============================================================

(require racket/match
         "../../../kernel/ir/ast/types.rkt")

(provide uniquify)

;; Counter for generating unique names
(define counter 0)

(define (fresh-name name)
  (set! counter (add1 counter))
  (string->symbol (format "~a.~a" name counter)))

(define (reset-counter!)
  (set! counter 0))

;; Environment: maps original names to unique names
(define (empty-env) '())

(define (extend-env env name new-name)
  (cons (cons name new-name) env))

(define (lookup-env env name)
  (cond
    [(assq name env) => cdr]
    [else (error 'uniquify "unbound variable: ~a" name)]))

;; ============================================================
;; Main Pass
;; ============================================================

(define (uniquify prog)
  (reset-counter!)
  (match prog
    [(Program info body)
     (Program info (uniquify-exp (empty-env) body))]))

(define (uniquify-exp env exp)
  (match exp
    ;; Literals - unchanged
    [(Int n) (Int n)]
    [(Bool b) (Bool b)]
    [(Void) (Void)]

    ;; Variable reference
    [(Var:named name)
     (Var:named (lookup-env env name))]

    ;; Let binding
    [(Let (Var:named name) rhs body)
     (define new-name (fresh-name name))
     (define new-env (extend-env env name new-name))
     (Let (Var:named new-name)
          (uniquify-exp env rhs)
          (uniquify-exp new-env body))]

    ;; Conditional
    [(If cond then else)
     (If (uniquify-exp env cond)
         (uniquify-exp env then)
         (uniquify-exp env else))]

    ;; Sequence
    [(Begin exprs body)
     (Begin (map (lambda (e) (uniquify-exp env e)) exprs)
            (uniquify-exp env body))]

    ;; Mutation
    [(SetBang (Var:named name) rhs)
     (SetBang (Var:named (lookup-env env name))
              (uniquify-exp env rhs))]
    [(GetBang (Var:named name))
     (GetBang (Var:named (lookup-env env name)))]

    ;; While loop
    [(WhileLoop cond body)
     (WhileLoop (uniquify-exp env cond)
                (uniquify-exp env body))]

    ;; Primitive operations
    [(Prim op args)
     (Prim op (map (lambda (a) (uniquify-exp env a)) args))]

    ;; Type annotations
    [(HasType exp type)
     (HasType (uniquify-exp env exp) type)]

    [_ (error 'uniquify-exp "unhandled expression: ~a" exp)]))
