#lang racket/base

;; ============================================================
;; Interpreter: AST
;; ============================================================
;;
;; Reference interpreter for the source AST language.
;; Used to verify semantics preservation across passes.
;;
;; ============================================================

(require racket/match
         "../../../kernel/ir/ast/types.rkt")

(provide interp-ast
         interp-exp)

;; ============================================================
;; Main Interpreter
;; ============================================================

;; Interpret a program
(define (interp-ast prog)
  (match prog
    [(Program info body)
     (interp-exp (empty-env) body)]))

;; Environment operations
(define (empty-env) '())

(define (extend-env env name val)
  (cons (cons name val) env))

(define (lookup-env env name)
  (cond
    [(assq name env) => cdr]
    [else (error 'lookup-env "unbound variable: ~a" name)]))

;; ============================================================
;; Expression Interpreter
;; ============================================================

(define (interp-exp env exp)
  (match exp
    ;; Literals
    [(Int n) n]
    [(Bool b) b]
    [(Void) (void)]
    [(Float f) f]
    [(String s) s]

    ;; Variable reference
    [(Var:named name)
     (lookup-env env name)]
    [(Var id)
     (lookup-env env id)]

    ;; Let binding
    [(Let (Var:named name) rhs body)
     (define val (interp-exp env rhs))
     (interp-exp (extend-env env name val) body)]
    [(Let (Var id) rhs body)
     (define val (interp-exp env rhs))
     (interp-exp (extend-env env id val) body)]

    ;; Conditional
    [(If cond then else)
     (if (interp-exp env cond)
         (interp-exp env then)
         (interp-exp env else))]

    ;; Sequence
    [(Begin exprs body)
     (for ([e exprs])
       (interp-exp env e))
     (interp-exp env body)]

    ;; Primitive operations
    [(Prim op args)
     (interp-prim op (map (lambda (a) (interp-exp env a)) args))]

    ;; Type annotation - interpret inner expression
    [(HasType exp _)
     (interp-exp env exp)]

    [_ (error 'interp-exp "unhandled expression: ~a" exp)]))

;; ============================================================
;; Primitive Operations
;; ============================================================

(define (interp-prim op args)
  (match* (op args)
    ;; Arithmetic
    [('+ (list a b)) (+ a b)]
    [('- (list a)) (- a)]
    [('- (list a b)) (- a b)]
    [('* (list a b)) (* a b)]
    [('/ (list a b)) (quotient a b)]
    [('remainder (list a b)) (remainder a b)]

    ;; Comparison
    [('eq? (list a b)) (eq? a b)]
    [('= (list a b)) (= a b)]
    [('< (list a b)) (< a b)]
    [('<= (list a b)) (<= a b)]
    [('> (list a b)) (> a b)]
    [('>= (list a b)) (>= a b)]

    ;; Logical
    [('not (list a)) (not a)]
    [('and (list a b)) (and a b)]
    [('or (list a b)) (or a b)]

    ;; I/O
    [('read '())
     (display "read> ")
     (flush-output)
     (read)]

    [('print (list v))
     (displayln v)
     (void)]

    [(_ _) (error 'interp-prim "unknown primitive: ~a with args ~a" op args)]))
