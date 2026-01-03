#lang racket/base

;; ============================================================
;; Type Checker
;; ============================================================
;;
;; Type checker for the source AST language.
;; Performs type inference and checking.
;;
;; ============================================================

(require racket/match
         racket/string
         "../../../kernel/ir/ast/types.rkt")

(provide type-check
         type-check-exp
         type-error?
         type-error-message)

;; ============================================================
;; Types
;; ============================================================

;; Type representations
(define (Integer) 'Integer)
(define (Boolean) 'Boolean)
(define (Void-type) 'Void)
(define (Float-type) 'Float)
(define (String-type) 'String)
(define (Function params return) (list '-> params return))

(define (type=? t1 t2)
  (equal? t1 t2))

(define (type->string type)
  (match type
    ['Integer "Integer"]
    ['Boolean "Boolean"]
    ['Void "Void"]
    ['Float "Float"]
    ['String "String"]
    [(list '-> params return)
     (format "(~a) -> ~a"
             (string-join (map type->string params) ", ")
             (type->string return))]
    [_ (format "~a" type)]))

;; ============================================================
;; Type Errors
;; ============================================================

(struct type-error (message location) #:transparent)

;; ============================================================
;; Type Environment
;; ============================================================

(define (empty-tenv) '())

(define (extend-tenv tenv name type)
  (cons (cons name type) tenv))

(define (lookup-tenv tenv name)
  (cond
    [(assq name tenv) => cdr]
    [else #f]))

;; ============================================================
;; Main Type Checker
;; ============================================================

(define (type-check prog)
  (match prog
    [(Program info body)
     (with-handlers ([exn:fail? (lambda (e)
                                   (type-error (exn-message e) 'program))])
       (type-check-exp (empty-tenv) body))]))

;; ============================================================
;; Expression Type Checker
;; ============================================================

(define (type-check-exp tenv exp)
  (match exp
    ;; Literals
    [(Int _) (Integer)]
    [(Bool _) (Boolean)]
    [(Void) (Void-type)]
    [(Float _) (Float-type)]
    [(String _) (String-type)]

    ;; Variable reference
    [(Var:named name)
     (define type (lookup-tenv tenv name))
     (unless type
       (error 'type-check "unbound variable: ~a" name))
     type]

    [(Var id)
     (define type (lookup-tenv tenv id))
     (unless type
       (error 'type-check "unbound variable: ~a" id))
     type]

    ;; Let binding
    [(Let (Var:named name) rhs body)
     (define rhs-type (type-check-exp tenv rhs))
     (type-check-exp (extend-tenv tenv name rhs-type) body)]

    [(Let (Var id) rhs body)
     (define rhs-type (type-check-exp tenv rhs))
     (type-check-exp (extend-tenv tenv id rhs-type) body)]

    ;; Conditional
    [(If cond then else)
     (define cond-type (type-check-exp tenv cond))
     (unless (type=? cond-type (Boolean))
       (error 'type-check "condition must be Boolean, got ~a" (type->string cond-type)))
     (define then-type (type-check-exp tenv then))
     (define else-type (type-check-exp tenv else))
     (unless (type=? then-type else-type)
       (error 'type-check "branches must have same type: ~a vs ~a"
              (type->string then-type) (type->string else-type)))
     then-type]

    ;; Sequence
    [(Begin exprs body)
     (for ([e exprs])
       (type-check-exp tenv e))
     (type-check-exp tenv body)]

    ;; Primitive operations
    [(Prim op args)
     (type-check-prim tenv op args)]

    ;; Type annotation
    [(HasType exp type)
     (define actual-type (type-check-exp tenv exp))
     (unless (type=? actual-type type)
       (error 'type-check "type annotation mismatch: expected ~a, got ~a"
              (type->string type) (type->string actual-type)))
     type]

    [_ (error 'type-check "unhandled expression: ~a" exp)]))

;; ============================================================
;; Primitive Type Checking
;; ============================================================

(define (type-check-prim tenv op args)
  (define arg-types (map (lambda (a) (type-check-exp tenv a)) args))

  (match* (op arg-types)
    ;; Arithmetic: Integer -> Integer
    [('+ (list t1 t2))
     (check-types op (list t1 t2) (list (Integer) (Integer)))
     (Integer)]
    [('- (list t))
     (check-types op (list t) (list (Integer)))
     (Integer)]
    [('- (list t1 t2))
     (check-types op (list t1 t2) (list (Integer) (Integer)))
     (Integer)]
    [('* (list t1 t2))
     (check-types op (list t1 t2) (list (Integer) (Integer)))
     (Integer)]
    [('/ (list t1 t2))
     (check-types op (list t1 t2) (list (Integer) (Integer)))
     (Integer)]
    [('remainder (list t1 t2))
     (check-types op (list t1 t2) (list (Integer) (Integer)))
     (Integer)]

    ;; Comparison: Integer -> Boolean
    [('< (list t1 t2))
     (check-types op (list t1 t2) (list (Integer) (Integer)))
     (Boolean)]
    [('<= (list t1 t2))
     (check-types op (list t1 t2) (list (Integer) (Integer)))
     (Boolean)]
    [('> (list t1 t2))
     (check-types op (list t1 t2) (list (Integer) (Integer)))
     (Boolean)]
    [('>= (list t1 t2))
     (check-types op (list t1 t2) (list (Integer) (Integer)))
     (Boolean)]

    ;; Equality: any -> Boolean
    [('eq? (list t1 t2))
     (unless (type=? t1 t2)
       (error 'type-check "eq? requires same types: ~a vs ~a"
              (type->string t1) (type->string t2)))
     (Boolean)]
    [('= (list t1 t2))
     (check-types op (list t1 t2) (list (Integer) (Integer)))
     (Boolean)]

    ;; Logical: Boolean -> Boolean
    [('not (list t))
     (check-types op (list t) (list (Boolean)))
     (Boolean)]
    [('and (list t1 t2))
     (check-types op (list t1 t2) (list (Boolean) (Boolean)))
     (Boolean)]
    [('or (list t1 t2))
     (check-types op (list t1 t2) (list (Boolean) (Boolean)))
     (Boolean)]

    ;; I/O
    [('read '())
     (Integer)]
    [('print (list _))
     (Void-type)]

    [(_ _)
     (error 'type-check-prim "unknown primitive: ~a with ~a args" op (length arg-types))]))

(define (check-types op actual expected)
  (for ([a actual] [e expected] [i (in-naturals)])
    (unless (type=? a e)
      (error 'type-check "~a: argument ~a expected ~a, got ~a"
             op i (type->string e) (type->string a)))))
