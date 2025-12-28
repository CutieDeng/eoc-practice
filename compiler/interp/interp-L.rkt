#lang racket/base

;; ============================================================
;; L-Language Interpreter
;; ============================================================
;;
;; Interprets L-language programs (high-level functional).
;; Supports:
;;   - Integers, booleans, void
;;   - Variables and let bindings
;;   - Conditionals and loops
;;   - Functions (lambda, application)
;;   - Vectors
;;   - Mutation (set!, get!)
;;
;; Used for:
;;   - Testing optimizations (run before/after, compare results)
;;   - Validating transformations
;;   - Debugging compiler passes
;; ============================================================

(require racket/match racket/list racket/class racket/fixnum racket/dict)
(require "framework.rkt")
(require "../ir/ast/types.rkt")

;; Also support old AST types for compatibility
(require "../core/p-types.rkt")

;; ============================================================
;; L-Language Interpreter Class
;; ============================================================

(define l-interpreter%
  (class base-interpreter%
    (super-new)

    (inherit-field context primitives)
    (inherit apply-primitive apply-closure)

    ;; Main expression interpreter
    (define/override (interp-expr env expr)
      (ctx-step! context)
      (ctx-trace context "interp-expr: ~a" expr)

      (match expr
        ;; === New unified AST ===
        [(LInt v) (RtInt v)]
        [(LBool v) (RtBool v)]
        [(LVoid) (RtVoid)]

        [(LVar name) (env-lookup env name)]
        [(LVarIdx idx) (list-ref env idx)]

        [(LLet name init body)
         (define init-val (interp-expr env init))
         (define new-env (env-extend env name init-val))
         (interp-expr new-env body)]

        [(LIf cond then else)
         (define cond-val (unwrap-value (interp-expr env cond)))
         (if cond-val
             (interp-expr env then)
             (interp-expr env else))]

        [(LWhile cond body)
         (let loop ()
           (define cond-val (unwrap-value (interp-expr env cond)))
           (when cond-val
             (interp-expr env body)
             (loop)))
         (RtVoid)]

        [(LBegin exprs result)
         (for ([e exprs])
           (interp-expr env e))
         (interp-expr env result)]

        [(LLambda params body)
         (RtClosure params body env)]

        [(LApply func args)
         (define func-val (interp-expr env func))
         (define arg-vals (map (lambda (a) (interp-expr env a)) args))
         (apply-closure func-val arg-vals)]

        [(LPrim op args)
         (define arg-vals (map (lambda (a) (interp-expr env a)) args))
         (apply-primitive op arg-vals)]

        [(LTyped expr _type)
         (interp-expr env expr)]

        ;; === Old AST compatibility ===
        [(Int v) (RtInt v)]
        [(Bool v) (RtBool v)]
        [(Void) (RtVoid)]

        [(Var id)
         (cond
           [(symbol? id) (unbox-if-box (env-lookup env id))]
           [(integer? id)
            (define keys (map car env))
            (if (< id (length keys))
                (unbox-if-box (env-lookup env (list-ref keys id)))
                (error 'interp-expr "Variable index out of bounds: ~a" id))]
           [else (error 'interp-expr "Unknown Var id type: ~a" id)])]

        [(Var:r name)
         (unbox-if-box (env-lookup env name))]

        [(Let x init body)
         (define init-val (interp-expr env init))
         (define var-name (if (symbol? x) x (string->symbol (format "v~a" x))))
         ;; Use box for mutable semantics
         (define new-env (env-extend env var-name (box init-val)))
         (interp-expr new-env body)]

        [(If cond then else)
         (define cond-val (unwrap-value (interp-expr env cond)))
         (if cond-val
             (interp-expr env then)
             (interp-expr env else))]

        [(WhileLoop cond body)
         (let loop ()
           (define cond-val (unwrap-value (interp-expr env cond)))
           (when cond-val
             (interp-expr env body)
             (loop)))
         (RtVoid)]

        [(Begin exprs result)
         (for ([e exprs])
           (interp-expr env e))
         (interp-expr env result)]

        [(SetBang var rhs)
         (define val (interp-expr env rhs))
         (define var-name (if (symbol? var) var (string->symbol (format "v~a" var))))
         (define cell (env-lookup env var-name))
         (when (box? cell)
           (set-box! cell val))
         (RtVoid)]

        [(GetBang var)
         (define var-name (if (symbol? var) var (string->symbol (format "v~a" var))))
         (unbox-if-box (env-lookup env var-name))]

        [(Lambda params _rty body)
         (define param-names
           (for/list ([p params])
             (if (pair? p) (car p) p)))
         (RtClosure param-names body env)]

        [(Apply func args)
         (define func-val (interp-expr env func))
         (define arg-vals (map (lambda (a) (interp-expr env a)) args))
         (match func-val
           [(RtClosure params body closure-env)
            (define new-env
              (for/fold ([e closure-env])
                        ([p params] [a arg-vals])
                (env-extend e p (box a))))
            (interp-expr new-env body)]
           [_ (error 'interp-expr "Not a function: ~a" func-val)])]

        [(Prim op args)
         (interp-prim env op args)]

        [(HasType expr _type)
         (interp-expr env expr)]

        [(FunRef name _arity)
         (env-lookup env name)]

        [(Call func args)
         (define func-val
           (match func
             [(? symbol?) (env-lookup env func)]
             [_ (interp-expr env func)]))
         (define arg-vals (map (lambda (a) (interp-expr env a)) args))
         (match func-val
           [(RtClosure params body closure-env)
            (define new-env
              (for/fold ([e closure-env])
                        ([p params] [a arg-vals])
                (env-extend e p (box a))))
            (interp-expr new-env body)]
           [_ (error 'interp-expr "Not a function: ~a" func-val)])]

        [_ (error 'interp-expr "Unknown expression: ~a" expr)]))

    ;; Helper for unboxing
    (define (unbox-if-box v)
      (if (box? v) (unbox v) v))

    ;; Interpret primitive operations (with special cases)
    (define/public (interp-prim env op args)
      (match op
        ;; Short-circuit boolean ops
        ['and
         (match args
           [(list e1 e2)
            (define v1 (unwrap-value (interp-expr env e1)))
            (if v1
                (interp-expr env e2)
                (RtBool #f))])]
        ['or
         (match args
           [(list e1 e2)
            (define v1 (unwrap-value (interp-expr env e1)))
            (if v1
                (RtBool #t)
                (interp-expr env e2))])]

        ;; I/O
        ['read
         (RtInt (read (InterpContext-input-port context)))]

        ['print
         (match args
           [(list e)
            (define val (interp-expr env e))
            (display (unwrap-value val) (InterpContext-output-port context))
            (RtVoid)])]

        ;; Unary minus
        ['-
         (match args
           [(list e)
            (define val (unwrap-value (interp-expr env e)))
            (RtInt (fx- 0 val))]
           [(list e1 e2)
            (define v1 (unwrap-value (interp-expr env e1)))
            (define v2 (unwrap-value (interp-expr env e2)))
            (RtInt (fx- v1 v2))])]

        ;; Addition
        ['+
         (match args
           [(list e1 e2)
            (define v1 (unwrap-value (interp-expr env e1)))
            (define v2 (unwrap-value (interp-expr env e2)))
            (RtInt (fx+ v1 v2))])]

        ;; Multiplication
        ['*
         (match args
           [(list e1 e2)
            (define v1 (unwrap-value (interp-expr env e1)))
            (define v2 (unwrap-value (interp-expr env e2)))
            (RtInt (fx* v1 v2))])]

        ;; Comparison
        ['<
         (match args
           [(list e1 e2)
            (define v1 (unwrap-value (interp-expr env e1)))
            (define v2 (unwrap-value (interp-expr env e2)))
            (RtBool (< v1 v2))])]

        ['<=
         (match args
           [(list e1 e2)
            (define v1 (unwrap-value (interp-expr env e1)))
            (define v2 (unwrap-value (interp-expr env e2)))
            (RtBool (<= v1 v2))])]

        ['>
         (match args
           [(list e1 e2)
            (define v1 (unwrap-value (interp-expr env e1)))
            (define v2 (unwrap-value (interp-expr env e2)))
            (RtBool (> v1 v2))])]

        ['>=
         (match args
           [(list e1 e2)
            (define v1 (unwrap-value (interp-expr env e1)))
            (define v2 (unwrap-value (interp-expr env e2)))
            (RtBool (>= v1 v2))])]

        ['eq?
         (match args
           [(list e1 e2)
            (define v1 (unwrap-value (interp-expr env e1)))
            (define v2 (unwrap-value (interp-expr env e2)))
            (RtBool (equal? v1 v2))])]

        ['not
         (match args
           [(list e)
            (define v (unwrap-value (interp-expr env e)))
            (RtBool (not v))])]

        ;; Vector operations
        ['vector
         (define vals (map (lambda (a) (interp-expr env a)) args))
         (RtVector vals)]

        ['vector-ref
         (match args
           [(list vec-e idx-e)
            (define vec (unwrap-value (interp-expr env vec-e)))
            (define idx (unwrap-value (interp-expr env idx-e)))
            (wrap-value (vector-ref vec idx))])]

        ['vector-set!
         (match args
           [(list vec-e idx-e val-e)
            (define vec (unwrap-value (interp-expr env vec-e)))
            (define idx (unwrap-value (interp-expr env idx-e)))
            (define val (unwrap-value (interp-expr env val-e)))
            (vector-set! vec idx val)
            (RtVoid)])]

        ['vector-length
         (match args
           [(list vec-e)
            (define vec (unwrap-value (interp-expr env vec-e)))
            (RtInt (vector-length vec))])]

        ['make-vector
         (match args
           [(list size-e init-e)
            (define size (unwrap-value (interp-expr env size-e)))
            (define init (unwrap-value (interp-expr env init-e)))
            (wrap-value (make-vector size init))])]

        ['vectorof-ref
         (match args
           [(list vec-e idx-e)
            (define vec (unwrap-value (interp-expr env vec-e)))
            (define idx (unwrap-value (interp-expr env idx-e)))
            (if (< idx (vector-length vec))
                (wrap-value (vector-ref vec idx))
                (error 'interp "vectorof-ref: index out of bounds"))])]

        ['vectorof-set!
         (match args
           [(list vec-e idx-e val-e)
            (define vec (unwrap-value (interp-expr env vec-e)))
            (define idx (unwrap-value (interp-expr env idx-e)))
            (define val (unwrap-value (interp-expr env val-e)))
            (if (< idx (vector-length vec))
                (begin (vector-set! vec idx val) (RtVoid))
                (error 'interp "vectorof-set!: index out of bounds"))])]

        ['vectorof-length
         (match args
           [(list vec-e)
            (define vec (unwrap-value (interp-expr env vec-e)))
            (RtInt (vector-length vec))])]

        ['exit
         (error 'interp "exit called")]

        ;; Unknown primitive
        [_
         (error 'interp-prim "Unknown primitive: ~a" op)]))

    ;; Interpret a program
    (define/override (interp-program prog)
      (match prog
        [(LProgram _info body)
         (unwrap-value (interp-expr empty-env body))]

        [(LProgramDefs _info defs body)
         (define fun-env
           (for/fold ([env empty-env])
                     ([def defs])
             (match def
               [(LDef name params _rty body)
                (env-extend env name (RtClosure params body env))]
               [_ env])))
         (unwrap-value (interp-expr fun-env body))]

        ;; Old AST compatibility
        [(Program _info body)
         (unwrap-value (interp-expr empty-env body))]

        [(ProgramDefs _info defs)
         (define env
           (for/fold ([env empty-env])
                     ([def defs])
             (match def
               [(Def name params _rty _info body)
                (define param-names
                  (for/list ([p params])
                    (if (pair? p) (car p) p)))
                (env-extend env name (RtClosure param-names body env))]
               [_ env])))
         (define main-closure (env-lookup env 'main))
         (unwrap-value (send this apply-closure main-closure '()))]

        [(ProgramDefsExp _info defs body)
         (define env
           (for/fold ([env empty-env])
                     ([def defs])
             (match def
               [(Def name params _rty _info body)
                (define param-names
                  (for/list ([p params])
                    (if (pair? p) (car p) p)))
                (env-extend env name (RtClosure param-names body env))]
               [_ env])))
         (unwrap-value (interp-expr env body))]

        [_ (error 'interp-program "Unknown program: ~a" prog)]))))

(provide l-interpreter%)

;; ============================================================
;; Convenience Functions
;; ============================================================

;; Create a new L interpreter
(define (make-l-interpreter #:trace? [trace? #f] #:fuel [fuel #f])
  (define ctx (make-interp-context #:trace? trace? #:fuel fuel))
  (new l-interpreter% [context ctx]))

;; Interpret an L program
(define (interp-L prog #:trace? [trace? #f] #:fuel [fuel #f])
  (define interp (make-l-interpreter #:trace? trace? #:fuel fuel))
  (send interp interp-program prog))

;; Interpret an L expression in empty environment
(define (eval-L expr #:env [env empty-env] #:trace? [trace? #f])
  (define interp (make-l-interpreter #:trace? trace?))
  (send interp interp-expr env expr))

(provide make-l-interpreter interp-L eval-L)

;; ============================================================
;; Testing Utilities
;; ============================================================

;; Test that two programs produce the same result
(define (test-equivalent prog1 prog2)
  (define result1 (interp-L prog1))
  (define result2 (interp-L prog2))
  (equal? result1 result2))

;; Test that optimization preserves semantics
(define (test-optimization original optimized)
  (test-equivalent original optimized))

;; Run with captured output
(define (interp-L-with-output prog)
  (define output (open-output-string))
  (define ctx (make-interp-context #:output output))
  (define interp (new l-interpreter% [context ctx]))
  (define result (send interp interp-program prog))
  (values result (get-output-string output)))

(provide test-equivalent test-optimization interp-L-with-output)
