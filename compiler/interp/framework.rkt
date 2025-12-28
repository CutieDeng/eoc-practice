#lang racket/base

;; ============================================================
;; Interpreter Framework
;; ============================================================
;;
;; Provides a modular interpreter infrastructure for:
;;   - L-language (high-level expressions)
;;   - C-language (low-level control flow)
;;   - CFG-based IR (for optimization testing)
;;
;; Design:
;;   - Trait-based composition for extensibility
;;   - Built-in tracing for debugging
;;   - Value boxing/unboxing for type safety
;;   - Environment abstraction
;; ============================================================

(require racket/match racket/list racket/hash racket/class)
(require "../ir/ast/types.rkt")

;; ============================================================
;; Runtime Values
;; ============================================================

;; Tagged runtime values
(struct RtInt (value) #:transparent)
(struct RtBool (value) #:transparent)
(struct RtVoid () #:transparent)
(struct RtVector (elements) #:transparent)
(struct RtClosure (params body env) #:transparent)
(struct RtPrimitive (name arity proc) #:transparent)

(provide (struct-out RtInt) (struct-out RtBool) (struct-out RtVoid)
         (struct-out RtVector) (struct-out RtClosure) (struct-out RtPrimitive))

;; Value predicates
(define (runtime-value? x)
  (or (RtInt? x) (RtBool? x) (RtVoid? x)
      (RtVector? x) (RtClosure? x) (RtPrimitive? x)
      ;; Also allow raw Racket values for convenience
      (integer? x) (boolean? x) (void? x) (vector? x)))

(provide runtime-value?)

;; Unwrap runtime values to Racket values
(define (unwrap-value v)
  (match v
    [(RtInt n) n]
    [(RtBool b) b]
    [(RtVoid) (void)]
    [(RtVector elems) (list->vector elems)]
    [_ v]))  ; Already unwrapped

;; Wrap Racket values to runtime values
(define (wrap-value v)
  (cond
    [(RtInt? v) v]
    [(RtBool? v) v]
    [(RtVoid? v) v]
    [(RtVector? v) v]
    [(RtClosure? v) v]
    [(integer? v) (RtInt v)]
    [(boolean? v) (RtBool v)]
    [(void? v) (RtVoid)]
    [(vector? v) (RtVector (vector->list v))]
    [else v]))

(provide unwrap-value wrap-value)

;; ============================================================
;; Environment
;; ============================================================

;; Immutable environment (association list style)
(define empty-env '())

(define (env-extend env name value)
  (cons (cons name value) env))

(define (env-extend* env names values)
  (append (map cons names values) env))

(define (env-lookup env name)
  (define binding (assoc name env))
  (if binding
      (cdr binding)
      (error 'env-lookup "Unbound variable: ~a" name)))

(define (env-has? env name)
  (if (assoc name env) #t #f))

(define (env-update env name value)
  (cond
    [(null? env) (error 'env-update "Unbound variable: ~a" name)]
    [(equal? (caar env) name) (cons (cons name value) (cdr env))]
    [else (cons (car env) (env-update (cdr env) name value))]))

(provide empty-env env-extend env-extend* env-lookup env-has? env-update)

;; Mutable environment (for imperative features)
(struct MutEnv (bindings parent) #:transparent)

(define (make-mut-env [parent #f])
  (MutEnv (make-hash) parent))

(define (mut-env-set! env name value)
  (hash-set! (MutEnv-bindings env) name value))

(define (mut-env-get env name)
  (cond
    [(hash-has-key? (MutEnv-bindings env) name)
     (hash-ref (MutEnv-bindings env) name)]
    [(MutEnv-parent env)
     (mut-env-get (MutEnv-parent env) name)]
    [else (error 'mut-env-get "Unbound variable: ~a" name)]))

(define (mut-env-update! env name value)
  (cond
    [(hash-has-key? (MutEnv-bindings env) name)
     (hash-set! (MutEnv-bindings env) name value)]
    [(MutEnv-parent env)
     (mut-env-update! (MutEnv-parent env) name value)]
    [else (error 'mut-env-update! "Unbound variable: ~a" name)]))

(provide MutEnv make-mut-env mut-env-set! mut-env-get mut-env-update!)

;; ============================================================
;; Interpreter Context
;; ============================================================

;; Interpreter state and configuration
(struct InterpContext (
  trace?        ; Boolean - enable tracing
  trace-depth   ; Integer - current trace depth
  step-count    ; Box[Integer] - step counter
  fuel          ; Integer or #f - max steps (for termination)
  input-port    ; Input port for read
  output-port   ; Output port for print
) #:transparent)

(define (make-interp-context
         #:trace? [trace? #f]
         #:fuel [fuel #f]
         #:input [input (current-input-port)]
         #:output [output (current-output-port)])
  (InterpContext trace? 0 (box 0) fuel input output))

(define (ctx-step! ctx)
  (define count (unbox (InterpContext-step-count ctx)))
  (set-box! (InterpContext-step-count ctx) (+ count 1))
  (when (and (InterpContext-fuel ctx)
             (> count (InterpContext-fuel ctx)))
    (error 'interpreter "Fuel exhausted after ~a steps" count)))

(define (ctx-trace ctx msg . args)
  (when (InterpContext-trace? ctx)
    (define indent (make-string (* 2 (InterpContext-trace-depth ctx)) #\space))
    (fprintf (InterpContext-output-port ctx)
             "~a~a~n" indent (apply format msg args))))

(define (ctx-deeper ctx)
  (struct-copy InterpContext ctx
               [trace-depth (+ 1 (InterpContext-trace-depth ctx))]))

(provide (struct-out InterpContext) make-interp-context ctx-step! ctx-trace ctx-deeper)

;; ============================================================
;; Primitive Operations
;; ============================================================

;; Standard primitive operations
(define (make-standard-primitives)
  (hash
   ;; Arithmetic
   '+ (RtPrimitive '+ 2 (lambda (a b) (RtInt (+ (unwrap-value a) (unwrap-value b)))))
   '- (RtPrimitive '- 2 (lambda (a b) (RtInt (- (unwrap-value a) (unwrap-value b)))))
   '* (RtPrimitive '* 2 (lambda (a b) (RtInt (* (unwrap-value a) (unwrap-value b)))))
   'quotient (RtPrimitive 'quotient 2
              (lambda (a b) (RtInt (quotient (unwrap-value a) (unwrap-value b)))))
   'remainder (RtPrimitive 'remainder 2
               (lambda (a b) (RtInt (remainder (unwrap-value a) (unwrap-value b)))))
   'negate (RtPrimitive 'negate 1
            (lambda (a) (RtInt (- (unwrap-value a)))))

   ;; Comparison
   '< (RtPrimitive '< 2 (lambda (a b) (RtBool (< (unwrap-value a) (unwrap-value b)))))
   '<= (RtPrimitive '<= 2 (lambda (a b) (RtBool (<= (unwrap-value a) (unwrap-value b)))))
   '> (RtPrimitive '> 2 (lambda (a b) (RtBool (> (unwrap-value a) (unwrap-value b)))))
   '>= (RtPrimitive '>= 2 (lambda (a b) (RtBool (>= (unwrap-value a) (unwrap-value b)))))
   'eq? (RtPrimitive 'eq? 2 (lambda (a b) (RtBool (equal? (unwrap-value a) (unwrap-value b)))))

   ;; Boolean
   'not (RtPrimitive 'not 1 (lambda (a) (RtBool (not (unwrap-value a)))))
   'and (RtPrimitive 'and 2 (lambda (a b) (RtBool (and (unwrap-value a) (unwrap-value b)))))
   'or (RtPrimitive 'or 2 (lambda (a b) (RtBool (or (unwrap-value a) (unwrap-value b)))))

   ;; Vector
   'vector-ref (RtPrimitive 'vector-ref 2
                (lambda (v i)
                  (define vec (unwrap-value v))
                  (define idx (unwrap-value i))
                  (wrap-value (vector-ref vec idx))))
   'vector-set! (RtPrimitive 'vector-set! 3
                 (lambda (v i x)
                   (define vec (unwrap-value v))
                   (define idx (unwrap-value i))
                   (vector-set! vec idx (unwrap-value x))
                   (RtVoid)))
   'vector-length (RtPrimitive 'vector-length 1
                   (lambda (v)
                     (RtInt (vector-length (unwrap-value v)))))
   'make-vector (RtPrimitive 'make-vector 2
                 (lambda (n init)
                   (RtVector (make-list (unwrap-value n) init))))))

(define standard-primitives (make-standard-primitives))

(provide standard-primitives)

;; ============================================================
;; Abstract Interpreter Interface
;; ============================================================

;; Interpreter trait (interface for composition)
(define interpreter<%>
  (interface ()
    ;; Core interpretation methods
    interp-expr      ; Interpret an expression
    interp-stmt      ; Interpret a statement
    interp-program)) ; Interpret a program

(provide interpreter<%>)

;; Base interpreter implementation
(define base-interpreter%
  (class* object% (interpreter<%>)
    (super-new)

    (init-field [context (make-interp-context)]
                [primitives standard-primitives])

    ;; Override in subclasses
    (define/public (interp-expr env expr)
      (error 'interp-expr "Not implemented for: ~a" expr))

    (define/public (interp-stmt env stmt)
      (error 'interp-stmt "Not implemented for: ~a" stmt))

    (define/public (interp-program prog)
      (error 'interp-program "Not implemented for: ~a" prog))

    ;; Helper: apply primitive
    (define/public (apply-primitive op args)
      (define prim (hash-ref primitives op #f))
      (unless prim
        (error 'apply-primitive "Unknown primitive: ~a" op))
      (apply (RtPrimitive-proc prim) args))

    ;; Helper: apply closure
    (define/public (apply-closure closure args)
      (match closure
        [(RtClosure params body env)
         (define new-env (env-extend* env params args))
         (interp-expr new-env body)]
        [_ (error 'apply-closure "Not a closure: ~a" closure)]))))

(provide base-interpreter%)
