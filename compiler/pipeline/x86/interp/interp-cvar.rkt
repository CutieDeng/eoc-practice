#lang racket/base

;; ============================================================
;; Interpreter: C-var
;; ============================================================
;;
;; Reference interpreter for the C-var IR.
;; Used to verify explicate-control transformation.
;;
;; ============================================================

(require racket/match
         "../ir/cvar.rkt")

(provide interp-cvar
         interp-cprogram)

;; ============================================================
;; Main Interpreter
;; ============================================================

;; Interpret a C-var program
(define (interp-cvar prog)
  (interp-cprogram prog))

(define (interp-cprogram prog)
  (match prog
    [(CProgram info blocks)
     ;; Start from 'start block
     (define state (make-hash))
     (interp-block blocks state 'start)]))

;; ============================================================
;; Block Interpreter
;; ============================================================

(define (interp-block blocks state label)
  (define block (hash-ref blocks label
                          (lambda () (error 'interp-block "unknown label: ~a" label))))
  (match block
    [(CBlock info tail)
     (interp-tail blocks state tail)]))

;; ============================================================
;; Tail Interpreter
;; ============================================================

(define (interp-tail blocks state tail)
  (match tail
    ;; Return
    [(CReturn exp)
     (interp-cexp state exp)]

    ;; Sequence
    [(CSeq stmt tail)
     (interp-stmt state stmt)
     (interp-tail blocks state tail)]

    ;; Goto
    [(CGoto label)
     (interp-block blocks state label)]

    ;; Conditional
    [(CIf cond then-label else-label)
     (if (interp-cexp state cond)
         (interp-block blocks state then-label)
         (interp-block blocks state else-label))]

    [_ (error 'interp-tail "unhandled tail: ~a" tail)]))

;; ============================================================
;; Statement Interpreter
;; ============================================================

(define (interp-stmt state stmt)
  (match stmt
    [(CAssign (CVar name) exp)
     (hash-set! state name (interp-cexp state exp))]))

;; ============================================================
;; Expression Interpreter
;; ============================================================

(define (interp-cexp state exp)
  (match exp
    ;; Atomic
    [(CInt n) n]
    [(CBool b) b]
    [(CVar name)
     (hash-ref state name
               (lambda () (error 'interp-cexp "unbound variable: ~a" name)))]

    ;; Primitive operations
    [(CPrim op args)
     (interp-cprim op (map (lambda (a) (interp-cexp state a)) args))]

    [_ (error 'interp-cexp "unhandled expression: ~a" exp)]))

;; ============================================================
;; Primitive Operations
;; ============================================================

(define (interp-cprim op args)
  (match* (op args)
    ;; Arithmetic
    [('+ (list a b)) (+ a b)]
    [('- (list a)) (- a)]
    [('- (list a b)) (- a b)]

    ;; Comparison
    [('eq? (list a b)) (eq? a b)]
    [('< (list a b)) (< a b)]
    [('<= (list a b)) (<= a b)]
    [('> (list a b)) (> a b)]
    [('>= (list a b)) (>= a b)]

    ;; Logical
    [('not (list a)) (not a)]

    ;; I/O
    [('read '())
     (display "read> ")
     (flush-output)
     (read)]

    [(_ _) (error 'interp-cprim "unknown primitive: ~a with args ~a" op args)]))
