#lang racket/base

;; ============================================================
;; C-Language Interpreter
;; ============================================================
;;
;; Interprets C-language programs (low-level control flow).
;; Supports:
;;   - Assignments and returns
;;   - Labels and goto
;;   - Conditional branches
;;   - Function calls
;;
;; Used for:
;;   - Testing lowering passes
;;   - Validating CFG transformations
;;   - Debugging backend code generation
;; ============================================================

(require racket/match racket/list racket/class racket/dict racket/fixnum)
(require "framework.rkt")
(require "interp-L.rkt")
;; Use old AST types for compatibility
(require "../core/core-types.rkt")
(require "../lib/ftree.rkt")

;; ============================================================
;; C-Language Interpreter Class
;; ============================================================

(define c-interpreter%
  (class l-interpreter%
    (super-new)

    (inherit-field context primitives)
    (inherit interp-expr interp-prim)

    ;; Current block table (for goto)
    (field [blocks (make-parameter #f)])

    ;; Interpret a statement, returns updated environment
    (define/override (interp-stmt env stmt)
      (ctx-step! context)
      (ctx-trace context "interp-stmt: ~a" stmt)

      (match stmt
        ;; Old AST
        [(Assign (Var x) rhs)
         (define var-name
           (cond
             [(symbol? x) x]
             [(integer? x) (string->symbol (format "v~a" x))]
             [else x]))
         (define val (interp-expr env rhs))
         (env-extend env var-name val)]

        [(Return arg)
         (interp-expr env arg)]

        [_ (error 'interp-stmt "Unknown statement: ~a" stmt)]))

    ;; Interpret a tail (sequence of statements ending in control flow)
    (define/public (interp-tail env tail)
      (ctx-step! context)
      (ctx-trace context "interp-tail: ~a" tail)

      (match tail
        ;; Handle pvector (finger tree) sequences
        [(? pvector? seq)
         (interp-pvector-seq env seq)]

        ;; Old AST - list of statements
        [(list stmt ... (Return arg))
         (define final-env
           (for/fold ([e env]) ([s stmt])
             (interp-stmt e s)))
         (interp-expr final-env arg)]

        [(list stmt ... (Goto label))
         (define final-env
           (for/fold ([e env]) ([s stmt])
             (interp-stmt e s)))
         (define block (dict-ref (blocks) label))
         (interp-tail final-env block)]

        [(list stmt ... (IfStmt cond (Goto then-label) (Goto else-label)))
         (define final-env
           (for/fold ([e env]) ([s stmt])
             (interp-stmt e s)))
         (define cond-val (unwrap-value (interp-expr final-env cond)))
         (define label (if cond-val then-label else-label))
         (define block (dict-ref (blocks) label))
         (interp-tail final-env block)]

        [_ (error 'interp-tail "Unknown tail: ~a" tail)]))

    ;; Interpret a pvector sequence
    (define/public (interp-pvector-seq env seq)
      (match seq
        ;; Empty sequence - shouldn't happen
        [(? pvector-empty?) (error 'interp-pvector-seq "Empty sequence")]

        ;; Single element
        [(pvector x)
         (match x
           [(Return arg) (interp-expr env arg)]
           [(Goto label)
            (define block (dict-ref (blocks) label))
            (interp-tail env block)]
           [(IfStmt cond (Goto then-label) (Goto else-label))
            (define cond-val (unwrap-value (interp-expr env cond)))
            (define label (if cond-val then-label else-label))
            (define block (dict-ref (blocks) label))
            (interp-tail env block)]
           [stmt
            (interp-stmt env stmt)])]

        ;; Multiple elements - process first, recurse on rest
        [(pvector** first (pvector _ rest))
         (match first
           [(Return arg) (interp-expr env arg)]
           [(Goto label)
            (define block (dict-ref (blocks) label))
            (interp-tail env block)]
           [(IfStmt cond (Goto then-label) (Goto else-label))
            (define cond-val (unwrap-value (interp-expr env cond)))
            (define label (if cond-val then-label else-label))
            (define block (dict-ref (blocks) label))
            (interp-tail env block)]
           [stmt
            (define new-env (interp-stmt env stmt))
            (interp-pvector-seq new-env rest)])]))

    ;; Interpret a C program
    (define/override (interp-program prog)
      (match prog
        ;; Old AST - blocks is a dict
        [(CProgram _info block-dict)
         (parameterize ([blocks block-dict])
           (define start-block (dict-ref block-dict 'start))
           (unwrap-value (interp-tail empty-env start-block)))]

        [_ (super interp-program prog)]))))

(provide c-interpreter%)

;; ============================================================
;; Convenience Functions
;; ============================================================

;; Create a new C interpreter
(define (make-c-interpreter #:trace? [trace? #f] #:fuel [fuel #f])
  (define ctx (make-interp-context #:trace? trace? #:fuel fuel))
  (new c-interpreter% [context ctx]))

;; Interpret a C program
(define (interp-C prog #:trace? [trace? #f] #:fuel [fuel #f])
  (define interp (make-c-interpreter #:trace? trace? #:fuel fuel))
  (send interp interp-program prog))

(provide make-c-interpreter interp-C)

;; ============================================================
;; Testing Utilities
;; ============================================================

;; Test L and C programs produce same result
(define (test-L-C-equivalent l-prog c-prog)
  (define l-result (interp-L l-prog))
  (define c-result (interp-C c-prog))
  (equal? l-result c-result))

;; Test lowering preserves semantics
(define (test-lowering original lowered)
  (test-L-C-equivalent original lowered))

(provide test-L-C-equivalent test-lowering)
