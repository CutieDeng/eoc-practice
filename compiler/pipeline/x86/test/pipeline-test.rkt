#lang racket/base

;; ============================================================
;; X86 Pipeline Tests
;; ============================================================

(require rackunit
         "../pipeline.rkt"
         "../check/interp-check.rkt"
         "../check/type-check.rkt"
         "../interp/interp-ast.rkt"
         "../../../kernel/ir/ast/types.rkt")

;; ============================================================
;; Test Programs
;; ============================================================

;; Simple integer literal
(define prog-int
  (Program '() (Int 42)))

;; Arithmetic: (+ 10 32)
(define prog-add
  (Program '() (Prim '+ (list (Int 10) (Int 32)))))

;; Nested arithmetic: (+ (- 10) 52)
(define prog-nested
  (Program '()
           (Prim '+ (list (Prim '- (list (Int 10)))
                          (Int 52)))))

;; Let binding: (let ([x 10]) (+ x 32))
(define prog-let
  (Program '()
           (Let (Var:named 'x) (Int 10)
                (Prim '+ (list (Var:named 'x) (Int 32))))))

;; Nested let: (let ([x 10]) (let ([y 32]) (+ x y)))
(define prog-nested-let
  (Program '()
           (Let (Var:named 'x) (Int 10)
                (Let (Var:named 'y) (Int 32)
                     (Prim '+ (list (Var:named 'x) (Var:named 'y)))))))

;; Conditional: (if #t 1 2)
(define prog-if-true
  (Program '() (If (Bool #t) (Int 1) (Int 2))))

;; Conditional with comparison: (if (< 1 2) 10 20)
(define prog-if-cmp
  (Program '()
           (If (Prim '< (list (Int 1) (Int 2)))
               (Int 10)
               (Int 20))))

;; Let with conditional
(define prog-let-if
  (Program '()
           (Let (Var:named 'x) (Int 5)
                (If (Prim '< (list (Var:named 'x) (Int 10)))
                    (Prim '+ (list (Var:named 'x) (Int 1)))
                    (Var:named 'x)))))

;; ============================================================
;; Interpreter Tests
;; ============================================================

(define interpreter-tests
  (test-suite "Interpreter Tests"
    (test-case "integer literal"
      (check-equal? (interp-ast prog-int) 42))

    (test-case "addition"
      (check-equal? (interp-ast prog-add) 42))

    (test-case "nested arithmetic"
      (check-equal? (interp-ast prog-nested) 42))

    (test-case "let binding"
      (check-equal? (interp-ast prog-let) 42))

    (test-case "nested let"
      (check-equal? (interp-ast prog-nested-let) 42))

    (test-case "if true"
      (check-equal? (interp-ast prog-if-true) 1))

    (test-case "if comparison"
      (check-equal? (interp-ast prog-if-cmp) 10))

    (test-case "let with if"
      (check-equal? (interp-ast prog-let-if) 6))))

;; ============================================================
;; Type Check Tests
;; ============================================================

(define type-check-tests
  (test-suite "Type Check Tests"
    (test-case "integer literal"
      (check-equal? (type-check prog-int) 'Integer))

    (test-case "addition"
      (check-equal? (type-check prog-add) 'Integer))

    (test-case "let binding"
      (check-equal? (type-check prog-let) 'Integer))

    (test-case "if true"
      (check-equal? (type-check prog-if-true) 'Integer))

    (test-case "if comparison"
      (check-equal? (type-check prog-if-cmp) 'Integer))))

;; ============================================================
;; Pass Tests
;; ============================================================

(define pass-tests
  (test-suite "Pass Tests"
    (test-case "uniquify preserves semantics"
      (define result (check-pass uniquify interp-ast interp-ast "uniquify" prog-nested-let))
      (check-true (interp-check-result-pass? result)))

    (test-case "remove-complex preserves semantics"
      (define prog1 (uniquify prog-nested))
      (define result (check-pass remove-complex-opera* interp-ast interp-ast "rco" prog1))
      (check-true (interp-check-result-pass? result)))

    (test-case "full pipeline on integer"
      (define-values (pass? results) (check-pipeline prog-int))
      (check-true pass?))

    (test-case "full pipeline on addition"
      (define-values (pass? results) (check-pipeline prog-add))
      (check-true pass?))

    (test-case "full pipeline on nested arithmetic"
      (define-values (pass? results) (check-pipeline prog-nested))
      (check-true pass?))

    (test-case "full pipeline on let"
      (define-values (pass? results) (check-pipeline prog-let))
      (check-true pass?))

    (test-case "full pipeline on nested let"
      (define-values (pass? results) (check-pipeline prog-nested-let))
      (check-true pass?))

    (test-case "full pipeline on if"
      (define-values (pass? results) (check-pipeline prog-if-true))
      (check-true pass?))

    (test-case "full pipeline on if comparison"
      (define-values (pass? results) (check-pipeline prog-if-cmp))
      (check-true pass?))))

;; ============================================================
;; Compilation Output Tests
;; ============================================================

(define compilation-tests
  (test-suite "Compilation Tests"
    (test-case "compile integer produces assembly"
      (define asm (compile-program prog-int))
      (check-true (string-contains? asm "main:"))
      (check-true (string-contains? asm "movq $42")))

    (test-case "compile addition produces assembly"
      (define asm (compile-program prog-add))
      (check-true (string-contains? asm "addq")))))

;; Helper
(define (string-contains? str substr)
  (regexp-match? (regexp-quote substr) str))

;; ============================================================
;; Run Tests
;; ============================================================

(module+ test
  (require rackunit/text-ui)
  (run-tests interpreter-tests)
  (run-tests type-check-tests)
  (run-tests pass-tests)
  (run-tests compilation-tests))
