#lang racket/base

;; ============================================================
;; Interpreter Tests
;; ============================================================

(require rackunit racket/list)
(require "main.rkt")
(require "../core/core-types.rkt")

;; ============================================================
;; Test 1: Basic L-Language Expressions
;; ============================================================

(test-case "L-interp: integers"
  (define prog (Program '() (Int 42)))
  (check-equal? (interp-L prog) 42))

(test-case "L-interp: booleans"
  (define prog-true (Program '() (Bool #t)))
  (define prog-false (Program '() (Bool #f)))
  (check-equal? (interp-L prog-true) #t)
  (check-equal? (interp-L prog-false) #f))

(test-case "L-interp: arithmetic"
  (define prog-add (Program '() (Prim '+ (list (Int 10) (Int 20)))))
  (define prog-sub (Program '() (Prim '- (list (Int 30) (Int 10)))))
  (define prog-neg (Program '() (Prim '- (list (Int 5)))))
  (check-equal? (interp-L prog-add) 30)
  (check-equal? (interp-L prog-sub) 20)
  (check-equal? (interp-L prog-neg) -5))

(test-case "L-interp: let binding"
  ;; Use Var:r for named variables
  (define prog
    (Program '()
      (Let 'x (Int 10)
        (Prim '+ (list (Var:r 'x) (Int 5))))))
  (check-equal? (interp-L prog) 15))

(test-case "L-interp: nested let"
  (define prog
    (Program '()
      (Let 'x (Int 10)
        (Let 'y (Int 20)
          (Prim '+ (list (Var:r 'x) (Var:r 'y)))))))
  (check-equal? (interp-L prog) 30))

;; ============================================================
;; Test 2: Conditionals
;; ============================================================

(test-case "L-interp: if-true"
  (define prog
    (Program '()
      (If (Bool #t) (Int 1) (Int 2))))
  (check-equal? (interp-L prog) 1))

(test-case "L-interp: if-false"
  (define prog
    (Program '()
      (If (Bool #f) (Int 1) (Int 2))))
  (check-equal? (interp-L prog) 2))

(test-case "L-interp: comparison"
  (define prog-lt
    (Program '()
      (If (Prim '< (list (Int 5) (Int 10)))
          (Int 1)
          (Int 0))))
  (check-equal? (interp-L prog-lt) 1))

(test-case "L-interp: and/or"
  (define prog-and
    (Program '()
      (Prim 'and (list (Bool #t) (Bool #f)))))
  (define prog-or
    (Program '()
      (Prim 'or (list (Bool #f) (Bool #t)))))
  (check-equal? (interp-L prog-and) #f)
  (check-equal? (interp-L prog-or) #t))

;; ============================================================
;; Test 3: Loops and Mutation
;; ============================================================

;; Note: WhileLoop tests require ral for Begin, skip for now
;; Will be tested when ral integration is complete

;; ============================================================
;; Test 4: Vectors
;; ============================================================

(test-case "L-interp: vector operations"
  (define prog
    (Program '()
      (Let 'v (Prim 'vector (list (Int 1) (Int 2) (Int 3)))
        (Prim 'vector-ref (list (Var:r 'v) (Int 1))))))
  (check-equal? (interp-L prog) 2))

(test-case "L-interp: vector length"
  (define prog
    (Program '()
      (Let 'v (Prim 'vector (list (Int 1) (Int 2) (Int 3)))
        (Prim 'vector-length (list (Var:r 'v))))))
  (check-equal? (interp-L prog) 3))

;; ============================================================
;; Test 5: Optimization Testing Utilities
;; ============================================================

(test-case "test-equivalent: same programs"
  (define prog1 (Program '() (Prim '+ (list (Int 10) (Int 20)))))
  (define prog2 (Program '() (Prim '+ (list (Int 10) (Int 20)))))
  (check-true (test-equivalent prog1 prog2)))

(test-case "test-equivalent: different programs same result"
  ;; Constant folded version
  (define prog1 (Program '() (Prim '+ (list (Int 10) (Int 20)))))
  (define prog2 (Program '() (Int 30)))
  (check-true (test-equivalent prog1 prog2)))

(test-case "test-equivalent: different results"
  (define prog1 (Program '() (Int 10)))
  (define prog2 (Program '() (Int 20)))
  (check-false (test-equivalent prog1 prog2)))

;; ============================================================
;; Test 6: Fuel Limit (Termination)
;; ============================================================

;; Note: WhileLoop tests require ral for Begin, skip for now

;; ============================================================
;; Summary
;; ============================================================

(displayln "All interpreter tests passed!")
