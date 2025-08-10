#! /usr/bin/env racket
#lang racket

(require "compiler/core/utilities.rkt")
(require "compiler.rkt")

(debug-level 4)

;; all the files in the tests/ directory with extension ".rkt".
(define all-tests
  (append-map (λ (p) 
    (define p0 (string-split (path->string p) "."))
    (if (string=? (cadr p0) "rkt") (list (car p0)) '()))
    (directory-list (build-path (current-directory) "tests"))))

(define (tests-for r)
  (append-map (λ (p)
    (define p0 (string-split p "_"))
    (cond
      [(string=? r (car p0)) (list (caddr p0))]
      [else null]))
      all-tests))
  
;; The following tests the intermediate-language outputs of the passes.
(define tests-for-var (tests-for "var"))
(define tests-for-cond (tests-for "cond"))
(define tests-for-while (tests-for "while"))
(define tests-for-vectors (tests-for "vectors"))

(define test-run (λ () (begin
  (run-compile-test-suites "var" compiler-passes "var_test" tests-for-var)
  ; (interp-tests "var" #f compiler-passes interp-Lvec-prime "var_test" tests-for-var)
  ; (interp-tests "cond" #f compiler-passes interp-Lvec-prime "cond_test" tests-for-cond)
  ; (interp-tests "while" #f compiler-passes interp-Lvec-prime "while_test" tests-for-while)
  ; (interp-tests "vectors" #f compiler-passes interp-Lvec-prime "vectors_test" tests-for-vectors)
  ; (compiler-tests "var" #f compiler-passes "var_test" tests-for-var)
  ; (compiler-tests "cond" #f compiler-passes "cond_test" tests-for-cond)
  ; (compiler-tests "while" #f compiler-passes "while_test" tests-for-while)
  ; (compiler-tests "vectors" #f compiler-passes "vectors_test" tests-for-vectors)
)))

; (require profile)
; (profile-thunk (λ () (test-run)) #:repeat 1)

(test-run)
