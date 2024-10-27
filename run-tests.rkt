#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp-Lvar.rkt")
(require "interp-Cvar.rkt")
(require "interp.rkt")
(require "compiler.rkt")

(require "interp-Lif.rkt")

;; (debug-level 1)
;; (AST-output-syntax 'concrete-syntax)

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
(interp-tests "var" #f compiler-passes interp-Lif "var_test" tests-for-var)
(interp-tests "cond" #f compiler-passes interp-Lif "cond_test" tests-for-cond)

;; The following tests the final x86 code.
(compiler-tests "var" #f compiler-passes "var_test" tests-for-var)
(compiler-tests "cond" #f compiler-passes "cond_test" tests-for-cond)
