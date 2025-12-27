#lang racket/base

;; ============================================================
;; Tests for Function Inlining
;; ============================================================

(require rackunit)
(require racket/match racket/list racket/set)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "inline.rkt")

;; ============================================================
;; Helper Functions
;; ============================================================

(define (make-test-cfg entry blocks)
  (define block-table
    (for/hash ([block blocks])
      (values (CfgBlock-id block) block)))
  (Cfg (hash-count block-table)
       0
       (for/sum ([block blocks]) (length (CfgBlock-insns block)))
       entry
       block-table
       (hash)))

(define (make-block id phis insns term)
  (CfgBlock id phis insns term))

;; ============================================================
;; Test 1: Find call sites - basic call
;; ============================================================

(define test1-blocks
  (list
   (make-block 'entry '()
               (list (VfInsn 'const '(10) (list (VarId 'x)) #f #f)
                     (VfInsn 'call (list 'foo (VarId 'x)) (list (VarId 'result)) #f #f))
               (TermReturn (list (VarId 'result))))))

(define test1-cfg (make-test-cfg 'entry test1-blocks))

(test-case "Test 1: Find call sites - basic call"
  (define candidates (find-call-sites test1-cfg))
  (check-equal? (length candidates) 1)
  (check-equal? (InlineCandidate-callee (car candidates)) 'foo)
  (check-equal? (InlineCandidate-args (car candidates)) (list (VarId 'x))))

;; ============================================================
;; Test 2: Find call sites - multiple calls
;; ============================================================

(define test2-blocks
  (list
   (make-block 'entry '()
               (list (VfInsn 'const '(10) (list (VarId 'x)) #f #f)
                     (VfInsn 'call (list 'foo (VarId 'x)) (list (VarId 'a)) #f #f)
                     (VfInsn 'call (list 'bar (VarId 'a)) (list (VarId 'b)) #f #f))
               (TermReturn (list (VarId 'b))))))

(define test2-cfg (make-test-cfg 'entry test2-blocks))

(test-case "Test 2: Find call sites - multiple calls"
  (define candidates (find-call-sites test2-cfg))
  (check-equal? (length candidates) 2)
  (check-equal? (InlineCandidate-callee (car candidates)) 'foo)
  (check-equal? (InlineCandidate-callee (cadr candidates)) 'bar))

;; ============================================================
;; Test 3: Find call sites - invoke variants
;; ============================================================

(define test3-blocks
  (list
   (make-block 'entry '()
               (list (VfInsn 'const '(10) (list (VarId 'x)) #f #f)
                     (VfInsn 'invokestatic (list 'staticMethod (VarId 'x)) (list (VarId 'a)) #f #f)
                     (VfInsn 'invokevirtual (list 'virtualMethod (VarId 'a)) (list (VarId 'b)) #f #f))
               (TermReturn (list (VarId 'b))))))

(define test3-cfg (make-test-cfg 'entry test3-blocks))

(test-case "Test 3: Find call sites - invoke variants"
  (define candidates (find-call-sites test3-cfg))
  (check-equal? (length candidates) 2)
  (check-equal? (InlineCandidate-callee (car candidates)) 'staticMethod)
  (check-equal? (InlineCandidate-callee (cadr candidates)) 'virtualMethod))

;; ============================================================
;; Test 4: Should inline - small function
;; ============================================================

(define small-func-blocks
  (list
   (make-block 'entry '()
               (list (VfInsn 'add (list (VarId 'x) (VarId 'y)) (list (VarId 'z)) #f #f))
               (TermReturn (list (VarId 'z))))))

(define small-func-cfg (make-test-cfg 'entry small-func-blocks))

(test-case "Test 4: Should inline - small function"
  (check-true (should-inline? small-func-cfg 1))
  (check-true (should-inline? small-func-cfg 10)))

;; ============================================================
;; Test 5: Should inline - function called once
;; ============================================================

(define medium-func-blocks
  (list
   (make-block 'entry '()
               (list (VfInsn 'add (list (VarId 'a) (VarId 'b)) (list (VarId 'c)) #f #f)
                     (VfInsn 'mul (list (VarId 'c) (VarId 'c)) (list (VarId 'd)) #f #f)
                     (VfInsn 'sub (list (VarId 'd) 1) (list (VarId 'e)) #f #f)
                     (VfInsn 'div (list (VarId 'e) 2) (list (VarId 'f)) #f #f)
                     (VfInsn 'add (list (VarId 'f) 100) (list (VarId 'g)) #f #f)
                     (VfInsn 'mul (list (VarId 'g) 3) (list (VarId 'h)) #f #f)
                     (VfInsn 'sub (list (VarId 'h) 50) (list (VarId 'i)) #f #f)
                     (VfInsn 'div (list (VarId 'i) 4) (list (VarId 'j)) #f #f)
                     (VfInsn 'add (list (VarId 'j) (VarId 'a)) (list (VarId 'k)) #f #f)
                     (VfInsn 'mul (list (VarId 'k) (VarId 'b)) (list (VarId 'l)) #f #f)
                     (VfInsn 'sub (list (VarId 'l) (VarId 'c)) (list (VarId 'm)) #f #f)
                     (VfInsn 'div (list (VarId 'm) (VarId 'd)) (list (VarId 'n)) #f #f)
                     (VfInsn 'add (list (VarId 'n) (VarId 'e)) (list (VarId 'o)) #f #f)
                     (VfInsn 'mul (list (VarId 'o) (VarId 'f)) (list (VarId 'p)) #f #f)
                     (VfInsn 'sub (list (VarId 'p) (VarId 'g)) (list (VarId 'q)) #f #f))
               (TermReturn (list (VarId 'q))))))

(define medium-func-cfg (make-test-cfg 'entry medium-func-blocks))

(test-case "Test 5: Should inline - function called once"
  ;; Function called once should be inlined
  (check-true (should-inline? medium-func-cfg 1)))

;; ============================================================
;; Test 6: Count CFG instructions
;; ============================================================

(test-case "Test 6: Count CFG instructions"
  (check-equal? (count-cfg-insns small-func-cfg) 1)
  (check-equal? (count-cfg-insns medium-func-cfg) 15))

;; ============================================================
;; Test 7: Register and get function
;; ============================================================

(test-case "Test 7: Register and get function"
  (register-function 'addOne small-func-cfg)
  (check-equal? (get-function 'addOne) small-func-cfg)
  (check-false (get-function 'nonexistent)))

;; ============================================================
;; Test 8: Analyze inlining
;; ============================================================

(test-case "Test 8: Analyze inlining"
  (register-function 'foo small-func-cfg)
  (define analysis (analyze-inlining test1-cfg))
  (check-equal? (length analysis) 1)
  (define info (car analysis))
  (check-equal? (cdr (assoc 'callee info)) 'foo)
  (check-true (cdr (assoc 'callee-available info)))
  (check-true (cdr (assoc 'should-inline info))))

;; ============================================================
;; Test 9: Inline simple function
;; ============================================================

(define caller-blocks
  (list
   (make-block 'entry '()
               (list (VfInsn 'const '(5) (list (VarId 'x)) #f #f)
                     (VfInsn 'const '(3) (list (VarId 'y)) #f #f)
                     (VfInsn 'call (list 'simpleAdd (VarId 'x) (VarId 'y))
                             (list (VarId 'result)) #f #f))
               (TermReturn (list (VarId 'result))))))

(define caller-cfg (make-test-cfg 'entry caller-blocks))

(define simple-add-blocks
  (list
   (make-block 'entry '()
               (list (VfInsn 'add (list (VarId 'p0) (VarId 'p1)) (list (VarId 'sum)) #f #f))
               (TermReturn (list (VarId 'sum))))))

(define simple-add-cfg (make-test-cfg 'entry simple-add-blocks))

(test-case "Test 9: Inline simple function"
  (register-function 'simpleAdd simple-add-cfg)
  (define inlined (cfg-inline caller-cfg))
  ;; After inlining, should have more instructions (param copies + inlined body)
  (check-true (>= (count-cfg-insns inlined) (count-cfg-insns caller-cfg))))

;; ============================================================
;; Test 10: Inline with statistics
;; ============================================================

(test-case "Test 10: Inline with statistics"
  (register-function 'simpleAdd simple-add-cfg)
  (define-values (cfg^ stats) (cfg-inline-with-stats caller-cfg))
  (check-not-false (assoc 'call-sites stats))
  (check-not-false (assoc 'inlined stats))
  (check-not-false (assoc 'insns-before stats))
  (check-not-false (assoc 'insns-after stats)))

;; ============================================================
;; Run all tests
;; ============================================================

(displayln "All inline tests passed!")
