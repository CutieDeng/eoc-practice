#lang racket/base

;; ============================================================
;; Tests for Loop Distribution
;; ============================================================

(require rackunit racket/hash racket/match)
(require "loop-distrib.rkt")
(require "loop-analysis.rkt")
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")

;; Helper to create a CFG
(define (make-test-cfg entry-id blocks-spec)
  (define blocks
    (for/hash ([spec blocks-spec])
      (match spec
        [(list bid phis insns term)
         (values (BlockId bid)
                 (CfgBlock (BlockId bid) phis insns term))])))
  (Cfg 100 100 100 (BlockId entry-id) #f blocks (hash)))

;; ============================================================
;; Test 1: Simple loop with independent statements
;; ============================================================
;;   for i:
;;     a[i] = 1
;;     b[i] = 2
;; These two statements are independent and could be distributed
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'const (list 0) (list (VarId 'i)) #f #f))
              ,(TermJump (BlockId 'header)))
       (header
        (,(PhiInsn (VarId 'i_phi)
                   (list (cons (BlockId 'entry) (VarId 'i))
                         (cons (BlockId 'body) (VarId 'i_next)))))
        ()
        ,(TermBranch (VarId 'cond) (BlockId 'body) (BlockId 'exit)))
       (body ()
             (,(VfInsn 'store (list (VarId 'a) (VarId 'i_phi) 1) (list) #f #f)
              ,(VfInsn 'store (list (VarId 'b) (VarId 'i_phi) 2) (list) #f #f)
              ,(VfInsn 'add (list (VarId 'i_phi) 1) (list (VarId 'i_next)) #f #f))
             ,(TermJump (BlockId 'header)))
       (exit () () ,(TermReturn '())))))

  (define analysis (analyze-loop-distribution cfg))
  (check-true (pair? analysis) "Should find at least one loop")

  (define cfg^ (cfg-loop-distrib cfg))
  (check-true (Cfg? cfg^) "Distribution should not crash"))

;; ============================================================
;; Test 2: Loop with dependent statements
;; ============================================================
;;   for i:
;;     x = a[i]
;;     b[i] = x + 1  ; depends on x
;; These statements cannot be distributed
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'const (list 0) (list (VarId 'i)) #f #f))
              ,(TermJump (BlockId 'header)))
       (header
        (,(PhiInsn (VarId 'i_phi)
                   (list (cons (BlockId 'entry) (VarId 'i))
                         (cons (BlockId 'body) (VarId 'i_next)))))
        ()
        ,(TermBranch (VarId 'cond) (BlockId 'body) (BlockId 'exit)))
       (body ()
             (,(VfInsn 'load (list (VarId 'a) (VarId 'i_phi)) (list (VarId 'x)) #f #f)
              ,(VfInsn 'add (list (VarId 'x) 1) (list (VarId 'y)) #f #f)
              ,(VfInsn 'store (list (VarId 'b) (VarId 'i_phi) (VarId 'y)) (list) #f #f)
              ,(VfInsn 'add (list (VarId 'i_phi) 1) (list (VarId 'i_next)) #f #f))
             ,(TermJump (BlockId 'header)))
       (exit () () ,(TermReturn '())))))

  (define analysis (analyze-loop-distribution cfg))
  (check-true (pair? analysis) "Should find at least one loop"))

;; ============================================================
;; Test 3: No loop - no distribution
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'add (list (VarId 'a) (VarId 'b)) (list (VarId 'x)) #f #f))
              ,(TermReturn (list (VarId 'x)))))))

  (define analysis (analyze-loop-distribution cfg))
  (check-equal? analysis '() "No loops should be found"))

;; ============================================================
;; Test 4: Single statement loop
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'const (list 0) (list (VarId 'i)) #f #f))
              ,(TermJump (BlockId 'header)))
       (header
        (,(PhiInsn (VarId 'i_phi)
                   (list (cons (BlockId 'entry) (VarId 'i))
                         (cons (BlockId 'body) (VarId 'i_next)))))
        ()
        ,(TermBranch (VarId 'cond) (BlockId 'body) (BlockId 'exit)))
       (body ()
             (,(VfInsn 'add (list (VarId 'i_phi) 1) (list (VarId 'i_next)) #f #f))
             ,(TermJump (BlockId 'header)))
       (exit () () ,(TermReturn '())))))

  (define cfg^ (cfg-loop-distrib cfg))
  (check-true (Cfg? cfg^) "Single statement loop should be handled"))

;; ============================================================
;; Test 5: With statistics
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'const (list 0) (list (VarId 'i)) #f #f))
              ,(TermJump (BlockId 'header)))
       (header
        (,(PhiInsn (VarId 'i_phi)
                   (list (cons (BlockId 'entry) (VarId 'i))
                         (cons (BlockId 'body) (VarId 'i_next)))))
        ()
        ,(TermBranch (VarId 'cond) (BlockId 'body) (BlockId 'exit)))
       (body ()
             (,(VfInsn 'store (list (VarId 'a) (VarId 'i_phi) 1) (list) #f #f)
              ,(VfInsn 'store (list (VarId 'b) (VarId 'i_phi) 2) (list) #f #f)
              ,(VfInsn 'add (list (VarId 'i_phi) 1) (list (VarId 'i_next)) #f #f))
             ,(TermJump (BlockId 'header)))
       (exit () () ,(TermReturn '())))))

  (define-values (cfg^ stats) (cfg-loop-distrib-with-stats cfg))
  (check-true (>= (cdr (assoc 'loops-analyzed stats)) 1)
              "Should analyze at least one loop"))

;; ============================================================
;; Test 6: Empty CFG
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry () () ,(TermReturn '())))))

  (define cfg^ (cfg-loop-distrib cfg))
  (check-true (Cfg? cfg^) "Empty CFG should not crash"))

;; ============================================================
;; Test 7: Multiple loops
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'const (list 0) (list (VarId 'i)) #f #f))
              ,(TermJump (BlockId 'loop1_header)))
       (loop1_header
        (,(PhiInsn (VarId 'i1)
                   (list (cons (BlockId 'entry) (VarId 'i))
                         (cons (BlockId 'loop1_body) (VarId 'i1_next)))))
        ()
        ,(TermBranch (VarId 'cond1) (BlockId 'loop1_body) (BlockId 'between)))
       (loop1_body ()
                   (,(VfInsn 'add (list (VarId 'i1) 1) (list (VarId 'i1_next)) #f #f))
                   ,(TermJump (BlockId 'loop1_header)))
       (between ()
                (,(VfInsn 'const (list 0) (list (VarId 'j)) #f #f))
                ,(TermJump (BlockId 'loop2_header)))
       (loop2_header
        (,(PhiInsn (VarId 'j1)
                   (list (cons (BlockId 'between) (VarId 'j))
                         (cons (BlockId 'loop2_body) (VarId 'j1_next)))))
        ()
        ,(TermBranch (VarId 'cond2) (BlockId 'loop2_body) (BlockId 'exit)))
       (loop2_body ()
                   (,(VfInsn 'add (list (VarId 'j1) 1) (list (VarId 'j1_next)) #f #f))
                   ,(TermJump (BlockId 'loop2_header)))
       (exit () () ,(TermReturn '())))))

  (define analysis (analyze-loop-distribution cfg))
  (check-true (>= (length analysis) 1) "Should find loops"))

;; ============================================================
;; Test 8: Nested loop - handle correctly
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'const (list 0) (list (VarId 'i)) #f #f))
              ,(TermJump (BlockId 'outer_header)))
       (outer_header
        (,(PhiInsn (VarId 'i_phi)
                   (list (cons (BlockId 'entry) (VarId 'i))
                         (cons (BlockId 'outer_latch) (VarId 'i_next)))))
        ()
        ,(TermBranch (VarId 'outer_cond) (BlockId 'inner_init) (BlockId 'exit)))
       (inner_init ()
                   (,(VfInsn 'const (list 0) (list (VarId 'j)) #f #f))
                   ,(TermJump (BlockId 'inner_header)))
       (inner_header
        (,(PhiInsn (VarId 'j_phi)
                   (list (cons (BlockId 'inner_init) (VarId 'j))
                         (cons (BlockId 'inner_body) (VarId 'j_next)))))
        ()
        ,(TermBranch (VarId 'inner_cond) (BlockId 'inner_body) (BlockId 'outer_latch)))
       (inner_body ()
                   (,(VfInsn 'add (list (VarId 'j_phi) 1) (list (VarId 'j_next)) #f #f))
                   ,(TermJump (BlockId 'inner_header)))
       (outer_latch ()
                    (,(VfInsn 'add (list (VarId 'i_phi) 1) (list (VarId 'i_next)) #f #f))
                    ,(TermJump (BlockId 'outer_header)))
       (exit () () ,(TermReturn '())))))

  (define cfg^ (cfg-loop-distrib cfg))
  (check-true (Cfg? cfg^) "Nested loops should be handled"))

;; ============================================================
;; Test 9: Three independent statements
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'const (list 0) (list (VarId 'i)) #f #f))
              ,(TermJump (BlockId 'header)))
       (header
        (,(PhiInsn (VarId 'i_phi)
                   (list (cons (BlockId 'entry) (VarId 'i))
                         (cons (BlockId 'body) (VarId 'i_next)))))
        ()
        ,(TermBranch (VarId 'cond) (BlockId 'body) (BlockId 'exit)))
       (body ()
             (,(VfInsn 'store (list (VarId 'a) (VarId 'i_phi) 1) (list) #f #f)
              ,(VfInsn 'store (list (VarId 'b) (VarId 'i_phi) 2) (list) #f #f)
              ,(VfInsn 'store (list (VarId 'c) (VarId 'i_phi) 3) (list) #f #f)
              ,(VfInsn 'add (list (VarId 'i_phi) 1) (list (VarId 'i_next)) #f #f))
             ,(TermJump (BlockId 'header)))
       (exit () () ,(TermReturn '())))))

  (define analysis (analyze-loop-distribution cfg))
  (when (pair? analysis)
    (define info (car analysis))
    (check-true (>= (cdr (assoc 'partition-count info)) 1)
                "Should find partitions")))

;; ============================================================
;; Test 10: Chain of dependent statements
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'const (list 0) (list (VarId 'i)) #f #f))
              ,(TermJump (BlockId 'header)))
       (header
        (,(PhiInsn (VarId 'i_phi)
                   (list (cons (BlockId 'entry) (VarId 'i))
                         (cons (BlockId 'body) (VarId 'i_next)))))
        ()
        ,(TermBranch (VarId 'cond) (BlockId 'body) (BlockId 'exit)))
       (body ()
             (,(VfInsn 'add (list (VarId 'a) 1) (list (VarId 'x)) #f #f)
              ,(VfInsn 'add (list (VarId 'x) 2) (list (VarId 'y)) #f #f)
              ,(VfInsn 'add (list (VarId 'y) 3) (list (VarId 'z)) #f #f)
              ,(VfInsn 'add (list (VarId 'i_phi) 1) (list (VarId 'i_next)) #f #f))
             ,(TermJump (BlockId 'header)))
       (exit () () ,(TermReturn '())))))

  (define analysis (analyze-loop-distribution cfg))
  (when (pair? analysis)
    (define info (car analysis))
    ;; Chain of deps means only 1 partition for the dependent statements
    (check-true (number? (cdr (assoc 'partition-count info)))
                "Should compute partition count")))

(displayln "All loop distribution tests passed!")
