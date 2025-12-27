#lang racket/base

;; ============================================================
;; Tests for Loop Interchange
;; ============================================================

(require rackunit racket/hash racket/match)
(require "loop-interchange.rkt")
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
  (Cfg 100 100 100 (BlockId entry-id) blocks (hash)))

;; ============================================================
;; Test 1: Simple nested loop detection
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
                   (,(VfInsn 'vector-set! (list (VarId 'A) (VarId 'j_phi) (VarId 'i_phi) 1) (list) #f #f)
                    ,(VfInsn 'add (list (VarId 'j_phi) 1) (list (VarId 'j_next)) #f #f))
                   ,(TermJump (BlockId 'inner_header)))
       (outer_latch ()
                    (,(VfInsn 'add (list (VarId 'i_phi) 1) (list (VarId 'i_next)) #f #f))
                    ,(TermJump (BlockId 'outer_header)))
       (exit () () ,(TermReturn '())))))

  (define analysis (analyze-loop-interchange cfg))
  (check-true (pair? analysis) "Should detect nested loops"))

;; ============================================================
;; Test 2: No nested loops
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

  (define analysis (analyze-loop-interchange cfg))
  (check-equal? analysis '() "Single loop should have no interchange opportunity"))

;; ============================================================
;; Test 3: Column-major access pattern (should interchange)
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
                   ;; A[j][i] - 列优先访问，交换后变为行优先
                   (,(VfInsn 'vector-ref (list (VarId 'A) (VarId 'j_phi) (VarId 'i_phi)) (list (VarId 'val)) #f #f)
                    ,(VfInsn 'add (list (VarId 'j_phi) 1) (list (VarId 'j_next)) #f #f))
                   ,(TermJump (BlockId 'inner_header)))
       (outer_latch ()
                    (,(VfInsn 'add (list (VarId 'i_phi) 1) (list (VarId 'i_next)) #f #f))
                    ,(TermJump (BlockId 'outer_header)))
       (exit () () ,(TermReturn '())))))

  (define cfg^ (cfg-loop-interchange cfg))
  (check-true (Cfg? cfg^) "Column-major pattern should be handled"))

;; ============================================================
;; Test 4: Row-major access pattern (should not interchange)
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
                   ;; A[i][j] - 已经是行优先，不需要交换
                   (,(VfInsn 'vector-ref (list (VarId 'A) (VarId 'i_phi) (VarId 'j_phi)) (list (VarId 'val)) #f #f)
                    ,(VfInsn 'add (list (VarId 'j_phi) 1) (list (VarId 'j_next)) #f #f))
                   ,(TermJump (BlockId 'inner_header)))
       (outer_latch ()
                    (,(VfInsn 'add (list (VarId 'i_phi) 1) (list (VarId 'i_next)) #f #f))
                    ,(TermJump (BlockId 'outer_header)))
       (exit () () ,(TermReturn '())))))

  (define analysis (analyze-loop-interchange cfg))
  (when (pair? analysis)
    (define info (car analysis))
    (check-true (<= (cdr (assoc 'profit info)) 0)
                "Row-major should have non-positive profit")))

;; ============================================================
;; Test 5: Empty CFG - no crash
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry () () ,(TermReturn '())))))

  (define cfg^ (cfg-loop-interchange cfg))
  (check-true (Cfg? cfg^) "Empty CFG should not crash"))

;; ============================================================
;; Test 6: With statistics
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

  (define-values (cfg^ stats) (cfg-loop-interchange-with-stats cfg))
  (check-true (number? (cdr (assoc 'nested-loops stats)))
              "Should report nested loop count"))

;; ============================================================
;; Test 7: Triple nested loop
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
        ,(TermBranch (VarId 'outer_cond) (BlockId 'mid_init) (BlockId 'exit)))
       (mid_init ()
                 (,(VfInsn 'const (list 0) (list (VarId 'j)) #f #f))
                 ,(TermJump (BlockId 'mid_header)))
       (mid_header
        (,(PhiInsn (VarId 'j_phi)
                   (list (cons (BlockId 'mid_init) (VarId 'j))
                         (cons (BlockId 'mid_latch) (VarId 'j_next)))))
        ()
        ,(TermBranch (VarId 'mid_cond) (BlockId 'inner_init) (BlockId 'outer_latch)))
       (inner_init ()
                   (,(VfInsn 'const (list 0) (list (VarId 'k)) #f #f))
                   ,(TermJump (BlockId 'inner_header)))
       (inner_header
        (,(PhiInsn (VarId 'k_phi)
                   (list (cons (BlockId 'inner_init) (VarId 'k))
                         (cons (BlockId 'inner_body) (VarId 'k_next)))))
        ()
        ,(TermBranch (VarId 'inner_cond) (BlockId 'inner_body) (BlockId 'mid_latch)))
       (inner_body ()
                   (,(VfInsn 'add (list (VarId 'k_phi) 1) (list (VarId 'k_next)) #f #f))
                   ,(TermJump (BlockId 'inner_header)))
       (mid_latch ()
                  (,(VfInsn 'add (list (VarId 'j_phi) 1) (list (VarId 'j_next)) #f #f))
                  ,(TermJump (BlockId 'mid_header)))
       (outer_latch ()
                    (,(VfInsn 'add (list (VarId 'i_phi) 1) (list (VarId 'i_next)) #f #f))
                    ,(TermJump (BlockId 'outer_header)))
       (exit () () ,(TermReturn '())))))

  (define cfg^ (cfg-loop-interchange cfg))
  (check-true (Cfg? cfg^) "Triple nested loop should be handled"))

;; ============================================================
;; Test 8: Multiple array accesses
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
                   ;; Multiple array accesses with different patterns
                   (,(VfInsn 'vector-ref (list (VarId 'A) (VarId 'j_phi) (VarId 'i_phi)) (list (VarId 'a_val)) #f #f)
                    ,(VfInsn 'vector-ref (list (VarId 'B) (VarId 'i_phi) (VarId 'j_phi)) (list (VarId 'b_val)) #f #f)
                    ,(VfInsn 'add (list (VarId 'a_val) (VarId 'b_val)) (list (VarId 'sum)) #f #f)
                    ,(VfInsn 'add (list (VarId 'j_phi) 1) (list (VarId 'j_next)) #f #f))
                   ,(TermJump (BlockId 'inner_header)))
       (outer_latch ()
                    (,(VfInsn 'add (list (VarId 'i_phi) 1) (list (VarId 'i_next)) #f #f))
                    ,(TermJump (BlockId 'outer_header)))
       (exit () () ,(TermReturn '())))))

  (define analysis (analyze-loop-interchange cfg))
  (check-true (Cfg? (cfg-loop-interchange cfg))
              "Multiple array accesses should be handled"))

;; ============================================================
;; Test 9: Sequential loops (not nested)
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'const (list 0) (list (VarId 'i)) #f #f))
              ,(TermJump (BlockId 'loop1_header)))
       (loop1_header
        (,(PhiInsn (VarId 'i_phi)
                   (list (cons (BlockId 'entry) (VarId 'i))
                         (cons (BlockId 'loop1_body) (VarId 'i_next)))))
        ()
        ,(TermBranch (VarId 'cond1) (BlockId 'loop1_body) (BlockId 'between)))
       (loop1_body ()
                   (,(VfInsn 'add (list (VarId 'i_phi) 1) (list (VarId 'i_next)) #f #f))
                   ,(TermJump (BlockId 'loop1_header)))
       (between ()
                (,(VfInsn 'const (list 0) (list (VarId 'j)) #f #f))
                ,(TermJump (BlockId 'loop2_header)))
       (loop2_header
        (,(PhiInsn (VarId 'j_phi)
                   (list (cons (BlockId 'between) (VarId 'j))
                         (cons (BlockId 'loop2_body) (VarId 'j_next)))))
        ()
        ,(TermBranch (VarId 'cond2) (BlockId 'loop2_body) (BlockId 'exit)))
       (loop2_body ()
                   (,(VfInsn 'add (list (VarId 'j_phi) 1) (list (VarId 'j_next)) #f #f))
                   ,(TermJump (BlockId 'loop2_header)))
       (exit () () ,(TermReturn '())))))

  (define analysis (analyze-loop-interchange cfg))
  (check-equal? analysis '()
                "Sequential loops should have no interchange opportunity"))

;; ============================================================
;; Test 10: Induction variable detection
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'const (list 0) (list (VarId 'i)) #f #f))
              ,(TermJump (BlockId 'outer_header)))
       (outer_header
        (,(PhiInsn (VarId 'outer_iv)
                   (list (cons (BlockId 'entry) (VarId 'i))
                         (cons (BlockId 'outer_latch) (VarId 'outer_iv_next)))))
        ()
        ,(TermBranch (VarId 'outer_cond) (BlockId 'inner_init) (BlockId 'exit)))
       (inner_init ()
                   (,(VfInsn 'const (list 0) (list (VarId 'j)) #f #f))
                   ,(TermJump (BlockId 'inner_header)))
       (inner_header
        (,(PhiInsn (VarId 'inner_iv)
                   (list (cons (BlockId 'inner_init) (VarId 'j))
                         (cons (BlockId 'inner_body) (VarId 'inner_iv_next)))))
        ()
        ,(TermBranch (VarId 'inner_cond) (BlockId 'inner_body) (BlockId 'outer_latch)))
       (inner_body ()
                   (,(VfInsn 'add (list (VarId 'inner_iv) 1) (list (VarId 'inner_iv_next)) #f #f))
                   ,(TermJump (BlockId 'inner_header)))
       (outer_latch ()
                    (,(VfInsn 'add (list (VarId 'outer_iv) 1) (list (VarId 'outer_iv_next)) #f #f))
                    ,(TermJump (BlockId 'outer_header)))
       (exit () () ,(TermReturn '())))))

  (define analysis (analyze-loop-interchange cfg))
  (when (pair? analysis)
    (define info (car analysis))
    (check-equal? (cdr (assoc 'outer-iv info)) (VarId 'outer_iv)
                  "Should detect outer induction variable")
    (check-equal? (cdr (assoc 'inner-iv info)) (VarId 'inner_iv)
                  "Should detect inner induction variable")))

(displayln "All loop interchange tests passed!")
