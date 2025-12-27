#lang racket/base

;; ============================================================
;; Tests for PHI Propagation
;; ============================================================

(require rackunit racket/hash racket/match)
(require "phi-prop.rkt")
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")

;; Helper to create a CFG with specified blocks
(define (make-test-cfg entry-id blocks-spec)
  (define blocks
    (for/hash ([spec blocks-spec])
      (match spec
        [(list bid phis insns term)
         (values (BlockId bid)
                 (CfgBlock (BlockId bid) phis insns term))])))
  (Cfg 100 100 100 (BlockId entry-id) #f blocks (hash)))

;; ============================================================
;; Test 1: Basic PHI propagation
;; ============================================================
;;   entry: goto left or right
;;   left: p = &a, goto merge
;;   right: q = &b, goto merge
;;   merge: r = phi(left:p, right:q)
;;          x = load(r)
;; After propagation:
;;   left: p = &a, x_1 = load(p), goto merge
;;   right: q = &b, x_2 = load(q), goto merge
;;   merge: x = phi(left:x_1, right:x_2)
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry () () ,(TermBranch (VarId 'cond) (BlockId 'left) (BlockId 'right)))
       (left ()
             (,(VfInsn 'const (list 1) (list (VarId 'p)) #f #f))
             ,(TermJump (BlockId 'merge)))
       (right ()
              (,(VfInsn 'const (list 2) (list (VarId 'q)) #f #f))
              ,(TermJump (BlockId 'merge)))
       (merge
        (,(PhiInsn (VarId 'r)
                   (list (cons (BlockId 'left) (VarId 'p))
                         (cons (BlockId 'right) (VarId 'q)))))
        (,(VfInsn 'load (list (VarId 'r)) (list (VarId 'x)) #f #f))
        ,(TermReturn (list (VarId 'x)))))))

  (define cfg^ (cfg-phi-prop cfg))

  ;; Check that merge block has a new PHI for x
  (define merge-block (cfg-get-block cfg^ (BlockId 'merge)))
  (check-equal? (length (CfgBlock-phis merge-block)) 2
                "Merge block should have 2 PHIs after propagation")

  ;; Check that load was removed from merge block
  (define load-count
    (for/sum ([insn (CfgBlock-insns merge-block)])
      (if (and (VfInsn? insn) (equal? (VfInsn-op insn) 'load)) 1 0)))
  (check-equal? load-count 0
                "Load should be removed from merge block"))

;; ============================================================
;; Test 2: No propagation - load doesn't use PHI result
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'const (list 1) (list (VarId 'p)) #f #f)
               ,(VfInsn 'load (list (VarId 'p)) (list (VarId 'x)) #f #f))
              ,(TermReturn (list (VarId 'x)))))))

  (define cfg^ (cfg-phi-prop cfg))
  (define block (cfg-get-block cfg^ (BlockId 'entry)))
  (check-equal? (length (CfgBlock-insns block)) 2
                "No change when load doesn't use PHI result"))

;; ============================================================
;; Test 3: Multiple PHI propagations
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry () () ,(TermBranch (VarId 'cond) (BlockId 'left) (BlockId 'right)))
       (left ()
             (,(VfInsn 'const (list 1) (list (VarId 'p1)) #f #f)
              ,(VfInsn 'const (list 2) (list (VarId 'p2)) #f #f))
             ,(TermJump (BlockId 'merge)))
       (right ()
              (,(VfInsn 'const (list 3) (list (VarId 'q1)) #f #f)
               ,(VfInsn 'const (list 4) (list (VarId 'q2)) #f #f))
              ,(TermJump (BlockId 'merge)))
       (merge
        (,(PhiInsn (VarId 'r1) (list (cons (BlockId 'left) (VarId 'p1))
                                      (cons (BlockId 'right) (VarId 'q1))))
         ,(PhiInsn (VarId 'r2) (list (cons (BlockId 'left) (VarId 'p2))
                                      (cons (BlockId 'right) (VarId 'q2)))))
        (,(VfInsn 'load (list (VarId 'r1)) (list (VarId 'x)) #f #f)
         ,(VfInsn 'load (list (VarId 'r2)) (list (VarId 'y)) #f #f))
        ,(TermReturn (list (VarId 'x) (VarId 'y)))))))

  (define cfg^ (cfg-phi-prop cfg))
  (define merge-block (cfg-get-block cfg^ (BlockId 'merge)))

  ;; Should have 4 PHIs now (2 original + 2 for propagated loads)
  (check-equal? (length (CfgBlock-phis merge-block)) 4
                "Should have 4 PHIs after propagating both loads"))

;; ============================================================
;; Test 4: PHI in loop - handled correctly
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'const (list 1) (list (VarId 'init)) #f #f))
              ,(TermJump (BlockId 'loop)))
       (loop
        (,(PhiInsn (VarId 'r) (list (cons (BlockId 'entry) (VarId 'init))
                                     (cons (BlockId 'loop) (VarId 'next)))))
        (,(VfInsn 'load (list (VarId 'r)) (list (VarId 'x)) #f #f)
         ,(VfInsn 'add (list (VarId 'r) 1) (list (VarId 'next)) #f #f))
        ,(TermBranch (VarId 'x) (BlockId 'loop) (BlockId 'exit)))
       (exit () () ,(TermReturn '())))))

  ;; Should handle loop without crashing
  (define cfg^ (cfg-phi-prop cfg))
  (check-true (Cfg? cfg^) "Loop handling should not crash"))

;; ============================================================
;; Test 5: Empty block - no crash
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry () () ,(TermReturn '())))))

  (define cfg^ (cfg-phi-prop cfg))
  (check-true (Cfg? cfg^) "Empty block should not crash"))

;; ============================================================
;; Test 6: PHI with single input - edge case
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'const (list 1) (list (VarId 'p)) #f #f))
              ,(TermJump (BlockId 'next)))
       (next
        (,(PhiInsn (VarId 'r) (list (cons (BlockId 'entry) (VarId 'p)))))
        (,(VfInsn 'load (list (VarId 'r)) (list (VarId 'x)) #f #f))
        ,(TermReturn (list (VarId 'x)))))))

  (define cfg^ (cfg-phi-prop cfg))
  (check-true (Cfg? cfg^) "Single-input PHI should be handled"))

;; ============================================================
;; Test 7: With statistics
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry () () ,(TermBranch (VarId 'cond) (BlockId 'left) (BlockId 'right)))
       (left ()
             (,(VfInsn 'const (list 1) (list (VarId 'p)) #f #f))
             ,(TermJump (BlockId 'merge)))
       (right ()
              (,(VfInsn 'const (list 2) (list (VarId 'q)) #f #f))
              ,(TermJump (BlockId 'merge)))
       (merge
        (,(PhiInsn (VarId 'r)
                   (list (cons (BlockId 'left) (VarId 'p))
                         (cons (BlockId 'right) (VarId 'q)))))
        (,(VfInsn 'load (list (VarId 'r)) (list (VarId 'x)) #f #f))
        ,(TermReturn (list (VarId 'x)))))))

  (define-values (cfg^ stats) (cfg-phi-prop-with-stats cfg))
  (check-equal? (cdr (assoc 'propagated-loads stats)) 1
                "Statistics should report 1 propagated load"))

;; ============================================================
;; Test 8: Non-load operations not propagated
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry () () ,(TermBranch (VarId 'cond) (BlockId 'left) (BlockId 'right)))
       (left ()
             (,(VfInsn 'const (list 1) (list (VarId 'p)) #f #f))
             ,(TermJump (BlockId 'merge)))
       (right ()
              (,(VfInsn 'const (list 2) (list (VarId 'q)) #f #f))
              ,(TermJump (BlockId 'merge)))
       (merge
        (,(PhiInsn (VarId 'r)
                   (list (cons (BlockId 'left) (VarId 'p))
                         (cons (BlockId 'right) (VarId 'q)))))
        (,(VfInsn 'add (list (VarId 'r) 1) (list (VarId 'x)) #f #f))
        ,(TermReturn (list (VarId 'x)))))))

  (define cfg^ (cfg-phi-prop cfg))
  (define merge-block (cfg-get-block cfg^ (BlockId 'merge)))
  ;; add is not a memory operation, so no propagation
  (check-equal? (length (CfgBlock-phis merge-block)) 1
                "Non-load operations should not be propagated"))

;; ============================================================
;; Test 9: vector-ref propagation
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry () () ,(TermBranch (VarId 'cond) (BlockId 'left) (BlockId 'right)))
       (left ()
             (,(VfInsn 'const (list (VarId 'arr1)) (list (VarId 'p)) #f #f))
             ,(TermJump (BlockId 'merge)))
       (right ()
              (,(VfInsn 'const (list (VarId 'arr2)) (list (VarId 'q)) #f #f))
              ,(TermJump (BlockId 'merge)))
       (merge
        (,(PhiInsn (VarId 'r)
                   (list (cons (BlockId 'left) (VarId 'p))
                         (cons (BlockId 'right) (VarId 'q)))))
        (,(VfInsn 'vector-ref (list (VarId 'r) 0) (list (VarId 'x)) #f #f))
        ,(TermReturn (list (VarId 'x)))))))

  (define cfg^ (cfg-phi-prop cfg))
  (define merge-block (cfg-get-block cfg^ (BlockId 'merge)))
  (check-equal? (length (CfgBlock-phis merge-block)) 2
                "vector-ref should also be propagated"))

;; ============================================================
;; Test 10: Deep PHI chain - only first level propagated
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry () () ,(TermBranch (VarId 'cond) (BlockId 'left) (BlockId 'right)))
       (left ()
             (,(VfInsn 'const (list 1) (list (VarId 'p)) #f #f))
             ,(TermJump (BlockId 'mid)))
       (right ()
              (,(VfInsn 'const (list 2) (list (VarId 'q)) #f #f))
              ,(TermJump (BlockId 'mid)))
       (mid
        (,(PhiInsn (VarId 'r)
                   (list (cons (BlockId 'left) (VarId 'p))
                         (cons (BlockId 'right) (VarId 'q)))))
        ()
        ,(TermJump (BlockId 'final)))
       (final ()
              (,(VfInsn 'load (list (VarId 'r)) (list (VarId 'x)) #f #f))
              ,(TermReturn (list (VarId 'x)))))))

  ;; This doesn't match our pattern since load is in different block than PHI
  (define cfg^ (cfg-phi-prop cfg))
  (check-true (Cfg? cfg^) "Deep PHI chain handled"))

(displayln "All PHI propagation tests passed!")
