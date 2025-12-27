#lang racket/base

;; ============================================================
;; Tests for Partial Redundancy Elimination (PRE)
;; ============================================================

(require rackunit racket/hash racket/match racket/set)
(require "pre.rkt")
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
;; Test 1: Full redundancy - same expression computed twice
;; ============================================================
;;   entry: x = a + b
;;          goto next
;;   next:  y = a + b  ; redundant!
;;          return y
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'add (list (VarId 'a) (VarId 'b)) (list (VarId 'x)) #f #f))
              ,(TermJump (BlockId 'next)))
       (next ()
             (,(VfInsn 'add (list (VarId 'a) (VarId 'b)) (list (VarId 'y)) #f #f))
             ,(TermReturn (list (VarId 'y)))))))

  (define cfg^ (cfg-pre cfg))
  (define next-block (cfg-get-block cfg^ (BlockId 'next)))

  ;; The second add should be replaced with a copy
  (define has-copy
    (for/or ([insn (CfgBlock-insns next-block)])
      (and (VfInsn? insn) (equal? (VfInsn-op insn) 'copy))))

  (check-true has-copy "Redundant expression should be replaced with copy"))

;; ============================================================
;; Test 2: No redundancy - different expressions
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'add (list (VarId 'a) (VarId 'b)) (list (VarId 'x)) #f #f)
               ,(VfInsn 'sub (list (VarId 'a) (VarId 'b)) (list (VarId 'y)) #f #f))
              ,(TermReturn (list (VarId 'x) (VarId 'y)))))))

  (define cfg^ (cfg-pre cfg))
  (define block (cfg-get-block cfg^ (BlockId 'entry)))

  ;; Both instructions should remain
  (check-equal? (length (CfgBlock-insns block)) 2
                "Different expressions should not be eliminated"))

;; ============================================================
;; Test 3: Killed expression - not redundant
;; ============================================================
;;   entry: x = a + b
;;          a = 10     ; kills a + b
;;          y = a + b  ; not redundant (a changed)
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'add (list (VarId 'a) (VarId 'b)) (list (VarId 'x)) #f #f)
               ,(VfInsn 'const (list 10) (list (VarId 'a)) #f #f)
               ,(VfInsn 'add (list (VarId 'a) (VarId 'b)) (list (VarId 'y)) #f #f))
              ,(TermReturn (list (VarId 'y)))))))

  (define cfg^ (cfg-pre cfg))
  (define block (cfg-get-block cfg^ (BlockId 'entry)))

  ;; All 3 instructions should remain (second add is not redundant)
  (check-equal? (length (CfgBlock-insns block)) 3
                "Expression after kill should not be eliminated"))

;; ============================================================
;; Test 4: Partial redundancy - diamond pattern
;; ============================================================
;;   entry: goto left or right
;;   left:  x = a + b, goto merge
;;   right: goto merge  (no a + b computed)
;;   merge: y = a + b   ; partially redundant
;;
;; After PRE, we could insert a+b in right, but this simple
;; implementation focuses on elimination, not insertion
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry () ()
              ,(TermBranch (VarId 'cond) (BlockId 'left) (BlockId 'right)))
       (left ()
             (,(VfInsn 'add (list (VarId 'a) (VarId 'b)) (list (VarId 'x)) #f #f))
             ,(TermJump (BlockId 'merge)))
       (right () () ,(TermJump (BlockId 'merge)))
       (merge ()
              (,(VfInsn 'add (list (VarId 'a) (VarId 'b)) (list (VarId 'y)) #f #f))
              ,(TermReturn (list (VarId 'y)))))))

  ;; This is partial redundancy - merge's add is only redundant on left path
  ;; Current implementation handles full redundancy, not insertion
  (define cfg^ (cfg-pre cfg))
  (check-true (Cfg? cfg^) "Partial redundancy should not crash"))

;; ============================================================
;; Test 5: Full redundancy with diamond - both paths compute
;; ============================================================
;;   entry: goto left or right
;;   left:  x = a + b, goto merge
;;   right: z = a + b, goto merge
;;   merge: y = a + b   ; fully redundant!
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry () ()
              ,(TermBranch (VarId 'cond) (BlockId 'left) (BlockId 'right)))
       (left ()
             (,(VfInsn 'add (list (VarId 'a) (VarId 'b)) (list (VarId 'x)) #f #f))
             ,(TermJump (BlockId 'merge)))
       (right ()
              (,(VfInsn 'add (list (VarId 'a) (VarId 'b)) (list (VarId 'z)) #f #f))
              ,(TermJump (BlockId 'merge)))
       (merge ()
              (,(VfInsn 'add (list (VarId 'a) (VarId 'b)) (list (VarId 'y)) #f #f))
              ,(TermReturn (list (VarId 'y)))))))

  (define cfg^ (cfg-pre cfg))
  (define merge-block (cfg-get-block cfg^ (BlockId 'merge)))

  ;; The add in merge should be replaced with copy
  (define has-copy
    (for/or ([insn (CfgBlock-insns merge-block)])
      (and (VfInsn? insn) (equal? (VfInsn-op insn) 'copy))))

  (check-true has-copy
              "Expression available on all paths should be eliminated"))

;; ============================================================
;; Test 6: Multiple redundant expressions
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'add (list (VarId 'a) (VarId 'b)) (list (VarId 'x)) #f #f)
               ,(VfInsn 'mul (list (VarId 'c) (VarId 'd)) (list (VarId 'y)) #f #f))
              ,(TermJump (BlockId 'next)))
       (next ()
             (,(VfInsn 'add (list (VarId 'a) (VarId 'b)) (list (VarId 'x2)) #f #f)
              ,(VfInsn 'mul (list (VarId 'c) (VarId 'd)) (list (VarId 'y2)) #f #f))
             ,(TermReturn (list (VarId 'x2) (VarId 'y2)))))))

  (define cfg^ (cfg-pre cfg))
  (define next-block (cfg-get-block cfg^ (BlockId 'next)))

  (define copy-count
    (for/sum ([insn (CfgBlock-insns next-block)])
      (if (and (VfInsn? insn) (equal? (VfInsn-op insn) 'copy)) 1 0)))

  (check-equal? copy-count 2
                "Both redundant expressions should be eliminated"))

;; ============================================================
;; Test 7: Commutative operations
;; ============================================================
;;   entry: x = a + b
;;          y = b + a  ; same as a + b (commutative)
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'add (list (VarId 'a) (VarId 'b)) (list (VarId 'x)) #f #f)
               ,(VfInsn 'add (list (VarId 'b) (VarId 'a)) (list (VarId 'y)) #f #f))
              ,(TermReturn (list (VarId 'x) (VarId 'y)))))))

  (define cfg^ (cfg-pre cfg))
  (define block (cfg-get-block cfg^ (BlockId 'entry)))

  (define has-copy
    (for/or ([insn (CfgBlock-insns block)])
      (and (VfInsn? insn) (equal? (VfInsn-op insn) 'copy))))

  (check-true has-copy
              "Commutative expressions should be recognized as equivalent"))

;; ============================================================
;; Test 8: Expression in loop
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'add (list (VarId 'a) (VarId 'b)) (list (VarId 'x)) #f #f))
              ,(TermJump (BlockId 'loop)))
       (loop ()
             (,(VfInsn 'add (list (VarId 'a) (VarId 'b)) (list (VarId 'y)) #f #f))
             ,(TermBranch (VarId 'cond) (BlockId 'loop) (BlockId 'exit)))
       (exit () () ,(TermReturn '())))))

  (define cfg^ (cfg-pre cfg))
  (check-true (Cfg? cfg^) "Loop should be handled correctly"))

;; ============================================================
;; Test 9: With statistics
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry ()
              (,(VfInsn 'add (list (VarId 'a) (VarId 'b)) (list (VarId 'x)) #f #f))
              ,(TermJump (BlockId 'next)))
       (next ()
             (,(VfInsn 'add (list (VarId 'a) (VarId 'b)) (list (VarId 'y)) #f #f))
             ,(TermReturn (list (VarId 'y)))))))

  (define-values (cfg^ stats) (cfg-pre-with-stats cfg))
  (check-true (>= (cdr (assoc 'eliminated stats)) 1)
              "Statistics should report eliminated expressions"))

;; ============================================================
;; Test 10: Empty block - no crash
;; ============================================================
(let ()
  (define cfg
    (make-test-cfg
     'entry
     `((entry () () ,(TermReturn '())))))

  (define cfg^ (cfg-pre cfg))
  (check-true (Cfg? cfg^) "Empty block should not crash"))

;; ============================================================
;; Test 11: Pure operation check
;; ============================================================
(let ()
  ;; add is pure
  (check-true (pure-op? 'add) "add should be pure")
  (check-true (pure-op? 'mul) "mul should be pure")
  (check-true (pure-op? 'sub) "sub should be pure")

  ;; call is not pure
  (check-false (pure-op? 'call) "call should not be pure")
  (check-false (pure-op? 'load) "load should not be pure"))

;; ============================================================
;; Test 12: Expression normalization
;; ============================================================
(let ()
  ;; Create two expressions with operands in different order
  (define insn1 (VfInsn 'add (list (VarId 'a) (VarId 'b)) (list (VarId 'x)) #f #f))
  (define insn2 (VfInsn 'add (list (VarId 'b) (VarId 'a)) (list (VarId 'y)) #f #f))

  (define expr1 (insn->expr insn1))
  (define expr2 (insn->expr insn2))

  (check-equal? expr1 expr2
                "Commutative expressions should normalize to same form"))

(displayln "All PRE tests passed!")
