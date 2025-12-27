#lang racket/base

;; ============================================================
;; Framework Tests
;; ============================================================

(require rackunit racket/list racket/hash)
(require "main.rkt")
(require "passes/const-fold.rkt")
(require "passes/dce.rkt")
(require "../../ir/cfg/main.rkt")

;; ============================================================
;; Helper: Create test CFG
;; ============================================================

(define (make-test-cfg entry blocks)
  (define block-table
    (for/hash ([block blocks])
      (values (CfgBlock-id block) block)))
  (Cfg (hash-count block-table)
       100
       (for/sum ([block blocks]) (length (CfgBlock-insns block)))
       entry
       #f
       block-table
       (hash)))

(define (make-block id insns term)
  (CfgBlock id '() insns term))

(define BID0 (BlockId 0))
(define V0 (VarId 0))
(define V1 (VarId 1))
(define V2 (VarId 2))

;; ============================================================
;; Test 1: Semantics Query
;; ============================================================

(test-case "Semantics: memory effect"
  (define insn1 (VfInsn 'add (list V0 V1) (list V2) #f #f))
  (define insn2 (VfInsn 'iload (list 0) (list V0) #f #f))
  (define insn3 (VfInsn 'istore (list V0 1) '() #f #f))

  (check-equal? (sem-memory-effect jvm-semantics insn1) 'none)
  (check-equal? (sem-memory-effect jvm-semantics insn2) 'read)
  (check-equal? (sem-memory-effect jvm-semantics insn3) 'write))

(test-case "Semantics: purity"
  (define pure-insn (VfInsn 'add (list V0 V1) (list V2) #f #f))
  (define impure-insn (VfInsn 'call (list 'foo V0) (list V1) #f #f))

  (check-true (sem-pure? jvm-semantics pure-insn))
  (check-false (sem-pure? jvm-semantics impure-insn)))

(test-case "Semantics: commutative"
  (define add-insn (VfInsn 'add (list V0 V1) (list V2) #f #f))
  (define sub-insn (VfInsn 'sub (list V0 V1) (list V2) #f #f))

  (check-true (sem-commutative? jvm-semantics add-insn))
  (check-false (sem-commutative? jvm-semantics sub-insn)))

;; ============================================================
;; Test 2: Pass Context
;; ============================================================

(test-case "Context: statistics"
  (define ctx (make-context jvm-semantics))
  (ctx-incr-stat! ctx 'test-counter)
  (ctx-incr-stat! ctx 'test-counter)
  (ctx-incr-stat! ctx 'test-counter 5)

  (define stats (ctx-get-stats ctx))
  (check-equal? (hash-ref stats 'test-counter) 7))

;; ============================================================
;; Test 3: Constant Evaluation
;; ============================================================

(test-case "Const eval: basic arithmetic"
  (define add-insn (VfInsn 'add (list 10 20) (list V0) #f #f))
  (define result (sem-const-eval jvm-semantics add-insn (lambda (_) #f)))
  (check-equal? result 30))

(test-case "Const eval: with variables"
  (define add-insn (VfInsn 'add (list V0 V1) (list V2) #f #f))
  (define var->const
    (lambda (v)
      (cond
        [(equal? v V0) 5]
        [(equal? v V1) 3]
        [else #f])))
  (define result (sem-const-eval jvm-semantics add-insn var->const))
  (check-equal? result 8))

;; ============================================================
;; Test 4: Alias Analysis
;; ============================================================

(test-case "Alias: local variables"
  (define loc1 (MemLoc 'local 'locals 0 1))
  (define loc2 (MemLoc 'local 'locals 0 1))
  (define loc3 (MemLoc 'local 'locals 1 1))

  (check-equal? (locs-may-alias? loc1 loc2) 'must-alias)
  (check-equal? (locs-may-alias? loc1 loc3) 'no-alias))

(test-case "Alias: different kinds"
  (define local-loc (MemLoc 'local 'locals 0 1))
  (define heap-loc (MemLoc 'heap V0 0 1))

  (check-equal? (locs-may-alias? local-loc heap-loc) 'no-alias))

;; ============================================================
;; Test 5: Constant Folding Pass
;; ============================================================

(test-case "Const-fold pass"
  (define cfg
    (make-test-cfg BID0
      (list
       (make-block BID0
                   (list (VfInsn 'const (list 10) (list V0) #f #f)
                         (VfInsn 'const (list 20) (list V1) #f #f)
                         (VfInsn 'add (list V0 V1) (list V2) #f #f))
                   (TermReturn (list V2))))))

  (define cfg^ (cfg-const-fold/sem cfg jvm-semantics))
  (define block (cfg-get-block cfg^ BID0))
  (define insns (CfgBlock-insns block))

  ;; Should have folded add to const
  (check-equal? (length insns) 3)
  (define last-insn (third insns))
  (check-equal? (VfInsn-op last-insn) 'const)
  (check-equal? (VfInsn-inputs last-insn) '(30)))

;; ============================================================
;; Test 6: DCE Pass
;; ============================================================

(test-case "DCE pass"
  (define cfg
    (make-test-cfg BID0
      (list
       (make-block BID0
                   (list (VfInsn 'const (list 10) (list V0) #f #f)
                         (VfInsn 'const (list 20) (list V1) #f #f)  ; unused
                         (VfInsn 'add (list V0 5) (list V2) #f #f))
                   (TermReturn (list V2))))))

  (define cfg^ (cfg-dce/sem cfg jvm-semantics))
  (define block (cfg-get-block cfg^ BID0))
  (define insns (CfgBlock-insns block))

  ;; V1 is unused, should be eliminated
  (check-equal? (length insns) 2))

;; ============================================================
;; Test 7: Pass Composition
;; ============================================================

(test-case "Pass composition"
  (define combined (compose-passes const-fold-pass dce-pass))
  (check-equal? (OptPass-name combined) 'const-fold+dce))

;; ============================================================
;; Summary
;; ============================================================

(displayln "All framework tests passed!")
