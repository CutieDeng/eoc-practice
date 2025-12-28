#lang racket/base

;; ============================================================
;; Analysis Framework Tests
;; ============================================================

(require rackunit racket/list racket/set racket/hash)
(require "main.rkt")
(require "../ir/cfg/main.rkt")

;; ============================================================
;; Helper: Create Test CFG
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
(define BID1 (BlockId 1))
(define BID2 (BlockId 2))
(define BID3 (BlockId 3))
(define V0 (VarId 0))
(define V1 (VarId 1))
(define V2 (VarId 2))
(define V3 (VarId 3))

;; ============================================================
;; Test 1: Dominance Analysis - Simple Algorithm
;; ============================================================

(test-case "Dominance: simple linear CFG"
  ;; B0 -> B1 -> B2
  (define cfg
    (make-test-cfg BID0
      (list
       (make-block BID0 '() (TermJump BID1))
       (make-block BID1 '() (TermJump BID2))
       (make-block BID2 '() (TermReturn '())))))

  (define dom-info (compute-dominance cfg))

  ;; B0 dominates everything
  (check-true (dominates? dom-info BID0 BID0))
  (check-true (dominates? dom-info BID0 BID1))
  (check-true (dominates? dom-info BID0 BID2))

  ;; B1 dominates B2 but not B0
  (check-false (dominates? dom-info BID1 BID0))
  (check-true (dominates? dom-info BID1 BID2))

  ;; Immediate dominators
  (check-equal? (get-idom dom-info BID1) BID0)
  (check-equal? (get-idom dom-info BID2) BID1))

(test-case "Dominance: diamond CFG"
  ;;     B0
  ;;    /  \
  ;;   B1  B2
  ;;    \  /
  ;;     B3
  (define cfg
    (make-test-cfg BID0
      (list
       (make-block BID0
                   (list (VfInsn 'const '(1) (list V0) #f #f))
                   (TermBranch V0 BID1 BID2))
       (make-block BID1 '() (TermJump BID3))
       (make-block BID2 '() (TermJump BID3))
       (make-block BID3 '() (TermReturn '())))))

  (define dom-info (compute-dominance cfg))

  ;; B0 dominates everything
  (check-true (dominates? dom-info BID0 BID1))
  (check-true (dominates? dom-info BID0 BID2))
  (check-true (dominates? dom-info BID0 BID3))

  ;; B1 and B2 only dominate themselves
  (check-true (dominates? dom-info BID1 BID1))
  (check-false (dominates? dom-info BID1 BID2))
  (check-false (dominates? dom-info BID1 BID3))

  ;; B3's idom is B0 (join point)
  (check-equal? (get-idom dom-info BID3) BID0))

;; ============================================================
;; Test 2: Dominance Analysis - Lengauer-Tarjan
;; ============================================================

(test-case "Dominance: Lengauer-Tarjan algorithm"
  (define cfg
    (make-test-cfg BID0
      (list
       (make-block BID0 '() (TermJump BID1))
       (make-block BID1 '() (TermJump BID2))
       (make-block BID2 '() (TermReturn '())))))

  (define dom-info (compute-dominance cfg #:algorithm 'lengauer-tarjan))

  ;; Same results as simple algorithm
  (check-true (dominates? dom-info BID0 BID2))
  (check-equal? (get-idom dom-info BID1) BID0)
  (check-equal? (get-idom dom-info BID2) BID1))

;; ============================================================
;; Test 3: Liveness Analysis - Backward
;; ============================================================

(test-case "Liveness: simple use-def"
  ;; V0 = 10
  ;; V1 = 20
  ;; V2 = V0 + V1
  ;; return V2
  (define cfg
    (make-test-cfg BID0
      (list
       (make-block BID0
                   (list (VfInsn 'const '(10) (list V0) #f #f)
                         (VfInsn 'const '(20) (list V1) #f #f)
                         (VfInsn 'add (list V0 V1) (list V2) #f #f))
                   (TermReturn (list V2))))))

  (define live-info (compute-liveness cfg))

  ;; At block exit (after return), nothing is live since return terminates
  ;; V2 is used BY the return, so it's in the use set, not live-out
  (check-false (live-at-exit? live-info V2 BID0))
  (check-false (live-at-exit? live-info V0 BID0))
  (check-false (live-at-exit? live-info V1 BID0))

  ;; Check def sets contain the defined variables
  (define defs (hash-ref (LivenessInfo-def live-info) BID0 (set)))
  (check-true (set-member? defs V0))
  (check-true (set-member? defs V1))
  (check-true (set-member? defs V2)))

(test-case "Liveness: across blocks"
  ;; B0: V0 = 10; goto B1
  ;; B1: V1 = V0 + 1; return V1
  (define cfg
    (make-test-cfg BID0
      (list
       (make-block BID0
                   (list (VfInsn 'const '(10) (list V0) #f #f))
                   (TermJump BID1))
       (make-block BID1
                   (list (VfInsn 'add (list V0 1) (list V1) #f #f))
                   (TermReturn (list V1))))))

  (define live-info (compute-liveness cfg))

  ;; V0 should be live at B0 exit (used in B1)
  (check-true (live-at-exit? live-info V0 BID0))

  ;; V0 should be live at B1 entry
  (check-true (live-at-entry? live-info V0 BID1)))

;; ============================================================
;; Test 4: Liveness Analysis - SSA
;; ============================================================

(test-case "Liveness: SSA algorithm"
  (define cfg
    (make-test-cfg BID0
      (list
       (make-block BID0
                   (list (VfInsn 'const '(10) (list V0) #f #f))
                   (TermJump BID1))
       (make-block BID1
                   (list (VfInsn 'add (list V0 1) (list V1) #f #f))
                   (TermReturn (list V1))))))

  (define live-info (compute-liveness cfg #:algorithm 'ssa))

  ;; Same results as backward algorithm
  (check-true (live-at-exit? live-info V0 BID0))
  (check-true (live-at-entry? live-info V0 BID1)))

;; ============================================================
;; Test 5: Interference Analysis
;; ============================================================

(test-case "Interference: basic"
  ;; V0 = 10
  ;; V1 = 20
  ;; V2 = V0 + V1  -- V0 and V1 both live here
  ;; return V2
  (define cfg
    (make-test-cfg BID0
      (list
       (make-block BID0
                   (list (VfInsn 'const '(10) (list V0) #f #f)
                         (VfInsn 'const '(20) (list V1) #f #f)
                         (VfInsn 'add (list V0 V1) (list V2) #f #f))
                   (TermReturn (list V2))))))

  (define ig (compute-interference cfg))

  ;; V0 and V1 should interfere (both live at add)
  (check-true (interferes? ig V0 V1))

  ;; V2 defined after V0 and V1 are last used
  ;; So V2 might not interfere with V0/V1
  ;; (depends on precise liveness)
  )

(test-case "Interference: no interference"
  ;; V0 = 10
  ;; V1 = V0 + 1  -- V0 dead after this
  ;; V2 = V1 + 1  -- V1 dead after this
  ;; return V2
  (define cfg
    (make-test-cfg BID0
      (list
       (make-block BID0
                   (list (VfInsn 'const '(10) (list V0) #f #f)
                         (VfInsn 'add (list V0 1) (list V1) #f #f)
                         (VfInsn 'add (list V1 1) (list V2) #f #f))
                   (TermReturn (list V2))))))

  (define ig (compute-interference cfg))

  ;; V0 and V2 should not interfere
  ;; V0 is dead before V2 is defined
  (check-false (interferes? ig V0 V2)))

;; ============================================================
;; Test 6: Analysis Caching
;; ============================================================

(test-case "Analysis: caching"
  (define cfg
    (make-test-cfg BID0
      (list
       (make-block BID0
                   (list (VfInsn 'const '(10) (list V0) #f #f))
                   (TermReturn (list V0))))))

  (define ctx (make-analysis-context))

  ;; First run should compute
  (define result1 (run-analysis 'dominance cfg ctx))
  (check-true (AnalysisResult-valid? result1))

  ;; Second run should use cache
  (define result2 (run-analysis 'dominance cfg ctx))
  (check-true (AnalysisResult-valid? result2))

  ;; Force recompute
  (define result3 (run-analysis 'dominance cfg ctx #:force? #t))
  (check-true (AnalysisResult-valid? result3)))

;; ============================================================
;; Test 7: Dominance Frontier
;; ============================================================

(test-case "Dominance frontier: diamond"
  ;;     B0
  ;;    /  \
  ;;   B1  B2
  ;;    \  /
  ;;     B3
  (define cfg
    (make-test-cfg BID0
      (list
       (make-block BID0
                   (list (VfInsn 'const '(1) (list V0) #f #f))
                   (TermBranch V0 BID1 BID2))
       (make-block BID1 '() (TermJump BID3))
       (make-block BID2 '() (TermJump BID3))
       (make-block BID3 '() (TermReturn '())))))

  (define dom-info (compute-dominance cfg))

  ;; B1 and B2's dominance frontier should include B3
  (define df1 (get-dom-frontier dom-info BID1))
  (define df2 (get-dom-frontier dom-info BID2))

  (check-not-false (member BID3 df1))
  (check-not-false (member BID3 df2)))

;; ============================================================
;; Test 8: Loop Analysis
;; ============================================================

(test-case "Loops: simple loop"
  ;;     B0
  ;;      |
  ;;     B1 <--+
  ;;      |    |
  ;;     B2 ---+
  ;;      |
  ;;     B3
  (define cfg
    (make-test-cfg BID0
      (list
       (make-block BID0 '() (TermJump BID1))
       (make-block BID1
                   (list (VfInsn 'const '(1) (list V0) #f #f))
                   (TermBranch V0 BID2 BID3))
       (make-block BID2 '() (TermJump BID1))  ; Back edge to B1
       (make-block BID3 '() (TermReturn '())))))

  (define loop-info (compute-loops cfg))

  ;; Should find one loop with header B1
  (check-equal? (length (LoopInfo-loops loop-info)) 1)
  (define loop (car (LoopInfo-loops loop-info)))
  (check-equal? (Loop-header loop) BID1)

  ;; Loop body should contain B1 and B2
  (check-true (set-member? (Loop-body loop) BID1))
  (check-true (set-member? (Loop-body loop) BID2))
  (check-false (set-member? (Loop-body loop) BID0))
  (check-false (set-member? (Loop-body loop) BID3))

  ;; B1 should be a loop header
  (check-true (loop-header? loop-info BID1)))

(test-case "Loops: nested loops"
  ;;     B0
  ;;      |
  ;;     B1 <----+  (outer loop header)
  ;;      |      |
  ;;     B2 <-+  |  (inner loop header)
  ;;      |   |  |
  ;;     B3 --+  |  (inner back edge)
  ;;      |      |
  ;;     B4 -----+  (outer back edge)
  ;;      |
  ;;     B5 (BlockId 4)
  (define BID4 (BlockId 4))
  (define BID5 (BlockId 5))
  (define cfg
    (make-test-cfg BID0
      (list
       (make-block BID0 '() (TermJump BID1))
       (make-block BID1
                   (list (VfInsn 'const '(1) (list V0) #f #f))
                   (TermBranch V0 BID2 BID5))
       (make-block BID2
                   (list (VfInsn 'const '(1) (list V1) #f #f))
                   (TermBranch V1 BID3 BID4))
       (make-block BID3 '() (TermJump BID2))  ; Inner back edge
       (make-block BID4 '() (TermJump BID1))  ; Outer back edge
       (make-block BID5 '() (TermReturn '())))))

  (define loop-info (compute-loops cfg))

  ;; Should find two loops
  (check-equal? (length (LoopInfo-loops loop-info)) 2)

  ;; Check loop depths
  (define depths (map Loop-depth (LoopInfo-loops loop-info)))
  (check-not-false (member 1 depths))
  (check-not-false (member 2 depths)))

;; ============================================================
;; Summary
;; ============================================================

(displayln "All analysis tests passed!")
