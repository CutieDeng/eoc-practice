#lang racket/base

;; ============================================================
;; Tests for Alias Analysis
;; ============================================================

(require rackunit racket/set)
(require "alias-analysis.rkt")
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")

;; ============================================================
;; Test 1: Same local variable - must alias
;; ============================================================
(let ()
  (define loc1 (MemLoc 'local 0 1))
  (define loc2 (MemLoc 'local 0 1))
  (check-equal? (may-alias? loc1 loc2) alias-must
                "Same local variable should must-alias"))

;; ============================================================
;; Test 2: Different local variables - no alias
;; ============================================================
(let ()
  (define loc1 (MemLoc 'local 0 1))
  (define loc2 (MemLoc 'local 1 1))
  (check-equal? (may-alias? loc1 loc2) alias-no
                "Different local variables should not alias"))

;; ============================================================
;; Test 3: Unknown base - may alias
;; ============================================================
(let ()
  (define loc1 (MemLoc 'unknown 0 1))
  (define loc2 (MemLoc 'local 0 1))
  (check-equal? (may-alias? loc1 loc2) alias-may
                "Unknown base should may-alias"))

;; ============================================================
;; Test 4: Same base, unknown offset - may alias
;; ============================================================
(let ()
  (define var (VarId 'arr))
  (define loc1 (MemLoc var 'unknown 1))
  (define loc2 (MemLoc var 0 1))
  (check-equal? (may-alias? loc1 loc2) alias-may
                "Unknown offset should may-alias"))

;; ============================================================
;; Test 5: Same base, same offset - must alias
;; ============================================================
(let ()
  (define var (VarId 'arr))
  (define loc1 (MemLoc var 5 1))
  (define loc2 (MemLoc var 5 1))
  (check-equal? (may-alias? loc1 loc2) alias-must
                "Same base and offset should must-alias"))

;; ============================================================
;; Test 6: Same base, non-overlapping ranges - no alias
;; ============================================================
(let ()
  (define var (VarId 'arr))
  (define loc1 (MemLoc var 0 4))  ; bytes 0-3
  (define loc2 (MemLoc var 10 4)) ; bytes 10-13
  (check-equal? (may-alias? loc1 loc2) alias-no
                "Non-overlapping ranges should not alias"))

;; ============================================================
;; Test 7: Memory operation classification
;; ============================================================
(let ()
  (check-true (memory-read-op? 'load) "load is a read op")
  (check-true (memory-read-op? 'vector-ref) "vector-ref is a read op")
  (check-true (memory-read-op? 'getfield) "getfield is a read op")
  (check-false (memory-read-op? 'store) "store is not a read op")

  (check-true (memory-write-op? 'store) "store is a write op")
  (check-true (memory-write-op? 'vector-set!) "vector-set! is a write op")
  (check-true (memory-write-op? 'putfield) "putfield is a write op")
  (check-false (memory-write-op? 'load) "load is not a write op"))

;; ============================================================
;; Test 8: Side effect detection
;; ============================================================
(let ()
  (check-true (has-side-effect? 'store) "store has side effect")
  (check-true (has-side-effect? 'call) "call has side effect")
  (check-true (has-side-effect? 'new) "new has side effect")
  (check-false (has-side-effect? 'add) "add has no side effect")
  (check-false (has-side-effect? 'load) "load has no side effect"))

;; ============================================================
;; Test 9: Instruction memory location extraction
;; ============================================================
(let ()
  (define insn1 (VfInsn 'load-local (list 5) (list (VarId 'x)) #f #f))
  (define loc1 (insn-memory-loc insn1))
  (check-equal? (MemLoc-base loc1) 'local)
  (check-equal? (MemLoc-offset loc1) 5)

  (define arr (VarId 'arr))
  (define insn2 (VfInsn 'vector-ref (list arr 3) (list (VarId 'y)) #f #f))
  (define loc2 (insn-memory-loc insn2))
  (check-equal? (MemLoc-base loc2) arr)
  (check-equal? (MemLoc-offset loc2) 3))

;; ============================================================
;; Test 10: Store kills detection
;; ============================================================
(let ()
  ;; Same location - kills
  (define insn1 (VfInsn 'store-local (list (VarId 'x) 5) '() #f #f))
  (define insn2 (VfInsn 'store-local (list (VarId 'y) 5) '() #f #f))
  (check-true (store-kills? insn1 insn2)
              "Store to same local should kill")

  ;; Different locations - doesn't kill
  (define insn3 (VfInsn 'store-local (list (VarId 'x) 3) '() #f #f))
  (check-false (store-kills? insn1 insn3)
               "Store to different local should not kill"))

;; ============================================================
;; Test 11: Points-to analysis operations
;; ============================================================
(let ()
  (define pt0 (points-to-empty))
  (define var (VarId 'p))
  (define pt1 (points-to-add pt0 var 'obj1))
  (define pt2 (points-to-add pt1 var 'obj2))

  (check-equal? (set-count (points-to-get pt0 var)) 0
                "Empty points-to should have no targets")
  (check-equal? (set-count (points-to-get pt1 var)) 1
                "After one add should have one target")
  (check-equal? (set-count (points-to-get pt2 var)) 2
                "After two adds should have two targets")
  (check-true (set-member? (points-to-get pt2 var) 'obj1))
  (check-true (set-member? (points-to-get pt2 var) 'obj2)))

;; ============================================================
;; Test 12: Points-to merge
;; ============================================================
(let ()
  (define pt1 (points-to-add (points-to-empty) (VarId 'p) 'obj1))
  (define pt2 (points-to-add (points-to-empty) (VarId 'p) 'obj2))
  (define pt3 (points-to-merge pt1 pt2))

  (check-equal? (set-count (points-to-get pt3 (VarId 'p))) 2
                "Merged points-to should have both targets"))

(displayln "All alias analysis tests passed!")
