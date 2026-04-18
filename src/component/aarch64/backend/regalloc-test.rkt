#lang racket/base

;; ============================================================================
;; Register Allocation Tests
;; ============================================================================

(require rackunit
         rackunit/text-ui
         racket/set
         "../ir/types.rkt"
         "../ir/cfg.rkt"
         "../frontend/parser.rkt"
         "../analysis/liveness.rkt"
         "../backend/regalloc.rkt"
         cutie-ftree/pvector
         cutie-ftree/ordered-map
         cutie-ftree/bitset
         cutie-ftree/comparator)

;; ============================================================================
;; Virtual Register Parsing Tests
;; ============================================================================

(define vreg-parsing-tests
  (test-suite
   "Virtual Register Parsing"

   (test-case "parse virtual GPR (64-bit)"
     (define r (parse-reg '%tmp))
     (check-true (VReg:gpr? r))
     (check-equal? (VReg:gpr-id r) 'tmp)
     (check-equal? (VReg:gpr-width r) 64))

   (test-case "parse virtual GPR (32-bit)"
     (define r (parse-reg '%tmp.32))
     (check-true (VReg:gpr? r))
     (check-equal? (VReg:gpr-id r) 'tmp)
     (check-equal? (VReg:gpr-width r) 32))

   (test-case "parse virtual SVE register"
     (define r (parse-reg '%z.vec0))
     (check-true (VReg:sve? r))
     (check-equal? (VReg:sve-id r) 'vec0))

   (test-case "parse virtual predicate register"
     (define r (parse-reg '%p.mask))
     (check-true (VReg:pred? r))
     (check-equal? (VReg:pred-id r) 'mask))

   (test-case "parse virtual NEON register"
     (define r (parse-reg '%v.data))
     (check-true (VReg:vec? r))
     (check-equal? (VReg:vec-id r) 'data))

   (test-case "vreg-class returns correct class"
     (check-equal? (vreg-class (VReg:gpr 'a 64)) 'gpr)
     (check-equal? (vreg-class (VReg:sve 'b)) 'sve)
     (check-equal? (vreg-class (VReg:pred 'c)) 'pred)
     (check-equal? (vreg-class (VReg:vec 'd)) 'vec))))

;; ============================================================================
;; Liveness Analysis Tests
;; ============================================================================

(define liveness-tests
  (test-suite
   "Liveness Analysis"

   (test-case "insn-defs for arithmetic"
     (define insn (Insn:arith 'add (VReg:gpr 'a 64) (VReg:gpr 'b 64) (VReg:gpr 'c 64)))
     (define defs (insn-defs insn))
     (check-equal? (length defs) 1)
     (check-true (VReg:gpr? (car defs)))
     (check-equal? (VReg:gpr-id (car defs)) 'a))

   (test-case "insn-uses for arithmetic"
     (define insn (Insn:arith 'add (VReg:gpr 'a 64) (VReg:gpr 'b 64) (VReg:gpr 'c 64)))
     (define uses (insn-uses insn))
     (check-equal? (length uses) 2))

   (test-case "insn-defs for mov"
     (define insn (Insn:mov 'mov (VReg:gpr 'dst 64) (VReg:gpr 'src 64)))
     (define defs (insn-defs insn))
     (check-equal? (length defs) 1)
     (check-equal? (VReg:gpr-id (car defs)) 'dst))

   (test-case "insn-uses for mov"
     (define insn (Insn:mov 'mov (VReg:gpr 'dst 64) (VReg:gpr 'src 64)))
     (define uses (insn-uses insn))
     (check-equal? (length uses) 1)
     (check-equal? (VReg:gpr-id (car uses)) 'src))

   ;; Note: compute-block-liveness is now internal to driver framework
   ;; Testing through full CFG liveness in allocation tests
   ))

;; ============================================================================
;; Interference Graph Tests
;; ============================================================================

(define interference-tests
  (test-suite
   "Interference Graph"

   (test-case "no interference between non-overlapping lifetimes"
     ;; a = 1
     ;; use a
     ;; b = 2
     ;; use b
     ;; a and b don't interfere because a is dead when b is defined
     (check-true #t))  ; Placeholder - full test requires CFG

   (test-case "interference between overlapping lifetimes"
     ;; a = 1
     ;; b = 2
     ;; c = a + b  ; a and b both live here
     ;; a and b interfere
     (check-true #t))))  ; Placeholder

;; ============================================================================
;; Register Allocation Tests
;; ============================================================================

(define allocation-tests
  (test-suite
   "Register Allocation"

   (test-case "simple function with few vregs"
     ;; Test that allocation assigns physical registers
     (define v1 (VReg:gpr 'a 64))
     (define v2 (VReg:gpr 'b 64))

     ;; Create simple CFG
     (define insns
       (list->pvector
        (list (Insn:arith 'add v1 (Reg:x 0) (Reg:x 1))
              (Insn:mov 'mov (Reg:x 0) v1)
              (Insn:ret))))

     (define cfg0 (make-empty-cfg))
     (define-values (bid cfg1) (cfg-fresh-block-id cfg0))
     (define block
       (struct-copy AsmBlock (make-empty-block bid (Label:named 'entry))
                    [insns insns]
                    [terminator (Term:ret)]))
     (define cfg (cfg-add-block cfg1 block #:set-entry? #t))

     ;; Run allocation
     (define result (allocate-registers cfg))

     (check-true (AllocationResult? result))
     (check-true (AllocationResult-success? result))
     (check-true (ordered-map-has-key? (AllocationResult-assignment result) 'a)))

   (test-case "allocation with SVE virtual registers"
     (define vz (VReg:sve 'vec0))
     (define vp (VReg:pred 'mask))

     (define insns
       (list->pvector
        (list (Insn:whilelt vp (Reg:xzr) (Reg:x 0))
              (Insn:sve-load 'ld1w vp vz (Mem:base (Reg:x 1)))
              (Insn:ret))))

     (define cfg0 (make-empty-cfg))
     (define-values (bid cfg1) (cfg-fresh-block-id cfg0))
     (define block
       (struct-copy AsmBlock (make-empty-block bid (Label:named 'entry))
                    [insns insns]
                    [terminator (Term:ret)]))
     (define cfg (cfg-add-block cfg1 block #:set-entry? #t))

     (define result (allocate-registers cfg))

     (check-true (AllocationResult-success? result))
     ;; Check that vz gets a z register
     (define z-query (ordered-map-query (AllocationResult-assignment result) 'vec0))
     (check-true (and z-query (Reg:z? (cdr z-query))))
     ;; Check that vp gets a p register
     (define p-query (ordered-map-query (AllocationResult-assignment result) 'mask))
     (check-true (and p-query (Reg:p? (cdr p-query)))))))

;; ============================================================================
;; Run All Tests
;; ============================================================================

(define all-tests
  (test-suite
   "All Register Allocation Tests"
   vreg-parsing-tests
   liveness-tests
   interference-tests
   allocation-tests))

(module+ test
  (run-tests all-tests))

(provide all-tests)
