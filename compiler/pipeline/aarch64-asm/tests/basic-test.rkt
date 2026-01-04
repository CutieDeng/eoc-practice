#lang racket/base

;; Basic test for the aarch64-asm pipeline

(require rackunit
         racket/string
         "../main.rkt"
         "../../../../cutie-ftree/pvector.rkt"
         (only-in "../../../../cutie-ftree/graph.rkt" vertex-id vertex-id-val))

;; Test 1: CFG construction with cutie-ftree/graph
(define (test-cfg-construction)
  (printf "Test 1: CFG construction with graph-based blocks...\n")

  ;; Create empty CFG
  (define cfg0 (make-empty-cfg))

  ;; Allocate block IDs
  (define-values (bid0 cfg1) (cfg-fresh-block-id cfg0))
  (define-values (bid1 cfg2) (cfg-fresh-block-id cfg1))

  ;; Verify block IDs are vertex-ids
  (check-pred vertex-id? bid0 "block ID should be vertex-id")
  (check-pred vertex-id? bid1 "block ID should be vertex-id")
  (check-not-equal? (vertex-id-val bid0) (vertex-id-val bid1) "block IDs should be unique")

  ;; Create blocks
  (define block0
    (block-set-terminator
     (block-append-insn
      (make-empty-block bid0 (Label:id 0))
      (Insn:mov 'mov (Reg:x 0) (Imm 42)))
     (Term:jump bid1)))

  (define block1
    (block-set-terminator
     (make-empty-block bid1 (Label:id 1))
     (Term:ret)))

  ;; Add blocks to CFG
  (define cfg3 (cfg-add-block cfg2 block0 #:set-entry? #t))
  (define cfg4 (cfg-add-block cfg3 block1))

  ;; Verify CFG structure
  (check-equal? (cfg-block-count cfg4) 2 "CFG should have 2 blocks")
  (check-equal? (AsmBlock-id (cfg-entry-block cfg4)) bid0 "entry block should be block0")

  ;; Test successors/predecessors
  (define succs (cfg-successors cfg4 bid0))
  (check-equal? (length succs) 1 "block0 should have 1 successor")
  (check-equal? (vertex-id-val (car succs)) (vertex-id-val bid1) "successor should be block1")

  (define preds (cfg-predecessors cfg4 bid1))
  (check-equal? (length preds) 1 "block1 should have 1 predecessor")

  (printf "  PASSED\n")
  cfg4)

;; Test 2: Emission
(define (test-emission cfg)
  (printf "Test 2: Assembly emission...\n")

  ;; Emit the CFG
  (define asm-text (emit-cfg cfg 'test_function))

  ;; Verify output contains expected elements
  (check-regexp-match #rx"\\.global test_function" asm-text
                      "should emit .global directive")
  (check-regexp-match #rx"mov x0, #42" asm-text
                      "should emit mov instruction")
  (check-regexp-match #rx"ret" asm-text
                      "should emit ret instruction")

  (printf "  Generated assembly:\n")
  (for ([line (string-split asm-text "\n")])
    (printf "    ~a\n" line))
  (printf "  PASSED\n"))

;; Test 3: Block iteration
(define (test-block-iteration cfg)
  (printf "Test 3: Block iteration...\n")

  (define block-count 0)
  (for ([bid (in-cfg-block-ids cfg)])
    (check-pred vertex-id? bid "block ID should be vertex-id")
    (define block (cfg-get-block cfg bid))
    (check-pred AsmBlock? block "should get valid block")
    (set! block-count (add1 block-count)))

  (check-equal? block-count 2 "should iterate over 2 blocks")
  (printf "  PASSED\n"))

;; Test 4: Register types
(define (test-register-types)
  (printf "Test 4: Register types...\n")

  ;; Physical registers
  (define x0 (Reg:x 0))
  (check-pred Reg:x? x0)
  (check-pred any-reg? x0)
  (check-pred gpr? x0)

  (define z0 (Reg:z 0))
  (check-pred Reg:z? z0)
  (check-pred any-reg? z0)
  (check-pred sve-reg? z0)

  (define p0 (Reg:p 0))
  (check-pred Reg:p? p0)
  (check-pred pred-reg? p0)

  ;; Virtual registers
  (define v1 (VReg:gpr 'temp 64))
  (check-pred vreg? v1)
  (check-equal? (vreg-id v1) 'temp)
  (check-equal? (vreg-class v1) 'gpr)

  (printf "  PASSED\n"))

;; Run all tests
(printf "=== AArch64 ASM Pipeline Tests ===\n\n")
(define cfg (test-cfg-construction))
(test-emission cfg)
(test-block-iteration cfg)
(test-register-types)
(printf "\n=== All tests passed! ===\n")
