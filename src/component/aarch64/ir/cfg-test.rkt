#lang racket/base

;; Unit Tests for AArch64 CFG (Control Flow Graph)

(require rackunit
         rackunit/text-ui
         racket/match
         "../../../../cutie-ftree/pvector.rkt"
         "../../../../cutie-ftree/ordered-map.rkt"
         "../../../../cutie-ftree/bitset.rkt"
         "../ir/types.rkt"
         "../ir/cfg.rkt")

;; ============================================================================
;; Block Tests
;; ============================================================================

(define block-tests
  (test-suite
   "Basic Block Operations"

   (test-case "create empty block"
     (define bid (BlockId 0))
     (define label (Label:named 'entry))
     (define block (make-empty-block bid label))

     (check-true (AsmBlock? block))
     (check-equal? (AsmBlock-id block) bid)
     (check-equal? (AsmBlock-label block) label)
     (check-equal? (block-insn-count block) 0)
     (check-false (AsmBlock-terminator block)))

   (test-case "append instruction to block"
     (define block (make-empty-block (BlockId 0) (Label:named 'test)))
     (define insn (Insn:arith 'add (Reg:x 0) (Reg:x 1) (Reg:x 2)))

     (define block2 (block-append-insn block insn))
     (check-equal? (block-insn-count block2) 1)

     ;; Original block unchanged (persistent)
     (check-equal? (block-insn-count block) 0))

   (test-case "append multiple instructions"
     (define block (make-empty-block (BlockId 0) (Label:named 'test)))
     (define insns (list
                    (Insn:arith 'add (Reg:x 0) (Reg:x 1) (Reg:x 2))
                    (Insn:arith 'sub (Reg:x 3) (Reg:x 4) (Reg:x 5))
                    (Insn:mov 'mov (Reg:x 6) (Reg:x 0))))

     (define block2 (block-append-insns block insns))
     (check-equal? (block-insn-count block2) 3))

   (test-case "set terminator"
     (define block (make-empty-block (BlockId 0) (Label:named 'test)))
     (define block2 (block-set-terminator block (Term:ret)))

     (check-true (Term:ret? (AsmBlock-terminator block2)))
     (check-false (AsmBlock-terminator block)))  ; Original unchanged

   (test-case "pvector instruction access"
     (define block (make-empty-block (BlockId 0) (Label:named 'test)))
     (define insn1 (Insn:arith 'add (Reg:x 0) (Reg:x 1) (Reg:x 2)))
     (define insn2 (Insn:arith 'sub (Reg:x 3) (Reg:x 4) (Reg:x 5)))

     (define block2 (block-append-insns block (list insn1 insn2)))
     (define insns (AsmBlock-insns block2))

     ;; Access by index
     (check-equal? (pvector-ref insns 0) insn1)
     (check-equal? (pvector-ref insns 1) insn2)

     ;; Iterate
     (define insn-list (pvector->list insns))
     (check-equal? (length insn-list) 2))))

;; ============================================================================
;; Terminator Tests
;; ============================================================================

(define terminator-tests
  (test-suite
   "Terminator Types"

   (test-case "return terminator"
     (define ret (Term:ret))
     (check-true (Term:ret? ret))
     (check-true (terminator? ret))
     (check-equal? (terminator-successors ret) '()))

   (test-case "jump terminator"
     (define target (BlockId 5))
     (define jmp (Term:jump target))
     (check-true (Term:jump? jmp))
     (check-equal? (Term:jump-target jmp) target)
     (check-equal? (terminator-successors jmp) (list target)))

   (test-case "conditional terminator"
     (define then-id (BlockId 1))
     (define else-id (BlockId 2))
     (define cond-term (Term:cond 'eq then-id else-id))
     (check-true (Term:cond? cond-term))
     (check-equal? (Term:cond-cond cond-term) 'eq)
     (check-equal? (terminator-successors cond-term) (list then-id else-id)))

   (test-case "unreachable terminator"
     (define unreach (Term:unreachable))
     (check-true (Term:unreachable? unreach))
     (check-equal? (terminator-successors unreach) '()))))

;; ============================================================================
;; CFG Tests
;; ============================================================================

(define cfg-tests
  (test-suite
   "CFG Operations"

   (test-case "create empty CFG"
     (define cfg (make-empty-cfg))
     (check-true (AsmCfg? cfg))
     (check-false (AsmCfg-entry cfg))
     (check-equal? (cfg-block-count cfg) 0))

   (test-case "fresh block ID generation"
     (define cfg0 (make-empty-cfg))
     (define-values (id1 cfg1) (cfg-fresh-block-id cfg0))
     (define-values (id2 cfg2) (cfg-fresh-block-id cfg1))

     (check-true (BlockId? id1))
     (check-true (BlockId? id2))
     (check-equal? (BlockId-id id1) 0)
     (check-equal? (BlockId-id id2) 1))

   (test-case "fresh label generation"
     (define cfg0 (make-empty-cfg))
     (define-values (label1 cfg1) (cfg-fresh-label cfg0))
     (define-values (label2 cfg2) (cfg-fresh-label cfg1))

     (check-true (Label:id? label1))
     (check-true (Label:id? label2))
     (check-equal? (Label:id-id label1) 0)
     (check-equal? (Label:id-id label2) 1))

   (test-case "add block to CFG"
     (define cfg0 (make-empty-cfg))
     (define-values (bid cfg1) (cfg-fresh-block-id cfg0))
     (define block (make-empty-block bid (Label:named 'entry)))
     (define block-with-term (block-set-terminator block (Term:ret)))

     (define cfg2 (cfg-add-block cfg1 block-with-term #:set-entry? #t))
     (check-equal? (cfg-block-count cfg2) 1)
     (check-equal? (AsmCfg-entry cfg2) bid))

   (test-case "get block from CFG"
     (define cfg0 (make-empty-cfg))
     (define-values (bid cfg1) (cfg-fresh-block-id cfg0))
     (define block (block-set-terminator
                    (make-empty-block bid (Label:named 'test))
                    (Term:ret)))
     (define cfg2 (cfg-add-block cfg1 block))

     (define retrieved (cfg-get-block cfg2 bid))
     (check-equal? retrieved block)

     ;; Non-existent block returns #f
     (check-false (cfg-get-block cfg2 (BlockId 999))))

   (test-case "update block in CFG"
     (define cfg0 (make-empty-cfg))
     (define-values (bid cfg1) (cfg-fresh-block-id cfg0))
     (define block (make-empty-block bid (Label:named 'test)))
     (define cfg2 (cfg-add-block cfg1 block))

     (define cfg3 (cfg-update-block cfg2 bid
                    (λ (b) (block-set-terminator b (Term:ret)))))

     (define updated (cfg-get-block cfg3 bid))
     (check-true (Term:ret? (AsmBlock-terminator updated))))

   (test-case "iterate over blocks"
     (define cfg0 (make-empty-cfg))

     ;; Add 3 blocks
     (define-values (bid1 cfg1) (cfg-fresh-block-id cfg0))
     (define-values (bid2 cfg2) (cfg-fresh-block-id cfg1))
     (define-values (bid3 cfg3) (cfg-fresh-block-id cfg2))

     (define block1 (block-set-terminator
                     (make-empty-block bid1 (Label:named 'b1))
                     (Term:jump bid2)))
     (define block2 (block-set-terminator
                     (make-empty-block bid2 (Label:named 'b2))
                     (Term:jump bid3)))
     (define block3 (block-set-terminator
                     (make-empty-block bid3 (Label:named 'b3))
                     (Term:ret)))

     (define cfg4 (cfg-add-block cfg3 block1 #:set-entry? #t))
     (define cfg5 (cfg-add-block cfg4 block2))
     (define cfg6 (cfg-add-block cfg5 block3))

     ;; Count blocks via iteration
     (define count
       (for/sum ([_ (in-cfg-blocks cfg6)]) 1))
     (check-equal? count 3)

     ;; Collect block IDs
     (define ids (for/list ([id (in-cfg-block-ids cfg6)]) id))
     (check-equal? (length ids) 3))))

;; ============================================================================
;; CFG Traversal Tests
;; ============================================================================

(define traversal-tests
  (test-suite
   "CFG Traversal"

   (test-case "cfg-successors"
     (define cfg0 (make-empty-cfg))
     (define-values (bid1 cfg1) (cfg-fresh-block-id cfg0))
     (define-values (bid2 cfg2) (cfg-fresh-block-id cfg1))

     (define block1 (block-set-terminator
                     (make-empty-block bid1 (Label:named 'b1))
                     (Term:jump bid2)))
     (define block2 (block-set-terminator
                     (make-empty-block bid2 (Label:named 'b2))
                     (Term:ret)))

     (define cfg3 (cfg-add-block cfg2 block1 #:set-entry? #t))
     (define cfg4 (cfg-add-block cfg3 block2))

     (check-equal? (cfg-successors cfg4 bid1) (list bid2))
     (check-equal? (cfg-successors cfg4 bid2) '()))

   (test-case "cfg-predecessors"
     (define cfg0 (make-empty-cfg))
     (define-values (bid1 cfg1) (cfg-fresh-block-id cfg0))
     (define-values (bid2 cfg2) (cfg-fresh-block-id cfg1))
     (define-values (bid3 cfg3) (cfg-fresh-block-id cfg2))

     ;; b1 -> b2, b1 -> b3 (conditional)
     (define block1 (block-set-terminator
                     (make-empty-block bid1 (Label:named 'b1))
                     (Term:cond 'eq bid2 bid3)))
     (define block2 (block-set-terminator
                     (make-empty-block bid2 (Label:named 'b2))
                     (Term:ret)))
     (define block3 (block-set-terminator
                     (make-empty-block bid3 (Label:named 'b3))
                     (Term:ret)))

     (define cfg4 (cfg-add-block cfg3 block1 #:set-entry? #t))
     (define cfg5 (cfg-add-block cfg4 block2))
     (define cfg6 (cfg-add-block cfg5 block3))

     (define preds (cfg-predecessors cfg6))
     (check-equal? (ordered-map-ref preds bid1 '()) '())  ; Entry has no preds
     (check-equal? (ordered-map-ref preds bid2 '()) (list bid1))
     (check-equal? (ordered-map-ref preds bid3 '()) (list bid1)))

   (test-case "cfg-reachable-blocks"
     (define cfg0 (make-empty-cfg))
     (define-values (bid1 cfg1) (cfg-fresh-block-id cfg0))
     (define-values (bid2 cfg2) (cfg-fresh-block-id cfg1))
     (define-values (bid3 cfg3) (cfg-fresh-block-id cfg2))  ; Unreachable

     (define block1 (block-set-terminator
                     (make-empty-block bid1 (Label:named 'b1))
                     (Term:jump bid2)))
     (define block2 (block-set-terminator
                     (make-empty-block bid2 (Label:named 'b2))
                     (Term:ret)))
     (define block3 (block-set-terminator
                     (make-empty-block bid3 (Label:named 'unreachable))
                     (Term:ret)))

     (define cfg4 (cfg-add-block cfg3 block1 #:set-entry? #t))
     (define cfg5 (cfg-add-block cfg4 block2))
     (define cfg6 (cfg-add-block cfg5 block3))

     (define reachable (cfg-reachable-blocks cfg6))
     (check-true (bitset-member? reachable 0))   ; bid1
     (check-true (bitset-member? reachable 1))   ; bid2
     (check-false (bitset-member? reachable 2))))) ; bid3 unreachable

;; ============================================================================
;; Persistence Tests
;; ============================================================================

(define persistence-tests
  (test-suite
   "Persistent Data Structure Properties"

   (test-case "block modification doesn't affect original"
     (define block1 (make-empty-block (BlockId 0) (Label:named 'test)))
     (define insn (Insn:arith 'add (Reg:x 0) (Reg:x 1) (Reg:x 2)))
     (define block2 (block-append-insn block1 insn))

     (check-equal? (block-insn-count block1) 0)
     (check-equal? (block-insn-count block2) 1))

   (test-case "CFG modification doesn't affect original"
     (define cfg1 (make-empty-cfg))
     (define-values (bid cfg2) (cfg-fresh-block-id cfg1))
     (define block (make-empty-block bid (Label:named 'test)))
     (define cfg3 (cfg-add-block cfg2 block))

     (check-equal? (cfg-block-count cfg1) 0)
     (check-equal? (cfg-block-count cfg3) 1))

   (test-case "pvector structural sharing"
     ;; This tests that pvector shares structure efficiently
     (define block1 (make-empty-block (BlockId 0) (Label:named 'test)))

     ;; Add 100 instructions
     (define block2
       (for/fold ([b block1])
                 ([i (in-range 100)])
         (block-append-insn b (Insn:arith 'add (Reg:x 0) (Reg:x 1) (Imm i)))))

     (check-equal? (block-insn-count block2) 100)

     ;; Original still has 0
     (check-equal? (block-insn-count block1) 0))))

;; ============================================================================
;; Run All Tests
;; ============================================================================

(define all-tests
  (test-suite
   "AArch64 CFG"
   block-tests
   terminator-tests
   cfg-tests
   traversal-tests
   persistence-tests))

(module+ main
  (run-tests all-tests))

(module+ test
  (run-tests all-tests))
