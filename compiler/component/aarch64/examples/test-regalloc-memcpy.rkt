#lang racket/base

;; ============================================================================
;; Test: Register Allocation for SVE memcpy
;; ============================================================================

(require racket/format
         racket/pretty
         racket/match
         racket/list
         racket/string
         racket/dict
         "../ir/types.rkt"
         "../ir/cfg.rkt"
         "../frontend/parser.rkt"
         "../analysis/liveness.rkt"
         "../backend/regalloc.rkt"
         "../../../../cutie-ftree/pvector.rkt"
         "../../../../cutie-ftree/ordered-map.rkt"
         "../../../../cutie-ftree/bitset.rkt"
         "../../../../cutie-ftree/comparator.rkt")

;; ============================================================================
;; Build CFG with Virtual Registers
;; ============================================================================

;; memcpy using virtual registers:
;;   %dst, %src, %len - virtual GPRs
;;   %z.data - virtual SVE register
;;   %p.mask - virtual predicate register

(define (build-memcpy-cfg)
  ;; Virtual registers
  (define %dst (VReg:gpr 'dst 64))
  (define %src (VReg:gpr 'src 64))
  (define %len (VReg:gpr 'len 64))
  (define %z.data (VReg:sve 'data))
  (define %p.mask (VReg:pred 'mask))

  ;; Entry block: initialize from physical registers
  (define entry-insns
    (list->pvector
     (list
      ;; Move arguments from physical to virtual registers
      (Insn:mov 'mov %dst (Reg:x 0))
      (Insn:mov 'mov %src (Reg:x 1))
      (Insn:mov 'mov %len (Reg:x 2)))))

  (define entry-block
    (AsmBlock (BlockId 0) (Label:named 'entry) entry-insns
              (Term:jump (BlockId 1))
              (ordered-map-empty symbol-compare)))

  ;; Loop block: main copy loop
  (define loop-insns
    (list->pvector
     (list
      ;; Generate predicate
      (Insn:whilelt %p.mask (Reg:xzr) %len)
      ;; Load with predicate
      (Insn:sve-load 'ld1b %p.mask %z.data (Mem:base %src))
      ;; Store with predicate
      (Insn:sve-store 'st1b %p.mask %z.data (Mem:base %dst))
      ;; Increment pointers (using add with immediate)
      (Insn:arith 'add %dst %dst (Imm 16))
      (Insn:arith 'add %src %src (Imm 16))
      ;; Decrement length
      (Insn:arith 'sub %len %len (Imm 16)))))

  (define loop-block
    (AsmBlock (BlockId 1) (Label:named 'loop) loop-insns
              (Term:cond 'gt (BlockId 1) (BlockId 2))  ; b.gt loop, else exit
              (ordered-map-empty symbol-compare)))

  ;; Exit block
  (define exit-insns
    (list->pvector
     (list (Insn:ret))))

  (define exit-block
    (AsmBlock (BlockId 2) (Label:named 'exit) exit-insns
              (Term:ret)
              (ordered-map-empty symbol-compare)))

  ;; Build CFG
  (define blocks
    (ordered-map-set
     (ordered-map-set
      (ordered-map-set
       (ordered-map-empty block-id-compare)
       (BlockId 0) entry-block)
      (BlockId 1) loop-block)
     (BlockId 2) exit-block))

  (AsmCfg (BlockId 0) blocks 3 3 #f))

;; ============================================================================
;; Print Helpers
;; ============================================================================

(define (print-reg r)
  (cond
    [(Reg:x? r) (format "x~a" (Reg:x-id r))]
    [(Reg:w? r) (format "w~a" (Reg:w-id r))]
    [(Reg:z? r) (format "z~a" (Reg:z-id r))]
    [(Reg:p? r) (format "p~a" (Reg:p-id r))]
    [(Reg:xzr? r) "xzr"]
    [(Reg:sp? r) "sp"]
    [(VReg:gpr? r) (format "%~a" (VReg:gpr-id r))]
    [(VReg:sve? r) (format "%z.~a" (VReg:sve-id r))]
    [(VReg:pred? r) (format "%p.~a" (VReg:pred-id r))]
    [(Imm? r) (format "#~a" (Imm-value r))]
    [(Mem:base? r) (format "[~a]" (print-reg (Mem:base-reg r)))]
    [else (format "~a" r)]))

(define (print-insn insn)
  (match insn
    [(Insn:mov op dst src)
     (format "  ~a ~a, ~a" op (print-reg dst) (print-reg src))]
    [(Insn:arith op dst src1 src2)
     (format "  ~a ~a, ~a, ~a" op (print-reg dst) (print-reg src1) (print-reg src2))]
    [(Insn:whilelt pd rn rm)
     (format "  whilelt ~a, ~a, ~a" (print-reg pd) (print-reg rn) (print-reg rm))]
    [(Insn:sve-load op pred dst addr)
     (format "  ~a ~a, ~a, ~a" op (print-reg dst) (print-reg pred) (print-reg addr))]
    [(Insn:sve-store op pred src addr)
     (format "  ~a ~a, ~a, ~a" op (print-reg src) (print-reg pred) (print-reg addr))]
    [(Insn:ret) "  ret"]
    [_ (format "  ~a" insn)]))

(define (print-block block)
  (printf "~a:\n" (Label:named-name (AsmBlock-label block)))
  (for ([insn (in-pvector (AsmBlock-insns block))])
    (printf "~a\n" (print-insn insn)))
  (define term (AsmBlock-terminator block))
  (match term
    [(Term:jump target) (printf "  b ~a\n" (BlockId-id target))]
    [(Term:cond cond then else) (printf "  b.~a ~a / b ~a\n" cond (BlockId-id then) (BlockId-id else))]
    [(Term:ret) (void)]
    [_ (void)]))

(define (print-cfg cfg title)
  (printf "\n~a\n" title)
  (printf "~a\n" (make-string (string-length title) #\=))
  (for ([bid (in-cfg-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (print-block block)
    (newline)))


;; ============================================================================
;; Collect Virtual Registers
;; ============================================================================

(define (collect-all-vregs cfg)
  (remove-duplicates
    (for*/list ([bid (in-cfg-block-ids cfg)]
                [block (in-value (cfg-get-block cfg bid))]
                #:when block
                [insn (in-pvector (AsmBlock-insns block))]
                [r (in-list (append (insn-defs insn) (insn-uses insn)))]
                #:when (vreg? r))
      r)
    #:key vreg-id))

;; ============================================================================
;; Main Test
;; ============================================================================

(module+ main
  (displayln "============================================")
  (displayln "Register Allocation Test: SVE memcpy")
  (displayln "============================================")

  ;; Build CFG with virtual registers
  (displayln "\nBuilding CFG with virtual registers...")
  (define cfg (build-memcpy-cfg))

  ;; Print original CFG
  (print-cfg cfg "Original CFG (with virtual registers)")

  ;; Collect vregs and build index
  (define all-vregs (collect-all-vregs cfg))
  (define vreg-index (build-vreg-index all-vregs))

  ;; Run liveness analysis
  (displayln "Running liveness analysis...")
  (define liveness (compute-liveness cfg vreg-index))
  (displayln "  Liveness analysis complete.")

  ;; Print live-in/live-out for each block
  (displayln "\nLiveness Information:")
  (displayln "---------------------")
  (for ([bid (in-cfg-block-ids cfg)])
    (define live-in (dict-ref (LivenessResult-live-in liveness) bid bitset-empty))
    (define live-out (dict-ref (LivenessResult-live-out liveness) bid bitset-empty))
    (printf "Block ~a:\n" (BlockId-id bid))
    ;; Convert bitset indices back to vreg names for display
    (printf "  live-in:  {~a}\n"
            (string-join
             (for/list ([idx (in-bitset live-in)])
               (symbol->string (idx->vreg-id vreg-index idx)))
             ", "))
    (printf "  live-out: {~a}\n"
            (string-join
             (for/list ([idx (in-bitset live-out)])
               (symbol->string (idx->vreg-id vreg-index idx)))
             ", ")))

  ;; Run register allocation
  (displayln "\nRunning register allocation...")
  (define alloc-result (allocate-registers cfg))

  (displayln "\nAllocation Result:")
  (displayln "------------------")
  (printf "  Success: ~a\n" (AllocationResult-success? alloc-result))
  (printf "  Spilled: ~a\n" (bitset-count (AllocationResult-spilled alloc-result)))

  (displayln "\nRegister Assignment:")
  (for ([kv (in-ordered-map (AllocationResult-assignment alloc-result))])
    (define vid (car kv))
    (define phys (cdr kv))
    (printf "  ~a -> ~a\n" vid (print-reg phys)))

  (when (not (bitset-empty? (AllocationResult-callee-saved-used alloc-result)))
    (displayln "\nCallee-saved registers used:")
    (for ([idx (in-bitset (AllocationResult-callee-saved-used alloc-result))])
      (printf "  color ~a\n" idx)))

  ;; Apply allocation to CFG
  (displayln "\nApplying register allocation...")
  (define allocated-cfg (apply-allocation cfg alloc-result))

  ;; Print allocated CFG
  (print-cfg allocated-cfg "Allocated CFG (with physical registers)")

  (displayln "============================================")
  (displayln "Register allocation test complete!")
  (displayln "============================================"))
