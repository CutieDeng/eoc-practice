#lang racket/base

;; ============================================================================
;; AArch64 Liveness Analysis (Pipeline Version)
;; ============================================================================
;;
;; Integrates with driver/dataflow/liveness framework.
;; Uses the new graph-based CFG with vertex-id as block IDs.
;;
;; ============================================================================

(require racket/match
         racket/list
         racket/class
         racket/dict
         "../ir/types.rkt"
         "../ir/cfg.rkt"
         (only-in "../../../../driver/dataflow/liveness.rkt"
                  liveness-interface<%>
                  [compute-liveness driver:compute-liveness]
                  LivenessResult LivenessResult?
                  LivenessResult-block-info LivenessResult-live-in LivenessResult-live-out
                  BlockLiveness BlockLiveness?
                  BlockLiveness-insn-liveness BlockLiveness-gen BlockLiveness-kill
                  InsnLiveness InsnLiveness?
                  InsnLiveness-live-before InsnLiveness-live-after)
         "../../../../../cutie-ftree/pvector.rkt"
         "../../../../../cutie-ftree/ordered-map.rkt"
         "../../../../../cutie-ftree/bitset.rkt"
         "../../../../../cutie-ftree/comparator.rkt"
         (only-in "../../../../../cutie-ftree/graph.rkt" vertex-id? vertex-id-val))

(provide
 ;; Main analysis (uses driver framework)
 compute-liveness

 ;; Result structures (re-exported from driver)
 LivenessResult LivenessResult?
 LivenessResult-block-info LivenessResult-live-in LivenessResult-live-out
 BlockLiveness BlockLiveness?
 BlockLiveness-insn-liveness BlockLiveness-gen BlockLiveness-kill
 InsnLiveness InsnLiveness?
 InsnLiveness-live-before InsnLiveness-live-after

 ;; AArch64-specific utilities
 insn-defs
 insn-uses
 get-live-after

 ;; Interface class (for advanced usage)
 make-aarch64-liveness-interface)

;; ============================================================================
;; Instruction Def/Use Analysis (AArch64-specific)
;; ============================================================================

;; Get registers defined (written) by an instruction
(define (insn-defs insn)
  (match insn
    [(Insn:arith _ dst _ _) (list dst)]
    [(Insn:arith2 _ dst _) (list dst)]
    [(Insn:load _ dst _) (list dst)]
    [(Insn:ldp _ dst1 dst2 _) (list dst1 dst2)]
    [(Insn:cmp _ _ _) '()]  ; Sets flags, not a register
    [(Insn:csel _ dst _ _ _) (list dst)]
    [(Insn:mov _ dst _) (list dst)]
    [(Insn:sve _ _ dst _) (list dst)]
    [(Insn:sve-load _ _ dst _) (list dst)]
    [(Insn:sve-reduce _ _ dst _) (list dst)]
    [(Insn:sve-cmp _ pd _ _ _) (list pd)]
    [(Insn:sve-pred-op _ pd _ _) (list pd)]
    [(Insn:whilelt pd _ _) (list pd)]
    [(Insn:ret) '()]
    [(Insn:branch _ _) '()]
    [(Insn:cond-branch _ _ _) '()]
    [(Insn:cbz _ _ _) '()]
    [(Insn:store _ _ _) '()]
    [(Insn:stp _ _ _ _) '()]
    [(Insn:sve-store _ _ _ _) '()]
    [_ '()]))

;; Get registers used (read) by an instruction
(define (insn-uses insn)
  (define (reg-or-vreg? x)
    (or (any-reg? x) (vreg? x)))
  (define (collect-uses args)
    (filter reg-or-vreg?
            (flatten (map extract-regs args))))

  (define (extract-regs arg)
    (match arg
      [(? any-reg?) (list arg)]
      [(? vreg?) (list arg)]
      [(Mem:base reg) (list reg)]
      [(Mem:offset reg _) (list reg)]
      [(Mem:pre reg _) (list reg)]
      [(Mem:post reg _) (list reg)]
      [(Mem:reg base idx) (list base idx)]
      [(Mem:scaled base idx _) (list base idx)]
      [_ '()]))

  (match insn
    [(Insn:arith _ _ src1 src2)
     (collect-uses (list src1 src2))]
    [(Insn:arith2 _ _ src)
     (collect-uses (list src))]
    [(Insn:load _ _ addr)
     (collect-uses (list addr))]
    [(Insn:store _ src addr)
     (collect-uses (list src addr))]
    [(Insn:ldp _ _ _ addr)
     (collect-uses (list addr))]
    [(Insn:stp _ src1 src2 addr)
     (collect-uses (list src1 src2 addr))]
    [(Insn:cmp _ src1 src2)
     (collect-uses (list src1 src2))]
    [(Insn:csel _ _ src1 src2 _)
     (collect-uses (list src1 src2))]
    [(Insn:mov _ _ src)
     (collect-uses (list src))]
    [(Insn:sve _ pred _ srcs)
     (cons pred (collect-uses srcs))]
    [(Insn:sve-load _ pred _ addr)
     (cons pred (collect-uses (list addr)))]
    [(Insn:sve-store _ pred src addr)
     (cons pred (collect-uses (list src addr)))]
    [(Insn:sve-reduce _ pred _ src)
     (list pred src)]
    [(Insn:sve-cmp _ _ pg src1 src2)
     (cons pg (collect-uses (list src1 src2)))]
    [(Insn:sve-pred-op _ _ pg pn)
     (list pg pn)]
    [(Insn:whilelt _ rn rm)
     (collect-uses (list rn rm))]
    [(Insn:cbz _ reg _)
     (collect-uses (list reg))]
    [(Insn:ret) '()]
    [(Insn:branch _ _) '()]
    [(Insn:cond-branch _ _ _) '()]
    [_ '()]))

;; ============================================================================
;; AArch64 Liveness Interface (implements liveness-interface<%>)
;; ============================================================================

;; VReg-index accessors (matches structure from regalloc.rkt)
(define (vreg-index-id->idx index)
  (vector-ref (struct->vector index) 1))

(define (vreg-index-idx->id index)
  (vector-ref (struct->vector index) 2))

(define (vreg-index-count index)
  (vector-ref (struct->vector index) 3))

;; Convert list of registers to bitset indices
(define (regs->bitset regs id->idx-map)
  (for/fold ([bs bitset-empty])
            ([r regs]
             #:when (vreg? r))
    (define idx (dict-ref id->idx-map (vreg-id r) #f))
    (if idx
        (bitset-add bs idx)
        bs)))

;; Helper to get block ID value (supports both vertex-id and legacy BlockId)
(define (get-block-id-val bid)
  (cond
    [(vertex-id? bid) (vertex-id-val bid)]
    [(and (struct? bid) (BlockId? bid)) (BlockId-id bid)]
    [(integer? bid) bid]
    [else (error 'get-block-id-val "unknown block id type: ~a" bid)]))

;; Create AArch64 liveness interface instance
(define (make-aarch64-liveness-interface vreg-index cfg)
  (define id->idx (vreg-index-id->idx vreg-index))
  (define idx->id (vreg-index-idx->id vreg-index))
  (define var-cnt (vreg-index-count vreg-index))

  ;; Precompute predecessors map for entire CFG
  ;; cfg-predecessors now returns list directly per block
  (define pred-map
    (for/fold ([m (ordered-map-empty integer-compare)])
              ([bid (in-cfg-block-ids cfg)])
      (define preds (cfg-predecessors cfg bid))
      (ordered-map-set m (get-block-id-val bid) preds)))

  (new aarch64-liveness-interface%
       [id->idx-map id->idx]
       [idx->id-vec idx->id]
       [var-count-val var-cnt]
       [predecessors-map pred-map]))

;; AArch64-specific implementation of liveness-interface<%>
(define aarch64-liveness-interface%
  (class* object% (liveness-interface<%>)
    (init-field id->idx-map     ; ordered-map[symbol -> int]
                idx->id-vec     ; pvector[int -> symbol]
                var-count-val   ; int
                predecessors-map) ; ordered-map[int -> (listof vertex-id)]

    (super-new)

    ;; === Variable normalization ===
    (define/public (var-count)
      var-count-val)

    (define/public (var->id var)
      (if (vreg? var)
          (dict-ref id->idx-map (vreg-id var) 0)
          0))

    (define/public (id->var id)
      (if (< id (pvector-length idx->id-vec))
          (pvector-ref idx->id-vec id)
          #f))

    ;; === CFG queries ===
    (define/public (block-ids cfg)
      (for/pvector ([bid (in-cfg-block-ids cfg)])
        bid))

    (define/public (get-block cfg bid)
      (cfg-get-block cfg bid))

    (define/public (block-insns block)
      (AsmBlock-insns block))

    (define/public (block-successors cfg bid)
      (list->pvector (cfg-successors cfg bid)))

    (define/public (block-predecessors cfg bid)
      (define vid-val (get-block-id-val bid))
      (list->pvector (dict-ref predecessors-map vid-val '())))

    ;; === Instruction def/use (returns bitset) ===
    (define/public (insn-defs-bitset insn)
      (regs->bitset (insn-defs insn) id->idx-map))

    (define/public (insn-uses-bitset insn)
      (regs->bitset (insn-uses insn) id->idx-map))

    ;; === Block ID comparison ===
    ;; Returns a comparator function that returns '< '= '>
    (define/public (block-id-compare)
      block-id-compare*)))

;; Block ID comparator using vertex-id-val
(define (block-id-compare* a b)
  (define a-val (get-block-id-val a))
  (define b-val (get-block-id-val b))
  (cond
    [(< a-val b-val) '<]
    [(> a-val b-val) '>]
    [else '=]))

;; ============================================================================
;; Main Entry Point
;; ============================================================================

;; Compute liveness for entire CFG using driver framework
;; vreg-index: VRegIndex struct with id->idx and idx->id mappings
(define (compute-liveness cfg vreg-index)
  (define interface (make-aarch64-liveness-interface vreg-index cfg))
  ;; Use driver's compute-liveness (imported with rename at top)
  (driver:compute-liveness cfg interface))

;; ============================================================================
;; Utilities
;; ============================================================================

;; Get live bitset after a specific instruction
(define (get-live-after liveness-result block-id insn-index)
  (define block-query (ordered-map-query (LivenessResult-block-info liveness-result) block-id))
  (define block-liveness (and block-query (cdr block-query)))
  (if (and block-liveness
           (< insn-index (pvector-length (BlockLiveness-insn-liveness block-liveness))))
      (InsnLiveness-live-after
       (pvector-ref (BlockLiveness-insn-liveness block-liveness) insn-index))
      bitset-empty))
