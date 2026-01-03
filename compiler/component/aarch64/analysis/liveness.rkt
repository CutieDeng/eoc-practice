#lang racket/base

;; ============================================================================
;; AArch64 Liveness Analysis
;; ============================================================================
;;
;; Computes variable liveness for aarch64 instruction sequences.
;; Uses bitset for all set operations (variables normalized to integers).
;; This is used by the register allocator to build interference graphs.
;;
;; ============================================================================

(require racket/match
         racket/list
         "../ir/types.rkt"
         "../ir/cfg.rkt"
         "../../../../cutie-ftree/pvector.rkt"
         "../../../../cutie-ftree/ordered-map.rkt"
         "../../../../cutie-ftree/bitset.rkt"
         "../../../../cutie-ftree/comparator.rkt")

(provide
 ;; Main analysis
 compute-liveness
 compute-block-liveness

 ;; Result structures
 (struct-out LivenessInfo)
 (struct-out InsnLiveness)

 ;; Utilities
 insn-defs
 insn-uses
 get-live-after)

;; ============================================================================
;; Result Structures
;; ============================================================================

;; Liveness info for entire function
(struct LivenessInfo (
  block-info     ; ordered-map[BlockId -> pvector[InsnLiveness]]
  live-in        ; ordered-map[BlockId -> bitset]
  live-out       ; ordered-map[BlockId -> bitset]
) #:prefab)

;; Liveness at each instruction point
(struct InsnLiveness (
  live-before    ; bitset of live variable indices
  live-after     ; bitset of live variable indices
) #:prefab)

;; ============================================================================
;; Instruction Def/Use Analysis
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
;; Helper: ordered-map-ref with default value
;; ============================================================================

(define (omap-ref m k default)
  (define result (ordered-map-query m k))
  (if result (cdr result) default))

;; ============================================================================
;; Convert registers to bitset using vreg-index
;; ============================================================================

;; Convert list of registers to bitset indices
(define (regs->bitset regs vreg-index)
  (define id->idx (vreg-index-id->idx vreg-index))
  (for/fold ([bs bitset-empty])
            ([r regs]
             #:when (vreg? r))
    (define idx (omap-ref id->idx (vreg-id r) #f))
    (if idx
        (bitset-add bs idx)
        bs)))

;; VReg-index accessor (matches structure from regalloc.rkt)
(define (vreg-index-id->idx index)
  (vector-ref (struct->vector index) 1))

;; ============================================================================
;; Block Liveness Analysis (using bitset)
;; ============================================================================

;; Compute liveness for a single basic block
;; Returns: (values pvector[InsnLiveness] live-in-bitset)
(define (compute-block-liveness insns live-out vreg-index)
  (define n (pvector-length insns))

  ;; Build liveness: iterate in reverse, use cons-left to build correct order
  (define-values (liveness final-live-in)
    (for/fold ([result (pvector-empty)]
               [live live-out])
              ([i (in-range (sub1 n) -1 -1)])
      (define insn (pvector-ref insns i))
      (define uses (regs->bitset (insn-uses insn) vreg-index))
      (define defs (regs->bitset (insn-defs insn) vreg-index))
      (define live-before (bitset-union uses (bitset-subtract live defs)))
      (values (pvector-cons-left result (InsnLiveness live-before live))
              live-before)))

  (values liveness final-live-in))

;; ============================================================================
;; CFG Liveness Analysis (using bitset)
;; ============================================================================

;; Compute liveness for entire CFG (pure functional)
;; vreg-index: VRegIndex struct with id->idx and idx->id mappings
(define (compute-liveness cfg vreg-index)
  (define block-ids (for/list ([bid (in-cfg-block-ids cfg)]) bid))

  ;; Initialize live-in and live-out as ordered-maps with empty bitsets
  (define init-live
    (for/fold ([m (ordered-map-empty block-id-compare)])
              ([bid block-ids])
      (ordered-map-set m bid bitset-empty)))

  ;; Build successor/predecessor maps (pure functional)
  (define-values (succs preds)
    (for/fold ([s (for/fold ([m (ordered-map-empty block-id-compare)])
                            ([bid block-ids])
                    (ordered-map-set m bid '()))]
               [p (for/fold ([m (ordered-map-empty block-id-compare)])
                            ([bid block-ids])
                    (ordered-map-set m bid '()))])
              ([bid block-ids])
      (define block (cfg-get-block cfg bid))
      (if (not block)
          (values s p)
          (let ([targets (terminator-targets (AsmBlock-terminator block))])
            (for/fold ([s2 s] [p2 p])
                      ([target targets])
              (values (ordered-map-set s2 bid (cons target (omap-ref s2 bid '())))
                      (ordered-map-set p2 target (cons bid (omap-ref p2 target '())))))))))

  ;; Block ID set for worklist membership tracking (using ordered-map as set)
  (define (bid-set-empty) (ordered-map-empty block-id-compare))
  (define (bid-set-add s bid) (ordered-map-set s bid #t))
  (define (bid-set-remove s bid)
    (if (ordered-map-has-key? s bid)
        (let-values ([(m _) (ordered-map-delete s bid)]) m)
        s))
  (define (bid-set-member? s bid) (ordered-map-has-key? s bid))

  ;; Worklist algorithm (backward dataflow) - pure functional
  (define (worklist-loop worklist in-worklist live-in live-out block-liveness)
    (if (pvector-empty? worklist)
        (values live-in live-out block-liveness)
        (let* ([bid (pvector-ref worklist 0)]
               [rest-worklist (pvector-drop worklist 1)]
               [new-in-worklist (bid-set-remove in-worklist bid)]
               [block (cfg-get-block cfg bid)])
          (if (not block)
              (worklist-loop rest-worklist new-in-worklist live-in live-out block-liveness)
              ;; live-out[B] = ∪ live-in[S] for all successors S
              (let ([new-out
                     (for/fold ([out bitset-empty])
                               ([s (omap-ref succs bid '())])
                       (bitset-union out (omap-ref live-in s bitset-empty)))])
                (define new-live-out (ordered-map-set live-out bid new-out))
                ;; Compute liveness for instructions in block
                (define-values (insn-liveness new-in)
                  (compute-block-liveness (AsmBlock-insns block) new-out vreg-index))
                (define new-block-liveness (ordered-map-set block-liveness bid insn-liveness))
                ;; If live-in changed, add predecessors to worklist
                (if (bitset-equal? new-in (omap-ref live-in bid bitset-empty))
                    (worklist-loop rest-worklist new-in-worklist live-in new-live-out new-block-liveness)
                    (let ([new-live-in (ordered-map-set live-in bid new-in)]
                          [preds-list (omap-ref preds bid '())])
                      (define-values (updated-worklist updated-in-worklist)
                        (for/fold ([wl rest-worklist] [iw new-in-worklist])
                                  ([p preds-list])
                          (if (bid-set-member? iw p)
                              (values wl iw)
                              (values (pvector-cons-right wl p) (bid-set-add iw p)))))
                      (worklist-loop updated-worklist updated-in-worklist
                                     new-live-in new-live-out new-block-liveness))))))))

  ;; Initial worklist (reverse order for backward analysis)
  (define init-worklist (list->pvector (reverse block-ids)))
  (define init-in-worklist
    (for/fold ([s (bid-set-empty)])
              ([bid block-ids])
      (bid-set-add s bid)))
  (define init-block-liveness (ordered-map-empty block-id-compare))

  (define-values (final-live-in final-live-out final-block-liveness)
    (worklist-loop init-worklist init-in-worklist init-live init-live init-block-liveness))

  (LivenessInfo final-block-liveness final-live-in final-live-out))

;; ============================================================================
;; Helper: Get terminator targets
;; ============================================================================

(define (terminator-targets term)
  (match term
    [(Term:ret) '()]
    [(Term:jump target) (list target)]
    [(Term:cond _ then-target else-target) (list then-target else-target)]
    [(Term:switch _ cases default)
     (cons default (map cdr cases))]
    [#f '()]
    [_ '()]))

;; ============================================================================
;; Utilities
;; ============================================================================

;; Get live bitset after a specific instruction
(define (get-live-after liveness-info block-id insn-index)
  (define block-query (ordered-map-query (LivenessInfo-block-info liveness-info) block-id))
  (define block-info (and block-query (cdr block-query)))
  (if (and block-info (< insn-index (pvector-length block-info)))
      (InsnLiveness-live-after (pvector-ref block-info insn-index))
      bitset-empty))
