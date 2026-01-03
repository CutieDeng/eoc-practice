#lang racket/base

;; ============================================================================
;; AArch64 Liveness Analysis
;; ============================================================================
;;
;; Computes variable liveness for aarch64 instruction sequences.
;; This is used by the register allocator to build interference graphs.
;;
;; ============================================================================

(require racket/match
         racket/set
         racket/list
         "../ir/types.rkt"
         "../ir/cfg.rkt"
         "../../../../cutie-ftree/pvector.rkt"
         "../../../../cutie-ftree/ordered-map.rkt")

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
  block-info     ; Hash[BlockId -> BlockLivenessInfo]
  live-in        ; Hash[BlockId -> Set[Reg/VReg]]
  live-out       ; Hash[BlockId -> Set[Reg/VReg]]
) #:prefab)

;; Liveness at each instruction point
(struct InsnLiveness (
  live-before    ; Set of live registers before this instruction
  live-after     ; Set of live registers after this instruction
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
;; Block Liveness Analysis
;; ============================================================================

;; Compute liveness for a single basic block
;; Returns: pvector of InsnLiveness, one per instruction
(define (compute-block-liveness insns live-out)
  ;; Process instructions in reverse order
  (define insn-list (pvector->list insns))
  (define n (length insn-list))

  ;; Build live-after for each instruction (reverse order)
  (define-values (liveness-list final-live-in)
    (for/fold ([result '()]
               [live live-out])
              ([insn (in-list (reverse insn-list))])
      (define uses (list->set (insn-uses insn)))
      (define defs (list->set (insn-defs insn)))
      (define live-before (set-union uses (set-subtract live defs)))
      (values (cons (InsnLiveness live-before live) result)
              live-before)))

  (values (list->pvector liveness-list) final-live-in))

;; ============================================================================
;; CFG Liveness Analysis
;; ============================================================================

;; Compute liveness for entire CFG
(define (compute-liveness cfg)
  (define block-ids (for/list ([bid (in-cfg-block-ids cfg)]) bid))

  ;; Initialize
  (define live-out (make-hash))
  (define live-in (make-hash))
  (define block-liveness (make-hash))

  (for ([bid (in-list block-ids)])
    (hash-set! live-out bid (set))
    (hash-set! live-in bid (set)))

  ;; Build successor/predecessor maps
  (define succs (make-hash))
  (define preds (make-hash))

  (for ([bid (in-list block-ids)])
    (hash-set! succs bid '())
    (hash-set! preds bid '()))

  (for ([bid (in-list block-ids)])
    (define block (cfg-get-block cfg bid))
    (when block
      (define term (AsmBlock-terminator block))
      (define targets (terminator-targets term))
      (for ([target targets])
        (hash-update! succs bid (lambda (s) (cons target s)) '())
        (hash-update! preds target (lambda (p) (cons bid p)) '()))))

  ;; Worklist algorithm (backward dataflow)
  (define worklist (reverse block-ids))
  (define in-worklist (list->set block-ids))

  (let loop ()
    (unless (null? worklist)
      (define bid (car worklist))
      (set! worklist (cdr worklist))
      (set! in-worklist (set-remove in-worklist bid))

      (define block (cfg-get-block cfg bid))
      (when block
        ;; live-out[B] = ∪ live-in[S] for all successors S
        (define new-out
          (for/fold ([out (set)])
                    ([s (hash-ref succs bid '())])
            (set-union out (hash-ref live-in s (set)))))

        (hash-set! live-out bid new-out)

        ;; Compute liveness for instructions in block
        (define-values (insn-liveness new-in)
          (compute-block-liveness (AsmBlock-insns block) new-out))

        (hash-set! block-liveness bid insn-liveness)

        ;; If live-in changed, add predecessors to worklist
        (unless (equal? new-in (hash-ref live-in bid (set)))
          (hash-set! live-in bid new-in)
          (for ([p (hash-ref preds bid '())])
            (unless (set-member? in-worklist p)
              (set! worklist (cons p worklist))
              (set! in-worklist (set-add in-worklist p))))))

      (loop)))

  (LivenessInfo block-liveness live-in live-out))

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

;; Get live registers after a specific instruction
(define (get-live-after liveness-info block-id insn-index)
  (define block-info (hash-ref (LivenessInfo-block-info liveness-info) block-id #f))
  (if (and block-info (< insn-index (pvector-length block-info)))
      (InsnLiveness-live-after (pvector-ref block-info insn-index))
      (set)))
