#lang racket/base

;; ============================================================
;; Driver: SCC-based Liveness Analysis Framework
;; ============================================================
;;
;; High-performance liveness analysis using:
;;   1. Variable normalization to integers (intbits representation)
;;   2. SCC decomposition for efficient loop handling
;;   3. DAG traversal for linear-time convergence
;;
;; Backend implementations provide concrete interfaces.
;;
;; Block ids are opaque values compared with equal?; block-keyed
;; maps are immutable hashes.  SCC-keyed maps are intmaps.
;;
;; ============================================================

(require racket/class
         racket/contract
         racket/intmap
         "../../kernel/data/data.rkt"
         "../graph/scc.rkt")

(provide
  ;; Interface
  liveness-interface<%>

  ;; Core algorithm
  compute-liveness

  ;; Result structures
  (struct-out LivenessResult)
  (struct-out BlockLiveness)
  (struct-out InsnLiveness))

;; ============================================================
;; Abstract Interface
;; ============================================================

;; Interface that backends must implement
;; All variables are normalized to non-negative integers for bitset efficiency
;;
(define liveness-interface<%>
  (interface ()
    ;; === Variable normalization ===
    [var-count (->m exact-nonnegative-integer?)]         ; Total variable count (intbits size)
    [var->id (->m any/c exact-nonnegative-integer?)]     ; Variable -> integer ID
    [id->var (->m exact-nonnegative-integer? any/c)]     ; ID -> variable

    ;; === CFG queries ===
    [block-ids (->m any/c pvector?)]                     ; CFG -> pvector of block IDs
    [get-block (->m any/c any/c any/c)]                  ; CFG, BlockId -> Block or #f
    [block-insns (->m any/c pvector?)]                   ; Block -> pvector of instructions
    [block-successors (->m any/c any/c pvector?)]        ; CFG, BlockId -> pvector of successor IDs
    [block-predecessors (->m any/c any/c pvector?)]      ; CFG, BlockId -> pvector of predecessor IDs

    ;; === Instruction def/use (returns bitset) ===
    [insn-defs-bitset (->m any/c intbits?)]               ; Instruction -> bitset of defined vars
    [insn-uses-bitset (->m any/c intbits?)]))             ; Instruction -> bitset of used vars

;; ============================================================
;; Result Structures
;; ============================================================

;; Complete liveness result for a CFG
(struct LivenessResult (
  block-info     ; hash[BlockId -> BlockLiveness]
  live-in        ; hash[BlockId -> bitset]
  live-out       ; hash[BlockId -> bitset]
) #:prefab)

;; Block-level liveness info
(struct BlockLiveness (
  insn-liveness  ; pvector[InsnLiveness]
  gen            ; bitset - variables used before defined in block
  kill           ; bitset - variables defined in block
) #:prefab)

;; Instruction-level liveness info
(struct InsnLiveness (
  live-before    ; bitset
  live-after     ; bitset
) #:prefab)

;; ============================================================
;; Block Gen/Kill Computation
;; ============================================================

;; Compute gen and kill sets for a basic block
;; gen = variables used before being defined in block
;; kill = variables defined in block
;;
(define (compute-block-gen-kill interface insns)
  (for/fold ([gen intbits-empty]
             [kill intbits-empty])
            ([insn (in-pvector insns)])
    (define defs (send interface insn-defs-bitset insn))
    (define uses (send interface insn-uses-bitset insn))
    ;; gen += (uses - kill)
    ;; kill += defs
    (values (intbits-union gen (intbits-subtract uses kill))
            (intbits-union kill defs))))

;; ============================================================
;; Per-Block Instruction Liveness
;; ============================================================

;; Compute liveness for each instruction in a block
;; Given block's live-out, compute live-before/live-after for each insn
;;
(define (compute-insn-liveness interface insns live-out)
  (define n (pvector-length insns))
  ;; Process backward from live-out
  (define-values (liveness _)
    (for/fold ([result (pvector-empty)]
               [live live-out])
              ([i (in-range (sub1 n) -1 -1)])
      (define insn (pvector-ref insns i))
      (define defs (send interface insn-defs-bitset insn))
      (define uses (send interface insn-uses-bitset insn))
      ;; live-before = uses ∪ (live-after - defs)
      (define live-before (intbits-union uses (intbits-subtract live defs)))
      (values (pvector-cons-left result (InsnLiveness live-before live))
              live-before)))
  liveness)

;; ============================================================
;; SCC-based Liveness Analysis
;; ============================================================

;; Main entry point: compute liveness using SCC decomposition
;;
(define (compute-liveness cfg interface)
  ;; Step 1: Collect all block IDs
  (define block-ids (send interface block-ids cfg))

  ;; Step 2: Build successor function for SCC computation
  (define (get-successors bid)
    (send interface block-successors cfg bid))

  ;; Step 3: Compute SCCs and build condensation DAG
  (define-values (node->scc-id scc-nodes scc-successors)
    (condensation-graph block-ids get-successors))
  (define num-sccs (pvector-length scc-nodes))

  ;; Step 4: Precompute gen/kill for all blocks
  (define block-gen-kill
    (for/fold ([m (hash)])
              ([bid (in-pvector block-ids)])
      (define block (send interface get-block cfg bid))
      (if (not block)
          m
          (let-values ([(gen kill)
                        (compute-block-gen-kill interface (send interface block-insns block))])
            (hash-set m bid (cons gen kill))))))

  ;; Helper to get gen/kill
  (define (block-gen bid)
    (define gk (hash-ref block-gen-kill bid #f))
    (if gk (car gk) intbits-empty))
  (define (block-kill bid)
    (define gk (hash-ref block-gen-kill bid #f))
    (if gk (cdr gk) intbits-empty))

  ;; Step 5: Compute live-out for each SCC in reverse topological order
  ;; Tarjan returns SCCs in reverse topological order
  (define scc-live-out
    (for/fold ([scc-out intmap-empty])
              ([scc-id (in-range (sub1 num-sccs) -1 -1)])
      ;; Collect live-in from successor SCCs
      (define succ-sccs (scc-successors scc-id))
      (define exit-live
        (for/fold ([live intbits-empty])
                  ([succ-scc (in-pvector succ-sccs)])
          (intbits-union live (intmap-ref scc-out succ-scc intbits-empty))))

      ;; Compute fixed point within this SCC
      (define scc-blocks (pvector-ref scc-nodes scc-id))
      (define scc-result
        (compute-scc-liveness cfg interface scc-blocks
                              block-gen block-kill exit-live get-successors))

      ;; Update scc-out with live-in of this SCC (for predecessor SCCs)
      (define scc-live-in
        (for/fold ([live intbits-empty])
                  ([bid (in-pvector scc-blocks)])
          (intbits-union live (hash-ref scc-result bid intbits-empty))))

      (intmap-set scc-out scc-id scc-live-in)))

  ;; Step 6: Compute final live-in/live-out for all blocks
  (define-values (live-in live-out)
    (compute-final-liveness cfg interface block-ids
                            block-gen block-kill get-successors))

  ;; Step 7: Compute instruction-level liveness
  (define block-info
    (for/fold ([m (hash)])
              ([bid (in-pvector block-ids)])
      (define block (send interface get-block cfg bid))
      (if (not block)
          m
          (let* ([insns (send interface block-insns block)]
                 [out (hash-ref live-out bid intbits-empty)]
                 [insn-lv (compute-insn-liveness interface insns out)]
                 [gen (block-gen bid)]
                 [kill (block-kill bid)])
            (hash-set m bid (BlockLiveness insn-lv gen kill))))))

  (LivenessResult block-info live-in live-out))

;; ============================================================
;; SCC Internal Fixed-Point Computation
;; ============================================================

;; Compute liveness within a single SCC
;; For SCCs with single block (no loop), one pass suffices
;; For SCCs with multiple blocks (loop), iterate until fixed point
;;
(define (compute-scc-liveness cfg interface scc-blocks
                               block-gen block-kill exit-live get-successors)
  ;; Create set of blocks in this SCC for fast lookup
  (define scc-set
    (for/fold ([s (hash)])
              ([bid (in-pvector scc-blocks)])
      (hash-set s bid #t)))

  (define (in-scc? bid)
    (hash-has-key? scc-set bid))

  ;; Initialize live-out
  (define init-out
    (for/fold ([m (hash)])
              ([bid (in-pvector scc-blocks)])
      (hash-set m bid exit-live)))

  ;; Fixed-point iteration
  (let loop ([out init-out])
    (define-values (new-out changed)
      (for/fold ([m out] [ch #f])
                ([bid (in-pvector scc-blocks)])
        ;; live-out[B] = ∪ live-in[S] for successors S
        ;; live-in[S] = gen[S] ∪ (live-out[S] - kill[S])
        (define succs (get-successors bid))
        (define new-live-out
          (for/fold ([live intbits-empty])
                    ([succ (in-pvector succs)])
            (define succ-out
              (if (in-scc? succ)
                  (hash-ref m succ intbits-empty)
                  exit-live))  ; External successor uses exit-live
            (define succ-in
              (intbits-union (block-gen succ)
                             (intbits-subtract succ-out (block-kill succ))))
            (intbits-union live succ-in)))
        (define old-out (hash-ref m bid intbits-empty))
        (if (= old-out new-live-out)
            (values m ch)
            (values (hash-set m bid new-live-out) #t))))
    (if changed
        (loop new-out)
        new-out)))

;; ============================================================
;; Final Liveness Computation
;; ============================================================

;; Compute final live-in/live-out for all blocks
;;
(define (compute-final-liveness cfg interface block-ids
                                 block-gen block-kill get-successors)
  ;; Initialize
  (define init-out
    (for/fold ([m (hash)])
              ([bid (in-pvector block-ids)])
      (hash-set m bid intbits-empty)))

  ;; Worklist iteration — block-ids is already a pvector, no round-trip needed.
  (define in-worklist
    (for/fold ([s (hash)])
              ([bid (in-pvector block-ids)])
      (hash-set s bid #t)))

  (let loop ([out init-out]
             [wl block-ids]
             [in-wl in-worklist])
    (if (pvector-empty? wl)
        ;; Compute live-in from live-out
        (let ([live-in
               (for/fold ([m (hash)])
                         ([bid (in-pvector block-ids)])
                 (define out-val (hash-ref out bid intbits-empty))
                 (define in-val (intbits-union (block-gen bid)
                                               (intbits-subtract out-val (block-kill bid))))
                 (hash-set m bid in-val))])
          (values live-in out))
        ;; Process next block
        (let* ([bid (pvector-ref wl 0)]
               [rest-wl (pvector-drop wl 1)]
               [new-in-wl (hash-remove in-wl bid)])
          ;; Compute new live-out
          (define succs (get-successors bid))
          (define new-out
            (for/fold ([live intbits-empty])
                      ([succ (in-pvector succs)])
              (define succ-out (hash-ref out succ intbits-empty))
              (define succ-in (intbits-union (block-gen succ)
                                             (intbits-subtract succ-out (block-kill succ))))
              (intbits-union live succ-in)))
          (define old-out (hash-ref out bid intbits-empty))
          (if (= old-out new-out)
              (loop out rest-wl new-in-wl)
              ;; Add predecessors to worklist
              (let* ([new-out-map (hash-set out bid new-out)]
                     [preds (send interface block-predecessors cfg bid)])
                (define-values (updated-wl updated-in-wl)
                  (for/fold ([w rest-wl] [iw new-in-wl])
                            ([p (in-pvector preds)])
                    (if (hash-has-key? iw p)
                        (values w iw)
                        (values (pvector-cons-right w p)
                                (hash-set iw p #t)))))
                (loop new-out-map updated-wl updated-in-wl)))))))
