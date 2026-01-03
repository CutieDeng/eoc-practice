#lang racket/base

;; AArch64 Assembly CFG (Control Flow Graph) Representation
;;
;; Uses persistent data structures from cutie-ftree:
;; - pvector for instruction sequences (O(log n) random access, O(log n) append)
;; - ordered-map for block storage (deterministic iteration, efficient lookup)
;; - bitset for register sets in analysis

(require racket/match
         "../../../../cutie-ftree/pvector.rkt"
         "../../../../cutie-ftree/ordered-map.rkt"
         "../../../../cutie-ftree/bitset.rkt"
         "../../../../cutie-ftree/comparator.rkt"
         "types.rkt")

(provide
 ;; Block ID type
 BlockId BlockId? BlockId-id
 block-id-compare

 ;; Block structure
 AsmBlock AsmBlock? AsmBlock-id AsmBlock-label AsmBlock-insns AsmBlock-terminator AsmBlock-info
 make-empty-block
 block-append-insn
 block-append-insns
 block-set-terminator
 block-insn-count

 ;; Terminators
 Term:ret Term:ret?
 Term:jump Term:jump? Term:jump-target
 Term:cond Term:cond? Term:cond-cond Term:cond-then-target Term:cond-else-target
 Term:switch Term:switch? Term:switch-value Term:switch-cases Term:switch-default
 Term:unreachable Term:unreachable?
 terminator?
 terminator-successors

 ;; CFG structure
 AsmCfg AsmCfg? AsmCfg-entry AsmCfg-blocks AsmCfg-next-block-id AsmCfg-next-label-id AsmCfg-config
 make-empty-cfg
 cfg-add-block
 cfg-get-block
 cfg-update-block
 cfg-remove-block
 cfg-block-count
 cfg-entry-block
 in-cfg-blocks
 in-cfg-block-ids

 ;; Label management
 cfg-fresh-block-id
 cfg-fresh-label

 ;; CFG traversal
 cfg-successors
 cfg-predecessors
 cfg-reachable-blocks

 ;; Block sequence utilities
 pvector-insns->list
 list->pvector-insns)

;; ============================================================================
;; Block ID
;; ============================================================================

(struct BlockId (id) #:prefab)

(define (block-id-compare a b)
  (cond
    [(< (BlockId-id a) (BlockId-id b)) '<]
    [(> (BlockId-id a) (BlockId-id b)) '>]
    [else '=]))

;; ============================================================================
;; Terminators
;; ============================================================================

;; Return from function
(struct Term:ret () #:prefab)

;; Unconditional jump
(struct Term:jump (target) #:prefab)

;; Conditional branch
(struct Term:cond (cond then-target else-target) #:prefab)

;; Switch/table jump
(struct Term:switch (value cases default) #:prefab)

;; Unreachable (for optimization)
(struct Term:unreachable () #:prefab)

(define (terminator? x)
  (or (Term:ret? x) (Term:jump? x) (Term:cond? x)
      (Term:switch? x) (Term:unreachable? x)))

;; Get successor block IDs from a terminator
(define (terminator-successors term)
  (match term
    [(Term:ret) '()]
    [(Term:jump target) (list target)]
    [(Term:cond _ then-target else-target) (list then-target else-target)]
    [(Term:switch _ cases default)
     (cons default (map cdr cases))]
    [(Term:unreachable) '()]))

;; ============================================================================
;; Basic Block with pvector for instructions
;; ============================================================================

;; AsmBlock:
;; - id: BlockId
;; - label: Label:id or Label:named (for emission)
;; - insns: pvector of instructions
;; - terminator: terminator instruction
;; - info: ordered-map of metadata (liveness, etc.)
(struct AsmBlock (id label insns terminator info) #:prefab)

(define (make-empty-block id label)
  (AsmBlock id label (pvector-empty) #f (ordered-map-empty symbol-compare)))

;; Append a single instruction to the block
(define (block-append-insn block insn)
  (struct-copy AsmBlock block
               [insns (pvector-cons-right (AsmBlock-insns block) insn)]))

;; Append multiple instructions
(define (block-append-insns block insn-list)
  (define new-insns
    (for/fold ([pv (AsmBlock-insns block)])
              ([insn (in-list insn-list)])
      (pvector-cons-right pv insn)))
  (struct-copy AsmBlock block [insns new-insns]))

;; Set the terminator
(define (block-set-terminator block term)
  (struct-copy AsmBlock block [terminator term]))

;; Get instruction count
(define (block-insn-count block)
  (pvector-length (AsmBlock-insns block)))

;; ============================================================================
;; CFG Structure with ordered-map for blocks
;; ============================================================================

;; AsmCfg:
;; - entry: BlockId of entry block
;; - blocks: ordered-map of BlockId -> AsmBlock
;; - next-block-id: counter for fresh block IDs
;; - next-label-id: counter for fresh label IDs
;; - config: AsmConfig (imported from config.rkt, or #f)
(struct AsmCfg (entry blocks next-block-id next-label-id config) #:prefab)

(define (make-empty-cfg #:config [config #f])
  (AsmCfg #f
          (ordered-map-empty block-id-compare)
          0
          0
          config))

;; Generate fresh block ID
(define (cfg-fresh-block-id cfg)
  (define id (AsmCfg-next-block-id cfg))
  (values (BlockId id)
          (struct-copy AsmCfg cfg [next-block-id (add1 id)])))

;; Generate fresh label
(define (cfg-fresh-label cfg #:prefix [prefix "L"])
  (define id (AsmCfg-next-label-id cfg))
  (values (Label:id id)
          (struct-copy AsmCfg cfg [next-label-id (add1 id)])))

;; Add a block to the CFG
(define (cfg-add-block cfg block #:set-entry? [set-entry? #f])
  (define new-blocks
    (ordered-map-set (AsmCfg-blocks cfg)
                     (AsmBlock-id block)
                     block))
  (struct-copy AsmCfg cfg
               [blocks new-blocks]
               [entry (if set-entry? (AsmBlock-id block) (AsmCfg-entry cfg))]))

;; Get a block by ID
(define (cfg-get-block cfg block-id)
  (ordered-map-ref (AsmCfg-blocks cfg) block-id #f))

;; Update a block in the CFG
(define (cfg-update-block cfg block-id updater)
  (define old-block (cfg-get-block cfg block-id))
  (when (not old-block)
    (error 'cfg-update-block "block not found: ~a" block-id))
  (define new-block (updater old-block))
  (define new-blocks
    (ordered-map-set (AsmCfg-blocks cfg) block-id new-block))
  (struct-copy AsmCfg cfg [blocks new-blocks]))

;; Remove a block
(define (cfg-remove-block cfg block-id)
  (define-values (new-blocks _)
    (ordered-map-delete (AsmCfg-blocks cfg) block-id))
  (struct-copy AsmCfg cfg [blocks new-blocks]))

;; Get block count
(define (cfg-block-count cfg)
  (ordered-map-count (AsmCfg-blocks cfg)))

;; Get entry block
(define (cfg-entry-block cfg)
  (cfg-get-block cfg (AsmCfg-entry cfg)))

;; Iterate over blocks
(define (in-cfg-blocks cfg)
  (in-ordered-map-values (AsmCfg-blocks cfg)))

(define (in-cfg-block-ids cfg)
  (in-ordered-map-keys (AsmCfg-blocks cfg)))

;; ============================================================================
;; CFG Traversal
;; ============================================================================

;; Get successors of a block
(define (cfg-successors cfg block-id)
  (define block (cfg-get-block cfg block-id))
  (if (and block (AsmBlock-terminator block))
      (terminator-successors (AsmBlock-terminator block))
      '()))

;; Compute predecessors (returns ordered-map: BlockId -> (listof BlockId))
(define (cfg-predecessors cfg)
  (define pred-map
    (for/fold ([m (ordered-map-empty block-id-compare)])
              ([block-id (in-cfg-block-ids cfg)])
      ;; Initialize with empty list
      (ordered-map-set m block-id '())))

  ;; Add edges
  (for/fold ([m pred-map])
            ([block-id (in-cfg-block-ids cfg)])
    (for/fold ([m2 m])
              ([succ-id (in-list (cfg-successors cfg block-id))])
      (define old-preds (ordered-map-ref m2 succ-id '()))
      (ordered-map-set m2 succ-id (cons block-id old-preds)))))

;; Find all reachable blocks from entry (returns bitset of block IDs)
(define (cfg-reachable-blocks cfg)
  (define entry (AsmCfg-entry cfg))
  (if (not entry)
      (bitset)
      (let loop ([worklist (list entry)]
                 [visited (bitset)])
        (if (null? worklist)
            visited
            (let ([current (car worklist)]
                  [rest (cdr worklist)])
              (if (bitset-member? visited (BlockId-id current))
                  (loop rest visited)
                  (let ([new-visited (bitset-add visited (BlockId-id current))]
                        [succs (cfg-successors cfg current)])
                    (loop (append succs rest) new-visited))))))))

;; ============================================================================
;; Utility: Convert between pvector and list
;; ============================================================================

(define (pvector-insns->list pv)
  (pvector->list pv))

(define (list->pvector-insns lst)
  (list->pvector lst))
