#lang racket/base

;; AArch64 Assembly CFG (Control Flow Graph) Representation
;;
;; Uses cutie-ftree/graph for control flow structure:
;; - graph for vertices (blocks) and edges (control flow)
;; - ordered-map for block data storage
;; - bitset for efficient set operations
;;
;; This design separates the graph structure from block content,
;; enabling use of graph algorithms (SCC, traversal, etc.) directly.

(require racket/match
         racket/dict
         "../../../../../cutie-ftree/pvector.rkt"
         "../../../../../cutie-ftree/ordered-map.rkt"
         "../../../../../cutie-ftree/bitset.rkt"
         "../../../../../cutie-ftree/comparator.rkt"
         "../../../../../cutie-ftree/graph.rkt"
         "types.rkt")

(provide
 ;; Block ID type (re-export from graph)
 vertex-id vertex-id? vertex-id-val

 ;; Block ID comparator
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
 AsmCfg AsmCfg?
 AsmCfg-entry
 AsmCfg-graph
 AsmCfg-block-data
 AsmCfg-next-label-id
 AsmCfg-config
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

 ;; CFG traversal (using graph structure)
 cfg-successors
 cfg-predecessors
 cfg-reachable-blocks

 ;; Edge management
 cfg-add-edge
 cfg-remove-edge

 ;; Block sequence utilities
 pvector-insns->list
 list->pvector-insns

 ;; Re-export useful graph functions
 graph-successors
 graph-predecessors
 graph-vertices-set)

;; ============================================================================
;; Block ID (using vertex-id from cutie-ftree/graph)
;; ============================================================================

;; Re-use vertex-id as block ID
;; vertex-id is already exported from cutie-ftree/graph

(define (block-id-compare a b)
  (vertex-id-compare a b))

;; ============================================================================
;; Terminators
;; ============================================================================

;; Return from function
(struct Term:ret () #:prefab)

;; Unconditional jump - target is vertex-id
(struct Term:jump (target) #:prefab)

;; Conditional branch - targets are vertex-ids
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
    [(Term:cond _ then-target else-target)
     (if else-target
         (list then-target else-target)
         (list then-target))]
    [(Term:switch _ cases default)
     (cons default (map cdr cases))]
    [(Term:unreachable) '()]
    [#f '()]))

;; ============================================================================
;; Basic Block with pvector for instructions
;; ============================================================================

;; AsmBlock:
;; - id: vertex-id (from cutie-ftree/graph)
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
;; CFG Structure using cutie-ftree/graph
;; ============================================================================

;; AsmCfg:
;; - graph: cutie-ftree/graph - vertices are blocks, edges are control flow
;; - block-data: ordered-map[vertex-id-val -> AsmBlock]
;; - entry: vertex-id of entry block (or #f)
;; - next-label-id: counter for fresh label IDs
;; - config: AsmConfig (or #f)
(struct AsmCfg (graph block-data entry next-label-id config) #:prefab)

(define (make-empty-cfg #:config [config #f])
  (AsmCfg graph-empty
          (ordered-map-empty integer-compare)
          #f
          0
          config))

;; Generate fresh block ID (allocates a vertex in the graph)
(define (cfg-fresh-block-id cfg)
  (define-values (new-graph vid) (graph-add-vertex (AsmCfg-graph cfg)))
  (values vid
          (struct-copy AsmCfg cfg [graph new-graph])))

;; Generate fresh label
(define (cfg-fresh-label cfg #:prefix [prefix "L"])
  (define id (AsmCfg-next-label-id cfg))
  (values (Label:id id)
          (struct-copy AsmCfg cfg [next-label-id (add1 id)])))

;; Add a block to the CFG
;; Note: The block's id must be a valid vertex-id in the graph
(define (cfg-add-block cfg block #:set-entry? [set-entry? #f])
  (define bid (AsmBlock-id block))
  (define vid-val (vertex-id-val bid))

  ;; Verify the vertex exists in the graph
  (unless (graph-vertex? (AsmCfg-graph cfg) bid)
    (error 'cfg-add-block "block id not in graph: ~a" bid))

  ;; Store block data
  (define new-block-data
    (ordered-map-set (AsmCfg-block-data cfg) vid-val block))

  ;; Add edges based on terminator
  (define term (AsmBlock-terminator block))
  (define successors (terminator-successors term))
  (define new-graph
    (for/fold ([g (AsmCfg-graph cfg)])
              ([succ (in-list successors)])
      ;; Only add edge if target vertex exists
      (if (and succ (vertex-id? succ) (graph-vertex? g succ))
          (let-values ([(g* _) (graph-add-edge g bid succ)])
            g*)
          g)))

  (struct-copy AsmCfg cfg
               [graph new-graph]
               [block-data new-block-data]
               [entry (if set-entry? bid (AsmCfg-entry cfg))]))

;; Get a block by ID
(define (cfg-get-block cfg block-id)
  (define vid-val
    (cond
      [(vertex-id? block-id) (vertex-id-val block-id)]
      [(integer? block-id) block-id]
      [else (error 'cfg-get-block "invalid block-id: ~a" block-id)]))
  (ordered-map-ref (AsmCfg-block-data cfg) vid-val #f))

;; Update a block in the CFG
(define (cfg-update-block cfg block-id updater)
  (define old-block (cfg-get-block cfg block-id))
  (when (not old-block)
    (error 'cfg-update-block "block not found: ~a" block-id))

  (define new-block (updater old-block))
  (define vid-val (vertex-id-val (AsmBlock-id new-block)))

  ;; Update block data
  (define new-block-data
    (ordered-map-set (AsmCfg-block-data cfg) vid-val new-block))

  ;; TODO: Update edges if terminator changed
  ;; For now, edges must be managed manually via cfg-add-edge/cfg-remove-edge

  (struct-copy AsmCfg cfg [block-data new-block-data]))

;; Remove a block
(define (cfg-remove-block cfg block-id)
  (define vid-val (vertex-id-val block-id))

  ;; Remove block data
  (define-values (new-block-data _)
    (ordered-map-delete (AsmCfg-block-data cfg) vid-val))

  ;; Remove vertex from graph (this also removes edges)
  (define new-graph
    (if (graph-vertex? (AsmCfg-graph cfg) block-id)
        (graph-remove-vertex* (AsmCfg-graph cfg) block-id)
        (AsmCfg-graph cfg)))

  (struct-copy AsmCfg cfg
               [graph new-graph]
               [block-data new-block-data]))

;; Get block count
(define (cfg-block-count cfg)
  (ordered-map-count (AsmCfg-block-data cfg)))

;; Get entry block
(define (cfg-entry-block cfg)
  (define entry (AsmCfg-entry cfg))
  (if entry
      (cfg-get-block cfg entry)
      #f))

;; Iterate over blocks
(define (in-cfg-blocks cfg)
  (in-ordered-map-values (AsmCfg-block-data cfg)))

(define (in-cfg-block-ids cfg)
  (in-generator
    (for ([vid-val (in-ordered-map-keys (AsmCfg-block-data cfg))])
      (yield (vertex-id vid-val)))))

(require racket/generator)

;; ============================================================================
;; Edge Management
;; ============================================================================

;; Add a control flow edge
(define (cfg-add-edge cfg from-id to-id)
  (unless (graph-vertex? (AsmCfg-graph cfg) from-id)
    (error 'cfg-add-edge "source block not in graph: ~a" from-id))
  (unless (graph-vertex? (AsmCfg-graph cfg) to-id)
    (error 'cfg-add-edge "target block not in graph: ~a" to-id))

  (define-values (new-graph _) (graph-add-edge (AsmCfg-graph cfg) from-id to-id))
  (struct-copy AsmCfg cfg [graph new-graph]))

;; Remove a control flow edge
(define (cfg-remove-edge cfg from-id to-id)
  (define new-graph
    (graph-remove-edge-between (AsmCfg-graph cfg) from-id to-id))
  (struct-copy AsmCfg cfg [graph new-graph]))

;; ============================================================================
;; CFG Traversal (using graph structure)
;; ============================================================================

;; Get successors of a block (returns list of vertex-ids)
(define (cfg-successors cfg block-id)
  (define vid
    (if (vertex-id? block-id)
        block-id
        (vertex-id block-id)))
  (define succ-bitset (graph-successors (AsmCfg-graph cfg) vid))
  (for/list ([v (in-bitset succ-bitset)])
    (vertex-id v)))

;; Get predecessors of a block (returns list of vertex-ids)
(define (cfg-predecessors cfg block-id)
  (define vid
    (if (vertex-id? block-id)
        block-id
        (vertex-id block-id)))
  (define pred-bitset (graph-predecessors (AsmCfg-graph cfg) vid))
  (for/list ([v (in-bitset pred-bitset)])
    (vertex-id v)))

;; Find all reachable blocks from entry (returns bitset of vertex-id vals)
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
              (define vid-val (vertex-id-val current))
              (if (bitset-member? visited vid-val)
                  (loop rest visited)
                  (let ([new-visited (bitset-add visited vid-val)]
                        [succs (cfg-successors cfg current)])
                    (loop (append succs rest) new-visited))))))))

;; ============================================================================
;; Utility: Convert between pvector and list
;; ============================================================================

(define (pvector-insns->list pv)
  (pvector->list pv))

(define (list->pvector-insns lst)
  (list->pvector lst))

;; ============================================================================
;; Backward Compatibility: BlockId alias
;; ============================================================================

;; For code that still uses BlockId
(define BlockId vertex-id)
(define BlockId? vertex-id?)
(define BlockId-id vertex-id-val)

(provide BlockId BlockId? BlockId-id)
