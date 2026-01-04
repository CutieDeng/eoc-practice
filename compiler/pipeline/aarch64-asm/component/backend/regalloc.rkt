#lang racket/base

;; ============================================================================
;; AArch64 Register Allocator (Pipeline Version)
;; ============================================================================
;;
;; Graph coloring register allocator for AArch64.
;; Uses bitset for all set operations (variables normalized to integers).
;; Integrates with driver/dataflow/liveness framework.
;; Adapted for new graph-based CFG with vertex-id block IDs.
;;
;; ============================================================================

(require racket/match
         racket/list
         racket/dict
         "../ir/types.rkt"
         "../ir/cfg.rkt"
         "../analysis/liveness.rkt"
         "../../../../../cutie-ftree/pvector.rkt"
         "../../../../../cutie-ftree/ordered-map.rkt"
         "../../../../../cutie-ftree/bitset.rkt"
         "../../../../../cutie-ftree/comparator.rkt"
         (only-in "../../../../../cutie-ftree/graph.rkt" vertex-id? vertex-id-val))

(provide
 ;; Main entry point
 allocate-registers

 ;; Result structure
 (struct-out AllocationResult)

 ;; Variable normalization
 (struct-out VRegIndex)
 build-vreg-index
 vreg-id->idx
 idx->vreg-id

 ;; Utilities
 build-interference-graph
 color-graph

 ;; Register sets
 gpr-caller-saved
 gpr-callee-saved
 gpr-allocatable
 sve-allocatable
 pred-allocatable

 ;; Apply allocation
 apply-allocation
 rewrite-insn)

;; ============================================================================
;; Register Sets
;; ============================================================================

(define gpr-caller-saved
  (for/list ([i (in-range 0 19)])
    (Reg:x i)))

(define gpr-callee-saved
  (for/list ([i (in-range 19 29)])
    (Reg:x i)))

(define gpr-allocatable
  (append gpr-caller-saved gpr-callee-saved))

(define sve-allocatable
  (for/list ([i (in-range 0 32)])
    (Reg:z i)))

(define pred-allocatable
  (for/list ([i (in-range 0 16)])
    (Reg:p i)))

;; ============================================================================
;; Allocation Result
;; ============================================================================

(struct AllocationResult (
  assignment        ; ordered-map[VReg-id -> PhysicalReg]
  spilled          ; bitset - vreg IDs that need to be spilled
  callee-saved-used ; bitset - physical register indices used
  success?         ; Boolean
) #:prefab)

;; ============================================================================
;; Variable Normalization
;; ============================================================================

;; Build vreg-id <-> integer mapping
(struct VRegIndex (
  id->idx    ; ordered-map[symbol -> int]
  idx->id    ; pvector[int -> symbol]
  count      ; total count
) #:prefab)

(define (build-vreg-index vregs)
  (define-values (id->idx idx->id)
    (for/fold ([m (ordered-map-empty symbol-compare)]
               [v (pvector-empty)])
              ([vreg vregs]
               [i (in-naturals)])
      (values (ordered-map-set m (vreg-id vreg) i)
              (pvector-cons-right v (vreg-id vreg)))))
  (VRegIndex id->idx idx->id (length vregs)))

(define (vreg-id->idx index vid)
  (define result (ordered-map-query (VRegIndex-id->idx index) vid))
  (and result (cdr result)))

(define (idx->vreg-id index idx)
  (pvector-ref (VRegIndex-idx->id index) idx))

;; ============================================================================
;; Interference Graph (using bitset for neighbors)
;; ============================================================================

;; Interference graph: pvector[bitset] - neighbors for each vertex
;; Vertex i's neighbors are stored at index i

(define (make-empty-igraph n)
  (for/pvector ([_ (in-range n)])
    bitset-empty))

(define (igraph-add-edge g v1 v2)
  (let* ([neighbors1 (pvector-ref g v1)]
         [neighbors2 (pvector-ref g v2)]
         [g1 (pvector-set g v1 (bitset-add neighbors1 v2))]
         [g2 (pvector-set g1 v2 (bitset-add neighbors2 v1))])
    g2))

(define (igraph-neighbors g v)
  (pvector-ref g v))

(define (igraph-degree g v)
  (bitset-count (igraph-neighbors g v)))

;; ============================================================================
;; Collect Virtual Registers
;; ============================================================================

(define (collect-vregs cfg)
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
;; Build Interference Graph
;; ============================================================================

;; Helper to get block-id-val for result lookup
(define (get-block-id-val bid)
  (cond
    [(vertex-id? bid) (vertex-id-val bid)]
    [(and (struct? bid) (BlockId? bid)) (BlockId-id bid)]
    [(integer? bid) bid]
    [else (error 'get-block-id-val "unknown block id type: ~a" bid)]))

(define (build-interference-graph cfg liveness-result vreg-index)
  (define n (VRegIndex-count vreg-index))
  (define id->idx (VRegIndex-id->idx vreg-index))
  (define block-info (LivenessResult-block-info liveness-result))

  ;; Build graph using bitset neighbors
  (for/fold ([graph (make-empty-igraph n)])
            ([bid (in-cfg-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if (not block)
        graph
        (let* ([vid-val (get-block-id-val bid)]
               [block-query (ordered-map-query block-info vid-val)]
               [block-liveness (and block-query (cdr block-query))])
          (if (not block-liveness)
              graph
              (let ([insn-liveness (BlockLiveness-insn-liveness block-liveness)])
                (for/fold ([g graph])
                          ([i (in-range (pvector-length (AsmBlock-insns block)))])
                  (define insn (pvector-ref (AsmBlock-insns block) i))
                  (define liveness (pvector-ref insn-liveness i))
                  (define live-after (InsnLiveness-live-after liveness))
                  (define defs (insn-defs insn))

                  ;; For each defined vreg, add edge to all other live vregs
                  (for*/fold ([g2 g])
                             ([d defs]
                              #:when (vreg? d)
                              [l (in-bitset live-after)])
                    (define d-idx (dict-ref id->idx (vreg-id d) #f))
                    (cond
                      [(not d-idx) g2]
                      [(= d-idx l) g2]  ; Skip self
                      [else (igraph-add-edge g2 d-idx l)])))))))))

;; ============================================================================
;; Graph Coloring
;; ============================================================================

(define (color-graph graph num-vertices num-colors)
  ;; Simplify phase: remove nodes with degree < num-colors
  (define-values (stack removed)
    (let simplify ([stk (pvector-empty)] [rem bitset-empty])
      ;; Find vertex with degree < num-colors (not yet removed)
      (define found-v
        (for/first ([v (in-range num-vertices)]
                    #:when (and (not (bitset-member? rem v))
                                (< (bitset-count (bitset-subtract (igraph-neighbors graph v) rem))
                                   num-colors)))
          v))
      (if found-v
          (simplify (pvector-cons-right stk found-v)
                    (bitset-add rem found-v))
          (values stk rem))))

  ;; Remaining high-degree nodes -> potential spills
  (define remaining
    (for/list ([v (in-range num-vertices)]
               #:when (not (bitset-member? removed v)))
      v))

  (define-values (final-stack initial-spills)
    (if (null? remaining)
        (values stack bitset-empty)
        ;; Add all remaining to stack, mark highest-degree as spill
        (let ([highest-degree-node
               (argmax (lambda (v)
                         (bitset-count (bitset-subtract (igraph-neighbors graph v) removed)))
                       remaining)])
          (values (pvector-append stack (list->pvector remaining))
                  (bitset-add bitset-empty highest-degree-node)))))

  ;; Select phase: assign colors
  (define-values (coloring to-spill)
    (for/fold ([col (ordered-map-empty integer-compare)]
               [spills initial-spills])
              ([i (in-range (sub1 (pvector-length final-stack)) -1 -1)])
      (define v (pvector-ref final-stack i))
      (if (bitset-member? spills v)
          (values col spills)
          ;; Find available color
          (let* ([neighbor-colors
                  (for/fold ([cs bitset-empty])
                            ([n (in-bitset (igraph-neighbors graph v))])
                    (define c (dict-ref col n #f))
                    (if c (bitset-add cs c) cs))]
                 [available-color
                  (for/first ([c (in-range num-colors)]
                              #:when (not (bitset-member? neighbor-colors c)))
                    c)])
            (if available-color
                (values (ordered-map-set col v available-color) spills)
                (values col (bitset-add spills v)))))))

  (values coloring to-spill))

;; Helper: argmax
(define (argmax f lst)
  (if (null? lst)
      #f
      (for/fold ([best (car lst)]
                 [best-val (f (car lst))]
                 #:result best)
                ([x (cdr lst)])
        (define val (f x))
        (if (> val best-val)
            (values x val)
            (values best best-val)))))

;; ============================================================================
;; Main Register Allocator
;; ============================================================================

(define (allocate-registers cfg)
  ;; Step 1: Collect all vregs and build index
  (define all-vregs (collect-vregs cfg))

  (if (null? all-vregs)
      ;; No virtual registers, nothing to allocate
      (AllocationResult
       (ordered-map-empty symbol-compare)
       bitset-empty
       bitset-empty
       #t)

      (let ()
        ;; Step 2: Compute liveness (with vreg normalization)
        (define vreg-index (build-vreg-index all-vregs))
        (define liveness (compute-liveness cfg vreg-index))

        ;; Step 3: Build vreg info by class
        (define vreg-by-id
          (for/fold ([m (ordered-map-empty symbol-compare)])
                    ([v all-vregs])
            (ordered-map-set m (vreg-id v) v)))

        ;; Separate by class
        (define gpr-vregs (filter (lambda (v) (eq? (vreg-class v) 'gpr)) all-vregs))
        (define sve-vregs (filter (lambda (v) (eq? (vreg-class v) 'sve)) all-vregs))
        (define pred-vregs (filter (lambda (v) (eq? (vreg-class v) 'pred)) all-vregs))

        ;; Step 4: Build and color interference graphs per class
        (define gpr-index (build-vreg-index gpr-vregs))
        (define sve-index (build-vreg-index sve-vregs))
        (define pred-index (build-vreg-index pred-vregs))

        (define gpr-liveness (compute-liveness cfg gpr-index))
        (define sve-liveness (compute-liveness cfg sve-index))
        (define pred-liveness (compute-liveness cfg pred-index))

        (define gpr-graph (build-interference-graph cfg gpr-liveness gpr-index))
        (define sve-graph (build-interference-graph cfg sve-liveness sve-index))
        (define pred-graph (build-interference-graph cfg pred-liveness pred-index))

        (define-values (gpr-coloring gpr-spills)
          (color-graph gpr-graph (VRegIndex-count gpr-index) (length gpr-allocatable)))
        (define-values (sve-coloring sve-spills)
          (color-graph sve-graph (VRegIndex-count sve-index) (length sve-allocatable)))
        (define-values (pred-coloring pred-spills)
          (color-graph pred-graph (VRegIndex-count pred-index) (length pred-allocatable)))

        ;; Step 5: Map colors to physical registers
        (define (assign-class coloring index allocatable asgn callee-used)
          (for/fold ([a asgn] [c callee-used])
                    ([kv (in-ordered-map coloring)])
            (define idx (car kv))
            (define color (cdr kv))
            (define vid (idx->vreg-id index idx))
            (define phys-reg (list-ref allocatable color))
            (values (ordered-map-set a vid phys-reg)
                    (if (and (eq? allocatable gpr-allocatable)
                             (member phys-reg gpr-callee-saved))
                        (bitset-add c color)
                        c))))

        (define-values (asgn1 callee1)
          (assign-class gpr-coloring gpr-index gpr-allocatable
                        (ordered-map-empty symbol-compare) bitset-empty))
        (define-values (asgn2 callee2)
          (assign-class sve-coloring sve-index sve-allocatable asgn1 callee1))
        (define-values (assignment callee-saved-used)
          (assign-class pred-coloring pred-index pred-allocatable asgn2 callee2))

        ;; Combine spills (convert from indices back to vreg-ids)
        (define all-spills
          (bitset-union
           (bitset-union gpr-spills sve-spills)
           pred-spills))

        (AllocationResult
         assignment
         all-spills
         callee-saved-used
         (bitset-empty? all-spills)))))

;; ============================================================================
;; Apply Allocation to CFG
;; ============================================================================

(define (apply-allocation cfg allocation)
  (define assignment (AllocationResult-assignment allocation))

  ;; Update each block in the CFG
  (for/fold ([current-cfg cfg])
            ([bid (in-cfg-block-ids cfg)])
    (cfg-update-block current-cfg bid
      (lambda (block)
        (define new-insns
          (for/pvector ([insn (in-pvector (AsmBlock-insns block))])
            (rewrite-insn insn assignment)))
        (struct-copy AsmBlock block [insns new-insns])))))

(define (rewrite-insn insn assignment)
  (define (rewrite-reg r)
    (if (vreg? r)
        (dict-ref assignment (vreg-id r) r)
        r))

  (define (rewrite-mem m)
    (match m
      [(Mem:base reg) (Mem:base (rewrite-reg reg))]
      [(Mem:offset reg off) (Mem:offset (rewrite-reg reg) off)]
      [(Mem:pre reg off) (Mem:pre (rewrite-reg reg) off)]
      [(Mem:post reg off) (Mem:post (rewrite-reg reg) off)]
      [(Mem:reg base idx) (Mem:reg (rewrite-reg base) (rewrite-reg idx))]
      [(Mem:scaled base idx scale) (Mem:scaled (rewrite-reg base) (rewrite-reg idx) scale)]
      [_ m]))

  (define (rewrite-operand op)
    (cond
      [(vreg? op) (rewrite-reg op)]
      [(mem-addr? op) (rewrite-mem op)]
      [else op]))

  (match insn
    [(Insn:arith op dst src1 src2)
     (Insn:arith op (rewrite-reg dst) (rewrite-operand src1) (rewrite-operand src2))]
    [(Insn:arith2 op dst src)
     (Insn:arith2 op (rewrite-reg dst) (rewrite-operand src))]
    [(Insn:load op dst addr)
     (Insn:load op (rewrite-reg dst) (rewrite-mem addr))]
    [(Insn:store op src addr)
     (Insn:store op (rewrite-operand src) (rewrite-mem addr))]
    [(Insn:mov op dst src)
     (Insn:mov op (rewrite-reg dst) (rewrite-operand src))]
    [(Insn:sve op pred dst srcs)
     (Insn:sve op (rewrite-reg pred) (rewrite-reg dst) (map rewrite-operand srcs))]
    [(Insn:sve-load op pred dst addr)
     (Insn:sve-load op (rewrite-reg pred) (rewrite-reg dst) (rewrite-mem addr))]
    [(Insn:sve-store op pred src addr)
     (Insn:sve-store op (rewrite-reg pred) (rewrite-operand src) (rewrite-mem addr))]
    [(Insn:whilelt pd rn rm)
     (Insn:whilelt (rewrite-reg pd) (rewrite-operand rn) (rewrite-operand rm))]
    [_ insn]))
