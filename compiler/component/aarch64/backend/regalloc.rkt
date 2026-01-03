#lang racket/base

;; ============================================================================
;; AArch64 Register Allocator
;; ============================================================================
;;
;; Graph coloring register allocator for AArch64.
;;
;; Algorithm:
;;   1. Compute liveness analysis
;;   2. Build interference graph (registers that are live at the same time)
;;   3. Graph coloring with simplification and potential spilling
;;   4. Assign physical registers to virtual registers
;;
;; Register classes:
;;   - GPR: x0-x18 (caller-saved), x19-x28 (callee-saved), x29 (FP), x30 (LR)
;;   - SVE: z0-z31
;;   - Pred: p0-p15
;;
;; ============================================================================

(require racket/match
         racket/set
         racket/list
         racket/hash
         "../ir/types.rkt"
         "../ir/cfg.rkt"
         "../analysis/liveness.rkt"
         "../../../../cutie-ftree/pvector.rkt"
         "../../../../cutie-ftree/ordered-map.rkt"
         "../../../../cutie-ftree/comparator.rkt")

(provide
 ;; Main entry point
 allocate-registers

 ;; Result structure
 (struct-out AllocationResult)

 ;; Utilities
 build-interference-graph
 color-graph

 ;; Register sets
 gpr-caller-saved
 gpr-callee-saved
 gpr-allocatable
 sve-allocatable
 pred-allocatable)

;; ============================================================================
;; Register Sets
;; ============================================================================

;; GPR caller-saved registers (can be used freely)
(define gpr-caller-saved
  (for/list ([i (in-range 0 19)])
    (Reg:x i)))

;; GPR callee-saved registers (must save/restore if used)
(define gpr-callee-saved
  (for/list ([i (in-range 19 29)])
    (Reg:x i)))

;; All allocatable GPRs (excluding sp, xzr, fp, lr)
(define gpr-allocatable
  (append gpr-caller-saved gpr-callee-saved))

;; SVE registers (z0-z31)
(define sve-allocatable
  (for/list ([i (in-range 0 32)])
    (Reg:z i)))

;; Predicate registers (p0-p15)
(define pred-allocatable
  (for/list ([i (in-range 0 16)])
    (Reg:p i)))

;; ============================================================================
;; Allocation Result
;; ============================================================================

(struct AllocationResult (
  assignment        ; Hash[VReg-id -> PhysicalReg]
  spilled          ; Set[VReg-id] - registers that need to be spilled
  callee-saved-used ; Set[PhysicalReg] - callee-saved registers used
  success?         ; Boolean
) #:prefab)

;; ============================================================================
;; Simple Interference Graph (using hash tables)
;; ============================================================================

;; Interference graph: Hash[VReg-id -> Set[VReg-id]]
(define (make-empty-igraph)
  (make-hash))

(define (igraph-add-vertex! g v)
  (unless (hash-has-key? g v)
    (hash-set! g v (mutable-set))))

(define (igraph-add-edge! g v1 v2)
  (igraph-add-vertex! g v1)
  (igraph-add-vertex! g v2)
  (set-add! (hash-ref g v1) v2)
  (set-add! (hash-ref g v2) v1))

(define (igraph-neighbors g v)
  (define neighbors (hash-ref g v #f))
  (if neighbors
      (for/set ([n (in-set neighbors)]) n)  ; Convert mutable to immutable
      (set)))

(define (igraph-degree g v)
  (set-count (igraph-neighbors g v)))

(define (igraph-vertices g)
  (hash-keys g))

;; ============================================================================
;; Build Interference Graph
;; ============================================================================

;; Build interference graph from liveness info
(define (build-interference-graph cfg liveness-info)
  (define graph (make-empty-igraph))

  ;; Collect all virtual registers
  (define all-vregs (collect-vregs cfg))

  ;; Add vertices for all vregs
  (for ([v all-vregs])
    (igraph-add-vertex! graph (vreg-id v)))

  ;; Add interference edges based on liveness
  (for ([bid (in-cfg-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (define insns (AsmBlock-insns block))
      (define block-liveness (hash-ref (LivenessInfo-block-info liveness-info) bid #f))

      (when block-liveness
        (for ([i (in-range (pvector-length insns))])
          (define insn (pvector-ref insns i))
          (define liveness (pvector-ref block-liveness i))
          (define live-set (InsnLiveness-live-after liveness))

          ;; For each defined register, add edge to all other live registers
          (define defs (insn-defs insn))
          (for ([d defs])
            (when (vreg? d)
              (for ([l (in-set live-set)])
                (when (and (vreg? l)
                           (not (equal? (vreg-id d) (vreg-id l)))
                           (eq? (vreg-class d) (vreg-class l)))
                  (igraph-add-edge! graph (vreg-id d) (vreg-id l))))))))))

  graph)

;; Collect all virtual registers in CFG
(define (collect-vregs cfg)
  (define vregs (mutable-set))

  (for ([bid (in-cfg-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (for ([insn (in-pvector (AsmBlock-insns block))])
        (for ([r (append (insn-defs insn) (insn-uses insn))])
          (when (vreg? r)
            (set-add! vregs r))))))

  (set->list vregs))

;; ============================================================================
;; Graph Coloring
;; ============================================================================

;; Color the interference graph
;; Returns: (Hash[VRegId -> Color], Set[VRegId to spill])
(define (color-graph graph vreg-info num-colors)
  ;; Simplify: repeatedly remove nodes with degree < num-colors
  (define stack '())
  (define removed (mutable-set))

  ;; Simplify phase
  (let simplify ()
    (define vertices (filter (lambda (v) (not (set-member? removed v)))
                             (igraph-vertices graph)))
    (define found #f)

    (for ([v vertices] #:break found)
      (define neighbors (set-subtract (igraph-neighbors graph v) removed))
      (define degree (set-count neighbors))
      (when (< degree num-colors)
        (set! stack (cons v stack))
        (set-add! removed v)
        (set! found #t)))

    (when found
      (simplify)))

  ;; Check for potential spills (remaining high-degree nodes)
  (define remaining
    (filter (lambda (v) (not (set-member? removed v)))
            (igraph-vertices graph)))

  (define to-spill (mutable-set))

  ;; If nodes remain, choose one to spill (highest degree heuristic)
  (when (not (null? remaining))
    (define highest-degree-node
      (argmax (lambda (v)
                (set-count (set-subtract (igraph-neighbors graph v) removed)))
              remaining))
    (set-add! to-spill highest-degree-node)
    ;; Add remaining to stack for coloring attempt
    (set! stack (append remaining stack)))

  ;; Select phase: assign colors
  (define coloring (make-hash))

  (for ([v (reverse stack)])
    (unless (set-member? to-spill v)
      ;; Find used colors by neighbors
      (define neighbor-colors
        (for/set ([n (in-set (igraph-neighbors graph v))])
          (hash-ref coloring n #f)))

      ;; Choose first available color
      (define available-colors
        (for/list ([c (in-range num-colors)]
                   #:when (not (set-member? neighbor-colors c)))
          c))

      (if (null? available-colors)
          (set-add! to-spill v)
          (hash-set! coloring v (car available-colors)))))

  (values coloring (for/set ([s to-spill]) s)))

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
  ;; Step 1: Compute liveness
  (define liveness (compute-liveness cfg))

  ;; Step 2: Collect virtual registers by class
  (define all-vregs (collect-vregs cfg))
  (define vreg-by-id (make-hash))
  (for ([v all-vregs])
    (hash-set! vreg-by-id (vreg-id v) v))

  ;; Separate by class
  (define gpr-vregs (filter (lambda (v) (eq? (vreg-class v) 'gpr)) all-vregs))
  (define sve-vregs (filter (lambda (v) (eq? (vreg-class v) 'sve)) all-vregs))
  (define pred-vregs (filter (lambda (v) (eq? (vreg-class v) 'pred)) all-vregs))

  ;; Step 3: Build interference graphs per class
  (define gpr-graph (build-class-interference-graph cfg liveness 'gpr gpr-vregs))
  (define sve-graph (build-class-interference-graph cfg liveness 'sve sve-vregs))
  (define pred-graph (build-class-interference-graph cfg liveness 'pred pred-vregs))

  ;; Step 4: Color each graph
  (define-values (gpr-coloring gpr-spills)
    (color-graph gpr-graph vreg-by-id (length gpr-allocatable)))

  (define-values (sve-coloring sve-spills)
    (color-graph sve-graph vreg-by-id (length sve-allocatable)))

  (define-values (pred-coloring pred-spills)
    (color-graph pred-graph vreg-by-id (length pred-allocatable)))

  ;; Step 5: Map colors to physical registers
  (define assignment (make-hash))
  (define callee-saved-used (mutable-set))

  ;; Assign GPRs
  (for ([(vid color) (in-hash gpr-coloring)])
    (define phys-reg (list-ref gpr-allocatable color))
    (hash-set! assignment vid phys-reg)
    (when (member phys-reg gpr-callee-saved)
      (set-add! callee-saved-used phys-reg)))

  ;; Assign SVE registers
  (for ([(vid color) (in-hash sve-coloring)])
    (define phys-reg (list-ref sve-allocatable color))
    (hash-set! assignment vid phys-reg))

  ;; Assign predicate registers
  (for ([(vid color) (in-hash pred-coloring)])
    (define phys-reg (list-ref pred-allocatable color))
    (hash-set! assignment vid phys-reg))

  ;; Combine spills
  (define all-spills (set-union gpr-spills sve-spills pred-spills))

  (AllocationResult
   assignment
   all-spills
   (for/set ([r callee-saved-used]) r)
   (set-empty? all-spills)))

;; Build interference graph for a specific register class
(define (build-class-interference-graph cfg liveness class vregs)
  (define graph (make-empty-igraph))

  ;; Add vertices
  (for ([v vregs])
    (igraph-add-vertex! graph (vreg-id v)))

  ;; Add edges based on liveness
  (for ([bid (in-cfg-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (define block-liveness (hash-ref (LivenessInfo-block-info liveness) bid #f))
      (when block-liveness
        (for ([i (in-range (pvector-length block-liveness))])
          (define liveness (pvector-ref block-liveness i))
          (define live-set (InsnLiveness-live-after liveness))
          (define class-live
            (for/list ([r (in-set live-set)]
                       #:when (and (vreg? r) (eq? (vreg-class r) class)))
              (vreg-id r)))

          ;; Add edges between all pairs of live registers
          (for* ([v1 class-live]
                 [v2 class-live]
                 #:when (not (equal? v1 v2)))
            (igraph-add-edge! graph v1 v2))))))

  graph)

;; ============================================================================
;; Apply Allocation to CFG
;; ============================================================================

(define (apply-allocation cfg allocation)
  (define assignment (AllocationResult-assignment allocation))

  ;; Transform each block
  (define new-blocks
    (for/fold ([blocks (ordered-map-empty block-id-compare)])
              ([bid (in-cfg-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (define new-insns
        (for/pvector ([insn (in-pvector (AsmBlock-insns block))])
          (rewrite-insn insn assignment)))
      (ordered-map-set blocks bid
                       (struct-copy AsmBlock block [insns new-insns]))))

  (struct-copy AsmCfg cfg [blocks new-blocks]))

;; Rewrite instruction with physical registers
(define (rewrite-insn insn assignment)
  (define (rewrite-reg r)
    (if (vreg? r)
        (hash-ref assignment (vreg-id r) r)
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

(provide apply-allocation rewrite-insn)
