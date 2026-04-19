#lang racket/base

;; ============================================================
;; Kernel IR: RVSDG Type Definitions
;; ============================================================
;;
;; Regionalized Value State Dependence Graph (RVSDG) types.
;; A functional graph-based IR for representing control flow,
;; data flow, and state dependencies.
;;
;; Design principles:
;; 1. Region is the core container with extensible info field
;; 2. Node types are pure data structures
;; 3. info field uses ordered-map for extensibility
;;
;; Recommended info keys:
;;   'types        -> ordered-map: NodeId/WireId -> Type
;;   'effects      -> ordered-map: NodeId -> EffectRow
;;   'source-map   -> ordered-map: NodeId -> SrcLoc
;;   'names        -> ordered-map: NodeId/WireId -> Symbol
;;   'tensor-shapes -> ordered-map: WireId -> Shape (for ML compilers)
;;
;; ============================================================

(provide
  ;; Identifiers
  (struct-out NodeId)
  (struct-out InputId)
  (struct-out OutputId)
  (struct-out WireId)

  ;; Region container
  (struct-out Region)

  ;; Basic nodes
  (struct-out Simple)
  (struct-out Gamma)
  (struct-out Theta)

  ;; Function nodes
  (struct-out Lambda)
  (struct-out Delta)
  (struct-out Phi)

  ;; Program root
  (struct-out Omega)

  ;; Effect system nodes
  (struct-out Psi)
  (struct-out Shift)
  (struct-out EffHandler)
  (struct-out EffPerform)

  ;; Exception nodes
  (struct-out Kappa)
  (struct-out Throw)

  ;; Predicates
  rvsdg-id?
  structured-node?
  simple-node?
  effect-node?)

;; ============================================================
;; Identifiers
;; ============================================================

;; Node identifier
(struct NodeId (id) #:prefab)

;; Input port identifier
(struct InputId (id) #:prefab)

;; Output port identifier
(struct OutputId (id) #:prefab)

;; Wire (edge) identifier
(struct WireId (id) #:prefab)

;; ============================================================
;; Region: Core Container
;; ============================================================

;; A Region is the fundamental container in RVSDG, representing
;; a lexical scope containing nodes, wires, and ports.
;;
;; Fields:
;;   info         : ordered-map - extensible metadata
;;
;;   wire->input  : ordered-map: WireId -> InputId - wire targets
;;   wire->output : ordered-map: WireId -> OutputId - wire sources
;;
;;   input->wire  : ordered-map: InputId -> WireId - reverse mapping
;;   input->node  : ordered-map: InputId -> NodeId - port ownership
;;   output->wire : ordered-map: OutputId -> WireId - reverse mapping
;;   output->node : ordered-map: OutputId -> NodeId - port ownership
;;
;;   node->input  : ordered-map: NodeId -> (cons InputId count) - node inputs
;;   node->output : ordered-map: NodeId -> (cons OutputId count) - node outputs
;;   node->value  : ordered-map: NodeId -> NodeValue - node content
;;
;;   wire-cnt     : Natural - ID allocator
;;   input-cnt    : Natural - ID allocator
;;   output-cnt   : Natural - ID allocator
;;   node-cnt     : Natural - ID allocator
;;
(struct Region (
  info

  wire->input
  wire->output

  input->wire
  input->node
  output->wire
  output->node

  node->input
  node->output
  node->value

  wire-cnt
  input-cnt
  output-cnt
  node-cnt
) #:prefab)

;; ============================================================
;; Basic Nodes
;; ============================================================

;; Simple node: primitive operation
;; op: symbol representing the operation
(struct Simple (op) #:prefab)

;; Gamma node: conditional branch
;; regions: list of Region, one per branch
(struct Gamma (regions) #:prefab)

;; Theta node: tail-controlled loop
;; region: Region containing loop body
(struct Theta (region) #:prefab)

;; ============================================================
;; Function Nodes
;; ============================================================

;; Lambda node: function definition
;; region: Region containing function body
(struct Lambda (region) #:prefab)

;; Delta node: global/mutable variable
;; region: Region containing initialization
(struct Delta (region) #:prefab)

;; Phi node: mutually recursive function group
;; regions: list of Region, one per function
(struct Phi (regions) #:prefab)

;; ============================================================
;; Program Root
;; ============================================================

;; Omega node: program entry point
;; region: Region containing top-level program
(struct Omega (region) #:prefab)

;; ============================================================
;; Effect System Nodes
;; ============================================================

;; Psi node: delimited continuation boundary (prompt/reset)
;; tag: effect tag identifier
;; region: Region within the boundary
(struct Psi (tag region) #:prefab)

;; Shift node: continuation capture (shift/control)
;; tag: effect tag to capture to
(struct Shift (tag) #:prefab)

;; Effect handler node
;; ops: list of handled operation names
;; handler-region: Region containing handlers
;; return-region: Region for return case
(struct EffHandler (ops handler-region return-region) #:prefab)

;; Effect perform node
;; op-name: symbol naming the effect operation
(struct EffPerform (op-name) #:prefab)

;; ============================================================
;; Exception Nodes
;; ============================================================

;; Kappa node: try-catch structure.
;;
;; Field shapes (contract):
;;   try-region : Region containing the try body.  A `Throw` node
;;                installed anywhere inside this region short-circuits
;;                to handler selection; the thrown value is made
;;                available to every handler-region as its extra
;;                region-arg output (the exception-ref).  If the try
;;                body can also fall through normally, its sink is a
;;                `Simple '(region-result M)` whose input shape must
;;                match every handler-region's region-result.
;;
;;   handlers   : pvector[(cons catch-type Region)]
;;     catch-type : String  - Java exception class name (e.g.
;;                            "java.lang.RuntimeException"), matched
;;                            first-wins in list order, OR
;;                  #f      - catch-all (finally-style), always last
;;
;; I/O convention for the Kappa node in its parent region:
;;   inputs  : N ctx values, threaded as `Simple '(region-arg N)`
;;             into both try-region and every handler-region.
;;             Handler-regions additionally receive the exception-ref
;;             as their (N+1)-th region-arg output.
;;   outputs : M values, uniform across try-region (if it falls
;;             through) and every handler-region.  A "terminal" Kappa
;;             is the initial supported shape: try-region and all
;;             handler-regions install their own ret / throw sink
;;             internally, so M = 0 and the node carries no outputs.
(struct Kappa (try-region handlers) #:prefab)

;; Throw node: exception raise.  Acts as a region sink (1 input,
;; 0 outputs) when installed inside a region.  The enclosing Kappa's
;; try-region binding determines whether the throw is caught (Kappa's
;; handlers take over) or propagates further (outer Lambda / Kappa).
;; exn-tag : Symbol or #f - optional static tag metadata.
(struct Throw (exn-tag) #:prefab)

;; ============================================================
;; Predicates
;; ============================================================

(define (rvsdg-id? x)
  (or (NodeId? x)
      (InputId? x)
      (OutputId? x)
      (WireId? x)))

(define (simple-node? x)
  (Simple? x))

(define (structured-node? x)
  (or (Gamma? x)
      (Theta? x)
      (Lambda? x)
      (Delta? x)
      (Phi? x)
      (Omega? x)
      (Psi? x)
      (EffHandler? x)
      (Kappa? x)))

(define (effect-node? x)
  (or (Psi? x)
      (Shift? x)
      (EffHandler? x)
      (EffPerform? x)))
