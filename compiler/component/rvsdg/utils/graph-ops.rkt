#lang racket/base

;; ============================================================
;; Component: RVSDG Graph Operations
;; ============================================================
;;
;; Provides graph operation functions for RVSDG regions that can
;; be passed to parameterized driver algorithms.
;;
;; This module bridges the kernel RVSDG types with the driver layer
;; by providing the function signatures that driver algorithms expect.
;; ============================================================

(require racket/match racket/list racket/set)
(require "../../../kernel/ir/rvsdg/main.rkt")
(require "../../../kernel/data/main.rkt")

(provide
  ;; Graph operation closures
  region-make-successors
  region-make-predecessors

  ;; Region accessors
  region-all-node-ids
  region-get-node-value
  region-get-node-inputs
  region-get-node-outputs

  ;; Wire/port operations
  region-get-wire-source
  region-get-wire-target

  ;; Node classification
  region-get-structured-nodes
  region-get-simple-nodes)

;; ============================================================
;; Graph Operations (Closures for Driver)
;; ============================================================

;; Create a get-successors function for RVSDG data dependency
;; Successors are nodes that consume the outputs of a node
;; Returns: NodeId -> (Listof NodeId)
;;
(define (region-make-successors region)
  (lambda (node-id)
    (define outputs (region-get-node-outputs region node-id))
    (define result (mutable-set))
    ;; For each output, find which nodes consume the wires
    (for ([out-id outputs])
      (define wire-id (ordered-map-ref (Region-output->wire region) out-id #f))
      (when wire-id
        ;; Find nodes that use this wire via their inputs
        (define input-id (ordered-map-ref (Region-wire->input region) wire-id #f))
        (when input-id
          (define consumer-node (ordered-map-ref (Region-input->node region) input-id #f))
          (when consumer-node
            (set-add! result consumer-node)))))
    (set->list result)))

;; Create a get-predecessors function for RVSDG data dependency
;; Predecessors are nodes that produce the inputs of a node
;; Returns: NodeId -> (Listof NodeId)
;;
(define (region-make-predecessors region)
  (lambda (node-id)
    (define inputs (region-get-node-inputs region node-id))
    (define result (mutable-set))
    ;; For each input, find which nodes produce the wires
    (for ([in-id inputs])
      (define wire-id (ordered-map-ref (Region-input->wire region) in-id #f))
      (when wire-id
        ;; Find nodes that produce this wire via their outputs
        (define output-id (ordered-map-ref (Region-wire->output region) wire-id #f))
        (when output-id
          (define producer-node (ordered-map-ref (Region-output->node region) output-id #f))
          (when producer-node
            (set-add! result producer-node)))))
    (set->list result)))

;; ============================================================
;; Region Accessors
;; ============================================================

;; Get all node IDs in a region
(define (region-all-node-ids region)
  (ordered-map-keys (Region-node->value region)))

;; Get the value (type) of a node
(define (region-get-node-value region node-id)
  (ordered-map-ref (Region-node->value region) node-id #f))

;; Get input IDs for a node (returns list of InputId)
(define (region-get-node-inputs region node-id)
  (define entry (ordered-map-ref (Region-node->input region) node-id #f))
  (if entry
      (let ([start-id (car entry)]
            [count (cdr entry)])
        (for/list ([i (in-range count)])
          (InputId (+ (InputId-id start-id) i))))
      '()))

;; Get output IDs for a node (returns list of OutputId)
(define (region-get-node-outputs region node-id)
  (define entry (ordered-map-ref (Region-node->output region) node-id #f))
  (if entry
      (let ([start-id (car entry)]
            [count (cdr entry)])
        (for/list ([i (in-range count)])
          (OutputId (+ (OutputId-id start-id) i))))
      '()))

;; ============================================================
;; Wire/Port Operations
;; ============================================================

;; Get the source (OutputId) of a wire
(define (region-get-wire-source region wire-id)
  (ordered-map-ref (Region-wire->output region) wire-id #f))

;; Get the target (InputId) of a wire
(define (region-get-wire-target region wire-id)
  (ordered-map-ref (Region-wire->input region) wire-id #f))

;; ============================================================
;; Node Classification
;; ============================================================

;; Get all structured nodes in a region
(define (region-get-structured-nodes region)
  (for/list ([nid (region-all-node-ids region)]
             #:when (structured-node? (region-get-node-value region nid)))
    nid))

;; Get all simple nodes in a region
(define (region-get-simple-nodes region)
  (for/list ([nid (region-all-node-ids region)]
             #:when (simple-node? (region-get-node-value region nid)))
    nid))
