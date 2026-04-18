#lang racket/base

;; ============================================================
;; Component: RVSDG Region Builder
;; ============================================================
;;
;; Low-level helpers for incrementally constructing a `Region`.
;; Each operation returns the updated region plus whatever fresh
;; IDs were allocated.  The Region invariants we maintain:
;;
;;   - A node's inputs are consecutive InputIds; (start-id . count)
;;     is stored in node->input.  Same for outputs / node->output.
;;   - Every InputId/OutputId has a back-pointer to its owning NodeId
;;     via input->node / output->node.
;;   - A wire is a single producer→single consumer edge.  Both the
;;     forward maps (wire->input, wire->output) and the reverse maps
;;     (input->wire, output->wire) are kept in sync.
;;
;; These are pure functional operations over the prefab Region.
;; ============================================================

(require (except-in "../../../kernel/data/data.rkt" integer-compare)
         "../../../kernel/ir/rvsdg/rvsdg.rkt")

(provide
  region-empty
  region-add-node
  region-add-wire)

;; ============================================================
;; Identifier comparators
;; ============================================================

(define (id-val-compare a b id-val)
  (define ai (id-val a))
  (define bi (id-val b))
  (cond
    [(< ai bi) '<]
    [(> ai bi) '>]
    [else '=]))

(define (node-id-compare a b)   (id-val-compare a b NodeId-id))
(define (input-id-compare a b)  (id-val-compare a b InputId-id))
(define (output-id-compare a b) (id-val-compare a b OutputId-id))
(define (wire-id-compare a b)   (id-val-compare a b WireId-id))

;; ============================================================
;; Empty region
;; ============================================================

(define (region-empty [info-cmp symbol-compare])
  (Region
    (ordered-map-empty info-cmp)        ; info
    (ordered-map-empty wire-id-compare)  ; wire->input
    (ordered-map-empty wire-id-compare)  ; wire->output
    (ordered-map-empty input-id-compare) ; input->wire
    (ordered-map-empty input-id-compare) ; input->node
    (ordered-map-empty output-id-compare); output->wire
    (ordered-map-empty output-id-compare); output->node
    (ordered-map-empty node-id-compare)  ; node->input
    (ordered-map-empty node-id-compare)  ; node->output
    (ordered-map-empty node-id-compare)  ; node->value
    0 0 0 0))                            ; wire/input/output/node counts

;; ============================================================
;; Node allocation
;; ============================================================

;; Allocate a new node with `n-in` inputs and `n-out` outputs.
;; Returns (values new-region node-id input-ids output-ids)
;; where input-ids / output-ids are pvectors of fresh InputIds /
;; OutputIds in allocation order.
(define (region-add-node r value n-in n-out)
  (define nid (NodeId (Region-node-cnt r)))
  (define input-start (Region-input-cnt r))
  (define output-start (Region-output-cnt r))

  (define in-ids
    (for/pvector ([i (in-range n-in)]) (InputId (+ input-start i))))
  (define out-ids
    (for/pvector ([i (in-range n-out)]) (OutputId (+ output-start i))))

  ;; Register ownership of each port back to this node.
  (define input->node*
    (for/fold ([m (Region-input->node r)]) ([iid (in-pvector in-ids)])
      (ordered-map-set m iid nid)))
  (define output->node*
    (for/fold ([m (Region-output->node r)]) ([oid (in-pvector out-ids)])
      (ordered-map-set m oid nid)))

  (define node->input*
    (ordered-map-set (Region-node->input r) nid (cons (InputId input-start) n-in)))
  (define node->output*
    (ordered-map-set (Region-node->output r) nid (cons (OutputId output-start) n-out)))
  (define node->value*
    (ordered-map-set (Region-node->value r) nid value))

  (values
    (struct-copy Region r
      [input->node input->node*]
      [output->node output->node*]
      [node->input node->input*]
      [node->output node->output*]
      [node->value node->value*]
      [input-cnt (+ input-start n-in)]
      [output-cnt (+ output-start n-out)]
      [node-cnt (add1 (Region-node-cnt r))])
    nid
    in-ids
    out-ids))

;; ============================================================
;; Wire allocation
;; ============================================================

;; Create a wire connecting OutputId → InputId.
;; Returns (values new-region wire-id).
(define (region-add-wire r src-out dst-in)
  (define wid (WireId (Region-wire-cnt r)))
  (values
    (struct-copy Region r
      [wire->output (ordered-map-set (Region-wire->output r) wid src-out)]
      [wire->input  (ordered-map-set (Region-wire->input r) wid dst-in)]
      [output->wire (ordered-map-set (Region-output->wire r) src-out wid)]
      [input->wire  (ordered-map-set (Region-input->wire r) dst-in wid)]
      [wire-cnt (add1 (Region-wire-cnt r))])
    wid))
