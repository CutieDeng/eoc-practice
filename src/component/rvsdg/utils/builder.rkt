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
  region-add-wire
  region-add-region-arg
  region-add-region-result
  build-handler-region-entry
  install-kappa-terminal)

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

;; ============================================================
;; Sub-region entry / exit conveniences
;; ============================================================
;;
;; RVSDG sub-regions that participate in a structured node (Gamma,
;; Theta, Kappa, ...) conventionally begin with a synthetic
;; `Simple '(region-arg N)` producer that mirrors the parent node's
;; ctx inputs, and (when the sub-region falls through) end with a
;; `Simple '(region-result M)` consumer that mirrors the parent
;; node's outputs.  These two helpers centralise that allocation so
;; callers don't reinvent it.

;; Allocate a `Simple '(region-arg n)` producer at the start of a
;; fresh sub-region.  Returns (values region arg-output-ids).
(define (region-add-region-arg r n)
  (define-values (r* _nid _ins outs)
    (region-add-node r (Simple (list 'region-arg n)) 0 n))
  (values r* outs))

;; Allocate a `Simple '(region-result m)` consumer at the end of a
;; sub-region.  Returns (values region result-input-ids).
(define (region-add-region-result r m)
  (define-values (r* _nid ins _outs)
    (region-add-node r (Simple (list 'region-result m)) m 0))
  (values r* ins))

;; Build a fresh handler-region for a Kappa.  The handler sees all
;; ctx vars from the enclosing parent plus the exception-ref as an
;; additional region-arg output; the caller's body-construction code
;; then extends the returned region before handing it to
;; install-kappa-terminal.
;;
;; Returns (values handler-region ctx-arg-oids exn-arg-oid) where
;;   ctx-arg-oids : pvector[OutputId] of length n-ctx (order matches
;;                  the parent Kappa node's ctx input order)
;;   exn-arg-oid  : OutputId - the (N+1)-th region-arg output, i.e.
;;                  the exception-ref available to the handler body.
(define (build-handler-region-entry n-ctx)
  (define r0 (region-empty))
  (define-values (r1 all-outs) (region-add-region-arg r0 (add1 n-ctx)))
  (define ctx-outs
    (for/pvector ([o (in-pvector all-outs)] [i (in-naturals)]
                  #:when (< i n-ctx))
      o))
  (define exn-out (pvector-ref all-outs n-ctx))
  (values r1 ctx-outs exn-out))

;; ============================================================
;; Kappa installation (terminal shape: 0 outputs)
;; ============================================================
;;
;; Install a Kappa node in `parent` with the given ctx inputs.  The
;; try-region and each handler-region must have already been built
;; and terminated internally (ret / throw sink) — this helper owns
;; only the parent-side wiring and the node allocation.
;;
;; Arguments:
;;   parent      : Region - the enclosing region
;;   ctx-oids    : pvector[OutputId] - N producer outputs in `parent`
;;                 that supply each sub-region's region-arg values
;;   try-region  : Region - pre-built try body (has a region-arg
;;                 producer matching `(length ctx-oids)`)
;;   handlers    : pvector[(cons catch-type Region)] - first-wins
;;                 ordered handler list.  Each handler Region's
;;                 region-arg producer has `(+ n-ctx 1)` outputs.
;;
;; Returns (values new-parent kappa-nid).
(define (install-kappa-terminal parent ctx-oids try-region handlers)
  (define n-ctx (pvector-length ctx-oids))
  (define kappa-val (Kappa try-region handlers))
  (define-values (parent1 knid k-ins _outs)
    (region-add-node parent kappa-val n-ctx 0))
  (define parent2
    (for/fold ([r parent1])
              ([src (in-pvector ctx-oids)]
               [dst (in-pvector k-ins)])
      (define-values (r* _w) (region-add-wire r src dst))
      r*))
  (values parent2 knid))
