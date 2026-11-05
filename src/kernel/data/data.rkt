#lang racket/base

;; ============================================================
;; Kernel Data Structures
;; ============================================================
;;
;; Single entry point for the persistent data structures used by
;; every IR.  All of them come from the Racket standard library:
;;
;;   - racket/pvector : persistent vector (pvector, in-pvector,
;;                      for/pvector, pvector-map, pvector-filter, ...)
;;   - racket/graph   : persistent multigraph with opaque vertex-id /
;;                      edge-id (only the Graph-type API is re-exported;
;;                      the generic traversal algorithms are NOT, to
;;                      avoid name clashes with driver/graph/*)
;;   - racket/intmap  : persistent integer-keyed map (typed-id keyed
;;                      IR tables store the id value as key)
;;   - racket/intbits : integer bit sets (replaces the former bitset:
;;                      intbits-empty / intbits-set / intbits-ref / ...)
;;   - racket/compare : three-way comparators integer-compare,
;;                      symbol-compare, string-compare ('< '= '>)
;;
;; Convention: every pvector operation takes the pvector FIRST
;; (standard-library order), e.g. (pvector-map pv f), (pvector-filter
;; pv pred), (pvector-fold-left pv init (lambda (acc x) ...)).
;; The helpers defined below follow the same order.
;;
;; idmap.rkt adds idmap-*: tables keyed by typed ids (BlockId, VarId,
;; NodeId, ...) stored as an intmap of the id payload.
;; ============================================================

(require racket/pvector
         racket/intbits
         racket/intmap
         (only-in racket/compare
                  integer-compare
                  symbol-compare
                  string-compare)
         (only-in racket/graph
                  graph
                  graph?
                  graph-empty
                  graph-add-vertex
                  graph-add-edge
                  graph-add-edge-pair
                  graph-remove-vertex
                  graph-remove-vertex*
                  graph-remove-edge
                  graph-remove-edge*
                  graph-remove-edge-between
                  graph-remove-edges-between
                  graph-vertex?
                  graph-edge?
                  graph-vertex-count
                  graph-edge-count
                  graph-vertices-set
                  graph-edges-set
                  graph-edges-between
                  graph-has-edge-to?
                  graph-edge-src
                  graph-edge-dst
                  graph-edge-endpoints
                  graph-edge-pair
                  graph-in-edges
                  graph-out-edges
                  graph-in-degree
                  graph-out-degree
                  graph-successors
                  graph-predecessors
                  in-graph-vertices
                  in-graph-edges
                  in-graph-out-edges
                  in-graph-successors
                  in-graph-predecessors
                  vertex-id?
                  vertex-id-val
                  vertex-id-compare
                  edge-id?
                  edge-id-val
                  edge-id-compare)
         "idmap.rkt")

(provide (all-from-out racket/pvector)
         (all-from-out racket/intbits)
         (all-from-out racket/intmap)
         (all-from-out racket/compare)
         (all-from-out racket/graph)
         (all-from-out "idmap.rkt"))

;; ============================================================
;; pvector helpers with no standard-library equivalent
;; ============================================================

;; Keep elements for which pred is false.
(define (pvector-filter-not pv pred)
  (pvector-filter pv (lambda (x) (not (pred x)))))

;; First element satisfying pred, or #f.
(define (pvector-findf pv pred)
  (for/first ([x (in-pvector pv)] #:when (pred x))
    x))

;; Index of the first element satisfying pred, or #f.
(define (pvector-index-where pv pred)
  (for/first ([x (in-pvector pv)] [i (in-naturals)] #:when (pred x))
    i))

;; Index of the first element equal to v, or #f.
(define (pvector-index-of pv v [is-equal? equal?])
  (for/first ([x (in-pvector pv)] [i (in-naturals)] #:when (is-equal? x v))
    i))

;; Drop EVERY element equal to v (unlike racket's `remove`).
(define (pvector-remove-all pv v [is-equal? equal?])
  (pvector-filter pv (lambda (x) (not (is-equal? x v)))))

;; Concatenate any number of pvectors (pvector-append is binary).
(define (pvector-concat* . pvs)
  (for/fold ([acc (pvector-empty)]) ([pv (in-list pvs)])
    (pvector-append acc pv)))

(provide pvector-filter-not
         pvector-findf
         pvector-index-where
         pvector-index-of
         pvector-remove-all
         pvector-concat*)
