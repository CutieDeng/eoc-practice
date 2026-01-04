#lang racket/base

;; ============================================================
;; Driver: Generic Dataflow Analysis Framework
;; ============================================================
;;
;; Parameterized dataflow analysis that works with any IR.
;; All graph operations are passed as parameters.
;; Uses pvector and ordered-map for all data structures.
;;
;; ============================================================

(require "../../kernel/data/data.rkt")

(provide
  ;; Core dataflow framework
  dataflow-forward
  dataflow-backward

  ;; Iterative solver
  dataflow-iterate

  ;; Lattice operations
  make-lattice
  lattice-bottom
  lattice-top
  lattice-join
  lattice-meet
  lattice-leq?

  ;; Common lattices
  make-set-lattice
  bitvector-lattice)

;; ============================================================
;; Lattice Structure
;; ============================================================

;; A lattice provides:
;;   - bottom : value
;;   - top    : value (optional, can be #f)
;;   - join   : value value -> value
;;   - meet   : value value -> value (optional, can be #f)
;;   - leq?   : value value -> boolean
;;
(struct Lattice (bottom top join meet leq?) #:prefab)

(define (make-lattice #:bottom bottom
                      #:join join
                      #:leq? leq?
                      #:top [top #f]
                      #:meet [meet #f])
  (Lattice bottom top join meet leq?))

(define (lattice-bottom lat) (Lattice-bottom lat))
(define (lattice-top lat) (Lattice-top lat))
(define (lattice-join lat a b) ((Lattice-join lat) a b))
(define (lattice-meet lat a b)
  (define meet-fn (Lattice-meet lat))
  (if meet-fn (meet-fn a b) (error 'lattice-meet "meet not defined")))
(define (lattice-leq? lat a b) ((Lattice-leq? lat) a b))

;; ============================================================
;; Common Lattices
;; ============================================================

;; Set lattice using ordered-map (as set)
;; Bottom: empty map, Join: union, Meet: intersection
;; Takes a comparator for set elements
;;
(define (make-set-lattice element-compare)
  (define empty-set (ordered-map-empty element-compare))

  (define (set-union a b)
    (for/fold ([result a])
              ([k (ordered-map-keys b)])
      (ordered-map-set result k #t)))

  (define (set-intersect a b)
    (for/fold ([result (ordered-map-empty element-compare)])
              ([k (ordered-map-keys a)])
      (if (ordered-map-has-key? b k)
          (ordered-map-set result k #t)
          result)))

  (define (subset? a b)
    (for/and ([k (ordered-map-keys a)])
      (ordered-map-has-key? b k)))

  (make-lattice
    #:bottom empty-set
    #:join set-union
    #:meet set-intersect
    #:leq? subset?))

;; Bitvector lattice
;; Bottom: 0, Join: bitwise-or, Meet: bitwise-and
;;
(define (bitvector-lattice #:size size)
  (define top (sub1 (expt 2 size)))
  (make-lattice
    #:bottom 0
    #:top top
    #:join bitwise-ior
    #:meet bitwise-and
    #:leq? (lambda (a b) (= (bitwise-and a b) a))))

;; ============================================================
;; Forward Dataflow Analysis
;; ============================================================

;; Forward dataflow analysis
;; Parameters:
;;   node-compare     : comparator for nodes
;;   nodes            : pvector of nodes - all nodes in RPO
;;   get-predecessors : node -> pvector of nodes
;;   lattice          : Lattice
;;   transfer         : node value -> value  (transfer function)
;;   init             : ordered-map (optional initial values)
;; Returns: ordered-map node -> value (IN values for each node)
;;
(define (dataflow-forward node-compare nodes get-predecessors lattice transfer
                          #:init [init (ordered-map-empty node-compare)])
  (define bottom (lattice-bottom lattice))

  ;; Initialize IN values
  (define in-values
    (for/fold ([m (ordered-map-empty node-compare)])
              ([node (in-pvector nodes)])
      (ordered-map-set m node (ordered-map-ref init node bottom))))

  ;; Compute OUT values
  (define (compute-out node in-vals)
    (transfer node (ordered-map-ref in-vals node bottom)))

  ;; Iterate until fixed point
  (define (iterate in-vals)
    (define-values (new-in-vals changed)
      (for/fold ([vals in-vals] [ch #f])
                ([node (in-pvector nodes)])
        (define preds (get-predecessors node))
        (define new-in
          (if (pvector-empty? preds)
              (ordered-map-ref vals node bottom)  ; Keep initial value for entry
              (for/fold ([acc bottom])
                        ([pred (in-pvector preds)])
                (lattice-join lattice acc (compute-out pred vals)))))
        (if (equal? new-in (ordered-map-ref vals node bottom))
            (values vals ch)
            (values (ordered-map-set vals node new-in) #t))))
    (values new-in-vals changed))

  ;; Run until fixed point
  (let loop ([vals in-values])
    (define-values (new-vals changed) (iterate vals))
    (if changed
        (loop new-vals)
        new-vals)))

;; ============================================================
;; Backward Dataflow Analysis
;; ============================================================

;; Backward dataflow analysis
;; Parameters:
;;   node-compare   : comparator for nodes
;;   nodes          : pvector of nodes - all nodes in RPO
;;   get-successors : node -> pvector of nodes
;;   lattice        : Lattice
;;   transfer       : node value -> value
;;   init           : ordered-map (optional initial values)
;; Returns: ordered-map node -> value (OUT values for each node)
;;
(define (dataflow-backward node-compare nodes get-successors lattice transfer
                           #:init [init (ordered-map-empty node-compare)])
  (define bottom (lattice-bottom lattice))
  (define rpo-nodes (pvector-reverse nodes))  ; Process in reverse order

  ;; Initialize OUT values
  (define out-values
    (for/fold ([m (ordered-map-empty node-compare)])
              ([node (in-pvector nodes)])
      (ordered-map-set m node (ordered-map-ref init node bottom))))

  ;; Compute IN values
  (define (compute-in node out-vals)
    (transfer node (ordered-map-ref out-vals node bottom)))

  ;; Iterate until fixed point
  (define (iterate out-vals)
    (define-values (new-out-vals changed)
      (for/fold ([vals out-vals] [ch #f])
                ([node (in-pvector rpo-nodes)])
        (define succs (get-successors node))
        (define new-out
          (if (pvector-empty? succs)
              (ordered-map-ref vals node bottom)  ; Keep initial value for exit
              (for/fold ([acc bottom])
                        ([succ (in-pvector succs)])
                (lattice-join lattice acc (compute-in succ vals)))))
        (if (equal? new-out (ordered-map-ref vals node bottom))
            (values vals ch)
            (values (ordered-map-set vals node new-out) #t))))
    (values new-out-vals changed))

  ;; Run until fixed point
  (let loop ([vals out-values])
    (define-values (new-vals changed) (iterate vals))
    (if changed
        (loop new-vals)
        new-vals)))

;; ============================================================
;; Generic Iterative Solver
;; ============================================================

;; Generic worklist-based iterative solver
;; Parameters:
;;   node-compare   : comparator for nodes
;;   nodes          : pvector of nodes - initial worklist
;;   get-dependents : node -> pvector of nodes - nodes to update when node changes
;;   update         : node state -> (values new-state changed?)
;;   init-state     : initial state
;; Returns: final state
;;
(define (dataflow-iterate node-compare nodes get-dependents update init-state)
  ;; Use ordered-map as a set for the worklist
  (define (pvector->set pv)
    (for/fold ([s (ordered-map-empty node-compare)])
              ([x (in-pvector pv)])
      (ordered-map-set s x #t)))

  (let loop ([worklist (pvector->set nodes)]
             [state init-state])
    (if (ordered-map-empty? worklist)
        state
        ;; Get and remove first element from worklist
        (let* ([node (car (ordered-map-min worklist))]
               [worklist* (let-values ([(m _) (ordered-map-delete worklist node)]) m)])
          (define-values (new-state changed?) (update node state))
          (if changed?
              ;; Add dependents to worklist
              (let ([deps (get-dependents node)])
                (loop (for/fold ([w worklist*])
                                ([dep (in-pvector deps)])
                        (ordered-map-set w dep #t))
                      new-state))
              (loop worklist* new-state))))))
