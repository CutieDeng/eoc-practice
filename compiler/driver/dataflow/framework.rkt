#lang racket/base

;; ============================================================
;; Driver: Generic Dataflow Analysis Framework
;; ============================================================
;;
;; Parameterized dataflow analysis that works with any IR.
;; All graph operations are passed as parameters.
;;
;; ============================================================

(require racket/list)
(require racket/set)
(require "../../kernel/data/main.rkt")

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
  set-lattice
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

;; Set lattice (powerset lattice)
;; Bottom: empty set, Join: union, Meet: intersection
;;
(define (set-lattice)
  (make-lattice
    #:bottom (set)
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
;;   nodes            : (listof node) - all nodes in RPO
;;   get-predecessors : node -> (listof node)
;;   lattice          : Lattice
;;   transfer         : node value -> value  (transfer function)
;;   init             : hash (optional initial values)
;; Returns: hash node -> value (IN values for each node)
;;
(define (dataflow-forward nodes get-predecessors lattice transfer
                          #:init [init (make-hash)])
  (define bottom (lattice-bottom lattice))

  ;; Initialize IN values
  (define in-values (make-hash))
  (for ([node nodes])
    (hash-set! in-values node
      (hash-ref init node bottom)))

  ;; Compute OUT values
  (define (compute-out node)
    (transfer node (hash-ref in-values node)))

  ;; Iterate until fixed point
  (define (iterate)
    (define changed #f)
    (for ([node nodes])
      (define preds (get-predecessors node))
      (define new-in
        (if (null? preds)
            (hash-ref in-values node)  ; Keep initial value for entry
            (for/fold ([acc bottom]) ([pred preds])
              (lattice-join lattice acc (compute-out pred)))))
      (unless (equal? new-in (hash-ref in-values node))
        (hash-set! in-values node new-in)
        (set! changed #t)))
    changed)

  ;; Run until fixed point
  (let loop ()
    (when (iterate)
      (loop)))

  in-values)

;; ============================================================
;; Backward Dataflow Analysis
;; ============================================================

;; Backward dataflow analysis
;; Parameters:
;;   nodes          : (listof node) - all nodes in RPO
;;   get-successors : node -> (listof node)
;;   lattice        : Lattice
;;   transfer       : node value -> value
;;   init           : hash (optional initial values)
;; Returns: hash node -> value (OUT values for each node)
;;
(define (dataflow-backward nodes get-successors lattice transfer
                           #:init [init (make-hash)])
  (define bottom (lattice-bottom lattice))
  (define rpo-nodes (reverse nodes))  ; Process in reverse order

  ;; Initialize OUT values
  (define out-values (make-hash))
  (for ([node nodes])
    (hash-set! out-values node
      (hash-ref init node bottom)))

  ;; Compute IN values
  (define (compute-in node)
    (transfer node (hash-ref out-values node)))

  ;; Iterate until fixed point
  (define (iterate)
    (define changed #f)
    (for ([node rpo-nodes])
      (define succs (get-successors node))
      (define new-out
        (if (null? succs)
            (hash-ref out-values node)  ; Keep initial value for exit
            (for/fold ([acc bottom]) ([succ succs])
              (lattice-join lattice acc (compute-in succ)))))
      (unless (equal? new-out (hash-ref out-values node))
        (hash-set! out-values node new-out)
        (set! changed #t)))
    changed)

  ;; Run until fixed point
  (let loop ()
    (when (iterate)
      (loop)))

  out-values)

;; ============================================================
;; Generic Iterative Solver
;; ============================================================

;; Generic worklist-based iterative solver
;; Parameters:
;;   nodes          : (listof node) - initial worklist
;;   get-dependents : node -> (listof node) - nodes to update when node changes
;;   update         : node state -> (values new-state changed?)
;;   init-state     : initial state
;; Returns: final state
;;
(define (dataflow-iterate nodes get-dependents update init-state)
  (define worklist (list->mutable-set nodes))
  (define state init-state)

  (let loop ()
    (unless (set-empty? worklist)
      (define node (set-first worklist))
      (set-remove! worklist node)

      (define-values (new-state changed?) (update node state))
      (set! state new-state)

      (when changed?
        (for ([dep (get-dependents node)])
          (set-add! worklist dep)))

      (loop)))

  state)

;; Helper: create mutable set from list
(define (list->mutable-set lst)
  (define s (mutable-set))
  (for ([x lst])
    (set-add! s x))
  s)
