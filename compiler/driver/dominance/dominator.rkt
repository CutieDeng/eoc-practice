#lang racket/base

;; ============================================================
;; Driver: Dominator Algorithms
;; ============================================================
;;
;; Parameterized dominator tree algorithms.
;; These do NOT depend on any specific graph representation.
;; Uses pvector and ordered-map for all data structures.
;;
;; ============================================================

(require "../../kernel/data/main.rkt")
(require "../graph/traversal.rkt")

(provide
  ;; Dominator computation
  compute-dominators
  compute-idom
  compute-dominator-tree

  ;; Dominator frontier
  compute-dominance-frontier

  ;; Post-dominator variants
  compute-post-dominators
  compute-post-idom

  ;; Utilities
  dominates?
  strictly-dominates?)

;; ============================================================
;; Set operations using ordered-map
;; ============================================================

(define (empty-set compare) (ordered-map-empty compare))
(define (set-singleton compare x) (ordered-map-set (empty-set compare) x #t))
(define (set-contains? s x) (ordered-map-has-key? s x))
(define (set-add s x) (ordered-map-set s x #t))

(define (set-intersect compare a b)
  (for/fold ([result (empty-set compare)])
            ([k (ordered-map-keys a)])
    (if (ordered-map-has-key? b k)
        (ordered-map-set result k #t)
        result)))

;; ============================================================
;; Dominator Computation (Cooper-Harvey-Kennedy Algorithm)
;; ============================================================

;; Compute dominators using iterative algorithm
;; Parameters:
;;   node-compare     : comparator for nodes
;;   nodes            : pvector of nodes - all nodes in RPO
;;   entry            : node - entry node
;;   get-predecessors : node -> pvector of nodes
;; Returns: ordered-map node -> ordered-map (as set) - dominators for each node
;;
(define (compute-dominators node-compare nodes entry get-predecessors)
  ;; All nodes as a set
  (define all-nodes-set
    (for/fold ([s (empty-set node-compare)])
              ([n (in-pvector nodes)])
      (set-add s n)))

  ;; Initialize: dom[entry] = {entry}, dom[n] = all nodes
  (define initial-dom
    (for/fold ([m (ordered-map-empty node-compare)])
              ([node (in-pvector nodes)])
      (ordered-map-set m node
        (if (equal? node entry)
            (set-singleton node-compare entry)
            all-nodes-set))))

  ;; Iterate until fixed point
  (define (iterate dom)
    (define-values (new-dom changed)
      (for/fold ([d dom] [ch #f])
                ([node (in-pvector nodes)])
        (if (equal? node entry)
            (values d ch)
            (let ()
              (define preds (get-predecessors node))
              (define new-dom-set
                (if (pvector-empty? preds)
                    (set-singleton node-compare node)
                    (set-add
                      (for/fold ([acc all-nodes-set])
                                ([pred (in-pvector preds)])
                        (set-intersect node-compare acc (ordered-map-ref d pred (empty-set node-compare))))
                      node)))
              (if (equal? (ordered-map-keys new-dom-set)
                          (ordered-map-keys (ordered-map-ref d node (empty-set node-compare))))
                  (values d ch)
                  (values (ordered-map-set d node new-dom-set) #t))))))
    (values new-dom changed))

  (let loop ([dom initial-dom])
    (define-values (new-dom changed) (iterate dom))
    (if changed
        (loop new-dom)
        new-dom)))

;; ============================================================
;; Immediate Dominator (Cooper-Harvey-Kennedy)
;; ============================================================

;; Compute immediate dominators
;; Parameters:
;;   node-compare     : comparator for nodes
;;   nodes            : pvector of nodes - all nodes in RPO
;;   entry            : node - entry node
;;   get-predecessors : node -> pvector of nodes
;; Returns: ordered-map node -> node (idom for each node, entry has #f)
;;
(define (compute-idom node-compare nodes entry get-predecessors)
  ;; Build node -> RPO index mapping
  (define node->idx
    (for/fold ([m (ordered-map-empty node-compare)])
              ([node (in-pvector nodes)]
               [idx (in-naturals)])
      (ordered-map-set m node idx)))

  (define initial-idom (ordered-map-set (ordered-map-empty node-compare) entry #f))

  ;; Intersect function
  (define (intersect idom b1 b2)
    (let loop ([finger1 b1] [finger2 b2])
      (cond
        [(equal? finger1 finger2) finger1]
        [(> (ordered-map-ref node->idx finger1 0)
            (ordered-map-ref node->idx finger2 0))
         (loop (ordered-map-ref idom finger1 #f) finger2)]
        [else
         (loop finger1 (ordered-map-ref idom finger2 #f))])))

  ;; Iterate until fixed point
  (define (iterate idom)
    (define-values (new-idom changed)
      (for/fold ([d idom] [ch #f])
                ([node (in-pvector nodes)])
        (if (equal? node entry)
            (values d ch)
            (let ()
              ;; Filter predecessors that have idom computed
              (define preds
                (for/fold ([acc (pvector-empty)])
                          ([p (in-pvector (get-predecessors node))])
                  (if (ordered-map-has-key? d p)
                      (pvector-cons-right acc p)
                      acc)))
              (if (pvector-empty? preds)
                  (values d ch)
                  (let ()
                    (define-values (first-pred rest-preds) (pvector-pop-left preds))
                    (define new-idom-val
                      (for/fold ([acc first-pred])
                                ([pred (in-pvector rest-preds)])
                        (if (ordered-map-has-key? d pred)
                            (intersect d pred acc)
                            acc)))
                    (if (equal? new-idom-val (ordered-map-ref d node #f))
                        (values d ch)
                        (values (ordered-map-set d node new-idom-val) #t))))))))
    (values new-idom changed))

  (let loop ([idom initial-idom])
    (define-values (new-idom changed) (iterate idom))
    (if changed
        (loop new-idom)
        new-idom)))

;; ============================================================
;; Dominator Tree
;; ============================================================

;; Build dominator tree from idom
;; Parameters:
;;   node-compare : comparator for nodes
;;   nodes        : pvector of nodes
;;   idom         : ordered-map node -> node
;; Returns: ordered-map node -> pvector of nodes (children in dom tree)
;;
(define (compute-dominator-tree node-compare nodes idom)
  ;; Initialize empty children lists
  (define initial-tree
    (for/fold ([t (ordered-map-empty node-compare)])
              ([node (in-pvector nodes)])
      (ordered-map-set t node (pvector-empty))))

  ;; Build tree
  (for/fold ([tree initial-tree])
            ([node (in-pvector nodes)])
    (define parent (ordered-map-ref idom node #f))
    (if parent
        (ordered-map-set tree parent
          (pvector-cons-right (ordered-map-ref tree parent (pvector-empty)) node))
        tree)))

;; ============================================================
;; Dominance Frontier
;; ============================================================

;; Compute dominance frontier
;; Parameters:
;;   node-compare     : comparator for nodes
;;   nodes            : pvector of nodes
;;   entry            : node
;;   get-predecessors : node -> pvector of nodes
;;   idom             : ordered-map (optional, computed if not provided)
;; Returns: ordered-map node -> ordered-map (as set) - DF for each node
;;
(define (compute-dominance-frontier node-compare nodes entry get-predecessors
                                    #:idom [idom #f])
  (define dom-idom
    (or idom (compute-idom node-compare nodes entry get-predecessors)))

  ;; Initialize empty DF sets
  (define initial-df
    (for/fold ([df (ordered-map-empty node-compare)])
              ([node (in-pvector nodes)])
      (ordered-map-set df node (empty-set node-compare))))

  ;; For each node with multiple predecessors
  (for/fold ([df initial-df])
            ([node (in-pvector nodes)])
    (define preds (get-predecessors node))
    (if (> (pvector-length preds) 1)
        ;; For each predecessor, walk up dom tree
        (for/fold ([df* df])
                  ([pred (in-pvector preds)])
          (let loop ([runner pred] [df** df*])
            (if (and runner
                     (not (equal? runner (ordered-map-ref dom-idom node #f))))
                (loop (ordered-map-ref dom-idom runner #f)
                      (ordered-map-set df** runner
                        (set-add (ordered-map-ref df** runner (empty-set node-compare)) node)))
                df**)))
        df)))

;; ============================================================
;; Post-Dominator Computation
;; ============================================================

;; Helper: build reverse graph predecessors
(define (reverse-preds nodes get-successors)
  (lambda (node)
    (for/fold ([acc (pvector-empty)])
              ([n (in-pvector nodes)])
      (define succs (get-successors n))
      (if (for/or ([s (in-pvector succs)]) (equal? s node))
          (pvector-cons-right acc n)
          acc))))

;; Compute post-dominators (dominators in reverse CFG)
;; Parameters:
;;   node-compare   : comparator for nodes
;;   nodes          : pvector of nodes - all nodes in reverse RPO
;;   exit           : node - exit node
;;   get-successors : node -> pvector of nodes
;; Returns: ordered-map node -> ordered-map (as set)
;;
(define (compute-post-dominators node-compare nodes exit get-successors)
  (compute-dominators node-compare nodes exit (reverse-preds nodes get-successors)))

;; Compute post immediate dominators
;; Parameters:
;;   node-compare   : comparator for nodes
;;   nodes          : pvector of nodes
;;   exit           : node
;;   get-successors : node -> pvector of nodes
;; Returns: ordered-map node -> node
;;
(define (compute-post-idom node-compare nodes exit get-successors)
  (compute-idom node-compare (pvector-reverse nodes) exit (reverse-preds nodes get-successors)))

;; ============================================================
;; Utilities
;; ============================================================

;; Check if a dominates b
;; Parameters:
;;   node-compare : comparator for nodes
;;   dom          : ordered-map from compute-dominators
;;   a            : node
;;   b            : node
;; Returns: boolean
;;
(define (dominates? node-compare dom a b)
  (set-contains? (ordered-map-ref dom b (empty-set node-compare)) a))

;; Check if a strictly dominates b (a dom b and a != b)
;;
(define (strictly-dominates? node-compare dom a b)
  (and (not (equal? a b))
       (dominates? node-compare dom a b)))
