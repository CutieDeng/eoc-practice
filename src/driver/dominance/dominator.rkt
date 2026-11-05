#lang racket/base

;; ============================================================
;; Driver: Dominator Analysis
;; ============================================================
;;
;; Parameterized dominance algorithms.  They do NOT depend on any
;; specific graph representation: nodes are opaque values compared
;; with equal?, and the graph is supplied through `nodes` (a
;; pvector) plus a predecessor/successor function.
;;
;; Data structures:
;;   - node -> X maps          : immutable equal?-hash
;;   - sets of nodes           : immutable hash node -> #t
;;   - ordered node collections: pvector, ordered by position in
;;                               `nodes` (so results are
;;                               deterministic and independent of
;;                               hash iteration order)
;;
;; Precondition shared by compute-idom / compute-dominance-frontier:
;; `nodes` must be in reverse postorder (the CHK intersect walks
;; idom chains by RPO index).
;;
;; ============================================================

(require "../../kernel/data/data.rkt")

(provide
  ;; Dominator computation
  compute-dominators
  compute-idom
  compute-dominator-tree
  compute-dominance-frontier

  ;; Post-dominators
  compute-post-dominators
  compute-post-idom

  ;; Utilities
  dominates?
  strictly-dominates?)

;; ============================================================
;; Set operations (immutable hash as set)
;; ============================================================

(define (set-singleton x) (hash x #t))
(define (set-contains? s x) (hash-has-key? s x))
(define (set-add s x) (hash-set s x #t))

(define (set-intersect a b)
  (for/fold ([result (hash)])
            ([k (in-hash-keys a)])
    (if (hash-has-key? b k)
        (hash-set result k #t)
        result)))

;; ============================================================
;; Dominator Computation (iterative data-flow)
;; ============================================================

;; Compute dominators using iterative algorithm
;; Parameters:
;;   nodes            : pvector of nodes - all nodes in RPO
;;   entry            : node - entry node
;;   get-predecessors : node -> pvector of nodes
;; Returns: hash node -> hash (as set) - dominators for each node
;;
(define (compute-dominators nodes entry get-predecessors)
  ;; All nodes as a set
  (define all-nodes-set
    (for/fold ([s (hash)])
              ([n (in-pvector nodes)])
      (set-add s n)))

  ;; Initialize: dom[entry] = {entry}, dom[n] = all nodes
  (define initial-dom
    (for/fold ([m (hash)])
              ([node (in-pvector nodes)])
      (hash-set m node
                (if (equal? node entry)
                    (set-singleton entry)
                    all-nodes-set))))

  ;; Iterate until fixed point
  (define (iterate dom)
    (for/fold ([d dom] [ch #f])
              ([node (in-pvector nodes)])
      (if (equal? node entry)
          (values d ch)
          (let ()
            (define preds (get-predecessors node))
            (define new-dom-set
              (if (pvector-empty? preds)
                  (set-singleton node)
                  (set-add
                    (for/fold ([acc all-nodes-set])
                              ([pred (in-pvector preds)])
                      (set-intersect acc (hash-ref d pred (hash))))
                    node)))
            (if (equal? new-dom-set (hash-ref d node (hash)))
                (values d ch)
                (values (hash-set d node new-dom-set) #t))))))

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
;;   nodes            : pvector of nodes - all nodes in RPO
;;   entry            : node - entry node
;;   get-predecessors : node -> pvector of nodes
;; Returns: hash node -> node (idom for each node, entry maps to #f)
;;
(define (compute-idom nodes entry get-predecessors)
  ;; Build node -> RPO index mapping
  (define node->idx
    (for/fold ([m (hash)])
              ([node (in-pvector nodes)]
               [idx (in-naturals)])
      (hash-set m node idx)))

  (define initial-idom (hash entry #f))

  ;; Intersect function
  (define (intersect idom b1 b2)
    (let loop ([finger1 b1] [finger2 b2])
      (cond
        [(equal? finger1 finger2) finger1]
        [(> (hash-ref node->idx finger1 0)
            (hash-ref node->idx finger2 0))
         (loop (hash-ref idom finger1 #f) finger2)]
        [else
         (loop finger1 (hash-ref idom finger2 #f))])))

  ;; Iterate until fixed point
  (define (iterate idom)
    (for/fold ([d idom] [ch #f])
              ([node (in-pvector nodes)])
      (if (equal? node entry)
          (values d ch)
          (let ()
            ;; Filter predecessors that have idom computed
            (define preds
              (for/fold ([acc (pvector-empty)])
                        ([p (in-pvector (get-predecessors node))])
                (if (hash-has-key? d p)
                    (pvector-cons-right acc p)
                    acc)))
            (if (pvector-empty? preds)
                (values d ch)
                (let ()
                  (define-values (first-pred rest-preds) (pvector-pop-left preds))
                  (define new-idom-val
                    (for/fold ([acc first-pred])
                              ([pred (in-pvector rest-preds)])
                      (if (hash-has-key? d pred)
                          (intersect d pred acc)
                          acc)))
                  (if (equal? new-idom-val (hash-ref d node #f))
                      (values d ch)
                      (values (hash-set d node new-idom-val) #t))))))))

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
;;   nodes : pvector of nodes
;;   idom  : hash node -> node
;; Returns: hash node -> pvector of nodes (children, in `nodes` order)
;;
(define (compute-dominator-tree nodes idom)
  ;; Initialize empty children lists
  (define initial-tree
    (for/fold ([t (hash)])
              ([node (in-pvector nodes)])
      (hash-set t node (pvector-empty))))

  ;; Build tree
  (for/fold ([tree initial-tree])
            ([node (in-pvector nodes)])
    (define parent (hash-ref idom node #f))
    (if parent
        (hash-set tree parent
                  (pvector-cons-right (hash-ref tree parent (pvector-empty)) node))
        tree)))

;; ============================================================
;; Dominance Frontier
;; ============================================================

;; Compute dominance frontier
;; Parameters:
;;   nodes            : pvector of nodes (RPO)
;;   entry            : node
;;   get-predecessors : node -> pvector of nodes
;;   idom             : hash (optional, computed if not provided)
;; Returns: hash node -> pvector of nodes - DF for each node, each
;;          frontier listed in `nodes` order
;;
(define (compute-dominance-frontier nodes entry get-predecessors
                                    #:idom [idom #f])
  (define dom-idom
    (or idom (compute-idom nodes entry get-predecessors)))

  ;; DF as hash node -> hash-set
  (define df-sets
    (for/fold ([df (hash)])
              ([node (in-pvector nodes)])
      (define preds (get-predecessors node))
      (if (> (pvector-length preds) 1)
          ;; For each predecessor, walk up dom tree
          (for/fold ([df* df])
                    ([pred (in-pvector preds)])
            (let loop ([runner pred] [df** df*])
              (if (and runner
                       (not (equal? runner (hash-ref dom-idom node #f))))
                  (loop (hash-ref dom-idom runner #f)
                        (hash-set df** runner
                                  (set-add (hash-ref df** runner (hash)) node)))
                  df**)))
          df)))

  ;; Materialize each frontier as a pvector in `nodes` order.
  (for/fold ([df (hash)])
            ([node (in-pvector nodes)])
    (define s (hash-ref df-sets node (hash)))
    (hash-set df node
              (for/pvector ([n (in-pvector nodes)] #:when (hash-has-key? s n))
                n))))

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
;;   nodes          : pvector of nodes - all nodes in reverse RPO
;;   exit           : node - exit node
;;   get-successors : node -> pvector of nodes
;; Returns: hash node -> hash (as set)
;;
(define (compute-post-dominators nodes exit get-successors)
  (compute-dominators nodes exit (reverse-preds nodes get-successors)))

;; Compute post immediate dominators
;; Parameters:
;;   nodes          : pvector of nodes
;;   exit           : node
;;   get-successors : node -> pvector of nodes
;; Returns: hash node -> node
;;
(define (compute-post-idom nodes exit get-successors)
  (compute-idom (pvector-reverse nodes) exit (reverse-preds nodes get-successors)))

;; ============================================================
;; Utilities
;; ============================================================

;; Check if a dominates b
;; Parameters:
;;   dom : hash from compute-dominators
;;   a   : node
;;   b   : node
;; Returns: boolean
;;
(define (dominates? dom a b)
  (set-contains? (hash-ref dom b (hash)) a))

;; Check if a strictly dominates b (a dom b and a != b)
;;
(define (strictly-dominates? dom a b)
  (and (not (equal? a b))
       (dominates? dom a b)))
