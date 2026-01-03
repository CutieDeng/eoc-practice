#lang racket/base

;; ============================================================
;; Driver: Dominator Algorithms
;; ============================================================
;;
;; Parameterized dominator tree algorithms.
;; These do NOT depend on any specific graph representation.
;;
;; ============================================================

(require racket/list)
(require racket/set)
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
;; Dominator Computation (Cooper-Harvey-Kennedy Algorithm)
;; ============================================================

;; Compute dominators using iterative algorithm
;; Parameters:
;;   nodes            : (listof node) - all nodes in RPO
;;   entry            : node - entry node
;;   get-predecessors : node -> (listof node)
;; Returns: hash node -> (setof node) - dominators for each node
;;
(define (compute-dominators nodes entry get-predecessors)
  (define all-nodes-set (for/set ([n nodes]) n))
  (define dom (make-hash))

  ;; Initialize: dom[entry] = {entry}, dom[n] = all nodes
  (hash-set! dom entry (set entry))
  (for ([node nodes] #:unless (equal? node entry))
    (hash-set! dom node all-nodes-set))

  ;; Iterate until fixed point
  (define (iterate)
    (define changed #f)
    (for ([node nodes] #:unless (equal? node entry))
      (define preds (get-predecessors node))
      (define new-dom
        (if (null? preds)
            (set node)
            (set-add
              (apply set-intersect
                (for/list ([pred preds])
                  (hash-ref dom pred (set))))
              node)))
      (unless (equal? new-dom (hash-ref dom node))
        (hash-set! dom node new-dom)
        (set! changed #t)))
    changed)

  (let loop ()
    (when (iterate)
      (loop)))

  dom)

;; ============================================================
;; Immediate Dominator (Cooper-Harvey-Kennedy)
;; ============================================================

;; Compute immediate dominators
;; Parameters:
;;   nodes            : (listof node) - all nodes in RPO
;;   entry            : node - entry node
;;   get-predecessors : node -> (listof node)
;; Returns: hash node -> node (idom for each node, entry has #f)
;;
(define (compute-idom nodes entry get-predecessors)
  ;; Build node -> RPO index mapping
  (define node->idx (make-hash))
  (for ([node nodes] [idx (in-naturals)])
    (hash-set! node->idx node idx))

  (define idom (make-hash))
  (hash-set! idom entry #f)

  ;; Intersect function
  (define (intersect b1 b2)
    (let loop ([finger1 b1] [finger2 b2])
      (cond
        [(equal? finger1 finger2) finger1]
        [(> (hash-ref node->idx finger1)
            (hash-ref node->idx finger2))
         (loop (hash-ref idom finger1) finger2)]
        [else
         (loop finger1 (hash-ref idom finger2))])))

  ;; Iterate until fixed point
  (define (iterate)
    (define changed #f)
    (for ([node nodes] #:unless (equal? node entry))
      (define preds
        (filter (lambda (p) (hash-has-key? idom p))
          (get-predecessors node)))
      (unless (null? preds)
        (define new-idom
          (for/fold ([acc (car preds)]) ([pred (cdr preds)])
            (if (hash-has-key? idom pred)
                (intersect pred acc)
                acc)))
        (unless (equal? new-idom (hash-ref idom node #f))
          (hash-set! idom node new-idom)
          (set! changed #t))))
    changed)

  (let loop ()
    (when (iterate)
      (loop)))

  idom)

;; ============================================================
;; Dominator Tree
;; ============================================================

;; Build dominator tree from idom
;; Parameters:
;;   nodes : (listof node)
;;   idom  : hash node -> node
;; Returns: hash node -> (listof node) - children in dom tree
;;
(define (compute-dominator-tree nodes idom)
  (define tree (make-hash))

  ;; Initialize empty children lists
  (for ([node nodes])
    (hash-set! tree node '()))

  ;; Build tree
  (for ([node nodes])
    (define parent (hash-ref idom node #f))
    (when parent
      (hash-set! tree parent
        (cons node (hash-ref tree parent)))))

  tree)

;; ============================================================
;; Dominance Frontier
;; ============================================================

;; Compute dominance frontier
;; Parameters:
;;   nodes            : (listof node)
;;   entry            : node
;;   get-predecessors : node -> (listof node)
;;   idom             : hash (optional, computed if not provided)
;; Returns: hash node -> (setof node) - DF for each node
;;
(define (compute-dominance-frontier nodes entry get-predecessors
                                    #:idom [idom #f])
  (define dom-idom
    (or idom (compute-idom nodes entry get-predecessors)))

  (define df (make-hash))
  (for ([node nodes])
    (hash-set! df node (set)))

  ;; For each node with multiple predecessors
  (for ([node nodes])
    (define preds (get-predecessors node))
    (when (> (length preds) 1)
      ;; For each predecessor, walk up dom tree
      (for ([pred preds])
        (let loop ([runner pred])
          (when (and runner
                     (not (equal? runner (hash-ref dom-idom node #f))))
            (hash-set! df runner
              (set-add (hash-ref df runner) node))
            (loop (hash-ref dom-idom runner #f)))))))

  df)

;; ============================================================
;; Post-Dominator Computation
;; ============================================================

;; Compute post-dominators (dominators in reverse CFG)
;; Parameters:
;;   nodes          : (listof node) - all nodes in reverse RPO
;;   exit           : node - exit node
;;   get-successors : node -> (listof node)
;; Returns: hash node -> (setof node)
;;
(define (compute-post-dominators nodes exit get-successors)
  ;; Flip the graph direction
  (compute-dominators
    nodes
    exit
    (lambda (node)
      ;; Predecessors in reverse graph = successors in original
      (for/list ([n nodes]
                 #:when (member node (get-successors n)))
        n))))

;; Compute post immediate dominators
;; Parameters:
;;   nodes          : (listof node)
;;   exit           : node
;;   get-successors : node -> (listof node)
;; Returns: hash node -> node
;;
(define (compute-post-idom nodes exit get-successors)
  (compute-idom
    (reverse nodes)
    exit
    (lambda (node)
      (for/list ([n nodes]
                 #:when (member node (get-successors n)))
        n))))

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
  (set-member? (hash-ref dom b (set)) a))

;; Check if a strictly dominates b (a dom b and a != b)
;;
(define (strictly-dominates? dom a b)
  (and (not (equal? a b))
       (dominates? dom a b)))
