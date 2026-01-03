#lang racket/base

;; ============================================================
;; Driver: Graph Traversal Algorithms
;; ============================================================
;;
;; Parameterized graph traversal algorithms.
;; These do NOT depend on any specific graph representation.
;; All graph operations are passed as parameters.
;;
;; ============================================================

(require racket/list)
(require racket/set)
(require "../../kernel/data/main.rkt")

(provide
  ;; Traversal algorithms
  dfs-preorder
  dfs-postorder
  dfs-reverse-postorder
  bfs

  ;; Topology sort
  topology-sort

  ;; Reachability
  reachable-from
  reachable-from-set

  ;; Path finding
  find-path
  all-paths)

;; ============================================================
;; DFS Traversal
;; ============================================================

;; DFS preorder traversal
;; Parameters:
;;   get-successors : node -> (listof node)
;;   start          : node - starting node
;; Returns: (listof node) in preorder
;;
(define (dfs-preorder get-successors start)
  (define visited (mutable-set))
  (define result '())

  (define (visit node)
    (unless (set-member? visited node)
      (set-add! visited node)
      (set! result (cons node result))
      (for ([succ (get-successors node)])
        (visit succ))))

  (visit start)
  (reverse result))

;; DFS postorder traversal
;; Parameters:
;;   get-successors : node -> (listof node)
;;   start          : node - starting node
;; Returns: (listof node) in postorder
;;
(define (dfs-postorder get-successors start)
  (define visited (mutable-set))
  (define result '())

  (define (visit node)
    (unless (set-member? visited node)
      (set-add! visited node)
      (for ([succ (get-successors node)])
        (visit succ))
      (set! result (cons node result))))

  (visit start)
  (reverse result))

;; DFS reverse postorder (topological order for DAGs)
;; Parameters:
;;   get-successors : node -> (listof node)
;;   start          : node - starting node
;; Returns: (listof node) in reverse postorder
;;
(define (dfs-reverse-postorder get-successors start)
  (reverse (dfs-postorder get-successors start)))

;; ============================================================
;; BFS Traversal
;; ============================================================

;; Breadth-first search
;; Parameters:
;;   get-successors : node -> (listof node)
;;   start          : node - starting node
;; Returns: (listof node) in BFS order
;;
(define (bfs get-successors start)
  (define visited (mutable-set))
  (define result '())
  (define queue (list start))

  (set-add! visited start)

  (let loop ()
    (unless (null? queue)
      (define node (car queue))
      (set! queue (cdr queue))
      (set! result (cons node result))

      (for ([succ (get-successors node)])
        (unless (set-member? visited succ)
          (set-add! visited succ)
          (set! queue (append queue (list succ)))))

      (loop)))

  (reverse result))

;; ============================================================
;; Topology Sort
;; ============================================================

;; Topological sort using Kahn's algorithm
;; Parameters:
;;   nodes            : (listof node) - all nodes
;;   get-successors   : node -> (listof node)
;;   get-predecessors : node -> (listof node)
;; Returns: (listof node) in topological order, or #f if cycle exists
;;
(define (topology-sort nodes get-successors get-predecessors)
  (define in-degree (make-hash))

  ;; Calculate in-degrees
  (for ([node nodes])
    (hash-set! in-degree node (length (get-predecessors node))))

  ;; Find nodes with zero in-degree
  (define queue
    (filter (lambda (n) (= (hash-ref in-degree n) 0)) nodes))

  (define result '())

  (let loop ()
    (unless (null? queue)
      (define node (car queue))
      (set! queue (cdr queue))
      (set! result (cons node result))

      (for ([succ (get-successors node)])
        (define new-deg (- (hash-ref in-degree succ) 1))
        (hash-set! in-degree succ new-deg)
        (when (= new-deg 0)
          (set! queue (append queue (list succ)))))

      (loop)))

  ;; Check if all nodes are processed (no cycle)
  (if (= (length result) (length nodes))
      (reverse result)
      #f))

;; ============================================================
;; Reachability
;; ============================================================

;; Find all nodes reachable from start
;; Parameters:
;;   get-successors : node -> (listof node)
;;   start          : node - starting node
;; Returns: (listof node) reachable from start
;;
(define (reachable-from get-successors start)
  (dfs-preorder get-successors start))

;; Find all nodes reachable from any node in starts
;; Parameters:
;;   get-successors : node -> (listof node)
;;   starts         : (listof node) - starting nodes
;; Returns: (setof node) reachable from starts
;;
(define (reachable-from-set get-successors starts)
  (define visited (mutable-set))

  (define (visit node)
    (unless (set-member? visited node)
      (set-add! visited node)
      (for ([succ (get-successors node)])
        (visit succ))))

  (for ([start starts])
    (visit start))

  (for/set ([n visited]) n))

;; ============================================================
;; Path Finding
;; ============================================================

;; Find a path from start to end (if exists)
;; Parameters:
;;   get-successors : node -> (listof node)
;;   start          : node
;;   end            : node
;; Returns: (listof node) path, or #f if no path
;;
(define (find-path get-successors start end)
  (define visited (mutable-set))

  (define (search node path)
    (cond
      [(equal? node end) (reverse (cons node path))]
      [(set-member? visited node) #f]
      [else
       (set-add! visited node)
       (for/or ([succ (get-successors node)])
         (search succ (cons node path)))]))

  (search start '()))

;; Find all paths from start to end
;; Parameters:
;;   get-successors : node -> (listof node)
;;   start          : node
;;   end            : node
;; Returns: (listof (listof node)) all paths
;;
(define (all-paths get-successors start end)
  (define (search node path visited)
    (cond
      [(equal? node end) (list (reverse (cons node path)))]
      [(set-member? visited node) '()]
      [else
       (define new-visited (set-add visited node))
       (apply append
         (for/list ([succ (get-successors node)])
           (search succ (cons node path) new-visited)))]))

  (search start '() (set)))
