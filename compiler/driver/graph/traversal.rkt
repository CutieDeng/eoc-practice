#lang racket/base

;; ============================================================
;; Driver: Graph Traversal Algorithms
;; ============================================================
;;
;; Parameterized graph traversal algorithms using pvector.
;; These do NOT depend on any specific graph representation.
;; All graph operations are passed as parameters.
;;
;; ============================================================

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
;;   node-compare   : comparator for nodes
;;   get-successors : node -> pvector of nodes
;;   start          : node - starting node
;; Returns: pvector of nodes in preorder
;;
(define (dfs-preorder node-compare get-successors start)
  (define visited (ordered-map-empty node-compare))

  (define (visit node result)
    (cond
      [(ordered-map-has-key? visited node) result]
      [else
       (set! visited (ordered-map-set visited node #t))
       (define result* (pvector-cons-right result node))
       (for/fold ([r result*])
                 ([succ (in-pvector (get-successors node))])
         (visit succ r))]))

  (visit start (pvector-empty)))

;; DFS postorder traversal
;; Parameters:
;;   node-compare   : comparator for nodes
;;   get-successors : node -> pvector of nodes
;;   start          : node - starting node
;; Returns: pvector of nodes in postorder
;;
(define (dfs-postorder node-compare get-successors start)
  (define visited (ordered-map-empty node-compare))

  (define (visit node result)
    (cond
      [(ordered-map-has-key? visited node) result]
      [else
       (set! visited (ordered-map-set visited node #t))
       (define result*
         (for/fold ([r result])
                   ([succ (in-pvector (get-successors node))])
           (visit succ r)))
       (pvector-cons-right result* node)]))

  (visit start (pvector-empty)))

;; DFS reverse postorder (topological order for DAGs)
;; Parameters:
;;   node-compare   : comparator for nodes
;;   get-successors : node -> pvector of nodes
;;   start          : node - starting node
;; Returns: pvector of nodes in reverse postorder
;;
(define (dfs-reverse-postorder node-compare get-successors start)
  (pvector-reverse (dfs-postorder node-compare get-successors start)))

;; ============================================================
;; BFS Traversal
;; ============================================================

;; Breadth-first search
;; Parameters:
;;   node-compare   : comparator for nodes
;;   get-successors : node -> pvector of nodes
;;   start          : node - starting node
;; Returns: pvector of nodes in BFS order
;;
(define (bfs node-compare get-successors start)
  (define visited (ordered-map-empty node-compare))
  (set! visited (ordered-map-set visited start #t))

  (let loop ([queue (pvector-cons-right (pvector-empty) start)]
             [result (pvector-empty)])
    (cond
      [(pvector-empty? queue) result]
      [else
       (define-values (node queue*) (pvector-pop-left queue))
       (define result* (pvector-cons-right result node))

       (define-values (new-queue new-visited)
         (for/fold ([q queue*] [v visited])
                   ([succ (in-pvector (get-successors node))])
           (cond
             [(ordered-map-has-key? v succ) (values q v)]
             [else
              (values (pvector-cons-right q succ)
                      (ordered-map-set v succ #t))])))

       (set! visited new-visited)
       (loop new-queue result*)])))

;; ============================================================
;; Topology Sort
;; ============================================================

;; Topological sort using Kahn's algorithm
;; Parameters:
;;   node-compare     : comparator for nodes
;;   nodes            : pvector of nodes - all nodes
;;   get-successors   : node -> pvector of nodes
;;   get-predecessors : node -> pvector of nodes
;; Returns: pvector of nodes in topological order, or #f if cycle exists
;;
(define (topology-sort node-compare nodes get-successors get-predecessors)
  (define in-degree (ordered-map-empty node-compare))

  ;; Calculate in-degrees
  (for ([node (in-pvector nodes)])
    (set! in-degree
          (ordered-map-set in-degree node
                           (pvector-length (get-predecessors node)))))

  ;; Find nodes with zero in-degree
  (define initial-queue
    (for/fold ([q (pvector-empty)])
              ([node (in-pvector nodes)])
      (if (= (ordered-map-ref in-degree node 0) 0)
          (pvector-cons-right q node)
          q)))

  (let loop ([queue initial-queue]
             [result (pvector-empty)]
             [degrees in-degree])
    (cond
      [(pvector-empty? queue)
       ;; Check if all nodes are processed (no cycle)
       (if (= (pvector-length result) (pvector-length nodes))
           result
           #f)]
      [else
       (define-values (node queue*) (pvector-pop-left queue))
       (define result* (pvector-cons-right result node))

       (define-values (new-queue new-degrees)
         (for/fold ([q queue*] [d degrees])
                   ([succ (in-pvector (get-successors node))])
           (define new-deg (- (ordered-map-ref d succ 0) 1))
           (define d* (ordered-map-set d succ new-deg))
           (if (= new-deg 0)
               (values (pvector-cons-right q succ) d*)
               (values q d*))))

       (loop new-queue result* new-degrees)])))

;; ============================================================
;; Reachability
;; ============================================================

;; Find all nodes reachable from start
;; Parameters:
;;   node-compare   : comparator for nodes
;;   get-successors : node -> pvector of nodes
;;   start          : node - starting node
;; Returns: pvector of nodes reachable from start
;;
(define (reachable-from node-compare get-successors start)
  (dfs-preorder node-compare get-successors start))

;; Find all nodes reachable from any node in starts
;; Parameters:
;;   node-compare   : comparator for nodes
;;   get-successors : node -> pvector of nodes
;;   starts         : pvector of nodes - starting nodes
;; Returns: ordered-map (as set) of reachable nodes
;;
(define (reachable-from-set node-compare get-successors starts)
  (define visited (ordered-map-empty node-compare))

  (define (visit node)
    (unless (ordered-map-has-key? visited node)
      (set! visited (ordered-map-set visited node #t))
      (for ([succ (in-pvector (get-successors node))])
        (visit succ))))

  (for ([start (in-pvector starts)])
    (visit start))

  visited)

;; ============================================================
;; Path Finding
;; ============================================================

;; Find a path from start to end (if exists)
;; Parameters:
;;   node-compare   : comparator for nodes
;;   get-successors : node -> pvector of nodes
;;   start          : node
;;   end            : node
;; Returns: pvector of nodes (path), or #f if no path
;;
(define (find-path node-compare get-successors start end)
  (define visited (ordered-map-empty node-compare))

  (define (search node path)
    (cond
      [(equal? node end)
       (pvector-cons-right path node)]
      [(ordered-map-has-key? visited node) #f]
      [else
       (set! visited (ordered-map-set visited node #t))
       (define path* (pvector-cons-right path node))
       (for/or ([succ (in-pvector (get-successors node))])
         (search succ path*))]))

  (search start (pvector-empty)))

;; Find all paths from start to end
;; Parameters:
;;   node-compare   : comparator for nodes
;;   get-successors : node -> pvector of nodes
;;   start          : node
;;   end            : node
;; Returns: pvector of pvector (all paths)
;;
(define (all-paths node-compare get-successors start end)
  (define (search node path visited)
    (cond
      [(equal? node end)
       (pvector-cons-right (pvector-empty) (pvector-cons-right path node))]
      [(ordered-map-has-key? visited node) (pvector-empty)]
      [else
       (define new-visited (ordered-map-set visited node #t))
       (define path* (pvector-cons-right path node))
       (for/fold ([paths (pvector-empty)])
                 ([succ (in-pvector (get-successors node))])
         (pvector-append paths (search succ path* new-visited)))]))

  (search start (pvector-empty) (ordered-map-empty node-compare)))
