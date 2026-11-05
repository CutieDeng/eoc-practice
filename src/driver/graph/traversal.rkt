#lang racket/base

;; ============================================================
;; Driver: Graph Traversal Algorithms
;; ============================================================
;;
;; Parameterized graph traversal algorithms using pvector.
;; These do NOT depend on any specific graph representation.
;; All graph operations are passed as parameters.
;;
;; Nodes are compared with equal?; visited sets are immutable
;; hashes.  Every result that is a sequence is a pvector whose
;; order is determined solely by the traversal and the order in
;; which get-successors returns successors.
;;
;; ============================================================

(require "../../kernel/data/data.rkt")

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
;;   get-successors : node -> pvector of nodes
;;   start          : node - starting node
;; Returns: pvector of nodes in preorder
;;
(define (dfs-preorder get-successors start)
  (define visited (hash))

  (define (visit node result)
    (cond
      [(hash-has-key? visited node) result]
      [else
       (set! visited (hash-set visited node #t))
       (define result* (pvector-cons-right result node))
       (for/fold ([r result*])
                 ([succ (in-pvector (get-successors node))])
         (visit succ r))]))

  (visit start (pvector-empty)))

;; DFS postorder traversal
;; Parameters:
;;   get-successors : node -> pvector of nodes
;;   start          : node - starting node
;; Returns: pvector of nodes in postorder
;;
(define (dfs-postorder get-successors start)
  (define visited (hash))

  (define (visit node result)
    (cond
      [(hash-has-key? visited node) result]
      [else
       (set! visited (hash-set visited node #t))
       (define result*
         (for/fold ([r result])
                   ([succ (in-pvector (get-successors node))])
           (visit succ r)))
       (pvector-cons-right result* node)]))

  (visit start (pvector-empty)))

;; DFS reverse postorder (topological order for DAGs)
;; Parameters:
;;   get-successors : node -> pvector of nodes
;;   start          : node - starting node
;; Returns: pvector of nodes in reverse postorder
;;
(define (dfs-reverse-postorder get-successors start)
  (pvector-reverse (dfs-postorder get-successors start)))

;; ============================================================
;; BFS Traversal
;; ============================================================

;; Breadth-first search
;; Parameters:
;;   get-successors : node -> pvector of nodes
;;   start          : node - starting node
;; Returns: pvector of nodes in BFS order
;;
(define (bfs get-successors start)
  (let loop ([queue (pvector-cons-right (pvector-empty) start)]
             [visited (hash start #t)]
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
             [(hash-has-key? v succ) (values q v)]
             [else
              (values (pvector-cons-right q succ)
                      (hash-set v succ #t))])))

       (loop new-queue new-visited result*)])))

;; ============================================================
;; Topology Sort
;; ============================================================

;; Topological sort using Kahn's algorithm
;; Parameters:
;;   nodes            : pvector of nodes - all nodes
;;   get-successors   : node -> pvector of nodes
;;   get-predecessors : node -> pvector of nodes
;; Returns: pvector of nodes in topological order, or #f if cycle exists
;;
(define (topology-sort nodes get-successors get-predecessors)
  ;; Calculate in-degrees
  (define in-degree
    (for/fold ([m (hash)])
              ([node (in-pvector nodes)])
      (hash-set m node (pvector-length (get-predecessors node)))))

  ;; Find nodes with zero in-degree
  (define initial-queue
    (for/fold ([q (pvector-empty)])
              ([node (in-pvector nodes)])
      (if (= (hash-ref in-degree node 0) 0)
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
           (define new-deg (- (hash-ref d succ 0) 1))
           (define d* (hash-set d succ new-deg))
           (if (= new-deg 0)
               (values (pvector-cons-right q succ) d*)
               (values q d*))))

       (loop new-queue result* new-degrees)])))

;; ============================================================
;; Reachability
;; ============================================================

;; Find all nodes reachable from start
;; Parameters:
;;   get-successors : node -> pvector of nodes
;;   start          : node - starting node
;; Returns: pvector of nodes reachable from start (preorder)
;;
(define (reachable-from get-successors start)
  (dfs-preorder get-successors start))

;; Find all nodes reachable from any node in starts
;; Parameters:
;;   get-successors : node -> pvector of nodes
;;   starts         : pvector of nodes - starting nodes
;; Returns: immutable hash (as set, node -> #t) of reachable nodes
;;
(define (reachable-from-set get-successors starts)
  (define visited (hash))

  (define (visit node)
    (unless (hash-has-key? visited node)
      (set! visited (hash-set visited node #t))
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
;;   get-successors : node -> pvector of nodes
;;   start          : node
;;   end            : node
;; Returns: pvector of nodes (path), or #f if no path
;;
(define (find-path get-successors start end)
  (define visited (hash))

  (define (search node path)
    (cond
      [(equal? node end)
       (pvector-cons-right path node)]
      [(hash-has-key? visited node) #f]
      [else
       (set! visited (hash-set visited node #t))
       (define path* (pvector-cons-right path node))
       (for/or ([succ (in-pvector (get-successors node))])
         (search succ path*))]))

  (search start (pvector-empty)))

;; Find all paths from start to end
;; Parameters:
;;   get-successors : node -> pvector of nodes
;;   start          : node
;;   end            : node
;; Returns: pvector of pvector (all paths)
;;
(define (all-paths get-successors start end)
  (define (search node path visited)
    (cond
      [(equal? node end)
       (pvector-cons-right (pvector-empty) (pvector-cons-right path node))]
      [(hash-has-key? visited node) (pvector-empty)]
      [else
       (define new-visited (hash-set visited node #t))
       (define path* (pvector-cons-right path node))
       (for/fold ([paths (pvector-empty)])
                 ([succ (in-pvector (get-successors node))])
         (pvector-append paths (search succ path* new-visited)))]))

  (search start (pvector-empty) (hash)))
