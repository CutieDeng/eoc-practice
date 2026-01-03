#lang racket/base

;; ============================================================
;; Driver: Strongly Connected Components
;; ============================================================
;;
;; Parameterized SCC algorithms (Kosaraju, Tarjan).
;; These do NOT depend on any specific graph representation.
;;
;; ============================================================

(require racket/list)
(require racket/set)
(require "../../kernel/data/main.rkt")

(provide
  ;; SCC algorithms
  scc-kosaraju
  scc-tarjan

  ;; Condensation graph
  condensation-graph

  ;; Utilities
  scc-id
  scc-members)

;; ============================================================
;; Kosaraju's Algorithm
;; ============================================================

;; Find SCCs using Kosaraju's algorithm
;; Parameters:
;;   nodes            : (listof node) - all nodes
;;   get-successors   : node -> (listof node)
;;   get-predecessors : node -> (listof node)
;; Returns: (listof (listof node)) - list of SCCs (each SCC is a list of nodes)
;;
(define (scc-kosaraju nodes get-successors get-predecessors)
  ;; Phase 1: DFS on original graph, record finish order
  (define visited (mutable-set))
  (define finish-order '())

  (define (dfs1 node)
    (unless (set-member? visited node)
      (set-add! visited node)
      (for ([succ (get-successors node)])
        (dfs1 succ))
      (set! finish-order (cons node finish-order))))

  (for ([node nodes])
    (dfs1 node))

  ;; Phase 2: DFS on reversed graph in reverse finish order
  (set-clear! visited)
  (define sccs '())

  (define (dfs2 node component)
    (cond
      [(set-member? visited node) component]
      [else
       (set-add! visited node)
       (define new-component (cons node component))
       (for/fold ([comp new-component]) ([pred (get-predecessors node)])
         (dfs2 pred comp))]))

  (for ([node finish-order])
    (unless (set-member? visited node)
      (define component (dfs2 node '()))
      (set! sccs (cons component sccs))))

  (reverse sccs))

;; ============================================================
;; Tarjan's Algorithm
;; ============================================================

;; Find SCCs using Tarjan's algorithm
;; Parameters:
;;   nodes          : (listof node) - all nodes
;;   get-successors : node -> (listof node)
;; Returns: (listof (listof node)) - list of SCCs in reverse topological order
;;
(define (scc-tarjan nodes get-successors)
  (define index-counter 0)
  (define index (make-hash))
  (define lowlink (make-hash))
  (define on-stack (mutable-set))
  (define stack '())
  (define sccs '())

  (define (strongconnect node)
    ;; Set index and lowlink
    (hash-set! index node index-counter)
    (hash-set! lowlink node index-counter)
    (set! index-counter (+ index-counter 1))

    ;; Push to stack
    (set! stack (cons node stack))
    (set-add! on-stack node)

    ;; Visit successors
    (for ([succ (get-successors node)])
      (cond
        [(not (hash-has-key? index succ))
         ;; Not visited
         (strongconnect succ)
         (hash-set! lowlink node
           (min (hash-ref lowlink node)
                (hash-ref lowlink succ)))]
        [(set-member? on-stack succ)
         ;; On stack, part of current SCC
         (hash-set! lowlink node
           (min (hash-ref lowlink node)
                (hash-ref index succ)))]))

    ;; If root of SCC, pop and record
    (when (= (hash-ref lowlink node) (hash-ref index node))
      (define scc
        (let loop ([component '()])
          (define top (car stack))
          (set! stack (cdr stack))
          (set-remove! on-stack top)
          (define new-component (cons top component))
          (if (equal? top node)
              new-component
              (loop new-component))))
      (set! sccs (cons scc sccs))))

  (for ([node nodes])
    (unless (hash-has-key? index node)
      (strongconnect node)))

  (reverse sccs))

;; ============================================================
;; Condensation Graph
;; ============================================================

;; Build condensation graph (DAG of SCCs)
;; Parameters:
;;   nodes          : (listof node) - all nodes
;;   get-successors : node -> (listof node)
;; Returns: (values
;;            node->scc-id  : hash - maps node to SCC id
;;            scc-nodes     : vector - maps SCC id to list of nodes
;;            scc-successors : procedure - scc-id -> (listof scc-id))
;;
(define (condensation-graph nodes get-successors)
  (define sccs (scc-tarjan nodes get-successors))
  (define num-sccs (length sccs))

  ;; Build node -> SCC id mapping
  (define node->scc-id (make-hash))
  (for ([scc sccs] [id (in-naturals)])
    (for ([node scc])
      (hash-set! node->scc-id node id)))

  ;; Build SCC nodes vector
  (define scc-nodes (list->vector sccs))

  ;; Build SCC adjacency
  (define scc-adj (make-vector num-sccs (set)))
  (for ([node nodes])
    (define src-scc (hash-ref node->scc-id node))
    (for ([succ (get-successors node)])
      (define dst-scc (hash-ref node->scc-id succ))
      (unless (= src-scc dst-scc)
        (vector-set! scc-adj src-scc
          (set-add (vector-ref scc-adj src-scc) dst-scc)))))

  ;; SCC successors function
  (define (scc-successors scc-id)
    (set->list (vector-ref scc-adj scc-id)))

  (values node->scc-id scc-nodes scc-successors))

;; ============================================================
;; Utilities
;; ============================================================

;; Get SCC id for a node
;; Parameters:
;;   node->scc-id : hash from condensation-graph
;;   node         : node
;; Returns: integer SCC id
;;
(define (scc-id node->scc-id node)
  (hash-ref node->scc-id node))

;; Get members of an SCC
;; Parameters:
;;   scc-nodes : vector from condensation-graph
;;   scc-id    : integer SCC id
;; Returns: (listof node)
;;
(define (scc-members scc-nodes scc-id)
  (vector-ref scc-nodes scc-id))
