#lang racket/base

;; ============================================================
;; Driver: Strongly Connected Components
;; ============================================================
;;
;; Parameterized SCC algorithms (Kosaraju, Tarjan) using pvector.
;; These do NOT depend on any specific graph representation.
;;
;; ============================================================

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
;;   node-compare     : comparator for nodes
;;   nodes            : pvector of nodes - all nodes
;;   get-successors   : node -> pvector of nodes
;;   get-predecessors : node -> pvector of nodes
;; Returns: pvector of pvector - list of SCCs (each SCC is a pvector of nodes)
;;
(define (scc-kosaraju node-compare nodes get-successors get-predecessors)
  ;; Phase 1: DFS on original graph, record finish order
  (define visited (ordered-map-empty node-compare))
  (define finish-order (pvector-empty))

  (define (dfs1 node)
    (unless (ordered-map-has-key? visited node)
      (set! visited (ordered-map-set visited node #t))
      (for ([succ (in-pvector (get-successors node))])
        (dfs1 succ))
      (set! finish-order (pvector-cons-right finish-order node))))

  (for ([node (in-pvector nodes)])
    (dfs1 node))

  ;; Phase 2: DFS on reversed graph in reverse finish order
  (set! visited (ordered-map-empty node-compare))
  (define sccs (pvector-empty))

  (define (dfs2 node component)
    (cond
      [(ordered-map-has-key? visited node) component]
      [else
       (set! visited (ordered-map-set visited node #t))
       (define new-component (pvector-cons-right component node))
       (for/fold ([comp new-component])
                 ([pred (in-pvector (get-predecessors node))])
         (dfs2 pred comp))]))

  ;; Process in reverse finish order
  (define reversed-finish (pvector-reverse finish-order))
  (for ([node (in-pvector reversed-finish)])
    (unless (ordered-map-has-key? visited node)
      (define component (dfs2 node (pvector-empty)))
      (set! sccs (pvector-cons-right sccs component))))

  sccs)

;; ============================================================
;; Tarjan's Algorithm
;; ============================================================

;; Find SCCs using Tarjan's algorithm
;; Parameters:
;;   node-compare   : comparator for nodes
;;   nodes          : pvector of nodes - all nodes
;;   get-successors : node -> pvector of nodes
;; Returns: pvector of pvector - list of SCCs in reverse topological order
;;
(define (scc-tarjan node-compare nodes get-successors)
  (define index-counter 0)
  (define index (ordered-map-empty node-compare))
  (define lowlink (ordered-map-empty node-compare))
  (define on-stack (ordered-map-empty node-compare))
  (define stack (pvector-empty))
  (define sccs (pvector-empty))

  (define (strongconnect node)
    ;; Set index and lowlink
    (set! index (ordered-map-set index node index-counter))
    (set! lowlink (ordered-map-set lowlink node index-counter))
    (set! index-counter (+ index-counter 1))

    ;; Push to stack
    (set! stack (pvector-cons-right stack node))
    (set! on-stack (ordered-map-set on-stack node #t))

    ;; Visit successors
    (for ([succ (in-pvector (get-successors node))])
      (cond
        [(not (ordered-map-has-key? index succ))
         ;; Not visited
         (strongconnect succ)
         (set! lowlink
               (ordered-map-set lowlink node
                 (min (ordered-map-ref lowlink node 0)
                      (ordered-map-ref lowlink succ 0))))]
        [(ordered-map-has-key? on-stack succ)
         ;; On stack, part of current SCC
         (set! lowlink
               (ordered-map-set lowlink node
                 (min (ordered-map-ref lowlink node 0)
                      (ordered-map-ref index succ 0))))]))

    ;; If root of SCC, pop and record
    (when (= (ordered-map-ref lowlink node 0)
             (ordered-map-ref index node 0))
      (define-values (scc new-stack)
        (let loop ([component (pvector-empty)] [stk stack])
          (define-values (top stk*) (pvector-pop-right stk))
          (set! on-stack (ordered-map-delete* on-stack top))
          (define new-component (pvector-cons-left component top))
          (if (equal? top node)
              (values new-component stk*)
              (loop new-component stk*))))
      (set! stack new-stack)
      (set! sccs (pvector-cons-right sccs scc))))

  (for ([node (in-pvector nodes)])
    (unless (ordered-map-has-key? index node)
      (strongconnect node)))

  sccs)

;; Helper: delete without error if key doesn't exist
(define (ordered-map-delete* om key)
  (if (ordered-map-has-key? om key)
      (let-values ([(m _) (ordered-map-delete om key)]) m)
      om))

;; ============================================================
;; Condensation Graph
;; ============================================================

;; Build condensation graph (DAG of SCCs)
;; Parameters:
;;   node-compare   : comparator for nodes
;;   nodes          : pvector of nodes - all nodes
;;   get-successors : node -> pvector of nodes
;; Returns: (values
;;            node->scc-id  : ordered-map - maps node to SCC id
;;            scc-nodes     : pvector - maps SCC id to pvector of nodes
;;            scc-successors : procedure - scc-id -> pvector of scc-ids)
;;
(define (condensation-graph node-compare nodes get-successors)
  (define sccs (scc-tarjan node-compare nodes get-successors))
  (define num-sccs (pvector-length sccs))

  ;; Build node -> SCC id mapping
  (define node->scc-id
    (for/fold ([m (ordered-map-empty node-compare)])
              ([scc (in-pvector sccs)]
               [id (in-naturals)])
      (for/fold ([m* m])
                ([node (in-pvector scc)])
        (ordered-map-set m* node id))))

  ;; Build SCC adjacency using ordered-map of ordered-map (as set)
  (define scc-adj
    (for/fold ([adj (ordered-map-empty integer-compare)])
              ([node (in-pvector nodes)])
      (define src-scc (ordered-map-ref node->scc-id node 0))
      (for/fold ([adj* adj])
                ([succ (in-pvector (get-successors node))])
        (define dst-scc (ordered-map-ref node->scc-id succ 0))
        (if (= src-scc dst-scc)
            adj*
            (let* ([existing (ordered-map-ref adj* src-scc (ordered-map-empty integer-compare))]
                   [updated (ordered-map-set existing dst-scc #t)])
              (ordered-map-set adj* src-scc updated))))))

  ;; SCC successors function
  (define (scc-successors scc-id)
    (define adj-map (ordered-map-ref scc-adj scc-id (ordered-map-empty integer-compare)))
    (list->pvector (ordered-map-keys adj-map)))

  (values node->scc-id sccs scc-successors))

;; ============================================================
;; Utilities
;; ============================================================

;; Get SCC id for a node
;; Parameters:
;;   node->scc-id : ordered-map from condensation-graph
;;   node         : node
;; Returns: integer SCC id
;;
(define (scc-id node->scc-id node)
  (ordered-map-ref node->scc-id node #f))

;; Get members of an SCC
;; Parameters:
;;   scc-nodes : pvector from condensation-graph
;;   scc-id    : integer SCC id
;; Returns: pvector of nodes
;;
(define (scc-members scc-nodes scc-id)
  (pvector-ref scc-nodes scc-id))
