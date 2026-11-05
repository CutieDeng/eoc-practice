#lang racket/base

;; ============================================================
;; Driver: Strongly Connected Components
;; ============================================================
;;
;; Parameterized SCC algorithms (Kosaraju, Tarjan) using pvector.
;; These do NOT depend on any specific graph representation.
;;
;; Nodes are compared with equal?; per-node bookkeeping uses
;; immutable hashes.  SCC ids are naturals, so SCC-indexed
;; structures use racket/intmap and racket/intbits.
;;
;; ============================================================

(require racket/intmap
         "../../kernel/data/data.rkt")

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
;;   nodes            : pvector of nodes - all nodes
;;   get-successors   : node -> pvector of nodes
;;   get-predecessors : node -> pvector of nodes
;; Returns: pvector of pvector - list of SCCs (each SCC is a pvector of nodes)
;;
(define (scc-kosaraju nodes get-successors get-predecessors)
  ;; Phase 1: DFS on original graph, record finish order
  (define visited (hash))
  (define finish-order (pvector-empty))

  (define (dfs1 node)
    (unless (hash-has-key? visited node)
      (set! visited (hash-set visited node #t))
      (for ([succ (in-pvector (get-successors node))])
        (dfs1 succ))
      (set! finish-order (pvector-cons-right finish-order node))))

  (for ([node (in-pvector nodes)])
    (dfs1 node))

  ;; Phase 2: DFS on reversed graph in reverse finish order
  (set! visited (hash))
  (define sccs (pvector-empty))

  (define (dfs2 node component)
    (cond
      [(hash-has-key? visited node) component]
      [else
       (set! visited (hash-set visited node #t))
       (define new-component (pvector-cons-right component node))
       (for/fold ([comp new-component])
                 ([pred (in-pvector (get-predecessors node))])
         (dfs2 pred comp))]))

  ;; Process in reverse finish order
  (for ([node (in-pvector (pvector-reverse finish-order))])
    (unless (hash-has-key? visited node)
      (define component (dfs2 node (pvector-empty)))
      (set! sccs (pvector-cons-right sccs component))))

  sccs)

;; ============================================================
;; Tarjan's Algorithm
;; ============================================================

;; Find SCCs using Tarjan's algorithm
;; Parameters:
;;   nodes          : pvector of nodes - all nodes
;;   get-successors : node -> pvector of nodes
;; Returns: pvector of pvector - list of SCCs in reverse topological order
;;
(define (scc-tarjan nodes get-successors)
  (define index-counter 0)
  (define index (hash))
  (define lowlink (hash))
  (define on-stack (hash))
  (define stack (pvector-empty))
  (define sccs (pvector-empty))

  (define (strongconnect node)
    ;; Set index and lowlink
    (set! index (hash-set index node index-counter))
    (set! lowlink (hash-set lowlink node index-counter))
    (set! index-counter (+ index-counter 1))

    ;; Push to stack
    (set! stack (pvector-cons-right stack node))
    (set! on-stack (hash-set on-stack node #t))

    ;; Visit successors
    (for ([succ (in-pvector (get-successors node))])
      (cond
        [(not (hash-has-key? index succ))
         ;; Not visited
         (strongconnect succ)
         (set! lowlink
               (hash-set lowlink node
                         (min (hash-ref lowlink node 0)
                              (hash-ref lowlink succ 0))))]
        [(hash-has-key? on-stack succ)
         ;; On stack, part of current SCC
         (set! lowlink
               (hash-set lowlink node
                         (min (hash-ref lowlink node 0)
                              (hash-ref index succ 0))))]))

    ;; If root of SCC, pop and record
    (when (= (hash-ref lowlink node 0)
             (hash-ref index node 0))
      (define-values (scc new-stack)
        (let loop ([component (pvector-empty)] [stk stack])
          (define-values (top stk*) (pvector-pop-right stk))
          (set! on-stack (hash-remove on-stack top))
          (define new-component (pvector-cons-left component top))
          (if (equal? top node)
              (values new-component stk*)
              (loop new-component stk*))))
      (set! stack new-stack)
      (set! sccs (pvector-cons-right sccs scc))))

  (for ([node (in-pvector nodes)])
    (unless (hash-has-key? index node)
      (strongconnect node)))

  sccs)

;; ============================================================
;; Condensation Graph
;; ============================================================

;; Build condensation graph (DAG of SCCs)
;; Parameters:
;;   nodes          : pvector of nodes - all nodes
;;   get-successors : node -> pvector of nodes
;; Returns: (values
;;            node->scc-id   : immutable hash - maps node to SCC id
;;            scc-nodes      : pvector - maps SCC id to pvector of nodes
;;            scc-successors : procedure - scc-id -> pvector of scc-ids (ascending))
;;
(define (condensation-graph nodes get-successors)
  (define sccs (scc-tarjan nodes get-successors))

  ;; Build node -> SCC id mapping
  (define node->scc-id
    (for*/fold ([m (hash)])
               ([(scc id) (in-indexed (in-pvector sccs))]
                [node (in-pvector scc)])
      (hash-set m node id)))

  ;; Build SCC adjacency: intmap scc-id -> intbits of successor scc-ids
  (define scc-adj
    (for*/fold ([adj intmap-empty])
               ([node (in-pvector nodes)]
                [succ (in-pvector (get-successors node))])
      (define src-scc (hash-ref node->scc-id node 0))
      (define dst-scc (hash-ref node->scc-id succ 0))
      (if (= src-scc dst-scc)
          adj
          (intmap-set adj src-scc
                      (intbits-set (intmap-ref adj src-scc intbits-empty) dst-scc)))))

  ;; SCC successors function — pvector output for driver-algorithm compat.
  (define (scc-successors scc-id)
    (for/pvector ([k (in-intbits (intmap-ref scc-adj scc-id intbits-empty))])
      k))

  (values node->scc-id sccs scc-successors))

;; ============================================================
;; Utilities
;; ============================================================

;; Get SCC id for a node
;; Parameters:
;;   node->scc-id : hash from condensation-graph
;;   node         : node
;; Returns: integer SCC id, or #f
;;
(define (scc-id node->scc-id node)
  (hash-ref node->scc-id node #f))

;; Get members of an SCC
;; Parameters:
;;   scc-nodes : pvector from condensation-graph
;;   scc-id    : integer SCC id
;; Returns: pvector of nodes
;;
(define (scc-members scc-nodes scc-id)
  (pvector-ref scc-nodes scc-id))
