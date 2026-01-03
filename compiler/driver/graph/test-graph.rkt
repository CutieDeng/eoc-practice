#lang racket/base

;; ============================================================
;; Driver: Graph Algorithm Tests
;; ============================================================

(require rackunit)
(require "main.rkt")
(require "../../kernel/data/main.rkt")

;; ============================================================
;; Test Graph Setup
;; ============================================================

;; Create a simple directed graph:
;;   0 → 1 → 2
;;   ↓   ↓
;;   3 → 4
;;
(define (make-test-graph)
  (define edges
    '((0 . 1) (0 . 3)
      (1 . 2) (1 . 4)
      (3 . 4)))

  (define adj (ordered-map-empty integer-compare))
  (for ([e edges])
    (define src (car e))
    (define dst (cdr e))
    (define existing (ordered-map-ref adj src (pvector-empty)))
    (set! adj (ordered-map-set adj src (pvector-cons-right existing dst))))

  (lambda (node)
    (ordered-map-ref adj node (pvector-empty))))

;; Create predecessors from edges
(define (make-test-predecessors)
  (define edges
    '((0 . 1) (0 . 3)
      (1 . 2) (1 . 4)
      (3 . 4)))

  (define pred (ordered-map-empty integer-compare))
  (for ([e edges])
    (define src (car e))
    (define dst (cdr e))
    (define existing (ordered-map-ref pred dst (pvector-empty)))
    (set! pred (ordered-map-set pred dst (pvector-cons-right existing src))))

  (lambda (node)
    (ordered-map-ref pred node (pvector-empty))))

;; Test nodes
(define test-nodes (list->pvector '(0 1 2 3 4)))

;; ============================================================
;; Helper functions
;; ============================================================

(define (pvector-contains? pv elem)
  (for/or ([e (in-pvector pv)])
    (equal? e elem)))

(define (pvector-index-of pv elem)
  (for/first ([e (in-pvector pv)]
              [i (in-naturals)]
              #:when (equal? e elem))
    i))

(define (pvector-last pv)
  (pvector-ref pv (- (pvector-length pv) 1)))

;; ============================================================
;; Traversal Tests
;; ============================================================

(define traversal-tests
  (test-suite "Traversal Tests"

    (test-case "dfs-preorder"
      (define get-succ (make-test-graph))
      (define result (dfs-preorder integer-compare get-succ 0))
      (check-equal? (pvector-ref result 0) 0)
      (check-true (pvector-contains? result 1))
      (check-true (pvector-contains? result 2))
      (check-true (pvector-contains? result 3))
      (check-true (pvector-contains? result 4)))

    (test-case "dfs-postorder"
      (define get-succ (make-test-graph))
      (define result (dfs-postorder integer-compare get-succ 0))
      ;; Last visited should be start
      (check-equal? (pvector-last result) 0))

    (test-case "bfs"
      (define get-succ (make-test-graph))
      (define result (bfs integer-compare get-succ 0))
      (check-equal? (pvector-ref result 0) 0)
      ;; Level 1 nodes should come before level 2
      (check-true (< (pvector-index-of result 1) (pvector-index-of result 2)))
      (check-true (< (pvector-index-of result 3) (pvector-index-of result 4))))

    (test-case "reachable-from"
      (define get-succ (make-test-graph))
      (define result (reachable-from integer-compare get-succ 0))
      (check-equal? (pvector-length result) 5))

    (test-case "find-path"
      (define get-succ (make-test-graph))
      (define path (find-path integer-compare get-succ 0 4))
      (check-not-false path)
      (check-equal? (pvector-ref path 0) 0)
      (check-equal? (pvector-last path) 4))))

;; ============================================================
;; Topology Sort Tests
;; ============================================================

(define topology-tests
  (test-suite "Topology Sort Tests"

    (test-case "topology-sort on DAG"
      (define get-succ (make-test-graph))
      (define get-pred (make-test-predecessors))
      (define result (topology-sort integer-compare test-nodes get-succ get-pred))
      (check-not-false result)
      ;; Verify topological order: all edges go forward
      (for ([i (in-range (pvector-length result))])
        (define node (pvector-ref result i))
        (for ([succ (in-pvector (get-succ node))])
          (define succ-idx (pvector-index-of result succ))
          (check-true (< i succ-idx)
                      (format "~a should come before ~a" node succ)))))

    (test-case "topology-sort returns #f for cycle"
      ;; Create a cycle: 0 → 1 → 2 → 0
      (define (get-succ node)
        (case node
          [(0) (list->pvector '(1))]
          [(1) (list->pvector '(2))]
          [(2) (list->pvector '(0))]
          [else (pvector-empty)]))
      (define (get-pred node)
        (case node
          [(0) (list->pvector '(2))]
          [(1) (list->pvector '(0))]
          [(2) (list->pvector '(1))]
          [else (pvector-empty)]))
      (define result (topology-sort integer-compare (list->pvector '(0 1 2)) get-succ get-pred))
      (check-false result))))

;; ============================================================
;; SCC Tests
;; ============================================================

(define scc-tests
  (test-suite "SCC Tests"

    (test-case "scc-kosaraju on DAG"
      (define get-succ (make-test-graph))
      (define get-pred (make-test-predecessors))
      (define sccs (scc-kosaraju integer-compare test-nodes get-succ get-pred))
      ;; DAG has 5 SCCs (each node is its own SCC)
      (check-equal? (pvector-length sccs) 5))

    (test-case "scc-tarjan on DAG"
      (define get-succ (make-test-graph))
      (define sccs (scc-tarjan integer-compare test-nodes get-succ))
      (check-equal? (pvector-length sccs) 5))

    (test-case "scc-tarjan finds cycle"
      ;; Graph with SCC: {0, 1, 2} and singleton {3}
      ;; 0 → 1 → 2 → 0, 0 → 3
      (define (get-succ node)
        (case node
          [(0) (list->pvector '(1 3))]
          [(1) (list->pvector '(2))]
          [(2) (list->pvector '(0))]
          [(3) (pvector-empty)]
          [else (pvector-empty)]))
      (define nodes (list->pvector '(0 1 2 3)))
      (define sccs (scc-tarjan integer-compare nodes get-succ))
      ;; Should have 2 SCCs
      (check-equal? (pvector-length sccs) 2)
      ;; One SCC should have 3 nodes
      (check-true (for/or ([scc (in-pvector sccs)])
                   (= (pvector-length scc) 3))))

    (test-case "condensation-graph"
      ;; Same cycle graph
      (define (get-succ node)
        (case node
          [(0) (list->pvector '(1 3))]
          [(1) (list->pvector '(2))]
          [(2) (list->pvector '(0))]
          [(3) (pvector-empty)]
          [else (pvector-empty)]))
      (define nodes (list->pvector '(0 1 2 3)))
      (define-values (node->scc-id scc-nodes scc-succ) (condensation-graph integer-compare nodes get-succ))

      ;; All cycle nodes should be in same SCC
      (check-equal? (scc-id node->scc-id 0) (scc-id node->scc-id 1))
      (check-equal? (scc-id node->scc-id 1) (scc-id node->scc-id 2))
      ;; Node 3 should be in different SCC
      (check-not-equal? (scc-id node->scc-id 0) (scc-id node->scc-id 3)))))

;; ============================================================
;; Run Tests
;; ============================================================

(module+ test
  (require rackunit/text-ui)
  (run-tests traversal-tests)
  (run-tests topology-tests)
  (run-tests scc-tests))
