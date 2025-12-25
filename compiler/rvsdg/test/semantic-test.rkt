#lang racket/base

;; ============================================================
;; Semantic Layer 测试
;; ============================================================

(require rackunit)
(require racket/set)
(require racket/list)
(require "../core-def.rkt")
(require "../raw/region-ctor.rkt")
(require "../raw/node-ctor.rkt")
(require "../safe/node.rkt")
(require "../safe/wire.rkt")
(require "../semantic/traverse.rkt")
(require "../semantic/transform.rkt")
(require "../semantic/structured.rkt")

;; 辅助函数：构建测试图
;; a -> b -> c (链式结构)
(define (build-chain-graph)
  (define r (Region-empty))
  (define-values (a _a-in a-out r1)
    (rvsdg/create-node-with-value r 0 1 (Simple 'a)))
  (define-values (b b-in b-out r2)
    (rvsdg/create-node-with-value r1 1 1 (Simple 'b)))
  (define-values (c c-in _c-out r3)
    (rvsdg/create-node-with-value r2 1 0 (Simple 'c)))
  (define-values (_w1 r4) (rvsdg/connect r3 a-out b-in))
  (define-values (_w2 r5) (rvsdg/connect r4 b-out c-in))
  (values a b c r5))

;; 辅助函数：构建菱形图
;;     a
;;    / \
;;   b   c
;;    \ /
;;     d
(define (build-diamond-graph)
  (define r (Region-empty))
  (define-values (a _a-in a-out r1)
    (rvsdg/create-node-with-value r 0 2 (Simple 'a)))
  (define-values (b b-in b-out r2)
    (rvsdg/create-node-with-value r1 1 1 (Simple 'b)))
  (define-values (c c-in c-out r3)
    (rvsdg/create-node-with-value r2 1 1 (Simple 'c)))
  (define-values (d d-in _d-out r4)
    (rvsdg/create-node-with-value r3 2 0 (Simple 'd)))

  (define a-out-0 (rvsdg/get-output-port r4 a 0))
  (define a-out-1 (rvsdg/get-output-port r4 a 1))
  (define d-in-0 (rvsdg/get-input-port r4 d 0))
  (define d-in-1 (rvsdg/get-input-port r4 d 1))

  (define-values (_w1 r5) (rvsdg/connect r4 a-out-0 b-in))
  (define-values (_w2 r6) (rvsdg/connect r5 a-out-1 c-in))
  (define-values (_w3 r7) (rvsdg/connect r6 b-out d-in-0))
  (define-values (_w4 r8) (rvsdg/connect r7 c-out d-in-1))

  (values a b c d r8))

;; ============================================================
;; Traverse 测试
;; ============================================================

(define traverse-tests
  (test-suite
   "Traverse Tests"

   (test-case "all-nodes returns all nodes"
     (define-values (a b c r) (build-chain-graph))
     (define nodes (rvsdg/all-nodes r))
     (check-equal? (length nodes) 3)
     (check-not-false (member a nodes))
     (check-not-false (member b nodes))
     (check-not-false (member c nodes)))

   (test-case "node-predecessors"
     (define-values (a b c r) (build-chain-graph))
     (check-equal? (rvsdg/node-predecessors r a) (set))
     (check-equal? (rvsdg/node-predecessors r b) (set a))
     (check-equal? (rvsdg/node-predecessors r c) (set b)))

   (test-case "node-successors"
     (define-values (a b c r) (build-chain-graph))
     (check-equal? (rvsdg/node-successors r a) (set b))
     (check-equal? (rvsdg/node-successors r b) (set c))
     (check-equal? (rvsdg/node-successors r c) (set)))

   (test-case "topological-order chain"
     (define-values (a b c r) (build-chain-graph))
     (define order (rvsdg/topological-order r))
     ;; a 必须在 b 之前，b 必须在 c 之前
     (check-true (< (index-of order a) (index-of order b)))
     (check-true (< (index-of order b) (index-of order c))))

   (test-case "topological-order diamond"
     (define-values (a b c d r) (build-diamond-graph))
     (define order (rvsdg/topological-order r))
     ;; a 在 b, c 之前；b, c 在 d 之前
     (check-true (< (index-of order a) (index-of order b)))
     (check-true (< (index-of order a) (index-of order c)))
     (check-true (< (index-of order b) (index-of order d)))
     (check-true (< (index-of order c) (index-of order d))))

   (test-case "transitive-predecessors"
     (define-values (a b c r) (build-chain-graph))
     (check-equal? (rvsdg/transitive-predecessors r c) (set a b)))

   (test-case "transitive-successors"
     (define-values (a b c r) (build-chain-graph))
     (check-equal? (rvsdg/transitive-successors r a) (set b c)))

   (test-case "reachable?"
     (define-values (a b c r) (build-chain-graph))
     (check-true (rvsdg/reachable? r a b))
     (check-true (rvsdg/reachable? r a c))
     (check-false (rvsdg/reachable? r c a)))

   (test-case "find-simple-nodes"
     (define-values (a b c r) (build-chain-graph))
     (check-equal? (rvsdg/find-simple-nodes r 'b) (list b)))))

;; ============================================================
;; Transform 测试
;; ============================================================

(define transform-tests
  (test-suite
   "Transform Tests"

   (test-case "clone-node"
     (define-values (a b c r) (build-chain-graph))
     (define-values (a-clone r^) (rvsdg/clone-node r a))
     ;; 克隆的节点应该存在且有相同的值
     (check-true (rvsdg/node-exists? r^ a-clone))
     (check-equal? (Simple-op (rvsdg/get-node-value r^ a-clone)) 'a)
     ;; 克隆的节点不应该有连接
     (check-equal? (rvsdg/node-successors r^ a-clone) (set)))

   (test-case "clone-subgraph"
     (define-values (a b c r) (build-chain-graph))
     (define-values (mapping r^) (rvsdg/clone-subgraph r (list a b)))
     (define a^ (hash-ref mapping a))
     (define b^ (hash-ref mapping b))
     ;; 内部连接应该被复制
     (check-equal? (rvsdg/node-successors r^ a^) (set b^)))

   (test-case "compute-live-nodes"
     (define-values (a b c r) (build-chain-graph))
     (define live (rvsdg/compute-live-nodes r (list c)))
     (check-true (set-member? live a))
     (check-true (set-member? live b))
     (check-true (set-member? live c)))))

;; ============================================================
;; Structured 测试
;; ============================================================

(define structured-tests
  (test-suite
   "Structured Node Tests"

   (test-case "create-gamma"
     (define r (Region-empty))
     (define then-region (Region-empty))
     (define else-region (Region-empty))
     (define-values (node-id _in _out r^)
       (rvsdg/create-if-then-else r then-region else-region 2 1))
     (check-true (rvsdg/node-exists? r^ node-id))
     (check-true (Gamma? (rvsdg/get-node-value r^ node-id)))
     (check-true (rvsdg/structured-node? r^ node-id)))

   (test-case "create-theta"
     (define r (Region-empty))
     (define body-region (Region-empty))
     (define-values (node-id _in _out r^)
       (rvsdg/create-theta r 2 2 body-region))
     (check-true (Theta? (rvsdg/get-node-value r^ node-id))))

   (test-case "create-lambda"
     (define r (Region-empty))
     (define body-region (Region-empty))
     (define-values (node-id _in _out r^)
       (rvsdg/create-lambda r 0 body-region))
     (check-true (Lambda? (rvsdg/get-node-value r^ node-id))))

   (test-case "get-sub-regions"
     (define r (Region-empty))
     (define then-region (Region-empty))
     (define else-region (Region-empty))
     (define-values (node-id _in _out r^)
       (rvsdg/create-if-then-else r then-region else-region 1 1))
     (define regions (rvsdg/get-sub-regions r^ node-id))
     (check-equal? (length regions) 2))))

;; ============================================================
;; 运行测试
;; ============================================================

(module+ test
  (require rackunit/text-ui)
  (run-tests traverse-tests)
  (run-tests transform-tests)
  (run-tests structured-tests))

(module+ main
  (require rackunit/text-ui)
  (displayln "Running Semantic Layer tests...")
  (displayln "\n=== Traverse Tests ===")
  (run-tests traverse-tests)
  (displayln "\n=== Transform Tests ===")
  (run-tests transform-tests)
  (displayln "\n=== Structured Tests ===")
  (run-tests structured-tests)
  (displayln "\nAll semantic tests completed."))
