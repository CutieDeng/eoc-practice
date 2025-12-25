#lang racket/base

;; ============================================================
;; Analysis Layer 测试
;; ============================================================

(require rackunit)
(require racket/set)
(require racket/list)
(require racket/class)
(require "../core-def.rkt")
(require "../raw/region-ctor.rkt")
(require "../raw/node-ctor.rkt")
(require "../safe/node.rkt")
(require "../safe/wire.rkt")
(require "../analysis/framework.rkt")

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

;; ============================================================
;; Lattice 测试
;; ============================================================

(define lattice-tests
  (test-suite
   "Lattice Tests"

   (test-case "set-lattice join"
     (define lat (make-set-lattice #f))
     (define join (Lattice-join lat))
     (check-equal? (join (set 1 2) (set 2 3)) (set 1 2 3))
     (check-equal? (join (set) (set 1)) (set 1)))

   (test-case "set-lattice meet"
     (define lat (make-set-lattice #f))
     (define meet (Lattice-meet lat))
     (check-equal? (meet (set 1 2) (set 2 3)) (set 2))
     (check-equal? (meet (set 1) (set 2)) (set)))

   (test-case "set-lattice leq"
     (define lat (make-set-lattice #f))
     (define leq? (Lattice-leq? lat))
     (check-true (leq? (set) (set 1)))
     (check-true (leq? (set 1) (set 1 2)))
     (check-false (leq? (set 1 2) (set 1))))

   (test-case "constant-lattice join"
     (define lat (make-constant-lattice))
     (define join (Lattice-join lat))
     (check-equal? (join 'bottom 5) 5)
     (check-equal? (join 5 'bottom) 5)
     (check-equal? (join 5 5) 5)
     (check-equal? (join 5 6) 'top)
     (check-equal? (join 'top 5) 'top))

   (test-case "constant-lattice leq"
     (define lat (make-constant-lattice))
     (define leq? (Lattice-leq? lat))
     (check-true (leq? 'bottom 5))
     (check-true (leq? 5 'top))
     (check-true (leq? 5 5))
     (check-false (leq? 5 6))
     (check-false (leq? 'top 5)))))

;; ============================================================
;; Dataflow Analysis 测试
;; ============================================================

(define dataflow-tests
  (test-suite
   "Dataflow Analysis Tests"

   (test-case "reaching-definitions on chain"
     (define-values (a b c r) (build-chain-graph))
     (define analysis (make-reaching-definitions-analysis))
     (define result (run-dataflow-analysis r analysis))

     ;; a 节点：到达定义只有自己
     (check-true (set-member? (analysis-state-get result a (set)) a))

     ;; b 节点：到达定义有 a 和 b
     (define b-defs (analysis-state-get result b (set)))
     (check-true (set-member? b-defs a))
     (check-true (set-member? b-defs b))

     ;; c 节点：到达定义有 a, b, c
     (define c-defs (analysis-state-get result c (set)))
     (check-true (set-member? c-defs a))
     (check-true (set-member? c-defs b))
     (check-true (set-member? c-defs c)))

   (test-case "analysis-state operations"
     (define state (make-analysis-state))
     (define state2 (analysis-state-set state 'node1 42))
     (check-equal? (analysis-state-get state2 'node1 0) 42)
     (check-equal? (analysis-state-get state2 'node2 0) 0))))

;; ============================================================
;; Analysis Manager 测试
;; ============================================================

(define manager-tests
  (test-suite
   "Analysis Manager Tests"

   (test-case "register and run analysis"
     (define-values (a b c r) (build-chain-graph))
     (define manager (new analysis-manager% [region r]))

     (send manager register 'reaching-defs (make-reaching-definitions-analysis))
     (define result (send manager run-analysis 'reaching-defs))

     (check-true (hash? result))
     (check-true (set? (hash-ref result a (set)))))

   (test-case "get-result"
     (define-values (a b c r) (build-chain-graph))
     (define manager (new analysis-manager% [region r]))

     (send manager register 'reaching-defs (make-reaching-definitions-analysis))

     ;; 获取单个节点的结果
     (define a-result (send manager get-result 'reaching-defs a))
     (check-true (set? a-result))
     (check-true (set-member? a-result a)))

   (test-case "invalidate cache"
     (define-values (a b c r) (build-chain-graph))
     (define manager (new analysis-manager% [region r]))

     (send manager register 'reaching-defs (make-reaching-definitions-analysis))

     ;; 运行分析
     (send manager run-analysis 'reaching-defs)

     ;; 使缓存失效
     (send manager invalidate 'reaching-defs)

     ;; 应该重新运行
     (define result (send manager run-analysis 'reaching-defs))
     (check-true (hash? result)))))

;; ============================================================
;; 运行测试
;; ============================================================

(module+ test
  (require rackunit/text-ui)
  (run-tests lattice-tests)
  (run-tests dataflow-tests)
  (run-tests manager-tests))

(module+ main
  (require rackunit/text-ui)
  (displayln "Running Analysis Layer tests...")
  (displayln "\n=== Lattice Tests ===")
  (run-tests lattice-tests)
  (displayln "\n=== Dataflow Tests ===")
  (run-tests dataflow-tests)
  (displayln "\n=== Manager Tests ===")
  (run-tests manager-tests)
  (displayln "\nAll analysis tests completed."))
