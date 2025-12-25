#lang racket/base

;; ============================================================
;; RVSDG 基础测试
;; ============================================================

(require rackunit)
(require "../core-def.rkt")
(require "../raw/region-ctor.rkt")
(require "../raw/node-ctor.rkt")
(require "../raw/node-ctor2.rkt")
(require "../raw/node-value.rkt")
(require "../raw/wire.rkt")
(require "../raw/connect.rkt")
(require "../raw/query.rkt")
(require "../safe/node.rkt")
(require "../safe/wire.rkt")
(require "../safe/validate.rkt")

;; ============================================================
;; Raw Layer Tests
;; ============================================================

(define raw-tests
  (test-suite
   "Raw Layer Tests"

   (test-case "Region-empty creates valid empty region"
     (define r (Region-empty))
     (check-equal? (rvsdg-raw/node-count r) 2)  ; START-NODE-CNT
     (check-equal? (rvsdg-raw/wire-count r) 0)
     (check-equal? (rvsdg-raw/input-count r) 0)
     (check-equal? (rvsdg-raw/output-count r) 0))

   (test-case "Node ID allocation"
     (define r (Region-empty))
     (define-values (id1 r1) (rvsdg-raw/alloc-node-ids r 1))
     (define-values (id2 r2) (rvsdg-raw/alloc-node-ids r1 1))
     (check-equal? (NodeId-id id1) 2)
     (check-equal? (NodeId-id id2) 3)
     (check-equal? (rvsdg-raw/node-count r2) 4))

   (test-case "Batch node ID allocation"
     (define r (Region-empty))
     (define-values (base-id r1) (rvsdg-raw/alloc-node-ids r 5))
     (check-equal? (NodeId-id base-id) 2)
     (check-equal? (rvsdg-raw/node-count r1) 7)
     ;; 验证偏移
     (check-equal? (NodeId-id (rvsdg-raw/node-offset base-id 0)) 2)
     (check-equal? (NodeId-id (rvsdg-raw/node-offset base-id 4)) 6))

   (test-case "Input/Output ID allocation"
     (define r (Region-empty))
     (define-values (in-id r1) (rvsdg-raw/alloc-input-ids r 3))
     (define-values (out-id r2) (rvsdg-raw/alloc-output-ids r1 2))
     (check-equal? (InputId-id in-id) 0)
     (check-equal? (OutputId-id out-id) 0)
     (check-equal? (rvsdg-raw/input-count r2) 3)
     (check-equal? (rvsdg-raw/output-count r2) 2))

   (test-case "Wire connection and disconnection"
     (define r (Region-empty))
     ;; 分配端口
     (define-values (in-id r1) (rvsdg-raw/alloc-input-ids r 1))
     (define-values (out-id r2) (rvsdg-raw/alloc-output-ids r1 1))
     ;; 分配并连接 wire
     (define-values (wire-id r3) (rvsdg-raw/alloc-wire-ids r2 1))
     (define r4 (rvsdg-raw/wire-input-output-connect r3 wire-id in-id out-id))
     ;; 验证连接
     (check-equal? (rvsdg-raw/wire-input r4 wire-id) in-id)
     (check-equal? (rvsdg-raw/wire-output r4 wire-id) out-id)
     (check-equal? (rvsdg-raw/input-wire r4 in-id) wire-id)
     (check-equal? (rvsdg-raw/output-wire r4 out-id) wire-id)
     ;; 断开连接
     (define r5 (rvsdg-raw/wire-input-output-disconnect r4 wire-id in-id out-id))
     (check-false (rvsdg-raw/wire-input r5 wire-id))
     (check-false (rvsdg-raw/input-wire r5 in-id)))

   (test-case "Info operations"
     (define r (Region-empty))
     ;; 设置 info
     (define r1 (rvsdg-raw/set-info r 'test-key 'test-value))
     (check-equal? (rvsdg-raw/get-info r1 'test-key) 'test-value)
     ;; 更新 info
     (define r2 (rvsdg-raw/set-info r1 'counter 0))
     (define r3 (rvsdg-raw/update-info r2 'counter add1 0))
     (check-equal? (rvsdg-raw/get-info r3 'counter) 1)
     ;; 删除 info
     (define r4 (rvsdg-raw/remove-info r3 'test-key))
     (check-false (rvsdg-raw/get-info r4 'test-key))
     ;; 多个 key
     (define r5 (rvsdg-raw/set-info r4 'alpha 'a))
     (define r6 (rvsdg-raw/set-info r5 'beta 'b))
     (check-equal? (rvsdg-raw/get-info r6 'alpha) 'a)
     (check-equal? (rvsdg-raw/get-info r6 'beta) 'b)
     (check-equal? (rvsdg-raw/get-info r6 'counter) 1))

   (test-case "Node value operations"
     (define r (Region-empty))
     (define-values (node-id r1) (rvsdg-raw/alloc-node-ids r 1))
     ;; 设置值
     (define r2 (rvsdg-raw/set-node-value r1 node-id (Simple '+)))
     (check-true (Simple? (rvsdg-raw/get-node-value r2 node-id)))
     (check-equal? (Simple-op (rvsdg-raw/get-node-value r2 node-id)) '+)
     ;; 删除值
     (define r3 (rvsdg-raw/remove-node-value r2 node-id))
     (check-false (rvsdg-raw/get-node-value r3 node-id)))))

;; ============================================================
;; Safe Layer Tests
;; ============================================================

(define safe-tests
  (test-suite
   "Safe Layer Tests"

   (test-case "Create node with value"
     (define r (Region-empty))
     (define-values (node-id in-id out-id r1)
       (rvsdg/create-node-with-value r 2 1 (Simple '+)))
     ;; 验证节点存在
     (check-true (rvsdg/node-exists? r1 node-id))
     ;; 验证端口
     (define-values (in-id^ in-cnt) (rvsdg/get-node-inputs r1 node-id))
     (define-values (out-id^ out-cnt) (rvsdg/get-node-outputs r1 node-id))
     (check-equal? in-id in-id^)
     (check-equal? in-cnt 2)
     (check-equal? out-id out-id^)
     (check-equal? out-cnt 1)
     ;; 验证值
     (check-true (Simple? (rvsdg/get-node-value r1 node-id))))

   (test-case "Connect nodes"
     (define r (Region-empty))
     ;; 创建两个节点
     (define-values (n1 _in1 out1 r1)
       (rvsdg/create-node-with-value r 0 1 (Simple 'const)))
     (define-values (n2 in2 _out2 r2)
       (rvsdg/create-node-with-value r1 1 1 (Simple 'neg)))
     ;; 连接
     (define-values (wire-id r3)
       (rvsdg/connect r2 out1 in2))
     ;; 验证连接
     (check-true (rvsdg/input-connected? r3 in2))
     (check-equal? (rvsdg/get-input-source r3 in2) out1)
     (check-equal? (rvsdg/get-output-target r3 out1) in2))

   (test-case "Delete node disconnects wires"
     (define r (Region-empty))
     ;; 创建并连接节点
     (define-values (n1 _in1 out1 r1)
       (rvsdg/create-node-with-value r 0 1 (Simple 'a)))
     (define-values (n2 in2 out2 r2)
       (rvsdg/create-node-with-value r1 1 1 (Simple 'b)))
     (define-values (n3 in3 _out3 r3)
       (rvsdg/create-node-with-value r2 1 0 (Simple 'c)))
     (define-values (_w1 r4) (rvsdg/connect r3 out1 in2))
     (define-values (_w2 r5) (rvsdg/connect r4 out2 in3))
     ;; 删除中间节点
     (define r6 (rvsdg/delete-node r5 n2))
     ;; 验证节点被删除
     (check-false (rvsdg/node-exists? r6 n2))
     ;; 验证连接被断开
     (check-false (rvsdg/input-connected? r6 in3)))

   (test-case "Get input/output ports"
     (define r (Region-empty))
     (define-values (node-id in-id out-id r1)
       (rvsdg/create-node r 3 2))
     ;; 获取各个端口
     (check-equal? (rvsdg/get-input-port r1 node-id 0)
                   (rvsdg-raw/input-offset in-id 0))
     (check-equal? (rvsdg/get-input-port r1 node-id 2)
                   (rvsdg-raw/input-offset in-id 2))
     (check-equal? (rvsdg/get-output-port r1 node-id 0)
                   (rvsdg-raw/output-offset out-id 0))
     (check-equal? (rvsdg/get-output-port r1 node-id 1)
                   (rvsdg-raw/output-offset out-id 1))
     ;; 越界检查
     (check-exn exn:fail? (lambda () (rvsdg/get-input-port r1 node-id 3)))
     (check-exn exn:fail? (lambda () (rvsdg/get-output-port r1 node-id 2))))))

;; ============================================================
;; Validation Tests
;; ============================================================

(define validation-tests
  (test-suite
   "Validation Tests"

   (test-case "Empty region is valid"
     (define r (Region-empty))
     (check-true (rvsdg/region-valid? r)))

   (test-case "Region with nodes is valid"
     (define r (Region-empty))
     (define-values (_n _in _out r1)
       (rvsdg/create-node-with-value r 2 1 (Simple '+)))
     (check-true (rvsdg/region-valid? r1)))

   (test-case "Region with connected nodes is valid"
     (define r (Region-empty))
     (define-values (n1 _in1 out1 r1)
       (rvsdg/create-node-with-value r 0 1 (Simple 'a)))
     (define-values (n2 in2 _out2 r2)
       (rvsdg/create-node-with-value r1 1 0 (Simple 'b)))
     (define-values (_w r3) (rvsdg/connect r2 out1 in2))
     (check-true (rvsdg/region-valid? r3)))))

;; ============================================================
;; Integration Tests
;; ============================================================

(define integration-tests
  (test-suite
   "Integration Tests"

   (test-case "Build simple expression: (a + b)"
     (define r (Region-empty))

     ;; 创建参数节点
     (define-values (a-id _a-in a-out r1)
       (rvsdg/create-node-with-value r 0 1 (Simple 'arg-a)))
     (define-values (b-id _b-in b-out r2)
       (rvsdg/create-node-with-value r1 0 1 (Simple 'arg-b)))

     ;; 创建加法节点
     (define-values (add-id add-in add-out r3)
       (rvsdg/create-node-with-value r2 2 1 (Simple '+)))

     ;; 连接
     (define add-in-0 (rvsdg/get-input-port r3 add-id 0))
     (define add-in-1 (rvsdg/get-input-port r3 add-id 1))
     (define-values (_w1 r4) (rvsdg/connect r3 a-out add-in-0))
     (define-values (_w2 r5) (rvsdg/connect r4 b-out add-in-1))

     ;; 验证
     (check-true (rvsdg/region-valid? r5))
     (check-equal? (rvsdg/get-input-source r5 add-in-0) a-out)
     (check-equal? (rvsdg/get-input-source r5 add-in-1) b-out))

   (test-case "Build chain: a -> neg -> add(_, b) -> result"
     (define r (Region-empty))

     ;; 创建节点
     (define-values (a-id _a-in a-out r1)
       (rvsdg/create-node-with-value r 0 1 (Simple 'a)))
     (define-values (b-id _b-in b-out r2)
       (rvsdg/create-node-with-value r1 0 1 (Simple 'b)))
     (define-values (neg-id neg-in neg-out r3)
       (rvsdg/create-node-with-value r2 1 1 (Simple 'neg)))
     (define-values (add-id add-in add-out r4)
       (rvsdg/create-node-with-value r3 2 1 (Simple '+)))

     ;; 连接链
     (define-values (_w1 r5) (rvsdg/connect r4 a-out neg-in))
     (define add-in-0 (rvsdg/get-input-port r5 add-id 0))
     (define add-in-1 (rvsdg/get-input-port r5 add-id 1))
     (define-values (_w2 r6) (rvsdg/connect r5 neg-out add-in-0))
     (define-values (_w3 r7) (rvsdg/connect r6 b-out add-in-1))

     ;; 验证整个图
     (check-true (rvsdg/region-valid? r7))

     ;; 验证数据流
     (check-equal? (rvsdg/get-input-source r7 neg-in) a-out)
     (check-equal? (rvsdg/get-input-source r7 add-in-0) neg-out)
     (check-equal? (rvsdg/get-input-source r7 add-in-1) b-out))))

;; ============================================================
;; Run Tests
;; ============================================================

(module+ test
  (require rackunit/text-ui)
  (run-tests raw-tests)
  (run-tests safe-tests)
  (run-tests validation-tests)
  (run-tests integration-tests))

(module+ main
  (require rackunit/text-ui)
  (displayln "Running RVSDG tests...")
  (displayln "\n=== Raw Layer Tests ===")
  (run-tests raw-tests)
  (displayln "\n=== Safe Layer Tests ===")
  (run-tests safe-tests)
  (displayln "\n=== Validation Tests ===")
  (run-tests validation-tests)
  (displayln "\n=== Integration Tests ===")
  (run-tests integration-tests)
  (displayln "\nAll tests completed."))
