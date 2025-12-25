#lang racket/base

;; ============================================================
;; RVSDG 统一导出模块
;; ============================================================
;;
;; 本模块提供 RVSDG 库的统一访问接口
;; 用户只需 (require "rvsdg/main.rkt") 即可使用全部功能
;; ============================================================

;; === Core Definitions ===
(require "core-def.rkt")
(provide (all-from-out "core-def.rkt"))

;; === Raw Layer (底层操作，谨慎使用) ===
(require "raw/region-ctor.rkt")
(require "raw/node-ctor.rkt")
(require "raw/node-ctor2.rkt")
(require "raw/node-dtor.rkt")
(require "raw/node-value.rkt")
(require "raw/wire.rkt")
(require "raw/connect.rkt")
(require "raw/query.rkt")

(provide
  ;; Region 构造
  Region-empty
  ;; 常量
  INPUT-NODE-ID OUTPUT-NODE-ID START-NODE-CNT
  ;; 比较函数 (for advanced users)
  wire-compare input-compare output-compare node-compare symbol-compare
  ;; Info 操作
  rvsdg-raw/get-info
  rvsdg-raw/set-info
  rvsdg-raw/remove-info
  rvsdg-raw/update-info
  ;; 低级 ID 分配
  rvsdg-raw/alloc-node-ids
  rvsdg-raw/alloc-input-ids
  rvsdg-raw/alloc-output-ids
  rvsdg-raw/alloc-wire-ids
  ;; 低级端口绑定
  rvsdg-raw/node-input-set
  rvsdg-raw/node-output-set
  rvsdg-raw/node-input-output-set
  ;; 低级查询
  rvsdg-raw/node-count
  rvsdg-raw/wire-count
  rvsdg-raw/input-count
  rvsdg-raw/output-count
  rvsdg-raw/wire-input
  rvsdg-raw/wire-output
  rvsdg-raw/input-wire
  rvsdg-raw/output-wire
  rvsdg-raw/input-node
  rvsdg-raw/output-node
  rvsdg-raw/node-inputs
  rvsdg-raw/node-outputs
  rvsdg-raw/node-offset
  rvsdg-raw/input-offset
  rvsdg-raw/output-offset
  rvsdg-raw/all-node-ids
  rvsdg-raw/all-wire-ids
  ;; 低级节点值操作
  rvsdg-raw/get-node-value
  rvsdg-raw/set-node-value
  rvsdg-raw/remove-node-value
  ;; 低级连接操作
  rvsdg-raw/wire-input-output-connect
  rvsdg-raw/wire-input-output-disconnect)

;; === Safe Layer (推荐使用) ===
(require "safe/node.rkt")
(require "safe/wire.rkt")
(require "safe/validate.rkt")

(provide
  ;; 节点操作
  rvsdg/create-node
  rvsdg/create-node-with-value
  rvsdg/delete-node
  rvsdg/delete-nodes
  rvsdg/node-exists?
  rvsdg/get-node-inputs
  rvsdg/get-node-outputs
  rvsdg/get-input-port
  rvsdg/get-output-port
  rvsdg/get-node-value
  ;; 连接操作
  rvsdg/connect
  rvsdg/disconnect-input
  rvsdg/disconnect-output
  rvsdg/reconnect-input
  rvsdg/input-connected?
  rvsdg/output-connected?
  rvsdg/wire-exists?
  rvsdg/input-exists?
  rvsdg/output-exists?
  rvsdg/get-input-source
  rvsdg/get-output-target
  rvsdg/get-input-node
  rvsdg/get-output-node
  ;; 验证
  (struct-out ValidationError)
  rvsdg/region-valid?
  rvsdg/validate-region
  rvsdg/assert-valid!)

;; === Semantic Layer (高级操作) ===
(require "semantic/traverse.rkt")
(require "semantic/transform.rkt")
(require "semantic/structured.rkt")

(provide
  ;; 遍历
  rvsdg/all-nodes
  rvsdg/filter-nodes
  rvsdg/find-nodes-by-type
  rvsdg/find-simple-nodes
  rvsdg/all-edges
  rvsdg/graph-size
  ;; 前驱/后继
  rvsdg/node-predecessors
  rvsdg/node-successors
  rvsdg/transitive-predecessors
  rvsdg/transitive-successors
  ;; 拓扑排序
  rvsdg/topological-order
  rvsdg/reverse-topological-order
  ;; 可达性
  rvsdg/reachable?
  rvsdg/reachable-from
  ;; 变换
  rvsdg/clone-node
  rvsdg/clone-nodes
  rvsdg/clone-subgraph
  rvsdg/compute-live-nodes
  rvsdg/eliminate-dead-nodes
  rvsdg/replace-node-outputs
  rvsdg/inline-identity-node
  rvsdg/insert-node-on-edge
  ;; 结构化节点
  rvsdg/create-gamma
  rvsdg/create-if-then-else
  rvsdg/create-theta
  rvsdg/create-lambda
  rvsdg/create-delta
  rvsdg/create-phi
  rvsdg/create-omega
  rvsdg/create-psi
  rvsdg/create-shift
  rvsdg/create-eff-handler
  rvsdg/create-eff-perform
  rvsdg/structured-node?
  rvsdg/simple-node?
  rvsdg/get-sub-regions
  rvsdg/set-sub-region)

;; === Analysis Layer (分析框架) ===
(require "analysis/framework.rkt")

(provide
  ;; 格
  (struct-out Lattice)
  make-set-lattice
  make-constant-lattice
  ;; 数据流分析
  FORWARD BACKWARD
  (struct-out DataflowAnalysis)
  run-dataflow-analysis
  make-liveness-analysis
  make-reaching-definitions-analysis
  ;; 分析状态
  make-analysis-state
  analysis-state-get
  analysis-state-set
  ;; 分析管理
  analysis<%>
  analysis-manager%)
