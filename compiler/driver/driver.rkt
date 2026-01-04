#lang racket/base

;; ============================================================
;; Driver: Unified Export
;; ============================================================
;;
;; The driver layer provides parameterized algorithms that work
;; with any graph or IR representation. All algorithms receive
;; their graph operations as function parameters.
;;
;; Modules:
;;   - graph:     DFS, BFS, topology sort, SCC, reachability
;;   - dataflow:  Generic dataflow framework, lattices
;;   - dominance: Dominator trees, dominance frontier
;;   - loop:      Natural loop detection, loop forest
;;   - worklist:  FIFO, LIFO, priority worklists
;;
;; Design principles:
;;   - Pure algorithms (no concrete IR dependencies)
;;   - Parameterized (graph operations passed as arguments)
;;   - Composable (algorithms build on each other)
;;
;; Usage:
;;   (require "driver/driver.rkt")
;;   ; or for fine-grained imports:
;;   (require "driver/graph/graph.rkt")
;;   (require "driver/dominance/dominance.rkt")
;;
;; ============================================================

(require "graph/graph.rkt")
(require "dataflow/dataflow.rkt")
(require "dominance/dominance.rkt")
(require "loop/loop.rkt")
(require "worklist/worklist.rkt")

(provide (all-from-out "graph/graph.rkt"))
(provide (all-from-out "dataflow/dataflow.rkt"))
(provide (all-from-out "dominance/dominance.rkt"))
(provide (all-from-out "loop/loop.rkt"))
(provide (all-from-out "worklist/worklist.rkt"))
