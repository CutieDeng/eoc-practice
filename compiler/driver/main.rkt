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
;;   (require "driver/main.rkt")
;;   ; or for fine-grained imports:
;;   (require "driver/graph/main.rkt")
;;   (require "driver/dominance/main.rkt")
;;
;; ============================================================

(require "graph/main.rkt")
(require "dataflow/main.rkt")
(require "dominance/main.rkt")
(require "loop/main.rkt")
(require "worklist/main.rkt")

(provide (all-from-out "graph/main.rkt"))
(provide (all-from-out "dataflow/main.rkt"))
(provide (all-from-out "dominance/main.rkt"))
(provide (all-from-out "loop/main.rkt"))
(provide (all-from-out "worklist/main.rkt"))
