#lang racket/base

;; ============================================================
;; Component: CFG Traversal Utilities
;; ============================================================
;;
;; Provides traversal operations for CFG transformations.
;; Uses the driver graph algorithms bound to CFG types.
;;
;; ============================================================

(require racket/set)
(require "../../../kernel/ir/cfg/cfg.rkt")
(require (except-in "../../../kernel/data/data.rkt" integer-compare))
(require "../../../driver/graph/graph.rkt")
(require "../utils/graph-ops.rkt")

(provide
  ;; Traversal orders
  cfg-dfs-preorder
  cfg-dfs-postorder
  cfg-reverse-postorder
  cfg-bfs-order

  ;; Reachability
  cfg-reachable-blocks
  cfg-unreachable-blocks

  ;; Path queries
  cfg-find-path
  cfg-all-paths)

;; ============================================================
;; Traversal Orders
;; ============================================================

;; Get blocks in DFS preorder
;; Returns: pvector[BlockId]
;;
(define (cfg-dfs-preorder cfg)
  (define entry (cfg-get-entry cfg))
  (define get-succs (cfg-make-successors cfg))
  (dfs-preorder block-id-compare get-succs entry))

;; Get blocks in DFS postorder
;; Returns: pvector[BlockId]
;;
(define (cfg-dfs-postorder cfg)
  (define entry (cfg-get-entry cfg))
  (define get-succs (cfg-make-successors cfg))
  (dfs-postorder block-id-compare get-succs entry))

;; Get blocks in reverse postorder (topological for acyclic CFG)
;; Returns: pvector[BlockId]
;;
(define (cfg-reverse-postorder cfg)
  (define entry (cfg-get-entry cfg))
  (define get-succs (cfg-make-successors cfg))
  (dfs-reverse-postorder block-id-compare get-succs entry))

;; Get blocks in BFS order
;; Returns: pvector[BlockId]
;;
(define (cfg-bfs-order cfg)
  (define entry (cfg-get-entry cfg))
  (define get-succs (cfg-make-successors cfg))
  (bfs block-id-compare get-succs entry))

;; ============================================================
;; Reachability
;; ============================================================

;; Get all blocks reachable from entry
;; Returns: pvector[BlockId]
;;
(define (cfg-reachable-blocks cfg)
  (define entry (cfg-get-entry cfg))
  (define get-succs (cfg-make-successors cfg))
  (reachable-from block-id-compare get-succs entry))

;; Get all unreachable blocks
;; Returns: pvector[BlockId]
;;
(define (cfg-unreachable-blocks cfg)
  (define all-blocks (cfg-all-block-ids cfg))
  (define reachable
    (for/fold ([s (set)]) ([bid (in-pvector (cfg-reachable-blocks cfg))])
      (set-add s bid)))
  (for/pvector ([bid (in-list all-blocks)]
                #:unless (set-member? reachable bid))
    bid))

;; ============================================================
;; Path Queries
;; ============================================================

;; Find a path between two blocks
;; Returns: pvector[BlockId] or #f
;;
(define (cfg-find-path cfg from-block to-block)
  (define get-succs (cfg-make-successors cfg))
  (find-path block-id-compare get-succs from-block to-block))

;; Find all paths between two blocks
;; Returns: pvector[pvector[BlockId]]
;;
(define (cfg-all-paths cfg from-block to-block)
  (define get-succs (cfg-make-successors cfg))
  (all-paths block-id-compare get-succs from-block to-block))
