#lang racket/base

;; ============================================================
;; Component: CFG Traversal Utilities
;; ============================================================
;;
;; Provides traversal operations for CFG transformations.
;; Uses the driver graph algorithms bound to CFG types.
;;
;; ============================================================

(require racket/list racket/set)
(require "../../../kernel/ir/cfg/main.rkt")
(require "../../../kernel/data/main.rkt")
(require "../../../driver/graph/main.rkt")
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
;; Returns: (Listof BlockId)
;;
(define (cfg-dfs-preorder cfg)
  (define entry (cfg-get-entry cfg))
  (define get-succs (cfg-make-successors cfg))
  (dfs-preorder get-succs entry))

;; Get blocks in DFS postorder
;; Returns: (Listof BlockId)
;;
(define (cfg-dfs-postorder cfg)
  (define entry (cfg-get-entry cfg))
  (define get-succs (cfg-make-successors cfg))
  (dfs-postorder get-succs entry))

;; Get blocks in reverse postorder (topological for acyclic CFG)
;; Returns: (Listof BlockId)
;;
(define (cfg-reverse-postorder cfg)
  (define entry (cfg-get-entry cfg))
  (define get-succs (cfg-make-successors cfg))
  (dfs-reverse-postorder get-succs entry))

;; Get blocks in BFS order
;; Returns: (Listof BlockId)
;;
(define (cfg-bfs-order cfg)
  (define entry (cfg-get-entry cfg))
  (define get-succs (cfg-make-successors cfg))
  (bfs get-succs entry))

;; ============================================================
;; Reachability
;; ============================================================

;; Get all blocks reachable from entry
;; Returns: (Listof BlockId)
;;
(define (cfg-reachable-blocks cfg)
  (define entry (cfg-get-entry cfg))
  (define get-succs (cfg-make-successors cfg))
  (reachable-from get-succs entry))

;; Get all unreachable blocks
;; Returns: (Listof BlockId)
;;
(define (cfg-unreachable-blocks cfg)
  (define all-blocks (cfg-all-block-ids cfg))
  (define reachable (list->set (cfg-reachable-blocks cfg)))
  (filter (lambda (bid) (not (set-member? reachable bid)))
          all-blocks))

;; ============================================================
;; Path Queries
;; ============================================================

;; Find a path between two blocks
;; Returns: (Listof BlockId) or #f
;;
(define (cfg-find-path cfg from-block to-block)
  (define get-succs (cfg-make-successors cfg))
  (find-path get-succs from-block to-block))

;; Find all paths between two blocks
;; Returns: (Listof (Listof BlockId))
;;
(define (cfg-all-paths cfg from-block to-block)
  (define get-succs (cfg-make-successors cfg))
  (all-paths get-succs from-block to-block))
