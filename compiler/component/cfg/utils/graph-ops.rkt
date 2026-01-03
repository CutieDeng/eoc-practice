#lang racket/base

;; ============================================================
;; Component: CFG Graph Operations
;; ============================================================
;;
;; Provides graph operation functions for CFG that can be passed
;; to parameterized driver algorithms.
;;
;; This module bridges the kernel CFG types with the driver layer
;; by providing the function signatures that driver algorithms expect.
;; ============================================================

(require racket/match racket/list)
(require "../../../kernel/ir/cfg/main.rkt")
(require "../../../kernel/data/main.rkt")

(provide
  ;; Graph operation closures
  cfg-make-successors
  cfg-make-predecessors

  ;; Direct operations
  cfg-all-block-ids
  cfg-get-block
  cfg-get-entry
  cfg-get-exit

  ;; Terminator helpers
  terminator-successors
  terminator-uses

  ;; Block list accessor
  cfg-blocks->list)

;; ============================================================
;; Graph Operations (Closures for Driver)
;; ============================================================

;; Create a get-successors function for a CFG
;; Returns: BlockId -> (Listof BlockId)
;;
(define (cfg-make-successors cfg)
  (lambda (block-id)
    (define block (cfg-get-block cfg block-id))
    (if block
        (terminator-successors (CfgBlock-terminator block))
        '())))

;; Create a get-predecessors function for a CFG
;; Returns: BlockId -> (Listof BlockId)
;;
(define (cfg-make-predecessors cfg)
  ;; Build predecessor map once
  (define pred-map (make-hash))
  (for ([bid (cfg-all-block-ids cfg)])
    (hash-set! pred-map bid '()))

  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (for ([succ (terminator-successors (CfgBlock-terminator block))])
        (hash-set! pred-map succ
                   (cons bid (hash-ref pred-map succ '()))))))

  (lambda (block-id)
    (hash-ref pred-map block-id '())))

;; ============================================================
;; CFG Accessors
;; ============================================================

;; Get all block IDs from CFG
(define (cfg-all-block-ids cfg)
  (define blocks (Cfg-blocks cfg))
  (if blocks
      (ordered-map-keys blocks)
      '()))

;; Get a block by ID
(define (cfg-get-block cfg block-id)
  (define blocks (Cfg-blocks cfg))
  (and blocks (ordered-map-ref blocks block-id #f)))

;; Get entry block ID
(define (cfg-get-entry cfg)
  (Cfg-entry cfg))

;; Get exit block ID (may be #f)
(define (cfg-get-exit cfg)
  (Cfg-exit cfg))

;; Convert blocks ordered-map to list
(define (cfg-blocks->list cfg)
  (define blocks (Cfg-blocks cfg))
  (if blocks
      (for/list ([bid (ordered-map-keys blocks)])
        (ordered-map-ref blocks bid #f))
      '()))

;; ============================================================
;; Terminator Helpers
;; ============================================================

;; Get successor block IDs from a terminator
(define (terminator-successors term)
  (match term
    [(TermJump target) (list target)]
    [(TermBranch _ then-target else-target)
     (list then-target else-target)]
    [(TermSwitch _ cases default)
     (cons default (map cdr cases))]
    [(TermReturn _) '()]
    [(TermThrow _) '()]
    [(TermUnreachable) '()]
    [_ '()]))

;; Get VarId uses from a terminator
(define (terminator-uses term)
  (match term
    [(TermJump _) '()]
    [(TermBranch cond _ _) (list cond)]
    [(TermSwitch value _ _) (list value)]
    [(TermReturn values) values]
    [(TermThrow exception) (list exception)]
    [(TermUnreachable) '()]
    [_ '()]))
