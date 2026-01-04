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
(require "../../../kernel/ir/cfg/cfg.rkt")
(require "../../../kernel/data/data.rkt")

(provide
  ;; Graph operation closures
  cfg-make-successors
  cfg-make-predecessors

  ;; Direct operations
  cfg-all-block-ids
  cfg-get-block
  cfg-get-entry
  cfg-get-exit

  ;; Info operations
  cfg-get-info
  cfg-set-info
  cfg-update-info

  ;; Block modification
  cfg-set-block
  cfg-block-append-insn
  cfg-block-update-insns

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

;; ============================================================
;; Info Map Operations
;; ============================================================

;; Get a value from the info map
(define (cfg-get-info cfg key [default #f])
  (define info (Cfg-info cfg))
  (if info
      (ordered-map-ref info key default)
      default))

;; Set a value in the info map (functional update)
(define (cfg-set-info cfg key value)
  (define old-info (or (Cfg-info cfg) (ordered-map-empty symbol-compare)))
  (define new-info (ordered-map-set old-info key value))
  (struct-copy Cfg cfg [info new-info]))

;; Update a value in the info map with a function
(define (cfg-update-info cfg key fn [default #f])
  (define old-value (cfg-get-info cfg key default))
  (define new-value (fn old-value))
  (cfg-set-info cfg key new-value))

;; Symbol comparison for info map
(define (symbol-compare a b)
  (cond
    [(symbol<? a b) '<]
    [(symbol<? b a) '>]
    [else '=]))

;; ============================================================
;; Block Modification Operations
;; ============================================================

;; Set/replace a block in the CFG
(define (cfg-set-block cfg block)
  (define bid (CfgBlock-id block))
  (define blocks (or (Cfg-blocks cfg) (ordered-map-empty block-id-compare)))
  (define new-blocks (ordered-map-set blocks bid block))
  (struct-copy Cfg cfg [blocks new-blocks]))

;; BlockId comparison for block map
(define (block-id-compare a b)
  (define a-id (BlockId-id a))
  (define b-id (BlockId-id b))
  (cond
    [(< a-id b-id) '<]
    [(> a-id b-id) '>]
    [else '=]))

;; Append an instruction to a block
(define (cfg-block-append-insn cfg block-id insn)
  (define block (cfg-get-block cfg block-id))
  (when (not block)
    (error 'cfg-block-append-insn "block not found: ~a" block-id))
  (define new-insns (append (CfgBlock-insns block) (list insn)))
  (define new-block (struct-copy CfgBlock block [insns new-insns]))
  (cfg-set-block cfg new-block))

;; Update instructions of a block
(define (cfg-block-update-insns cfg block-id fn)
  (define block (cfg-get-block cfg block-id))
  (when (not block)
    (error 'cfg-block-update-insns "block not found: ~a" block-id))
  (define new-insns (fn (CfgBlock-insns block)))
  (define new-block (struct-copy CfgBlock block [insns new-insns]))
  (cfg-set-block cfg new-block))

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
