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

(require racket/match)
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

  ;; Block sequence accessors
  in-cfg-blocks
  in-cfg-block-ids
  cfg-block-count
  cfg-edge-count
  cfg-fresh-block-id)

;; ============================================================
;; Graph Operations (Closures for Driver)
;; ============================================================

;; Create a get-successors function for a CFG.
;; Returns: BlockId -> pvector[BlockId]
;;
(define (cfg-make-successors cfg)
  (lambda (block-id)
    (define block (cfg-get-block cfg block-id))
    (if block
        (terminator-successors (CfgBlock-terminator block))
        (pvector-empty))))

;; Create a get-predecessors function for a CFG.
;; Returns: BlockId -> pvector[BlockId]
;;
(define (cfg-make-predecessors cfg)
  ;; Build predecessor map once (idmap keyed by BlockId).
  (define pred-map
    (for/fold ([m idmap-empty])
              ([bid (in-list (cfg-all-block-ids cfg))])
      (idmap-set m bid (pvector-empty))))

  (define pred-map*
    (for/fold ([m pred-map])
              ([bid (in-list (cfg-all-block-ids cfg))])
      (define block (cfg-get-block cfg bid))
      (cond
        [(not block) m]
        [else
         (for/fold ([m m])
                   ([succ (in-pvector (terminator-successors
                                        (CfgBlock-terminator block)))])
           (define cur (idmap-ref m succ (pvector-empty)))
           (idmap-set m succ (pvector-cons-right cur bid)))])))

  (lambda (block-id)
    (idmap-ref pred-map* block-id (pvector-empty))))

;; ============================================================
;; CFG Accessors
;; ============================================================

;; Get all block IDs from CFG
(define (cfg-all-block-ids cfg)
  (define blocks (Cfg-blocks cfg))
  (if blocks
      (idmap-keys blocks)
      '()))

;; Get a block by ID
(define (cfg-get-block cfg block-id)
  (define blocks (Cfg-blocks cfg))
  (and blocks (idmap-ref blocks block-id #f)))

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
      (hash-ref info key default)
      default))

;; Set a value in the info map (functional update)
(define (cfg-set-info cfg key value)
  (define old-info (or (Cfg-info cfg) (hash)))
  (define new-info (hash-set old-info key value))
  (struct-copy Cfg cfg [info new-info]))

;; Update a value in the info map with a function
(define (cfg-update-info cfg key fn [default #f])
  (define old-value (cfg-get-info cfg key default))
  (define new-value (fn old-value))
  (cfg-set-info cfg key new-value))

;; ============================================================
;; Block Modification Operations
;; ============================================================

;; Set/replace a block in the CFG
(define (cfg-set-block cfg block)
  (define bid (CfgBlock-id block))
  (define blocks (or (Cfg-blocks cfg) idmap-empty))
  (define new-blocks (idmap-set blocks bid block))
  (struct-copy Cfg cfg [blocks new-blocks]))

;; Append an instruction to a block
(define (cfg-block-append-insn cfg block-id insn)
  (define block (cfg-get-block cfg block-id))
  (when (not block)
    (error 'cfg-block-append-insn "block not found: ~a" block-id))
  (define new-insns (pvector-cons-right (CfgBlock-insns block) insn))
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

;; Sequence of all blocks in the CFG (empty sequence if none).
(define (in-cfg-blocks cfg)
  (define blocks (Cfg-blocks cfg))
  (if blocks
      (in-idmap-values blocks)
      (in-list '())))

;; Sequence of all block ids in the CFG (empty sequence if none).
(define (in-cfg-block-ids cfg)
  (define blocks (Cfg-blocks cfg))
  (if blocks
      (in-idmap-keys blocks)
      (in-list '())))

;; Number of blocks.
(define (cfg-block-count cfg)
  (define blocks (Cfg-blocks cfg))
  (if blocks (idmap-count blocks) 0))

;; Number of control-flow edges, counting one edge per terminator
;; successor slot (so a Term:cond with both arms to the same block
;; counts 2).
(define (cfg-edge-count cfg)
  (for/sum ([blk (in-cfg-blocks cfg)])
    (pvector-length (terminator-successors (CfgBlock-terminator blk)))))

;; Allocate a fresh BlockId.  Returns (values cfg* bid); the caller
;; is responsible for eventually inserting a block under bid.
(define (cfg-fresh-block-id cfg)
  (define n (Cfg-block-cnt cfg))
  (values (struct-copy Cfg cfg [block-cnt (add1 n)])
          (BlockId n)))

;; ============================================================
;; Terminator Helpers
;; ============================================================

;; Get successor block IDs from a terminator.
;; Returns: pvector[BlockId]
(define (terminator-successors term)
  (match term
    [(Term:jump target) (pvector target)]
    [(Term:cond _ then-target else-target)
     (pvector then-target else-target)]
    [(Term:switch _ cases default)
     ;; cases is pvector[(Pairof Integer BlockId)].
     (pvector-cons-left (pvector-map cases cdr) default)]
    [(Term:ret _) (pvector-empty)]
    [(Term:throw _) (pvector-empty)]
    [(Term:unreachable) (pvector-empty)]
    [_ (pvector-empty)]))

;; Get VarId uses from a terminator.
;; Returns: pvector[VarId]
(define (terminator-uses term)
  (match term
    [(Term:jump _) (pvector-empty)]
    [(Term:cond cond _ _) (pvector cond)]
    [(Term:switch value _ _) (pvector value)]
    [(Term:ret values) values]          ; already pvector
    [(Term:throw exception) (pvector exception)]
    [(Term:unreachable) (pvector-empty)]
    [_ (pvector-empty)]))
