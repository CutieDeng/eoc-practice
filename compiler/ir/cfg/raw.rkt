#lang racket/base

;; ============================================================
;; IR Layer: CFG Raw Operations
;; ============================================================
;;
;; Basic operations for manipulating CFGs.
;; ============================================================

(require racket/dict racket/match)
(require "../../lib/main.rkt")
(require "types.rkt")

;; === Comparison Functions ===

(define (block-id-compare a b)
  (integer-compare (BlockId-id a) (BlockId-id b)))

(define (var-id-compare a b)
  (integer-compare (VarId-id a) (VarId-id b)))

(define (insn-id-compare a b)
  (integer-compare (InsnId-id a) (InsnId-id b)))

(provide block-id-compare var-id-compare insn-id-compare)

;; === Empty CFG ===

(define (cfg-empty)
  (Cfg
    0                                      ; block-cnt
    0                                      ; var-cnt
    0                                      ; insn-cnt
    #f                                     ; entry
    #f                                     ; exit
    (ordl-make-empty block-id-compare)    ; blocks
    (ordl-make-empty symbol-compare)))    ; info

(provide cfg-empty)

;; === ID Allocation ===

(define (cfg-alloc-block-id cfg)
  (define id (BlockId (Cfg-block-cnt cfg)))
  (define cfg^ (struct-copy Cfg cfg [block-cnt (+ 1 (Cfg-block-cnt cfg))]))
  (values id cfg^))

(define (cfg-alloc-block-ids cfg n)
  (define base-id (BlockId (Cfg-block-cnt cfg)))
  (define cfg^ (struct-copy Cfg cfg [block-cnt (+ n (Cfg-block-cnt cfg))]))
  (values base-id cfg^))

(define (cfg-alloc-var-id cfg)
  (define id (VarId (Cfg-var-cnt cfg)))
  (define cfg^ (struct-copy Cfg cfg [var-cnt (+ 1 (Cfg-var-cnt cfg))]))
  (values id cfg^))

(define (cfg-alloc-var-ids cfg n)
  (define base-id (VarId (Cfg-var-cnt cfg)))
  (define cfg^ (struct-copy Cfg cfg [var-cnt (+ n (Cfg-var-cnt cfg))]))
  (values base-id cfg^))

(define (cfg-alloc-insn-id cfg)
  (define id (InsnId (Cfg-insn-cnt cfg)))
  (define cfg^ (struct-copy Cfg cfg [insn-cnt (+ 1 (Cfg-insn-cnt cfg))]))
  (values id cfg^))

(define (cfg-alloc-insn-ids cfg n)
  (define base-id (InsnId (Cfg-insn-cnt cfg)))
  (define cfg^ (struct-copy Cfg cfg [insn-cnt (+ n (Cfg-insn-cnt cfg))]))
  (values base-id cfg^))

(provide cfg-alloc-block-id cfg-alloc-block-ids)
(provide cfg-alloc-var-id cfg-alloc-var-ids)
(provide cfg-alloc-insn-id cfg-alloc-insn-ids)

;; === Block Operations ===

(define (cfg-get-block cfg block-id)
  (dict-ref (Cfg-blocks cfg) block-id #f))

(define (cfg-set-block cfg block)
  (define blocks^ (dict-set (Cfg-blocks cfg) (CfgBlock-id block) block))
  (struct-copy Cfg cfg [blocks blocks^]))

(define (cfg-remove-block cfg block-id)
  (define blocks^ (dict-remove (Cfg-blocks cfg) block-id))
  (struct-copy Cfg cfg [blocks blocks^]))

(define (cfg-has-block? cfg block-id)
  (dict-has-key? (Cfg-blocks cfg) block-id))

(provide cfg-get-block cfg-set-block cfg-remove-block cfg-has-block?)

;; === Entry/Exit Operations ===

(define (cfg-set-entry cfg block-id)
  (struct-copy Cfg cfg [entry block-id]))

(define (cfg-get-entry cfg)
  (Cfg-entry cfg))

(define (cfg-set-exit cfg block-id)
  (struct-copy Cfg cfg [exit block-id]))

(define (cfg-get-exit cfg)
  (Cfg-exit cfg))

(provide cfg-set-entry cfg-get-entry cfg-set-exit cfg-get-exit)

;; === Info Operations ===

(define (cfg-get-info cfg key)
  (dict-ref (Cfg-info cfg) key #f))

(define (cfg-set-info cfg key value)
  (define info^ (dict-set (Cfg-info cfg) key value))
  (struct-copy Cfg cfg [info info^]))

(define (cfg-remove-info cfg key)
  (define info^ (dict-remove (Cfg-info cfg) key))
  (struct-copy Cfg cfg [info info^]))

(define (cfg-update-info cfg key f default)
  (define old-value (or (cfg-get-info cfg key) default))
  (cfg-set-info cfg key (f old-value)))

(provide cfg-get-info cfg-set-info cfg-remove-info cfg-update-info)

;; === Block Creation ===

(define (cfg-create-block cfg)
  (define-values (id cfg^) (cfg-alloc-block-id cfg))
  (define block (CfgBlock id '() '() (TermUnreachable)))
  (define cfg^^ (cfg-set-block cfg^ block))
  (values id cfg^^))

(define (cfg-create-block-with-terminator cfg terminator)
  (define-values (id cfg^) (cfg-alloc-block-id cfg))
  (define block (CfgBlock id '() '() terminator))
  (define cfg^^ (cfg-set-block cfg^ block))
  (values id cfg^^))

(provide cfg-create-block cfg-create-block-with-terminator)

;; === Block Content Operations ===

(define (cfg-block-append-insn cfg block-id insn)
  (define block (cfg-get-block cfg block-id))
  (unless block (error 'cfg-block-append-insn "Block not found: ~a" block-id))
  (define block^ (struct-copy CfgBlock block
                   [insns (append (CfgBlock-insns block) (list insn))]))
  (cfg-set-block cfg block^))

(define (cfg-block-set-terminator cfg block-id terminator)
  (define block (cfg-get-block cfg block-id))
  (unless block (error 'cfg-block-set-terminator "Block not found: ~a" block-id))
  (define block^ (struct-copy CfgBlock block [terminator terminator]))
  (cfg-set-block cfg block^))

(define (cfg-block-add-phi cfg block-id phi)
  (define block (cfg-get-block cfg block-id))
  (unless block (error 'cfg-block-add-phi "Block not found: ~a" block-id))
  (define block^ (struct-copy CfgBlock block
                   [phis (append (CfgBlock-phis block) (list phi))]))
  (cfg-set-block cfg block^))

(provide cfg-block-append-insn cfg-block-set-terminator cfg-block-add-phi)

;; === Traversal ===

(define (cfg-all-block-ids cfg)
  (for/list ([(k v) (in-dict (Cfg-blocks cfg))]) k))

(define (cfg-block-count cfg)
  (dict-count (Cfg-blocks cfg)))

(provide cfg-all-block-ids cfg-block-count)

;; === Terminator Analysis ===

(define (terminator-successors term)
  (match term
    [(TermJump target) (list target)]
    [(TermBranch _ then-target else-target) (list then-target else-target)]
    [(TermSwitch _ cases default)
     (cons default (map cdr cases))]
    [(TermReturn _) '()]
    [(TermThrow _) '()]
    [(TermUnreachable) '()]))

(define (terminator-uses term)
  (match term
    [(TermJump _) '()]
    [(TermBranch cond _ _) (list cond)]
    [(TermSwitch value _ _) (list value)]
    [(TermReturn values) values]
    [(TermThrow exc) (list exc)]
    [(TermUnreachable) '()]))

(provide terminator-successors terminator-uses)
