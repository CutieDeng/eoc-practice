#lang racket/base

;; ============================================================
;; Component: CFG Dominance Analysis
;; ============================================================
;;
;; Computes dominance information for CFG using driver algorithms.
;; Binds the parameterized driver/dominance module to CFG types.
;;
;; ============================================================

(require racket/set)
(require "../../../kernel/ir/cfg/cfg.rkt")
(require (except-in "../../../kernel/data/data.rkt" integer-compare))
(require "../../../driver/dominance/dominance.rkt")
(require "../../../driver/graph/graph.rkt")
(require "../utils/graph-ops.rkt")
(require "../../common/common.rkt")

(provide
  ;; Analysis
  cfg-dominance-analysis

  ;; Result structure
  (struct-out CfgDominanceInfo)

  ;; Convenience functions
  cfg-compute-dominators
  cfg-compute-idom
  cfg-compute-dominator-tree
  cfg-compute-dominance-frontier

  ;; Post-dominance
  cfg-compute-post-idom)

;; ============================================================
;; Dominance Result Structure
;; ============================================================

(struct CfgDominanceInfo (
  dominators      ; Hash[BlockId -> Set[BlockId]]
  idom            ; Hash[BlockId -> BlockId or #f]
  dom-tree        ; Hash[BlockId -> (Listof BlockId)]
  dom-frontier    ; Hash[BlockId -> Set[BlockId]]
) #:transparent)

;; ============================================================
;; Dominance Analysis Implementation
;; ============================================================

;; Build a pvector of block ids from a CFG.
(define (cfg-block-ids-pv cfg)
  (for/pvector ([bid (in-cfg-block-ids cfg)]) bid))

;; Compute dominance info for a CFG
(define (compute-cfg-dominance cfg ctx)
  (define block-ids (cfg-block-ids-pv cfg))
  (define entry (cfg-get-entry cfg))
  (define get-preds (cfg-make-predecessors cfg))

  ;; Use driver algorithms
  (define dom (compute-dominators block-id-compare block-ids entry get-preds))
  (define idom (compute-idom block-id-compare block-ids entry get-preds))
  (define dom-tree (compute-dominator-tree block-id-compare block-ids idom))
  (define dom-frontier (compute-dominance-frontier block-id-compare
                                                   block-ids entry get-preds
                                                   #:idom idom))

  (AnalysisResult
    (CfgDominanceInfo dom idom dom-tree dom-frontier)
    'cfg-dominance
    (hash 'blocks (pvector-length block-ids))
    #t))

;; ============================================================
;; Analysis Registration
;; ============================================================

(define cfg-dominance-analysis
  (make-analysis 'cfg-dominance
    "Compute dominance information for CFG"
    (hash 'default compute-cfg-dominance)
    #:default 'default
    #:dependencies '()
    #:invalidated-by '(cfg-structure)))

(register-analysis! cfg-dominance-analysis)

;; ============================================================
;; Convenience Functions
;; ============================================================

;; Compute dominators for a CFG
(define (cfg-compute-dominators cfg)
  (define block-ids (cfg-block-ids-pv cfg))
  (define entry (cfg-get-entry cfg))
  (define get-preds (cfg-make-predecessors cfg))
  (compute-dominators block-id-compare block-ids entry get-preds))

;; Compute immediate dominators for a CFG
(define (cfg-compute-idom cfg)
  (define block-ids (cfg-block-ids-pv cfg))
  (define entry (cfg-get-entry cfg))
  (define get-preds (cfg-make-predecessors cfg))
  (compute-idom block-id-compare block-ids entry get-preds))

;; Compute dominator tree for a CFG
(define (cfg-compute-dominator-tree cfg)
  (define block-ids (cfg-block-ids-pv cfg))
  (define idom (cfg-compute-idom cfg))
  (compute-dominator-tree block-id-compare block-ids idom))

;; Compute dominance frontier for a CFG
(define (cfg-compute-dominance-frontier cfg)
  (define block-ids (cfg-block-ids-pv cfg))
  (define entry (cfg-get-entry cfg))
  (define get-preds (cfg-make-predecessors cfg))
  (compute-dominance-frontier block-id-compare block-ids entry get-preds))

;; Compute post-immediate dominators for a CFG
(define (cfg-compute-post-idom cfg)
  (define block-ids (cfg-block-ids-pv cfg))
  (define exit (cfg-get-exit cfg))
  (define get-succs (cfg-make-successors cfg))
  (when exit
    (compute-post-idom block-id-compare block-ids exit get-succs)))
