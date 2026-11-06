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
(require "../../../kernel/data/data.rkt")
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
;; Dom (driver/dominance/dom.rkt) for the kernel Cfg; unreachable blocks
;; are absent from every map
(define (cfg-dom cfg)
  (compute-dom (cfg-block-ids-pv cfg) (cfg-get-entry cfg) (cfg-make-successors cfg)))

;; the CfgDominanceInfo views of a Dom
(define (dom->info d)
  (define nodes (dom-rpo d))
  (define idom (for/hash ([b (in-pvector nodes)]) (values b (dom-idom-node d b))))
  (define dom-tree (for/hash ([b (in-pvector nodes)]) (values b (dom-children-nodes d b))))
  (define frontier (for/hash ([b (in-pvector nodes)])
                     (values b (for/hash ([f (in-pvector (dom-frontier-nodes d b))]) (values f #t)))))
  ;; dominator sets = ancestors-or-self in the dominator tree
  (define dominators
    (for/hash ([b (in-pvector nodes)])
      (values b (let loop ([x b] [acc (hash)])
                  (define acc* (hash-set acc x #t))
                  (define up (dom-idom-node d x))
                  (if up (loop up acc*) acc*)))))
  (CfgDominanceInfo dominators idom dom-tree frontier))

(define (compute-cfg-dominance cfg ctx)
  (define d (cfg-dom cfg))
  (AnalysisResult
    (dom->info d)
    'cfg-dominance
    (hash 'blocks (dom-size d))
    #t))

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
(define (cfg-compute-dominators cfg) (CfgDominanceInfo-dominators (dom->info (cfg-dom cfg))))
(define (cfg-compute-idom cfg) (CfgDominanceInfo-idom (dom->info (cfg-dom cfg))))
(define (cfg-compute-dominator-tree cfg) (CfgDominanceInfo-dom-tree (dom->info (cfg-dom cfg))))
(define (cfg-compute-dominance-frontier cfg) (CfgDominanceInfo-dom-frontier (dom->info (cfg-dom cfg))))

;; post-dominators: idom map of the reversed graph from the exit (#f without one)
(define (cfg-compute-post-idom cfg)
  (define exit (cfg-get-exit cfg))
  (and exit
       (let ([d (compute-post-dom (cfg-block-ids-pv cfg) exit (cfg-make-successors cfg))])
         (for/hash ([b (in-pvector (dom-rpo d))]) (values b (dom-idom-node d b))))))
