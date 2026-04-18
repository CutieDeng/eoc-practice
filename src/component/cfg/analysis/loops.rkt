#lang racket/base

;; ============================================================
;; Component: CFG Loop Analysis
;; ============================================================
;;
;; Computes loop structure information for CFG.
;; Uses the driver loop analysis bound to CFG types.
;;
;; ============================================================

(require racket/set)
(require "../../../kernel/ir/cfg/cfg.rkt")
(require (except-in "../../../kernel/data/data.rkt" integer-compare))
(require "../../../driver/loop/loop.rkt")
(require "../../../driver/dominance/dominance.rkt")
(require "../utils/graph-ops.rkt")
(require "../../common/common.rkt")

(provide
  ;; Analysis
  cfg-loop-analysis

  ;; Result structure
  (struct-out CfgLoopInfo)

  ;; Convenience functions
  cfg-find-loops
  cfg-find-back-edges
  cfg-find-loop-headers
  cfg-get-loop-depth
  cfg-get-innermost-loop)

;; ============================================================
;; Loop Result Structure
;; ============================================================

(struct CfgLoopInfo (
  loops           ; (Listof LoopInfo) - all natural loops
  back-edges      ; (Listof (Cons BlockId BlockId)) - (tail . header)
  headers         ; Set[BlockId] - loop headers
  parent-map      ; Hash[BlockId -> BlockId or #f] - loop nesting
  children-map    ; Hash[BlockId -> (Listof BlockId)] - loop children
) #:transparent)

;; ============================================================
;; Loop Analysis Implementation
;; ============================================================

;; Build a pvector of block ids from a CFG.
(define (cfg-block-ids-pv cfg)
  (for/pvector ([bid (in-cfg-block-ids cfg)]) bid))

(define (compute-cfg-loops cfg ctx)
  (define block-ids (cfg-block-ids-pv cfg))
  (define entry (cfg-get-entry cfg))
  (define get-succs (cfg-make-successors cfg))
  (define get-preds (cfg-make-predecessors cfg))

  ;; Use driver algorithms
  (define back-edges
    (find-back-edges block-id-compare block-ids entry get-succs get-preds))

  (define loops
    (find-natural-loops block-id-compare block-ids entry get-succs get-preds))

  (define headers
    (find-loop-headers block-id-compare block-ids entry get-succs get-preds))

  ;; Build loop forest with proper depths
  (define-values (updated-loops parent-map children-map)
    (if (null? loops)
        (values '() (hash) (hash))
        (build-loop-forest block-id-compare loops)))

  (AnalysisResult
    (CfgLoopInfo updated-loops back-edges headers parent-map children-map)
    'natural-loops
    (hash 'loops (length loops)
          'back-edges (length back-edges))
    #t))

;; ============================================================
;; Analysis Registration
;; ============================================================

(define cfg-loop-analysis
  (make-analysis 'cfg-loops
    "Compute loop structure for CFG"
    (hash 'default compute-cfg-loops)
    #:default 'default
    #:dependencies '()
    #:invalidated-by '(cfg-structure)))

(register-analysis! cfg-loop-analysis)

;; ============================================================
;; Convenience Functions
;; ============================================================

;; Find all loops in a CFG
(define (cfg-find-loops cfg)
  (define block-ids (cfg-block-ids-pv cfg))
  (define entry (cfg-get-entry cfg))
  (define get-succs (cfg-make-successors cfg))
  (define get-preds (cfg-make-predecessors cfg))
  (find-natural-loops block-id-compare block-ids entry get-succs get-preds))

;; Find all back edges in a CFG
(define (cfg-find-back-edges cfg)
  (define block-ids (cfg-block-ids-pv cfg))
  (define entry (cfg-get-entry cfg))
  (define get-succs (cfg-make-successors cfg))
  (define get-preds (cfg-make-predecessors cfg))
  (find-back-edges block-id-compare block-ids entry get-succs get-preds))

;; Find all loop headers in a CFG
(define (cfg-find-loop-headers cfg)
  (define block-ids (cfg-block-ids-pv cfg))
  (define entry (cfg-get-entry cfg))
  (define get-succs (cfg-make-successors cfg))
  (define get-preds (cfg-make-predecessors cfg))
  (find-loop-headers block-id-compare block-ids entry get-succs get-preds))

;; Get the nesting depth of a block (0 = not in loop)
;;
(define (cfg-get-loop-depth loop-info block-id)
  (define loops (CfgLoopInfo-loops loop-info))
  (define loop (get-innermost-loop block-id loops))
  (if loop
      (+ 1 (loop-info-depth loop))
      0))

;; Get the innermost loop containing a block
;; Returns: LoopInfo or #f
;;
(define (cfg-get-innermost-loop loop-info block-id)
  (define loops (CfgLoopInfo-loops loop-info))
  (get-innermost-loop block-id loops))
