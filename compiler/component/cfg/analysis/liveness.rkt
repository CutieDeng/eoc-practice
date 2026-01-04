#lang racket/base

;; ============================================================
;; Component: CFG Liveness Analysis
;; ============================================================
;;
;; Computes variable liveness information for CFG.
;; Uses the driver dataflow framework bound to CFG types.
;;
;; ============================================================

(require racket/list racket/set racket/match)
(require "../../../kernel/ir/cfg/cfg.rkt")
(require "../../../driver/dataflow/dataflow.rkt")
(require "../utils/graph-ops.rkt")
(require "../../common/common.rkt")

(provide
  ;; Analysis
  cfg-liveness-analysis

  ;; Result structure
  (struct-out CfgLivenessInfo)

  ;; Convenience functions
  cfg-compute-liveness
  cfg-live-at-entry?
  cfg-live-at-exit?
  cfg-get-live-in
  cfg-get-live-out)

;; ============================================================
;; Liveness Result Structure
;; ============================================================

(struct CfgLivenessInfo (
  live-in       ; Hash[BlockId -> Set[VarId]] - live at block entry
  live-out      ; Hash[BlockId -> Set[VarId]] - live at block exit
  def           ; Hash[BlockId -> Set[VarId]] - defined in block
  use           ; Hash[BlockId -> Set[VarId]] - used before def in block
) #:transparent)

;; ============================================================
;; Def/Use Set Computation
;; ============================================================

;; Compute def and use sets for each block
(define (compute-def-use-sets cfg)
  (define def-map (make-hash))
  (define use-map (make-hash))

  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (define def-set (mutable-set))
    (define use-set (mutable-set))

    (when block
      ;; PHI nodes: outputs are defs, inputs are uses
      (for ([phi (CfgBlock-phis block)])
        (when (PhiInsn? phi)
          (set-add! def-set (PhiInsn-output phi))
          (for ([src (PhiInsn-sources phi)])
            (define v (cdr src))
            (when (and (VarId? v) (not (set-member? def-set v)))
              (set-add! use-set v)))))

      ;; Instructions: process in order
      (for ([insn (CfgBlock-insns block)])
        (when (VfInsn? insn)
          ;; Uses before defs
          (for ([input (VfInsn-inputs insn)])
            (when (and (VarId? input) (not (set-member? def-set input)))
              (set-add! use-set input)))
          ;; Then defs
          (for ([out (VfInsn-outputs insn)])
            (set-add! def-set out))))

      ;; Terminator uses
      (for ([v (terminator-uses (CfgBlock-terminator block))])
        (when (and (VarId? v) (not (set-member? def-set v)))
          (set-add! use-set v))))

    (hash-set! def-map bid (for/set ([x def-set]) x))
    (hash-set! use-map bid (for/set ([x use-set]) x)))

  (values def-map use-map))

;; ============================================================
;; Liveness Analysis (Backward Dataflow)
;; ============================================================

(define (compute-cfg-liveness-backward cfg ctx)
  (define block-ids (cfg-all-block-ids cfg))
  (define get-succs (cfg-make-successors cfg))
  (define get-preds (cfg-make-predecessors cfg))

  ;; Compute def and use sets for each block
  (define-values (def-map use-map) (compute-def-use-sets cfg))

  ;; Initialize live-out sets (empty for all blocks)
  (define live-out (make-hash))
  (define live-in (make-hash))
  (for ([bid block-ids])
    (hash-set! live-out bid (set))
    (hash-set! live-in bid (set)))

  ;; Worklist algorithm (backward)
  (define worklist (reverse block-ids))
  (define iterations 0)

  (let loop ()
    (unless (null? worklist)
      (set! iterations (+ iterations 1))
      (define bid (car worklist))
      (set! worklist (cdr worklist))

      ;; live-out[B] = ∪ live-in[S] for all successors S
      (define succs (get-succs bid))
      (define new-out
        (for/fold ([out (set)])
                  ([s succs])
          (set-union out (hash-ref live-in s (set)))))

      ;; Always update live-out (computed from successors' live-in)
      (hash-set! live-out bid new-out)

      ;; live-in[B] = use[B] ∪ (live-out[B] - def[B])
      (define use-set (hash-ref use-map bid (set)))
      (define def-set (hash-ref def-map bid (set)))
      (define new-in (set-union use-set
                                (set-subtract new-out def-set)))

      ;; If live-in changed, add predecessors to worklist
      (unless (equal? new-in (hash-ref live-in bid (set)))
        (hash-set! live-in bid new-in)
        (define preds (get-preds bid))
        (set! worklist (append preds worklist)))

      (loop)))

  (AnalysisResult
    (CfgLivenessInfo live-in live-out def-map use-map)
    'backward-dataflow
    (hash 'iterations iterations)
    #t))

;; ============================================================
;; Analysis Registration
;; ============================================================

(define cfg-liveness-analysis
  (make-analysis 'cfg-liveness
    "Compute variable liveness for CFG"
    (hash 'backward compute-cfg-liveness-backward)
    #:default 'backward
    #:dependencies '()
    #:invalidated-by '(cfg-structure)))

(register-analysis! cfg-liveness-analysis)

;; ============================================================
;; Convenience Functions
;; ============================================================

;; Compute liveness with default algorithm
(define (cfg-compute-liveness cfg)
  (define ctx (make-analysis-context))
  (define result (run-analysis 'cfg-liveness cfg ctx))
  (AnalysisResult-data result))

;; Check if variable is live at block entry
(define (cfg-live-at-entry? live-info var bid)
  (set-member? (hash-ref (CfgLivenessInfo-live-in live-info) bid (set)) var))

;; Check if variable is live at block exit
(define (cfg-live-at-exit? live-info var bid)
  (set-member? (hash-ref (CfgLivenessInfo-live-out live-info) bid (set)) var))

;; Get all live variables at block entry
(define (cfg-get-live-in live-info bid)
  (set->list (hash-ref (CfgLivenessInfo-live-in live-info) bid (set))))

;; Get all live variables at block exit
(define (cfg-get-live-out live-info bid)
  (set->list (hash-ref (CfgLivenessInfo-live-out live-info) bid (set))))
