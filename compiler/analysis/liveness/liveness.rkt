#lang racket/base

;; ============================================================
;; Liveness Analysis
;; ============================================================
;;
;; Computes variable liveness information in a CFG.
;;
;; A variable V is live at program point P if there exists a path
;; from P to a use of V that does not pass through a definition of V.
;;
;; Provides multiple algorithms:
;;   1. Backward dataflow (classic iterative)
;;   2. SSA-based (fast for SSA form)
;;   3. Linear scan (for register allocation)
;;
;; References:
;;   - Aho et al. "Compilers: Principles, Techniques, and Tools"
;;   - Appel "Modern Compiler Implementation"
;; ============================================================

(require racket/match racket/list racket/set racket/hash)
(require "../framework.rkt")
(require "../../ir/cfg/main.rkt")
(require "../../lib/main.rkt")

;; ============================================================
;; Liveness Result Structure
;; ============================================================

;; Complete liveness information
(struct LivenessInfo (
  live-in       ; Hash[BlockId -> Set[VarId]] - live at block entry
  live-out      ; Hash[BlockId -> Set[VarId]] - live at block exit
  def           ; Hash[BlockId -> Set[VarId]] - defined in block
  use           ; Hash[BlockId -> Set[VarId]] - used before def in block
  intervals     ; Hash[VarId -> (Listof Interval)] - live intervals (optional)
) #:transparent)

;; Live interval for register allocation
(struct Interval (
  var           ; VarId
  start         ; Integer - start position
  end           ; Integer - end position
  uses          ; (Listof Integer) - use positions
) #:transparent)

(provide (struct-out LivenessInfo) (struct-out Interval))

;; ============================================================
;; Algorithm 1: Classic Backward Dataflow
;; ============================================================
;;
;; Standard backward dataflow analysis.
;; Time: O(n × k) where k is iteration count
;; Space: O(n × v) where v is number of variables

(define (compute-liveness-backward cfg ctx)
  (define block-ids (cfg-all-block-ids cfg))

  ;; Compute def and use sets for each block
  (define-values (def-map use-map) (compute-def-use-sets cfg))

  ;; Build successor map for backward analysis
  (define succ-map (build-successor-map cfg))

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
      (define succs (hash-ref succ-map bid '()))
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
        (define preds (get-predecessors-from-succ succ-map bid block-ids))
        (set! worklist (append preds worklist)))

      (loop)))

  (AnalysisResult
   (LivenessInfo live-in live-out def-map use-map (hash))
   'backward-dataflow
   (hash 'iterations iterations)
   #t))

;; ============================================================
;; Algorithm 2: SSA-Based Liveness
;; ============================================================
;;
;; Faster algorithm exploiting SSA properties.
;; Each variable is defined exactly once, simplifying analysis.
;; Time: O(n + e) where e is number of edges
;; Space: O(n × v)

(define (compute-liveness-ssa cfg ctx)
  (define block-ids (cfg-all-block-ids cfg))

  ;; Build def-use chains from SSA form
  (define var->def-block (make-hash))
  (define var->use-blocks (make-hash))

  ;; Collect definitions
  (for ([bid block-ids])
    (define block (cfg-get-block cfg bid))
    (when block
      ;; PHI definitions
      (for ([phi (CfgBlock-phis block)])
        (when (PhiInsn? phi)
          (hash-set! var->def-block (PhiInsn-output phi) bid)))
      ;; Instruction definitions
      (for ([insn (CfgBlock-insns block)])
        (when (VfInsn? insn)
          (for ([out (VfInsn-outputs insn)])
            (hash-set! var->def-block out bid))))))

  ;; Collect uses
  (for ([bid block-ids])
    (define block (cfg-get-block cfg bid))
    (when block
      (define (record-use! var)
        (when (VarId? var)
          (hash-set! var->use-blocks var
                     (set-add (hash-ref var->use-blocks var (set)) bid))))

      ;; PHI uses
      (for ([phi (CfgBlock-phis block)])
        (when (PhiInsn? phi)
          (for ([src (PhiInsn-sources phi)])
            (record-use! (cdr src)))))
      ;; Instruction uses
      (for ([insn (CfgBlock-insns block)])
        (when (VfInsn? insn)
          (for ([input (VfInsn-inputs insn)])
            (record-use! input))))
      ;; Terminator uses
      (for ([v (terminator-uses (CfgBlock-terminator block))])
        (record-use! v))))

  ;; For each variable, compute live range via upward exposed uses
  (define live-in (make-hash))
  (define live-out (make-hash))
  (for ([bid block-ids])
    (hash-set! live-in bid (set))
    (hash-set! live-out bid (set)))

  ;; For each variable, propagate liveness from uses to def
  (define succ-map (build-successor-map cfg))
  (define pred-map (build-predecessor-map cfg))

  (for ([(var use-blocks) (in-hash var->use-blocks)])
    (define def-block (hash-ref var->def-block var #f))
    (when def-block
      ;; BFS from use blocks back to def block
      (define visited (mutable-set))
      (define worklist (set->list use-blocks))

      (let loop ()
        (unless (null? worklist)
          (define bid (car worklist))
          (set! worklist (cdr worklist))

          (unless (set-member? visited bid)
            (set-add! visited bid)

            ;; Variable is live-in at this block (unless it's the def block)
            (unless (equal? bid def-block)
              (hash-set! live-in bid
                         (set-add (hash-ref live-in bid (set)) var))
              ;; Also live-out at predecessors
              (for ([p (hash-ref pred-map bid '())])
                (hash-set! live-out p
                           (set-add (hash-ref live-out p (set)) var))
                (set! worklist (cons p worklist)))))

          (loop)))))

  ;; Compute def/use sets
  (define-values (def-map use-map) (compute-def-use-sets cfg))

  (AnalysisResult
   (LivenessInfo live-in live-out def-map use-map (hash))
   'ssa-based
   (hash 'variables (hash-count var->def-block))
   #t))

;; ============================================================
;; Algorithm 3: Linear Scan (for Register Allocation)
;; ============================================================
;;
;; Computes live intervals for linear scan register allocation.
;; Time: O(n)
;; Space: O(v)

(define (compute-liveness-linear-scan cfg ctx)
  ;; First compute basic liveness
  (define basic-result (compute-liveness-backward cfg ctx))
  (define basic-info (AnalysisResult-data basic-result))

  ;; Compute linear order of blocks (reverse post-order)
  (define entry (cfg-get-entry cfg))
  (define linear-order (compute-linear-order cfg entry))

  ;; Assign positions to instructions
  (define pos-map (make-hash))
  (define current-pos 0)

  (for ([bid linear-order])
    (define block (cfg-get-block cfg bid))
    (when block
      ;; Position for block start
      (hash-set! pos-map (cons 'block-start bid) current-pos)
      (set! current-pos (+ current-pos 2))

      ;; Positions for instructions
      (for ([insn (CfgBlock-insns block)])
        (when (VfInsn? insn)
          (hash-set! pos-map insn current-pos)
          (set! current-pos (+ current-pos 2))))))

  ;; Compute intervals for each variable
  (define intervals (make-hash))

  ;; Collect all variables
  (define all-vars (mutable-set))
  (for ([bid (cfg-all-block-ids cfg)])
    (for ([v (hash-ref (LivenessInfo-def basic-info) bid (set))])
      (set-add! all-vars v))
    (for ([v (hash-ref (LivenessInfo-use basic-info) bid (set))])
      (set-add! all-vars v)))

  ;; For each variable, compute its interval
  (for ([var (in-set all-vars)])
    (define start +inf.0)
    (define end 0)
    (define uses '())

    (for ([bid linear-order])
      (define block-start (hash-ref pos-map (cons 'block-start bid) 0))

      ;; Check if live-in
      (when (set-member? (hash-ref (LivenessInfo-live-in basic-info) bid (set)) var)
        (set! start (min start block-start)))

      ;; Check definitions and uses in block
      (define block (cfg-get-block cfg bid))
      (when block
        (for ([insn (CfgBlock-insns block)])
          (when (VfInsn? insn)
            (define insn-pos (hash-ref pos-map insn 0))
            ;; Definition extends interval
            (when (member var (VfInsn-outputs insn))
              (set! start (min start insn-pos))
              (set! end (max end insn-pos)))
            ;; Use extends interval
            (for ([input (VfInsn-inputs insn)])
              (when (equal? input var)
                (set! end (max end insn-pos))
                (set! uses (cons insn-pos uses)))))))

      ;; Check if live-out
      (when (set-member? (hash-ref (LivenessInfo-live-out basic-info) bid (set)) var)
        (set! end (max end (+ block-start 2)))))

    ;; Only create interval if variable is actually used
    (when (and (< start +inf.0) (> end 0))
      (hash-set! intervals var
                 (list (Interval var (inexact->exact start) end (reverse uses))))))

  (AnalysisResult
   (LivenessInfo (LivenessInfo-live-in basic-info)
                 (LivenessInfo-live-out basic-info)
                 (LivenessInfo-def basic-info)
                 (LivenessInfo-use basic-info)
                 intervals)
   'linear-scan
   (hash 'intervals (hash-count intervals))
   #t))

;; ============================================================
;; Helper Functions
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

    (hash-set! def-map bid (list->set (set->list def-set)))
    (hash-set! use-map bid (list->set (set->list use-set))))

  (values def-map use-map))

;; Build successor map
(define (build-successor-map cfg)
  (define succ-map (make-hash))
  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if block
        (hash-set! succ-map bid
                   (terminator-successors (CfgBlock-terminator block)))
        (hash-set! succ-map bid '())))
  succ-map)

;; Build predecessor map
(define (build-predecessor-map cfg)
  (define pred-map (make-hash))
  (for ([bid (cfg-all-block-ids cfg)])
    (hash-set! pred-map bid '()))

  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (for ([succ (terminator-successors (CfgBlock-terminator block))])
        (hash-set! pred-map succ
                   (cons bid (hash-ref pred-map succ '()))))))
  pred-map)

;; Get predecessors from successor map
(define (get-predecessors-from-succ succ-map bid all-blocks)
  (for/list ([other all-blocks]
             #:when (member bid (hash-ref succ-map other '())))
    other))

;; Compute linear order via DFS
(define (compute-linear-order cfg entry)
  (define visited (mutable-set))
  (define order '())
  (define succ-map (build-successor-map cfg))

  (define (dfs bid)
    (unless (set-member? visited bid)
      (set-add! visited bid)
      (for ([succ (hash-ref succ-map bid '())])
        (dfs succ))
      (set! order (cons bid order))))

  (dfs entry)
  order)

;; ============================================================
;; Analysis Registration
;; ============================================================

(define liveness-analysis
  (Analysis
   'liveness
   "Compute variable liveness in CFG"
   (hash
    'backward compute-liveness-backward
    'ssa compute-liveness-ssa
    'linear-scan compute-liveness-linear-scan)
   'backward  ; Default algorithm
   '()        ; No dependencies
   '(cfg-structure)))  ; Invalidated by CFG changes

(register-analysis! liveness-analysis)

(provide liveness-analysis)

;; ============================================================
;; Convenience Functions
;; ============================================================

;; Compute liveness with default algorithm
(define (compute-liveness cfg #:algorithm [algo #f])
  (define ctx (make-analysis-context))
  (define result (run-analysis 'liveness cfg ctx #:algorithm algo))
  (AnalysisResult-data result))

;; Check if variable is live at block entry
(define (live-at-entry? live-info var bid)
  (set-member? (hash-ref (LivenessInfo-live-in live-info) bid (set)) var))

;; Check if variable is live at block exit
(define (live-at-exit? live-info var bid)
  (set-member? (hash-ref (LivenessInfo-live-out live-info) bid (set)) var))

;; Get live interval for a variable
(define (get-live-intervals live-info var)
  (hash-ref (LivenessInfo-intervals live-info) var '()))

;; Get all live variables at block entry
(define (get-live-in live-info bid)
  (set->list (hash-ref (LivenessInfo-live-in live-info) bid (set))))

;; Get all live variables at block exit
(define (get-live-out live-info bid)
  (set->list (hash-ref (LivenessInfo-live-out live-info) bid (set))))

(provide compute-liveness live-at-entry? live-at-exit?
         get-live-intervals get-live-in get-live-out)
