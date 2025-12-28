#lang racket/base

;; ============================================================
;; Interference Analysis
;; ============================================================
;;
;; Builds interference graph for register allocation.
;;
;; Two variables interfere if they are simultaneously live at
;; some program point. The interference graph has variables as
;; nodes and edges between interfering variables.
;;
;; Provides multiple algorithms:
;;   1. Classic (from liveness sets)
;;   2. SSA-based (using phi-congruence)
;;
;; References:
;;   - Chaitin et al. "Register Allocation via Coloring"
;;   - Briggs et al. "Improvements to Graph Coloring"
;; ============================================================

(require racket/match racket/list racket/set racket/hash)
(require "../framework.rkt")
(require "../liveness/main.rkt")
(require "../../ir/cfg/main.rkt")
(require "../../lib/main.rkt")

;; ============================================================
;; Interference Result Structure
;; ============================================================

;; Interference graph representation
(struct InterferenceGraph (
  nodes         ; Set[VarId] - all variables
  edges         ; Hash[VarId -> Set[VarId]] - interference edges (undirected)
  move-edges    ; Hash[VarId -> Set[VarId]] - move-related edges (for coalescing)
  spill-cost    ; Hash[VarId -> Number] - estimated spill cost
) #:transparent)

(provide (struct-out InterferenceGraph))

;; ============================================================
;; Algorithm 1: Classic Interference (from Liveness)
;; ============================================================
;;
;; Build interference from live-out sets.
;; Variables in the same live set interfere.
;; Time: O(n × v²) worst case
;; Space: O(v²)

(define (compute-interference-classic cfg ctx)
  ;; First compute liveness
  (define live-result (run-analysis 'liveness cfg ctx))
  (define live-info (AnalysisResult-data live-result))

  ;; Build interference edges
  (define nodes (mutable-set))
  (define edges (make-hash))
  (define move-edges (make-hash))
  (define spill-cost (make-hash))

  (define (add-edge! v1 v2)
    (unless (equal? v1 v2)
      (hash-set! edges v1
                 (set-add (hash-ref edges v1 (set)) v2))
      (hash-set! edges v2
                 (set-add (hash-ref edges v2 (set)) v1))))

  (define (add-move-edge! v1 v2)
    (unless (equal? v1 v2)
      (hash-set! move-edges v1
                 (set-add (hash-ref move-edges v1 (set)) v2))
      (hash-set! move-edges v2
                 (set-add (hash-ref move-edges v2 (set)) v1))))

  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      ;; Get live-out set for this block
      (define live (list->mutable-set
                    (hash-ref (LivenessInfo-live-out live-info) bid '())))

      ;; Process instructions in reverse order
      (define insns (reverse (CfgBlock-insns block)))
      (for ([insn insns])
        (when (VfInsn? insn)
          (define outputs (VfInsn-outputs insn))
          (define inputs (VfInsn-inputs insn))

          ;; Each output interferes with everything live
          (for ([out outputs])
            (set-add! nodes out)
            (hash-set! spill-cost out
                       (+ (hash-ref spill-cost out 0) 1))
            (for ([live-var (in-set live)])
              (add-edge! out live-var)))

          ;; Remove outputs from live set
          (for ([out outputs])
            (set-remove! live out))

          ;; Add inputs to live set
          (for ([input inputs])
            (when (VarId? input)
              (set-add! nodes input)
              (set-add! live input)
              (hash-set! spill-cost input
                         (+ (hash-ref spill-cost input 0) 1))))

          ;; Detect move instructions for coalescing
          (when (and (eq? (VfInsn-op insn) 'copy)
                     (= (length outputs) 1)
                     (= (length inputs) 1)
                     (VarId? (car inputs)))
            (add-move-edge! (car outputs) (car inputs)))))

      ;; PHI nodes: outputs interfere with live-in (except sources)
      (for ([phi (CfgBlock-phis block)])
        (when (PhiInsn? phi)
          (define out (PhiInsn-output phi))
          (set-add! nodes out)

          ;; PHI output interferes with live-in minus its sources
          (define live-in (hash-ref (LivenessInfo-live-in live-info) bid (set)))
          (define phi-sources (for/set ([src (PhiInsn-sources phi)])
                                (cdr src)))
          (for ([v (in-set live-in)])
            (unless (set-member? phi-sources v)
              (add-edge! out v)))

          ;; PHI is move-related with its sources
          (for ([src (PhiInsn-sources phi)])
            (when (VarId? (cdr src))
              (add-move-edge! out (cdr src))))))))

  (AnalysisResult
   (InterferenceGraph
    (list->set (set->list nodes))
    (for/hash ([(k v) (in-hash edges)])
      (values k (list->set (set->list v))))
    (for/hash ([(k v) (in-hash move-edges)])
      (values k (list->set (set->list v))))
    spill-cost)
   'classic
   (hash 'nodes (set-count nodes)
         'edges (for/sum ([v (hash-values edges)]) (set-count v)))
   #t))

;; ============================================================
;; Algorithm 2: SSA Interference (Phi-Congruence)
;; ============================================================
;;
;; Build interference using SSA properties.
;; More precise handling of PHI nodes.
;; Time: O(n + e)
;; Space: O(v²)

(define (compute-interference-ssa cfg ctx)
  ;; For now, use classic algorithm with SSA-aware liveness
  (define live-result (run-analysis 'liveness cfg ctx #:algorithm 'ssa))
  (define live-info (AnalysisResult-data live-result))

  ;; Build interference edges
  (define nodes (mutable-set))
  (define edges (make-hash))
  (define move-edges (make-hash))
  (define spill-cost (make-hash))

  (define (add-edge! v1 v2)
    (unless (equal? v1 v2)
      (hash-set! edges v1
                 (set-add (hash-ref edges v1 (set)) v2))
      (hash-set! edges v2
                 (set-add (hash-ref edges v2 (set)) v1))))

  (define (add-move-edge! v1 v2)
    (unless (equal? v1 v2)
      (hash-set! move-edges v1
                 (set-add (hash-ref move-edges v1 (set)) v2))
      (hash-set! move-edges v2
                 (set-add (hash-ref move-edges v2 (set)) v1))))

  ;; Build def points for each variable
  (define var->def-point (make-hash))
  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (for ([phi (CfgBlock-phis block)])
        (when (PhiInsn? phi)
          (hash-set! var->def-point (PhiInsn-output phi) (cons 'phi bid))))
      (for ([insn (CfgBlock-insns block)] [i (in-naturals)])
        (when (VfInsn? insn)
          (for ([out (VfInsn-outputs insn)])
            (hash-set! var->def-point out (cons 'insn (cons bid i))))))))

  ;; At each definition point, the defined variable interferes with
  ;; all live variables (SSA dominance property)
  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (define live (list->mutable-set
                    (hash-ref (LivenessInfo-live-out live-info) bid '())))

      ;; Process in reverse
      (define insns (reverse (CfgBlock-insns block)))
      (for ([insn insns])
        (when (VfInsn? insn)
          (define outputs (VfInsn-outputs insn))

          ;; Each output interferes with live set
          (for ([out outputs])
            (set-add! nodes out)
            (hash-set! spill-cost out
                       (+ (hash-ref spill-cost out 0) 1))
            (for ([v (in-set live)])
              (add-edge! out v)))

          ;; Update live set
          (for ([out outputs])
            (set-remove! live out))
          (for ([input (VfInsn-inputs insn)])
            (when (VarId? input)
              (set-add! nodes input)
              (set-add! live input)
              (hash-set! spill-cost input
                         (+ (hash-ref spill-cost input 0) 1))))

          ;; Move edges
          (when (and (eq? (VfInsn-op insn) 'copy)
                     (= (length outputs) 1)
                     (= (length (VfInsn-inputs insn)) 1)
                     (VarId? (car (VfInsn-inputs insn))))
            (add-move-edge! (car outputs) (car (VfInsn-inputs insn))))))

      ;; PHI handling for SSA
      (for ([phi (CfgBlock-phis block)])
        (when (PhiInsn? phi)
          (define out (PhiInsn-output phi))
          (set-add! nodes out)

          ;; In SSA, PHI output only interferes with values
          ;; that are live across the PHI (not the sources)
          (define live-in (hash-ref (LivenessInfo-live-in live-info) bid (set)))
          (define sources (for/set ([src (PhiInsn-sources phi)])
                            (cdr src)))
          (for ([v (in-set live-in)])
            (unless (set-member? sources v)
              (add-edge! out v)))

          ;; Move edges for PHI sources
          (for ([src (PhiInsn-sources phi)])
            (when (VarId? (cdr src))
              (add-move-edge! out (cdr src))))))))

  (AnalysisResult
   (InterferenceGraph
    (list->set (set->list nodes))
    (for/hash ([(k v) (in-hash edges)])
      (values k (list->set (set->list v))))
    (for/hash ([(k v) (in-hash move-edges)])
      (values k (list->set (set->list v))))
    spill-cost)
   'ssa
   (hash 'nodes (set-count nodes)
         'edges (for/sum ([v (hash-values edges)]) (set-count v)))
   #t))

;; ============================================================
;; Helper Functions
;; ============================================================

;; Convert list to mutable set
(define (list->mutable-set lst)
  (define s (mutable-set))
  (for ([x lst])
    (set-add! s x))
  s)

;; ============================================================
;; Analysis Registration
;; ============================================================

(define interference-analysis
  (Analysis
   'interference
   "Build interference graph for register allocation"
   (hash
    'classic compute-interference-classic
    'ssa compute-interference-ssa)
   'classic  ; Default algorithm
   '(liveness)  ; Depends on liveness
   '(cfg-structure)))  ; Invalidated by CFG changes

(register-analysis! interference-analysis)

(provide interference-analysis)

;; ============================================================
;; Convenience Functions
;; ============================================================

;; Compute interference with default algorithm
(define (compute-interference cfg #:algorithm [algo #f])
  (define ctx (make-analysis-context))
  (define result (run-analysis 'interference cfg ctx #:algorithm algo))
  (AnalysisResult-data result))

;; Check if two variables interfere
(define (interferes? ig v1 v2)
  (set-member? (hash-ref (InterferenceGraph-edges ig) v1 (set)) v2))

;; Get all variables that interfere with v
(define (get-interferences ig v)
  (set->list (hash-ref (InterferenceGraph-edges ig) v (set))))

;; Get degree (number of interferences) of a variable
(define (get-degree ig v)
  (set-count (hash-ref (InterferenceGraph-edges ig) v (set))))

;; Check if two variables are move-related
(define (move-related? ig v1 v2)
  (set-member? (hash-ref (InterferenceGraph-move-edges ig) v1 (set)) v2))

;; Get spill cost estimate
(define (get-spill-cost ig v)
  (hash-ref (InterferenceGraph-spill-cost ig) v 0))

(provide compute-interference interferes? get-interferences
         get-degree move-related? get-spill-cost)
