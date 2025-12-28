#lang racket/base

;; ============================================================
;; Abstract Analysis Framework
;; ============================================================
;;
;; Provides a unified interface for program analyses with:
;;   - Multiple algorithm implementations
;;   - Configurable strategies
;;   - Result caching
;;   - Performance profiling
;;
;; Design Philosophy:
;;   - Analyses are decoupled from specific IR representations
;;   - Multiple algorithms can implement the same analysis
;;   - Users can select algorithms based on performance needs
;; ============================================================

(require racket/match racket/list racket/hash)
(require "../lib/main.rkt")

;; ============================================================
;; Analysis Result Types
;; ============================================================

;; Result of running an analysis
(struct AnalysisResult (
  data        ; The computed analysis data
  algorithm   ; Symbol - which algorithm was used
  stats       ; Hash - performance statistics
  valid?      ; Boolean - whether result is still valid
) #:transparent)

(provide (struct-out AnalysisResult))

;; ============================================================
;; Analysis Interface
;; ============================================================

;; An analysis definition
(struct Analysis (
  name              ; Symbol - analysis name
  description       ; String - what this analysis computes
  algorithms        ; Hash[Symbol -> AnalysisAlgorithm]
  default-algorithm ; Symbol - default algorithm to use
  dependencies      ; (Listof Symbol) - other analyses this depends on
  invalidated-by    ; (Listof Symbol) - transformations that invalidate this
) #:transparent)

;; An algorithm implementing an analysis
(struct AnalysisAlgorithm (
  name            ; Symbol - algorithm name
  description     ; String - algorithm description
  complexity      ; String - time/space complexity
  best-for        ; String - when to use this algorithm
  compute         ; (CFG × AnalysisContext) -> AnalysisResult
) #:transparent)

(provide (struct-out Analysis) (struct-out AnalysisAlgorithm))

;; ============================================================
;; Analysis Context
;; ============================================================

;; Runtime context for analysis execution
(struct AnalysisContext (
  cache           ; Hash[Symbol -> AnalysisResult] - cached results
  config          ; Hash[Symbol -> Any] - configuration options
  stats-box       ; Box[Hash] - mutable statistics
) #:transparent)

(define (make-analysis-context #:config [config (hash)])
  (AnalysisContext (make-hash) config (box (hash))))

(define (ctx-get-cached ctx name)
  (define result (hash-ref (AnalysisContext-cache ctx) name #f))
  (and result (AnalysisResult-valid? result) result))

(define (ctx-set-cached! ctx name result)
  (hash-set! (AnalysisContext-cache ctx) name result))

(define (ctx-invalidate! ctx name)
  (define result (hash-ref (AnalysisContext-cache ctx) name #f))
  (when result
    (hash-set! (AnalysisContext-cache ctx) name
               (struct-copy AnalysisResult result [valid? #f]))))

(define (ctx-config ctx key [default #f])
  (hash-ref (AnalysisContext-config ctx) key default))

(define (ctx-record-stat! ctx key value)
  (define box (AnalysisContext-stats-box ctx))
  (set-box! box (hash-set (unbox box) key value)))

(define (ctx-get-stats ctx)
  (unbox (AnalysisContext-stats-box ctx)))

(provide make-analysis-context
         ctx-get-cached ctx-set-cached! ctx-invalidate!
         ctx-config ctx-record-stat! ctx-get-stats)

;; ============================================================
;; Analysis Registry
;; ============================================================

;; Global registry of available analyses
(define analysis-registry (make-hash))

(define (register-analysis! analysis)
  (hash-set! analysis-registry (Analysis-name analysis) analysis))

(define (get-analysis name)
  (hash-ref analysis-registry name #f))

(define (list-analyses)
  (hash-keys analysis-registry))

(provide register-analysis! get-analysis list-analyses)

;; ============================================================
;; Analysis Execution
;; ============================================================

;; Run an analysis on a CFG
(define (run-analysis analysis-name cfg ctx
                       #:algorithm [algorithm #f]
                       #:force? [force? #f])
  (define analysis (get-analysis analysis-name))
  (unless analysis
    (error 'run-analysis "Unknown analysis: ~a" analysis-name))

  ;; Check cache unless forced
  (unless force?
    (define cached (ctx-get-cached ctx analysis-name))
    (when cached
      (return cached)))

  ;; Ensure dependencies are computed
  (for ([dep (Analysis-dependencies analysis)])
    (run-analysis dep cfg ctx))

  ;; Select algorithm
  (define algo-name (or algorithm (Analysis-default-algorithm analysis)))
  (define algo (hash-ref (Analysis-algorithms analysis) algo-name #f))
  (unless algo
    (error 'run-analysis "Unknown algorithm ~a for analysis ~a"
           algo-name analysis-name))

  ;; Run the algorithm (supports both AnalysisAlgorithm struct and raw procedure)
  (define start-time (current-inexact-milliseconds))
  (define compute-fn
    (if (AnalysisAlgorithm? algo)
        (AnalysisAlgorithm-compute algo)
        algo))  ; Allow raw procedures for convenience
  (define result (compute-fn cfg ctx))
  (define end-time (current-inexact-milliseconds))

  ;; Record statistics
  (ctx-record-stat! ctx (string->symbol
                         (format "~a-time-ms" analysis-name))
                    (- end-time start-time))

  ;; Cache result
  (ctx-set-cached! ctx analysis-name result)

  result)

;; Helper for early return
(define-syntax-rule (return x) (begin x))

(provide run-analysis)

;; ============================================================
;; Common Analysis Patterns
;; ============================================================

;; Create a simple forward analysis
(define (make-forward-analysis name desc transfer join bottom)
  (AnalysisAlgorithm
   name desc
   "O(n × k) where k is iteration count"
   "General purpose forward dataflow"
   (lambda (cfg ctx)
     (forward-dataflow cfg transfer join bottom))))

;; Create a simple backward analysis
(define (make-backward-analysis name desc transfer join bottom)
  (AnalysisAlgorithm
   name desc
   "O(n × k) where k is iteration count"
   "General purpose backward dataflow"
   (lambda (cfg ctx)
     (backward-dataflow cfg transfer join bottom))))

(provide make-forward-analysis make-backward-analysis)

;; ============================================================
;; Dataflow Analysis Core
;; ============================================================

;; Generic forward dataflow analysis
(define (forward-dataflow cfg transfer join bottom)
  (define block-ids (cfg-all-block-ids cfg))
  (define entry (cfg-get-entry cfg))

  ;; Initialize all blocks with bottom
  (define state (make-hash))
  (for ([bid block-ids])
    (hash-set! state bid bottom))

  ;; Worklist algorithm
  (define worklist (list entry))
  (define iterations 0)

  (let loop ()
    (unless (null? worklist)
      (set! iterations (+ iterations 1))
      (define bid (car worklist))
      (set! worklist (cdr worklist))

      (define block (cfg-get-block cfg bid))
      (when block
        ;; Compute input from predecessors
        (define preds (get-predecessors cfg bid))
        (define input
          (if (null? preds)
              bottom
              (foldl (lambda (p acc)
                       (join acc (hash-ref state p bottom)))
                     bottom
                     preds)))

        ;; Apply transfer function
        (define output (transfer bid block input))

        ;; If changed, update and add successors to worklist
        (unless (equal? output (hash-ref state bid))
          (hash-set! state bid output)
          (define succs (get-successors cfg bid))
          (set! worklist (append succs worklist))))

      (loop)))

  (AnalysisResult state 'forward-dataflow
                  (hash 'iterations iterations) #t))

;; Generic backward dataflow analysis
(define (backward-dataflow cfg transfer join bottom)
  (define block-ids (cfg-all-block-ids cfg))

  ;; Initialize all blocks with bottom
  (define state (make-hash))
  (for ([bid block-ids])
    (hash-set! state bid bottom))

  ;; Worklist algorithm (reverse order)
  (define worklist (reverse block-ids))
  (define iterations 0)

  (let loop ()
    (unless (null? worklist)
      (set! iterations (+ iterations 1))
      (define bid (car worklist))
      (set! worklist (cdr worklist))

      (define block (cfg-get-block cfg bid))
      (when block
        ;; Compute input from successors
        (define succs (get-successors cfg bid))
        (define input
          (if (null? succs)
              bottom
              (foldl (lambda (s acc)
                       (join acc (hash-ref state s bottom)))
                     bottom
                     succs)))

        ;; Apply transfer function
        (define output (transfer bid block input))

        ;; If changed, update and add predecessors to worklist
        (unless (equal? output (hash-ref state bid))
          (hash-set! state bid output)
          (define preds (get-predecessors cfg bid))
          (set! worklist (append preds worklist))))

      (loop)))

  (AnalysisResult state 'backward-dataflow
                  (hash 'iterations iterations) #t))

(provide forward-dataflow backward-dataflow)

;; ============================================================
;; CFG Helpers (Import from ir/cfg)
;; ============================================================

(require "../ir/cfg/main.rkt")

;; Get predecessors of a block
(define (get-predecessors cfg bid)
  (define preds '())
  (for ([other-bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg other-bid))
    (when block
      (define succs (terminator-successors (CfgBlock-terminator block)))
      (when (member bid succs)
        (set! preds (cons other-bid preds)))))
  preds)

;; Get successors of a block
(define (get-successors cfg bid)
  (define block (cfg-get-block cfg bid))
  (if block
      (terminator-successors (CfgBlock-terminator block))
      '()))

(provide get-predecessors get-successors)
