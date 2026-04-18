#lang racket/base

;; ============================================================
;; Component: Analysis Framework
;; ============================================================
;;
;; Provides a unified interface for program analyses with:
;;   - Multiple algorithm implementations
;;   - Configurable strategies
;;   - Result caching
;;   - Performance profiling
;;
;; This framework is IR-agnostic. Concrete analyses (e.g., for CFG)
;; use this framework and bind it to specific IR types.
;; ============================================================

(require racket/match racket/list racket/hash)

;; ============================================================
;; Analysis Result Types
;; ============================================================

;; Result of running an analysis
(struct AnalysisResult (
  data        ; The computed analysis data
  algorithm   ; Symbol - which algorithm was used
  stats       ; Hash - performance statistics
  valid?      ; Boolean - whether result is still valid
) #:prefab)

(provide (struct-out AnalysisResult))

;; ============================================================
;; Analysis Interface
;; ============================================================

;; An analysis definition
(struct Analysis (
  name              ; Symbol - analysis name
  description       ; String - what this analysis computes
  algorithms        ; Hash[Symbol -> Procedure]
  default-algorithm ; Symbol - default algorithm to use
  dependencies      ; (Listof Symbol) - other analyses this depends on
  invalidated-by    ; (Listof Symbol) - transformations that invalidate this
) #:transparent)

(provide (struct-out Analysis))

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

(define (clear-registry!)
  (hash-clear! analysis-registry))

(provide register-analysis! get-analysis list-analyses clear-registry!)

;; ============================================================
;; Analysis Execution
;; ============================================================

;; Run an analysis on an IR
;; Parameters:
;;   analysis-name : Symbol
;;   ir            : Any IR (CFG, RVSDG, etc.)
;;   ctx           : AnalysisContext
;;   #:algorithm   : Symbol (optional, uses default if not specified)
;;   #:force?      : Boolean (ignore cache if true)
;;
(define (run-analysis analysis-name ir ctx
                       #:algorithm [algorithm #f]
                       #:force? [force? #f])
  (define analysis (get-analysis analysis-name))
  (unless analysis
    (error 'run-analysis "Unknown analysis: ~a" analysis-name))

  ;; Check cache unless forced
  (unless force?
    (define cached (ctx-get-cached ctx analysis-name))
    (when cached
      (begin cached)))

  ;; Ensure dependencies are computed
  (for ([dep (Analysis-dependencies analysis)])
    (run-analysis dep ir ctx))

  ;; Select algorithm
  (define algo-name (or algorithm (Analysis-default-algorithm analysis)))
  (define algo (hash-ref (Analysis-algorithms analysis) algo-name #f))
  (unless algo
    (error 'run-analysis "Unknown algorithm ~a for analysis ~a"
           algo-name analysis-name))

  ;; Run the algorithm
  (define start-time (current-inexact-milliseconds))
  (define result (algo ir ctx))
  (define end-time (current-inexact-milliseconds))

  ;; Record statistics
  (ctx-record-stat! ctx (string->symbol
                         (format "~a-time-ms" analysis-name))
                    (- end-time start-time))

  ;; Cache result
  (ctx-set-cached! ctx analysis-name result)

  result)

(provide run-analysis)

;; ============================================================
;; Convenience: Define Analysis Macro
;; ============================================================

;; Helper to create an analysis easily
(define (make-analysis name desc algorithms
                       #:default [default #f]
                       #:dependencies [deps '()]
                       #:invalidated-by [inv '()])
  (define algo-names (hash-keys algorithms))
  (define default-algo (or default (car algo-names)))
  (Analysis name desc algorithms default-algo deps inv))

(provide make-analysis)
