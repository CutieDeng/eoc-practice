#lang racket/base

;; ============================================================
;; Pipeline: L-Language Compilation
;; ============================================================
;;
;; Complete compilation pipeline for L-language:
;;   L-Program → CFG → RVSDG → (optimizations) → target
;;
;; L-language is a high-level functional expression language with:
;;   - Let bindings
;;   - If conditionals
;;   - While loops
;;   - Primitive operations
;;   - Lambda functions
;;
;; ============================================================

(require racket/match racket/list)
(require "../common/main.rkt")
(require "../../transform/l-to-cfg.rkt")
;; (require "../../transform/cfg-to-rvsdg.rkt")
(require "../../component/cfg/main.rkt")

(provide
  ;; Pipeline
  l-pipeline
  l-pipeline-to-cfg

  ;; Direct transforms
  l-program->cfg

  ;; Pipeline execution
  run-l-pipeline
  run-l-to-cfg

  ;; Pipeline info
  l-pipeline-info)

;; ============================================================
;; Pipeline Passes
;; ============================================================

;; Pass: L → CFG
(define pass-l-to-cfg
  (make-pass 'l-to-cfg
    l-program->cfg
    #:description "Convert L-language program to CFG"
    #:invalidates '(cfg-structure)))

;; Pass: CFG dominance analysis (analysis pass)
(define pass-cfg-dominance
  (make-pass 'cfg-dominance
    (lambda (cfg)
      ;; Run dominance analysis (side effect: caches result)
      (cfg-compute-dominators cfg)
      cfg)
    #:description "Compute CFG dominance information"
    #:requires '(l-to-cfg)))

;; Pass: CFG liveness analysis (analysis pass)
(define pass-cfg-liveness
  (make-pass 'cfg-liveness
    (lambda (cfg)
      ;; Run liveness analysis
      (cfg-compute-liveness cfg)
      cfg)
    #:description "Compute CFG liveness information"
    #:requires '(l-to-cfg)))

;; ============================================================
;; Pipeline Definitions
;; ============================================================

;; Basic pipeline: L → CFG
(define l-pipeline-to-cfg
  (make-pipeline 'l-to-cfg
    (list pass-l-to-cfg)))

;; Full pipeline: L → CFG with analyses
(define l-pipeline
  (make-pipeline 'l-full
    (list pass-l-to-cfg
          pass-cfg-dominance
          pass-cfg-liveness)))

;; ============================================================
;; Pipeline Execution
;; ============================================================

;; Run L → CFG pipeline
(define (run-l-to-cfg l-program)
  (run-pipeline l-pipeline-to-cfg l-program))

;; Run full L pipeline
(define (run-l-pipeline l-program)
  (run-pipeline l-pipeline l-program))

;; ============================================================
;; Pipeline Info
;; ============================================================

(define (l-pipeline-info)
  (hash
    'name 'l-language
    'description "High-level functional expression language"
    'source-ir 'l-program
    'target-ir 'cfg
    'passes (pipeline-pass-names l-pipeline)))
