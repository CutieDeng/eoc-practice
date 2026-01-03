#lang racket/base

;; ============================================================
;; Pipeline: C-Language Compilation
;; ============================================================
;;
;; Complete compilation pipeline for C-language IR:
;;   CProgram → CFG → RVSDG → (optimizations) → target
;;
;; C-language is a low-level control flow IR with:
;;   - Explicit basic blocks
;;   - Goto statements
;;   - Conditional branches
;;   - Already lowered control flow
;;
;; This IR is typically produced by explicate-control passes.
;;
;; ============================================================

(require racket/match racket/list)
(require "../common/main.rkt")
(require "../../transform/c-to-cfg.rkt")
(require "../../component/cfg/main.rkt")

(provide
  ;; Pipeline
  c-pipeline
  c-pipeline-to-cfg

  ;; Direct transforms
  c-program->cfg

  ;; Pipeline execution
  run-c-pipeline
  run-c-to-cfg

  ;; Pipeline info
  c-pipeline-info)

;; ============================================================
;; Pipeline Passes
;; ============================================================

;; Pass: C → CFG
(define pass-c-to-cfg
  (make-pass 'c-to-cfg
    c-program->cfg
    #:description "Convert C-language program to CFG"
    #:invalidates '(cfg-structure)))

;; Pass: CFG dominance analysis
(define pass-cfg-dominance
  (make-pass 'cfg-dominance
    (lambda (cfg)
      (cfg-compute-dominators cfg)
      cfg)
    #:description "Compute CFG dominance information"
    #:requires '(c-to-cfg)))

;; Pass: CFG liveness analysis
(define pass-cfg-liveness
  (make-pass 'cfg-liveness
    (lambda (cfg)
      (cfg-compute-liveness cfg)
      cfg)
    #:description "Compute CFG liveness information"
    #:requires '(c-to-cfg)))

;; Pass: CFG loop detection
(define pass-cfg-loops
  (make-pass 'cfg-loops
    (lambda (cfg)
      (cfg-find-loops cfg)
      cfg)
    #:description "Detect natural loops in CFG"
    #:requires '(cfg-dominance)))

;; ============================================================
;; Pipeline Definitions
;; ============================================================

;; Basic pipeline: C → CFG
(define c-pipeline-to-cfg
  (make-pipeline 'c-to-cfg
    (list pass-c-to-cfg)))

;; Full pipeline: C → CFG with analyses
(define c-pipeline
  (make-pipeline 'c-full
    (list pass-c-to-cfg
          pass-cfg-dominance
          pass-cfg-liveness
          pass-cfg-loops)))

;; ============================================================
;; Pipeline Execution
;; ============================================================

;; Run C → CFG pipeline
(define (run-c-to-cfg c-program)
  (run-pipeline c-pipeline-to-cfg c-program))

;; Run full C pipeline
(define (run-c-pipeline c-program)
  (run-pipeline c-pipeline c-program))

;; ============================================================
;; Pipeline Info
;; ============================================================

(define (c-pipeline-info)
  (hash
    'name 'c-language
    'description "Low-level control flow IR with explicit blocks"
    'source-ir 'c-program
    'target-ir 'cfg
    'passes (pipeline-pass-names c-pipeline)))
