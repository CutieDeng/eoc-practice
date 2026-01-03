#lang racket/base

;; ============================================================
;; Pipeline: Java Compilation
;; ============================================================
;;
;; Complete compilation pipeline for Java bytecode:
;;   .class file → JVM IR → CFG → RVSDG → (optimizations)
;;
;; Supports:
;;   - Reading Java class files via serialized format
;;   - Stack simulation for bytecode → value-flow conversion
;;   - Control flow reconstruction
;;   - CFG and RVSDG analysis
;;
;; ============================================================

(require racket/match racket/list)
(require "../common/main.rkt")
(require "../../transform/jvm-to-cfg.rkt")
;; (require "../../transform/cfg-to-rvsdg.rkt")
(require "../../component/cfg/main.rkt")
(require "ir/types.rkt")

(provide
  ;; Pipeline
  java-pipeline
  java-pipeline-to-cfg

  ;; Direct transforms
  jvm-method->cfg

  ;; Pipeline execution
  run-java-method-pipeline
  run-java-to-cfg

  ;; Method analysis
  java-analyze-method

  ;; Pipeline info
  java-pipeline-info)

;; ============================================================
;; Pipeline Passes
;; ============================================================

;; Pass: JVM Method → CFG
(define pass-jvm-to-cfg
  (make-pass 'jvm-to-cfg
    jvm-method->cfg
    #:description "Convert JVM bytecode method to CFG via stack simulation"
    #:invalidates '(cfg-structure)))

;; Pass: CFG dominance analysis
(define pass-cfg-dominance
  (make-pass 'cfg-dominance
    (lambda (cfg)
      (cfg-compute-dominators cfg)
      cfg)
    #:description "Compute CFG dominance information"
    #:requires '(jvm-to-cfg)))

;; Pass: CFG liveness analysis
(define pass-cfg-liveness
  (make-pass 'cfg-liveness
    (lambda (cfg)
      (cfg-compute-liveness cfg)
      cfg)
    #:description "Compute CFG liveness information"
    #:requires '(jvm-to-cfg)))

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

;; Basic pipeline: JVM → CFG
(define java-pipeline-to-cfg
  (make-pipeline 'java-to-cfg
    (list pass-jvm-to-cfg)))

;; Full pipeline: JVM → CFG with analyses
(define java-pipeline
  (make-pipeline 'java-full
    (list pass-jvm-to-cfg
          pass-cfg-dominance
          pass-cfg-liveness
          pass-cfg-loops)))

;; ============================================================
;; Pipeline Execution
;; ============================================================

;; Run JVM → CFG pipeline for a method
(define (run-java-to-cfg jvm-method)
  (run-pipeline java-pipeline-to-cfg jvm-method))

;; Run full Java pipeline for a method
(define (run-java-method-pipeline jvm-method)
  (run-pipeline java-pipeline jvm-method))

;; ============================================================
;; Method Analysis
;; ============================================================

;; Analyze a JVM method and return analysis results
(define (java-analyze-method jvm-method)
  (define cfg (jvm-method->cfg jvm-method))

  (define dominators (cfg-compute-dominators cfg))
  (define liveness (cfg-compute-liveness cfg))
  (define loops (cfg-find-loops cfg))

  (hash
    'method-name (JvmMethod-name jvm-method)
    'method-descriptor (JvmMethod-descriptor jvm-method)
    'cfg cfg
    'block-count (length (cfg-all-block-ids cfg))
    'dominators dominators
    'liveness liveness
    'loops loops
    'loop-count (length loops)))

;; ============================================================
;; Pipeline Info
;; ============================================================

(define (java-pipeline-info)
  (hash
    'name 'java
    'description "Java bytecode compilation pipeline"
    'source-ir 'jvm-method
    'target-ir 'cfg
    'passes (pipeline-pass-names java-pipeline)
    'features '(stack-simulation exception-handling)))
