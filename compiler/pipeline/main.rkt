#lang racket/base

;; ============================================================
;; Pipeline Layer: Unified Export
;; ============================================================
;;
;; The pipeline layer provides language-specific compilation flows
;; that compose transforms and analyses into complete pipelines.
;;
;; Structure:
;;   - common/   : Pass definition and composition utilities
;;   - lang-l/   : L-language (high-level functional) pipeline
;;   - lang-c/   : C-language (low-level control flow) pipeline
;;   - java/     : Java bytecode compilation pipeline
;;
;; Design principles:
;;   - Language-specific (knows source and target languages)
;;   - Composes component layer analyses and transforms
;;   - Provides end-to-end compilation flows
;;   - Supports pipeline customization and statistics
;;
;; Usage:
;;   (require "pipeline/main.rkt")
;;   (define cfg (run-l-to-cfg my-l-program))
;;   (define cfg (run-java-to-cfg my-jvm-method))
;;
;; ============================================================

(require "common/main.rkt")
(require "lang-l/main.rkt")
(require "lang-c/main.rkt")
(require "java/main.rkt")

;; Common utilities
(provide (all-from-out "common/main.rkt"))

;; Language pipelines (with prefix to avoid conflicts)
(provide
  ;; L-language
  l-pipeline
  l-pipeline-to-cfg
  run-l-pipeline
  run-l-to-cfg
  l-pipeline-info

  ;; C-language
  c-pipeline
  c-pipeline-to-cfg
  run-c-pipeline
  run-c-to-cfg
  c-pipeline-info

  ;; Java
  java-pipeline
  java-pipeline-to-cfg
  run-java-method-pipeline
  run-java-to-cfg
  java-analyze-method
  java-pipeline-info)

;; JVM IR types (from java pipeline)
(provide (all-from-out "java/main.rkt"))

;; ============================================================
;; Pipeline Registry
;; ============================================================

(define all-pipelines
  (hash
    'l-language l-pipeline
    'c-language c-pipeline
    'java java-pipeline))

(define (get-pipeline name)
  (hash-ref all-pipelines name #f))

(define (list-pipelines)
  (hash-keys all-pipelines))

(provide get-pipeline list-pipelines all-pipelines)
