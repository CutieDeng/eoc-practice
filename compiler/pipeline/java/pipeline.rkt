#lang racket/base

;; ============================================================
;; Pipeline: Java Compilation (STUB)
;; ============================================================
;;
;; Complete compilation pipeline for Java bytecode:
;;   .class file → JVM IR → CFG → RVSDG → (optimizations)
;;
;; Note: This is currently a stub. The transform and analysis
;; modules need to be reimplemented in the new architecture.
;;
;; TODO: Reimplement when the JVM→CFG transforms are ready.
;; ============================================================

(require racket/match racket/list)
(require "../common/common.rkt")
(require "ir/types.rkt")

(provide
  ;; Stub exports
  java-pipeline-info)

;; ============================================================
;; Stub Implementations
;; ============================================================

(define (java-pipeline-info)
  (hash
    'name 'java
    'description "Java bytecode compilation pipeline (not yet implemented)"
    'status 'stub))
