#lang racket/base

;; ============================================================
;; Pipeline: C-Language Module
;; ============================================================
;;
;; Re-exports C-language pipeline components.
;; ============================================================

(require "pipeline.rkt")

(provide (all-from-out "pipeline.rkt"))
