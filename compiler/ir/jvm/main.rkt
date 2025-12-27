#lang racket/base

;; ============================================================
;; IR Layer: JVM Module
;; ============================================================
;;
;; Unified entry point for JVM IR types.
;;
;; Usage:
;;   (require "ir/jvm/main.rkt")
;; ============================================================

(require "types.rkt")

(provide (all-from-out "types.rkt"))
