#lang racket/base

;; ============================================================
;; Optimization Framework - Main Entry Point
;; ============================================================
;;
;; Unified export for the abstract optimization framework.
;;
;; Usage:
;;   (require "optim/framework/main.rkt")
;;
;;   ;; Use JVM semantics
;;   (define ctx (make-context jvm-semantics))
;;   (run-pass some-pass cfg ctx)
;; ============================================================

(require "semantics.rkt")
(require "pass.rkt")
(require "jvm-semantics.rkt")

;; Re-export everything
(provide (all-from-out "semantics.rkt"))
(provide (all-from-out "pass.rkt"))
(provide (all-from-out "jvm-semantics.rkt"))
