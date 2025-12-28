#lang racket/base

;; ============================================================
;; Analysis Module
;; ============================================================
;;
;; Unified entry point for all program analyses.
;;
;; Provides:
;;   - Analysis framework (run-analysis, register-analysis!, etc.)
;;   - Dominance analysis (compute-dominance, dominates?, etc.)
;;   - Dataflow analysis (forward-dataflow, backward-dataflow)
;; ============================================================

(require "framework.rkt")
(require "dominance/main.rkt")
(require "liveness/main.rkt")
(require "interference/main.rkt")
(require "loops/main.rkt")

(provide (all-from-out "framework.rkt"))
(provide (all-from-out "dominance/main.rkt"))
(provide (all-from-out "liveness/main.rkt"))
(provide (all-from-out "interference/main.rkt"))
(provide (all-from-out "loops/main.rkt"))
