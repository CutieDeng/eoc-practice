#lang racket/base

;; ============================================================
;; Interpreter Module Entry Point
;; ============================================================
;;
;; Provides interpreters for:
;;   - L-language (high-level functional)
;;   - C-language (low-level control flow)
;;
;; Used for testing optimizations and validating transformations.
;; ============================================================

(require "framework.rkt")
(require "interp-L.rkt")
(require "interp-C.rkt")

(provide (all-from-out "framework.rkt"))
(provide (all-from-out "interp-L.rkt"))
(provide (all-from-out "interp-C.rkt"))
