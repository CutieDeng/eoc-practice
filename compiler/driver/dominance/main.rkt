#lang racket/base

;; ============================================================
;; Driver: Dominance Module
;; ============================================================
;;
;; Re-exports all dominator algorithms.
;; ============================================================

(require "dominator.rkt")

(provide (all-from-out "dominator.rkt"))
