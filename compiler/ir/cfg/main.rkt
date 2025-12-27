#lang racket/base

;; ============================================================
;; IR Layer: CFG Module
;; ============================================================
;;
;; Unified entry point for CFG data structures and operations.
;;
;; Usage:
;;   (require "ir/cfg/main.rkt")
;; ============================================================

(require "types.rkt")
(require "raw.rkt")

(provide (all-from-out "types.rkt"))
(provide (all-from-out "raw.rkt"))
