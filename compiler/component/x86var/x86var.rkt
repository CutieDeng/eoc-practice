#lang racket/base

;; ============================================================
;; Component: X86-var Module
;; ============================================================
;;
;; Re-exports all X86-var components:
;;   - transform: select-instructions, assign-homes, etc.
;;
;; X86-var is the x86 IR with pseudo-registers (variables)
;; from Essentials of Compilation.
;;
;; ============================================================

(require "transform/transform.rkt")

(provide (all-from-out "transform/transform.rkt"))
