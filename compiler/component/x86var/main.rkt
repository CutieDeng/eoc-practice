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

(require "transform/main.rkt")

(provide (all-from-out "transform/main.rkt"))
