#lang racket/base

;; ============================================================
;; Component: L-var Module
;; ============================================================
;;
;; Re-exports all L-var components:
;;   - transform: uniquify, remove-complex, explicate-control
;;
;; L-var is the high-level language from Essentials of Compilation.
;; It includes:
;;   - Integers, booleans
;;   - Let bindings, variables
;;   - Conditionals
;;   - Primitive operations
;;
;; ============================================================

(require "transform/transform.rkt")

(provide (all-from-out "transform/transform.rkt"))
