#lang racket/base

;; ============================================================
;; Kernel Data Structures
;; ============================================================
;;
;; Re-exports immutable data structures from cutie-ftree:
;;   - pvector: persistent vector (finger tree based)
;;   - ordered-map: ordered dictionary
;;   - bitset: bit set operations
;;   - graph: immutable graph
;;   - comparator: comparison functions
;;
;; These are the foundational data structures for all IRs.
;; ============================================================

(require "../../../cutie-ftree/main.rkt")

;; Re-export everything from cutie-ftree
(provide (all-from-out "../../../cutie-ftree/main.rkt"))

;; ============================================================
;; Additional pvector utilities
;; ============================================================

;; Reverse a pvector
;; Uses in-pvector-reverse for efficient iteration
(define (pvector-reverse pv)
  (for/pvector ([x (in-pvector-reverse pv)])
    x))

(provide pvector-reverse)
