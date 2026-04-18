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
;; Plus a small set of pvector utilities built on the primitives.
;; ============================================================

(require cutie-ftree/cutie-ftree)

;; Re-export everything from cutie-ftree.
(provide (all-from-out cutie-ftree/cutie-ftree))

;; ============================================================
;; pvector utilities
;; ============================================================
;;
;; These wrappers compose cutie-ftree primitives so the rest of
;; the compiler can stay pvector-native without re-deriving
;; list-style helpers at each callsite.
;;
;; Names follow racket/list conventions where they match, so
;; semantics are predictable:
;;   - filter/filter-not: predicate, keep-if/keep-unless
;;   - findf:             predicate, first match or #f
;;   - index-of:          value,     first index or #f
;;   - index-where:       predicate, first index or #f
;;   - remove-all:        value,     drops EVERY occurrence
;;                        (distinct from racket's `remove`, which
;;                         drops only the first — "all" is the
;;                         common case in compiler passes)
;;   - andmap/ormap:      short-circuiting predicate folds
;; ============================================================

;; Reverse a pvector.
;; Uses in-pvector-reverse for efficient iteration.
(define (pvector-reverse pv)
  (for/pvector ([x (in-pvector-reverse pv)])
    x))

(define (pvector-map f pv)
  (for/pvector ([x (in-pvector pv)])
    (f x)))

(define (pvector-filter pred pv)
  (for/pvector ([x (in-pvector pv)] #:when (pred x))
    x))

(define (pvector-filter-not pred pv)
  (for/pvector ([x (in-pvector pv)] #:unless (pred x))
    x))

(define (pvector-foldl f init pv)
  (for/fold ([acc init]) ([x (in-pvector pv)])
    (f acc x)))

(define (pvector-foldr f init pv)
  (for/fold ([acc init]) ([x (in-pvector-reverse pv)])
    (f acc x)))

(define (pvector-findf pred pv)
  (for/first ([x (in-pvector pv)] #:when (pred x))
    x))

(define (pvector-index-where pred pv)
  (for/first ([x (in-pvector pv)] [i (in-naturals)] #:when (pred x))
    i))

(define (pvector-index-of pv v [is-equal? equal?])
  (for/first ([x (in-pvector pv)] [i (in-naturals)] #:when (is-equal? x v))
    i))

;; Drop every element equal to v.
(define (pvector-remove-all pv v [is-equal? equal?])
  (for/pvector ([x (in-pvector pv)] #:unless (is-equal? x v))
    x))

(define (pvector-andmap pred pv)
  (for/and ([x (in-pvector pv)])
    (pred x)))

(define (pvector-ormap pred pv)
  (for/or ([x (in-pvector pv)])
    (pred x)))

(define (pvector-concat* . pvs)
  (for/fold ([acc (pvector-empty)]) ([pv (in-list pvs)])
    (pvector-append acc pv)))

(provide pvector-reverse
         pvector-map
         pvector-filter
         pvector-filter-not
         pvector-foldl
         pvector-foldr
         pvector-findf
         pvector-index-of
         pvector-index-where
         pvector-remove-all
         pvector-andmap
         pvector-ormap
         pvector-concat*)
