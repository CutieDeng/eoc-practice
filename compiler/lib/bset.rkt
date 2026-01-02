#lang racket/base

;; ============================================================
;; Foundation: Bit Set (Integer Set)
;; ============================================================
;;
;; Compatibility layer over cutie-ftree/bitset.
;; Provides bset-* aliases for backward compatibility.
;; ============================================================

(require "../../cutie-ftree/bitset.rkt")

;; Re-export all from bitset
(provide (all-from-out "../../cutie-ftree/bitset.rkt"))

;; ============================================================
;; Compatibility Aliases: bset → bitset
;; ============================================================

(define bset-union bitset-union)
(define bset-and bitset-intersection)
(define bset-member? bitset-member?)
(define bset-add bitset-add)
(define bset-remove bitset-remove)
(define bset-subtract bitset-subtract)

(define in-bset in-bitset)
(define in-bset/rev in-bitset/rev)

(define seq->bset seq->bitset)
(define bset->list bitset->list)
(define bset->vector bitset->vector)

;; bset constructor (variadic)
(define (bset . elements)
  (apply bitset elements))

;; bset* constructor (list)
(define bset* list->bitset*)

;; Legacy names for min/max
(define integer-leftmost bitset-max)
(define integer-rightmost bitset-min)

;; ============================================================
;; Exports
;; ============================================================

(provide bset-union bset-and bset-member? bset-add bset-remove bset-subtract)
(provide in-bset in-bset/rev)
(provide seq->bset bset->list bset->vector)
(provide bset bset*)
(provide integer-leftmost integer-rightmost)
