#lang racket/base

;; ============================================================
;; Foundation: Finger Tree Data Structures
;; ============================================================
;;
;; Re-exports cutie-ftree library:
;;   - ral: Random Access List
;;   - ordl: Ordered Dictionary (finger tree based)
;;
;; Also provides type-safe constructors and utilities.
;; ============================================================

(require racket/match racket/dict)
(require "../../cutie-ftree/main.rkt")

(provide (all-from-out "../../cutie-ftree/main.rkt"))

;; ============================================================
;; Type-Safe Comparison Functions
;; ============================================================

;; Integer comparison (returns -1, 0, 1 like Java's compareTo)
(define (integer-compare a b)
  (cond [(< a b) '<]
        [(> a b) '>]
        [else '=]))

;; Symbol comparison
(define (symbol-compare a b)
  (cond [(symbol<? a b) '<]
        [(equal? a b) '=]
        [else '>]))

;; String comparison
(define (string-compare a b)
  (cond [(string<? a b) '<]
        [(string=? a b) '=]
        [else '>]))

(provide integer-compare symbol-compare string-compare)

;; ============================================================
;; Pre-defined Ordl Constructors
;; ============================================================

;; Create an empty ordl for integer keys
(define (integer-ordl)
  (ordl-make-empty integer-compare))

;; Create an empty ordl for symbol keys
(define (symbol-ordl)
  (ordl-make-empty symbol-compare))

;; Create an empty ordl for string keys
(define (string-ordl)
  (ordl-make-empty string-compare))

(provide integer-ordl symbol-ordl string-ordl)

;; ============================================================
;; RAL Utilities
;; ============================================================

;; Create a single-element ral
(define (ral-single x)
  (ral-consl (ral-empty) x))

;; Convert a list to ral (preserving order)
(define (list->ral lst)
  (for/fold ([r (ral-empty)]) ([x (reverse lst)])
    (ral-consl r x)))

;; Convert ral to list
(define (ral->list r)
  (if (ral-empty? r)
      '()
      (let-values ([(hd tl) (ral-dropl r)])
        (cons hd (ral->list tl)))))

;; Map over ral
(define (ral-map f r)
  (list->ral (map f (ral->list r))))

;; Filter ral
(define (ral-filter pred r)
  (list->ral (filter pred (ral->list r))))

;; Fold over ral (left to right)
(define (ral-foldl f init r)
  (foldl f init (ral->list r)))

;; Fold over ral (right to left)
(define (ral-foldr f init r)
  (foldr f init (ral->list r)))

(provide ral-single list->ral ral->list ral-map ral-filter ral-foldl ral-foldr)

;; ============================================================
;; Ordl Utilities
;; ============================================================

;; Get all keys from ordl
(define (ordl-keys o)
  (for/list ([(k v) (in-dict o)]) k))

;; Get all values from ordl
(define (ordl-values o)
  (for/list ([(k v) (in-dict o)]) v))

;; Get all key-value pairs from ordl
(define (ordl-pairs o)
  (for/list ([(k v) (in-dict o)]) (cons k v)))

;; Safe lookup with default value (not thunk)
(define (ordl-ref-or o key default)
  (dict-ref o key default))

;; Map over ordl values
(define (ordl-map-values f o)
  (for/fold ([result o]) ([(k v) (in-dict o)])
    (dict-set result k (f v))))

;; Filter ordl by predicate on key-value pair
(define (ordl-filter pred o)
  (for/fold ([result (ordl-make-empty (Ordl-cmp-fn o))])
            ([(k v) (in-dict o)]
             #:when (pred k v))
    (dict-set result k v)))

;; Ordl struct accessor for comparison function
(define (Ordl-cmp-fn o)
  ;; Access the internal comparison function
  ;; This assumes ordl has a specific structure
  (if (Ordl? o)
      (match o
        [(Ordl cmp _) cmp])
      (error 'Ordl-cmp-fn "Not an ordl: ~a" o)))

(provide ordl-keys ordl-values ordl-pairs ordl-ref-or ordl-map-values ordl-filter)
