#lang racket/base

;; ============================================================
;; Foundation: Finger Tree Data Structures
;; ============================================================
;;
;; Re-exports cutie-ftree library:
;;   - pvector (persistent vector)
;;   - ordered-map
;;   - bitset
;;   - graph
;;   - comparator
;;
;; ============================================================

(require racket/match racket/dict)
(require (for-syntax racket/base))
(require "../../cutie-ftree/main.rkt")

;; Re-export all from cutie-ftree
(provide (all-from-out "../../cutie-ftree/main.rkt"))

;; ============================================================
;; Compatibility Aliases: Ordl → Ordered-Map
;; ============================================================

(define ordl-make-empty ordered-map-empty)
(define ordl-empty? ordered-map-empty?)
(define Ordl? ordered-map?)
(define ordl-query ordered-map-query)
(define ordl-query-weak ordered-map-query-weak)
(define ordl-delete ordered-map-delete)
(define in-ordl in-ordered-map)
(define in-ordl/rev in-ordered-map-reverse)

;; ordl-insert has different signature: (ordl key val replace?) vs ordered-map-set
(define (ordl-insert o key val [replace? #t])
  (if replace?
      (ordered-map-set o key val)
      (if (ordered-map-has-key? o key)
          o
          (ordered-map-set o key val))))

;; Ordl struct compatibility - match expander
(define-match-expander Ordl
  (lambda (stx)
    (syntax-case stx ()
      [(_ cmp-pat tree-pat)
       #'(ordered-map cmp-pat tree-pat)])))

;; Ordl-cmp-fn accessor
(define (Ordl-cmp-fn o)
  (ordered-map-cmp-fn o))

;; ordl-min and ordl-max return the min/max key-value pair
(define (ordl-min o)
  (ordered-map-min o))

(define (ordl-max o)
  (ordered-map-max o))

(provide ordl-make-empty ordl-empty? Ordl? ordl-query ordl-query-weak ordl-delete)
(provide ordl-insert Ordl Ordl-cmp-fn)
(provide ordl-min ordl-max)
(provide in-ordl in-ordl/rev)

;; ============================================================
;; Pre-defined Constructors
;; ============================================================

;; Create an empty ordered-map for integer keys
(define (integer-ordl)
  (ordered-map-empty integer-compare))

;; Create an empty ordered-map for symbol keys
(define (symbol-ordl)
  (ordered-map-empty symbol-compare))

;; Create an empty ordered-map for string keys
(define (string-ordl)
  (ordered-map-empty string-compare))

(provide integer-ordl symbol-ordl string-ordl)

;; ============================================================
;; Ordl Utilities
;; ============================================================

;; Get all keys from ordl
(define (ordl-keys o)
  (ordered-map-keys o))

;; Get all values from ordl
(define (ordl-values o)
  (ordered-map-values o))

;; Get all key-value pairs from ordl
(define (ordl-pairs o)
  (for/list ([kv (in-ordered-map o)]) kv))

;; Safe lookup with default value (not thunk)
(define (ordl-ref-or o key default)
  (ordered-map-ref o key default))

;; Map over ordl values
(define (ordl-map-values f o)
  (for/fold ([result o]) ([kv (in-ordered-map o)])
    (ordered-map-set result (car kv) (f (cdr kv)))))

;; Filter ordl by predicate on key-value pair
(define (ordl-filter pred o)
  (for/fold ([result (ordered-map-empty (ordered-map-cmp-fn o))])
            ([kv (in-ordered-map o)]
             #:when (pred (car kv) (cdr kv)))
    (ordered-map-set result (car kv) (cdr kv))))

(provide ordl-keys ordl-values ordl-pairs ordl-ref-or ordl-map-values ordl-filter)
