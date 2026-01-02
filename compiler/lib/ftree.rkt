#lang racket/base

;; ============================================================
;; Foundation: Finger Tree Data Structures
;; ============================================================
;;
;; Re-exports cutie-ftree library with compatibility aliases:
;;   - pvector (with ral-* aliases for compatibility)
;;   - ordered-map (with ordl-* aliases for compatibility)
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
;; Compatibility Aliases: RAL → PVector
;; ============================================================

(define ral-empty (pvector-empty))
(define ral-empty? pvector-empty?)
(define ral? pvector?)
(define ral-length pvector-length)
(define ral-consl pvector-cons-left)
(define ral-consr pvector-cons-right)
(define ral-dropl pvector-pop-left)   ; pop-left returns (values elem rest)
(define ral-dropr pvector-pop-right)  ; pop-right returns (values rest elem)
(define ral-viewl pvector-view-left)
(define ral-viewr pvector-view-right)
(define ral-ref pvector-ref)
(define ral-set pvector-set)
(define ral-append pvector-append)
(define ral-split pvector-split)
(define ral-take pvector-take)
(define ral-drop pvector-drop)
(define ral-take-right pvector-take-right)
(define ral-drop-right pvector-drop-right)
(define ral-split-at pvector-split-at)
(define ral-split-at-right pvector-split-at-right)
(define ral-copy pvector-copy)
(define ral-insert pvector-insert)
(define ral-delete-ft pvector-delete)
(define in-ral0 in-pvector)
(define vector->ral vector->pvector)
(define ral->vector pvector->vector)

(provide ral-empty ral-empty? ral? ral-length)
(provide ral-consl ral-consr ral-dropl ral-dropr)
(provide ral-viewl ral-viewr)
(provide ral-ref ral-set ral-append)
(provide ral-split ral-take ral-drop)
(provide ral-take-right ral-drop-right)
(provide ral-split-at ral-split-at-right)
(provide ral-copy ral-insert ral-delete-ft)
(provide in-ral0 vector->ral ral->vector)

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
;; Pre-defined Constructors (using compatibility names)
;; ============================================================

;; Create an empty ordl for integer keys
(define (integer-ordl)
  (ordered-map-empty integer-compare))

;; Create an empty ordl for symbol keys
(define (symbol-ordl)
  (ordered-map-empty symbol-compare))

;; Create an empty ordl for string keys
(define (string-ordl)
  (ordered-map-empty string-compare))

(provide integer-ordl symbol-ordl string-ordl)

;; ============================================================
;; RAL Utilities
;; ============================================================

;; Create a single-element ral
(define (ral-single x)
  (pvector-cons-left pvector-empty x))

;; Convert a list to ral (preserving order)
(define (list->ral lst)
  (list->pvector lst))

;; Convert ral to list
(define (ral->list r)
  (pvector->list r))

;; Map over ral
(define (ral-map f r)
  (list->pvector (map f (pvector->list r))))

;; Filter ral
(define (ral-filter pred r)
  (list->pvector (filter pred (pvector->list r))))

;; Fold over ral (left to right)
(define (ral-foldl f init r)
  (foldl f init (pvector->list r)))

;; Fold over ral (right to left)
(define (ral-foldr f init r)
  (foldr f init (pvector->list r)))

(provide ral-single list->ral ral->list ral-map ral-filter ral-foldl ral-foldr)

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

;; ============================================================
;; RAL Match Expander (Legacy compatibility)
;; ============================================================
;;
;; Supports patterns like:
;;   (ral) - empty
;;   (ral (x atom)) - single element
;;   (ral (a atom) (rest unlength)) - first and rest
;;   (ral (rest unlength) (x atom)) - rest and last
;;   (ral (_ unlength) (x atom) (y atom)) - rest, second-to-last, last
;;
;; Syntax:
;;   (pat atom) - matches exactly one element
;;   (pat unlength) - matches zero or more elements (rest pattern)

(require (for-syntax racket/list))

(begin-for-syntax
  ;; Check if a segment is an atom pattern
  (define (atom-segment? seg)
    (syntax-case seg (atom)
      [(_ atom) #t]
      [_ #f]))

  ;; Check if a segment is an unlength pattern
  (define (unlength-segment? seg)
    (syntax-case seg (unlength)
      [(_ unlength) #t]
      [_ #f]))

  ;; Extract the pattern from a segment
  (define (segment-pattern seg)
    (syntax-case seg (atom unlength)
      [(pat atom) #'pat]
      [(pat unlength) #'pat]))

  ;; Generate match code for ral patterns
  (define (generate-ral-match segments)
    (define seg-list (syntax->list segments))
    (cond
      ;; Empty: (ral)
      [(null? seg-list)
       #'(? pvector-empty?)]

      ;; All atoms: (ral (a atom) (b atom) ...)
      [(andmap atom-segment? seg-list)
       (with-syntax ([(pat ...) (map segment-pattern seg-list)]
                     [n (length seg-list)])
         #'(? pvector?
              (? (lambda (pv) (= (pvector-length pv) n)))
              (app pvector->list (list pat ...))))]

      ;; Single unlength at the end: (ral (a atom) ... (rest unlength))
      [(and (unlength-segment? (last seg-list))
            (andmap atom-segment? (drop-right seg-list 1)))
       (define atom-segs (drop-right seg-list 1))
       (define unlength-seg (last seg-list))
       (with-syntax ([(atom-pat ...) (map segment-pattern atom-segs)]
                     [rest-pat (segment-pattern unlength-seg)]
                     [atom-count (length atom-segs)])
         #'(? pvector?
              (? (lambda (pv) (>= (pvector-length pv) atom-count)))
              (app (lambda (pv)
                     (list (pvector->list (pvector-take pv atom-count))
                           (pvector-drop pv atom-count)))
                   (list (list atom-pat ...) rest-pat))))]

      ;; Single unlength at the start: (ral (rest unlength) (a atom) ...)
      [(and (unlength-segment? (car seg-list))
            (andmap atom-segment? (cdr seg-list)))
       (define unlength-seg (car seg-list))
       (define atom-segs (cdr seg-list))
       (with-syntax ([rest-pat (segment-pattern unlength-seg)]
                     [(atom-pat ...) (map segment-pattern atom-segs)]
                     [atom-count (length atom-segs)])
         #'(? pvector?
              (? (lambda (pv) (>= (pvector-length pv) atom-count)))
              (app (lambda (pv)
                     (define len (pvector-length pv))
                     (list (pvector-take pv (- len atom-count))
                           (pvector->list (pvector-drop pv (- len atom-count)))))
                   (list rest-pat (list atom-pat ...)))))]

      ;; General case: unlength in the middle or complex pattern
      [else
       (error 'ral "unsupported ral pattern combination")])))

(define-match-expander ral
  (lambda (stx)
    (syntax-case stx ()
      [(_) #'(? pvector-empty?)]
      [(_ seg ...)
       (generate-ral-match #'(seg ...))]))
  ;; Constructor form: (ral elem ...) creates a pvector
  (lambda (stx)
    (syntax-case stx ()
      [(_ elem ...) #'(list->pvector (list elem ...))]
      [_ #'list->pvector])))

(provide ral)
