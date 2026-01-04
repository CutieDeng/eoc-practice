#lang racket/base

;; ============================================================
;; Common IR Types
;; ============================================================
;;
;; Shared type definitions used across all IR representations.
;; Pure data definitions only - no algorithms or operations.
;;
;; ============================================================

(provide
  ;; Type representations
  (struct-out TypeInt)
  (struct-out TypeBool)
  (struct-out TypeVoid)
  (struct-out TypeVector)
  (struct-out TypeFunction)
  (struct-out TypeAny)
  type?

  ;; Info dictionary helpers
  empty-info
  info?)

(require "../data/data.rkt")

;; ============================================================
;; Type Representations
;; ============================================================

;; Primitive types
(struct TypeInt () #:prefab)
(struct TypeBool () #:prefab)
(struct TypeVoid () #:prefab)
(struct TypeAny () #:prefab)

;; Compound types
(struct TypeVector (element-type) #:prefab)
(struct TypeFunction (param-types return-type) #:prefab)

;; Type predicate
(define (type? x)
  (or (TypeInt? x)
      (TypeBool? x)
      (TypeVoid? x)
      (TypeAny? x)
      (TypeVector? x)
      (TypeFunction? x)))

;; ============================================================
;; Info Dictionary (metadata container)
;; ============================================================

;; Empty info dictionary (using symbol comparison)
(define empty-info (ordered-map-empty symbol-compare))

;; Check if something is a valid info dictionary
(define (info? x)
  (ordered-map? x))
