#lang racket/base

;; ============================================================
;; Pipeline: ID Generator
;; ============================================================
;;
;; Pure functional ID generator for compiler passes.
;; Provides unique sequential IDs for variables, blocks, etc.
;;
;; Usage:
;;   (define gen (make-id-generator 0))
;;   (define-values (id1 gen1) (id-generator-next gen))
;;   (define-values (id2 gen2) (id-generator-next gen1))
;;   ; id1 = 0, id2 = 1
;;
;; ============================================================

(provide
  ;; Constructor
  make-id-generator

  ;; Operations
  id-generator-next      ; gen -> (values id new-gen)
  id-generator-current   ; gen -> id (peek without advancing)
  id-generator-skip      ; gen n -> new-gen (skip n ids)

  ;; Predicate
  id-generator?)

;; ============================================================
;; Implementation
;; ============================================================

(struct IdGenerator (counter) #:transparent)

;; make-id-generator : [integer] -> IdGenerator
;; Create a new ID generator starting at the given value (default 0)
(define (make-id-generator [start 0])
  (IdGenerator start))

;; id-generator? : any -> boolean
(define (id-generator? x)
  (IdGenerator? x))

;; id-generator-next : IdGenerator -> (values integer IdGenerator)
;; Return the next ID and an updated generator
(define (id-generator-next gen)
  (define current (IdGenerator-counter gen))
  (values current (IdGenerator (+ current 1))))

;; id-generator-current : IdGenerator -> integer
;; Peek at the current ID without advancing
(define (id-generator-current gen)
  (IdGenerator-counter gen))

;; id-generator-skip : IdGenerator integer -> IdGenerator
;; Skip n IDs (useful for reserving ID ranges)
(define (id-generator-skip gen n)
  (IdGenerator (+ (IdGenerator-counter gen) n)))
