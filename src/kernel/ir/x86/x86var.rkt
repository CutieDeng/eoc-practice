#lang racket/base

;; ============================================================
;; X86-var IR Type Definitions
;; ============================================================
;;
;; X86 with pseudo-registers (variables).
;; Before register allocation / assign-homes.
;;
;; ============================================================

(require "types.rkt")

(provide
  ;; Re-export x86 types
  (all-from-out "types.rkt")

  ;; Pseudo-register (variable)
  (struct-out Var)

  ;; Predicates
  x86var-arg?)

;; ============================================================
;; Pseudo-registers
;; ============================================================

;; Variable (pseudo-register before allocation)
(struct Var (name) #:prefab)

;; ============================================================
;; Predicates
;; ============================================================

(define (x86var-arg? x)
  (or (arg? x)
      (Var? x)))
