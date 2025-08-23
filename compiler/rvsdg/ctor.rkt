#lang racket/base

(require "core-def.rkt")
(require cutie-ftree)

(define (((abstract-compare-generator compare) map) lhs rhs)
  (compare (map lhs) (map rhs))
)

(define integer-compare-generator (abstract-compare-generator integer-compare))

(define wire-compare (integer-compare-generator WireId-id))
(define input-compare (integer-compare-generator InputId-id))
(define output-compare (integer-compare-generator OutputId-id))
(define node-compare (integer-compare-generator NodeId-id))
(provide wire-compare input-compare output-compare node-compare integer-compare-generator abstract-compare-generator)

(define INPUT-NODE-ID 0)
(define OUTPUT-NODE-ID 1)
(define START-NODE-CNT 2)
(provide INPUT-NODE-ID OUTPUT-NODE-ID START-NODE-CNT)

(define Region-empty-instance (Region
  (ordl-make-empty wire-compare)
  (ordl-make-empty wire-compare)
  (ordl-make-empty input-compare)
  (ordl-make-empty input-compare)
  (ordl-make-empty output-compare)
  (ordl-make-empty output-compare)
  (ordl-make-empty node-compare)
  (ordl-make-empty node-compare)
  (ordl-make-empty node-compare)
  0
  0
  0
  START-NODE-CNT
))

(define (Region-empty) Region-empty-instance)
(provide Region-empty)
