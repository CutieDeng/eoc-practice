#lang racket

(struct Return (value) #:transparent)
(struct Assign (var val) #:transparent)
(struct Seq (lhs rhs) #:transparent)

(provide (all-defined-out))
