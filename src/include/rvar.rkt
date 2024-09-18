#lang racket

(require "prim.rkt")

(struct Let (var exp body) #:transparent)
(struct Var (var) #:transparent)

(provide (all-defined-out))
