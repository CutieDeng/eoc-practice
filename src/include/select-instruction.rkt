#lang racket

(struct Imm (dst n) #:transparent)
(struct Add (dst src1 src2) #:transparent)
(struct Sub (dst src1) #:transparent)
(struct Mov (dst src) #:transparent)

(struct Reg (name) #:transparent)

(provide (all-defined-out))