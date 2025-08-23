#lang racket/base

(require racket/match)

(require "core-def.rkt")

(define (rvsdg-raw/input-offset id index) (match id [(InputId x) (InputId (+ x index))]))
(provide rvsdg-raw/input-offset)
