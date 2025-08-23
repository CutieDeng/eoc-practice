#lang racket/base

(require racket/match)

(require "core-def.rkt")

(define (rvsdg-raw/output-offset id index) (match id [(OutputId x) (OutputId (+ x index))]))
(provide rvsdg-raw/output-offset)
