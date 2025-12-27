#lang racket/base

;; ============================================================
;; Layer 1: Intermediate Representation (IR)
;; ============================================================
;;
;; Central module for all IR definitions:
;;   - CFG: Control Flow Graph (SSA form)
;;   - JVM: JVM bytecode representation
;;   - RVSDG: (to be added)
;;
;; Usage:
;;   (require "ir/main.rkt")
;; ============================================================

(require "cfg/main.rkt")
(require "jvm/main.rkt")

(provide (all-from-out "cfg/main.rkt"))
(provide (all-from-out "jvm/main.rkt"))
