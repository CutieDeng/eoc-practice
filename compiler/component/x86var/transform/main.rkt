#lang racket/base

;; ============================================================
;; Component: X86-var Transforms
;; ============================================================
;;
;; Re-exports all X86-var transformation passes:
;;   - select-instructions: C-var to X86-var
;;   - assign-homes: Allocate variables to stack
;;   - patch-instructions: Fix x86 constraints
;;   - prelude-conclusion: Add prologue/epilogue
;;   - emit-x86: Generate assembly text
;;
;; ============================================================

(require "select-instructions.rkt"
         "assign-homes.rkt"
         "patch-instructions.rkt"
         "prelude-conclusion.rkt"
         "emit-x86.rkt")

(provide (all-from-out "select-instructions.rkt"))
(provide (all-from-out "assign-homes.rkt"))
(provide (all-from-out "patch-instructions.rkt"))
(provide (all-from-out "prelude-conclusion.rkt"))
(provide (all-from-out "emit-x86.rkt"))
