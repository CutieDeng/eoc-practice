#lang racket/base

;; ============================================================
;; Pipeline Layer: Unified Export
;; ============================================================
;;
;; The pipeline layer provides language-specific compilation flows
;; that compose transforms and analyses into complete pipelines.
;;
;; Structure:
;;   - common/ : Pass definition and composition utilities
;;   - x86/    : EoC x86 backend (Lvar → Cvar → x86)
;;   - java/   : Java bytecode compilation pipeline (stub)
;;
;; EoC Pipeline (x86):
;;   Lvar (AST) → uniquify → remove-complex → explicate-control →
;;   Cvar → select-instructions → x86var →
;;   assign-homes → patch-instructions → prelude-conclusion → x86
;;
;; ============================================================

(require "common/common.rkt")
(require "x86/pipeline.rkt")
(require "java/java.rkt")

;; Common utilities
(provide (all-from-out "common/common.rkt"))

;; X86 pipeline (EoC)
(provide (all-from-out "x86/pipeline.rkt"))

;; Java pipeline (stub)
(provide (all-from-out "java/java.rkt"))
