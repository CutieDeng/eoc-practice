#lang racket/base

;; ============================================================
;; Kernel IR: Unified Export
;; ============================================================
;;
;; Re-exports all IR type definitions:
;;   - common: shared types (Type representations)
;;   - cfg: Control Flow Graph types
;;
;; For AST and RVSDG, use direct imports to avoid conflicts:
;;   (require "kernel/ir/ast/main.rkt")
;;   (require "kernel/ir/rvsdg/main.rkt")
;;
;; Or use prefixes:
;;   (require (prefix-in ast: "kernel/ir/ast/main.rkt"))
;;   (require (prefix-in rvsdg: "kernel/ir/rvsdg/main.rkt"))
;;
;; ============================================================

(require "common.rkt")
(require "cfg/main.rkt")

;; Export common and CFG (primary IR, no conflicts)
(provide (all-from-out "common.rkt"))
(provide (all-from-out "cfg/main.rkt"))

;; For AST and RVSDG, users should import directly
;; to choose their own prefixing strategy
