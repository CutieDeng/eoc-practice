#lang racket/base

;; ============================================================
;; Component Layer: Unified Export
;; ============================================================
;;
;; The component layer binds parameterized driver algorithms
;; to concrete IR types (CFG, RVSDG).
;;
;; Structure:
;;   - common/  : Shared analysis framework
;;   - cfg/     : CFG-specific components
;;       analysis/  : dominance, liveness, loops
;;       transform/ : traversal utilities
;;       utils/     : graph operations
;;   - rvsdg/   : RVSDG-specific components
;;       utils/     : graph operations
;;       analysis/  : (placeholder)
;;       transform/ : (placeholder)
;;
;; Design principles:
;;   - IR-specific (binds to CFG or RVSDG types)
;;   - Language-agnostic (not tied to source/target language)
;;   - Uses driver algorithms (no reimplementation)
;;   - Provides concrete analysis interfaces
;;
;; Usage:
;;   (require "component/main.rkt")
;;   ; or for fine-grained imports:
;;   (require "component/cfg/main.rkt")
;;   (require "component/rvsdg/main.rkt")
;;
;; ============================================================

(require "common/main.rkt")
(require "cfg/main.rkt")
(require "rvsdg/main.rkt")
(require "lvar/main.rkt")
(require "x86var/main.rkt")

(provide (all-from-out "common/main.rkt"))
(provide (all-from-out "cfg/main.rkt"))
(provide (all-from-out "rvsdg/main.rkt"))
(provide (all-from-out "lvar/main.rkt"))
(provide (all-from-out "x86var/main.rkt"))
