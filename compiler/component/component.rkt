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
;;   (require "component/component.rkt")
;;   ; or for fine-grained imports:
;;   (require "component/cfg/cfg.rkt")
;;   (require "component/rvsdg/rvsdg.rkt")
;;
;; ============================================================

(require "common/common.rkt")
(require "cfg/cfg.rkt")
(require "rvsdg/rvsdg.rkt")
(require "lvar/lvar.rkt")
(require "x86var/x86var.rkt")

(provide (all-from-out "common/common.rkt"))
(provide (all-from-out "cfg/cfg.rkt"))
(provide (all-from-out "rvsdg/rvsdg.rkt"))
(provide (all-from-out "lvar/lvar.rkt"))
(provide (all-from-out "x86var/x86var.rkt"))
