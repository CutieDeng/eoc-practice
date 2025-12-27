#lang racket/base

;; ============================================================
;; Compatibility Shim: core/cfg.rkt
;; ============================================================
;;
;; Forwards to new location: ir/cfg/types.rkt
;; This file exists for backward compatibility during migration.
;; ============================================================

(require "../ir/cfg/types.rkt")

(provide (all-from-out "../ir/cfg/types.rkt"))
