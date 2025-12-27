#lang racket/base

;; ============================================================
;; Compatibility Shim: core/jvm.rkt
;; ============================================================
;;
;; Forwards to new location: ir/jvm/types.rkt
;; This file exists for backward compatibility during migration.
;; ============================================================

(require "../ir/jvm/types.rkt")

(provide (all-from-out "../ir/jvm/types.rkt"))
