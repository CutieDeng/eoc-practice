#lang racket/base

;; ============================================================
;; Compatibility Shim: cfg/raw.rkt
;; ============================================================
;;
;; Forwards to new location: ir/cfg/raw.rkt
;; This file exists for backward compatibility during migration.
;; Also re-exports types for convenience.
;; ============================================================

(require "../ir/cfg/main.rkt")

(provide (all-from-out "../ir/cfg/main.rkt"))
