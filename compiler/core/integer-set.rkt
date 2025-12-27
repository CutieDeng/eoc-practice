#lang racket/base

;; ============================================================
;; Compatibility Shim: core/integer-set.rkt
;; ============================================================
;;
;; Forwards to new location: lib/bset.rkt
;; This file exists for backward compatibility during migration.
;; ============================================================

(require "../lib/bset.rkt")

(provide (all-from-out "../lib/bset.rkt"))
