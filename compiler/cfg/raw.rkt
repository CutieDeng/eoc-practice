#lang racket/base

;; ============================================================
;; Compatibility Shim: cfg/raw.rkt
;; ============================================================
;;
;; Forwards to new location: kernel/ir/cfg/types.rkt
;; This file exists for backward compatibility during migration.
;; ============================================================

(require "../kernel/ir/cfg/types.rkt")

(provide (all-from-out "../kernel/ir/cfg/types.rkt"))
