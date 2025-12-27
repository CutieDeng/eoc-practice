#lang racket/base

;; ============================================================
;; Compatibility Shim: ftree.rkt
;; ============================================================
;;
;; Forwards to new location: lib/ftree.rkt
;; This file exists for backward compatibility during migration.
;; ============================================================

(require "lib/ftree.rkt")

(provide (all-from-out "lib/ftree.rkt"))
