#lang racket/base

;; ============================================================
;; Layer 0: Foundation Library
;; ============================================================
;;
;; Unified entry point for all foundation data structures:
;;   - ftree.rkt: Finger tree based structures (ral, ordl)
;;   - graph.rkt: Immutable graph data structure
;;
;; Usage:
;;   (require "lib/main.rkt")    ; from compiler/
;;   (require "../lib/main.rkt") ; from compiler/*/
;; ============================================================

(require "ftree.rkt")
(require "graph.rkt")
(require "bset.rkt")

(provide (all-from-out "ftree.rkt"))
(provide (all-from-out "graph.rkt"))
(provide (all-from-out "bset.rkt"))
