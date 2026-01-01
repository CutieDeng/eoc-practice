#lang racket/base

;; ============================================================
;; RVSDG Core - Compatibility Stub
;; ============================================================
;;
;; This file provides compatibility for legacy code.
;; The new implementation is in core-def.rkt and core-ctx.rkt.
;;
;; This module is deprecated - do not use in new code.
;; ============================================================

(require "../lib/ftree.rkt")
(require "core-def.rkt")

;; Re-export core definitions for backward compatibility
(provide (all-from-out "core-def.rkt"))
;; Note: ftree.rkt exports are not re-exported to avoid duplicate identifier conflicts

;; === Legacy Types (Stubs) ===

;; These structs were used in the old interpreter.
;; They are kept here only for compatibility.

(struct Graph (
  nodes        ; ordl: id -> node
  input-src    ; ordl: input-id -> output-id
  input-loc    ; ordl: input-id -> node-id
  output-loc   ; ordl: output-id -> node-id
  parent       ; ordl: node-id -> parent-node-id
) #:transparent)

(struct Literal (value output) #:transparent)
(struct Primitive (op n m input output) #:transparent)

;; === Legacy Constructors (Stubs) ===

(define (empty-graph)
  (Graph (ordl-make-empty integer-compare)
         (ordl-make-empty integer-compare)
         (ordl-make-empty integer-compare)
         (ordl-make-empty integer-compare)
         (ordl-make-empty integer-compare)))

(define (create-omega graph n m)
  (error 'create-omega "Legacy function - not implemented"))

(define (create-literal graph value)
  (error 'create-literal "Legacy function - not implemented"))

(define (create-primitive graph op n m)
  (error 'create-primitive "Legacy function - not implemented"))

(define (connect-edge-raw graph from to)
  (error 'connect-edge-raw "Legacy function - not implemented"))

(define (set-parent-raw graph child parent)
  (error 'set-parent-raw "Legacy function - not implemented"))

;; === Exports ===

(provide (struct-out Graph))
(provide (struct-out Literal))
(provide (struct-out Primitive))

(provide empty-graph)
(provide create-omega create-literal create-primitive)
(provide connect-edge-raw set-parent-raw)
