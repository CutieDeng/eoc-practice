#lang racket/base

;; ============================================================
;; Compiler Kernel: Unified Export
;; ============================================================
;;
;; The kernel layer provides foundational data structures:
;;
;; 1. Data structures (from cutie-ftree):
;;    - pvector: persistent vector
;;    - ordered-map: ordered dictionary
;;    - bitset: bit set
;;    - graph: immutable graph
;;    - comparators: integer-compare, symbol-compare, string-compare
;;
;; 2. IR type definitions:
;;    - CFG: Control Flow Graph (BlockId, VarId, Cfg, CfgBlock, VfInsn, ...)
;;    - AST: Abstract Syntax Tree (Int, Bool, Let, If, Lambda, ...)
;;    - RVSDG: Regionalized Value State Dependency Graph (Region, NodeId, ...)
;;    - Common: Type representations
;;
;; Design principles:
;;    - Pure data definitions only (no algorithms)
;;    - Zero external dependencies (except cutie-ftree)
;;    - Maximal stability (changes rarely)
;;
;; Usage:
;;    (require "kernel/main.rkt")
;;    ; or for fine-grained imports:
;;    (require "kernel/data/main.rkt")
;;    (require "kernel/ir/cfg/main.rkt")
;;
;; ============================================================

(require "data/main.rkt")
(require "ir/main.rkt")

(provide (all-from-out "data/main.rkt"))
(provide (all-from-out "ir/main.rkt"))
