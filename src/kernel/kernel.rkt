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
;;    (require "kernel/kernel.rkt")
;;    ; or for fine-grained imports:
;;    (require "kernel/data/data.rkt")
;;    (require "kernel/ir/cfg/cfg.rkt")
;;
;; ============================================================

(require "data/data.rkt")
(require "ir/ir.rkt")

(provide (all-from-out "data/data.rkt"))
(provide (all-from-out "ir/ir.rkt"))
