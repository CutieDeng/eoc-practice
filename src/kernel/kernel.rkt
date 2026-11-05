#lang racket/base

;; ============================================================
;; Compiler Kernel: Unified Export
;; ============================================================
;;
;; The kernel layer provides foundational data structures:
;;
;; 1. Data structures (Racket standard library, via data/data.rkt):
;;    - pvector: persistent vector
;;    - idmap: typed-id keyed table (intmap underneath)
;;    - intmap / intbits: integer-keyed map / bit set
;;    - graph: immutable graph (aarch64 AsmCfg only)
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
;;    - Zero dependencies outside the Racket standard library
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
(require (except-in "ir/ir.rkt" integer-compare))

(provide (all-from-out "data/data.rkt"))
(provide (all-from-out "ir/ir.rkt"))
