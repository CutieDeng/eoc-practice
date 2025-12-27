#lang racket/base

;; ============================================================
;; Abstract Instruction Semantics Interface
;; ============================================================
;;
;; This module defines abstract interfaces for instruction semantics.
;; Different IR representations (JVM, LLVM, custom) can provide
;; concrete implementations.
;;
;; Design Philosophy:
;;   - Optimization algorithms should not know about specific opcodes
;;   - Semantics providers describe what operations DO, not what they ARE
;;   - This enables reuse across different frontends and IRs
;; ============================================================

(require racket/match racket/contract)
(require "../../ir/cfg/types.rkt")

;; ============================================================
;; Instruction Categories
;; ============================================================

;; Memory effect classification
(define memory-effect/c
  (or/c 'none      ; No memory access
        'read      ; Reads from memory
        'write     ; Writes to memory
        'read-write ; Both reads and writes
        'unknown)) ; Conservative assumption

;; Purity classification
(define purity/c
  (or/c 'pure           ; No side effects, same inputs → same outputs
        'read-only      ; May read memory, no writes
        'side-effecting ; Has observable side effects
        'unknown))      ; Conservative assumption

;; Control flow effect
(define control-effect/c
  (or/c 'none       ; Normal sequential flow
        'branch     ; Conditional branch
        'call       ; Function call (may not return)
        'throw      ; May throw exception
        'terminate  ; Terminates execution
        'unknown))

(provide memory-effect/c purity/c control-effect/c)

;; ============================================================
;; Memory Location Abstraction
;; ============================================================

;; Abstract memory location for alias analysis
(struct MemLoc (
  kind      ; 'local | 'heap | 'static | 'unknown
  base      ; VarId or symbol identifying the base
  offset    ; Integer offset or 'unknown
  size      ; Integer size or 'unknown
) #:prefab)

(define mem-unknown (MemLoc 'unknown 'unknown 'unknown 'unknown))

(provide (struct-out MemLoc) mem-unknown)

;; ============================================================
;; Instruction Semantics Provider Interface
;; ============================================================

;; A semantics provider describes the behavior of instructions
;; without exposing the specific opcode names

(struct InsnSemantics (
  ;; Memory effects
  memory-effect     ; VfInsn -> memory-effect/c

  ;; Memory location extraction (for alias analysis)
  read-locations    ; VfInsn -> (Listof MemLoc)
  write-locations   ; VfInsn -> (Listof MemLoc)

  ;; Purity analysis
  purity            ; VfInsn -> purity/c

  ;; Control flow
  control-effect    ; VfInsn -> control-effect/c

  ;; Value properties
  is-constant?      ; VfInsn -> Boolean (produces constant value)
  is-copy?          ; VfInsn -> Boolean (simple variable copy)
  is-commutative?   ; VfInsn -> Boolean (operand order doesn't matter)
  is-associative?   ; VfInsn -> Boolean (grouping doesn't matter)

  ;; Expression identity (for GVN/CSE)
  ;; Returns a canonical key for the expression
  expr-key          ; VfInsn -> Any (comparable key for expression)

  ;; Constant evaluation
  ;; Attempts to evaluate instruction with constant inputs
  try-const-eval    ; VfInsn × (VarId -> (or/c Integer #f)) -> (or/c value #f)

  ;; Algebraic simplification patterns
  ;; Returns simplified instruction or #f if no simplification
  try-simplify      ; VfInsn × (VarId -> VfInsn) -> (or/c VfInsn #f)
) #:transparent)

(provide (struct-out InsnSemantics))

;; ============================================================
;; Semantics Query Helpers
;; ============================================================

;; Query memory effect
(define (sem-memory-effect sem insn)
  ((InsnSemantics-memory-effect sem) insn))

;; Query read locations
(define (sem-read-locs sem insn)
  ((InsnSemantics-read-locations sem) insn))

;; Query write locations
(define (sem-write-locs sem insn)
  ((InsnSemantics-write-locations sem) insn))

;; Query purity
(define (sem-purity sem insn)
  ((InsnSemantics-purity sem) insn))

;; Check if instruction is pure
(define (sem-pure? sem insn)
  (eq? (sem-purity sem insn) 'pure))

;; Check if instruction reads memory
(define (sem-reads-memory? sem insn)
  (memq (sem-memory-effect sem insn) '(read read-write unknown)))

;; Check if instruction writes memory
(define (sem-writes-memory? sem insn)
  (memq (sem-memory-effect sem insn) '(write read-write unknown)))

;; Check if instruction has side effects
(define (sem-has-side-effects? sem insn)
  (not (memq (sem-purity sem insn) '(pure read-only))))

;; Check if instruction is a constant producer
(define (sem-constant? sem insn)
  ((InsnSemantics-is-constant? sem) insn))

;; Check if instruction is a simple copy
(define (sem-copy? sem insn)
  ((InsnSemantics-is-copy? sem) insn))

;; Check if instruction is commutative
(define (sem-commutative? sem insn)
  ((InsnSemantics-is-commutative? sem) insn))

;; Check if instruction is associative
(define (sem-associative? sem insn)
  ((InsnSemantics-is-associative? sem) insn))

;; Get expression key for value numbering
(define (sem-expr-key sem insn)
  ((InsnSemantics-expr-key sem) insn))

;; Try constant evaluation
(define (sem-const-eval sem insn var->const)
  ((InsnSemantics-try-const-eval sem) insn var->const))

;; Try algebraic simplification
(define (sem-simplify sem insn var->def)
  ((InsnSemantics-try-simplify sem) insn var->def))

(provide sem-memory-effect sem-read-locs sem-write-locs
         sem-purity sem-pure?
         sem-reads-memory? sem-writes-memory? sem-has-side-effects?
         sem-constant? sem-copy? sem-commutative? sem-associative?
         sem-expr-key sem-const-eval sem-simplify)

;; ============================================================
;; Alias Analysis Interface
;; ============================================================

;; Alias query result
(define alias-result/c (or/c 'no-alias 'may-alias 'must-alias))

;; Check if two memory locations may alias
(define (locs-may-alias? loc1 loc2)
  (cond
    ;; Either unknown → conservative may-alias
    [(or (eq? (MemLoc-kind loc1) 'unknown)
         (eq? (MemLoc-kind loc2) 'unknown))
     'may-alias]

    ;; Different kinds → no alias (locals vs heap vs static)
    [(not (eq? (MemLoc-kind loc1) (MemLoc-kind loc2)))
     'no-alias]

    ;; Same kind - check base and offset
    [else
     (define base1 (MemLoc-base loc1))
     (define base2 (MemLoc-base loc2))
     (define off1 (MemLoc-offset loc1))
     (define off2 (MemLoc-offset loc2))

     (cond
       ;; Unknown base → may-alias
       [(or (eq? base1 'unknown) (eq? base2 'unknown))
        'may-alias]

       ;; Different base variables
       [(not (equal? base1 base2))
        ;; For heap objects, different bases may still alias
        (if (eq? (MemLoc-kind loc1) 'heap)
            'may-alias
            'no-alias)]

       ;; Same base, check offsets
       [(or (eq? off1 'unknown) (eq? off2 'unknown))
        'may-alias]

       [(equal? off1 off2)
        'must-alias]

       ;; Different offsets - check for overlap
       [else
        (define sz1 (MemLoc-size loc1))
        (define sz2 (MemLoc-size loc2))
        (if (and (integer? sz1) (integer? sz2))
            (if (or (<= (+ off1 sz1) off2)
                    (<= (+ off2 sz2) off1))
                'no-alias
                'may-alias)
            'may-alias)])]))

(provide alias-result/c locs-may-alias?)

;; Check if instructions may alias
(define (insns-may-alias? sem insn1 insn2)
  (define locs1 (append (sem-read-locs sem insn1)
                        (sem-write-locs sem insn1)))
  (define locs2 (append (sem-read-locs sem insn2)
                        (sem-write-locs sem insn2)))

  (for*/or ([l1 locs1]
            [l2 locs2])
    (not (eq? (locs-may-alias? l1 l2) 'no-alias))))

(provide insns-may-alias?)

;; ============================================================
;; Default/Null Semantics Provider
;; ============================================================

;; Conservative semantics that assumes worst case for everything
;; Useful as a fallback or for testing
(define null-semantics
  (InsnSemantics
   ;; memory-effect: assume read-write
   (lambda (insn) 'unknown)
   ;; read-locations: unknown
   (lambda (insn) (list mem-unknown))
   ;; write-locations: unknown
   (lambda (insn) (list mem-unknown))
   ;; purity: assume side-effecting
   (lambda (insn) 'unknown)
   ;; control-effect: assume unknown
   (lambda (insn) 'unknown)
   ;; is-constant?: no
   (lambda (insn) #f)
   ;; is-copy?: no
   (lambda (insn) #f)
   ;; is-commutative?: no
   (lambda (insn) #f)
   ;; is-associative?: no
   (lambda (insn) #f)
   ;; expr-key: use instruction itself
   (lambda (insn) insn)
   ;; try-const-eval: can't evaluate
   (lambda (insn var->const) #f)
   ;; try-simplify: can't simplify
   (lambda (insn var->def) #f)))

(provide null-semantics)
