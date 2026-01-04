#lang racket/base

;; AArch64 Assembly IR Types
;;
;; This module defines the core IR types for the aarch64-asm enhanced language.
;; All types use #:prefab for serializability and pattern matching.

(require racket/match)

(provide
 ;; General-purpose registers
 Reg:x Reg:x? Reg:x-id
 Reg:w Reg:w? Reg:w-id
 Reg:sp Reg:sp?
 Reg:xzr Reg:xzr?
 Reg:wzr Reg:wzr?

 ;; NEON/SIMD registers
 Reg:v Reg:v? Reg:v-id Reg:v-width
 Reg:b Reg:b? Reg:b-id
 Reg:h Reg:h? Reg:h-id
 Reg:s Reg:s? Reg:s-id
 Reg:d Reg:d? Reg:d-id
 Reg:q Reg:q? Reg:q-id

 ;; SVE registers
 Reg:z Reg:z? Reg:z-id
 Reg:p Reg:p? Reg:p-id
 Reg:ffr Reg:ffr?

 ;; Virtual registers (for register allocation)
 VReg:gpr VReg:gpr? VReg:gpr-id VReg:gpr-width
 VReg:vec VReg:vec? VReg:vec-id
 VReg:sve VReg:sve? VReg:sve-id
 VReg:pred VReg:pred? VReg:pred-id
 vreg? vreg-id vreg-class

 ;; Register predicates
 gpr? simd-reg? sve-reg? pred-reg? any-reg? physical-reg?

 ;; Immediate values
 Imm Imm? Imm-value
 Imm:shifted Imm:shifted? Imm:shifted-value Imm:shifted-shift

 ;; Memory addressing modes
 Mem:base Mem:base? Mem:base-reg
 Mem:offset Mem:offset? Mem:offset-reg Mem:offset-offset
 Mem:pre Mem:pre? Mem:pre-reg Mem:pre-offset
 Mem:post Mem:post? Mem:post-reg Mem:post-offset
 Mem:reg Mem:reg? Mem:reg-base Mem:reg-index
 Mem:scaled Mem:scaled? Mem:scaled-base Mem:scaled-index Mem:scaled-scale
 mem-addr?

 ;; Labels
 Label:id Label:id? Label:id-id
 Label:named Label:named? Label:named-name
 Label:local Label:local? Label:local-parent Label:local-id
 label?

 ;; Operand type
 operand?

 ;; Instructions
 AsmInsn AsmInsn? AsmInsn-op AsmInsn-args
 Insn:arith Insn:arith? Insn:arith-op Insn:arith-dst Insn:arith-src1 Insn:arith-src2
 Insn:arith2 Insn:arith2? Insn:arith2-op Insn:arith2-dst Insn:arith2-src
 Insn:load Insn:load? Insn:load-op Insn:load-dst Insn:load-addr
 Insn:store Insn:store? Insn:store-op Insn:store-src Insn:store-addr
 Insn:ldp Insn:ldp? Insn:ldp-op Insn:ldp-dst1 Insn:ldp-dst2 Insn:ldp-addr
 Insn:stp Insn:stp? Insn:stp-op Insn:stp-src1 Insn:stp-src2 Insn:stp-addr
 Insn:branch Insn:branch? Insn:branch-op Insn:branch-target
 Insn:cond-branch Insn:cond-branch? Insn:cond-branch-op Insn:cond-branch-cond Insn:cond-branch-target
 Insn:cbz Insn:cbz? Insn:cbz-op Insn:cbz-reg Insn:cbz-target
 Insn:cmp Insn:cmp? Insn:cmp-op Insn:cmp-src1 Insn:cmp-src2
 Insn:csel Insn:csel? Insn:csel-op Insn:csel-dst Insn:csel-src1 Insn:csel-src2 Insn:csel-cond
 Insn:mov Insn:mov? Insn:mov-op Insn:mov-dst Insn:mov-src
 Insn:ret Insn:ret?

 ;; SVE instructions
 Insn:sve Insn:sve? Insn:sve-op Insn:sve-pred Insn:sve-dst Insn:sve-srcs
 Insn:sve-load Insn:sve-load? Insn:sve-load-op Insn:sve-load-pred Insn:sve-load-dst Insn:sve-load-addr
 Insn:sve-store Insn:sve-store? Insn:sve-store-op Insn:sve-store-pred Insn:sve-store-src Insn:sve-store-addr
 Insn:sve-reduce Insn:sve-reduce? Insn:sve-reduce-op Insn:sve-reduce-pred Insn:sve-reduce-dst Insn:sve-reduce-src
 Insn:sve-cmp Insn:sve-cmp? Insn:sve-cmp-op Insn:sve-cmp-pd Insn:sve-cmp-pg Insn:sve-cmp-src1 Insn:sve-cmp-src2
 Insn:sve-pred-op Insn:sve-pred-op? Insn:sve-pred-op-op Insn:sve-pred-op-pd Insn:sve-pred-op-pg Insn:sve-pred-op-pn
 Insn:whilelt Insn:whilelt? Insn:whilelt-pd Insn:whilelt-rn Insn:whilelt-rm

 ;; Instruction predicate
 insn?

 ;; Condition codes
 condition-code?

 ;; Function definition
 AsmFunction AsmFunction? AsmFunction-name AsmFunction-params AsmFunction-ret-type AsmFunction-body
 AsmParam AsmParam? AsmParam-reg AsmParam-type

 ;; Types for type annotations
 Type:scalar Type:scalar? Type:scalar-width Type:scalar-signed?
 Type:float Type:float? Type:float-width
 Type:vec Type:vec? Type:vec-elem Type:vec-lanes
 Type:sve Type:sve? Type:sve-elem
 Type:sve2 Type:sve2? Type:sve2-elem
 Type:pred Type:pred?
 Type:ptr Type:ptr? Type:ptr-elem
 Type:void Type:void?
 asm-type?)

;; ============================================================================
;; General-purpose Registers (x0-x30, w0-w30)
;; ============================================================================

;; 64-bit general purpose register
(struct Reg:x (id) #:prefab)

;; 32-bit general purpose register (lower 32 bits of x register)
(struct Reg:w (id) #:prefab)

;; Stack pointer
(struct Reg:sp () #:prefab)

;; Zero registers
(struct Reg:xzr () #:prefab)  ; 64-bit zero
(struct Reg:wzr () #:prefab)  ; 32-bit zero

;; ============================================================================
;; NEON/SIMD Registers (v0-v31 with different widths)
;; ============================================================================

;; General vector register with width specifier
;; width is one of: 'b (8-bit), 'h (16-bit), 's (32-bit), 'd (64-bit), 'q (128-bit)
(struct Reg:v (id width) #:prefab)

;; Specific width accessors (for convenience)
(struct Reg:b (id) #:prefab)  ; 8-bit
(struct Reg:h (id) #:prefab)  ; 16-bit
(struct Reg:s (id) #:prefab)  ; 32-bit
(struct Reg:d (id) #:prefab)  ; 64-bit
(struct Reg:q (id) #:prefab)  ; 128-bit

;; ============================================================================
;; SVE Registers
;; ============================================================================

;; Scalable vector register (z0-z31)
(struct Reg:z (id) #:prefab)

;; Predicate register (p0-p15)
(struct Reg:p (id) #:prefab)

;; First fault register
(struct Reg:ffr () #:prefab)

;; ============================================================================
;; Virtual Registers (for register allocation)
;; ============================================================================

;; Virtual GPR (will be allocated to x0-x30 or w0-w30)
;; id: unique identifier (symbol or number)
;; width: 32 or 64
(struct VReg:gpr (id width) #:prefab)

;; Virtual vector register (will be allocated to v0-v31)
(struct VReg:vec (id) #:prefab)

;; Virtual SVE register (will be allocated to z0-z31)
(struct VReg:sve (id) #:prefab)

;; Virtual predicate register (will be allocated to p0-p15)
(struct VReg:pred (id) #:prefab)

;; Virtual register predicates
(define (vreg? x)
  (or (VReg:gpr? x) (VReg:vec? x) (VReg:sve? x) (VReg:pred? x)))

;; Get virtual register id
(define (vreg-id v)
  (cond
    [(VReg:gpr? v) (VReg:gpr-id v)]
    [(VReg:vec? v) (VReg:vec-id v)]
    [(VReg:sve? v) (VReg:sve-id v)]
    [(VReg:pred? v) (VReg:pred-id v)]
    [else (error 'vreg-id "not a virtual register: ~a" v)]))

;; Get virtual register class
(define (vreg-class v)
  (cond
    [(VReg:gpr? v) 'gpr]
    [(VReg:vec? v) 'vec]
    [(VReg:sve? v) 'sve]
    [(VReg:pred? v) 'pred]
    [else (error 'vreg-class "not a virtual register: ~a" v)]))

;; ============================================================================
;; Register Predicates
;; ============================================================================

(define (gpr? x)
  (or (Reg:x? x) (Reg:w? x) (Reg:sp? x) (Reg:xzr? x) (Reg:wzr? x)))

(define (simd-reg? x)
  (or (Reg:v? x) (Reg:b? x) (Reg:h? x) (Reg:s? x) (Reg:d? x) (Reg:q? x)))

(define (sve-reg? x)
  (or (Reg:z? x) (Reg:ffr? x)))

(define (pred-reg? x)
  (Reg:p? x))

;; Physical register predicate
(define (physical-reg? x)
  (or (gpr? x) (simd-reg? x) (sve-reg? x) (pred-reg? x)))

;; Any register (physical or virtual)
(define (any-reg? x)
  (or (physical-reg? x) (vreg? x)))

;; ============================================================================
;; Immediate Values
;; ============================================================================

;; Plain immediate
(struct Imm (value) #:prefab)

;; Shifted immediate (value << shift)
(struct Imm:shifted (value shift) #:prefab)

;; ============================================================================
;; Memory Addressing Modes
;; ============================================================================

;; Base register only: [Xn]
(struct Mem:base (reg) #:prefab)

;; Base + immediate offset: [Xn, #imm]
(struct Mem:offset (reg offset) #:prefab)

;; Pre-indexed: [Xn, #imm]!
(struct Mem:pre (reg offset) #:prefab)

;; Post-indexed: [Xn], #imm
(struct Mem:post (reg offset) #:prefab)

;; Register offset: [Xn, Xm]
(struct Mem:reg (base index) #:prefab)

;; Scaled register offset: [Xn, Xm, LSL #scale]
(struct Mem:scaled (base index scale) #:prefab)

(define (mem-addr? x)
  (or (Mem:base? x) (Mem:offset? x) (Mem:pre? x)
      (Mem:post? x) (Mem:reg? x) (Mem:scaled? x)))

;; ============================================================================
;; Labels
;; ============================================================================

;; Numeric label (internal CFG use)
(struct Label:id (id) #:prefab)

;; Named label (user-visible)
(struct Label:named (name) #:prefab)

;; Scoped local label
(struct Label:local (parent id) #:prefab)

(define (label? x)
  (or (Label:id? x) (Label:named? x) (Label:local? x)))

;; ============================================================================
;; Operands
;; ============================================================================

(define (operand? x)
  (or (any-reg? x) (Imm? x) (Imm:shifted? x) (mem-addr? x) (label? x)))

;; ============================================================================
;; Condition Codes
;; ============================================================================

(define (condition-code? x)
  (and (memq x '(eq ne cs cc mi pl vs vc hi ls ge lt gt le al nv
                 ;; Aliases
                 hs lo))
       #t))

;; ============================================================================
;; Instructions - Generic Form
;; ============================================================================

;; Generic instruction (op args...)
(struct AsmInsn (op args) #:prefab)

;; ============================================================================
;; Instructions - Specialized Forms
;; ============================================================================

;; Arithmetic: add, sub, mul, sdiv, udiv, etc.
(struct Insn:arith (op dst src1 src2) #:prefab)

;; Unary arithmetic: neg, abs, etc.
(struct Insn:arith2 (op dst src) #:prefab)

;; Load single register
(struct Insn:load (op dst addr) #:prefab)

;; Store single register
(struct Insn:store (op src addr) #:prefab)

;; Load pair
(struct Insn:ldp (op dst1 dst2 addr) #:prefab)

;; Store pair
(struct Insn:stp (op src1 src2 addr) #:prefab)

;; Unconditional branch
(struct Insn:branch (op target) #:prefab)

;; Conditional branch (b.cond)
(struct Insn:cond-branch (op cond target) #:prefab)

;; Compare and branch if zero/non-zero
(struct Insn:cbz (op reg target) #:prefab)

;; Compare
(struct Insn:cmp (op src1 src2) #:prefab)

;; Conditional select
(struct Insn:csel (op dst src1 src2 cond) #:prefab)

;; Move
(struct Insn:mov (op dst src) #:prefab)

;; Return
(struct Insn:ret () #:prefab)

;; ============================================================================
;; SVE Instructions
;; ============================================================================

;; SVE arithmetic with predication: fadd z0.s, p0/m, z1.s, z2.s
(struct Insn:sve (op pred dst srcs) #:prefab)

;; SVE contiguous load: ld1w z0.s, p0/z, [x0]
(struct Insn:sve-load (op pred dst addr) #:prefab)

;; SVE contiguous store: st1w z0.s, p0, [x0]
(struct Insn:sve-store (op pred src addr) #:prefab)

;; SVE reduction: faddv s0, p0, z0.s
(struct Insn:sve-reduce (op pred dst src) #:prefab)

;; SVE comparison: cmpeq p0.s, p1/z, z0.s, z1.s
(struct Insn:sve-cmp (op pd pg src1 src2) #:prefab)

;; SVE predicate operations: pnext, pfirst, etc.
(struct Insn:sve-pred-op (op pd pg pn) #:prefab)

;; SVE while loop: whilelt p0.s, x0, x1
(struct Insn:whilelt (pd rn rm) #:prefab)

;; ============================================================================
;; Instruction Predicate
;; ============================================================================

(define (insn? x)
  (or (AsmInsn? x)
      (Insn:arith? x) (Insn:arith2? x)
      (Insn:load? x) (Insn:store? x)
      (Insn:ldp? x) (Insn:stp? x)
      (Insn:branch? x) (Insn:cond-branch? x) (Insn:cbz? x)
      (Insn:cmp? x) (Insn:csel? x) (Insn:mov? x) (Insn:ret? x)
      (Insn:sve? x) (Insn:sve-load? x) (Insn:sve-store? x)
      (Insn:sve-reduce? x) (Insn:sve-cmp? x)
      (Insn:sve-pred-op? x) (Insn:whilelt? x)))

;; ============================================================================
;; Types for Type Annotations
;; ============================================================================

;; Scalar integer types
(struct Type:scalar (width signed?) #:prefab)

;; Floating point types
(struct Type:float (width) #:prefab)

;; NEON vector types (fixed lanes)
(struct Type:vec (elem lanes) #:prefab)

;; SVE scalable vector
(struct Type:sve (elem) #:prefab)

;; SVE2 scalable vector
(struct Type:sve2 (elem) #:prefab)

;; Predicate type
(struct Type:pred () #:prefab)

;; Pointer type
(struct Type:ptr (elem) #:prefab)

;; Void type (for functions returning nothing)
(struct Type:void () #:prefab)

(define (asm-type? x)
  (or (Type:scalar? x) (Type:float? x) (Type:vec? x)
      (Type:sve? x) (Type:sve2? x) (Type:pred? x)
      (Type:ptr? x) (Type:void? x)))

;; ============================================================================
;; Function Definition
;; ============================================================================

;; Function parameter
(struct AsmParam (reg type) #:prefab)

;; Complete function definition
(struct AsmFunction (name params ret-type body) #:prefab)
