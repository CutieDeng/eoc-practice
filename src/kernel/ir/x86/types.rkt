#lang racket/base

;; ============================================================
;; X86 IR Type Definitions
;; ============================================================
;;
;; Based on Essentials of Compilation (EoC) book.
;; Defines the x86-64 instruction set representation.
;;
;; ============================================================

(provide
  ;; Arguments
  (struct-out Imm)
  (struct-out Reg)
  (struct-out Deref)
  (struct-out ByteReg)
  (struct-out Global)

  ;; Instructions
  (struct-out Instr)
  (struct-out Callq)
  (struct-out Retq)
  (struct-out Jmp)
  (struct-out JmpIf)
  (struct-out IndirectCallq)

  ;; Program structure
  (struct-out X86Block)
  (struct-out X86Program)

  ;; Registers
  register?
  caller-saved-registers
  callee-saved-registers
  argument-registers
  all-registers

  ;; Condition codes
  cc?
  negate-cc

  ;; Predicates
  arg?
  instr?)

;; ============================================================
;; Arguments (Operands)
;; ============================================================

;; Immediate (constant) value
(struct Imm (value) #:prefab)

;; Register
(struct Reg (name) #:prefab)

;; Memory reference: offset(reg)
(struct Deref (reg offset) #:prefab)

;; Byte register (for setcc instructions)
(struct ByteReg (name) #:prefab)

;; Global label reference
(struct Global (name) #:prefab)

;; ============================================================
;; Instructions
;; ============================================================

;; Generic instruction: op arg*
;; op is a symbol like 'movq, 'addq, 'subq, etc.
;; args is a list of arguments
(struct Instr (op args) #:prefab)

;; Call instruction
(struct Callq (label arity) #:prefab)

;; Return instruction
(struct Retq () #:prefab)

;; Unconditional jump
(struct Jmp (label) #:prefab)

;; Conditional jump
(struct JmpIf (cc label) #:prefab)

;; Indirect call (for closures)
(struct IndirectCallq (arg arity) #:prefab)

;; ============================================================
;; Program Structure
;; ============================================================

;; Basic block: label + list of instructions
(struct X86Block (info instrs) #:prefab)

;; X86 Program
;; info: metadata (locals, stack-size, etc.)
;; blocks: hash from label to X86Block
(struct X86Program (info blocks) #:prefab)

;; ============================================================
;; Registers
;; ============================================================

(define caller-saved-registers
  '(rax rcx rdx rsi rdi r8 r9 r10 r11))

(define callee-saved-registers
  '(rbx r12 r13 r14 r15))

(define argument-registers
  '(rdi rsi rdx rcx r8 r9))

(define all-registers
  (append caller-saved-registers callee-saved-registers '(rsp rbp)))

(define (register? x)
  (and (symbol? x)
       (memq x all-registers)))

;; ============================================================
;; Condition Codes
;; ============================================================

(define condition-codes
  '(e ne l le g ge))

(define (cc? x)
  (and (symbol? x)
       (memq x condition-codes)))

(define (negate-cc cc)
  (case cc
    [(e) 'ne]
    [(ne) 'e]
    [(l) 'ge]
    [(le) 'g]
    [(g) 'le]
    [(ge) 'l]
    [else (error 'negate-cc "unknown condition code: ~a" cc)]))

;; ============================================================
;; Predicates
;; ============================================================

(define (arg? x)
  (or (Imm? x)
      (Reg? x)
      (Deref? x)
      (ByteReg? x)
      (Global? x)))

(define (instr? x)
  (or (Instr? x)
      (Callq? x)
      (Retq? x)
      (Jmp? x)
      (JmpIf? x)
      (IndirectCallq? x)))
