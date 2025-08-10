#lang racket/base

(require racket/contract/base)
(require "p-types.rkt")

(require cutie-ftree)

(struct Assign (lhs rhs) #:transparent)
(provide (struct-out Assign))

(struct Return (arg) #:transparent)
(provide (struct-out Return))

(struct Goto (label) #:transparent)
(provide (struct-out Goto))

(struct Imm (value) #:transparent)
(provide (struct-out Imm))

(struct Reg (name) #:transparent)
(provide (struct-out Reg))

(struct Deref (reg offset) #:transparent)
(provide (struct-out Deref))

(struct Instr (name arg*) #:transparent)
(provide (struct-out Instr))

(struct Callq (target arity) #:transparent)
(provide (struct-out Callq))

(struct Retq () #:transparent)
(provide (struct-out Retq))

(struct IndirectCallq (target arity) #:transparent)
(provide (struct-out IndirectCallq))

(struct IndirectJmp (target) #:transparent)
(provide (struct-out IndirectJmp))

(struct Jmp (target) #:transparent)
(provide (struct-out Jmp))

(struct TailJmp (target arity) #:transparent)
(provide (struct-out TailJmp))

(struct Block (info instr*) #:transparent)
(provide (struct-out Block))

(struct ByteReg (name) #:transparent)
(provide (struct-out ByteReg))

(struct JmpIf (cnd target) #:transparent)
(provide (struct-out JmpIf))

; Value struct definitions
(struct Tagged (value tag) #:transparent)
(provide (struct-out Tagged))

(struct Function (params body env) #:transparent)
(provide (struct-out Function))

(define Type? (or/c ))

(struct IfStmt (cnd thn els) #:transparent)
(provide (contract-out (struct IfStmt ([cnd exp?] [thn (or/c ral? integer?)] [els (or/c ral? integer?)]))))

(struct CProgram (info blocks) #:transparent)

(provide 
  (struct-out CProgram)
)

(struct X86Program (info blocks) #:transparent)
(provide (struct-out X86Program))

(struct RvsdgGraph (info graph) #:transparent)
(provide (struct-out RvsdgGraph))

(provide (all-from-out "p-types.rkt"))
