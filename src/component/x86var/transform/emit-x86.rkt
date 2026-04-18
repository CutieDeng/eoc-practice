#lang racket/base

;; ============================================================
;; Component: X86-var Emit X86 Assembly
;; ============================================================
;;
;; Generate x86-64 assembly text from X86Program.
;;
;; Input:  X86Program (complete)
;; Output: String containing x86 assembly
;;
;; ============================================================

(require racket/match
         racket/string
         racket/format
         "../../../kernel/ir/x86/types.rkt")

(provide emit-x86
         print-x86)

;; ============================================================
;; Main Pass
;; ============================================================

(define (emit-x86 prog)
  (match prog
    [(X86Program info blocks)
     (string-append
      (emit-header)
      (emit-blocks blocks))]))

(define (print-x86 prog)
  (displayln (emit-x86 prog)))

;; ============================================================
;; Emit Header
;; ============================================================

(define (emit-header)
  (string-append
   "    .globl main\n"
   "    .text\n"))

;; ============================================================
;; Emit Blocks
;; ============================================================

(define (emit-blocks blocks)
  (string-join
   (for/list ([(label block) (in-hash blocks)])
     (emit-block label block))
   "\n"))

(define (emit-block label block)
  (match block
    [(X86Block info instrs)
     (string-append
      (emit-label label)
      (string-join (map emit-instr instrs) "\n")
      "\n")]))

(define (emit-label label)
  (~a label ":\n"))

;; ============================================================
;; Emit Instructions
;; ============================================================

(define (emit-instr instr)
  (match instr
    [(Instr op args)
     (emit-regular-instr op args)]
    [(Callq label arity)
     (~a "    callq " label)]
    [(Retq)
     "    retq"]
    [(Jmp label)
     (~a "    jmp " label)]
    [(JmpIf cc label)
     (~a "    j" cc " " label)]
    [(IndirectCallq arg arity)
     (~a "    callq *" (emit-arg arg))]))

(define (emit-regular-instr op args)
  (case op
    [(movq addq subq xorq cmpq negq pushq popq)
     (~a "    " op " " (string-join (map emit-arg args) ", "))]
    [(movzbq)
     (~a "    movzbq " (string-join (map emit-arg args) ", "))]
    [(set)
     (match args
       [(list cc reg)
        (~a "    set" cc " " (emit-arg reg))])]
    [else
     (~a "    " op " " (string-join (map emit-arg args) ", "))]))

;; ============================================================
;; Emit Arguments
;; ============================================================

(define (emit-arg arg)
  (match arg
    [(Imm n)
     (~a "$" n)]
    [(Reg name)
     (~a "%" name)]
    [(Deref reg offset)
     (if (= offset 0)
         (~a "(%" reg ")")
         (~a offset "(%" reg ")"))]
    [(ByteReg name)
     (~a "%" name)]
    [(Global name)
     (~a name "(%rip)")]
    [_ (error 'emit-arg "unknown arg: ~a" arg)]))
