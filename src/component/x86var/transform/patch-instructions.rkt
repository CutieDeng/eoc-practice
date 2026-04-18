#lang racket/base

;; ============================================================
;; Component: X86-var Patch Instructions
;; ============================================================
;;
;; Fix x86 instruction constraints:
;; - Two memory operands not allowed
;; - Some instructions have operand size restrictions
;;
;; Input:  X86Program with stack locations
;; Output: X86Program with valid x86 instructions
;;
;; ============================================================

(require racket/match
         "../../../kernel/ir/x86/types.rkt")

(provide patch-instructions)

;; ============================================================
;; Main Pass
;; ============================================================

(define (patch-instructions prog)
  (match prog
    [(X86Program info blocks)
     (define new-blocks
       (for/hash ([(label block) (in-hash blocks)])
         (values label (patch-block block))))
     (X86Program info new-blocks)]))

(define (patch-block block)
  (match block
    [(X86Block info instrs)
     (X86Block info (append-map patch-instr instrs))]))

;; ============================================================
;; Patch Individual Instructions
;; ============================================================

(define (patch-instr instr)
  (match instr
    ;; movq with two memory operands
    [(Instr 'movq (list (and src (Deref _ _)) (and dst (Deref _ _))))
     (list (Instr 'movq (list src (Reg 'rax)))
           (Instr 'movq (list (Reg 'rax) dst)))]

    ;; movq with equal source and destination
    [(Instr 'movq (list src dst))
     #:when (equal? src dst)
     '()]

    ;; addq/subq with two memory operands
    [(Instr (and op (or 'addq 'subq)) (list (and src (Deref _ _)) (and dst (Deref _ _))))
     (list (Instr 'movq (list src (Reg 'rax)))
           (Instr op (list (Reg 'rax) dst)))]

    ;; cmpq with two memory operands
    [(Instr 'cmpq (list (and src1 (Deref _ _)) (and src2 (Deref _ _))))
     (list (Instr 'movq (list src1 (Reg 'rax)))
           (Instr 'cmpq (list (Reg 'rax) src2)))]

    ;; cmpq with immediate as second operand (not allowed)
    [(Instr 'cmpq (list src (Imm n)))
     (list (Instr 'movq (list (Imm n) (Reg 'rax)))
           (Instr 'cmpq (list src (Reg 'rax))))]

    ;; movzbq constraints
    [(Instr 'movzbq (list src (and dst (Deref _ _))))
     (list (Instr 'movzbq (list src (Reg 'rax)))
           (Instr 'movq (list (Reg 'rax) dst)))]

    ;; xorq with two memory operands
    [(Instr 'xorq (list (and src (Deref _ _)) (and dst (Deref _ _))))
     (list (Instr 'movq (list src (Reg 'rax)))
           (Instr 'xorq (list (Reg 'rax) dst)))]

    ;; Default: instruction is valid
    [_ (list instr)]))

;; Helper: flatten nested lists
(define (append-map f lst)
  (apply append (map f lst)))
