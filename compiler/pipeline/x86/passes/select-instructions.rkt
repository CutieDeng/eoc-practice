#lang racket/base

;; ============================================================
;; Pass: Select Instructions
;; ============================================================
;;
;; Convert C-var to x86-var instructions.
;; Chooses appropriate x86 instructions for each operation.
;;
;; Input:  CProgram
;; Output: X86Program (with pseudo-registers)
;;
;; ============================================================

(require racket/match
         "../ir/cvar.rkt"
         "../ir/x86var.rkt")

(provide select-instructions)

;; ============================================================
;; Main Pass
;; ============================================================

(define (select-instructions prog)
  (match prog
    [(CProgram info blocks)
     (define x86-blocks
       (for/hash ([(label block) (in-hash blocks)])
         (values label (select-block block))))
     (X86Program info x86-blocks)]))

(define (select-block block)
  (match block
    [(CBlock info tail)
     (X86Block info (select-tail tail))]))

;; ============================================================
;; Select Tail
;; ============================================================

(define (select-tail tail)
  (match tail
    ;; Return
    [(CReturn exp)
     (append (select-exp (Reg 'rax) exp)
             (list (Jmp 'conclusion)))]

    ;; Sequence
    [(CSeq stmt tail)
     (append (select-stmt stmt)
             (select-tail tail))]

    ;; Goto
    [(CGoto label)
     (list (Jmp label))]

    ;; Conditional
    [(CIf cond then-label else-label)
     (select-pred cond then-label else-label)]

    [_ (error 'select-tail "unhandled tail: ~a" tail)]))

;; ============================================================
;; Select Statement
;; ============================================================

(define (select-stmt stmt)
  (match stmt
    [(CAssign (CVar name) exp)
     (select-exp (Var name) exp)]))

;; ============================================================
;; Select Expression (assignment target)
;; ============================================================

(define (select-exp dest exp)
  (match exp
    ;; Atomic expressions
    [(CInt n)
     (list (Instr 'movq (list (Imm n) dest)))]
    [(CBool #t)
     (list (Instr 'movq (list (Imm 1) dest)))]
    [(CBool #f)
     (list (Instr 'movq (list (Imm 0) dest)))]
    [(CVar name)
     (if (equal? dest (Var name))
         '()
         (list (Instr 'movq (list (Var name) dest))))]

    ;; Primitive operations
    [(CPrim 'read '())
     (list (Callq 'read_int 0)
           (Instr 'movq (list (Reg 'rax) dest)))]

    [(CPrim '- (list arg))
     (define src (select-atom arg))
     (list (Instr 'movq (list src dest))
           (Instr 'negq (list dest)))]

    [(CPrim '+ (list arg1 arg2))
     (define src1 (select-atom arg1))
     (define src2 (select-atom arg2))
     (list (Instr 'movq (list src1 dest))
           (Instr 'addq (list src2 dest)))]

    [(CPrim '- (list arg1 arg2))
     (define src1 (select-atom arg1))
     (define src2 (select-atom arg2))
     (list (Instr 'movq (list src1 dest))
           (Instr 'subq (list src2 dest)))]

    [(CPrim 'not (list arg))
     (define src (select-atom arg))
     (list (Instr 'movq (list src dest))
           (Instr 'xorq (list (Imm 1) dest)))]

    [(CPrim (and op (or 'eq? '< '<= '> '>=)) (list arg1 arg2))
     (define src1 (select-atom arg1))
     (define src2 (select-atom arg2))
     (define cc (prim->cc op))
     (list (Instr 'cmpq (list src2 src1))
           (Instr 'set (list cc (ByteReg 'al)))
           (Instr 'movzbq (list (ByteReg 'al) dest)))]

    [_ (error 'select-exp "unhandled expression: ~a" exp)]))

;; ============================================================
;; Select Predicate (for conditional jumps)
;; ============================================================

(define (select-pred cond then-label else-label)
  (match cond
    [(CPrim (and op (or 'eq? '< '<= '> '>=)) (list arg1 arg2))
     (define src1 (select-atom arg1))
     (define src2 (select-atom arg2))
     (define cc (prim->cc op))
     (list (Instr 'cmpq (list src2 src1))
           (JmpIf cc then-label)
           (Jmp else-label))]

    [(CPrim 'not (list arg))
     ;; Flip branches
     (select-pred arg else-label then-label)]

    [_
     ;; General case: evaluate to temp and compare
     (define temp (Var 'tmp.pred))
     (append (select-exp temp cond)
             (list (Instr 'cmpq (list (Imm 0) temp))
                   (JmpIf 'ne then-label)
                   (Jmp else-label)))]))

;; ============================================================
;; Helpers
;; ============================================================

(define (select-atom atom)
  (match atom
    [(CInt n) (Imm n)]
    [(CBool #t) (Imm 1)]
    [(CBool #f) (Imm 0)]
    [(CVar name) (Var name)]))

(define (prim->cc op)
  (case op
    [(eq?) 'e]
    [(<) 'l]
    [(<=) 'le]
    [(>) 'g]
    [(>=) 'ge]
    [else (error 'prim->cc "unknown comparison: ~a" op)]))
