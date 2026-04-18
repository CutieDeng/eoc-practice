#lang racket/base

;; ============================================================
;; Component: X86-var Prelude and Conclusion
;; ============================================================
;;
;; Add function prologue and epilogue.
;; Creates main entry point and conclusion block.
;;
;; Input:  X86Program with patched instructions
;; Output: X86Program with main and conclusion blocks
;;
;; ============================================================

(require racket/match
         "../../../kernel/ir/x86/types.rkt")

(provide prelude-and-conclusion)

;; ============================================================
;; Main Pass
;; ============================================================

(define (prelude-and-conclusion prog)
  (match prog
    [(X86Program info blocks)
     (define stack-size (cdr (assq 'stack-size info)))
     (define new-blocks
       (hash-set*
        blocks
        'main (create-main-block stack-size)
        'conclusion (create-conclusion-block stack-size)))
     (X86Program info new-blocks)]))

;; ============================================================
;; Create Main Block (Prologue)
;; ============================================================

(define (create-main-block stack-size)
  (X86Block
   '()
   (list
    ;; Save base pointer
    (Instr 'pushq (list (Reg 'rbp)))
    ;; Set up new base pointer
    (Instr 'movq (list (Reg 'rsp) (Reg 'rbp)))
    ;; Allocate stack space
    (Instr 'subq (list (Imm stack-size) (Reg 'rsp)))
    ;; Jump to program start
    (Jmp 'start))))

;; ============================================================
;; Create Conclusion Block (Epilogue)
;; ============================================================

(define (create-conclusion-block stack-size)
  (X86Block
   '()
   (list
    ;; Deallocate stack space
    (Instr 'addq (list (Imm stack-size) (Reg 'rsp)))
    ;; Restore base pointer
    (Instr 'popq (list (Reg 'rbp)))
    ;; Return
    (Retq))))
