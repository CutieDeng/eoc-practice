#lang racket/base

(define caller-save-regs '(rax rcx rdx rsi rdi r8 r9 r10 r11))
(define callee-save-regs '(rsp rbp rbx r12 r13 r14 r15))
(define pass-args-regs '(rdi rsi rdx rcx r8 r9))
(define caller-and-callee-regs (append caller-save-regs callee-save-regs))

(provide caller-save-regs callee-save-regs pass-args-regs caller-and-callee-regs)
