#lang racket/base

(require racket/dict)
(require "core/integer-set.rkt")

(define x86_64-regs '(
  (rax . 0)
  (rbx . 1)
  (rcx . 2)
  (rdx . 3)
  (rsi . 4)
  (rdi . 5)
  (rbp . 6)
  (rsp . 7)
  (r8 . 8)
  (r9 . 9)
  (r10 . 10)
  (r11 . 11)
  (r12 . 12)
  (r13 . 13)
  (r14 . 14)
  (r15 . 15)
))
(provide x86_64-regs)

(define caller-save-regs '(rax rcx rdx rsi rdi r8 r9 r10 r11))
(define callee-save-regs '(rsp rbp rbx r12 r13 r14 r15))
(define pass-args-regs '(rdi rsi rdx rcx r8 r9))
(define caller-and-callee-regs (append caller-save-regs callee-save-regs))

(provide caller-save-regs callee-save-regs pass-args-regs caller-and-callee-regs)

(define pass-args-regs-bset
  (for/fold ([s 0]) ([p pass-args-regs]) (bset-union s (dict-ref x86_64-regs p)))
)
(provide pass-args-regs-bset)

(define callee-save-regs-bset
  (for/fold ([s 0]) ([p callee-save-regs]) (bset-union s (dict-ref x86_64-regs p)))
)
(provide callee-save-regs-bset)
