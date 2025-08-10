#lang racket

(require "core/core-types.rkt")
(require "x86abi.rkt")
(require "core/integer-set.rkt")

(define deref-reg-and-var (match-lambda
  [(or (Reg x) (Var x)) x] [_ -1]
))

(define instr-analysis
  (class object%
    (super-new)
    (define/public (write-from-instr instr)
      (define w (write-from-instr-raw instr))
      w
      ; (list->set (filter (λ (i) (not ((or/c Imm? Bool? Global? Deref?) i))) (set->list w)))
    )
    (define/public (read-from-instr instr)
      (define r (read-from-instr-raw instr))
      r
      ; (list->set (filter (λ (i) (not ((or/c Imm? Bool? Global? Deref?) i))) (set->list r)))
    )
    (define (write-from-instr-raw instr) (match instr
      [(Instr 'movq (list _ dst)) (bset (deref-reg-and-var dst))]
      [(or (Instr 'addq (list _ dst)) (Instr 'subq (list _ dst))) (bset (deref-reg-and-var dst))]
      [(Callq _ _) callee-save-regs-bset]
      [(Instr 'negq (list dst)) (bset (deref-reg-and-var dst))]
      [(Instr 'cmpq (list _ _)) (bset)]
      [(Jmp _) (bset)]
      [(JmpIf _ _) (bset)]
    ))
    (define (read-from-instr-raw instr) (match instr
      [(Instr (or 'addq 'subq) (list src dst)) (bset (deref-reg-and-var src) (deref-reg-and-var dst))]
      [(Instr 'movq (list src _)) (bset (deref-reg-and-var src))]
      [(Instr 'negq (list src)) (bset (deref-reg-and-var src))]
      [(Callq _ count) 
        pass-args-regs-bset]
      [(Jmp _) (bset)]
      [(JmpIf _ _) (bset)]
      [(Instr 'cmpq (list a b)) (bset (deref-reg-and-var a) (deref-reg-and-var b))]
    ))
  ))

(provide instr-analysis)
