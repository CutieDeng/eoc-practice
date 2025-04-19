#lang racket

(require "../utilities.rkt")
(require "x86abi.rkt")

(define instr-analysis
  (class object%
    (super-new)
    (define/public (write-from-instr instr)
      (define w (write-from-instr-raw instr))
      (list->set (filter (λ (i) (not ((or/c Imm? Bool? Global? Deref?) i))) (set->list w)))
    )
    (define/public (read-from-instr instr)
      (define r (read-from-instr-raw instr))
      (list->set (filter (λ (i) (not ((or/c Imm? Bool? Global? Deref?) i))) (set->list r)))
    )
    (define (write-from-instr-raw instr) (match instr
      [(Instr 'movq (list _ dst)) (set dst)]
      [(or (Instr 'addq (list _ dst)) (Instr 'subq (list _ dst))) (set dst)]
      [(Callq _ _) (list->set (map Reg caller-save-regs))]
      [(Instr 'negq (list dst)) (set dst)]
      [(Instr 'cmpq (list _ _)) (set )]
      [(Jmp _) (set )]
      [(JmpIf _ _) (set )]
    ))
    (define (read-from-instr-raw instr) (match instr
      [(Instr (or 'addq 'subq) (list src dst)) (set src dst)]
      [(Instr 'movq (list src _)) (set src)]
      [(Instr 'negq (list src)) (set src)]
      [(Callq _ count) 
        (list->set (map Reg (take pass-args-regs count)))]
      [(Jmp _) (set )]
      [(JmpIf _ _) (set )]
      [(Instr 'cmpq (list a b)) (set a b)]
    ))
  ))

(provide instr-analysis)
