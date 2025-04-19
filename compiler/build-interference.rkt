#lang racket

(require "../utilities.rkt")
(require "graph-core.rkt")
(require cutie-ftree)

(require "x86abi.rkt")
(require "interference.rkt")

(define pass-build-interference
  (class object%
    (super-new) 
    (define (write-from-instr instr)
      (define w (write-from-instr-raw instr))
      (list->set (filter (λ (i) (not ((or/c Imm? Bool? Global? Deref?) i))) (set->list w)))
    )
    (define (read-from-instr instr)
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
    (define (writes-from-block block) (match block
      [(Block _ instr*)
        (for/fold ([ws (ral-empty)]) ([i (in-ral0 instr*)])
          (ral-consr ws (write-from-instr i))
        )
      ]
    ))
    (field [interference-obj (new interference)])
    (define (build-block block) (match block [(Block info instr*)
      (define live (dict-ref info 'live))
      (define ws (writes-from-block block))
      (send interference-obj solve-weak ws live)
    ]))
    (define/public (pass p) (match p [(X86Program info blocks)
      (for ([(_name block) (in-dict blocks)])
        (build-block block)
      )
      (define i (get-field i-graph interference-obj))
      (define info^ (dict-set info 'interference i))
      (X86Program info^ blocks)
    ]))
  ))

(provide pass-build-interference)
