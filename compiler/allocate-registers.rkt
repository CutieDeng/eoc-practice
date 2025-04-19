#lang racket

(require "../utilities.rkt")
(require cutie-ftree)
(require "x86abi.rkt")

(define pass-allocate-registers
  (class object%
    (super-new)
    (define/public (pass p) (match p
      [(X86Program info blocks)
        (define table (dict-ref info 'color-graph))
        (define allo (allocate-registers-block table))
        (define slot-num (+ 1 (foldl max -1 (sequence->list (in-dict-values table)))))
        (define blocks^ 
          (for/fold ([a (ordl-make-empty symbol-compare)]) ([(tag block-inner) (in-dict blocks)]) 
            (ordl-insert a tag (allo block-inner) #f))
        )
        (define stack-size (max 0 (* (- slot-num (length caller-and-callee-regs)) 8)))
        (X86Program (dict-set (dict-set info 'stack-size stack-size) 'num-root-spills 0) blocks^)
      ]
    ))
    (define ((allocate-instr table) instr) (match instr
      [(Instr i list-value) (Instr i (map (allocate-register table) list-value))]
      [(or (Jmp _) (JmpIf _ _)) instr]
      [(Callq _ _) instr]
    ))
    (define ((allocate-register table) value) (match value
      [(or (Imm _) (Bool _)) value]
      [(or (Global _) (Deref _ _)) value]
      [(Reg _) value]
      [_
        (define order (dict-ref table value 0))
        (match order
          [_ #:when (< order (length caller-and-callee-regs)) 
            (Reg (list-ref caller-and-callee-regs order))] 
          [_ (Deref 'rbp (- (* 8 (- order (length caller-and-callee-regs))) 8))]
        )
      ]
    ))
    (define ((allocate-registers-block table) block) (match block
      [(Block info instr*)
        (define m (allocate-instr table))
        (define instr*^ (vector->ral (for/vector #:length (ral-length instr*)
          ([i (in-ral0 instr*)]) (m i))))
        (Block info instr*^)
      ]
    ))
  )
)

(provide pass-allocate-registers)
