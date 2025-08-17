#lang racket

(require "core/core-types.rkt" "core/utilities.rkt")
(require cutie-ftree)
(require "x86abi.rkt")

(define pass-allocate-registers
  (class object%
    (super-new)
    (field [color-graph #f])
    (define/public (pass p) (match p
      [(X86Program info blocks)
        (set-field! color-graph this (dict-ref info 'color-graph))
        (printf "color-traph: ~a~n" color-graph)
        (define slot-num (+ 1 (sequence-fold max -1 (in-dict-values color-graph))))
        (define blocks^ 
          (for/fold ([bbs (ordl-make-empty integer-compare)]) ([(bb-id bb) (in-dict blocks)]) 
            (dict-set bbs bb-id (pass-block bb)))
        )
        (define stack-size (* (max 0 (- slot-num 16)) 8))
        (X86Program (dict-set (dict-set info 'stack-size stack-size) 'num-root-spills 0) blocks^)
      ]
    ))
    (define pass-insn (match-lambda
      [(Instr i vs) (Instr i (map pass-reg vs))]
      [(and insn (or (Jmp _) (JmpIf _ _))) insn]
      [(and insn (Callq _ _)) insn]
    ))
    (define (pass-reg value) (match value
      [(or (Imm _) (Bool _)) value]
      [(or (Global _) (Deref _ _)) value]
      [(Reg _) value]
      [(Var id)
        (define order (dict-ref color-graph id 0)) ; maybe not in color-graph
        (match order
          [_ #:when (< order 16) 
            (Reg order)] 
          [_ (Deref 'rbp (- (* 8 (- order 16)) 8))]
        )
      ]
    ))
    (define (pass-block block) (match block
      [(Block info instr*)
        (define instr*^
          (for/fold ([instr*^ (ral-empty)]) ([insn (in-ral0 instr*)])
            (ral-consr instr*^ (pass-insn insn))
          ))
        (Block info instr*^)
      ]
    ))
  )
)

(provide pass-allocate-registers)
