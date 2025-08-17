#lang racket

(require "core/core-types.rkt" "core/utilities.rkt")
(require cutie-ftree)

(define pass-patch-instructions
  (class object%
    (super-new)
    (define/public (pass p) (match p
      [(X86Program info blocks)
        (define blocks^ (for/fold ([a (ordl-make-empty integer-compare)]) ([(bb-id bb) (in-dict blocks)]) 
          (dict-set a bb-id (patch-instr-block bb))))
        (X86Program info blocks^)
      ]
    ))
    (define (patch-instr-block block) (match block
      [(Block info instr*)
        (define instr*^ (patch-instr* instr* (ral-empty)))
        (Block info instr*^)
      ]
    ))
    (define (patch-instr* instr* cont) (match instr*
      [(? ral-empty?) cont]
      [_ (define-values (instr rest) (ral-dropl instr*))
        (match instr
          [(Instr 'movq (list (Deref r0 o0) (Deref r1 o1)))
            (patch-instr*
              rest
              (ral-consr (ral-consr cont (Instr 'movq (list (Deref r0 o0) (Reg 0)))) (Instr 'movq (list (Reg 0) (Deref r1 o1))))
            )
          ]
          [(Instr i (list (Deref r0 o0) (Deref r1 o1)))
            (patch-instr*
              rest
              (ral-consr (ral-consr (ral-consr cont (Instr 'movq (list (Deref r1 o1) (Reg 0)))) (Instr i (list (Deref r0 o0) (Reg 0)))) (Instr 'movq (list (Reg 0) (Deref r1 o1))))
            ) 
          ]
          [(Instr 'movq (list (Reg a) (Reg a))) 
            (patch-instr* rest cont)
          ]
          [(or (Instr 'addq (list (Imm 0) _)) (Instr 'subq (list (Imm 0) _))) 
            (patch-instr* rest cont)
          ]
          [_ (patch-instr* rest (ral-consr cont instr))]
        )
      ]
    ))
  ))

(provide pass-patch-instructions)
