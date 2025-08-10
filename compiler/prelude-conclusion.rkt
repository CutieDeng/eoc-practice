#lang racket

(require "core/core-types.rkt" "core/utilities.rkt")
(require cutie-ftree)

(define pass-prelude-and-conclusion
  (class object%
    (super-new)
    (define (get-prelude p) (match p [(X86Program info _)
      (define stack-size (align (dict-ref info 'stack-size) 16))
      (define insts (vector
        (Instr 'pushq (list (Reg 'rbp)))
        (Instr 'movq (list (Reg 'rsp) (Reg 'rbp))) 
        (Instr 'subq (list (Imm stack-size) (Reg 'rsp)))
        (Instr 'movq (list (Imm 65536) (Reg 'rdi)))
        (Instr 'movq (list (Imm 65536) (Reg 'rsi)))
        (Callq 'initialize 2)
        (Instr 'movq (list (Global 'rootstack_begin) (Reg 'r15)))
        (Instr 'movq (list (Imm 0) (Deref 'r15 0)))
        (Instr 'addq (list (Imm 8) (Deref 'r15 0)))
        (Jmp 2)
      ))
      (Block (ordl-make-empty symbol-compare) (vector->ral insts))
    ]))
    (define (get-conclusion p) (match p [(X86Program info _)
      (define stack-size (dict-ref info 'stack-size))
      (define insts (vector
        (Instr 'subq (list (Imm 8) (Reg 'r15)))
        (Instr 'addq (list (Imm stack-size) (Reg 'rsp)))
        (Instr 'popq (list (Reg 'rbp)))
        (Retq )
      ))
      (Block (ordl-make-empty symbol-compare) (vector->ral insts))
    ]))
    (define/public (pass p) (match p [(X86Program info blocks)
      (define prelude (get-prelude p))
      (define conclusion (get-conclusion p))
      (define blocks^
        (for/fold ([a (ordl-make-empty integer-compare)]) ([(tag block) (in-dict blocks)]) 
          (match-define (Block info instr*) block)
          (define instr*^
            (cond
              [(ral-empty? instr*) (ral-consr instr* (Jmp 1))]
              [else (define last-instr (ral-viewr instr*))
                (if (Jmp? last-instr) instr* (ral-consr instr* (Jmp 1)))
              ]
            ))
          (ordl-insert a tag (Block info instr*^) #f)
        )
      )
      (X86Program info
        (dict-set 
          (dict-set blocks^ 0 prelude)
          1 conclusion))
    ]))
  ))

(provide pass-prelude-and-conclusion)
