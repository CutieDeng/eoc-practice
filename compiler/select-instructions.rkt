#lang racket

(require "../utilities.rkt")
(require cutie-ftree)

(define pass-select-instructions
  (class object%
    (super-new)
    (define/public pass (match-lambda [(CProgram info blocks)
      (define blocks^
        (for/fold ([a (ordl-make-empty symbol-compare)]) ([(block-tag block) (in-dict blocks)])
          (ordl-insert a block-tag (Block (ordl-make-empty symbol-compare) (pass-instr* block)) #f)
        )
      )
      (X86Program info blocks^)
    ]))
    (define pass-instr* (match-lambda
      [(Seq a res) (pass-instr a (pass-instr* res))]      
      [(Return arg) (pass-instr (Assign (Reg 'rax) arg) (ral-empty))]
      [(Goto label) (vector->ral (vector (Jmp label)))]
      [(IfStmt (Prim 'eq? (list (and lhs (or (Int _) (Bool _) (Void ))) (and rhs (or (Int _) (Bool _) (Void ))))) thn els)
        (pass-instr* (if (equal? lhs rhs) thn els))
      ]
      [(IfStmt (Prim 'eq? (list (and lhs (or (Var _))) (and rhs (or (Var _))))) thn els)
        (vector->ral (vector (Instr 'cmpq (list lhs rhs)) (JmpIf 'e (Goto-label thn)) (Jmp (Goto-label els))))
      ]
      [(IfStmt (Prim 'eq? (list (and lhs (or (Var _))) (Bool rhs))) thn els)
        (vector->ral (vector
          (Instr 'movq (list (Imm (if rhs 1 0)) (Reg 'rax)))
          (Instr 'cmpq (list (Reg 'rax) lhs))
          (JmpIf 'e (Goto-label thn))
          (Jmp (Goto-label els))
        ))
      ]
      [(IfStmt (Prim 'eq? (list (and lhs (or (Var _))) (Int rhs))) thn els)
        (vector->ral (vector
          (Instr 'movq (list (Imm rhs) (Reg 'rax)))
          (Instr 'cmpq (list (Reg 'rax) lhs))
          (JmpIf 'e (Goto-label thn))
          (Jmp (Goto-label els))
        ))
      ]
      [(IfStmt (Prim 'eq? (list (and lhs (or (Int _) (Bool _))) rhs)) thn els)
        (pass-instr* (IfStmt (Prim 'eq? (list rhs lhs)) thn els))
      ]
      [(IfStmt (Prim '< (list (and lhs (or (Int _) (Bool _) (Void ))) (and rhs (or (Int _) (Bool _) (Void ))))) thn els)
        (pass-instr* (if (< lhs rhs) thn els))
      ]
      [(IfStmt (Prim '< (list (and lhs (Var _)) (and rhs (Var _)))) thn els)
        (vector->ral (vector
          (Instr 'cmpq (list rhs lhs))
          (JmpIf 'l (Goto-label thn))
          (Jmp (Goto-label els))
        ))
      ]
      [(IfStmt (Prim '< (list (and lhs (Var _)) (Int rhs))) thn els)
        (vector->ral (vector
          (Instr 'movq (list (Imm rhs) (Reg 'rax)))
          (Instr 'cmpq (list (Reg 'rax) lhs))
          (JmpIf 'l (Goto-label thn))
          (Jmp (Goto-label els))
        ))
      ]
      [(IfStmt (Prim '< (list (Int lhs) (and rhs (Var _)))) thn els)
        (vector->ral (vector
          (Instr 'movq (list (Imm lhs) (Reg 'rax)))
          (Instr 'cmpq (list rhs (Reg 'rax)))
          (JmpIf 'l (Goto-label thn))
          (Jmp (Goto-label els))
        ))
      ]
    ))
    (define (pass-instr instr cont) (match instr
      [(Assign lhs (Int imm))
        (ral-consl cont (Instr 'movq (list (Imm imm) lhs)))]
      [(Assign lhs (and v (Var _)))
        (ral-consl cont (Instr 'movq (list v lhs)))]
      [(Assign lhs (Prim '+ (list lhs (Int r))))
        (ral-consl cont (Instr 'addq (list (Imm r) lhs)))]
      [(Assign lhs (Prim '+ (list lhs (and r (Var _)))))
        (ral-consl cont (Instr 'addq (list r lhs)))]
      [(Assign lhs (Prim '+ (list other lhs)))
        (pass-instr (Assign lhs (Prim '+ (list lhs other)) cont))]
      [(Assign lhs (Prim '+ (list (Int l) (Int r))))
        (ral-consl cont (Instr 'movq (list (Imm (+ l r)) lhs)))]
      [(Assign lhs (Prim '+ (list (Int l) (and r (Var _)))))
        (ral-consl (ral-consl cont (Instr 'addq (list (Imm l) lhs))) (Instr 'movq (list r lhs)))]
      [(Assign lhs (Prim '+ (list (and l (Var _)) (and r (Var _)))))
        (ral-consl (ral-consl cont (Instr 'addq (list r lhs))) (Instr 'movq (list l lhs)))]
      [(Assign lhs (Prim '+ (list (and l (Var _)) (and r (Int _)))))
        (pass-instr (Assign lhs (Prim '+ (list r l))) cont)]
      [(Assign lhs (Prim '- (list (Int a))))
        (ral-consl cont (Instr 'movq (list (Imm (- a)) lhs)))]
      [(Assign lhs (Prim '- (list (and a (Var _)))))
        (ral-consl (ral-consl cont (Instr 'negq (list lhs))) (Instr 'movq (list a lhs)))]
      [(Assign lhs (Prim 'read '()))
        (ral-consl (ral-consl cont (Instr 'movq (list (Reg 'rax) lhs))) (Callq 'read_int 0))]
      [(Assign lhs (Bool rhs))
        (ral-consl cont (Instr 'movq (list (if rhs (Imm 1) (Imm 0)) lhs)))]
      [(Assign lhs (Prim 'not (list (and operand (Var _)))))
        (ral-consl (ral-consl cont (Instr 'xorq (list (Imm 1) lhs))) (Instr 'movq (list operand lhs)))]
      [(Assign lhs (Prim 'not (list (Bool operand))))
        (ral-consl cont (Instr 'movq (list (if operand (Imm 0) (Imm 1)) lhs)))]
      [(Assign lhs (Prim '- (list lhs lhs)))
        (ral-consl cont (Instr 'movq (list (Imm 0) lhs)))]
      [(Assign lhs (Prim '- (list lhs (and op1 (Var _)))))
        (ral-consl cont (Instr 'subq (list op1 lhs)))]
      [(Assign lhs (Prim '- (list lhs (Int op1))))
        (ral-consl cont (Instr 'subq (list (Imm op1) lhs)))]
      [(Assign lhs (Prim '- (list (and op0 (Var _)) (and op1 (Var _)))))
        (ral-consl (ral-consl cont (Instr 'subq (list op1 lhs))) (Instr 'movq (list op0 lhs)))]
      [(Assign lhs (Prim '- (list (and op0 (Var _)) (Int op1))))
        (ral-consl (ral-consl cont (Instr 'subq (list (Imm op1) lhs))) (Instr 'movq (list op0 lhs)))]
      [(Assign lhs (Prim '- (list (Int op0) (and op1 (Var _)))))
        (ral-consl (ral-consl cont (Instr 'subq (list op1 lhs))) (Instr 'subq (list op1 lhs)))]
      [(Assign lhs (Prim '- (list (Int op0) (Int op1))))
        (ral-consl cont (Instr 'movq (list (Imm (- op0 op1)) lhs)))]
      [(Assign _ (Void)) cont]
      [(Assign lhs (Prim 'eq? (list (and rhs0 (Var _)) (and rhs1 (Var _)))))
        (ral-consl (ral-consl (ral-consl cont (Instr 'movzbq (list (ByteReg 'al) lhs))) (Instr 'sete (list (ByteReg 'al)))) (Instr 'cmpq (list rhs0 rhs1)))]
      [(Assign lhs (Prim 'eq? (list (and rhs0 (Var _)) (Int rhs1))))
        (ral-consl (ral-consl (ral-consl cont (Instr 'movzbq (list (ByteReg 'al) lhs))) (Instr 'sete (list (ByteReg 'al)))) (Instr 'cmpq (list rhs0 (Imm rhs1))))]
      [(Assign lhs (Prim 'eq? (list (Int rhs0) (and rhs1 (Var _)))))
        (ral-consl (ral-consl (ral-consl cont (Instr 'movzbq (list (ByteReg 'al) lhs)))
          (Instr 'sete (list (ByteReg 'al))))
            (Instr 'cmpq (list (Imm rhs0) rhs1)))]
      [(Prim 'vector-set! (list (and v (Var _)) (Int idx) (and val (Var _))))
        (ral-consl (ral-consl (ral-consl cont (Instr 'movq (list (Reg 'rax) (Deref 'r11 (* 8 (+ idx 1))))))
          (Instr 'movq (list v (Reg 'r11))))
            (Instr 'movq (list val (Reg 'rax))))]
      [(Prim 'vector-set! (list (and v (Var _)) (Int idx) (Int val)))
        (ral-consl (ral-consl (ral-consl cont (Instr 'movq (list (Reg 'rax) (Deref 'r11 (* 8 (+ idx 1)))))) 
          (Instr 'movq (list v (Reg 'r11)))) (Instr 'movq (list (Imm val) (Reg 'rax))))]
      [(Assign _ (and rhs (Prim 'vector-set! _)))
        (pass-instr rhs cont)]
      [(Assign lhs (Allocate len types))
        (ral-consl (ral-consl (ral-consl (ral-consl cont (Instr 'movq (list (Reg 'r11) lhs)))
          (Instr 'movq (list (Imm (cast-types-to-tag types)) (Deref 'r11 0))))
            (Instr 'addq (list (Imm (* 8 (+ 1 len))) (Global 'free_ptr))))
              (Instr 'movq (list (Global 'free_ptr) (Reg 'r11))))]
      [(Collect bytes)
        (ral-consl (ral-consl (ral-consl cont (Callq 'collect 2)) (Instr 'movq (list (Imm bytes) (Reg 'rsi)))) (Instr 'movq (list (Reg 'r15) (Reg 'rdi))))]
      [(Assign lhs (GlobalValue v)) 
        (ral-consl cont (Instr 'movq (list (Global v) lhs)))]
      [(Assign lhs (Prim 'vector-ref (list (and rhs0 (Var _)) (Int rhs1))))
        (ral-consl (ral-consl cont (Instr 'movq (list (Deref 'r11 (* 8 (+ rhs1 1))) lhs))) (Instr 'movq (list rhs0 (Reg 'r11))))]
    ))
    (define cast-types-to-tag (match-lambda
      [`(Vector ,types ...)
        (define types-mask 
          (let get-bit-fields ([idx 0] [rest-types types] [mask 1] [rst 0])
            (match rest-types
              [`(Integer ,rest ...) 
                (get-bit-fields (+ idx 1) rest (* mask 2) rst)]
              [`((Vector ,_ ...) ,rest ...) 
                (get-bit-fields (+ idx 1) rest (arithmetic-shift mask 1) (bitwise-ior rst mask))]
              ['() rst]
          ))
        )
        (bitwise-ior 
          (arithmetic-shift types-mask 7)
          (arithmetic-shift (length types) 1)
          1
        )
      ]
    ))
  ))

(provide pass-select-instructions)
