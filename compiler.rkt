#lang racket

(require racket/set)
(require racket/fixnum)
(require "interp.rkt")
(require "utilities.rkt")
; (provide (all-defined-out))

(require "interp-Lvec-prime.rkt")
(require "type-check-Lvec.rkt")
(require "interp-Cvec.rkt")
(require "type-check-Cvec.rkt")

(require graph)
(require "graph-printing.rkt")
(require "priority_queue.rkt")

(require data/queue)

(require racket/pretty)

(define (aligned x a) (match (modulo x a)
  [0 x]
  [y (+ x a (- y))]
))

(define caller-save-regs '(rax rcx rdx rsi rdi r8 r9 r10 r11))

(define callee-save-regs '(rsp rbp rbx r12 r13 r14 r15))

(define pass-args-regs '(rdi rsi rdx rcx r8 r9))

(define caller-and-callee-regs (append caller-save-regs callee-save-regs))

(define pass-abstract
  (class object%
    (super-new)
    (abstract pass)
  ))

(define (pass-program-mixin clazz)
  (class clazz
    (super-new)
    (inherit pass-exp)
    (define/public pass (match-lambda 
      [(Program info body) (Program info (pass-exp body))]))
  )
)

(define pass-program-abstract
  (class pass-abstract
    (super-new)
    (abstract pass-exp)
    (define/override pass (match-lambda 
      [(Program info exp) (Program info (pass-exp exp))]
    ))
  ))

(define pass-shrink
  (class object%
    (super-new)
    (define/public pass-exp (match-lambda
      [(Prim 'and (list a b)) (If (pass-exp a) (pass-exp b) (Bool #f))]
      [(Prim 'or (list a b)) (If (pass-exp a) (Bool #t) (pass-exp b))]
      [(Let x e body) (Let x (pass-exp e) (pass-exp body))]
      [(If cnd thn els) (If (pass-exp cnd) (pass-exp thn) (pass-exp els))]
      [(Prim op es) (Prim op (map (λ (v) (pass-exp v)) es))]
      [(Begin es e) (Begin (map (λ (v) (pass-exp v)) es) (pass-exp e))]
      [(WhileLoop cnd body) (WhileLoop (pass-exp cnd) (pass-exp body))]
      [(and value (or (Int _) (Bool _) (Var _) (SetBang _ _) (GetBang _) (Void ))) value]
    ))
  ))

(define shrink (λ (p) (send (new (pass-program-mixin pass-shrink)) pass p)))

(define uniquify-env (make-parameter '()))

(define pass-program-ext-abstract
  (class pass-abstract
    (super-new)
    (abstract pass-exp)
    (define/public (init-env) '())
    (define/override pass (match-lambda
      [(Program info exp) (Program info ((pass-exp (init-env)) exp))]
    ))
  ))

(define pass-Lwhile-uniquify
  (class object%
    (super-new)
    (define/public (pass-exp exp)
      (define e (uniquify-env))
      (match exp
        [(Var x) (Var (dict-ref e x))]
        [(or (Int _) (Bool _) (Void )) exp]
        [(Let x exp body)
          (define exp^ (pass-exp exp))
          (define x^ (gensym x))
          (parameterize ([uniquify-env (dict-set e x x^)])
            (define body^ (pass-exp body))
            (Let x^ exp^ body^)
          )
        ]
        [(Prim op es)
          (Prim op (for/list ([e^ es]) (pass-exp e^)))]
        [(If cnd thn els)
          (define cnd^ (pass-exp cnd))
          (define thn^ (pass-exp thn))
          (define els^ (pass-exp els))
          (If cnd^ thn^ els^)
        ]
        [(SetBang var rhs)
          (SetBang (dict-ref e var) (pass-exp rhs))]
        [(Begin es body)
          (Begin (for/list ([e^ es]) (pass-exp e^)) (pass-exp body))]
        [(WhileLoop cnd body)
          (WhileLoop (pass-exp cnd) (pass-exp body))]
      )
    )
  ))

(define uniquify (λ (p) (send (new (pass-program-mixin pass-Lwhile-uniquify)) pass p)))

(define rco-env (make-parameter #f))

(define pass-Lvec-rco
  (class object%
    (super-new)
    (define/public (pass-exp p)
      (parameterize ([rco-env '()])
        (expand-lets
          (match p
            [(? atom?) p]
            [(Let x e body)
              (Let x (pass-exp e) (pass-exp body))]
            [(Prim op es) 
              (Prim op (for/list ([e es]) (pass-atom e)))]
            [(If cnd thn els) (If (pass-exp cnd) (pass-exp thn) (pass-exp els))]
            [(GetBang _) p]
            [(WhileLoop cnd body) (WhileLoop (pass-exp cnd) (pass-exp body))]
            [(SetBang var rhs) (SetBang var (pass-exp rhs))]
            [(Begin rs e) (Begin (for/list ([r rs]) (pass-exp r)) (pass-exp e))]
            [(Collect (? integer?)) p]
            [(GlobalValue _) p]
            [(Allocate (? integer?) _) p]
          ))
      )
    )
    (define/public (pass-atom p)
      (define p^ (pass-exp p))
      (cond
        [(atom? p^) p^]
        [else (define tmp (gensym 'tmp)) (rco-env (dict-set (rco-env) tmp p^)) (Var tmp)])
    )
    (define/public atom? (match-lambda 
      [(or (Bool _) (Int _) (Var _) (Void )) #t]
      [_ #f]
    ))
    (define/public (expand-lets p)
      (for/fold ([init p]) ([e (rco-env)]) 
        (match-define (cons var rhs) e)
        (Let var rhs init)
      ))
    )
  )

(define pass-rco (pass-program-mixin pass-Lvec-rco))

(define remove-complex-opera* (λ (p) (send (new pass-rco) pass p)))

(define explicate-control-env (make-parameter #f))

(define pass-Lvec-explicate-control
  (class object%
    (super-new)
    (define/public pass (match-lambda 
      [(Program info body)
        (parameterize ([explicate-control-env (make-hash)]) 
          (define b (explicate-tail body))
          (hash-set! (explicate-control-env) 'start b)
          (CProgram info (explicate-control-env))
        ) 
      ]
    ))
    (define/public (explicate-tail p) (match p
      [(Collect _) (Return p)]
      [(Allocate (? integer?) _) (Return p)]
      [(GlobalValue _) (Return p)]
      [(Prim 'vector-ref (list _ (Int _))) (Return p)]
      [(Prim 'vector-set! (list _ (Int _) _)) (Return p)]
      [(Prim 'vector-length (list _)) (Return p)]
      [(Begin es body) 
        (for/foldr ([init-cont (explicate-tail body)]) ([e es])
          (explicate-effect e init-cont)
        )]
      [(or (WhileLoop _ _) (SetBang _ _)) 
        (explicate-effect p (Return (Void )))]
      [(If cnd thn els) (explicate-pred cnd (explicate-tail thn) (explicate-tail els))]
      [(Let x e body) (explicate-assign e x (explicate-tail body))]
      [(or (Prim _ _) (Int _) (Bool _) (Void ) (Var _)) (Return p)]
    ))
    (define/public (explicate-assign p x cont) (match p
      [(or (Collect _) (Prim 'vector-set! _)) 
        (Seq p cont)]
      [(or (Allocate (? integer?) _) 
          (Prim 'vector-ref (list (Int _) _)) (Prim 'vector-length _)
          (GlobalValue _))
        (Seq (Assign (Var x) p) cont)
      ]
      [(Begin es body) 
        (for/foldr ([init-cont (explicate-assign body x cont)]) ([e es])
          (explicate-effect e init-cont)
        )]
      [(or (WhileLoop _ _) (SetBang _ _)) 
        (explicate-effect p cont)]
      [(If cnd thn els) 
        (define cont-blk (create-block cont)) 
        (define thn-ass (explicate-assign thn x cont-blk))
        (define els-ass (explicate-assign els x cont-blk))
        (explicate-pred cnd thn-ass els-ass)
      ]
      [(Let y e body) (explicate-assign e y (explicate-assign body x cont))]
      [(or (Prim _ _) (Int _) (Bool _) (Void ) (Var _)) (Seq (Assign (Var x) p) cont)]
    ))
    (define/public (explicate-pred cnd thn els) 
      (define thn-blk (create-block thn))
      (define els-blk (create-block els))
      (match cnd
        [(Prim 'vector-ref (list _ (Int _))) 
          (define tmp (gensym 'tmp))
          (Seq (Assign (Var tmp) cnd) (IfStmt (Prim 'eq? (Var tmp) (Bool #t)) thn-blk els-blk))
        ]
        [(Begin es body)
          (define cont (explicate-pred body thn els))
          (for/foldr ([cont cont]) ([e es])
            (explicate-effect e cont)
          )
        ]
        [(If cnd2 thn2 els2)
          (define thn2-blk (explicate-pred thn2 thn-blk els-blk))
          (define els2-blk (explicate-pred els2 thn-blk els-blk))
          (explicate-pred cnd2 thn2-blk els2-blk)
        ]
        [(Let y e body)
          (explicate-assign e y (explicate-pred body thn-blk els-blk))
        ]
        [(or (Var _) (Bool _))
          (IfStmt (Prim 'eq? (list cnd (Bool #t))) thn-blk els-blk)
        ]
        [(Prim 'not (list _))
          (IfStmt (Prim 'eq? (list cnd (Bool #f))) thn-blk els-blk)
        ]
        [(Prim (or 'eq? '< '>) (list _ _))
          (IfStmt cnd thn-blk els-blk)
        ]
      ))
    (define/public (explicate-effect p cont) (match p
      [(Collect _) (Seq p cont)]
      [(or (Allocate (? integer?) _) (GlobalValue _)) cont]
      [(Prim 'vector-ref (list _ (Int _))) cont]
      [(Prim 'vector-set! (list _ (Int _) _)) (Seq p cont)]
      [(Prim 'vector-length (list _)) cont]
      [(SetBang var rhs) (explicate-assign rhs var cont)]
      [(WhileLoop cnd body)
        (define body-lbl (gensym 'label))
        (define cnd-lbl (gensym 'label))
        (define cnd^ (explicate-pred cnd (Goto body-lbl) cont))
        (define body^ (explicate-effect body (Goto cnd-lbl))) 
        (define env (explicate-control-env))
        (hash-set! env body-lbl body^)
        (hash-set! env cnd-lbl cnd^)
        (Goto cnd-lbl)
      ]
      [(Begin es body)
        (for/foldr ([init-body (explicate-effect body cont)]) ([e es])
          (explicate-effect e init-body))
      ]
      [(If cnd thn els)
        (define cont-blk (create-block cont))
        (define thn^ (explicate-effect thn cont-blk))
        (define els^ (explicate-effect els cont-blk))
        (explicate-pred cnd thn^ els^)
      ]
      [(Let var rhs body)
        (explicate-assign rhs var (explicate-effect body cont))
      ]
      [(Prim _ args)
        (for/foldr ([init-body cont]) ([arg args])
          (explicate-effect arg init-body) 
        )]
      [(or (Var _) (Int _) (Bool _) (Void )) cont]
    ))
    (define/public create-block (match-lambda 
      [(and raw-jmp (Goto _)) raw-jmp]
      [other (define lbl (gensym 'label)) (hash-set! (explicate-control-env) lbl other) (Goto lbl)]
    ))
  )
)

(define explicate-control (λ (p) (send (new pass-Lvec-explicate-control) pass p)))

(define pass-select-instructions-vec
  (class object%
    (super-new)
    (define/public pass (match-lambda [(CProgram info blocks)
      (define blocks^
        (for/hash ([(block-tag block) (in-hash blocks)])
          (values block-tag (Block '() (pass-instr* block)))
        ))
      (X86Program info blocks^)
    ]))
    (define/public pass-instr* (match-lambda
      [(Seq a res) (pass-instr a (pass-instr* res))]      
      [(Return arg) (pass-instr (Assign (Reg 'rax) arg) '())]
      [(Goto label) (list (Jmp label))]
      [(IfStmt (Prim 'eq? (list (and lhs (or (Int _) (Bool _) (Void ))) (and rhs (or (Int _) (Bool _) (Void ))))) thn els)
        (pass-instr* (if (equal? lhs rhs) thn els))
      ]
      [(IfStmt (Prim 'eq? (list (and lhs (or (Var _))) (and rhs (or (Var _))))) thn els)
        (list
          (Instr 'cmpq (list lhs rhs))
          (JmpIf 'e (Goto-label thn))
          (Jmp (Goto-label els))
        )
      ]
      [(IfStmt (Prim 'eq? (list (and lhs (or (Var _))) (Bool rhs))) thn els)
        (list
          (Instr 'movq (list (Imm (if rhs 1 0)) (Reg 'rax)))
          (Instr 'cmpq (list (Reg 'rax) lhs))
          (JmpIf 'e (Goto-label thn))
          (Jmp (Goto-label els))
        )
      ]
      [(IfStmt (Prim 'eq? (list (and lhs (or (Var _))) (Int rhs))) thn els)
        (list
          (Instr 'movq (list (Imm rhs) (Reg 'rax)))
          (Instr 'cmpq (list (Reg 'rax) lhs))
          (JmpIf 'e (Goto-label thn))
          (Jmp (Goto-label els))
        )
      ]
      [(IfStmt (Prim 'eq? (list (and lhs (or (Int _) (Bool _))) rhs)) thn els)
        (pass-instr* (IfStmt (Prim 'eq? (list rhs lhs)) thn els))
      ]
      [(IfStmt (Prim '< (list (and lhs (or (Int _) (Bool _) (Void ))) (and rhs (or (Int _) (Bool _) (Void ))))) thn els)
        (pass-instr* (if (< lhs rhs) thn els))
      ]
      [(IfStmt (Prim '< (list (and lhs (Var _)) (and rhs (Var _)))) thn els)
        (list 
          (Instr 'cmpq (list rhs lhs))
          (JmpIf 'l (Goto-label thn))
          (Jmp (Goto-label els))
        )
      ]
      [(IfStmt (Prim '< (list (and lhs (Var _)) (Int rhs))) thn els)
        (list
          (Instr 'movq (list (Imm rhs) (Reg 'rax)))
          (Instr 'cmpq (list (Reg 'rax) lhs))
          (JmpIf 'l (Goto-label thn))
          (Jmp (Goto-label els))
        )
      ]
      [(IfStmt (Prim '< (list (Int lhs) (and rhs (Var _)))) thn els)
        (list
          (Instr 'movq (list (Imm lhs) (Reg 'rax)))
          (Instr 'cmpq (list rhs (Reg 'rax)))
          (JmpIf 'l (Goto-label thn))
          (Jmp (Goto-label els))
        )
      ]
    ))
    (define/public (pass-instr instr cont) (match instr
      [(Assign lhs (Int imm))
        (cons (Instr 'movq (list (Imm imm) lhs)) cont)]
      [(Assign lhs (and v (Var _)))
        (cons (Instr 'movq (list v lhs)) cont)]
      [(Assign lhs (Prim '+ (list lhs (Int r))))
        (cons (Instr 'addq (list (Imm r) lhs)) cont)]
      [(Assign lhs (Prim '+ (list lhs (and r (Var _)))))
        (cons (Instr 'addq (list r lhs)) cont)]
      [(Assign lhs (Prim '+ (list other lhs)))
        (pass-instr (Assign lhs (Prim '+ (list lhs other)) cont))]
      [(Assign lhs (Prim '+ (list (Int l) (Int r))))
        (cons (Instr 'movq (list (Imm (+ l r)) lhs)) cont)]
      [(Assign lhs (Prim '+ (list (Int l) (and r (Var _)))))
        #;(cons (Instr 'movq (list (Imm l) lhs))
          (cons (Instr 'addq (list r lhs))
            cont))
        (cons (Instr 'movq (list r lhs))
          (cons (Instr 'addq (list (Imm l) lhs))
            cont))]
      [(Assign lhs (Prim '+ (list (and l (Var _)) (and r (Var _)))))
        (cons (Instr 'movq (list l lhs))
          (cons (Instr 'addq (list r lhs))
            cont))]
      [(Assign lhs (Prim '+ (list (and l (Var _)) (and r (Int _)))))
        (pass-instr (Assign lhs (Prim '+ (list r l))) cont)]
      [(Assign lhs (Prim '- (list (Int a))))
        (cons (Instr 'movq (list (Imm (- a)) lhs)) cont)]
      [(Assign lhs (Prim '- (list (and a (Var _)))))
        (cons (Instr 'movq (list a lhs))
          (cons (Instr 'negq (list lhs)) cont))]
      [(Assign lhs (Prim 'read '()))
        (cons (Callq 'read_int 0)
          (cons (Instr 'movq (list (Reg 'rax) lhs))
            cont))]
      [(Assign lhs (Bool rhs))
        (cons (Instr 'movq (list (if rhs (Imm 1) (Imm 0)) lhs)) cont)]
      [(Assign lhs (Prim 'not (list (and operand (Var _)))))
        (cons (Instr 'movq (list operand lhs)) 
          (cons (Instr 'xorq (list (Imm 1) lhs))
            cont))]
      [(Assign lhs (Prim 'not (list (Bool operand))))
        (cons (Instr 'movq (list (if operand (Imm 0) (Imm 1)) lhs)) 
          cont)]
      [(Assign lhs (Prim '- (list lhs lhs)))
        (cons (Instr 'movq (list (Imm 0) lhs)) cont)]
      [(Assign lhs (Prim '- (list lhs (and op1 (Var _)))))
        (cons (Instr 'subq (list op1 lhs)) cont)]
      [(Assign lhs (Prim '- (list lhs (Int op1))))
        (cons (Instr 'subq (list (Imm op1) lhs)) cont)]
      [(Assign lhs (Prim '- (list (and op0 (Var _)) (and op1 (Var _)))))
        (cons (Instr 'movq (list op0 lhs))
          (cons (Instr 'subq (list op1 lhs))
            cont))]
      [(Assign lhs (Prim '- (list (and op0 (Var _)) (Int op1))))
        (cons (Instr 'movq (list op0 lhs))
          (cons (Instr 'subq (list (Imm op1) lhs))
            cont))]
      [(Assign lhs (Prim '- (list (Int op0) (and op1 (Var _)))))
        (cons (Instr 'movq (list (Imm op0) lhs))
          (cons (Instr 'subq (list op1 lhs))
            cont))]
      [(Assign lhs (Prim '- (list (Int op0) (Int op1))))
        (cons (Instr 'movq (list (Imm (- op0 op1)) lhs))
          cont)]
      [(Assign _ (Void)) cont]
      [(Assign lhs (Prim 'eq? (list (and rhs0 (Var _)) (and rhs1 (Var _)))))
        (cons (Instr 'cmpq (list rhs0 rhs1))
          (cons (Instr 'sete (list (ByteReg 'al))) 
            (cons (Instr 'movzbq (list (ByteReg 'al) lhs))
              cont)))]
      [(Assign lhs (Prim 'eq? (list (and rhs0 (Var _)) (Int rhs1))))
        (cons (Instr 'cmpq (list rhs0 (Imm rhs1)))
          (cons (Instr 'sete (list (ByteReg 'al))) 
            (cons (Instr 'movzbq (list (ByteReg 'al) lhs))
              cont)))]
      [(Assign lhs (Prim 'eq? (list (Int rhs0) (and rhs1 (Var _)))))
        (cons (Instr 'cmpq (list (Imm rhs0) rhs1))
          (cons (Instr 'sete (list (ByteReg 'al))) 
            (cons (Instr 'movzbq (list (ByteReg 'al) lhs))
              cont)))]
      [(Prim 'vector-set! (list (and v (Var _)) (Int idx) (and val (Var _))))
        (cons (Instr 'movq (list val (Reg 'rax)))
          (cons (Instr 'movq (list v (Reg 'r11)))
            (cons (Instr 'movq (list (Reg 'rax) (Deref 'r11 (* 8 (+ idx 1))))) cont)))]
      #;[(Prim 'vector-set! (list (Int v) (Int idx) (and val (Var _))))
        (cons (Instr 'movq (list val (Reg 'rax)))
          (cons (Instr 'movq (list (Imm v) (Reg 'r11)))
            (cons (Instr 'movq (list (Reg 'rax) (Deref 'r11 (* 8 (+ idx 1))))) cont)))]
      [(Prim 'vector-set! (list (and v (Var _)) (Int idx) (Int val)))
        (cons (Instr 'movq (list (Imm val) (Reg 'rax)))
          (cons (Instr 'movq (list v (Reg 'r11)))
            (cons (Instr 'movq (list (Reg 'rax) (Deref 'r11 (* 8 (+ idx 1))))) cont)))]
      #;[(Prim 'vector-set! (list (Int v) (Int idx) (Int val)))
        (cons (Instr 'movq (list (Imm val) (Reg 'rax)))
          (cons (Instr 'movq (list (Imm v) (Reg 'r11)))
            (cons (Instr 'movq (list (Reg 'rax) (Deref 'r11 (* 8 (+ idx 1)))))
              cont)))]
      [(Assign _ (and rhs (Prim 'vector-set! _)))
        (pass-instr rhs cont)]
      [(Assign lhs (Allocate len types))
        (cons (Instr 'movq (list (Global 'free_ptr) (Reg 'r11)))
          (cons (Instr 'addq (list (Imm (* 8 (+ 1 len))) (Global 'free_ptr)))
            (cons (Instr 'movq (list (Imm (cast-types-to-tag types)) (Deref 'r11 0)))
              (cons (Instr 'movq (list (Reg 'r11) lhs)) 
                cont))))]
      [(Collect bytes)
        (cons (Instr 'movq (list (Reg 'r15) (Reg 'rdi)))
          (cons (Instr 'movq (list (Imm bytes) (Reg 'rsi)))
            (cons (Callq 'collect 2)
              cont)))]
      [(Assign lhs (GlobalValue v)) 
        (cons (Instr 'movq (list (Global v) lhs)) cont)]
      [(Assign lhs (Prim 'vector-ref (list (and rhs0 (Var _)) (Int rhs1))))
        (cons (Instr 'movq (list rhs0 (Reg 'r11)))
          (cons (Instr 'movq (list (Deref 'r11 (* 8 (+ rhs1 1))) lhs)) cont))]
    ))
  ))

(define select-instructions (λ (p) (send (new pass-select-instructions-vec) pass p)))

(require "multigraph.rkt")

(define (pass-read-write-mixin super-class)
  (class super-class
    (super-new)
    (define/public (get-write instr) (match instr
      [(Instr 'movq (list _ dst)) (set dst)]
      [(or (Instr 'addq (list _ dst)) (Instr 'subq (list _ dst))) (set dst)]
      [(Callq _ _) (list->set (map Reg caller-save-regs))]
      [(Instr 'negq dst) (set dst)]
      [(Instr 'cmpq (list _ _)) (set )]
      [(Jmp _) (set )]
      [(JmpIf _ _) (set )]
    ))
    (define/public (get-read instr)
      (define raw-set (get-read-with-imm instr))
      (define l (filter (λ (i) (not (or (Imm? i) (Bool? i)))) (set->list raw-set)))
      (list->set l)
    )
    (define/public (get-read-with-imm instr) (match instr
      [(Instr (or 'addq 'subq) (list src dst)) (set src dst)]
      [(Instr 'movq (list src _)) (set src)]
      [(Instr 'negq src) (set src)]
      [(Callq _ count) 
        (set (map Reg (take pass-args-regs count)))]
      [(Jmp _) (set )]
      [(JmpIf _ _) (set )]
      [(Instr 'cmpq (list a b)) (set a b)]
    ))
  ))

(define (pass-default-set-to-read-mixin super-class)
  (class super-class
    (super-new)
    (define/public (default-set-to-read)
      (set (Reg 'rax))
    )
  ))

(define (pass-build-interference-mixin super-class)
  (class super-class
    (super-new) 
    (inherit get-read get-write)
    (define/override (pass p) (match p
      [(X86Program info blocks)
        (define init-graph (init-build-interference-from-blocks blocks (undirected-graph '())))
        (define pass-block^ (pass-block init-graph))
        (define blocks^ (for/hash ([(tag block-value) (in-hash blocks)]) 
          (define block^ (pass-block^ block-value))
          (values tag block^)
        ))
        (X86Program (dict-set info 'interference init-graph) blocks^)
      ]
    ))
    (define/public (init-build-interference-from-blocks blocks graph)
      (for ([(_ block) (in-hash blocks)])
        (init-build-interference (Block-instr* block) graph)
      )
      graph
    )
    (define/public (init-build-interference b graph) (match b
      [(cons instr instr*)
        (for ([w (in-set (get-write instr))])
          (add-vertex! graph w))
        (init-build-interference instr* graph)
      ]
      ['() graph]
    ))
    (define/public ((pass-block interference) block) (match block
      [(Block info instr*)
        (pass-instr* instr* (cdr (dict-ref info 'live)) interference)
        block
      ]
    ))
    (define/public (pass-instr* instr* live graph) (match* (instr* live)
      [((cons instr rest) (cons l live-rest))
        (define writes (get-write instr))
        (define reads l)
        (for ([w (in-set writes)])
          (for ([r (in-set reads)])
            (add-edge! graph w r)
          )
        )
        (pass-instr* rest live-rest graph)
      ]
      [(_ '()) (void)]
    ))
  ))

(define pass-color-graph
  (class pass-abstract
    (super-new)
    (define/override (pass p) (match p
      [(X86Program info blocks)
        (define interference (dict-ref info 'interference))
        (pre-interference-handle interference)
        ; (define color-graph (build-color-graph interference))
        (define int-graph (build-int-color-graph interference (match-lambda 
          [_ #t]
        )))
        (define vec-graph (build-vec-color-graph interference (match-lambda
          [_ #f]
        )))
        (X86Program (dict-set (dict-set info 'color-graph int-graph) 'vec-color-graph vec-graph) blocks)
      ]
    ))
    (define/public (pre-interference-handle interference-graph)
      (for ([r (in-vertices interference-graph)])
        (add-edge! interference-graph r (Reg 'r15))
        (add-edge! interference-graph r (Reg 'rsp))
        (add-edge! interference-graph r (Reg 'rbp))
      )
    )
    (define/public (build-int-color-graph interference-graph int-filter)
      (define q (make-pqueue (λ (a b) (>= (cdr a) (cdr b)))))
      (define (make-pqueue-raw) (make-pqueue >=))
      (for ([n (sequence-filter int-filter (in-vertices interference-graph))])
        (pqueue-push! q (cons n 0))
      )
      (define neighbors-set 
        (for/fold ([nei-set '()]) ([i (range (length caller-and-callee-regs))] [r caller-and-callee-regs])
          (with-handlers ([exn:fail? (λ (_e) nei-set)])
            (for/fold ([nei-set^ nei-set]) ([n (sequence-filter int-filter (in-neighbors interference-graph (Reg r)))])
              (define inner (dict-ref nei-set^ n '()))
              (define inner-2 (set-add inner i))
              (pqueue-push! q (cons n (length inner-2)))
              (dict-set nei-set^ n inner-2)
            ))
        )
      )
      (define selections (for/fold ([select '()]) ([i (range (length caller-and-callee-regs))] [r caller-and-callee-regs])
        (dict-set select (Reg r) i)
      ))
      (define color-graph-value (let color-find ([neighbors-set neighbors-set] [selections selections])
        (cond
          [(= (pqueue-count q) 0) selections]
          [else
            (define pop (pqueue-pop! q))
            (cond
              [(dict-has-key? selections (car pop)) (color-find neighbors-set selections)]
              [else
                (define color (let color-find-2 ([color 0])
                  (if (set-member? (dict-ref neighbors-set (car pop) '()) color)
                    (color-find-2 (+ color 1))
                    color
                    )))
                (define neighbors-set-2 (for/fold ([n neighbors-set]) ([v (sequence-filter int-filter (in-neighbors interference-graph (car pop)))])
                  (define inner (dict-ref n v '()))
                  (define inner-2 (set-add inner color))
                  (pqueue-push! q (cons v (length inner-2)))
                  (dict-set n v inner-2)
                ))
                (color-find neighbors-set-2 (dict-set selections (car pop) color))
              ]
            )
          ]
        )
      ))
      color-graph-value
    )
    (define/public (build-vec-color-graph interference-graph vertice-filter)
      (define pq (make-pqueue (λ (a b) (> (cdr a) (cdr b)))))
      (for ([v (sequence-filter vertice-filter (in-vertices interference-graph))])
        (pqueue-push! pq (cons v 0))
      )
      (define neighbors-set '())
      (let color-find ([neighbors-set neighbors-set] [selections '()]) (cond
        [(= (pqueue-count pq) 0) selections]
        [else
          (define p (pqueue-pop! pq))
          (cond
            [(dict-has-key? selections (car p)) (color-find neighbors-set selections)]
            [else 
              (define select-color (let select-color ([n 0]) 
                (cond
                  [(set-member? (dict-ref neighbors-set (car p) '()) n) (select-color (+ n 1))]
                  [else n]
                )))
              (define neighbors-set^ (for/fold ([neighbors-set neighbors-set]) ([v (sequence-filter vertice-filter (in-vertices interference-graph))])
                (define set^ (set-add (dict-ref neighbors-set v '()) select-color))
                (pqueue-push! pq (cons v (length set^)))
                (dict-set neighbors-set v set^)
              ))
              (color-find neighbors-set^ (dict-set selections (car p) select-color))
            ]
          )
        ]
      ))
    )
  ))

(define color-graph (λ (p) (send (new pass-color-graph) pass p)))

(define pass-allocate-registers
  (class pass-abstract
    (super-new)
    (define/override (pass p) (match p
      [(X86Program info blocks)
        (define table (dict-ref info 'color-graph))
        (define allo (allocate-registers-block table))
        (define slot-num (+ 1 (foldl max -1 (map cdr table))))
        (define blocks^ 
          (for/hash ([(tag block-inner) (in-hash blocks)]) 
            (values tag (allo block-inner)))
        )
        (define stack-size (max 0 (* (- slot-num (length caller-and-callee-regs)) 8)))
        (X86Program (dict-set info 'stack-size stack-size) blocks^)
      ]
    ))
    (define/public ((allocate-instr table) instr) (match instr
      [(Instr i list-value) (Instr i (map (allocate-register table) list-value))]
      [(or (Jmp _) (JmpIf _ _)) instr]
      [(Callq _ _) instr]
    ))
    (define/public ((allocate-register table) value) (match value
      [(or (Imm _) (Bool _)) value]
      [(Reg _) value]
      [_
        (define order (dict-ref table value))
        (match order
          [_ #:when (< order (length caller-and-callee-regs)) 
            (Reg (list-ref caller-and-callee-regs order))] 
          [_ (Deref 'rbp (- (* 8 (- order (length caller-and-callee-regs))) 8))]
        )
      ]
    ))
    (define/public ((allocate-registers-block table) block) (match block
      [(Block info instr*)
        (define instr*^ (map (allocate-instr table) instr*))
        (Block info instr*^)
      ]
    ))
  )
)

(define pass-patch-instructions
  (class pass-abstract
    (super-new)
    (define/override (pass p) (match p
      [(X86Program info blocks)
        (define blocks^ (for/hash ([(tag block-inner) (in-hash blocks)]) 
          (values tag (patch-instr-block block-inner))))
        (X86Program info blocks^)
      ]
    ))
    (define/public (patch-instr-block block) (match block
      [(Block info instr*)
        (define instr*^ (patch-instr* instr*))
        (Block info instr*^)
      ]
    ))
    (define/public (patch-instr* instr*) (match instr*
      [(cons instr rest)
        (match instr
          [(Instr 'movq (list (Deref r0 o0) (Deref r1 o1)))
            (patch-instr*
              (cons 
                (Instr 'movq (list (Deref r0 o0) (Reg 'rax)))
                (cons (Instr 'movq (list (Reg 'rax) (Deref r1 o1)))
                  rest)))
          ]
          [(Instr i (list (Deref r0 o0) (Deref r1 o1)))
            (patch-instr*
              (cons 
                (Instr 'movq (list (Deref r1 o1) (Reg 'rax)))
                (cons (Instr i (list (Deref r0 o0) (Reg 'rax)))
                  (cons (Instr 'movq (list (Reg 'rax) (Deref r1 o1)))
                    rest)))
            )
          ]
          [(Instr 'movq (list (Reg a) (Reg a))) 
            (patch-instr* rest) ; optimize the useless movq op.
          ]
          [(or (Instr 'addq (list (Imm 0) _)) (Instr 'subq (list (Imm 0) _))) 
            (patch-instr* rest) ; drop the non-sense addition and subtraction.
          ]
          [_ (cons instr (patch-instr* rest))]
        )
      ]
      ['() '()]
    ))
  ))

(define pass-prelude-and-conclusion
  (class pass-abstract
    (super-new)
    (define/public (get-prelude p) (match p [(X86Program info _)
      (define stack-size (aligned (dict-ref info 'stack-size) 16))
      (Block '() 
        (list 
          (Instr 'pushq (list (Reg 'rbp))) 
          (Instr 'movq (list (Reg 'rsp) (Reg 'rbp))) 
          (Instr 'subq (list (Imm stack-size) (Reg 'rsp)))
          (Instr 'movq (list (Imm 65536) (Reg 'rdi)))
          (Instr 'movq (list (Imm 65536) (Reg 'rsi)))
          (Callq 'initialize 2)
          (Instr 'movq (list (Global 'rootstack_begin) (Reg 'r15)))
          (Instr 'movq (list (Imm 0) (Deref 'r15 0)))
          (Instr 'addq (list (Imm 8) (Deref 'r15 0)))
          (Jmp 'start)
        ))
    ]))
    (define/public (get-conclusion p) (match p [(X86Program info _)
      (define stack-size (dict-ref info 'stack-size))
      (Block '()
        (list 
          (Instr 'subq (list (Imm 8) (Reg 'r15)))
          (Instr 'addq (list (Imm stack-size) (Reg 'rsp)))
          (Instr 'popq (list (Reg 'rbp)))
          (Retq )
        ))
    ]))
    (define/override (pass p) (match p [(X86Program info blocks)
      (define prelude (get-prelude p))
      (define conclusion (get-conclusion p))
      (define blocks^
        (for/hash ([(tag block) (in-hash blocks)]) 
          (match-define (Block info instr*) block)
          (match instr*
            [(list) (values tag (Block info (append instr* (list (Jmp 'conclusion)))))]
            [_ (match (last instr*)
              [(? Jmp?) (values tag block)]
              [_ (values tag (Block info (append instr* (list (Jmp 'conclusion)))))]
            )]
          )
        ))
      (X86Program info 
        (dict-set 
          (dict-set blocks^ 'main prelude)
          'conclusion conclusion))
    ]))
  ))

(define prelude-and-conclusion (λ (p) (send (new pass-prelude-and-conclusion) pass p)))

(define cnt (make-parameter #f))
(define block-visited (make-parameter #f))
(define id-map (make-parameter #f))
(define graph (make-parameter #f))
(define id-inv-map (make-parameter #f))
(define simple-graph (make-parameter #f))
(define visit-temporary-set (make-parameter #f))

(define (pass-uncover-live-mixin2 clz)
  (class clz
    (super-new)
    (define/public pass (match-lambda [(and p (X86Program info blocks))
      (search-blocks p (get-blocks-graph p))
    ]))
    (define/public get-block-tail (match-lambda
      [(cons (JmpIf _ tag) rest) (cons tag (get-block-tail rest))]
      [(cons (Jmp tag) _) (list tag)]
      [(cons _ rest) (get-block-tail rest)]
      ['() '()]
    ))
    (define/public get-blocks-tail (match-lambda [(X86Program info blocks)
      (for/hash ([(tag block) (in-hash blocks)]) 
        (match-define (Block _ instr*) block)
        (values tag (get-block-tail instr*))
      )
    ]))
    (define/public get-blocks-graph (match-lambda [(X86Program info blocks)
      (define g (make-multigraph '()))
      (for ([(tag block) (in-hash blocks)])
        (match-define (Block _ binstr*) block)
        (define tos (get-block-tail binstr*))
        (add-vertex! g tag)
        (for ([t tos]) (add-directed-edge! g tag t))
      )
      g
    ]))
    (define (topology-order graph)
      (define visited (mutable-set))
      (define (find nodetag cont)
        (set-add! visited nodetag)
        (for ([to (in-neighbors graph nodetag)])
          (unless (set-member? visited to)
            (set! cont (find to cont)))
        )
        (cons nodetag cont)
      )
      (define ret '())
      (for ([n (in-vertices graph)])
        (cond
          [(set-member? visited n) (void)]
          [else 
            (set! ret (find n ret))
          ])
      )
      ret
    )
    (define/public (search-blocks program g)
      (match-define (X86Program info blocks) program)
      (parameterize ([graph g] [cnt 0] [block-visited (make-hash)] [id-map (make-hash)] [id-inv-map (make-hash)] [visit-temporary-set (mutable-set)])
        (for ([tag (in-hash-keys blocks)]) 
          (search-block-impl tag))
        (for ([tag (in-hash-keys blocks)])
          (update-id tag))
        (parameterize ([simple-graph (make-multigraph '())])
          (build-simple-graph)
          (define inv-graph (transpose (simple-graph)))
          (define torder (topology-order inv-graph))
          (define collects (make-hash))
          (let ([bv (block-visited)])
            (for ([tag (in-hash-keys blocks)]) 
              (define id (dict-ref bv tag))
              (define get (dict-ref collects id (λ () '())))
              (dict-set! collects id (cons tag get))
            )
          )
          (define torder^ (for/list ([ti torder]) (dict-ref collects ti)))
          (set! info (dict-set info 'connect-component torder^))
          (set! info (dict-set info 'graph (graph)))
          (X86Program info blocks)
        )
      )
    )
    (define/public (update-id tag)
      (define id (dict-ref (block-visited) tag))
      (cond
        [(equal? id (dict-ref (id-map) tag)) id]
        [else
          (define nxt (update-id (dict-ref (id-inv-map) id)))
          (dict-set! (block-visited) tag nxt)
          nxt
        ])
    )
    (define/public (search-block-impl block-tag)
      (define g (graph))
      (define visited (block-visited))
      (match (dict-ref visited block-tag (λ () #f))
        [#f 
          (define id (cnt))
          (cnt (+ id 1))
          (dict-set! (id-map) block-tag id)
          (dict-set! (id-inv-map) id block-tag)
          (dict-set! visited block-tag id)
          (when (set-member? (visit-temporary-set) block-tag) (error 'search-block-impl "Invalid state, revisit a node. "))
          (set-add! (visit-temporary-set) block-tag)
          (define (drop) (set-remove! (visit-temporary-set) block-tag))
          (define tos
            (for/list ([next-tag (in-neighbors g block-tag)])
              (search-block-impl next-tag)
            ))
          (drop)
          (set! tos (filter (compose not false?) tos))
          (match tos
            ['() id]
            [_ (=> fail)
              (define m (apply min tos))
              (when (> m id) (fail))
              (dict-set! visited block-tag m)
              m
            ]
            [_ id]
          )
        ]
        [a
          (if (set-member? (visit-temporary-set) block-tag) a #f)]
      )
    )
    (define/public (build-simple-graph)
      (define g (graph))
      (define bv (block-visited))
      (define sg (simple-graph))
      (for ([f (in-vertices g)])
        (define f^ (dict-ref bv f))
        (add-vertex! sg f^)
        (for ([t (in-neighbors g f)])
          (define t^ (dict-ref bv t))
          (unless (equal? f^ t^)
            (add-directed-edge! sg f^ t^)
          )
        )
      )
    )
  )
)

(define connect-component (λ (p) (send (new (pass-uncover-live-mixin2 object%)) pass p)))

(define (pass-read-write-mixin2 super-class)
  (class super-class
    (super-new)
    (define/public (get-write-raw instr) (match instr
      [(Instr 'movq (list _ dst)) (set dst)]
      [(or (Instr 'addq (list _ dst)) (Instr 'subq (list _ dst))) (set dst)]
      [(Callq _ _) (list->set (map Reg caller-save-regs))]
      [(Instr 'negq dst) (set dst)]
      [(Instr 'cmpq (list _ _)) (set )]
      [(Jmp _) (set )]
      [(JmpIf _ _) (set )]
    ))
    (define/public (get-write instr)
      (define s (get-write-raw instr))
      (list->set (filter (λ (i) (not (or (Global? i) (Deref? i)))) (set->list s)))
    )
    (define/public (get-read instr)
      (define raw-set (get-read-with-imm instr))
      (define l (filter (λ (i) (not (or (Imm? i) (Bool? i) (Global? i) (Deref? i)))) (set->list raw-set)))
      (list->set l)
    )
    (define/public (get-read-with-imm instr) (match instr
      [(Instr (or 'addq 'subq) (list src dst)) (set src dst)]
      [(Instr 'movq (list src _)) (set src)]
      [(Instr 'negq src) (set src)]
      [(Callq _ count) 
        (set (map Reg (take pass-args-regs count)))]
      [(Jmp _) (set )]
      [(JmpIf _ _) (set )]
      [(Instr 'cmpq (list a b)) (set a b)]
    ))
  ))

(define pass-block-uncover-live-enhanced
  (class (pass-read-write-mixin2 object%)
    (super-new)
    (inherit get-read get-write)
    (define/public pass-block (match-lambda [(Block info instr*)
      (define a (mutable-set))
      (define d (mutable-set))
      (pass-instr* instr* a d) 
      (set! info (dict-set info 'live-change (cons a d)))
      (Block info instr*)
    ]))
    (define/public (pass-instr* instr* add-set drop-set) (match instr*
      [(cons instr rest) 
        (pass-instr* rest add-set drop-set)
        (define r (get-read instr))
        (define w (get-write instr))
        (set-union! drop-set w)
        (set-subtract! add-set w)
        (set-union! add-set r)
        (set-subtract! drop-set r)
      ]
      ['() (void)]
    ))
    (define/public pass (match-lambda [(X86Program info blocks)
      (define blocks^ (for/hash ([(tag block) (in-hash blocks)])
        (values tag (pass-block block))
      ))
      (X86Program info blocks^)
    ]))
  )
)

(define block-uncover-live-enhanced (λ (p) (send (new pass-block-uncover-live-enhanced) pass p)))

(define subqueue (make-parameter #f))

(define (pass-uncover-live-mixin clz)
  (class clz
    (super-new)
    (inherit get-read get-write)
    (define/public (analyze-dataflow graph transfer bottom join)
      (define s (subqueue))
      (define mapping (make-hash))
      (define change-able (mutable-set))
      (define worklist (make-queue))
      (define graph-t (transpose graph))
      (for ([sij (in-vertices graph-t)]) (dict-set! mapping sij bottom))
      (for ([si s])
        (set-clear! change-able)
        (for ([sij si]) (set-add! change-able sij))
        (for ([sij si]) (enqueue! worklist sij))
        (while (not (queue-empty? worklist))
          (define node (dequeue! worklist))
          (define preds (in-neighbors graph-t node))
          (define input
            (match preds
              ['() (set (Reg 'rax))]
              [(cons _ _)
                (for/fold ([state bottom]) ([pred (in-neighbors graph-t node)]) 
                  (join state (dict-ref mapping pred)))
              ]
            ))
          (define output (transfer node input))
          (cond [(not (equal? output (dict-ref mapping node))) 
            (dict-set! mapping node output)
            (for ([s (in-neighbors graph node)])
              (when (set-member? change-able s)
                (enqueue! worklist s))
            )]))
      )
      mapping
    )
    (define/public (pass-instr* instr* end) (match instr*
      ['() (list end)]
      [(or (list (JmpIf _ _) (Jmp _)) (list (Jmp _)))
        (list end)
      ]
      [(cons instr rest)
        (define instr-write (get-write instr))
        (define instr-read (get-read instr))
        (define rest-set (pass-instr* rest end))
        (define s-m (set-subtract (car rest-set) instr-write))
        (define s (set-union s-m instr-read))
        (cons s rest-set)
      ]
    ))
    (define/override (pass p) (match p [(X86Program info blocks)
      (define graph (dict-ref info 'graph))
      (define components (dict-ref info 'connect-component))
      (define live-map
        (parameterize ([subqueue components])
          (analyze-dataflow graph 
            (lambda (node input)
              (define block (dict-ref blocks node))
              (match-define (Block info _) block)
              (match-define (cons add drop) (dict-ref info 'live-change))
              (define input^ (set-subtract input drop))
              (define input^^ (set-union input^ add))
              input^^
            )
            (set)
            set-union
          )
        )
      )
      (define blocks^ (for/hash ([(b-tag b) (in-hash blocks)])
        (match-define (Block info instr*) b)
        (define tos (in-neighbors graph b-tag))
        (define end (for/fold ([end-set (set)]) ([to tos])
          (set-union (dict-ref live-map to) end-set)))
        (values b-tag (Block (dict-set info 'live (pass-instr* instr* end)) instr*))
      ))
      (X86Program info blocks^)
    ]))
  )
)

(define pass-collect-set! 
  (class pass-abstract
    (super-new)
    (define/public (pass-body body) (match body
      [(or (Var _) (Int _) (Bool _) (Void)) (set)]
      [(Let _ rhs body) (set-union (pass-body rhs) (pass-body body))]
      [(SetBang var rhs) (set-union (pass-body rhs) (set var))]
      [(If cnd thn els) (set-union (pass-body cnd) (set-union (pass-body thn) (pass-body els)))]
      [(Prim _ args) (for/fold ([s (set)]) ([a args]) (set-union s (pass-body a)))]
      [(Begin es e) (set-union (for/fold ([s (set)]) ([e es]) (set-union s (pass-body e))) (pass-body e))]
      [(WhileLoop cnd body) (set-union (pass-body cnd) (pass-body body))]
    ))
    (define/override (pass p)
      (match p [(Program info body)
        (Program (dict-set info 'set! (pass-body body)) body)
      ])
    )
  ))

(define collect-set! (lambda (p) (send (new pass-collect-set!) pass p)))

(define pass-uncover-get!-exp 
  (class pass-abstract
    (super-new)
    (define/public ((pass-body set!-vars) body) (match body
      [(Var x) #:when (set-member? set!-vars body) (GetBang x)]
      [(or (Var _) (Int _) (Bool _) (Void)) body]
      [(SetBang var rhs) (SetBang var ((pass-body set!-vars) rhs))]
      [(Let x rhs body) (Let x ((pass-body set!-vars) rhs) ((pass-body set!-vars) body))]
      [(If cnd thn els) (If ((pass-body set!-vars) cnd) ((pass-body set!-vars) thn) ((pass-body set!-vars) els))]
      [(Prim op args) (Prim op (for/list ([a args]) ((pass-body set!-vars) a)))]
      [(WhileLoop cnd body) (WhileLoop ((pass-body set!-vars) cnd) ((pass-body set!-vars) body))]
      [(Begin es e) (Begin (for/list ([e es]) ((pass-body set!-vars) e)) ((pass-body set!-vars) e))]
    ))
    (define/override (pass p) (match p [(Program info body)
      (Program (dict-remove info 'set!) ((pass-body (dict-ref info 'set!)) body))
    ]))
  ))

(define uncover-get!-exp (λ (p) (send (new pass-uncover-get!-exp) pass p)))

(define (patch-instructions p) (send (new pass-patch-instructions) pass p))

(define pass-expose-allocation
  (class pass-program-abstract
    (super-new)
    (define/public (expand-env-r body env) (match env
      [(cons (cons x v) rest) (Let x v (expand-env-r body rest))]
      ['() body]
    ))
    (define/override pass-exp (match-lambda
      [(HasType (Prim 'vector es) types)
        (define-values (es^ env^) (for/fold ([es^ '()] [env^ '()]) ([e es])
          (define tmp (gensym 'tmp))
          (values (cons tmp es^) (dict-set env^ tmp (pass-exp e)))
          ))
        (define bytes (* (+ 1 (length es)) 8))
        (define pre-collect (λ (b) (Let '_ (If 
          (Prim '< (list 
            (Prim '+ (list (GlobalValue 'free_ptr) (Int bytes)))
            (GlobalValue 'fromspace_end))) (Void) (Collect bytes)) b)))
        (define v (gensym 'tmp))
        (define inner (for/foldr ([b (Var v)]) ([e es^] [idx (in-range (length es))])
          (Let '_ (Prim 'vector-set! (list (Var v) (Int idx) (Var e))) b)
          ))
        (set! inner (expand-env-r inner env^))
        (set! inner (Let v (Allocate (length es) types) inner))
        (set! inner (pre-collect inner))
        inner
      ]
      [(Prim o es) (Prim o (for/list ([e es]) (pass-exp e)))]
      [(If cnd thn els) (If (pass-exp cnd) (pass-exp thn) (pass-exp els))]
      [(Begin es e) (Begin (for/list ([e es]) (pass-exp e)) (pass-exp e))]
      [(Let x rhs body) (Let x (pass-exp rhs) (pass-exp body))]
      [(SetBang var rhs) (SetBang var (pass-exp rhs))]
      [(WhileLoop cnd body) (WhileLoop (pass-exp cnd) (pass-exp body))]
      [body body]
    ))
  ))

(define expose-allocation (λ (p) (send (new pass-expose-allocation) pass p)))


(define (pass-build-interference-mixin2 super-class)
  (class super-class
    (super-new) 
    (inherit get-read get-write)
    (define/override (pass p) (super pass p))
  ))

(define build-interference (λ (p) (send (new 
  (pass-build-interference-mixin2 
  (pass-build-interference-mixin 
  (pass-read-write-Cwhile-mixin pass-abstract)))) pass p)))

(define (pass-read-write-Cwhile-mixin super-class)
  (class (pass-read-write-mixin super-class)
    (super-new)
    (inherit get-read-with-imm)
    (define/override (get-write instr) 
      (define set^ (super get-write instr))
      (list->set (filter (λ (i) (not (or (Global? i) (Deref? i)))) (set->list set^)))
    )
    (define/override (get-read instr)
      (define set^ (get-read-with-imm instr))
      (define l (filter (λ (i) (not (or (Imm? i) (Bool? i) (Global? i) (Deref? i)))) (set->list set^)))
      (list->set l)
    )
  ))

(define uncover-live (λ (p) (send (new 
  (pass-uncover-live-mixin
    (pass-read-write-Cwhile-mixin
      (pass-default-set-to-read-mixin pass-abstract)))) pass p)))

(define pass-allocate-registers-Lvec
  (class pass-allocate-registers
    (super-new)
    (define/override ((allocate-register table) value) (match value
      [(or (Global _) (Deref _ _)) value]
      [_ ((super allocate-register table) value)]
    ))
    (define/override (pass p) (match (super pass p) [(X86Program info blocks)
      ; (pretty-print (map (λ (b) (cons (car b) (Block-instr* (cdr b)))) blocks))
      (X86Program (dict-set info 'num-root-spills 0) blocks)
    ]))
  )
)

(define allocate-registers (λ (p) (send (new pass-allocate-registers-Lvec) pass p)))

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

(define collect-vector-variables 
  (class pass-abstract
    (super-new)
    (define/override pass (match-lambda 
      [(CProgram info blocks)
        (error 'pass "unimpl")
      ]))
    (define/public ((pass-block env) block)
      (error 'pass-block "unimpl")
    )
    (define/public (merge-type-info lhs rhs)
      (for/fold ([r lhs]) ([(k v) (in-dict-pairs rhs)])
        (define r-sub (dict-ref r k '()))
        (define r-sub^ (set-union r-sub v))
        (dict-set r k r-sub^)
      )
    )
    (define/public (get-all-uninit-types block)
      (void)
    )
  ))

(define collect-type-info
  (class pass-abstract
    (super-new)
    (define/override pass (match-lambda 
      [(Program info exp)
        (error 'pass) 
      ]
    ))
    (field [prim-table (make-hash)])
    (define/public (pass-exp table) (match-lambda
      [(Let var rhs body)
        (define rhs-t ((pass-exp table) rhs))
        (hash-set! table var rhs-t) 
        ((pass-exp table) body)
      ]
      [(Prim op operands) 
        prim-table
        (error 'prim)
      ]
      [(If cnd thn els)
        (define cnd-t ((pass-exp table) cnd))
        (match cnd-t
          ['Void (void)]
          [_ (collect (Void) cnd-t cnd)])
        (define thn-t ((pass-exp table) thn))
        (define els-t ((pass-exp table) els))
        (cond
          [(not (equal? thn-t els-t)) (collect thn-t els-t els)])
        thn-t
      ]
      [(WhileLoop cnd body)
        (define cnd-t ((pass-exp table) cnd))
        (match cnd-t 
          ['Boolean (void)]
          [_ (collect 'Boolean cnd-t cnd)])
        ((pass-exp table) body)
        'Void
      ]
      [(SetBang var rhs)
        (define rhs-t ((pass-exp table) rhs))
        (match (hash-ref table var (λ () #f))
          [#f (hash-set! table var rhs-t)]
          [var-t 
            (unless (equal? var-t rhs-t) (collect var-t rhs-t rhs))
          ])
        'Void
      ]
      [(Begin es body)
        (for ([e es]) ((pass-exp table) e))
        ((pass-exp table) body)
      ]
      [(GetBang var)
        (hash-ref table var (λ () #f))
      ]
      [(Int _) 'Integer]
      [(Bool _) 'Boolean]
      [(GlobalValue _) 'Integer]
      [(HasType _ t) t]
      [(Collect _) 'Void]
    ))
    (abstract collect)
  ))

; (debug-level 2)
(define compiler-passes
  `(
    ("shrink" ,shrink ,interp-Lvec-prime ,type-check-Lvec)
    ("uniquify" ,uniquify ,interp-Lvec-prime ,type-check-Lvec)
    ("collect-set!" ,collect-set! ,interp-Lvec-prime ,type-check-Lvec)
    ("uncover-get!-exp" ,uncover-get!-exp ,interp-Lvec-prime ,type-check-Lvec-has-type #;,type-check-Lvec)
    ("expose-allocation" ,expose-allocation ,interp-Lvec-prime ,type-check-Lvec)
    ("remove complex opera*" ,remove-complex-opera* ,interp-Lvec-prime ,type-check-Lvec)
    ("explicate control" ,explicate-control ,interp-Cvec ,type-check-Cvec)
    ("instruction selection" ,select-instructions ,interp-pseudo-x86-2)
    ("connect component preparation" ,connect-component ,interp-pseudo-x86-2)
    ("block uncover live enhanced" ,block-uncover-live-enhanced ,interp-pseudo-x86-2)
    ("uncover live" ,uncover-live ,interp-pseudo-x86-2)
    ("build interference graph" ,build-interference ,interp-pseudo-x86-2)
    ("build color graph" ,color-graph ,interp-pseudo-x86-2)
    ("allocate registers" ,allocate-registers ,interp-x86-2)
    ("patch instructions" ,patch-instructions ,interp-x86-2)
    ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-2)
    ; ("patch instructions" ,patch-instructions ,interp-x86-2)
  ))

(provide compiler-passes)