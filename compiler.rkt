#lang racket
(require racket/set)
(require racket/fixnum)
(require "interp.rkt")
(require "utilities.rkt")
(provide (all-defined-out))

(require "interp-Lvec.rkt")
(require "type-check-Lvec.rkt")
(require "interp-Cwhile.rkt")
(require "type-check-Cwhile.rkt")

(require graph)
(require "graph-printing.rkt")
(require "priority_queue.rkt")

(require data/queue)

(define (aligned x a) (cond
  [(zero? (modulo x a)) x]
  [else (+ x (- a (modulo x a)))]))

(define caller-save-regs 
  (list 'rax 'rcx 'rdx 'rsi 'rdi 'r8 'r9 'r10 'r11)
)

(define callee-save-regs
  (list 'rsp 'rbp 'rbx 'r12 'r13 'r14 'r15)
)

(define pass-args-regs
  (list 'rdi 'rsi 'rdx 'rcx 'r8 'r9)
)

(define pass-abstract
  (class object%
    (super-new)
    (abstract pass)
    ))

(define pass-shrink
  (class pass-abstract
    (super-new)
    (define/override (pass p)
      (match p
        [(Program info exp)
          (define exp-2 (pass-exp exp))
          (Program info exp-2)]))
    (define/public (pass-exp p)
      (match p
        [(Prim 'and (list a b)) (If (pass-exp a) (pass-exp b) (Bool #f))]
        [(Prim 'or (list a b)) (If (pass-exp a) (Bool #t) (pass-exp b))]
        [(Let x e body) (Let x (pass-exp e) (pass-exp body))]
        [(If cnd thn els) (If (pass-exp cnd) (pass-exp thn) (pass-exp els))]
        [(Prim op es) (Prim op (map (λ (v) (pass-exp v)) es))]
        [_ p]
      ))
  ))

(define shrink (λ (p) (send (new pass-shrink) pass p)))

(define pass-Lvar-uniquify
  (class pass-abstract
    (super-new)
    (define/override (pass p)
      (match p
        [(Program info body)
          (define body-2 ((pass-exp '()) body))
          (Program info body-2)
        ]))
    (define/public ((pass-exp env) exp) (match exp
        [(Var x) (Var (dict-ref env x))]
        [(Int _) exp]
        [(Let x exp body)
          (define exp^ ((pass-exp env) exp))
          (define x^ (gensym x))
          (define body^ ((pass-exp (dict-set env x x^)) body))
          (Let x^ exp^ body^)]
        [(Prim op es)
          (Prim op (for/list ([e es]) ((pass-exp env) e)))]
    ))
  )
)

(define pass-Lif-uniquify
  (class pass-Lvar-uniquify
    (super-new)
    (define/override ((pass-exp env) exp) (match exp
        [(Bool _) exp]
        [(If cnd thn els)
          (define cnd-2 ((pass-exp env) cnd))
          (define thn-2 ((pass-exp env) thn))
          (define els-2 ((pass-exp env) els))
          (If cnd-2 thn-2 els-2)
        ]
        [_ ((super pass-exp env) exp)]
    ))
  ))

(define pass-Lvar-rco
  (class pass-abstract
    (super-new)
    (define/override (pass p) (match p
      [(Program info body) (Program info (pass-exp body))]))
    (define/public ((exp-cast-atom env) p) (match p
      [(or (Int _) (Var _)) (values env p)]
      [_ (let ([x (gensym 'tmp)])
        (values (dict-set env x p) (Var x)))]
    ))
    (define/public ((expand-lets env) p) (match env
      ['() p]
      [(cons (cons x e) rest)
        ((expand-lets rest) (Let x e p))]
    ))
    (define/public ((pass-atom env) p)
      ((exp-cast-atom env) (pass-exp p))
    )
    (define/public (pass-exp p)
      (match p
        [(Prim op es)
          (define-values (env-2 es-2)
            (for/foldr ([env-c '()] [es-c '()]) ([e es])
              (define-values (env-nxt e-nxt) ((pass-atom env-c) e))
              (values env-nxt (cons e-nxt es-c))
              ))
          ((expand-lets env-2) (Prim op es-2))
        ]
        [(Let x e body)
          (define new-e (pass-exp e))
          (define new-body (pass-exp body))
          (Let x new-e new-body)
        ]
        [_ p]
      )
    )
  ))

(define pass-Lif-rco
  (class pass-Lvar-rco
    (super-new)
    (define/override ((exp-cast-atom env) p) (match p
      [(Bool _) p]
      [_ ((super exp-cast-atom env) p)])
    )
    (define/override (pass-exp p) (match p
      [(If cnd thn els)
        (If (pass-exp cnd) (pass-exp thn) (pass-exp els))]
      [_ (super pass-exp p)]
    ))
  ))

;; explicate-control : Lvar^mon -> Cvar
(define pass-Lvar-explicate-control
  (class pass-abstract
    (super-new)
    (define/override (pass p) (match p
      [(Program info exp)
        (CProgram info (dict-set '() 'start (explicate-tail exp)))
      ])
    )
    (define/public (explicate-tail p) (match p
      [(Let x e body) (explicate-assign e x (explicate-tail body))]
      [_ (Return p)]
    ))
    (define/public (explicate-assign p x cont) (match p
      [(Let y e body) (explicate-assign e y (explicate-assign body x cont))]
      [_ (Seq (Assign (Var x) p) cont)]
    ))
  ))

(define pass-Lif-explicate-control
  (class pass-Lvar-explicate-control
    (super-new)
    (define/override ((explicate-tail env) p) (match p
      [(If cnd thn els) 
        (define-values (env-2 thn^) ((explicate-tail env) thn))
        (define-values (env-3 els^) ((explicate-tail env-2) els))
        ((explicate-pred env-3) cnd thn^ els^)
      ]
      [(Let x e body) 
        (define-values (env-2 body-2) ((explicate-tail env) body))
        ((explicate-assign env-2) e x body-2)
      ]
      [_ (values env (Return p))]
    ))
    (define/override ((explicate-assign env) p x cont) (match p
      [(If cnd thn els)
        (define-values (env-2 cont^) ((create-block env) cont))
        (define-values (env-3 thn^) ((explicate-assign env-2) thn x cont^))
        (define-values (env-4 els^) ((explicate-assign env-3) els x cont^))
        ((explicate-pred env-4) cnd thn^ els^)
      ]
      [(Let y e body)
        (define-values (env-2 cont^) ((explicate-assign env) body x cont))
        (define-values (env-3 body^) ((explicate-assign env-2) e y cont^))
        (values env-3 body^)
      ]
      [_ (values env (Seq (Assign (Var x) p) cont))]
    ))
    (define/public ((explicate-pred env) cnd thn els) (match cnd
      [(If cnd-2 thn-2 els-2)
        (define-values (env^ thn^ els^) ((create-block-2 env) thn els))
        (define-values (env^^ thn^^) ((explicate-pred env^) thn-2 thn^ els^))
        (define-values (env^^^ els^^) ((explicate-pred env^^) els-2 thn^ els^))
        ((explicate-pred env^^^) cnd-2 thn^^ els^^)
      ]
      [(Let y e body)
        (define-values (env-2 body^) ((explicate-pred env) body thn els))
        ((explicate-assign env-2) e y body^)
      ]
      [_
        (define-values (env^ thn^ els^) ((create-block-2 env) thn els))
        (match cnd
          [(Var _)
            (values env^ (IfStmt (Prim 'eq (list cnd (Bool #t))) thn^ els^))]
          [(Bool b)
            (values env (if b thn els))]
          [(Prim 'not (list _))
            (values env^ (IfStmt (Prim 'eq (list cnd (Bool #f))) thn^ els^))]
          [(Prim _ (list _ _))
            (values env^ (IfStmt cnd thn^ els^))]
        )
      ]
    ))
    (define/public ((create-block env) stmt) (match stmt
      [(Goto _) (values env stmt)]
      [_ 
        (define lbl (gensym 'label))
        (define env-2 (dict-set env lbl stmt))
        (values env-2 (Goto lbl))
      ]
    ))
    (define/public ((create-block-2 env) stmt0 stmt1)
      (define-values (env^ stmt0^) ((create-block env) stmt0))
      (define-values (env^^ stmt1^) ((create-block env^) stmt1))
      (values env^^ stmt0^ stmt1^)
    )
    (define/override (pass p) (match p
      [(Program info exp)
        (define-values (env stmt) ((explicate-tail '()) exp))
        (define env^ (dict-set env 'start stmt))
        (CProgram info env^)
      ]))
  ))

;; select-instructions : Cvar -> x86var
(define pass-select-instructions
  (class pass-abstract
    (super-new)
    (define/override (pass p) (match p
      [(CProgram info blocks)
        (define blocks^ 
          (for/list ([b blocks]) 
            (define b^ (pass-instr* (cdr b)))
            (cons (car b) (Block '() b^))
          ))
        (X86Program info 
          blocks^
        )
      ]
    ))
    (define/public (pass-instr* stmts) (match stmts
      [(Seq a rest)
        (append (pass-instr a) (pass-instr* rest))]
      [(Return arg)
        (pass-instr (Assign (Reg 'rax) arg))]
    ))
    (define/public (cast x) (match x
      [(Int n) (Imm n)]
      [_ x]))
    (define/public (pass-instr a) (match a
      [(Assign lhs rhs)
        (define p-2 (match rhs
          [(or (Int _) (Var _))
            (list (Instr 'movq (list (cast rhs) lhs)))]
          [(Prim '+ (list a b))
            (list (Instr 'movq (list (cast a) (Reg 'rax))) (Instr 'addq (list (cast b) (Reg 'rax))) (Instr 'movq (list (Reg 'rax) lhs)))]
          [(Prim '- (list a))
            (list (Instr 'movq (list (cast a) (Reg 'rax))) (Instr 'negq (list (Reg 'rax))) (Instr 'movq (list (Reg 'rax) lhs)))]
          [(Prim 'read '()) 
            (list (Callq 'read_int 0) (Instr 'movq (list (Reg 'rax) lhs)))]
          ))
        p-2]))
  ))

(define pass-select-instructions-If
  (class pass-select-instructions
    (super-new)
    (define/override (pass-instr* stmts) (match stmts
      [(IfStmt cnd thn els)
        (match cnd
          [(Prim 'eq? (list lhs rhs))
            (define comp-rst (λ ()
              (if (equal? lhs rhs)
                (list (Jmp (Goto-label thn)))
                (list (Jmp (Goto-label els)))
                )
            ))
            (match* (lhs rhs)
              [((Var _) (Var _))
                (list
                  (Instr 'cmpq (list (cast lhs) (cast rhs)))
                  (JmpIf 'e (Goto-label thn))
                  (Jmp (Goto-label els))
                )
              ]
              [((Var _) _)
                (list
                  (Instr 'movq (list (cast rhs) (Reg 'rax)))
                  (Instr 'cmpq (list (cast lhs) (Reg 'rax)))
                  (JmpIf 'e (Goto-label thn))
                  (Jmp (Goto-label els))
                )
              ]
              [(_ (Var _))
                (pass-instr* (IfStmt (Prim 'eq? (list rhs lhs))) thn els)
              ]
              [((Int _) (Int _)) 
                (comp-rst)
              ]
              [((Bool _) (Bool _))
                (comp-rst)
              ]
            )
          ]
          [(Prim '< (list lhs rhs))
            (define comp-rst (lazy
              (if (< (Imm-value lhs) (Imm-value rhs))
                (list (Jmp (Goto-label thn)))
                (list (Jmp (Goto-label els)))
              )
            ))
            (match* (lhs rhs)
              [((Var _) (Var _))
                (list
                  (Instr 'cmpq (list (cast rhs) (cast lhs)))
                  (JmpIf 'l (Goto-label thn))
                  (Jmp (Goto-label els))
                )
              ]
              [((Var _) _)
                (list
                  (Instr 'movq (list (cast rhs) (Reg 'rax)))
                  (Instr 'cmpq (list (Reg 'rax) (cast lhs)))
                  (JmpIf 'l (Goto-label thn))
                  (Jmp (Goto-label els))
                )
              ]
              [(_ (Var _))
                (list
                  (Instr 'movq (list (cast lhs) (Reg 'rax)))
                  (Instr 'cmpq (list (cast rhs) (Reg 'rax)))
                  (JmpIf 'l (Goto-label thn))
                  (Jmp (Goto-label els))
                )
              ]
              [((Int _) (Int _)) 
                (comp-rst)
              ]
            )
          ]
        )
      ]
      [(Goto label)
        (list (Jmp label))
      ]
      [_ (super pass-instr* stmts)]
    ))
    (define/override (cast x) (match x
      [(Bool b) (Imm (if b 1 0))]
      [_ (super cast x)]))
    (define/override (pass-instr stmt) (match stmt
      [(Assign lhs rhs) (match rhs
        [(Bool _)
          (list (Instr 'movq (list (cast rhs) lhs)))
        ]
        [(Prim 'not (list e))
          (list
            (Instr 'movq (list (cast e) lhs))
            (Instr 'xorq (list (Imm 1) lhs))
          )
        ]
        [(Prim 'eq? (list inner-lhs inner-rhs))
          (list
            (Instr 'cmpq (list (cast inner-lhs) (cast inner-rhs)))
            (Instr 'sete (list (ByteReg 'al)))
            (Instr 'movzbq (list (ByteReg 'al) lhs))
          )
        ]
        [(Prim '- (list inner-lhs inner-rhs))
          (list 
            (Instr 'movq (list (cast inner-lhs) lhs))
            (Instr 'subq (list (cast inner-rhs) lhs))
          )
        ]
        [_ (super pass-instr stmt)]
      )]
    ))
  ))

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
        (define blocks^ (for/list ([block blocks]) (match block [(cons tag block-value)
          (define block^ (pass-block^ block-value))
          (cons tag block^)
        ])))
        (X86Program (dict-set info 'interference init-graph) blocks^)
      ]
    ))
    (define/public (init-build-interference-from-blocks blocks graph)
      (for ([b blocks]) (match b [(cons _ block)
        (init-build-interference (Block-instr* block) graph)
      ]))
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
            (unless (equal? w r) (add-edge! graph w r))
            )
          )
        (pass-instr* rest live-rest graph)
      ]
      [(_ '()) (void)]
    ))
  ))

(define build-interference (λ (p) (send (new (pass-build-interference-mixin (pass-read-write-mixin pass-abstract))) pass p)))

(define pass-color-graph
  (class pass-abstract
    (super-new)
    (define/override (pass p) (match p
      [(X86Program info blocks)
        (define color-graph (build-color-graph (dict-ref info 'interference)))
        (X86Program (dict-set info 'color-graph color-graph) blocks)
      ]
    ))
    (define/public (build-color-graph interference-graph)
      (define q (make-pqueue (λ (a b) (> (cdr a) (cdr b)))))
      (for ([n (in-vertices interference-graph)])
        (pqueue-push! q (cons n 0))
      )
      (define neighbors-set 
        (with-handlers ([exn:fail? (λ (_e) '())])
          (for/fold ([nei-set '()]) ([n (in-neighbors interference-graph (Reg 'rax))])
            (define inner (dict-ref nei-set n '()))
            (define inner-2 (set-add inner 0))
            (pqueue-push! q (cons n (length inner-2)))
            (dict-set nei-set n inner-2)
          ))
      )
      (define color-graph-value (let color-find ([neighbors-set neighbors-set] [selections (dict-set '() (Reg 'rax) 0)])
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
                (define neighbors-set-2 (for/fold ([n neighbors-set]) ([v (in-neighbors interference-graph (car pop))])
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
          (for/list ([block blocks]) (match block [(cons tag block-inner)
            (cons tag (allo block-inner))]))
        )
        (define stack-size (max 0 (* (- slot-num (length caller-save-regs)) 8)))
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
      [_
        (define order (dict-ref table value))
        (match order
          [_ #:when (< order (length caller-save-regs)) (Reg (list-ref caller-save-regs order))] 
          [_ (Deref 'rbp (- (* 8 (- order (length caller-save-regs))) 8))]
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

(define allocate-registers (λ (p) (send (new pass-allocate-registers) pass p)))

(define pass-patch-instructions
  (class pass-abstract
    (super-new)
    (define/override (pass p) (match p
      [(X86Program info blocks)
        (define blocks^ (for/list ([block blocks]) (match block [(cons tag block-inner)
          (cons tag (patch-instr-block block-inner))]))
        )
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
          (Jmp 'start)
        ))
    ]))
    (define/public (get-conclusion p) (match p [(X86Program info _)
      (define stack-size (dict-ref info 'stack-size))
      (Block '()
        (list 
          (Instr 'addq (list (Imm stack-size) (Reg 'rsp)))
          (Instr 'popq (list (Reg 'rbp)))
          (Retq )
        ))
    ]))
    (define/override (pass p) (match p [(X86Program info blocks)
      (define prelude (get-prelude p))
      (define conclusion (get-conclusion p))
      (define blocks^
        (for/list ([bl blocks]) (match bl
          [(cons tag (Block info instr*))
            (match instr*
              [(list) (cons tag (Block info (append instr* (list (Jmp 'conclusion)))))]
              [_ (match (last instr*)
                [(? Jmp?) bl]
                [_ (cons tag (Block info (append instr* (list (Jmp 'conclusion)))))]
              )]
            )
          ])
        ))
      (X86Program info 
        (dict-set 
          (dict-set blocks^ 'main prelude)
          'conclusion conclusion))
    ]))
  ))

(define prelude-and-conclusion (λ (p) (send (new pass-prelude-and-conclusion) pass p)))

(define (pass-uncover-live-mixin clz)
  (class clz
    (super-new)
    (inherit get-read get-write)
    (inherit default-set-to-read)
    (define/public (get-block-end instr*) (match instr*
      [(cons (or (JmpIf _ _) (Jmp _)) _) instr*]
      [(cons _ rest) (get-block-end rest)]
      ['() '()]
    ))
    (define/public (get-block-next instr*)
      (define jmps (get-block-end instr*))
      (define get-label (lambda (instr) (match instr 
        [(Jmp lbl) lbl]
        [(JmpIf _ lbl) lbl]
      )))
      (map get-label jmps)
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
    (define/public (analyze-dataflow graph transfer bottom join)
      (define mapping (make-hash))
      (for ([v (in-vertices graph)])
        (dict-set! mapping v bottom))
      (define worklist (make-queue))
      (for ([v (in-vertices graph)])
        (enqueue! worklist v))
      (define graph-t (transpose graph))
      (while (not (queue-empty? worklist))
        (define node (dequeue! worklist))
        (define preds (in-neighbors graph-t node))
        (define input
          (match preds
            ['() (default-set-to-read)]
            [(cons _ _)
              (for/fold ([state bottom]) ([pred (in-neighbors graph-t node)]) 
                (join state (dict-ref mapping pred)))
            ]
          ))
        (define output (transfer node input))
        (cond [(not (equal? output (dict-ref mapping node))) 
          (dict-set! mapping node output)
          (for ([s (in-neighbors graph node)])
            (enqueue! worklist s))]))
      mapping
    )
    (define/override (pass p) (match p [(X86Program info blocks)
      (define graph (make-multigraph '()))
      (for ([bl blocks]) (match bl [(cons b-tag _)
        (add-vertex! graph b-tag)
      ]))
      (for ([bl blocks]) (match bl [(cons b-tag b)
        (define tos (get-block-next (Block-instr* b)))
        (for ([to tos])
          (add-directed-edge! graph b-tag to))
      ]))
      (define live-map (analyze-dataflow graph 
        (lambda (node input)
          (define block (dict-ref blocks node))
          (define pass-instr*-rst (pass-instr* (Block-instr* block) input))
          (car pass-instr*-rst)
        )
        (set)
        set-union
      ))
      (define blocks^ (for/list ([bl blocks]) (match bl [(cons b-tag (Block info instr*))
        (define tos (get-block-next instr*))
        (define end (for/fold ([end-set (set)]) ([to tos])
          (set-union (dict-ref live-map to) end-set)))
        (cons b-tag (Block (dict-set info 'live (pass-instr* instr* end)) instr*))
      ])))
      (X86Program info blocks^)
    ]))
  )
)

(define uncover-live (λ (p) (send (new 
  (pass-uncover-live-mixin
    (pass-read-write-mixin (pass-default-set-to-read-mixin pass-abstract)))) pass p)))
  
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
      (Program info ((pass-body (dict-ref info 'set!)) body))
    ]))
  ))

(define uncover-get!-exp (λ (p) (send (new pass-uncover-get!-exp) pass p)))

(define pass-Lwhile-rco
  (class pass-Lif-rco
    (super-new)
    (define/override (pass-exp p) (match p
      [(GetBang _) p] 
      [(WhileLoop cnd body) (WhileLoop (pass-exp cnd) (pass-exp body))]
      [(SetBang var rhs) (SetBang var (pass-exp rhs))] 
      [(Begin rs e) (Begin (for/list ([r rs]) (pass-exp r)) (pass-exp e))]
      [_ (super pass-exp p)]
    ))
  ))

(define pass-Lwhile-explicate-control
  (class pass-Lif-explicate-control
    (super-new)
    (inherit create-block)
    (define/public ((explicate-effect env) p cont) (match p
      [(SetBang var rhs) ((explicate-assign env) rhs var cont)]
      [(WhileLoop cnd body)
        (define body-lbl (gensym 'label))
        (define cnd-lbl (gensym 'label))
        (define-values (env^ cnd^) ((explicate-pred env) cnd (Goto body-lbl) cont))
        (define-values (env^^ body^) ((explicate-effect env^) body (Goto cnd-lbl)))
        (define env^^^ (dict-set env^^ body-lbl body^))
        (define env^^^^ (dict-set env^^^ cnd-lbl cnd^))
        (values env^^^^ (Goto cnd-lbl))
      ]
      [(Begin es body)
        (define-values (env^ body^) ((explicate-effect env) body cont))
        (for/foldr ([env-c env^] [body-c body^]) ([e es])
          ((explicate-effect env-c) e body-c))
      ]
      [(If cnd thn els)
        (define-values (env^ cont^) ((create-block env) cont))
        (define-values (env^^ thn^) ((explicate-effect env^) thn cont^))
        (define-values (env^^^ els^) ((explicate-effect env^^) els cont^))
        (define-values (env^^^^ cnd^) ((explicate-pred env^^^) cnd thn^ els^)) 
        (values env^^^^ cnd^)
      ]
      [(Let var rhs body)
        (define-values (env^ body^) ((explicate-effect env) body cont))
        (define-values (env^^ rhs^) ((explicate-assign env^) rhs var body^))
        (values env^^ rhs^)
      ]
      [(Prim _ args)
        (for/foldr ([env env] [arg-c cont]) ([arg args])
          ((explicate-effect env) arg arg-c))
      ]
      [_ (values env cont)]
    ))
    (define/public (get-void-rst) (Return (Void)))
    (define/override ((explicate-pred env) cnd thn els) (match cnd
      [(Begin es body)
        (define-values (env^ body^) ((explicate-pred env) body thn els))
        (for/foldr ([env-c env^] [body-c body^]) ([e es])
          ((explicate-effect env-c) e body-c))
      ]
      [(or (WhileLoop _ _) (SetBang _ _))
        (error 'explicate-pred "unexpected type of cnd with actual Void")
      ]
      [_ ((super explicate-pred env) cnd thn els)]
    ))
    (define/override ((explicate-tail env) p) (match p
      [(Begin es body)
        (define-values (env^ body^) ((explicate-tail env) body))
        (for/foldr ([env-c env^] [body-c body^]) ([e es])
          ((explicate-effect env-c) e body-c))
      ]
      [(or (WhileLoop _ _) (SetBang _ _))
        ((explicate-effect env) p (get-void-rst))]
      [_ ((super explicate-tail env) p)]
    ))
    (define/override ((explicate-assign env) p x cont) (match p
      [(or (WhileLoop _ _) (SetBang _ _)) ((explicate-effect env) p cont)]
      [(Begin es body)
        (define-values (env^ body^) ((explicate-assign env) body x cont))
        (for/foldr ([env-c env^] [body-c body^]) ([e es])
          ((explicate-effect env-c) e x body-c))
      ]
      [_ ((super explicate-assign env) p x cont)]
    ))
  ))

(define pass-Lwhile-uniquify
  (class pass-Lif-uniquify
    (super-new)
    (define/override ((pass-exp env) exp) (match exp
      [(SetBang var rhs) 
        (SetBang (dict-ref env var) ((pass-exp env) rhs))]
      [(Begin es body)
        (Begin (for/list ([e es]) ((pass-exp env) e)) ((pass-exp env) body))] 
      [(WhileLoop cnd body) 
        (WhileLoop ((pass-exp env) cnd) ((pass-exp env) body))]
      [_ ((super pass-exp env) exp)]
    ))
  )
)

(define uniquify (λ (p) (send (new pass-Lwhile-uniquify) pass p)))

(define pass-select-instructions-while
  (class pass-select-instructions-If
    (super-new)
    (inherit cast)
    (define/override (pass-instr stmt) (match stmt
      [(Assign lhs (Prim '+ (or (list lhs e) (list e lhs))))
        (list (Instr 'addq (list (cast e) lhs)))]
      [_ (super pass-instr stmt)]
    ))
  ))

(define select-instructions (λ (p) (send (new pass-select-instructions-while) pass p)))

(define (patch-instructions p) (send (new pass-patch-instructions) pass p))

(define pass-expose-allocation
  (class pass-abstract
    (super-new)
    (define/override (pass p) (match p [(Program info body)
      (Program info (pass-body body))
    ]))
    (define/public (expand-env-r body env) (match env
      [(cons (cons x v) rest) (Let x v (expand-env-r body rest))]
      ['() body]
    ))
    (define/public (pass-body body) (match body 
      [(HasType (Prim 'vector es) types)
        (define-values (es^ env^) (for/fold ([es^ '()] [env^ '()]) ([e es])
          (define tmp (gensym 'tmp))
          (values (cons tmp es^) (dict-set env^ tmp (pass-body e)))
          ))
        (define bytes (* (+ 1 (length es)) 8))
        (define pre-collect (λ (b) (Let '_ (If (Prim '< (GlobalValue 'free_ptr) bytes) (Void) (Collect bytes)) b)))
        (define v (gensym 'tmp))
        (define inner (for/foldr ([b (Var v)]) ([e es^] [idx (in-range (length es))])
          (Let '_ (Prim 'vector-set! (list (Var v) idx (Var e))))
          ))
        (set! inner (Let v (Allocate (length es) types)))
        (set! inner (pre-collect inner))
        (set! inner (expand-env-r inner env^))
        inner
      ]
      [(Prim o es) (Prim o (for/list ([e es]) (pass-body e)))]
      [(If cnd thn els) (If (pass-body cnd) (pass-body thn) (pass-body els))]
      [(Begin es e) (Begin (for/list ([e es]) (pass-body e)) (pass-body e))]
      [(Let x rhs body) (Let x (pass-body rhs) (pass-body body))]
      [(SetBang var rhs) (SetBang var (pass-body rhs))]
      [(WhileLoop cnd body) (WhileLoop (pass-body cnd) (pass-body body))]
      [_ body]
    ))
  ))

(define expose-allocation (λ (p) (send (new pass-expose-allocation) pass p)))

(define pass-Lvec-rco
  (class pass-Lwhile-rco
    (super-new)
    (define/override (pass-exp p) (match p 
      [(Collect (Int _)) p]
      [(GlobalValue _) p]  
      [(Allocate (Int _) _) p]
      [_ (super pass-exp p)]
    ))
  ))

(define remove-complex-opera* (λ (p) (send (new pass-Lvec-rco) pass p)))

(define pass-Lvec-explicate-control
  (class pass-Lwhile-explicate-control
    (super-new)
    (inherit create-block-2)
    (define/override ((explicate-effect env) p cont) (match p
      [(Collect _) (values env (Seq p cont))]
      [(Allocate _ _) (values env (Seq (Assign (Var '_) p) cont))]
      [(GlobalValue _) (values env cont)]
      [(Prim 'vector-ref (list _ (Int _))) cont]
      [(Prim 'vector-set! (list _ (Int _) _)) (Seq p cont)]
      [(Prim 'vector-length (list _)) cont]
      [_ ((super explicate-effect env) p cont)]
    ))
    (define/override ((explicate-tail env) p) (match p
      [(Collect _) (values env (Return (Void)))]
      [(Allocate _ _) (values env (Return (Void)))]
      [(GlobalValue _) (values env (Return (Void)))]
      [(Prim 'vector-ref (list _ (Int _))) (Return p)]
      [(Prim 'vector-set! (list _ (Int _) _)) (Seq p (Return (Void)))]
      [(Prim 'vector-length (list _)) (Return p)]
      [_ ((super explicate-tail env) p)]
    ))
    (define/override ((explicate-pred env) cnd thn els) (match cnd
      [(Prim 'vector-ref (list _ (Int _))) 
        (define tmp (gensym 'tmp))
        (define-values (env^ thn^ els^) ((create-block-2 env) thn els))
        (values env^ 
          (Seq (Assign (Var tmp) cnd) (IfStmt (Prim 'eq (Var tmp) (Bool #t)) thn^ els^))
        )
      ]
      [_ ((super explicate-pred env) cnd thn els)]
    ))
    (define/override ((explicate-assign env) p x cont) (match p
      [(or (Prim 'vector-set! _) (Collect _)) (values env ((explicate-effect env) p cont))]
      [(Allocate (Int _) _) (values env (Seq (Assign (Var x) p) cont))]
      [(Prim 'vector-ref (Int _)) (values env (Seq (Assign (Var x) p) cont))]
      [(Prim 'vector-length _) (values env (Seq (Assign (Var x) p) cont))]
      [(GlobalValue _) (values env (Seq (Assign (Var x) p) cont))]
      [_ ((super explicate-assign env) p x cont)]
    ))
  ))

(define explicate-control (λ (p) (send (new pass-Lvec-explicate-control) pass p)))

; (debug-level 2)
(define compiler-passes
  `(
    ("shrink" ,shrink ,interp-Lvec ,type-check-Lvec)
    ("uniquify" ,uniquify ,interp-Lvec ,type-check-Lvec)
    ("collect-set!" ,collect-set! ,interp-Lvec ,type-check-Lvec)
    ("uncover-get!-exp" ,uncover-get!-exp ,interp-Lvec ,type-check-Lvec-has-type)
    ("expose-allocation" ,expose-allocation ,interp-Lvec ,type-check-Lvec)
    ("remove complex opera*" ,remove-complex-opera* ,interp-Lvec ,type-check-Lvec)
    ("explicate control" ,explicate-control ,interp-Cwhile ,type-check-Cwhile)
    ("instruction selection" ,select-instructions ,interp-pseudo-x86-1)
    ("uncover live" ,uncover-live ,interp-pseudo-x86-1)
    ("build interference graph" ,build-interference ,interp-pseudo-x86-1)
    ("build color graph" ,color-graph ,interp-pseudo-x86-1)
    ("allocate registers" ,allocate-registers ,interp-x86-1)
    ("patch instructions" ,patch-instructions ,interp-x86-1)
    ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-1)
    ("patch instructions" ,patch-instructions ,interp-x86-1)
  ))
