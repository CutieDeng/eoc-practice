#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require "interp-Lint.rkt")
(require "interp-Lvar.rkt")
(require "interp-Cvar.rkt")
(require "interp.rkt")
(require "type-check-Lvar.rkt")
(require "type-check-Cvar.rkt")
(require "utilities.rkt")
(provide (all-defined-out))

(require "interp-Lif.rkt")
(require "type-check-Lif.rkt")

(require graph)
(require "graph-printing.rkt")
(require "priority_queue.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lint examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following compiler pass is just a silly one that doesn't change
;; anything important, but is nevertheless an example of a pass. It
;; flips the arguments of +. -Jeremy
(define (flip-exp e)
  (match e
    [(Var x) e]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (Prim '- (list (flip-exp e1)))]
    [(Prim '+ (list e1 e2)) (Prim '+ (list (flip-exp e2) (flip-exp e1)))]))

(define (flip-Lint e)
  (match e
    [(Program info e) (Program info (flip-exp e))]))


;; Next we have the partial evaluation pass described in the book.
(define (pe-neg r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [else (Prim '- (list r))]))

(define (pe-add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r2))]))

(define (pe-exp e)
  (match e
    [(Int n) (Int n)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (pe-neg (pe-exp e1))]
    [(Prim '+ (list e1 e2)) (pe-add (pe-exp e1) (pe-exp e2))]))

(define (pe-Lint p)
  (match p
    [(Program info e) (Program info (pe-exp e))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HW1 Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; explicate-control : Lvar^mon -> Cvar
(define (explicate-control p)
  (match p
    [(Program info body) (CProgram info (dict-set '() 'start (explicate-tail body)))]))

(define (explicate-tail p)
  (match p
    [(Int _) (Return p)]
    [(Var _) (Return p)]
    [(Prim _ _) (Return p)]
    [(Let x e body) (explicate-assign e x (explicate-tail body))]))

(define (explicate-assign p x cont)
  (match p
    [(Int _) (Seq (Assign (Var x) p) cont)]
    [(Var _) (Seq (Assign (Var x) p) cont)]
    [(Prim _ _) (Seq (Assign (Var x) p) cont)]
    [(Let y e body) (explicate-assign e y (explicate-assign body x cont))])) 

(define (select-instr p)
  (define (cast x)
    (match x
      [(Int n) (Imm n)]
      [_ x]))

  (match p
    [(Assign lhs rhs)
      (define p-2 (match rhs
        [(Var _) 
          (list (Instr 'movq (list rhs lhs)))]
        [(Int rhs-i)
          (list (Instr 'movq (list (Imm rhs-i) lhs)))]
        [(Prim '+ (list a b))
          (list (Instr 'movq (list (cast a) (Reg 'rax))) (Instr 'addq (list (cast b) (Reg 'rax))) (Instr 'movq (list (Reg 'rax) lhs)))]
        [(Prim '- (list a))
          (list (Instr 'movq (list (cast a) (Reg 'rax))) (Instr 'negq (list (Reg 'rax))) (Instr 'movq (list (Reg 'rax) lhs)))]
        [(Prim 'read '()) 
          (list (Callq 'read_int (list )) (Instr 'movq (list (Reg 'rax) lhs)))]
        ))
      p-2])
)

(define (select-instrs p)
  (match p
    [(Seq first rest)
      (define p-2 (select-instr first))
      (append p-2 (select-instrs rest))]
    [(Return arg)
      (select-instr (Assign (Reg 'rax) arg))]))

;; select-instructions : Cvar -> x86var
(define (select-instructions p)
  (match p
    [(CProgram info block)
      (define block-2 (select-instrs (dict-ref block 'start)))
      (X86Program info (dict-set '() 'start (Block '() block-2)))]))

;; assign-homes : x86var -> x86var
(define (assign-homes p)
  (match p
    [(X86Program info b)
      (define-values (top local-offsets) (local-types-to-offset (dict-ref info 'locals-types)))
      (define info-2 (dict-set info 'local-offsets local-offsets))
      (define info-3 (dict-set info-2 'stack-size (aligned top 16)))
      (define b-2 (for/list ([x b])
        (match x
          [(cons l block) (cons l (assign-homes-block block local-offsets))])))
      (X86Program info-3 b-2)]))

(define (assign-homes-instr ins local-offsets)
  (match ins
    [(Instr i l) 
      (Instr i (for/list ([x l])
        (match x
          [(Var r) (Deref 'rbp (- (dict-ref local-offsets r)))]
          [o o])))]))

(define (assign-homes-block block local-offsets)
  (match block
    [(Block info ins)
      (Block info (for/list ([i ins])
        (assign-homes-instr i local-offsets)))]))

(define (aligned x a) (cond
  [(zero? (modulo x a)) x]
  [else (+ x (- a (modulo x a)))]))

(define (local-types-to-offset types)
  (for/fold ([offset 0] [offsets '()]) ([ty types])
    (match ty
      [(cons x 'Integer)
        (define offset-2 (aligned (+ offset 8) 8))
        (values offset-2 (cons (cons x offset-2) offsets))])))
  
;; patch-instructions : x86var -> x86int
(define (patch-instructions p)
  (match p
    [(X86Program info b)
      (X86Program info (for/list ([x b])
        (match x
          [(cons l block) (cons l (patch-instr-block block))])))]))

(define (patch-instr i)
  (match i
    [(Instr 'movq (list (Deref r0 o0) (Deref r1 o1)))
      (list (Instr 'movq (list (Deref r0 o0) (Reg 'rax))) (Instr 'movq (list (Reg 'rax) (Deref r1 o1))))]
    [(Instr i (list (Deref r0 o0) (Deref r1 o1)))
      (list (Instr 'movq (list (Deref r1 o1) (Reg 'rax))) (Instr i (list (Deref r0 o0) (Reg 'rax))) (Instr 'movq (list (Reg 'rax) (Deref r1 o1))))]
    [(Instr 'movq (list (Reg a) (Reg b))) 
      (cond
        [(equal? a b) (list )] ; optimize the useless movq op.
        [else (list i)])]
    [(or (Instr 'addq (list (Imm 0) _)) (Instr 'subq (list (Imm 0) _))) 
      (list ) ; drop the non-sense addition and subtraction.
    ]
    [e (list e)]))

(define (patch-instr-block b)
  (match b
    [(Block info ins)
      (Block info (append-map patch-instr ins))])) 

;; prelude-and-conclusion : x86int -> x86int
(define (prelude-and-conclusion p)
  (match p
    [(X86Program info b)
      (define stack-size (aligned (dict-ref info 'stack-size) 16))
      (define prelude (Block '() 
        (list 
          (Instr 'pushq (list (Reg 'rbp))) 
          (Instr 'movq (list (Reg 'rsp) (Reg 'rbp))) 
          (Instr 'subq (list (Imm stack-size) (Reg 'rsp)))
          (Jmp 'start)
        )))
      (define conclusion (Block '() (list 
        (Instr 'addq (list (Imm stack-size) (Reg 'rsp)))
        (Instr 'popq (list (Reg 'rbp)))
        (Retq )
      )))
      (define b-start (dict-ref b 'start))
      (define b-start-block (Block-instr* b-start))
      (define b-start-2 (Block (Block-info b-start) (append b-start-block (list (Jmp 'conclusion)))))
      (set! b (dict-set b 'start b-start-2))
      (X86Program info (cons (cons 'main prelude) (cons (cons 'conclusion conclusion) b)))]))

(define caller-save-regs 
  (list 'rax 'rcx 'rdx 'rsi 'rdi 'r8 'r9 'r10 'r11)
)

(define callee-save-regs
  (list 'rsp 'rbp 'rbx 'r12 'r13 'r14 'r15)
)

(define pass-args-regs
  (list 'rdi 'rsi 'rdx 'rcx 'r8 'r9)
)

#;(define (uncover-live-blocks blocks)
  (define tags (sequence->list (in-dict-keys blocks)))
  (define blocks-2
    (for/fold ([blocks blocks]) ([tag tags])
      (define blocks-3 (uncover-live-block tag blocks))
      blocks-3
      )
  )
  blocks-2
)

#;(define (uncover-live-block tag blocks)
  (match (dict-ref blocks tag)
    [(Block i block)
      (cond
        [(dict-has-key? i 'live) blocks]
        [else 
          (define-values (b-2 blocks-2) (uncover-live-block-impl tag block blocks))
          (define rst-blocks (dict-set blocks-2 tag b-2))
          rst-blocks
        ])]))
  
#;(define (uncover-live-block-impl tag block-inner blocks)
  (cond
    [(null? block-inner) (values (list ) blocks)]
    [else
      (define i (car block-inner))
      (define r (cdr block-inner))
      (define-values (l blocks-2) (uncover-live-block-impl tag r blocks))
      (define filterimm (λ (x) (not (Imm? x))))
      (define biop (λ (l blocks src dst)
          (define old-set (car l))
          (define new-set (set-union old-set (set (list src dst))))
          (define new-set-2 (list->set (filter filterimm (set->list new-set))))
          (values (cons new-set-2 l) blocks)))
      (match i
        [(Instr 'movq (list src dst))
            (define old-set (car l))
            (define new-set (set-union (set-subtract old-set (set (list dst))) (set (list src))))
            (define new-set-2 (list->set (filter filterimm (set->list new-set))))
            (values (cons new-set-2 l) blocks-2)]
        [(Instr 'addq (list src dst))
          (biop l blocks-2 src dst)
        ]
        [(Instr 'subq (list src dst))
          (biop l blocks-2 src dst)
        ]
        [(Jmp tag)
          (define blocks-2 (uncover-live-block tag blocks))
          (define l (car (dict-ref (dict-ref blocks-2 tag) 'live)))
          (values (list l) blocks-2)
        ]
      )]
    ))

(struct PendingError ())

(define (inv-trans-uncover-live-set instr s blocks)
  (match instr
    [(Instr 'movq (list a b)) (set-union (set-subtract s (set b)) (set a))]
    [(or (Instr 'addq (list a b)) (Instr 'subq (list a b))) (set-union s (set a b))]
    [(Jmp tag) 
      (define jmp-top (dict-ref blocks tag))
      (unless (dict-has-key? (Block-info jmp-top) 'live) (raise (PendingError)))
      (car (dict-ref (Block-info jmp-top) 'live))
      ]
    [(Instr 'negq (list a)) (set-union s (set a))]
    [(Callq _ count)
      (define s-2 (set-subtract s (set (map Reg callee-save-regs))))
      (define s-3 (set-union s-2 (set (map Reg (take pass-args-regs count)))))
      s-3
      ]
    ))

(define (inv-trans-uncover-live-set-wrap . args)
  (define rst (apply inv-trans-uncover-live-set args))
  (define rst-2 (list->set (filter (compose not Imm?) (set->list rst))))
  rst-2
  )

(define (uncover-live-instr* instr* blocks)
  (match instr*
    ['() (list (set ))]
    [(cons instr rest)
      (define rest-uncover (uncover-live-instr* rest blocks))
      (define uncover (car rest-uncover))
      (define uncover-2 (inv-trans-uncover-live-set-wrap instr uncover blocks))
      (cons uncover-2 rest-uncover)
      ]
    ))

(define (uncover-live-block block blocks)
  (match block
    [(Block info instr*)
      (uncover-live-instr* instr* blocks)
      ]))

(define (uncover-live-blocks blocks)
  (define-values (fail blocks-3)
    (for/fold ([fail? #f] [blocks blocks]) ([tag (in-dict-keys blocks)])
      (define block (dict-ref blocks tag))
      (with-handlers ([PendingError? (λ (_) (values #t blocks))])
        (define rst (uncover-live-block block blocks))
        (define block-2 (Block (dict-set (Block-info block) 'live rst) (Block-instr* block)))
        (values fail? (dict-set blocks tag block-2))
        )
      )
    )
  (if fail (uncover-live-blocks blocks-3) blocks-3)
  )

(define (uncover-live p)
  (match p
    [(X86Program info bs)
      (define new-blocks (uncover-live-blocks bs))
      (X86Program info new-blocks)
      ]))

(define (all-writes-in-instr instr)
  (match instr
    [(Instr 'movq (list _ dst)) (set dst)]
    [(or (Instr 'addq (list _ dst)) (Instr 'subq (list _ dst))) (set dst)]
    [(Callq _ _) (list->set (map Reg caller-save-regs))]
    [(Instr 'negq dst) (set dst)]
    [(Jmp _) (set )]
    ))

(define (pre-build-interference b graph)
  (match b
    [(cons c ons)
      (for ([w (in-set (all-writes-in-instr c))])
        (add-vertex! graph w))
      (pre-build-interference ons graph)
    ]
    ['() graph]
  ))

(define (build-interference-instr* b s graph)
  (define carb (car b))
  (define cars (car s))
  (define writes (all-writes-in-instr carb))
  (for ([w (in-set writes)])
    (for ([r (in-set cars)])
      (unless (equal? w r) (add-edge! graph w r))
      )
    )
  (cond
    [(null? (cdr b)) graph]
    [else (build-interference-instr* (cdr b) (cdr s) graph)])
)

(define (build-interference-block b)
  (match b
    [(Block info instr*)
      (define g (build-interference-instr* instr* (dict-ref info 'live) 
        (pre-build-interference instr* (undirected-graph '()))
        ))
      (define info-2 (dict-set info 'interference g))
      (Block info-2 instr*)
      ]))

(define (build-interference p)
  (match p
    [(X86Program info blocks) 
      (define blocks-2 (for/list ([b blocks])
        (define bg (build-interference-block (cdr b)))
        (cons (car b) bg)))
      (X86Program info blocks-2)
      ]))

(define (color-graph p)
  (match p
    [(X86Program info blocks) 
      (define blocks-2 (map color-graph-block blocks))
      (X86Program info blocks-2)
    ]))

(define (color-graph-block block)
  (match block
    [(cons tag (Block info instr*))
      (define inte (dict-ref info 'interference))
      (define q (make-pqueue (λ (a b) (> (cdr a) (cdr b)))))
      (for ([n (in-vertices inte)])
        (pqueue-push! q (cons n 0))
        )
      (define neighbors-set 
        (with-handlers ([exn:fail? (λ (_e) '())])
          (for/fold ([nei-set '()]) ([n (in-neighbors inte (Reg 'rax))])
            (define inner (dict-ref nei-set n '()))
            (define inner-2 (set-add inner 0))
            (pqueue-push! q (cons n (length inner-2)))
            (dict-set nei-set n inner-2)
          ))
      )
      (define color-g (let color-find ([neighbors-set neighbors-set] [selections (dict-set '() (Reg 'rax) 0)] #;[remain (set-subtract (sequence->list (in-vertices inte)) (set (Reg 'rax)))])
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
                (define neighbors-set-2 (for/fold ([n neighbors-set]) ([v (in-neighbors inte (car pop))])
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
      (define info-2 (dict-set info 'color-graph color-g))
      (cons tag (Block info-2 instr*))
    ]))

(define ((allocate-register m) v)
  (match v
    [(Imm _) v]
    [_ 
      (define r (dict-ref m v))
      (cond
        [(< r (length caller-save-regs)) (Reg (list-ref caller-save-regs r))]
        [else (Deref 'rbp (- (* 8 (- r (length caller-save-regs))) 8))]
      )
    ]))

(define ((allocate-instr m) instr)
  (match instr
    [(Instr i l) (Instr i (map (allocate-register m) l))]
    [(Jmp _) instr]
    [(Callq _ _) instr]
  ))

(define (allocate-registers-block block)
  (match block
    [(Block info instr*)
      (define color-graph (dict-ref info 'color-graph))
      (define count (foldl max -1 (map cdr color-graph)))
      (when (= count -1) (error 'allocate-registers-block "no color graph"))
      (define stack-size (* (- (+ count 1) (length caller-save-regs)) 8))
      (define info-2 (dict-set info 'stack-size stack-size))
      (define instr*-2 (map (allocate-instr color-graph) instr*))
      (Block info-2 instr*-2)
    ]))

(define (allocate-registers p)
  (match p
    [(X86Program info blocks)
      (define blocks-2 (for/list ([b blocks])
        (cons (car b) (allocate-registers-block (cdr b)))))
      (define stack-size (dict-ref (Block-info (cdar blocks-2)) 'stack-size))
      (define info-2 (dict-set info 'stack-size (max stack-size 0)))
      (X86Program info-2 blocks-2)
    ]))

(define (shrink-exp exp)
  (match exp
    [(Prim 'and (list a b)) (If a b (Bool #f))]
    [(Prim 'or (list a b)) (If a (Bool #t) b)]
    ; [(Prim '- (list a b)) (Prim '+ (list a (Prim '- (list b))))]
    [_ exp]
    ))

(define (shrink p)
  (match p
    [(Program info exp)
      (Program info (shrink-exp exp))
    ]))

(define pass-abstract
  (class object%
    (super-new)
    (abstract pass)
    ))

(define pass-Lvar-uniquify
  (class pass-abstract
    (super-new)
    (define/override (pass p)
      (match p
        [(Program info body)
          (define body-2 ((pass-exp '()) body))
          (Program info body-2)
        ]))
    (define/public ((pass-exp env) exp)
      (match exp
        [(Var x) 
          (define x-2 (dict-ref env x))
          (Var x-2)]
        [(Int _) exp]
        [(Let x exp body)
          (define new-exp ((pass-exp env) exp))
          (define new-x (gensym x))
          (define new-env (dict-set env x new-x))
          (define new-body ((pass-exp new-env) body))
          (Let new-x new-exp new-body)]
        [(Prim op es)
          (define new-es
            (for/fold ([nes '()]) ([e es])
              (define nenv-2 ((pass-exp env) e))
              (cons nenv-2 nes)))
          (Prim op (reverse new-es))]
      )
    )
  )
)

(define uniquify (λ (p) (send (new pass-Lif-uniquify) pass p)))

(define pass-Lif-uniquify
  (class pass-Lvar-uniquify
    (super-new)
    (define/override ((pass-exp env) exp)
      (match exp
        [(Bool _) exp]
        [(If e1 e2 e3)
          ; (define-values (new-env new-))]
          (error )
        ]
        [_ ((super pass-exp env) exp)]
      )
    )
  ))

(define pass-Lvar-rco
  (class pass-abstract
    (super-new)
    (define/override (pass p)
      (match p
        [(Program info body) (Program info (pass-exp body))]))
    (define/public ((exp-cast-atom env) p)
      (define origin (λ () (values env p)))
      (match p
        [(or (Int _) (Var _)) (origin)]
        [_ (let ([x (gensym 'tmp)])
          (values (dict-set env x p) (Var x)))]
        )
    )
    (define/public ((expand-lets env) p)
      (match env
        ['() p]
        [(cons (cons x e) rest)
          ((expand-lets rest) (Let x e p))])
    )
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
    (define/override ((exp-cast-atom env) p)
      (match p
        [(Bool _) p]
        [_ ((super exp-cast-atom env) p)])
    )
    (define/override (pass-exp p)
      (match p
        [(If cnd thn els)
          (If (pass-exp cnd) (pass-exp thn) (pass-exp els))]
        [_ (super pass-exp p)]
      ))
  ))

(define remove-complex-opera* (λ (p) (send (new pass-Lif-rco) pass p)))

; (debug-level 2)
;; Define the compiler passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; must be named "compiler.rkt"
(define compiler-passes
  `(

     ("shrink" ,shrink ,interp-Lif ,type-check-Lif)

     ;; Uncomment the following passes as you finish them.
     ("uniquify" ,uniquify ,interp-Lif ,type-check-Lif)

     ("remove complex opera*" ,remove-complex-opera* ,interp-Lif ,type-check-Lif)

     ("explicate control" ,explicate-control ,interp-Cvar ,type-check-Cvar)
     
     ("instruction selection" ,select-instructions ,interp-pseudo-x86-0)

     ("uncover live" ,uncover-live ,interp-pseudo-x86-0)
     ("build interference graph" ,build-interference ,interp-pseudo-x86-0)
     ("build color graph" ,color-graph ,interp-pseudo-x86-0)
     ; ("assign homes" ,assign-homes ,interp-x86-0)
     ("allocate registers" ,allocate-registers ,interp-x86-0)

     ("patch instructions" ,patch-instructions ,interp-x86-0)
     ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-0)

     ("patch instructions" ,patch-instructions ,interp-x86-0)
     ))
