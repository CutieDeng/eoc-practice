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

(define (uniquify-var env)
  (λ (e)
    (cond
      [(dict-has-key? env e) (values env (dict-ref env e))]
      [else 
        (define e-2 (gensym e))
        (define env-2 (dict-set env e e-2))
        (values env-2 e-2)])
    ))

(define (uniquify-exp env)
  (lambda (e)
    (match e
      [(Var x) 
        (define-values (env-2 x-2) ((uniquify-var env) x))
        (values env-2 (Var x-2))]
      [(Int n) (values env e)]
      [(Let x e body)
        (define-values (new-env new-x) ((uniquify-var env) x))
        (define-values (new-env-2 new-e) ((uniquify-exp new-env) e))
        (define-values (new-env-3 new-body) ((uniquify-exp new-env-2) body))
        (values new-env-3 
          (Let new-x new-e new-body))]
      [(Prim op es)
        (define-values (new-env new-es)
          (for/fold ([nenv env] [nes '()]) ([e es])
            (define-values (nenv-2 ne) ((uniquify-exp nenv) e))
            (values nenv-2 (cons ne nes))))
        (values new-env (Prim op (reverse new-es)))]
    )))

;; uniquify : Lvar -> Lvar
(define (uniquify p)
  (match p
    [(Program info e) 
      (define-values (_ e2) ((uniquify-exp '()) e))
      (Program info e2)]))

(define ((exp-cast-atom env) p)
  (match p
    [(Int _) (values env p)]
    [(Var _) (values env p)]
    [_ (let ([x (gensym 'tmp)])
      (define e-2 (dict-set env x p))
      (values e-2 (Var x)))]))
    
(define ((expand-lets env) p)
  (match env
    ['() p]
    [(cons (cons x e) rest)
      ((expand-lets rest) (Let x e p))]))

(define ((rco-atom env) p)
  (define p-2 (rco-exp p))
  (define-values (env-2 p-3) ((exp-cast-atom env) p-2))
  (values env-2 p-3))

(define (rco-exp p)
  (match p
    [(Var _) p]
    [(Int _) p]
    [(Prim op es)
      (define-values (env-2 e-2) 
        (for/fold ([envo '()] [new-es '()]) ([e es])
          (define-values (env-2 e-2) ((rco-atom envo) e))
          (values env-2 (cons e-2 new-es))))
      (define e-3 ((expand-lets env-2) (Prim op (reverse e-2))))
      e-3]
    [(Let x e body)
      (define new-e (rco-exp e))
      (define new-body (rco-exp body))
      (Let x new-e new-body)]))

;; remove-complex-opera* : Lvar -> Lvar^mon
(define (remove-complex-opera* p)
  (match p
    [(Program info e)
      (define p-2 (rco-exp e))
      (Program info p-2)
    ]))

;; explicate-control : Lvar^mon -> Cvar
(define (explicate-control p)
  (match p
    [(Program info body) (CProgram info (list (cons 'start (explicate-tail body))))]))

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
      (X86Program info (list (cons 'start (Block '() block-2))))]))

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
        (define offset-2 (aligned (+ offset 4) 4))
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
    [e (list e)]))

(define (patch-instr-block b)
  (match b
    [(Block info ins)
      (Block info (append-map patch-instr ins))])) 

;; prelude-and-conclusion : x86int -> x86int
(define (prelude-and-conclusion p)
  (define prelude (Block '() (list )))
  (define conclusion (Block '() (list (Instr 'retq '()))))
  (match p
    [(X86Program info b)
      (X86Program info (cons (cons 'prelude prelude) (cons (cons 'conclusion conclusion) b)))]))

(debug-level 2)
;; Define the compiler passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; must be named "compiler.rkt"
(define compiler-passes
  `(
     ;; Uncomment the following passes as you finish them.
     ("uniquify" ,uniquify ,interp-Lvar ,type-check-Lvar)
     ("remove complex opera*" ,remove-complex-opera* ,interp-Lvar ,type-check-Lvar)
     ("explicate control" ,explicate-control ,interp-Cvar ,type-check-Cvar)
     ("instruction selection" ,select-instructions ,interp-pseudo-x86-0)
     ("assign homes" ,assign-homes ,interp-x86-0)
     ("patch instructions" ,patch-instructions ,interp-x86-0)
     ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-0)
     ))
