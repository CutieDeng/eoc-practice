#lang racket

(require "core/core-types.rkt")
(require "control-flow-graph/var-id-reassign.rkt")
(require cutie-ftree)

(define (ral-single stmt)
  (ral-consl (ral-empty) stmt)
)
(define (return-single-bb p)
  (ral-single (Return p))
)

(define pass-explicate-control
  (class (pass-var-id-reassign-mixin object% #f)
    (super-new)
    (define (bind-bb-id id bb)
      (set! control-flow-graph (dict-set control-flow-graph id bb))
    )
    (define create-block (match-lambda
      [(and raw-jmp (ral ((Goto _) atom))) raw-jmp]
      [other (define lbl (gen-bb-id))
        (bind-bb-id lbl other)
        (ral-single (Goto lbl))]
      ))
    (define (gen-bb-id)
      (begin0 bb-cnt (set! bb-cnt (+ bb-cnt 1))))
    (inherit-field var-cnt)
    (inherit gen-var-id)
    (field [control-flow-graph (ordl-make-empty integer-compare)] [bb-cnt 3])
    (define/public pass (match-lambda 
      [(Program info body)
        (set! var-cnt (dict-ref info 'var-cnt))
        (define b (explicate-tail body))
        (bind-bb-id 2 b)
        (define info^ (dict-set* info 'bb-cnt bb-cnt 'var-cnt var-cnt))
        (CProgram info^ control-flow-graph)
      ]
    ))
    (define (explicate-tail p) (match p
      [(Collect _) (return-single-bb p)]
      [(Allocate (? integer?) _) (return-single-bb p)]
      [(GlobalValue _) (return-single-bb p)]
      [(Prim 'vector-ref (list _ (Int _))) (return-single-bb p)]
      [(Prim 'vector-set! (list _ (Int _) _)) (return-single-bb p)]
      [(Prim 'vector-length (list _)) (return-single-bb p)]
      [(Begin es body) 
        (for/foldr ([init-cont (explicate-tail body)]) ([e (in-ral0 es)])
          (explicate-effect e init-cont)
        )]
      [(or (WhileLoop _ _) (SetBang _ _))
        (explicate-effect p (return-single-bb (Void )))]
      [(If cnd thn els) (explicate-pred cnd (explicate-tail thn) (explicate-tail els))]
      [(Let x e body) (explicate-assign e x (explicate-tail body))]
      [(GetBang x) (return-single-bb (Var x))]
      [(or (Prim _ _) (Int _) (Bool _) (Void ) (Var _)) (return-single-bb p)]
    ))
    (define (explicate-assign p x cont) (match p
      [(or (Collect _) (Prim 'vector-set! _))
        (ral-consl cont p)]
      [(or (Allocate (? integer?) _)
          (Prim 'vector-ref (list (Int _) _)) (Prim 'vector-length _)
          (GlobalValue _))
        (ral-consl cont (Assign (Var x) p))
      ]
      [(Begin es body)
        (for/foldr ([init-cont (explicate-assign body x cont)]) ([e (in-ral0 es)])
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
      [(GetBang v)
        (ral-consl cont (Assign (Var x) (Var v)))]
      [(or (Prim _ _) (Int _) (Bool _) (Void ) (Var _)) (ral-consl cont (Assign (Var x) p))]
    ))
    (define (explicate-pred cnd thn els)
      (define thn-blk (create-block thn))
      (define els-blk (create-block els))
      (match cnd
        [(Prim 'vector-ref (list _ (Int _)))
          (define tmp (Var (gen-var-id)))
          (ral-consl (ral-single (IfStmt (Prim 'eq? tmp (Bool #t)) thn-blk els-blk)) (Assign tmp cnd))
        ]
        [(Begin es body)
          (define cont (explicate-pred body thn els))
          (for/foldr ([cont cont]) ([e (in-ral0 es)])
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
          (ral-single (IfStmt (Prim 'eq? (list cnd (Bool #t))) thn-blk els-blk))
        ]
        [(Prim 'not (list _))
          (ral-single (IfStmt (Prim 'eq? (list cnd (Bool #f))) thn-blk els-blk))
        ]
        [(Prim (or 'eq? '< '>) (list _ _))
          (ral-single (IfStmt cnd thn-blk els-blk))
        ]
        [(GetBang v)
          (ral-single (IfStmt (Prim 'eq? (list (Var v) (Bool #t)) thn-blk els-blk)))
        ]
      ))
    (define (explicate-effect p cont) (match p
      [(Collect _) (ral-consl cont p)]
      [(or (Allocate (? integer?) _) (GlobalValue _)) cont]
      [(Prim 'vector-ref (list _ (Int _))) cont]
      [(Prim 'vector-set! (list _ (Int _) _)) (ral-consl cont p)]
      [(Prim 'vector-length (list _)) cont]
      [(SetBang var rhs) (explicate-assign rhs var cont)]
      [(WhileLoop cnd body)
        (define body-lbl (gen-bb-id))
        (define cnd-lbl (gen-bb-id))
        (define cnd^ (explicate-pred cnd (Goto body-lbl) cont))
        (define body^ (explicate-effect body (Goto cnd-lbl))) 
        (bind-bb-id body-lbl body^)
        (bind-bb-id cnd-lbl cnd^)
        (ral-single (Goto cnd-lbl))
      ]
      [(Begin es body)
        (for/foldr ([init-body (explicate-effect body cont)]) ([e (in-ral0 es)])
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
        (for/foldr ([init-body cont]) ([arg (in-ral0 args)])
          (explicate-effect arg init-body) 
        )]
      [(GetBang x) (ral-consl cont (Assign (Var (gen-var-id)) (Var x)))]
      [(or (Var _) (Int _) (Bool _) (Void )) cont]
    ))
  )
)

(provide pass-explicate-control)
