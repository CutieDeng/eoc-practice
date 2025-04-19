#lang racket

(require "../utilities.rkt")
(require cutie-ftree)

(define pass-explicate-control
  (class object%
    (super-new)
    (field [control-flow (ordl-make-empty symbol-compare)])
    (define/public pass (match-lambda 
      [(Program info body)
        (define b (explicate-tail body))
        (set! control-flow (dict-set control-flow 'start b))
        (CProgram info control-flow)
      ]
    ))
    (define (explicate-tail p) (match p
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
      [(GetBang x) (Return (Var x))]
      [(or (Prim _ _) (Int _) (Bool _) (Void ) (Var _)) (Return p)]
    ))
    (define (explicate-assign p x cont) (match p
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
      [(GetBang v)
        (Seq (Assign (Var x) (Var v)) cont)]
      [(or (Prim _ _) (Int _) (Bool _) (Void ) (Var _)) (Seq (Assign (Var x) p) cont)]
    ))
    (define (explicate-pred cnd thn els) 
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
        [(GetBang v)
          (IfStmt (Prim 'eq? (list (Var v) (Bool #t)) thn-blk els-blk))
        ]
      ))
    (define (explicate-effect p cont) (match p
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
        (set! control-flow
          (dict-set 
            (dict-set control-flow body-lbl body^)
            cnd-lbl cnd^)
        )
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
      [(GetBang x) (Assign (Var (gensym 'utmp)) (Var x))]
      [(or (Var _) (Int _) (Bool _) (Void )) cont]
    ))
    (define create-block (match-lambda 
      [(and raw-jmp (Goto _)) raw-jmp]
      [other (define lbl (gensym 'label)) 
        (set! control-flow (dict-set control-flow lbl other))
        (Goto lbl)]
    ))
  )
)

(provide pass-explicate-control)

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
