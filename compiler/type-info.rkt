#lang racket

(require "../utilities.rkt")
(require cutie-ftree)

(define type-info
  (class object%
    (super-new)
    (define/public pass (match-lambda 
      [(Program info exp)
        (error 'pass) 
      ]
    ))
    (field [prim-table (ordl-make-empty symbol-compare)])
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
