#lang racket/base

(require racket/class racket/match)
(require racket/fixnum)
(require racket/dict)
(require "../core/core-types.rkt")

(require cutie-ftree)

(define interp-L-class (class object% (super-new)

  (field [env (make-parameter #f)])

  (define (read-fixnum)
    (define r (read))
    (cond
      [(fixnum? r) r]
      [else (error 'interp-exp "expected: an integer, actual: ~a" r)]
    )
  )

  (define/private (assert-boolean! v)
    (cond
      [(boolean? v) v]
      [else (error 'interp-exp "expect boolean, actual: ~a" v)]
    )
  )

  (define/private (interp-cnd e)
    (assert-boolean! (interp-exp e)))

  (define/public (interp-exp e) (match e
    [(Var x) (unbox (dict-ref (env) x))]
    [(Var:r x) (unbox (dict-ref (env) x))]
    [(Let x e body)
      (define env^ (dict-set (env) x (box (interp-exp e))))
      (parameterize ([env env^]) (interp-exp body))
    ]
    [(Int n) n]
    [(Prim 'read '()) (read-fixnum)]
    [(Prim '- `(,e))
      (define v (interp-exp e))
      (fx- 0 v)]
    [(Prim '+ `(,e1 ,e2))
      (define v1 (interp-exp e1))
      (define v2 (interp-exp e2))
      (fx+ v1 v2)]
    [(Prim '- `(,e1 ,e2))
      (define v1 (interp-exp e1))
      (define v2 (interp-exp e2))
      (fx- v1 v2)]
    [(Bool b) b]
    [(If cnd thn els)
      (match (interp-cnd cnd)
        [#t (interp-exp thn)]
        [#f (interp-exp els)]
      )] 
    [(Prim 'and `(,e1 ,e2))
      (and (interp-cnd e1) (interp-cnd e2))
    ]
    [(Prim 'or `(,e1 ,e2))
      (or (interp-cnd e1) (interp-cnd e2))
    ]
    [(Prim op args)
      (apply (interp-op op) (for/list ([arg args]) (interp-exp arg)))
    ]
    [(GetBang x) (unbox (dict-ref (env) x))]
    [(SetBang x rhs) (set-box! (dict-ref env x) (interp-exp rhs))]
    [(WhileLoop cnd body)
      (let loop () 
        (cond
          [(interp-cnd cnd) (interp-exp body) (loop)]
          [else (void)]
        ))]
    [(Begin es body)
      (for ([e es]) (interp-exp e))
      (interp-exp body)
    ]
    [(Void) (void)]
  ))

  (define op-not (match-lambda [#t #f] [#f #t]))

  (define op-eq? (match-lambda** 
    [((and (? fixnum?) v1) (and (? fixnum?) v2)) (eq? v1 v2)]
    [((and (? boolean?) v1) (and (? boolean?) v2)) (eq? v1 v2)]
    [((and (? vector?) v1) (and (? vector?) v2)) (eq? v1 v2)]
  ))

  (define op-< (match-lambda** [((and (? fixnum?) v1) (and (? fixnum?) v2)) (< v1 v2)]))
  (define op-<= (match-lambda** [((and (? fixnum?) v1) (and (? fixnum?) v2)) (<= v1 v2)]))
  (define op-> (match-lambda** [((and (? fixnum?) v1) (and (? fixnum?) v2)) (> v1 v2)]))
  (define op->= (match-lambda** [((and (? fixnum?) v1) (and (? fixnum?) v2)) (>= v1 v2)]))

  (define/public (interp-op op) (match op
    ['+ fx+]
    ['- fx-]
    ['read read-fixnum]
    ['not op-not]
    ['eq? op-eq?]
    ['< op-<]
    ['<= op-<=]
    ['> op->]
    ['>= op->=]
    [_ (error 'interp-op "unknown op: ~a" op)]
  ))

  (define/public interp (match-lambda [(Program _ e) (interp-exp e)]))

))
