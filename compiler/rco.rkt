#lang racket

(require "../utilities.rkt")
(require "program-default.rkt")
(require cutie-ftree)

(define pass-rco
  (class pass-program
    (super-new)
    (field [rco-env (ral-empty)])
    ; (define/override (pass p) (define p^ (super pass p)) (displayln p) (displayln p^) (displayln "\n\n") p^)
    (define/override (pass-exp p)
      (set! rco-env (ral-consr rco-env (box (ral-empty))))
      (define rst (expand-lets
        (match p
          [(Prim op es) 
            (Prim op (for/list ([e es]) (pass-atom e)))]
          [e (super pass-exp e)]
        )))
      (match-define-values (x rco-env^) (ral-dropr rco-env))
      (unless (box? x) (assert-unreachable))
      (set! rco-env rco-env^)
      rst
    )
    (define (pass-atom p)
      (define p^ (pass-exp p))
      (define rco-env0 (ral-viewr rco-env))
      (cond
        [(atom? p^) p^]
        ; [else (define tmp (gensym 'tmp)) (set-box! rco-env0 (ral-consl (unbox rco-env0) (cons tmp p^))) (Var tmp)]
        ; must use consr but not consl
        [else (define tmp (gensym 'tmp)) (set-box! rco-env0 (ral-consr (unbox rco-env0) (cons tmp p^))) (Var tmp)]
      )
    )
    (define atom? (match-lambda 
      [(or (Bool _) (Int _) (Var _) (Void )) #t]
      [_ #f]
    ))
    (define (expand-lets p)
      (define tmp-env (unbox (ral-viewr rco-env)))
      (for/fold ([init p]) ([e (in-ral0 tmp-env)]) 
        (match-define (cons var rhs) e)
        (Let var rhs init)
      ))
    )
  )

(provide pass-rco)
