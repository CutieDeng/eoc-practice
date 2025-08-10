#lang racket
(require racket/fixnum)
(require racket/dict)
(require "compiler/core/core-types.rkt")
(require "interp-Lint.rkt")
(provide interp-Lvar-class)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define interp-Lvar-class
  (class interp-Lint-class
    (super-new)
    
    (define/override ((interp-exp env) e)
      (match e
        [(Var x) (dict-ref env x)]
        [(Let x e body)
         (define new-env (dict-set env x ((interp-exp env) e)))
         ((interp-exp new-env) body)]
        [_ ((super interp-exp env) e)]
        ))

    ))

(define (interp-Lvar p)
  (send (new interp-Lvar-class) interp-program p))

