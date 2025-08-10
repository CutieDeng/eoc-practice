#lang racket/base

(require racket/class)

(define (pass-var-id-reassign-mixin clazz var-id-start)
  (class clazz
    (super-new)
    (field [var-cnt var-id-start])
    (define/public (gen-var-id)
      (begin0 var-cnt (set! var-cnt (+ var-cnt 1)))
    )
  )
)

(provide pass-var-id-reassign-mixin)
