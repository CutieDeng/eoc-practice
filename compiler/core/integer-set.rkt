#lang racket/base

(require racket/sequence)

(define bset-union bitwise-ior)
(define bset-and bitwise-and)

(define bset-member? bitwise-bit-set?)

(define (bset-add s m)
  (bitwise-ior s (arithmetic-shift 1 m))
)

(define (bset-remove s m)
  (bitwise-and s (bitwise-not (arithmetic-shift 1 m)))
)

; s != 0
(define (integer-leftmost s)
  (- (integer-length s) 1)
)

(define (in-bset/rev s)
  (make-do-sequence (lambda () (initiate-sequence
    #:init-pos s
    #:pos->element integer-leftmost
    #:continue-with-pos? (compose not zero?)
    #:next-pos (lambda (s) (bitwise-xor s (arithmetic-shift 1 (integer-leftmost s))))
  )))
)

(define (integer-rightmost s)
  (- (integer-length (bitwise-and (- s) s)) 1)
)

(define (in-bset s)
  (make-do-sequence (lambda () (initiate-sequence
    #:init-pos s
    #:pos->element integer-rightmost
    #:continue-with-pos? (compose not zero?)
    #:next-pos (lambda (s) (bitwise-xor s (arithmetic-shift 1 (integer-rightmost s))))
  )))
)

(define (bset-subtract s s2)
  (bitwise-and s (bitwise-not s2))
)

(define (seq->bset s)
  (for/fold ([s^ 0]) ([i s])
    (bset-add s^ i))
)

(define (bset->list s)
  (for/list ([i (in-bset s)]) i)
)

(define (bset->vector s)
  (list->vector (bset->list s))
)

(define (bset . b*)
  (bset* b*)
)

(define (bset* ls)
  (for/fold ([s 0]) ([i ls]) (bset-add s i))
)

(provide integer-leftmost integer-rightmost)
(provide bset-add bset-remove bset-member? bset-union bset-subtract)
(provide in-bset in-bset/rev)
(provide seq->bset bset->list bset->vector)
(provide bset bset*)
(provide bset-and)
