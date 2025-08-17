#lang racket

(require "core/core-types.rkt")
(require "core/integer-set.rkt")
(require "graph-core.rkt")
(require "core/utilities.rkt")
(require cutie-ftree)

(require "x86abi.rkt")

(define (min-reg-id-from-mask mask)
  (define maska1 (+ mask 1))
  (define v (bitwise-and maska1 (bitwise-not mask)))
  (- (integer-length v) 1)
)

(define (bset-length b)
  (sequence-length (in-bset b))
)

(define (cons-integer-compare lhs rhs)
  (match-define (cons lhs0 lhs1) lhs)
  (match-define (cons rhs0 rhs1) rhs)
  (cond
    [(< lhs0 rhs0) '<]
    [(> lhs0 rhs0) '>]
    [(< lhs1 rhs1) '<]
    [(> lhs1 rhs1) '>]
    [else '=]
  )
)

(define pass-color-graph
  (class object%
    (super-new)
    (field 
      [interference-graph #f]
      ; (cons mask-sz var-id) -> #t
      [queue (ordl-make-empty cons-integer-compare)]
      ; var-id -> reg-id
      [select (ordl-make-empty integer-compare)]
      ; var-id -> reject-reg-set
      [mask (ordl-make-empty integer-compare)]
      )
    (define/public (pass p) (match p
      [(X86Program info blocks)
        (set! interference-graph (dict-ref info 'interference))
        (pre-interference-handle)
        (build-color-graph)
        (X86Program (dict-set* info 'color-graph select) blocks)
      ]
    ))
    (define/private (pre-interference-handle)
      (define graph (for/fold ([g interference-graph]) ([i (in-range 16)])
        (add-vertex g i)))
      (define graph^
        (for/fold ([g graph]) ([v (in-vertices interference-graph)])
          (define g^ (add-edge g v 15))
          (define g^^ (add-edge g^ v (dict-ref x86_64-regs 'rsp)))
          (define g^^^ (add-edge g^^ v (dict-ref x86_64-regs 'rbp)))
          g^^^
        ))
      (set! interference-graph graph^)
      (define select^
        (for/fold ([s select]) ([i (in-range 16)])
          (dict-set s i i)))
      (set! select select^)
    )
    (define/private (build-color-graph-pre)
      (set! mask (for/fold ([m mask]) ([u (in-vertices interference-graph)])
        (define u-select (dict-ref select u #f))
        (cond
          [u-select
            (for/fold ([m m]) ([v (in-neighbors interference-graph u)])
              (define v-origin-mask (dict-ref m v 0))
              (define v-mask (bset-add v-origin-mask u-select))
              (dict-set m v v-mask)
            )
          ]
          [else m]
        )))
      (define queue^
        (for/fold ([q queue]) ([u (in-vertices interference-graph)])
          (define u-select (dict-ref select u #f))
          (cond 
            [u-select q]
            [else
              (define u-mask (dict-ref mask u 0))
              (define u-mask-sz (bset-length u-mask))
              (dict-set q (cons u-mask-sz u) #t)
            ]
          )))
      (set! queue queue^)
    )
    (define/private (build-color-graph)
      (build-color-graph-pre)
      (let loop ()
        (cond
          [(ordl-empty? queue) (void)]
          [else
            (match-define (cons (and h (cons m var-id)) #t) (ordl-max queue))
            (set! queue (dict-remove queue h))
            (define reg-id (min-reg-id-from-mask m))
            (set! select (dict-set select var-id reg-id))
            (define-values (mask^ q)
              (for/fold ([m mask] [q queue]) ([u (in-neighbors interference-graph var-id)])
                (define cur-m (dict-ref m u 0))
                (cond
                  [(bitwise-bit-set? cur-m reg-id) (values m q)]
                  [(dict-has-key? select u) (values m q)]
                  [else
                    (define cur-m^ (bitwise-ior cur-m (arithmetic-shift 1 reg-id)))
                    (define cur-m-sz (bset-length cur-m))
                    (define q^ (dict-set (dict-remove q (cons cur-m-sz u)) (cons (+ cur-m-sz 1) u) #t))
                    (define m^ (dict-set m u cur-m^))
                    (values m^ q^)
                  ])
              ))
            (set! mask mask^)
            (set! queue q)
            (loop)
          ]
        )
      )
    )
  ))

(provide pass-color-graph)
