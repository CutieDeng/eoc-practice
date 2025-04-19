#lang racket

(require racket/set)
(require racket/fixnum)
(require "interp.rkt")
(require "utilities.rkt")

(require "interp-Lvec-prime.rkt") (require "type-check-Lvec.rkt")
(require "interp-Cvec.rkt")
(require "type-check-Cvec.rkt")

(require "compiler/graph-core.rkt")
(require "graph-printing.rkt")
(require "priority_queue.rkt")

(require data/queue)

(require racket/pretty)

(require "compiler/x86abi.rkt")

(define pass-abstract
  (class object%
    (super-new)
    (abstract pass)
  ))

(require "compiler/shrink.rkt")
(define shrink (λ (p) (send (new pass-shrink) pass p)))

(require "compiler/uniquify.rkt")
(define uniquify (λ (p) (send (new pass-uniquify) pass p)))

(require "compiler/rco.rkt")
(define remove-complex-opera* (λ (p) (send (new pass-rco) pass p)))

(require "compiler/explicate-control.rkt")
(define explicate-control (λ (p) (send (new pass-explicate-control) pass p)))

(require "compiler/select-instructions.rkt")
(define select-instructions (λ (p) (send (new pass-select-instructions) pass p)))

(define pass-color-graph
  (class pass-abstract
    (super-new)
    (define/override (pass p) (match p
      [(X86Program info blocks)
        (define interference (dict-ref info 'interference))
        (define interference-var (box interference))
        (pre-interference-handle interference-var)
        (define int-graph (build-int-color-graph (unbox interference-var) (match-lambda 
          [_ #t]
        )))
        (define vec-graph (build-vec-color-graph (unbox interference-var) (match-lambda
          [_ #f]
        )))
        (X86Program (dict-set (dict-set info 'color-graph int-graph) 'vec-color-graph vec-graph) blocks)
      ]
    ))
    (define/public (pre-interference-handle interference-graph)
      (define graph (unbox interference-graph))
      (for ([r (in-vertices graph)])
        ; (add-edge! interference-graph r (Reg 'r15))
        ; (add-edge! interference-graph r (Reg 'rsp))
        ; (add-edge! interference-graph r (Reg 'rbp))
        (set! graph (add-edge (add-vertex graph (Reg 'r15)) r (Reg 'r15)))
        (set! graph (add-edge (add-vertex graph (Reg 'rsp)) r (Reg 'rsp)))
        (set! graph (add-edge (add-vertex graph (Reg 'rbp)) r (Reg 'rbp)))
      )
      (set-box! interference-graph graph)
    )
    (define/public (build-int-color-graph interference-graph int-filter)
      (define q (make-pqueue (λ (a b) (>= (cdr a) (cdr b)))))
      (define (make-pqueue-raw) (make-pqueue >=))
      (for ([n (sequence-filter int-filter (in-vertices interference-graph))])
        (pqueue-push! q (cons n 0))
      )
      (define neighbors-set 
        (for/fold ([nei-set '()]) ([i (range (length caller-and-callee-regs))] [r caller-and-callee-regs])
          (with-handlers ([exn:fail? (λ (_e) nei-set)])
            (for/fold ([nei-set^ nei-set]) ([n (sequence-filter int-filter (in-neighbors interference-graph (Reg r)))])
              (define inner (dict-ref nei-set^ n '()))
              (define inner-2 (set-add inner i))
              (pqueue-push! q (cons n (length inner-2)))
              (dict-set nei-set^ n inner-2)
            ))
        )
      )
      (define selections (for/fold ([select '()]) ([i (range (length caller-and-callee-regs))] [r caller-and-callee-regs])
        (dict-set select (Reg r) i)
      ))
      (define color-graph-value (let color-find ([neighbors-set neighbors-set] [selections selections])
        (cond
          [(= (pqueue-count q) 0) selections]
          [else
            (define pop (pqueue-pop! q))
            (cond
              [(dict-has-key? selections (car pop)) (color-find neighbors-set selections)]
              [else
                (define color (let color-find-2 ([color 0])
                  (if (set-member? (dict-ref neighbors-set (car pop) '()) color)
                    (color-find-2 (+ color 1))
                    color
                    )))
                (define neighbors-set-2 (for/fold ([n neighbors-set]) ([v (sequence-filter int-filter (in-neighbors interference-graph (car pop)))])
                  (define inner (dict-ref n v '()))
                  (define inner-2 (set-add inner color))
                  (pqueue-push! q (cons v (length inner-2)))
                  (dict-set n v inner-2)
                ))
                (color-find neighbors-set-2 (dict-set selections (car pop) color))
              ]
            )
          ]
        )
      ))
      color-graph-value
    )
    (define/public (build-vec-color-graph interference-graph vertice-filter)
      (define pq (make-pqueue (λ (a b) (> (cdr a) (cdr b)))))
      (for ([v (sequence-filter vertice-filter (in-vertices interference-graph))])
        (pqueue-push! pq (cons v 0))
      )
      (define neighbors-set '())
      (let color-find ([neighbors-set neighbors-set] [selections '()]) (cond
        [(= (pqueue-count pq) 0) selections]
        [else
          (define p (pqueue-pop! pq))
          (cond
            [(dict-has-key? selections (car p)) (color-find neighbors-set selections)]
            [else 
              (define select-color (let select-color ([n 0]) 
                (cond
                  [(set-member? (dict-ref neighbors-set (car p) '()) n) (select-color (+ n 1))]
                  [else n]
                )))
              (define neighbors-set^ (for/fold ([neighbors-set neighbors-set]) ([v (sequence-filter vertice-filter (in-vertices interference-graph))])
                (define set^ (set-add (dict-ref neighbors-set v '()) select-color))
                (pqueue-push! pq (cons v (length set^)))
                (dict-set neighbors-set v set^)
              ))
              (color-find neighbors-set^ (dict-set selections (car p) select-color))
            ]
          )
        ]
      ))
    )
  ))

(define color-graph (λ (p) (send (new pass-color-graph) pass p)))

(require cutie-ftree)

(require "compiler/prelude-conclusion.rkt")
(define prelude-and-conclusion (λ (p) (send (new pass-prelude-and-conclusion) pass p)))

(require "compiler/connect-component.rkt")
(define connect-component (λ (p) (send (new pass-connect-component) pass p)))

(require "compiler/block-uncover-live.rkt")
(define block-uncover-live (λ (p) (send (new pass-block-uncover-live) pass p)))

(require "compiler/collect-set.rkt")
(define collect-set! (lambda (p) (send (new pass-collect-set!) pass p)))

(require "compiler/uncover-get.rkt")
(define uncover-get!-exp (λ (p) (send (new pass-uncover-get!-exp) pass p)))

(require "compiler/patch-instructions.rkt")
(define patch-instructions (λ (p) (send (new pass-patch-instructions) pass p)))

(require "compiler/expose-allocation.rkt")
(define expose-allocation (λ (p) (send (new pass-expose-allocation) pass p)))

(require "compiler/build-interference.rkt")
(define build-interference (λ (p) (send (new pass-build-interference) pass p)))

(require "compiler/uncover-live.rkt")
(define uncover-live (λ (p) (send (new pass-uncover-live) pass p)))

(require "compiler/allocate-registers.rkt")
(define allocate-registers (λ (p) (send (new pass-allocate-registers) pass p)))

(require "compiler/dominance.rkt")
(define dominace (λ (x) (send (new pass-dominance) pass x)))

(define init-program (match-lambda 
  ([Program info x] [Program (for/fold ([info (ordl-make-empty symbol-compare)]) ([(k v) (in-dict info)])
    (ordl-insert info k v #f)) x])))

; (debug-level 2)
(debug-level 0)
(define compiler-passes
  `(
    ("init" ,init-program ,interp-Lvec-prime ,type-check-Lvec)
    ("shrink" ,shrink ,interp-Lvec-prime ,type-check-Lvec)
    ("uniquify" ,uniquify ,interp-Lvec-prime ,type-check-Lvec)
    ("collect-set!" ,collect-set! ,interp-Lvec-prime ,type-check-Lvec)
    ("uncover-get!-exp" ,uncover-get!-exp ,interp-Lvec-prime ,type-check-Lvec-has-type #;,type-check-Lvec)
    ("expose-allocation" ,expose-allocation ,interp-Lvec-prime ,type-check-Lvec)
    ("remove complex opera*" ,remove-complex-opera* ,interp-Lvec-prime ,type-check-Lvec)
    ("explicate control" ,explicate-control ,interp-Cvec ,type-check-Cvec)
    ("instruction selection" ,select-instructions ,interp-pseudo-x86-2)
    ("connect component preparation" ,connect-component ,interp-pseudo-x86-2)
    ("ssa-calc" ,dominace ,interp-pseudo-x86-2)
    ("block uncover live" ,block-uncover-live ,interp-pseudo-x86-2)
    ("uncover live" ,uncover-live ,interp-pseudo-x86-2)
    ("build interference graph" ,build-interference ,interp-pseudo-x86-2)
    ("build color graph" ,color-graph ,interp-pseudo-x86-2)
    ("allocate registers" ,allocate-registers ,interp-x86-2)
    ("patch instructions" ,patch-instructions ,interp-x86-2)
    ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-2)
    ("patch instructions" ,patch-instructions ,interp-x86-2)
  ))

(provide compiler-passes)
