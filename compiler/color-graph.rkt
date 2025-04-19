#lang racket

(require "../utilities.rkt")
(require "graph-core.rkt")
(require cutie-ftree)

(require "../priority_queue.rkt")

(require "x86abi.rkt")
(require "interference.rkt")

(define pass-color-graph
  (class object%
    (super-new)
    (field [interference-graph #f])
    (define/public (pass p) (match p
      [(X86Program info blocks)
        (set! interference-graph (dict-ref info 'interference))
        (pre-interference-handle)
        (define int-graph (build-int-color-graph (match-lambda 
          [_ #t]
        )))
        (define vec-graph (build-vec-color-graph (match-lambda
          [_ #f]
        )))
        (X86Program (dict-set (dict-set info 'color-graph int-graph) 'vec-color-graph vec-graph) blocks)
      ]
    ))
    (define (pre-interference-handle)
      (define graph interference-graph)
      (for ([r (in-vertices interference-graph)])
        (set! graph (add-edge (add-vertex graph (Reg 'r15)) r (Reg 'r15)))
        (set! graph (add-edge (add-vertex graph (Reg 'rsp)) r (Reg 'rsp)))
        (set! graph (add-edge (add-vertex graph (Reg 'rbp)) r (Reg 'rbp)))
      )
      (set! interference-graph graph)
    )
    (define (build-int-color-graph int-filter)
      (define q (make-pqueue (λ (a b) (>= (cdr a) (cdr b)))))
      ; (define (make-pqueue-raw) (make-pqueue >=))
      (for ([n (sequence-filter int-filter (in-vertices interference-graph))])
        (pqueue-push! q (cons n 0))
      )
      (define neighbors-set 
        (for/fold ([nei-set (ordl-make-empty interference-cmp-fn)]) ([i (range (length caller-and-callee-regs))] [r caller-and-callee-regs])
          (with-handlers ([exn:fail? (λ (_e) nei-set)])
            (for/fold ([nei-set^ nei-set]) ([n (sequence-filter int-filter (in-neighbors interference-graph (Reg r)))])
              (define inner (dict-ref nei-set^ n '()))
              (define inner-2 (set-add inner i))
              (pqueue-push! q (cons n (length inner-2)))
              (dict-set nei-set^ n inner-2)
            ))
        )
      )
      (define selections (for/fold ([select (ordl-make-empty interference-cmp-fn)]) ([i (range (length caller-and-callee-regs))] [r caller-and-callee-regs])
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
    (define (build-vec-color-graph vertice-filter)
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

(provide pass-color-graph)
