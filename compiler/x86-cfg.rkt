#lang racket

(require "../utilities.rkt")
(require "graph-core.rkt")
(require cutie-ftree)

(define x86-control-flow-graph
  (class object%
    (super-new)
    (define/public (build-cfg program) (match program [(X86Program _ blocks)
      (build-graph (init-graph blocks) blocks)
    ]))
    ; return list, 0, 1, 2 element
    (define (block-tail block)
      (cond
        [(ral-empty? block) '()]
        [(equal? 1 (ral-length block))
          (match (ral-viewr block)
            [(Jmp t) (list t)]
            [_ (list)]
          )
        ]
        [else
          (match-define-values (x2 block^) (ral-dropr block))
          (match-define x1 (ral-viewr block^))
          (match* (x1 x2)
            [((JmpIf _ t1) (Jmp t2)) (list t1 t2)]
            [(_ (Jmp t)) (list t)]
            [(_ _) (list)]
          )
        ]
      )
    )
    (define (init-graph blocks)
      (for/fold ([graph (graph-make-empty)]) ([node (in-dict-keys blocks)])
        (add-vertex graph node)
      )
    )
    (define (build-graph graph blocks)
      (for/fold ([graph graph]) ([(node content) (in-dict blocks)])
        (match-define (Block _ instr*) content)
        (define tos (block-tail instr*))
        (for/fold ([graph graph]) ([t tos]) (add-directed-edge graph node t))
      )
    )
  ))

(provide x86-control-flow-graph)
