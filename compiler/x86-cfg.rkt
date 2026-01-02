#lang racket

(require "core/core-types.rkt")
(require "core/utilities.rkt")
(require "graph-core.rkt")
(require "lib/ftree.rkt")

(define x86-control-flow-graph
  (class object%
    (super-new)
    (define/public (build-cfg program) (match program [(X86Program _ blocks)
      (build-graph (init-graph blocks) blocks)
    ]))
    ; return list, 0, 1, 2 element
    (define (block-tail block)
      (match block
        [(pvector** (pvector _ _) (JmpIf _ t1) (Jmp t2)) `(,t1 ,t2)]
        [(pvector** (pvector _ _) (Jmp t)) `(,t)]
        [(? pvector?) '()]
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
