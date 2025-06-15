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
      (match block
        [(ral (_ unlength) ((JmpIf _ t1) atom) ((Jmp t2) atom)) `(,t1 ,t2)]
        [(ral (_ unlength) ((Jmp t) atom)) `(,t)]
        [(? ral?) '()]
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
