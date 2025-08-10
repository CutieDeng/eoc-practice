#lang racket

(require "core/core-types.rkt")
(require "core/utilities.rkt")
(require "graph-core.rkt")
(require cutie-ftree)

(define (block-tos block)
  (match block
    [(ral (_ unlength) ((JmpIf _ t1) atom) ((Jmp t2) atom)) `(,t1 ,t2)]
    [(ral (_ unlength) ((Jmp t) atom)) `(,t)]
    [(? ral?) '()]
  )
)

(define x86-control-flow-graph
  (class object%
    (super-new)
    (define/public (build-cfg program) (match program [(X86Program _ blocks)
      (build-graph (init-graph blocks) blocks)
    ]))
    ; return list, 0, 1, 2 element
    (define (init-graph blocks)
      (debug "init-graph" (sequence->list (in-dict-keys blocks)))
      (for/fold ([graph (graph-make-empty)]) ([bb-id (in-dict-keys blocks)])
        (add-vertex graph bb-id)
      )
    )
    (define (build-graph graph blocks)
      (for/fold ([graph graph]) ([(bb-id bb) (in-dict blocks)])
        (match-define (Block _ instr*) bb)
        (define tos (block-tos instr*))
        (for/fold ([graph graph]) ([t tos]) (add-directed-edge graph bb-id t))
      )
    )
  ))

(provide x86-control-flow-graph)
