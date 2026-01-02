#lang racket

(require "core/core-types.rkt")
(require "core/utilities.rkt")
(require "graph-core.rkt")
(require "core/integer-set.rkt")
(require "lib/ftree.rkt")

(require "analyze-dataflow.rkt")
(require "x86instr.rkt")

(define (pvector-rev seq)
  (for/fold ([v (pvector-empty)]) ([i (in-pvector seq)]) (pvector-cons-left v i))
)

(define pass-uncover-live
  (class object%
    (super-new)
    (field [instr-ana (new instr-analysis)])
    (define (pass-instr* instr* end) (match instr*
      [(pvector) (pvector-cons-left (pvector-empty) end)]
      [(pvector** (pvector _ rest) last)
        (match last
          [(or (? Jmp?) (? JmpIf?)) (pass-instr* rest end)]
          [_ (pass-instr instr* end (pvector-empty))]
        )
      ]
    ))
    (define (pass-instr instr* current cont) (match instr*
      [(pvector) (pvector-cons-left cont current)]
      [(pvector** (pvector _ rest) instr)
        (define r (send instr-ana read-from-instr instr))
        (define w (send instr-ana write-from-instr instr))
        (define current^ (bset-union r (bset-subtract current w)))
        (pass-instr rest current^ (pvector-cons-left cont current))
      ]
    ))
    (define/public (pass p) (match p [(X86Program info blocks)
      (debug "pass" info)
      (define graph (dict-ref info 'graph))
      (define components (dict-ref info 'connect-component))
      (define ana (new analyzer))
      (define graph-t (transpose graph))
      (define (transfer bb-id input)
        (match-define (Block info _) (dict-ref blocks bb-id))
        (match-define (cons add-set drop-set) (dict-ref info 'live-change))
        (bset-union (bset-subtract input drop-set) add-set)
      )
      (define bottom 0)
      (define default-set (bset 0)) ; (Reg 'rax)
      (define join bset-union)
      (send ana analyze-dataflow
        graph-t
        transfer
        bottom
        join
        (pvector-rev components)
        (lambda (i) i)
        default-set
      )
      (define t (get-field analyze-value ana))
      (define blocks^ (for/fold ([blocks^ (ordl-make-empty integer-compare)]) ([(bb-id bb) (in-dict blocks)])
        (match-define (Block info instr*) bb)
        (define succs (in-neighbors graph bb-id))
        (define cur
          (for/fold ([cur (bset)]) ([i succs])
            (bset-union cur (dict-ref t i))
          ))
        (dict-set blocks^ bb-id (Block (dict-set info 'live (pass-instr* instr* cur)) instr*))
      ))
      (X86Program info blocks^)
    ]))
  )
)

(provide pass-uncover-live)
