#lang racket

(require "../utilities.rkt")
(require "graph-core.rkt")
(require cutie-ftree)

(require "x86abi.rkt")
(require "interference.rkt")
(require "x86instr.rkt")

(define pass-build-interference
  (class object%
    (super-new) 
    (define (writes-from-block block) (match block
      [(Block _ instr*)
        (define analysis (new instr-analysis))
        (for/fold ([ws (ral-empty)]) ([i (in-ral0 instr*)])
          (ral-consr ws (send analysis write-from-instr i))
        )
      ]
    ))
    (field [interference-obj (new interference)])
    (define (build-block block) (match block [(Block info instr*)
      (define live (dict-ref info 'live))
      (define ws (writes-from-block block))
      (send interference-obj solve-weak ws live)
    ]))
    (define/public (pass p) (match p [(X86Program info blocks)
      (for ([(_name block) (in-dict blocks)])
        (build-block block)
      )
      (define i (get-field i-graph interference-obj))
      (define info^ (dict-set info 'interference i))
      (X86Program info^ blocks)
    ]))
  ))

(provide pass-build-interference)
