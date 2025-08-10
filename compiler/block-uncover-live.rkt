#lang racket

(require "core/core-types.rkt")
(require "core/integer-set.rkt")
(require cutie-ftree)

(require "x86instr.rkt")

(define pass-block-uncover-live
  (class object%
    (super-new)
    (field [analysis (new instr-analysis)])
    (define pass-block (match-lambda [(Block info instr*)
      (define a (box 0))
      (define d (box 0))
      (pass-instr* instr* a d)
      (define info^ (dict-set info 'live-change (cons (unbox a) (unbox d))))
      (Block info^ instr*)
    ]))
    (define (pass-instr* instr* add-set drop-set) (match instr*
      [(ral) (void)]
      [(ral (rest unlength) (instr atom))
        (define r (send analysis read-from-instr instr))
        (define w (send analysis write-from-instr instr))
        (set-box! drop-set (bset-subtract (bset-union (unbox drop-set) w) r))
        (set-box! add-set (bset-union (bset-subtract (unbox add-set) w) r))
        (pass-instr* rest add-set drop-set)
      ]
    ))
    (define/public pass (match-lambda [(X86Program info blocks)
      (define blocks^ (for/fold ([blocks^ (ordl-make-empty integer-compare)]) ([(bb-id bb) (in-dict blocks)])
        (dict-set blocks^ bb-id (pass-block bb))
      ))
      (X86Program info blocks^)
    ]))
  )
)

(provide pass-block-uncover-live)
