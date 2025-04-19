#lang racket

(require "../utilities.rkt")
(require cutie-ftree)

(require "x86instr.rkt")

(define pass-block-uncover-live
  (class object%
    (super-new)
    (field [analysis (new instr-analysis)])
    (define pass-block (match-lambda [(Block info instr*)
      (define a (mutable-set))
      (define d (mutable-set))
      (pass-instr* instr* a d) 
      (define info^ (dict-set info 'live-change (cons a d)))
      (Block info^ instr*)
    ]))
    (define (pass-instr* instr* add-set drop-set) (match instr*
      [(? ral-empty?) (void)]
      [_
        (define-values (instr rest) (ral-dropr instr*))
        (define r (send analysis read-from-instr instr))
        (define w (send analysis write-from-instr instr))
        (set-union! drop-set w)
        (set-subtract! add-set w)
        (set-union! add-set r)
        (set-subtract! drop-set r)
        (pass-instr* rest add-set drop-set)
      ]
    ))
    (define/public pass (match-lambda [(X86Program info blocks)
      (define blocks^ (for/fold ([a (ordl-make-empty symbol-compare)]) ([(tag block) (in-dict blocks)])
        (ordl-insert a tag (pass-block block) #f)
      ))
      (X86Program info blocks^)
    ]))
  )
)

(provide pass-block-uncover-live)
