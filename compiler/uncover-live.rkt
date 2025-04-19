#lang racket

(require "../utilities.rkt")
(require "graph-core.rkt")
(require cutie-ftree)

(require "analyze-dataflow.rkt")
(require "x86instr.rkt")

(define pass-uncover-live
  (class object%
    (super-new)
    (define (pass-instr* instr* end) (cond
      [(ral-empty? instr*) (vector->ral (vector end))]
      [else
        (define-values (last rest) (ral-dropr instr*))
        (if (or (Jmp? last) (JmpIf? last)) (pass-instr* rest end) (pass-instr instr* end (ral-empty)))
      ]
    ))
    (field [instr-ana (new instr-analysis)])
    (define (pass-instr instr* current cont) (cond
      [(ral-empty? instr*) (ral-consl cont current)]
      [else
        (define-values (instr rest) (ral-dropr instr*))
        (define r (send instr-ana read-from-instr instr))
        (define w (send instr-ana write-from-instr instr))
        (define current^ (set-union r (set-subtract current w)))
        (pass-instr rest current^ (ral-consl cont current))
      ]
    ))
    (define (rev seq)
      (for/fold ([v (ral-empty)]) ([i (in-ral0 seq)]) (ral-consl v i))
    )
    (define/public (pass p) (match p [(X86Program info blocks)
      (define graph (dict-ref info 'graph))
      (define components (dict-ref info 'connect-component))
      (define id2block (dict-ref info 'id2block))
      (define ana (new analyzer))
      (send ana analyze-dataflow 
        (transpose graph)
        (lambda (node input)
          (define block (dict-ref blocks node))
          (match-define (Block info _) block)
          (match-define (cons add drop) (dict-ref info 'live-change))
          (define input^ (set-subtract input drop))
          (define input^^ (set-union input^ add))
          input^^
        )
        (set)
        set-union
        (rev components)
        (lambda (i) (dict-ref id2block i))
      )
      (define t (get-field result ana))
      (define blocks^ (for/fold ([a (ordl-make-empty symbol-compare)]) ([(b-tag b) (in-dict blocks)])
        (match-define (Block info instr*) b)
        (define tos (in-neighbors graph b-tag))
        (define end (mutable-set))
        (for ([to tos])
          (set-union! end (dict-ref t to)))
        (define end^ (for/set ([i (in-mutable-set end)]) i))
        (ordl-insert a b-tag (Block (dict-set info 'live (pass-instr* instr* end^)) instr*) #f)
      ))
      (X86Program info blocks^)
    ]))
  )
)

(provide pass-uncover-live)
