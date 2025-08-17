#lang racket

(require "core/utilities.rkt")
(require "core/core-types.rkt")
(require "core/integer-set.rkt")
(require "graph-core.rkt")
(require cutie-ftree)

(require "debug0.rkt")

(define dominance-tree
  (class object%
    (super-new)
    (field [intermediate-dominator (ordl-make-empty integer-compare)] [tree-graph #f])
    (define/public (build-intermediate-dominator-tree dominanced)
      (define-values (bb->sz sz->bb) (for/fold ([bb->sz (ordl-make-empty integer-compare)] [sz->bb (ordl-make-empty integer-compare)])
        ([(bb-id dominator-bb-set) (in-dict dominanced)])
          (define szv (sequence-length (in-bset dominator-bb-set)))
          (values
            (dict-set bb->sz bb-id szv)
            (dict-set sz->bb szv (bset-add (dict-ref sz->bb szv 0) bb-id))
          )
      ))
      (define graph (for/fold ([g (graph-make-empty)]) ([bb-id (in-dict-keys dominanced)])
        (add-vertex g bb-id)))
      (define graph^ (for/fold ([graph graph]) ([(bb-id dominator-bb-set) (in-dict dominanced)])
        (define bb-sz (dict-ref bb->sz bb-id))
        (define bb-exp-sz (- bb-sz 1))
        (define candidate (dict-ref sz->bb bb-exp-sz 0))
        (define intermediate-dom-set (bset-and candidate dominator-bb-set))
        (define intermediate-dom (for/first ([b (in-bset intermediate-dom-set)]) b))
        (when (> (sequence-length (in-bset intermediate-dom-set)) 1) (error 'dominance-tree))
        (cond [intermediate-dom
          (set! intermediate-dominator (dict-set intermediate-dominator bb-id intermediate-dom))
          (add-directed-edge graph intermediate-dom bb-id)
        ]
        [else graph])
        ))
      (set! tree-graph graph^)
    )
  ))

(define pass-dominance-tree
  (class object%
    (super-new)
    (define/public (pass p) (match p [(X86Program info blocks)
      (define t (new dominance-tree))
      (send t build-intermediate-dominator-tree (dict-ref info 'dominanced))
      (define info^ (dict-set* info 'dominator-tree-graph (get-field tree-graph t) 'intermediate-dominator (get-field intermediate-dominator t)))
      (X86Program info^ blocks)
    ]))
  ))

(provide pass-dominance-tree dominance-tree)
