#lang racket

(require "../utilities.rkt")
(require "graph-core.rkt")
(require cutie-ftree)

(require "debug0.rkt")

(define pass-dominance-tree
  (class object%
    (super-new)
    (define/public (pass p) (match p [(X86Program info blocks)
      (define domin (dict-ref info 'dominanced))
      (define sz (ordl-make-empty symbol-compare))
      (define sz^ (for/fold ([sz sz]) ([(name d) (in-dict domin)])
        (dict-set sz name (length (set->list d)))
      ))
      (define graph (graph-make-empty))
      (define graph^ (for/fold ([graph graph]) ([name (in-dict-keys domin)])
        (add-vertex graph name)
      ))
      (define graph^^ (for/fold ([graph graph^]) ([(node domined) (in-dict domin)])
        (define domined^ (set->list domined))
        (define node-value (dict-ref sz^ node))
        (define node-value-sub1 (sub1 node-value))
        (define rst (for/fold ([rst '()]) ([d domined^])
          (if (equal? (dict-ref sz^ d) node-value-sub1) (cons d rst) rst)
        ))
        (match rst
          ['() (unless (equal? (length domined^) 1)
            ; (debug-graph (dict-ref info 'graph))
            ; (debug-con-seq (dict-ref info 'connect-component) (dict-ref info 'id2block))
            ; (debug-dag (dict-ref info 'dag) (dict-ref info 'group2id) (dict-ref info 'id2block))
            ; (debug-dict domin)
            ; (printf "now node: ~a\n" node)
            ; (printf "\tnode domined: ~a\n" domined^)
            (assert-unreachable))
            graph]
          [(list x) (add-directed-edge graph x node)]
        )
      ))
      (define info^ (dict-set info 'dominance-tree graph^^))
      (X86Program info^ blocks)
    ]))
  )
)

(provide pass-dominance-tree)
