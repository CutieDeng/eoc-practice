#lang racket

(require "../utilities.rkt")
(require "graph-core.rkt")
(require cutie-ftree)

(require "debug0.rkt")

(define dominance-tree
  (class object%
    (super-new)
    (field [intermediate-dominator (ordl-make-empty symbol-compare)] [tree #f])
    (define/public (get domined)
      (define sz (ordl-make-empty symbol-compare))
      (define sz^ (for/fold ([sz sz]) ([(name d) (in-dict domined)])
        (dict-set sz name (length (set->list d)))
      ))
      (define graph^ (graph-make-empty))
      (define graph^^ (for/fold ([graph graph^]) ([name (in-dict-keys domined)])
        (add-vertex graph name)
      ))
      (define intermediate-dominator^ (ordl-make-empty symbol-compare))
      (define graph^^^ (for/fold ([graph graph^^]) ([(node domined) (in-dict domined)])
        (define domined^ (set->list domined))
        (define node-value (dict-ref sz^ node))
        (define node-value-sub1 (sub1 node-value))
        (define rst (for/fold ([rst '()]) ([d domined^])
          (if (equal? (dict-ref sz^ d) node-value-sub1) (cons d rst) rst)
        ))
        (match rst
          ['() (unless (equal? (length domined^) 1)
            (assert-unreachable))
            graph]
          [(list x) (set! intermediate-dominator^ (dict-set intermediate-dominator^ node x)) (add-directed-edge graph x node)]
        )
      ))
      (set! intermediate-dominator intermediate-dominator^)
      (set! tree graph^^)
    )
  ))

(define pass-dominance-tree
  (class object%
    (super-new)
    (define/public (pass p) (match p [(X86Program info blocks)
      (define t (new dominance-tree))
      (send t get (dict-ref info 'dominanced))
      (X86Program 
        (dict-set (dict-set info 'dominance-tree (get-field tree t)) 'intermediate-dominator (get-field intermediate-dominator t))
        blocks)
    ]))
  ))

(provide pass-dominance-tree dominance-tree)
