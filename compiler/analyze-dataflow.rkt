#lang racket

(require "../utilities.rkt")
(require "graph-core.rkt")
(require cutie-ftree)

(define analyzer
  (class object%
    (super-new)
    (field [worklist (ral-empty)] [result (ordl-make-empty symbol-compare)])
    (define/public (analyze-dataflow graph transfer bottom join batches fn)
      (define change-able (mutable-set))
      (define graph-t (transpose graph))
      (for ([sij (in-vertices graph-t)])
        (set! result (dict-set result sij (transfer sij bottom))))
      (for ([si (in-ral0 batches)])
        (set-clear! change-able)
        (for ([sij (in-dict-keys si)])
          (define sij^ (fn sij))
          (set-add! change-able sij^)
          (set! worklist (ral-consr worklist sij^)))
        (while (not (ral-empty? worklist))
          (define-values (node w^) (ral-dropr worklist)) (set! worklist w^)
          (define preds (get-neighbors graph-t node))
          (define input
            (cond
              [(ral-empty? preds) (set (Reg 'rax))]
              [else
                (for/fold ([state bottom]) ([pred (in-ral0 preds)])
                  (join state (dict-ref result pred))
                )
              ]
            )
          )
          (define output (transfer node input))
          (cond [(not (equal? output (dict-ref result node))) 
            (set! result (dict-set result node output))
            (for ([s (in-neighbors graph node)])
              (when (set-member? change-able s)
                (set! worklist (ral-consr worklist s)))
            )]))
      )
    )
  ))

(provide analyzer)
