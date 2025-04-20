#lang racket

(require racket/set)
(require racket/fixnum)
(require "interp.rkt")
(require "utilities.rkt")

(require "interp-Lvec-prime.rkt") (require "type-check-Lvec.rkt")
(require "interp-Cvec.rkt")
(require "type-check-Cvec.rkt")

(require "compiler/graph-core.rkt")
(require "graph-printing.rkt")
(require "priority_queue.rkt")

(require data/queue)

(require racket/pretty)

(require "compiler/x86abi.rkt")

(require "compiler/shrink.rkt")
(define shrink (λ (p) (send (new pass-shrink) pass p)))

(require "compiler/uniquify.rkt")
(define uniquify (λ (p) (send (new pass-uniquify) pass p)))

(require "compiler/rco.rkt")
(define remove-complex-opera* (λ (p) (send (new pass-rco) pass p)))

(require "compiler/explicate-control.rkt")
(define explicate-control (λ (p) (send (new pass-explicate-control) pass p)))

(require "compiler/select-instructions.rkt")
(define select-instructions (λ (p) (send (new pass-select-instructions) pass p)))

(require "compiler/color-graph.rkt")
(define color-graph (λ (p) (send (new pass-color-graph) pass p)))

(require cutie-ftree)

(require "compiler/prelude-conclusion.rkt")
(define prelude-and-conclusion (λ (p) (send (new pass-prelude-and-conclusion) pass p)))

(require "compiler/connect-component.rkt")
(define connect-component (λ (p) (send (new pass-connect-component) pass p)))

(require "compiler/block-uncover-live.rkt")
(define block-uncover-live (λ (p) (send (new pass-block-uncover-live) pass p)))

(require "compiler/collect-set.rkt")
(define collect-set! (lambda (p) (send (new pass-collect-set!) pass p)))

(require "compiler/uncover-get.rkt")
(define uncover-get!-exp (λ (p) (send (new pass-uncover-get!-exp) pass p)))

(require "compiler/patch-instructions.rkt")
(define patch-instructions (λ (p) (send (new pass-patch-instructions) pass p)))

(require "compiler/expose-allocation.rkt")
(define expose-allocation (λ (p) (send (new pass-expose-allocation) pass p)))

(require "compiler/build-interference.rkt")
(define build-interference (λ (p) (send (new pass-build-interference) pass p)))

(require "compiler/uncover-live.rkt")
(define uncover-live (λ (p) (send (new pass-uncover-live) pass p)))

(require "compiler/allocate-registers.rkt")
(define allocate-registers (λ (p) (send (new pass-allocate-registers) pass p)))

(require "compiler/dominance.rkt")
(require "compiler/dominance-tree.rkt")
(define dominace (λ (x) 
  (define x^ (send (new pass-dominance) pass x))
  (define x^^ (send (new pass-dominance-tree) pass x^))
  x^^
))

(define init-program (match-lambda 
  ([Program info x] [Program (for/fold ([info (ordl-make-empty symbol-compare)]) ([(k v) (in-dict info)])
    (ordl-insert info k v #f)) x])))

; (debug-level 2)
(debug-level 0)
(define compiler-passes
  `(
    ("init" ,init-program ,interp-Lvec-prime ,type-check-Lvec)
    ("shrink" ,shrink ,interp-Lvec-prime ,type-check-Lvec)
    ("uniquify" ,uniquify ,interp-Lvec-prime ,type-check-Lvec)
    ("collect-set!" ,collect-set! ,interp-Lvec-prime ,type-check-Lvec)
    ("uncover-get!-exp" ,uncover-get!-exp ,interp-Lvec-prime ,type-check-Lvec-has-type #;,type-check-Lvec)
    ("expose-allocation" ,expose-allocation ,interp-Lvec-prime ,type-check-Lvec)
    ("remove complex opera*" ,remove-complex-opera* ,interp-Lvec-prime ,type-check-Lvec)
    ("explicate control" ,explicate-control ,interp-Cvec ,type-check-Cvec)
    ("instruction selection" ,select-instructions ,interp-pseudo-x86-2)
    ("connect component preparation" ,connect-component ,interp-pseudo-x86-2)
    ("dominance" ,dominace ,interp-pseudo-x86-2)
    ("block uncover live" ,block-uncover-live ,interp-pseudo-x86-2)
    ("uncover live" ,uncover-live ,interp-pseudo-x86-2)
    ("build interference graph" ,build-interference ,interp-pseudo-x86-2)
    ("build color graph" ,color-graph ,interp-pseudo-x86-2)
    ("allocate registers" ,allocate-registers ,interp-x86-2)
    ("patch instructions" ,patch-instructions ,interp-x86-2)
    ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-2)
    ("patch instructions" ,patch-instructions ,interp-x86-2)
  ))

(provide compiler-passes)
