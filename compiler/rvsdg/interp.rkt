#lang racket/base

(require racket/class racket/match racket/dict)
(require cutie-ftree)
(require "core.rkt")
(require racket/trace)

(define Interp (class object%
  (init-field graph)
  (super-new)
  (define/public (interp-omega env id)
    (eprintf "interp-omega ~a\n" id)
    (match-define (Omega n m argument result) id)
    (define env^ env)
    (define env^^ (for/fold ([env env^]) ([i (in-range m)]) (define iid (+ result i)) (interp-input env iid)))
    env^^
  )
  (define/public (interp-theta env id)
    (match-define (Gamma n m input output argument result) id)
    (void)
    (error 'interp-theta "unimpl")
  )
  (define/public (interp-lambda env id)
    (void)
    (error 'interp-lambda "unimpl")
  )
  (define/public (interp-delta env id)
    (void)
    (error 'interp-delta "unimpl")
  )
  (define/public (interp-literal env literal)
    (match-define (Literal v output-id) literal)
    (dict-set env output-id v)
  )
  (define/public (interp-phi env id)
    (void)
    (error 'interp-phi "unimpl")
  )
  (define/public (interp-gamma env id)
    (match-define (Gamma n m input output argument result) id)
    (define env^ (for/fold ([env env]) ([i (in-range (+ m 1))]) (define iid (+ input i)) (interp-input env iid)))
    (define v0 (interp-input-raw env^ input))
    (when (or (< v0 0) (>= v0 n)) (error 'interp-gamma "Invalid fall through cond ~a, in [0, ~a)" v0 n))
    (define result^ (+ result (* m v0)))
    (define env^^ (for/fold ([env env^]) ([i (in-range m)]) (define iid (+ result^ i)) (interp-input env iid)))
    (define env^^^ (for/fold ([env env^^]) ([i (in-range m)]) 
      (define iid (+ result^ i)) (define oid (+ output i))
      (dict-set env oid (interp-input-raw env^^ iid))
    ))
    env^^^
  )
  (define/public (interp-primitive env p)
    (eprintf "interp-primitive ~a\n" p)
    (match-define (Primitive op n m input output) p)
    (define env^ (for/fold ([env env]) ([i (in-range n)]) (define iid (+ input i)) (interp-input env iid)))
    (match* (op n m)
      [('+ _ 1)
        (define a (for/fold ([a 0]) ([i (in-range n)])
          (define iid (+ input i))
          (define oid (dict-ref (Graph-input-src graph) iid))
          (+ a (dict-ref env^ oid))
        ))
        (define oid output)
        (dict-set env^ oid a)
      ]
      [('- 2 1)
        (define oid0 (dict-ref (Graph-input-src graph) input))
        (define oid1 (dict-ref (Graph-input-src graph) (+ input 1)))
        (define a (- (dict-ref env^ oid0) (dict-ref env^ oid1)))
        (define oid output)
        (dict-set env^ oid a)
      ]
      [('* _ 1)
        (define a (for/fold ([a 1]) ([i (in-range n)])
          (define iid (+ input i))
          (define oid (dict-ref (Graph-input-src graph) iid))
          (* a (dict-ref env^ oid))
        ))
        (define oid output)
        (dict-set env^ oid a)
      ]
      [('/ 2 1)
        (define oid0 (dict-ref (Graph-input-src graph) input))
        (define oid1 (dict-ref (Graph-input-src graph) (+ input 1)))
        (define a (/ (dict-ref env^ oid0) (dict-ref env^ oid1)))
        (define oid output)
        (dict-set env^ oid a)
      ]
    )
  )
  (define/public (interp-argument env id)
    (define node (cdr (ordl-query-weak (Graph-output-loc graph) id 'le)))
    (void)
    (error 'interp-argument "unimpl")
  )
  (define/public (interp-input env id)
    (eprintf "interp-input ~a\n" id)
    (define dst (dict-ref (Graph-input-src graph) id))
    (cond
      [(dict-has-key? env dst) (dict-ref env dst)]
      [else
        (define src-node (cdr (ordl-query-weak (Graph-output-loc graph) dst 'le)))
        (define p (Graph-parent graph))
        (define dst-node (cdr (ordl-query-weak (Graph-input-loc graph) id 'le)))
        (define dst-parent (dict-ref p dst-node #f))
        (eprintf "interp-input ~a <- ~a(~a)\n" id dst src-node)
        (cond
          [(and dst-parent (= src-node dst-parent))
            (interp-argument env dst)
          ]
          [else
            (eprintf "input -> interp ~a\n" src-node)
            (interp env src-node)
          ]
        )
      ]
    )
  )
  (define/public (interp-input-raw env id)
    (dict-ref env (dict-ref (Graph-input-src graph) id))
  )
  (define/public (interp env id)
    (define graph-nodes (Graph-nodes graph))
    (define node (dict-ref graph-nodes id))
    (eprintf "interp ~a\n" node)
    (cond 
      [(Omega? node) (interp-omega env node)]
      [(Delta? node) (interp-delta env node)]
      [(Lambda? node) (interp-lambda env node)]
      [(Literal? node) (interp-literal env node)]
      [(Primitive? node) (interp-primitive env node)]
      [(Phi? node) (interp-phi env node)]
      [(Theta? node) (interp-theta env node)]
      [(Gamma? node) (interp-gamma env node)]
    )
  )
))

(define (test0)
  (define graph (empty-graph))
  (define-values (omega-id omega graph^) (create-omega graph 0 1))
  (define-values (literal-id literal graph^^) (create-literal graph^ 23))
  (define graph^^^ (connect-edge-raw graph^^ (Literal-output literal) (Omega-result omega)))
  (define graph^^^^ (set-parent-raw graph^^^ literal-id omega-id))
  (define obj (new Interp [graph graph^^^^]))
  (define env (send obj interp (ordl-make-empty integer-compare) omega-id))
  (send obj interp-input-raw env (Omega-result omega))
)

(define (test1)
  (define graph (empty-graph))
  (define-values (omega-id omega graph^) (create-omega graph 0 1))
  (define-values (literal-id literal graph^^) (create-literal graph^ 23))
  (define-values (literal-id2 literal2 graph^^^) (create-literal graph^^ 19))
  (define-values (plus-id plus graph^^^^) (create-primitive graph^^^ '+ 2 1))
  (define graph^^^^^ 
    (connect-edge-raw
      (connect-edge-raw
        (connect-edge-raw graph^^^^ (Literal-output literal) (Primitive-input plus))
        (Literal-output literal2) (+ (Primitive-input plus) 1)
      )
      (Primitive-output plus) (Omega-result omega) 
    )
  )
  (define graph^^^^^^ 
    (set-parent-raw
      (set-parent-raw
        (set-parent-raw graph^^^^^ literal-id omega-id)
        literal-id2 omega-id
      )
      plus-id omega-id
    )
  )
  (define obj (new Interp [graph graph^^^^^^]))
  (define env (send obj interp (ordl-make-empty integer-compare) omega-id))
  (send obj interp-input-raw env (Omega-result omega))
)

(test1)
