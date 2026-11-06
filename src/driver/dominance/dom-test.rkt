#lang racket/base

;; Dom: correctness against the textbook set-based definitions on random
;; CFGs, dom-fix against compute-dom, the GCC prune_unused_phi_nodes
;; semantics against the liveness pruning used by driver/ssa, and a
;; timing table (printed, not asserted).

(require rackunit racket/list racket/vector
         "../../kernel/data/data.rkt"
         "dom.rkt")

;; ---- random reducible-ish CFGs ------------------------------------------
;; node i has 1-2 forward successors among (i+1 .. n-1) plus, with
;; probability PBACK, one back edge to an earlier node; node 0 is the entry.
(define (random-cfg n #:pback [pback 0.2] #:seed [seed 1])
  (random-seed seed)
  (for/vector ([i (in-range n)])
    (define fwd
      (cond
        [(= i (sub1 n)) '()]
        [else
         (define k (if (< (random) 0.6) 1 2))
         (remove-duplicates
          (for/list ([_ k]) (+ i 1 (random (max 1 (min 8 (- n i 1)))))))]))
    (define back (if (and (> i 0) (< (random) pback)) (list (random i)) '()))
    (list->pvector (append fwd back))))

(define (succs-of g) (lambda (i) (vector-ref g i)))
(define (nodes-of g) (for/pvector ([i (in-range (vector-length g))]) i))

;; ---- oracle: iterative set-based dominators (the former driver) ---------
(define (naive-dominators g)
  (define n (vector-length g))
  (define preds (make-vector n '()))
  (for* ([i n] [s (in-pvector (vector-ref g i))]) (vector-set! preds s (cons i (vector-ref preds s))))
  (define reach (let loop ([s (list 0)] [seen (hash)])
                  (cond [(null? s) seen]
                        [(hash-ref seen (car s) #f) (loop (cdr s) seen)]
                        [else (loop (append (pvector->list (vector-ref g (car s))) (cdr s)) (hash-set seen (car s) #t))])))
  (define all (for/hash ([i n] #:when (hash-ref reach i #f)) (values i #t)))
  (define dom (for/hash ([i (in-hash-keys all)]) (values i (if (= i 0) (hash 0 #t) all))))
  (let fix ([dom dom])
    (define-values (dom* changed)
      (for/fold ([d dom] [ch #f]) ([i (in-hash-keys all)] #:unless (= i 0))
        (define ps (filter (lambda (p) (hash-ref all p #f)) (vector-ref preds i)))
        (define inter (for/fold ([acc all]) ([p ps]) (for/hash ([k (in-hash-keys acc)] #:when (hash-ref (hash-ref d p) k #f)) (values k #t))))
        (define new (hash-set inter i #t))
        (if (equal? new (hash-ref d i)) (values d ch) (values (hash-set d i new) #t))))
    (if changed (fix dom*) dom*)))

(define (check-dom g)
  (define d (compute-dom (nodes-of g) 0 (succs-of g)))
  (define oracle (naive-dominators g))
  ;; same reachable set
  (check-equal? (dom-size d) (hash-count oracle))
  (for ([(x doms) (in-hash oracle)])
    (define i (dom-index d x))
    ;; dominates? agrees with the sets
    (for ([(y _) (in-hash oracle)])
      (check-equal? (dom-dominates? d (dom-index d y) i) (hash-ref doms y #f)
                    (format "dominates ~a ~a" y x)))
    ;; idom is the strict dominator dominated by all other strict dominators
    (unless (= i 0)
      (define strict (for/list ([(y _) (in-hash doms)] #:unless (= y x)) y))
      (define id (dom-node d (dom-idom d i)))
      (check-true (and (memv id strict) #t))
      (for ([y strict]) (check-true (hash-ref (hash-ref oracle id) y #f) (format "idom ~a of ~a vs ~a" id x y))))
    ;; frontier: y in DF(x) iff x dominates a pred of y and does not strictly dominate y
    (define df (for/list ([f (in-pvector (dom-frontier d i))]) (dom-node d f)))
    (for ([(y _) (in-hash oracle)])
      (define j (dom-index d y))
      (define expected
        (and (not (and (not (= x y)) (hash-ref (hash-ref oracle y) x #f)))
             (for/or ([p (in-pvector (dom-preds d j))]) (dom-dominates? d i p))))
      (check-equal? (and (memv y df) #t) expected (format "DF ~a ~a" x y)))))

;; ---- GCC prune_unused_phi_nodes semantics ---------------------------------
;; PHIS/KILLS/USES: intbits of indices; returns the live PHI blocks.
(define (gcc-prune d phis kills uses)
  (cond
    [(intbits-empty? uses) intbits-empty]
    [else
     (define phis1 (intbits-subtract phis (intbits-subtract kills uses)))
     (cond
       [(intbits-empty? phis1) phis1]
       [else
        (define defs (intbits-union kills phis1))
        ;; nearest def dominating (ancestor-or-self of) I, or #f
        (define (nearest-def i)
          (let loop ([i i]) (cond [(intbits-ref defs i) i] [(zero? i) #f] [else (loop (dom-idom d i))])))
        (let loop ([work (intbits->list uses)] [uses uses] [live intbits-empty])
          (cond
            [(null? work) live]
            [else
             (define b (car work))
             (define p (if (intbits-ref phis1 b) b (let ([u (and (> b 0) (nearest-def (dom-idom d b)))]) (and u (intbits-ref phis1 u) u))))
             (cond
               [(or (not p) (intbits-ref live p)) (loop (cdr work) uses live)]
               [else
                (define-values (work* uses*)
                  (for/fold ([w (cdr work)] [us uses]) ([u (in-pvector (dom-preds d p))])
                    (if (or (intbits-ref us u) (intbits-ref kills u)) (values w us) (values (cons u w) (intbits-set us u)))))
                (loop work* uses* (intbits-set live p))])]))])]))

;; the liveness pruning of driver/ssa/construct.rkt, on indices
(define (live-prune d phis kills uses)
  (define live
    (let loop ([work (intbits->list uses)] [live uses])
      (cond
        [(null? work) live]
        [else
         (define-values (w* l*)
           (for/fold ([w (cdr work)] [l live]) ([p (in-pvector (dom-preds d (car work)))])
             (if (or (intbits-ref l p) (intbits-ref kills p)) (values w l) (values (cons p w) (intbits-set l p)))))
         (loop w* l*)])))
  (intbits-intersect phis live))

(define (random-bits n k) (for/fold ([s intbits-empty]) ([_ k]) (intbits-set s (random n))))

(module+ test
  (test-case "compute-dom agrees with the set definitions on random CFGs"
    (for ([seed (in-range 1 41)])
      (check-dom (random-cfg (+ 4 (modulo (* seed 7) 60)) #:seed seed))))

  (test-case "post-dominators agree with dominators of the reversed graph"
    (define g (random-cfg 40 #:seed 7))
    (define n (vector-length g))
    (define sink (sub1 n))
    (define pd (compute-post-dom (nodes-of g) sink (succs-of g)))
    (define rev (make-vector n '()))
    (for* ([i n] [s (in-pvector (vector-ref g i))]) (vector-set! rev s (cons i (vector-ref rev s))))
    (define rg (for/vector ([l (in-vector rev)]) (list->pvector (reverse l))))
    (define d2 (compute-dom (nodes-of rg) sink (succs-of rg)))
    (check-equal? (dom-size pd) (dom-size d2))
    (for ([x (in-pvector (dom-rpo pd))])
      (check-equal? (dom-idom-node pd x) (dom-idom-node d2 x))))

  (test-case "dom-fix after an edge change equals compute-dom"
    (for ([seed (in-range 1 31)])
      (define g (random-cfg (+ 10 (modulo (* seed 11) 50)) #:seed seed))
      (define d (compute-dom (nodes-of g) 0 (succs-of g)))
      (define n (vector-length g))
      ;; add an edge a -> b between reachable nodes, then remove one
      (define a (dom-node d (random (dom-size d))))
      (define b (dom-node d (random (dom-size d))))
      (define g2 (vector-copy g))
      (vector-set! g2 a (if (pvector-index-of (vector-ref g a) b) (vector-ref g a) (pvector-cons-right (vector-ref g a) b)))
      (define d2 (dom-fix d (pvector b) (succs-of g2)))
      (define full (compute-dom (nodes-of g2) 0 (succs-of g2)))
      (check-equal? (Dom-idom d2) (Dom-idom full))
      (check-equal? (Dom-frontier d2) (Dom-frontier full))
      (check-equal? (Dom-nodes d2) (Dom-nodes full))))

  (test-case "GCC prune_unused_phi_nodes == pruning by liveness"
    (for ([seed (in-range 1 201)])
      (define g (random-cfg (+ 6 (modulo (* seed 13) 40)) #:seed seed))
      (define d (compute-dom (nodes-of g) 0 (succs-of g)))
      (define n (dom-size d))
      (random-seed (+ 1000 seed))
      (define kills (random-bits n (add1 (random 4))))
      (define uses (random-bits n (random 5)))
      (define phis (dom-idf d kills))
      (check-equal? (gcc-prune d phis kills uses) (live-prune d phis kills uses)
                    (format "seed ~a kills ~a uses ~a" seed (intbits->list kills) (intbits->list uses)))))

  (test-case "timing (informational)"
    (for ([n '(2000 20000)])
      (define g (random-cfg n #:seed 3))
      (collect-garbage)
      (define t0 (current-inexact-milliseconds))
      (define d (compute-dom (nodes-of g) 0 (succs-of g)))
      (define t1 (current-inexact-milliseconds))
      (define b (dom-node d (quotient (dom-size d) 2)))
      (define g2 (vector-copy g))
      (vector-set! g2 1 (pvector-cons-right (vector-ref g 1) b))
      (define t2 (current-inexact-milliseconds))
      (define d2 (dom-fix d (pvector b) (succs-of g2)))
      (define t3 (current-inexact-milliseconds))
      (printf ";; Dom n=~a reachable=~a: compute-dom ~a ms, dom-fix ~a ms\n"
              n (dom-size d) (/ (round (* 10 (- t1 t0))) 10.0) (/ (round (* 10 (- t3 t2))) 10.0))
      (check-true (> (dom-size d) 0)))))
