#lang racket/base

;; ============================================================
;; Driver: dominance information (Dom), representation independent
;; ============================================================
;;
;; One computation, one persistent result, O(1) queries:
;;
;;   (compute-dom nodes entry succs) -> Dom
;;     nodes : pvector of nodes (any order; unreachable nodes are left out)
;;     succs : node -> pvector node
;;
;; Internally every node becomes its reverse-postorder number (a dense
;; fixnum; the entry is 0).  Immediate dominators come from the
;; Cooper-Harvey-Kennedy iteration over that numbering: O(n) per pass,
;; two or three passes on real control-flow graphs, no sets.  The
;; dominator tree is numbered by a DFS (dfs-in/dfs-out) so that
;; `dom-dominates?` is two integer comparisons, exactly like GCC's
;; bb_dom_dfs_in/out.  Dominance frontiers use Cooper's two-finger
;; walk; iterated frontiers a bitset worklist.
;;
;; The result is a prefab of pvectors and an immutable hash, i.e. a
;; datum (serializable with kernel/ir/gimple/datum.rkt).  The
;; computation uses local mutable vectors; nothing mutable escapes.
;;
;; Incremental update: `dom-fix` re-runs the CHK iteration only for
;; the nodes whose dominators can have changed after an edge change
;; (the forward closure of the nodes whose predecessors changed) while
;; the others keep their immediate dominators.  The RPO is recomputed
;; (an edge deletion can invalidate the old numbering), so the saving
;; is the iteration over unaffected nodes; see dom-test.rkt for the
;; measured ratio.  Node insertion/removal needs compute-dom.
;;
;; Post-dominance: `compute-post-dom` runs the same algorithm on the
;; reversed graph from the exit; nodes that cannot reach the exit
;; (infinite loops) are left out, as with GCC without fake edges.
;; ============================================================

(require racket/list racket/vector
         "../../kernel/data/data.rkt")

(provide (struct-out Dom)
         compute-dom compute-post-dom dom-fix
         dom-size dom-index dom-node dom-rpo dom-reachable?
         dom-idom dom-idom-node dom-children dom-depth dom-succs dom-preds
         dom-dominates? dom-strictly-dominates? dom-nearest-common-dominator
         dom-frontier dom-idf
         dom-children-nodes dom-frontier-nodes)

;; nodes    : pvector idx -> node, in RPO (idx 0 = entry)
;; index    : immutable hash node -> idx
;; succs    : pvector idx -> pvector idx (successor order preserved)
;; preds    : pvector idx -> pvector idx (edge creation order)
;; idom     : pvector idx -> idx (entry -> 0)
;; children : pvector idx -> pvector idx, ascending (= RPO order)
;; dfs-in, dfs-out : dominator-tree DFS interval
;; depth    : pvector idx -> depth in the dominator tree
;; frontier : pvector idx -> pvector idx, ascending
(struct Dom (nodes index succs preds idom children dfs-in dfs-out depth frontier) #:prefab)

;; ------------------------------------------------------------
;; Construction
;; ------------------------------------------------------------

;; reverse postorder, successors in order (pre_and_rev_post_order_compute)
(define (rpo-of entry succs)
  (define seen (make-hash))
  (define post '())
  (let visit ([n entry])
    (unless (hash-ref seen n #f)
      (hash-set! seen n #t)
      (for ([s (in-pvector (succs n))]) (visit s))
      (set! post (cons n post))))
  (list->vector post))

;; dense adjacency from the node vector
(define (adjacency nodes index succs)
  (define n (vector-length nodes))
  (define sv (make-vector n #f))
  (define pl (make-vector n '()))
  (for ([i (in-range n)])
    (define ss (for/list ([s (in-pvector (succs (vector-ref nodes i)))]) (hash-ref index s)))
    (vector-set! sv i (list->vector ss))
    (for ([j (in-list ss)]) (vector-set! pl j (cons i (vector-ref pl j)))))
  (values sv (for/vector ([l (in-vector pl)]) (list->vector (reverse l)))))

;; Cooper-Harvey-Kennedy: IDOM is a vector with -1 for "undefined"; only
;; the indices in WORK (ascending) are recomputed, the others are fixed.
(define (chk! idom preds work)
  (define (intersect a b)
    (let loop ([a a] [b b])
      (cond
        [(= a b) a]
        [(> a b) (loop (vector-ref idom a) b)]
        [else (loop a (vector-ref idom b))])))
  (let iterate ()
    (define changed #f)
    (for ([i (in-vector work)] #:unless (zero? i))
      (define new
        (for/fold ([acc -1]) ([p (in-vector (vector-ref preds i))])
          (cond
            [(= (vector-ref idom p) -1) acc]
            [(= acc -1) p]
            [else (intersect p acc)])))
      (unless (or (= new -1) (= new (vector-ref idom i)))
        (vector-set! idom i new)
        (set! changed #t)))
    (when changed (iterate))))

(define (finish-dom nodes index sv pv idom)
  (define n (vector-length nodes))
  ;; children (ascending because i ascends), depth
  (define ch (make-vector n '()))
  (for ([i (in-range (sub1 n) 0 -1)]) (vector-set! ch (vector-ref idom i) (cons i (vector-ref ch (vector-ref idom i)))))
  (define depth (make-vector n 0))
  (for ([i (in-range 1 n)]) (vector-set! depth i (add1 (vector-ref depth (vector-ref idom i)))))
  ;; dominator-tree DFS numbering
  (define din (make-vector n 0))
  (define dout (make-vector n 0))
  (define counter 0)
  (let visit ([i 0])
    (vector-set! din i counter) (set! counter (add1 counter))
    (for ([c (in-list (vector-ref ch i))]) (visit c))
    (vector-set! dout i counter) (set! counter (add1 counter)))
  ;; dominance frontiers (Cooper, Harvey, Kennedy).  GCC's ENTRY_BLOCK
  ;; gives the first block an extra predecessor; here the entry (idx 0)
  ;; has an implicit one, so a single back edge into it already counts.
  (define df (make-vector n '()))
  (for ([b (in-range n)] #:when (>= (+ (vector-length (vector-ref pv b)) (if (zero? b) 1 0)) 2))
    (define stop (if (zero? b) -1 (vector-ref idom b)))   ; the entry's dominator is the virtual ENTRY
    (for ([p (in-vector (vector-ref pv b))])
      (let runner ([r p])
        (unless (= r stop)
          (unless (and (pair? (vector-ref df r)) (= (car (vector-ref df r)) b))
            (vector-set! df r (cons b (vector-ref df r))))
          (unless (zero? r) (runner (vector-ref idom r)))))))
  (define (freeze v) (for/pvector ([x (in-vector v)]) x))
  (define (freeze-vv v) (for/pvector ([x (in-vector v)]) (for/pvector ([y (in-vector x)]) y)))
  (define (freeze-lists v) (for/pvector ([l (in-vector v)]) (list->pvector (sort (remove-duplicates l) <))))
  (Dom (freeze nodes) index (freeze-vv sv) (freeze-vv pv) (freeze idom) (freeze-lists ch)
       (freeze din) (freeze dout) (freeze depth) (freeze-lists df)))

(define (compute-dom nodes entry succs)
  (define order (rpo-of entry succs))
  (define index (for/hash ([x (in-vector order)] [i (in-naturals)]) (values x i)))
  (define-values (sv pv) (adjacency order index succs))
  (define idom (make-vector (vector-length order) -1))
  (vector-set! idom 0 0)
  (chk! idom pv (for/vector ([i (in-range (vector-length order))]) i))
  (finish-dom order index sv pv idom))

;; post-dominators: the same on the reversed graph from EXIT
(define (compute-post-dom nodes exit succs)
  (define rev (make-hash))
  (for* ([n (in-pvector nodes)] [s (in-pvector (succs n))])
    (hash-set! rev s (cons n (hash-ref rev s '()))))
  (compute-dom nodes exit (lambda (n) (list->pvector (reverse (hash-ref rev n '()))))))

;; after an edge change: CHANGED are the nodes whose predecessor set
;; changed; SUCCS is the new successor function over the same node set
(define (dom-fix d changed succs)
  (define entry (pvector-ref (Dom-nodes d) 0))
  (define order (rpo-of entry succs))
  (define index (for/hash ([x (in-vector order)] [i (in-naturals)]) (values x i)))
  (unless (= (vector-length order) (dom-size d))
    (error 'dom-fix "node set changed (~a -> ~a reachable); use compute-dom" (dom-size d) (vector-length order)))
  (define-values (sv pv) (adjacency order index succs))
  ;; affected = forward closure of CHANGED in the new graph
  (define n (vector-length order))
  (define affected (make-vector n #f))
  (let visit ([stack (for/list ([c (in-pvector changed)]) (hash-ref index c))])
    (unless (null? stack)
      (define i (car stack))
      (cond
        [(vector-ref affected i) (visit (cdr stack))]
        [else (vector-set! affected i #t)
              (visit (append (vector->list (vector-ref sv i)) (cdr stack)))])))
  ;; idoms of unaffected nodes carry over (translated to the new numbering)
  (define idom (make-vector n -1))
  (for ([i (in-range n)] #:unless (vector-ref affected i))
    (define old-i (hash-ref (Dom-index d) (vector-ref order i)))
    (vector-set! idom i (hash-ref index (pvector-ref (Dom-nodes d) (pvector-ref (Dom-idom d) old-i)))))
  (vector-set! idom 0 0)
  (chk! idom pv (for/vector ([i (in-range n)] #:when (vector-ref affected i)) i))
  (finish-dom order index sv pv idom))

;; ------------------------------------------------------------
;; Queries
;; ------------------------------------------------------------

(define (dom-size d) (pvector-length (Dom-nodes d)))
(define (dom-index d node) (hash-ref (Dom-index d) node #f))
(define (dom-node d i) (pvector-ref (Dom-nodes d) i))
(define (dom-rpo d) (Dom-nodes d))
(define (dom-reachable? d node) (hash-has-key? (Dom-index d) node))
(define (dom-idom d i) (pvector-ref (Dom-idom d) i))
(define (dom-idom-node d node)
  (define i (dom-index d node))
  (and i (> i 0) (dom-node d (dom-idom d i))))
(define (dom-children d i) (pvector-ref (Dom-children d) i))
(define (dom-depth d i) (pvector-ref (Dom-depth d) i))
(define (dom-succs d i) (pvector-ref (Dom-succs d) i))
(define (dom-preds d i) (pvector-ref (Dom-preds d) i))
(define (dom-frontier d i) (pvector-ref (Dom-frontier d) i))

;; a dominates b (indices), reflexive
(define (dom-dominates? d a b)
  (and (<= (pvector-ref (Dom-dfs-in d) a) (pvector-ref (Dom-dfs-in d) b))
       (<= (pvector-ref (Dom-dfs-out d) b) (pvector-ref (Dom-dfs-out d) a))))
(define (dom-strictly-dominates? d a b) (and (not (= a b)) (dom-dominates? d a b)))

(define (dom-nearest-common-dominator d a b)
  (let loop ([a a] [b b])
    (cond
      [(= a b) a]
      [(> (dom-depth d a) (dom-depth d b)) (loop (dom-idom d a) b)]
      [else (loop a (dom-idom d b))])))

;; iterated dominance frontier of a set of indices (intbits) -> intbits
(define (dom-idf d set)
  (let loop ([work (intbits->list set)] [in-work set] [result intbits-empty])
    (cond
      [(null? work) result]
      [else
       (define-values (work* in-work* result*)
         (for/fold ([w (cdr work)] [iw in-work] [r result]) ([f (in-pvector (dom-frontier d (car work)))])
           (cond
             [(intbits-ref r f) (values w iw r)]
             [else (values (if (intbits-ref iw f) w (cons f w)) (intbits-set iw f) (intbits-set r f))])))
       (loop work* in-work* result*)])))

;; node-level conveniences
(define (dom-children-nodes d node)
  (for/pvector ([c (in-pvector (dom-children d (dom-index d node)))]) (dom-node d c)))
(define (dom-frontier-nodes d node)
  (for/pvector ([c (in-pvector (dom-frontier d (dom-index d node)))]) (dom-node d c)))
