#lang racket/base

;; ============================================================
;; Driver: SSA construction over a port (component pattern)
;; ============================================================
;;
;; The algorithm never touches an IR.  Everything it needs is supplied
;; by an `ssa-port`: a record of closures over an opaque adapter value
;; `a` that the port threads through the whole run (a persistent IR
;; adapter returns a new `a`; a mutable one may return the same `a`).
;;
;; Algorithm (Cytron et al., as gcc/tree-into-ssa.cc does it):
;;   1. reverse postorder from the entry, following successors in
;;      order (pre_and_rev_post_order_compute); dominators, dominance
;;      frontiers and the dominator tree from the driver dominance
;;      module (which needs RPO);
;;   2. per block: killed variables and upward-exposed uses
;;      (mark_def_sites);
;;   3. per variable, in `variables` order: iterated dominance frontier
;;      of its def blocks, optionally pruned to the blocks where the
;;      variable is live on entry (prune_unused_phi_nodes), PHIs
;;      inserted in ascending block-key order;
;;   4. rename by a dominator-tree walk: PHI results, then the block's
;;      statements (uses first, then defs, through the port's
;;      rename-stmts callback), then the PHI arguments of every
;;      successor, then the dominator children.
;;
;; Policies that differ between real SSA builders are port fields, so
;; the Java builder keeps its exact output and the GIMPLE builder
;; matches GCC:
;;   prune?      #t: pruned SSA (GCC); #f: PHIs at the whole IDF (Java)
;;   child-order 'rpo (GCC's dom_walker) or 'blocks (the port's block
;;               order, Java)
;;   phi-names   'insert: PHI results are named when inserted (GCC);
;;               'rename: when the renamer reaches the block (Java)
;;   entry-vars  variables defined at the entry before anything else
;;               (Java's phantom parameter defs); a use with no reaching
;;               definition otherwise asks the port for a default
;;               definition (GCC's x_N(D)), created once per variable.
;;
;; Port closures (A is the adapter value):
;;   entry        : A -> bid
;;   blocks       : A -> pvector bid         all blocks; also the 'blocks order
;;   succs, preds : A bid -> pvector bid
;;   block-key    : bid -> exact integer
;;   variables    : A -> pvector var         candidates, in PHI-insertion order
;;   var-key      : var -> exact integer
;;   block-summary: A bid -> (values kills upward-uses)   pvectors of var
;;   entry-vars   : A -> pvector var
;;   new-name     : A var kind -> (values name A)   kind: 'def 'phi 'default 'entry
;;   insert-phi   : A bid var name-or-#f -> A
;;   phis         : A bid -> pvector (cons var name-or-#f)
;;   set-phi-result : A bid var name -> A
;;   set-phi-arg  : A bid var pred-bid name -> A
;;   rename-stmts : A bid acc use def -> (values A acc)
;;                  use : A acc var -> (values name A acc)
;;                  def : A acc var -> (values name A acc)
;;                  the port threads (A, acc) through its statements in
;;                  order, calling use for every use before def for the
;;                  statement's definitions; acc is opaque to the port
;;   finish       : A -> result
;; ============================================================

(require racket/list
         "../../kernel/data/data.rkt"
         "../dominance/dominator.rkt")

(provide (struct-out ssa-port) ssa-construct compute-rpo)

(struct ssa-port
  (entry blocks succs preds block-key variables var-key block-summary entry-vars
   new-name insert-phi phis set-phi-result set-phi-arg rename-stmts finish
   prune? child-order phi-names))

;; ------------------------------------------------------------
;; Reverse postorder (recursive DFS, successors in order)
;; ------------------------------------------------------------

(define (compute-rpo entry succs block-key)
  (define-values (post _seen)
    (let visit ([b entry] [post '()] [seen intbits-empty])
      (cond
        [(intbits-ref seen (block-key b)) (values post seen)]
        [else
         (define-values (post1 seen1)
           (for/fold ([post post] [seen (intbits-set seen (block-key b))]) ([s (in-pvector (succs b))])
             (visit s post seen)))
         (values (cons b post1) seen1)])))
  (list->pvector post))

;; ------------------------------------------------------------
;; Construction
;; ------------------------------------------------------------

(define (ssa-construct p a0)
  (define (entry a) ((ssa-port-entry p) a))
  (define (succs a b) ((ssa-port-succs p) a b))
  (define (preds a b) ((ssa-port-preds p) a b))
  (define bkey (ssa-port-block-key p))
  (define vkey (ssa-port-var-key p))
  (define (new-name a v kind) ((ssa-port-new-name p) a v kind))

  ;; 1. order and dominance (RPO is what the dominance driver requires)
  (define root (entry a0))
  (define rpo (compute-rpo root (lambda (b) (succs a0 b)) bkey))
  (define reachable (for/fold ([s intbits-empty]) ([b (in-pvector rpo)]) (intbits-set s (bkey b))))
  (define (preds-fn b) (preds a0 b))
  (define idom (compute-idom rpo root preds-fn))
  (define df (compute-dominance-frontier rpo root preds-fn #:idom idom))
  (define child-order
    (case (ssa-port-child-order p)
      [(rpo) rpo]
      [else (for/pvector ([b (in-pvector ((ssa-port-blocks p) a0))] #:when (intbits-ref reachable (bkey b))) b)]))
  (define dom-tree (compute-dominator-tree child-order idom))

  ;; 2. def blocks and upward-exposed uses per variable (intbits of block keys)
  (define entry-vars ((ssa-port-entry-vars p) a0))
  (define-values (defs-of uses-of)
    (for/fold ([defs (for/fold ([m intmap-empty]) ([v (in-pvector entry-vars)])
                       (intmap-set m (vkey v) (intbits-set intbits-empty (bkey root))))]
               [uses intmap-empty]
               #:result (values defs uses))
              ([b (in-pvector rpo)])
      (define-values (kills ups) ((ssa-port-block-summary p) a0 b))
      (values (for/fold ([m defs]) ([v (in-pvector kills)])
                (intmap-set m (vkey v) (intbits-set (intmap-ref m (vkey v) intbits-empty) (bkey b))))
              (for/fold ([m uses]) ([v (in-pvector ups)])
                (intmap-set m (vkey v) (intbits-set (intmap-ref m (vkey v) intbits-empty) (bkey b)))))))
  (define key->block (for/fold ([m intmap-empty]) ([b (in-pvector rpo)]) (intmap-set m (bkey b) b)))
  (define (block-of k) (intmap-ref key->block k))

  ;; iterated dominance frontier of a set of block keys
  (define (idf def-keys)
    (let loop ([work (intbits->list def-keys)] [in-work def-keys] [result intbits-empty])
      (cond
        [(null? work) result]
        [else
         (define b (block-of (car work)))
         (define-values (work* in-work* result*)
           (for/fold ([w (cdr work)] [iw in-work] [r result]) ([d (in-pvector (hash-ref df b (pvector-empty)))])
             (define k (bkey d))
             (cond
               [(intbits-ref r k) (values w iw r)]
               [else (values (if (intbits-ref iw k) w (cons k w)) (intbits-set iw k) (intbits-set r k))])))
         (loop work* in-work* result*)])))

  ;; blocks where V is live on entry: backward from upward-exposed uses,
  ;; stopping at blocks that define V
  (define (live-in-keys kills uses)
    (let loop ([work (intbits->list uses)] [live uses])
      (cond
        [(null? work) live]
        [else
         (define-values (work* live*)
           (for/fold ([w (cdr work)] [l live]) ([pb (in-pvector (preds a0 (block-of (car work))))])
             (define k (bkey pb))
             (if (or (not (intbits-ref reachable k)) (intbits-ref l k) (intbits-ref kills k))
                 (values w l)
                 (values (cons k w) (intbits-set l k)))))
         (loop work* live*)])))

  ;; 3. PHI insertion, variables in port order, blocks in ascending key order
  (define a1
    (for/fold ([a a0]) ([v (in-pvector ((ssa-port-variables p) a0))])
      (define kills (intmap-ref defs-of (vkey v) #f))
      (cond
        [(not kills) a]
        [else
         (define cands (idf kills))
         (define uses (intmap-ref uses-of (vkey v) intbits-empty))
         (define places
           (if (ssa-port-prune? p) (intbits-intersect cands (live-in-keys kills uses)) cands))
         (for/fold ([a a]) ([k (in-list (intbits->list places))])
           (define b (block-of k))
           (cond
             [(eq? (ssa-port-phi-names p) 'insert)
              (let-values ([(name a) (new-name a v 'phi)]) ((ssa-port-insert-phi p) a b v name))]
             [else ((ssa-port-insert-phi p) a b v #f)]))])))

  ;; 4. renaming.  acc = (cons stacks defaults): stacks is an intmap
  ;; var-key -> list of names (top first), defaults an intmap var-key ->
  ;; default-definition name (global, created on demand).
  (define (top stacks v) (let ([s (intmap-ref stacks (vkey v) '())]) (and (pair? s) (car s))))
  (define (push stacks v name) (intmap-set stacks (vkey v) (cons name (intmap-ref stacks (vkey v) '()))))
  (define (use a acc v)
    (define cur (top (car acc) v))
    (cond
      [cur (values cur a acc)]
      [(intmap-ref (cdr acc) (vkey v) #f) => (lambda (d) (values d a acc))]
      [else
       (let-values ([(d a) (new-name a v 'default)])
         (values d a (cons (car acc) (intmap-set (cdr acc) (vkey v) d))))]))
  (define (def a acc v)
    (let-values ([(name a) (new-name a v 'def)])
      (values name a (cons (push (car acc) v name) (cdr acc)))))

  (define (rename-block a acc b)
    ;; PHI results
    (define-values (a1 acc1)
      (for/fold ([a a] [acc acc]) ([pr (in-pvector ((ssa-port-phis p) a b))])
        (define v (car pr))
        (define-values (name a*)
          (cond
            [(cdr pr) (values (cdr pr) a)]
            [else (let-values ([(n a) (new-name a v 'phi)]) (values n ((ssa-port-set-phi-result p) a b v n)))]))
        (values a* (cons (push (car acc) v name) (cdr acc)))))
    ;; statements
    (define-values (a2 acc2) ((ssa-port-rename-stmts p) a1 b acc1 use def))
    ;; PHI arguments of the successors
    (define-values (a3 acc3)
      (for*/fold ([a a2] [acc acc2]) ([s (in-pvector (succs a2 b))] [pr (in-pvector ((ssa-port-phis p) a s))])
        (define-values (name a* acc*) (use a acc (car pr)))
        (values ((ssa-port-set-phi-arg p) a* s (car pr) b name) acc*)))
    ;; dominator children: each sees this block's stacks; defaults persist
    (for/fold ([a a3] [acc acc3] #:result (values a (cons (car acc3) (cdr acc))))
              ([c (in-pvector (hash-ref dom-tree b (pvector-empty)))])
      (define-values (a* acc*) (rename-block a (cons (car acc3) (cdr acc)) c))
      (values a* (cons (car acc3) (cdr acc*)))))

  ;; entry definitions
  (define-values (a2 acc0)
    (for/fold ([a a1] [stacks intmap-empty] #:result (values a (cons stacks intmap-empty)))
              ([v (in-pvector entry-vars)])
      (define-values (name a*) (new-name a v 'entry))
      (values a* (push stacks v name))))
  (define-values (a3 _acc) (rename-block a2 acc0 root))
  ((ssa-port-finish p) a3))
