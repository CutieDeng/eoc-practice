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
;;   1. dominance information from driver/dominance/dom.rkt (RPO by a
;;      successor-order DFS, CHK immediate dominators, dominator-tree
;;      DFS numbers, Cooper frontiers);
;;   2. per block: killed variables and upward-exposed uses
;;      (mark_def_sites);
;;   3. per variable, in `variables` order: iterated dominance frontier
;;      of its def blocks, optionally pruned to the blocks where the
;;      variable is live on entry (prune_unused_phi_nodes), PHIs
;;      inserted in ascending block-key order (GCC: bitmap order of
;;      basic block indices);
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
;;   block-key    : bid -> exact integer     PHI insertion order within a variable
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
         "../dominance/dom.rkt")

(provide (struct-out ssa-port) ssa-construct)

(struct ssa-port
  (entry blocks succs preds block-key variables var-key block-summary entry-vars
   new-name insert-phi phis set-phi-result set-phi-arg rename-stmts finish
   prune? child-order phi-names))

(define (ssa-construct p a0)
  (define (succs a b) ((ssa-port-succs p) a b))
  (define bkey (ssa-port-block-key p))
  (define vkey (ssa-port-var-key p))
  (define (new-name a v kind) ((ssa-port-new-name p) a v kind))

  ;; 1. dominance over the port's graph; blocks are indices from here on
  (define root ((ssa-port-entry p) a0))
  (define d (compute-dom ((ssa-port-blocks p) a0) root (lambda (b) (succs a0 b))))
  (define n (dom-size d))
  (define (bid i) (dom-node d i))
  (define (idx b) (dom-index d b))
  (define children
    (case (ssa-port-child-order p)
      [(rpo) (lambda (i) (dom-children d i))]
      [else
       (define pos (for/hash ([b (in-pvector ((ssa-port-blocks p) a0))] [k (in-naturals)]) (values b k)))
       (lambda (i) (list->pvector (sort (pvector->list (dom-children d i)) < #:key (lambda (c) (hash-ref pos (bid c))))))]))

  ;; 2. def blocks and upward-exposed uses per variable (intbits of indices)
  (define entry-vars ((ssa-port-entry-vars p) a0))
  (define-values (defs-of uses-of)
    (for/fold ([defs (for/fold ([m intmap-empty]) ([v (in-pvector entry-vars)])
                       (intmap-set m (vkey v) (intbits-set intbits-empty 0)))]
               [uses intmap-empty]
               #:result (values defs uses))
              ([i (in-range n)])
      (define-values (kills ups) ((ssa-port-block-summary p) a0 (bid i)))
      (values (for/fold ([m defs]) ([v (in-pvector kills)])
                (intmap-set m (vkey v) (intbits-set (intmap-ref m (vkey v) intbits-empty) i)))
              (for/fold ([m uses]) ([v (in-pvector ups)])
                (intmap-set m (vkey v) (intbits-set (intmap-ref m (vkey v) intbits-empty) i))))))

  ;; indices where V is live on entry: backward from upward-exposed uses,
  ;; stopping at blocks that define V
  (define (live-in kills uses)
    (let loop ([work (intbits->list uses)] [live uses])
      (cond
        [(null? work) live]
        [else
         (define-values (work* live*)
           (for/fold ([w (cdr work)] [l live]) ([q (in-pvector (dom-preds d (car work)))])
             (if (or (intbits-ref l q) (intbits-ref kills q)) (values w l) (values (cons q w) (intbits-set l q)))))
         (loop work* live*)])))

  ;; 3. PHI insertion, variables in port order, blocks by ascending block key
  (define a1
    (for/fold ([a a0]) ([v (in-pvector ((ssa-port-variables p) a0))])
      (define kills (intmap-ref defs-of (vkey v) #f))
      (cond
        [(not kills) a]
        [else
         (define cands (dom-idf d kills))
         (define places
           (if (ssa-port-prune? p) (intbits-intersect cands (live-in kills (intmap-ref uses-of (vkey v) intbits-empty))) cands))
         (for/fold ([a a]) ([i (in-list (sort (intbits->list places) < #:key (lambda (i) (bkey (bid i)))))])
           (define b (bid i))
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
      [(intmap-ref (cdr acc) (vkey v) #f) => (lambda (dd) (values dd a acc))]
      [else
       (let-values ([(dd a) (new-name a v 'default)])
         (values dd a (cons (car acc) (intmap-set (cdr acc) (vkey v) dd))))]))
  (define (def a acc v)
    (let-values ([(name a) (new-name a v 'def)])
      (values name a (cons (push (car acc) v name) (cdr acc)))))

  (define (rename-block a acc i)
    (define b (bid i))
    ;; PHI results
    (define-values (a1 acc1)
      (for/fold ([a a] [acc acc]) ([pr (in-pvector ((ssa-port-phis p) a b))])
        (define v (car pr))
        (define-values (name a*)
          (cond
            [(cdr pr) (values (cdr pr) a)]
            [else (let-values ([(nm a) (new-name a v 'phi)]) (values nm ((ssa-port-set-phi-result p) a b v nm)))]))
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
              ([c (in-pvector (children i))])
      (define-values (a* acc*) (rename-block a (cons (car acc3) (cdr acc)) c))
      (values a* (cons (car acc3) (cdr acc*)))))

  ;; entry definitions
  (define-values (a2 acc0)
    (for/fold ([a a1] [stacks intmap-empty] #:result (values a (cons stacks intmap-empty)))
              ([v (in-pvector entry-vars)])
      (define-values (name a*) (new-name a v 'entry))
      (values a* (push stacks v name))))
  (define-values (a3 _acc) (rename-block a2 acc0 0))
  ((ssa-port-finish p) a3))
