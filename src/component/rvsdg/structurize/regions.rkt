#lang racket/base

;; ============================================================
;; CFG -> RVSDG structurizer: sub-region construction and context (var) collection
;; ============================================================
;; Split out of structurize.rkt (roadmap Phase 3.1); see that file's
;; header for the algorithm.  The CFG is read through port.rkt.
;; ============================================================

(require racket/match
         racket/list
         "../../../kernel/ir/cfg/types.rkt"
         "../../../kernel/ir/rvsdg/rvsdg.rkt"
         "../utils/builder.rkt"
         "../port.rkt"
         "../../../kernel/data/data.rkt"
         "ctx.rkt"
         "reach.rkt"
         "sinks.rkt")

(provide collect-arm-ctx build-exit-arm-region add-entry-phi-srcs-to-ctx build-noop-arm-region build-branch-region collect-gamma-ctx collect-theta-ctx pick-phi-source)


;; Walk every block in the arm's reach-set and collect parent-scope
;; VarIds read by any VfInsn input or by the block's terminator
;; (Term:ret values, Term:throw exception, Term:cond predicate),
;; excluding VarIds locally defined anywhere in the arm (union of phi
;; outputs + VfInsn outputs across the reach-set).  Returns an
;; idmap used as a set of VarId -> #t.
(define (collect-arm-ctx cfg parent-var->out reach-set)
  (define locals
    (for/fold ([s idmap-empty])
              ([kv (in-idmap reach-set)])
      (define blk (cv-block cfg (car kv)))
      (define s1
        (for/fold ([s s]) ([phi (in-pvector (CfgBlock-phis blk))])
          (idmap-set s (PhiInsn-output phi) #t)))
      (for/fold ([s s1]) ([insn (in-pvector (CfgBlock-insns blk))])
        (for/fold ([s s]) ([o (in-pvector (VfInsn-outputs insn))]
                           #:when (VarId? o))
          (idmap-set s o #t)))))
  (define (consider x acc)
    (cond
      [(and (VarId? x)
            (not (idmap-ref locals x #f))
            (idmap-ref parent-var->out x #f))
       (idmap-set acc x #t)]
      [else acc]))
  (for/fold ([acc idmap-empty])
            ([kv (in-idmap reach-set)])
    (define blk (cv-block cfg (car kv)))
    (define acc1
      (for/fold ([a acc]) ([insn (in-pvector (CfgBlock-insns blk))])
        (for/fold ([a a]) ([x (in-pvector (VfInsn-inputs insn))])
          (consider x a))))
    (define term-reads
      (match (CfgBlock-terminator blk)
        [(Term:ret vs) vs]
        [(Term:throw v) (pvector v)]
        [(Term:cond p _ _) (pvector p)]
        [_ (pvector-empty)]))
    (for/fold ([a acc1]) ([x (in-pvector term-reads)])
      (consider x a))))


;; Build a sub-region for an exit arm.  Works for single-block arms
;; (reach-set of size 1, entry block ends in Term:ret / Term:throw)
;; and for multi-block arms (reach-set contains Term:jump / Term:cond
;; internal blocks in addition to ret / throw leaves).  Calls
;; translate-segment with stop-bid=#f so the arm's entire control
;; flow lowers inside the sub-region; the returned payload selects
;; the sink to install.
;;
;; If the entry block carries phis, `pred-bid` names the dispatching
;; block (the cond-bid / switch-bid the arm branches from).  For each
;; phi, we look up its source VarId at pred-bid via pick-phi-source and
;; pre-bind the phi output to the same oid as that source VarId.
;; Callers must include those source VarIds in ctx-vars so the binding
;; resolves against the region-arg output map.
(define (build-exit-arm-region cfg branch-bid pred-bid ctx-vars which)
  (define n-ctx (pvector-length ctx-vars))
  (define entry-blk (cv-block cfg branch-bid))
  (define entry-phis (CfgBlock-phis entry-blk))
  (when (and (> (pvector-length entry-phis) 0) (not pred-bid))
    (error 'build-exit-arm-region
           "exit arm ~a (~a) entry block has phis but no pred-bid"
           branch-bid which))
  (define sub0 (region-empty))
  (define-values (sub1 _arg-nid _arg-ins arg-outs)
    (region-add-node sub0 (Simple (list 'region-arg n-ctx)) 0 n-ctx))
  (define sub-var->out-base
    (for/fold ([m idmap-empty])
              ([v (in-pvector ctx-vars)]
               [oid (in-pvector arg-outs)])
      (idmap-set m v oid)))
  (define sub-var->out
    (for/fold ([m sub-var->out-base])
              ([phi (in-pvector entry-phis)])
      (define src-var (pick-phi-source phi pred-bid which))
      (define src-oid
        (or (idmap-ref sub-var->out-base src-var #f)
            (error 'build-exit-arm-region
                   "exit arm ~a (~a) entry phi ~a source ~a missing from ctx"
                   branch-bid which (PhiInsn-output phi) src-var)))
      (idmap-set m (PhiInsn-output phi) src-oid)))
  (define-values (sub2 sub-var->out2 payload)
    (translate-segment cfg branch-bid #f sub1 sub-var->out))
  (match payload
    [(cons 'ret vs)      (install-return sub2 sub-var->out2 vs)]
    [(cons 'throw v)     (install-throw  sub2 sub-var->out2 v)]
    ['already-terminated sub2]
    [other (error 'build-exit-arm-region
                  "exit arm ~a (~a) did not terminate with ret/throw: payload=~s"
                  branch-bid which other)]))


;; Merge the phi sources at `pred-bid` for every phi on the entry
;; block of `branch-bid` into `ctx-set`.  Only the source VarIds that
;; exist in `parent-var->out` are added — this mirrors
;; collect-arm-ctx, which also filters against the parent scope.
(define (add-entry-phi-srcs-to-ctx cfg ctx-set branch-bid pred-bid
                                   parent-var->out which)
  (define phis (CfgBlock-phis (cv-block cfg branch-bid)))
  (for/fold ([s ctx-set])
            ([phi (in-pvector phis)])
    (define src (pick-phi-source phi pred-bid which))
    (cond
      [(and (VarId? src) (idmap-ref parent-var->out src #f))
       (idmap-set s src #t)]
      [else s])))


;; ============================================================
;; Asymmetric early-exit Gamma (one arm exits, other continues)
;; ============================================================
;;
;; Scope: exactly one of (then-bid, else-bid) is a single block ending
;; in Term:ret / Term:throw.  The other branch's first block is the
;; continuation point that translate-segment resumes from in the
;; outer region (after this Gamma falls through).
;;
;; The Gamma has 0 outputs.  Two sub-regions:
;;   - exit arm: built by build-terminal-arm-region (return/throw
;;     installed inside).
;;   - continue arm: a no-op region with just region-arg(0, n-ctx)
;;     and region-result(0, 0).
;; Sub-regions are placed in [then, else] order so that the raw
;; predicate (truthy → region 0) selects the same arm bytecode took
;; on the original Term:cond.
;;
;; The continuation arm runs in the outer region; its parent-scope
;; reads remain in the outer var->out (unchanged), so we do not seed
;; ctx-vars from it -- only the exit arm's reads need to be
;; threaded into the Gamma as context inputs.

(define (build-noop-arm-region n-ctx)
  (define sub0 (region-empty))
  (define-values (sub1 _arg-nid _arg-ins _arg-outs)
    (region-add-node sub0 (Simple (list 'region-arg n-ctx)) 0 n-ctx))
  (define-values (sub2 _res-nid _res-ins _res-outs)
    (region-add-node sub1 (Simple (list 'region-result 0)) 0 0))
  sub2)


;; ============================================================
;; Sub-region builder
;; ============================================================

(define (build-branch-region cfg branch-bid branch-pred join-bid ctx-vars phis which)
  (define n-ctx (pvector-length ctx-vars))
  (define n-phis (pvector-length phis))

  (define sub0 (region-empty))

  ;; Entry "region-arg" producer -- outputs mirror ctx-vars.
  (define-values (sub1 _arg-nid _arg-ins arg-outs)
    (region-add-node sub0 (Simple (list 'region-arg n-ctx)) 0 n-ctx))

  ;; Seed sub var→out.
  (define sub-var->out
    (for/fold ([m idmap-empty])
              ([v (in-pvector ctx-vars)]
               [oid (in-pvector arg-outs)])
      (idmap-set m v oid)))

  ;; Translate the arm's chain of blocks.  translate-segment walks
  ;; from branch-bid through Term:jump edges (and inner Gamma /
  ;; Theta nodes) until it reaches join-bid.  The active loop map
  ;; rides on `current-theta-ctxs`, so any loop header encountered
  ;; inside this arm is lowered into a Theta living within this
  ;; sub-region.
  (define-values (sub2 sub-var->out2 payload)
    (translate-segment cfg branch-bid join-bid sub1 sub-var->out))
  (unless (eq? payload #f)
    (error 'build-branch-region
           "arm ~a reached non-join terminator (payload=~s) before join ~a"
           which payload join-bid))

  ;; For each phi at the join, look up the source var coming from this
  ;; arm's immediate predecessor of the join (= last block in the
  ;; arm's chain).
  (define result-src-vars
    (for/pvector ([phi (in-pvector phis)])
      (pick-phi-source phi branch-pred which)))

  ;; Allocate the `region-result` sink with n-phis inputs.
  (define-values (sub3 _res-nid res-ins _res-outs)
    (region-add-node sub2 (Simple (list 'region-result n-phis)) n-phis 0))

  ;; Wire each result input from the branch-local var→out.
  (for/fold ([r sub3])
            ([src-var (in-pvector result-src-vars)]
             [iid (in-pvector res-ins)])
    (define src-oid
      (or (idmap-ref sub-var->out2 src-var #f)
          (error 'build-branch-region
                 "phi source ~a undefined in ~a sub-region"
                 src-var which)))
    (define-values (r* _w) (region-add-wire r src-oid iid))
    r*))


;; Collect parent-scope VarIds actually needed by either Gamma arm.
;; Each arm is described by the set of BlockIds reachable from the
;; arm entry up to (but not including) the join -- including blocks
;; inside any nested Gammas within the arm.  A var counts as "needed"
;; by the arm iff some VfInsn on any reachable block reads it, that
;; var is not redefined by any VfInsn output or phi output within the
;; arm, and the var is present in parent-var->out.  Also folds in the
;; phi source contributed by the arm's predecessor at the outer join.
;; Returns an idmap used as a set of VarId -> #t.
(define (collect-gamma-ctx cfg parent-var->out
                           then-blocks then-pred
                           else-blocks else-pred phis)
  ;; Union of all locally-defined VarIds across every block in the arm
  ;; (phi outputs + VfInsn outputs).  Safe over-approximation: SSA
  ;; scoping already ensures outer-parent reads go through phi outputs
  ;; at the outer join, not arm-internal defs.
  (define (arm-local-defs blocks)
    (for/fold ([s idmap-empty])
              ([kv (in-idmap blocks)])
      (define blk (cv-block cfg (car kv)))
      (define s*
        (for/fold ([s s]) ([phi (in-pvector (CfgBlock-phis blk))])
          (idmap-set s (PhiInsn-output phi) #t)))
      (for/fold ([s s*]) ([insn (in-pvector (CfgBlock-insns blk))])
        (for/fold ([s s]) ([o (in-pvector (VfInsn-outputs insn))]
                           #:when (VarId? o))
          (idmap-set s o #t)))))
  (define (walk-arm blocks arm-pred acc)
    (define locals (arm-local-defs blocks))
    (define acc*
      (for/fold ([a acc])
                ([kv (in-idmap blocks)])
        (define insns (CfgBlock-insns (cv-block cfg (car kv))))
        (for/fold ([a a]) ([insn (in-pvector insns)])
          (for/fold ([a a]) ([x (in-pvector (VfInsn-inputs insn))])
            (cond
              [(and (VarId? x)
                    (not (idmap-ref locals x #f))
                    (idmap-ref parent-var->out x #f))
               (idmap-set a x #t)]
              [else a])))))
    ;; Phi source contributed at the outer join from this arm's pred.
    (for/fold ([a acc*]) ([phi (in-pvector phis)])
      (define src (pick-phi-source phi arm-pred 'gamma-ctx))
      (cond
        [(and (VarId? src)
              (idmap-ref parent-var->out src #f))
         (idmap-set a src #t)]
        [else a])))
  (walk-arm else-blocks else-pred
            (walk-arm then-blocks then-pred
                      idmap-empty)))


;; Collect parent-scope VarIds actually read inside the loop.  Walks
;; the header block and every block on the body chain (body-entry →
;; ... → latch), tracking locally-defined outputs per block (seeded
;; with header phi outputs, which are local to the sub-region).  Any
;; VfInsn input that is a VarId, not locally-defined in the block's
;; prefix, and present in `parent-var->out` counts as a read.
;; `body-blocks` is an ordered-set of BlockIds (as produced by
;; `theta-body-blocks`); a single-block body degenerates to just the
;; latch entry.  Returns an idmap used as a set of VarId -> #t.
(define (collect-theta-ctx cfg parent-var->out header-bid body-blocks phis)
  (define phi-out-set
    (for/fold ([m idmap-empty])
              ([p (in-pvector phis)])
      (idmap-set m (PhiInsn-output p) #t)))
  (define (walk-block bid acc)
    (define blk (cv-block cfg bid))
    (define insns (CfgBlock-insns blk))
    (define-values (_ acc*)
      (for/fold ([locals phi-out-set]
                 [a acc])
                ([insn (in-pvector insns)])
        (define a*
          (for/fold ([a a]) ([x (in-pvector (VfInsn-inputs insn))])
            (cond
              [(and (VarId? x)
                    (not (idmap-ref locals x #f))
                    (idmap-ref parent-var->out x #f))
               (idmap-set a x #t)]
              [else a])))
        (define locals*
          (for/fold ([s locals]) ([o (in-pvector (VfInsn-outputs insn))]
                                  #:when (VarId? o))
            (idmap-set s o #t)))
        (values locals* a*)))
    acc*)
  (for/fold ([acc (walk-block header-bid
                              idmap-empty)])
            ([kv (in-idmap body-blocks)])
    (walk-block (car kv) acc)))


;; Find the VarId contributed by `branch-bid` to a phi's sources.
(define (pick-phi-source phi branch-bid which)
  (define srcs (PhiInsn-sources phi))
  (or (for/or ([s (in-pvector srcs)])
        (and (equal? (car s) branch-bid) (cdr s)))
      (error 'pick-phi-source
             "phi at ~a has no source from ~a branch (block ~a); sources=~s"
             (PhiInsn-output phi) which branch-bid
             (for/list ([s (in-pvector srcs)]) s))))
