#lang racket/base

;; ============================================================
;; CFG -> RVSDG structurizer: natural loops -> Theta
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
         "regions.rkt"
         "insn.rkt")

(provide translate-theta-two-latch-body translate-theta)


;; ============================================================
;; Multi-latch body: 2-arm diamond
;; ============================================================
;;
;; Handles the common multi-latch pattern where two `continue`-like
;; paths share the same loop header:
;;
;;     while (c) { ...; if (x) {...; continue;} else {...; continue;} }
;;
;; Structural requirement: walking body-entry-bid's Term:jump chain
;; reaches a block whose Term:cond fans out to two distinct latches,
;; each arm's scoped reach containing exactly one latch, the two
;; reach-sets disjoint, and every boundary terminator closing back
;; to `header-bid`.  Each arm becomes a sub-region of a Gamma whose
;; outputs are the per-phi merged values; after the Gamma the body
;; sub-var->out maps every header-phi-output VarId to the
;; corresponding Gamma output.  Any other 2-latch shape (e.g. an
;; arm ending in ret / throw, or more than two latches) is left to
;; specialised paths (asymmetric-exit Gamma) or errors out.
(define (translate-theta-two-latch-body cfg body-entry-bid header-bid
                                        latches phis sub sub-var->out)
  ;; Walk Term:jump chain from body-entry-bid, translating each
  ;; block's insns, until we land on a Term:cond.  That cond is the
  ;; diamond dispatch whose two arms each close via a distinct
  ;; latch.
  (define-values (dispatch-bid dispatch-term sub1 sub-var->out1)
    (let loop ([bid body-entry-bid]
               [s sub]
               [sv sub-var->out])
      (define blk (cv-block cfg bid))
      (unless (= 0 (pvector-length (CfgBlock-phis blk)))
        (error 'translate-theta-two-latch-body
               "body pre-dispatch block ~a has phis, not yet supported" bid))
      (define-values (s* sv*)
        (translate-insns (CfgBlock-insns blk) s sv))
      (match (CfgBlock-terminator blk)
        [(Term:jump n) (loop n s* sv*)]
        [(and t (Term:cond _ _ _)) (values bid t s* sv*)]
        [other
         (error 'translate-theta-two-latch-body
                "body pre-dispatch block ~a has unsupported terminator ~s"
                bid other)])))

  (define pred-var (Term:cond-cond dispatch-term))
  (define then-bid (Term:cond-then-target dispatch-term))
  (define else-bid (Term:cond-else-target dispatch-term))

  ;; Per-arm scoped reach; each must cover exactly one of the two
  ;; latches; reaches must be disjoint; every leaf terminator closes
  ;; back to header-bid (no ret / throw / switch).
  (define (arm-latch-for arm-bid which)
    (define reach (arm-reach-set cfg arm-bid))
    (define hits
      (for/list ([kv (in-idmap reach)]
                 #:when (idmap-ref latches (car kv) #f))
        (car kv)))
    (unless (= 1 (length hits))
      (error 'translate-theta-two-latch-body
             "~a arm (entry=~a, dispatch=~a) scoped reach contains ~a latches: ~s"
             which arm-bid dispatch-bid (length hits) hits))
    (values reach (car hits)))
  (define-values (then-reach then-latch) (arm-latch-for then-bid 'then))
  (define-values (else-reach else-latch) (arm-latch-for else-bid 'else))
  (when (equal? then-latch else-latch)
    (error 'translate-theta-two-latch-body
           "both arms of dispatch ~a reach the same latch ~a; 2-latch diamond requires distinct latches"
           dispatch-bid then-latch))
  (unless (reach-sets-disjoint? then-reach else-reach)
    (error 'translate-theta-two-latch-body
           "arm reach-sets overlap at dispatch ~a; 2-latch diamond requires disjoint arms"
           dispatch-bid))
  (define (arm-closes-to-header? reach)
    (for/and ([kv (in-idmap reach)])
      (match (CfgBlock-terminator (cv-block cfg (car kv)))
        [(Term:jump n) (or (idmap-ref reach n #f)
                           (equal? n header-bid))]
        [(Term:cond _ tb eb) (and (idmap-ref reach tb #f)
                                  (idmap-ref reach eb #f))]
        [_ #f])))
  (unless (and (arm-closes-to-header? then-reach)
               (arm-closes-to-header? else-reach))
    (error 'translate-theta-two-latch-body
           "2-latch diamond arms at dispatch ~a must close back to header ~a only (no ret/throw)"
           dispatch-bid header-bid))

  ;; Unified ctx: union of the per-arm ctx-var sets, so each Gamma
  ;; sub-region can share the same Gamma input layout.
  (define then-ctx (collect-arm-ctx cfg sub-var->out1 then-reach))
  (define else-ctx (collect-arm-ctx cfg sub-var->out1 else-reach))
  (define combined-ctx-set
    (for/fold ([s then-ctx]) ([kv (in-idmap else-ctx)])
      (idmap-set s (car kv) #t)))
  (define combined-ctx-vars
    (for/pvector ([kv (in-idmap combined-ctx-set)]) (car kv)))
  (define n-ctx (pvector-length combined-ctx-vars))
  (define n-phis (pvector-length phis))

  (define (build-arm arm-bid arm-latch-bid which)
    (define arm0 (region-empty))
    (define-values (arm1 _arg-nid _arg-ins arg-outs)
      (region-add-node arm0 (Simple (list 'region-arg n-ctx)) 0 n-ctx))
    (define arm-var->out
      (for/fold ([m idmap-empty])
                ([v (in-pvector combined-ctx-vars)]
                 [oid (in-pvector arg-outs)])
        (idmap-set m v oid)))
    (define-values (arm2 arm-var->out2 payload)
      (translate-segment cfg arm-bid header-bid arm1 arm-var->out))
    (unless (eq? payload #f)
      (error 'translate-theta-two-latch-body
             "~a arm ~a did not close via back-edge (payload=~s)"
             which arm-bid payload))
    (define-values (arm3 _res-nid res-ins _res-outs)
      (region-add-node arm2 (Simple (list 'region-result n-phis)) n-phis 0))
    (for/fold ([r arm3])
              ([phi (in-pvector phis)]
               [iid (in-pvector res-ins)])
      (define src-var (pick-phi-source phi arm-latch-bid which))
      (define src-oid
        (or (idmap-ref arm-var->out2 src-var #f)
            (error 'translate-theta-two-latch-body
                   "~a arm latch source ~a (phi-output ~a) undefined"
                   which src-var (PhiInsn-output phi))))
      (define-values (r* _w) (region-add-wire r src-oid iid))
      r*))

  (define then-region (build-arm then-bid then-latch 'then))
  (define else-region (build-arm else-bid else-latch 'else))

  ;; Install the merge Gamma in the body sub-region: 1 predicate +
  ;; n-ctx inputs, n-phis outputs.
  (define gamma-val (Gamma (list then-region else-region)))
  (define-values (sub-g _gnid g-ins g-outs)
    (region-add-node sub1 gamma-val (add1 n-ctx) n-phis))

  (define pred-oid
    (or (idmap-ref sub-var->out1 pred-var #f)
        (error 'translate-theta-two-latch-body
               "dispatch predicate ~a undefined" pred-var)))
  (define-values (sub-p _pw)
    (region-add-wire sub-g pred-oid (pvector-ref g-ins 0)))
  (define sub-wired
    (for/fold ([r sub-p])
              ([v (in-pvector combined-ctx-vars)]
               [i (in-naturals 1)])
      (define src-oid
        (or (idmap-ref sub-var->out1 v #f)
            (error 'translate-theta-two-latch-body "ctx var ~a undefined" v)))
      (define-values (r* _w) (region-add-wire r src-oid (pvector-ref g-ins i)))
      r*))

  ;; Bind each header-phi-output VarId to its Gamma output so the
  ;; outer update-oids computation can look it up uniformly.
  (define sub-var->out-final
    (for/fold ([m sub-var->out1])
              ([phi (in-pvector phis)]
               [oid (in-pvector g-outs)])
      (idmap-set m (PhiInsn-output phi) oid)))

  (values sub-wired sub-var->out-final))


;; ============================================================
;; Theta recovery (while-loop)
;; ============================================================
;;
;; Scope: a single natural loop.  The header must end in Term:cond;
;; one arm leads (possibly through Term:jump steps and/or inner
;; convergent diamonds) to one or more latches, each of which jumps
;; back to the header.  The other cond arm exits the loop.  Single-
;; latch loops use the straight translate-segment body walk;
;; two-latch diamond loops (two `continue`-ing arms of an inner if)
;; use `translate-theta-two-latch-body` to materialise a merge
;; Gamma inside the body.  Deviations raise self-identifying errors.
;;
;; Sub-region layout:
;;   - `Simple '(region-arg N)` producer provides one output per
;;     loop-carried phi (these mirror the Theta node's inputs).
;;   - Header's non-phi insns are translated in-region.
;;   - Body blocks' insns are translated in-region via a scoped
;;     translate-segment call starting at the cond's body-arm and
;;     with stop-bid=header; the Term:jump chain walks through any
;;     intermediate body blocks and the latch before closing on the
;;     back-edge.
;;   - `Simple '(region-result (+ N 1))` consumer takes (predicate,
;;     loop-carried-updates...); `Simple 'not` is inserted when the
;;     natural cond polarity would iterate on the wrong branch.
;;
;; Returns (values region var->out payload) shaped like
;; translate-segment -- after the Theta, translation continues at
;; the exit-arm block and may hit a terminator there.
(define (translate-theta cfg theta-ctx stop-bid region var->out)
  (define header-bid (Theta-Ctx-header theta-ctx))
  (define latches (Theta-Ctx-latches theta-ctx))
  (define n-latches (idmap-count latches))
  (define header-blk (cv-block cfg header-bid))
  (define phis (CfgBlock-phis header-blk))

  ;; Build phi index: phi-output VarId -> PhiInsn.  Every loop-carried
  ;; parent var has an entry here; every other parent var is
  ;; loop-invariant and passes through unchanged.
  (define phi-by-output
    (for/fold ([m idmap-empty])
              ([p (in-pvector phis)])
      (idmap-set m (PhiInsn-output p) p)))

  ;; The header must end in Term:cond: one arm jumps (possibly via a
  ;; chain of Term:jump blocks) to some latch and constitutes the loop
  ;; body; the other is the exit.  We resolve this up-front so that
  ;; collect-theta-ctx can see every body block's reads.
  (define header-term (CfgBlock-terminator header-blk))
  (unless (Term:cond? header-term)
    (error 'translate-theta
           "header ~a must end in Term:cond for Theta lowering (got ~a)"
           header-bid header-term))
  (define pred-var (Term:cond-cond header-term))
  (define then-bid (Term:cond-then-target header-term))
  (define else-bid (Term:cond-else-target header-term))
  (define-values (body-is-then? body-entry-bid exit-arm-bid)
    (cond
      [(body-arm-reaches-any-latch? cfg then-bid latches header-bid)
       (values #t then-bid else-bid)]
      [(body-arm-reaches-any-latch? cfg else-bid latches header-bid)
       (values #f else-bid then-bid)]
      [else
       (error 'translate-theta
              "neither cond arm reaches any latch of ~a without crossing back-edge (then=~a else=~a latches=~s)"
              header-bid then-bid else-bid
              (for/list ([kv (in-idmap latches)]) (car kv)))]))
  (define body-blocks
    (theta-body-blocks cfg body-entry-bid latches header-bid))

  ;; Theta's inputs/outputs cover:
  ;;   (a) phi outputs at the header (loop-carried; always needed
  ;;       because the body rebinds via them)
  ;;   (b) parent-scope vars actually read inside header or body
  ;;       (loop-invariant context)
  ;; Phi entry sources themselves don't need to be in carried-vars --
  ;; they are resolved by direct lookup in parent var->out when wiring
  ;; Theta inputs below.
  (define carried-vars
    (let* ([phi-outs
            (for/pvector ([p (in-pvector phis)]) (PhiInsn-output p))]
           [reads
            (collect-theta-ctx cfg var->out header-bid body-blocks phis)]
           [seen
            (for/fold ([s idmap-empty])
                      ([v (in-pvector phi-outs)])
              (idmap-set s v #t))])
      (for/fold ([out phi-outs] [s seen]
                 #:result out)
                ([kv (in-idmap reads)])
        (define v (car kv))
        (if (idmap-ref s v #f)
            (values out s)
            (values (pvector-cons-right out v)
                    (idmap-set s v #t))))))
  (define n-carried (pvector-length carried-vars))

  ;; For each carried var, derive the "entry" source var (value on
  ;; the first iteration's input to Theta).  Loop-carried: phi's
  ;; source whose predecessor-bid is not in `latches` (i.e. the pre-
  ;; header feed).  Invariant: the var itself.
  (define entry-src-vars
    (for/pvector ([v (in-pvector carried-vars)])
      (define phi (idmap-ref phi-by-output v #f))
      (cond
        [phi
         (or (for/or ([s (in-pvector (PhiInsn-sources phi))])
               (and (not (idmap-ref latches (car s) #f)) (cdr s)))
             (error 'translate-theta
                    "phi ~a has no non-latch source" v))]
        [else v])))

  ;; ---- Build the sub-region. ----
  (define sub0 (region-empty))
  (define-values (sub1 _arg-nid _arg-ins arg-outs)
    (region-add-node sub0 (Simple (list 'region-arg n-carried)) 0 n-carried))

  ;; Sub-var->out maps each carried var to its region-arg output.
  (define sub-var->out-init
    (for/fold ([m idmap-empty])
              ([v (in-pvector carried-vars)]
               [oid (in-pvector arg-outs)])
      (idmap-set m v oid)))

  ;; Translate header's non-phi insns (includes the comparison VfInsn
  ;; that produces the Term:cond predicate).
  (define-values (sub2 sub-var->out2)
    (translate-insns (CfgBlock-insns header-blk) sub1 sub-var->out-init))

  ;; Translate the body block(s), stopping at the back-edge(s) into
  ;; header.  Single-latch: translate-segment walks the Term:jump
  ;; chain from body-entry-bid through the latch and naturally stops
  ;; at header (= stop-bid); latch-write values live in
  ;; sub-var->out3 and feed update-oids via phi's latch-source.
  ;; Multi-latch (currently: exactly 2 latches, 2-arm diamond): the
  ;; body-entry block's Term:cond fans out to two arms whose scoped
  ;; reach-sets each contain exactly one distinct latch.  Each arm
  ;; becomes a sub-region of an inner Gamma whose outputs are the
  ;; header phis' merged values; after the Gamma each phi-output
  ;; VarId maps to the corresponding Gamma output.  In both cases we
  ;; restrict the arm-walking helpers to body-blocks so any inner
  ;; Gamma's `find-branch-join` cannot wander past a latch.
  (define-values (sub3 sub-var->out3)
    (cond
      [(= n-latches 1)
       (define latch-bid (for/or ([kv (in-idmap latches)]) (car kv)))
       (define-values (s sv body-payload)
         (parameterize ([current-arm-scope body-blocks])
           (translate-segment cfg body-entry-bid header-bid sub2 sub-var->out2)))
       (unless (eq? body-payload #f)
         (error 'translate-theta
                "loop body reached a top-level ret/throw (payload=~s) before closing the back-edge; guarded early-exits should have lowered via an asymmetric Gamma inside the body"
                body-payload))
       (values s sv)]
      [(= n-latches 2)
       (parameterize ([current-arm-scope body-blocks])
         (translate-theta-two-latch-body cfg body-entry-bid header-bid
                                         latches phis sub2 sub-var->out2))]
      [else
       (error 'translate-theta
              "header ~a has ~a back-edges; currently only 1- and 2-latch loops are supported"
              header-bid n-latches)]))

  ;; Iteration predicate (see translate-theta docstring).
  (define pred-oid
    (or (idmap-ref sub-var->out3 pred-var #f)
        (error 'translate-theta "predicate ~a undefined in sub-region" pred-var)))
  (define-values (sub4 iter-pred-oid)
    (cond
      [body-is-then? (values sub3 pred-oid)]
      [else
       (define-values (s* _not-nid not-ins not-outs)
         (region-add-node sub3 (Simple 'not) 1 1))
       (define-values (s** _w)
         (region-add-wire s* pred-oid (pvector-ref not-ins 0)))
       (values s** (pvector-ref not-outs 0))]))

  ;; For each carried var, its "update" going back to the next
  ;; iteration / exposed as Theta's output.  Single-latch: the phi's
  ;; latch source lives in sub-var->out3 under the per-latch VarId.
  ;; Multi-latch: the phi-output VarId itself has already been bound
  ;; in sub-var->out3 to the merge-Gamma output.  Invariants: the
  ;; sub-region's own current value (which equals the region-arg
  ;; output since nothing rebound it).
  (define update-oids
    (for/pvector ([v (in-pvector carried-vars)])
      (define phi (idmap-ref phi-by-output v #f))
      (cond
        [(and phi (= n-latches 1))
         (define latch-bid (for/or ([kv (in-idmap latches)]) (car kv)))
         (define latch-var (pick-phi-source phi latch-bid 'latch))
         (or (idmap-ref sub-var->out3 latch-var #f)
             (error 'translate-theta "latch source ~a undefined" latch-var))]
        [else
         (or (idmap-ref sub-var->out3 v #f)
             (error 'translate-theta "carried var ~a lost during body" v))])))

  ;; region-result: 1 (predicate) + n-carried updates.
  (define-values (sub5 _res-nid res-ins _res-outs)
    (region-add-node sub4
                     (Simple (list 'region-result (add1 n-carried)))
                     (add1 n-carried) 0))
  (define-values (sub6 _wpred)
    (region-add-wire sub5 iter-pred-oid (pvector-ref res-ins 0)))
  (define sub7
    (for/fold ([r sub6])
              ([oid (in-pvector update-oids)]
               [i (in-naturals 1)])
      (define-values (r* _w) (region-add-wire r oid (pvector-ref res-ins i)))
      r*))

  ;; ---- Install Theta node in the parent region. ----
  (define theta-val (Theta sub7))
  (define-values (region1 _tnid t-ins t-outs)
    (region-add-node region theta-val n-carried n-carried))

  ;; Wire Theta inputs from parent-scope values.
  (define region2
    (for/fold ([r region1])
              ([entry-var (in-pvector entry-src-vars)]
               [iid (in-pvector t-ins)])
      (define src-oid
        (or (idmap-ref var->out entry-var #f)
            (error 'translate-theta "entry source ~a undefined" entry-var)))
      (define-values (r* _w) (region-add-wire r src-oid iid))
      r*))

  ;; Publish each carried var's post-loop value = Theta output.
  (define var->out*
    (for/fold ([m var->out])
              ([v (in-pvector carried-vars)]
               [oid (in-pvector t-outs)])
      (idmap-set m v oid)))

  ;; Continue translating from the exit-arm; the loop map lives in
  ;; the parameter so sibling / outer loops keep dispatching.
  (translate-segment cfg exit-arm-bid stop-bid region2 var->out*))
