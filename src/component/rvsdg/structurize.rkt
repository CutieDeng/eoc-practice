#lang racket/base

;; ============================================================
;; Component: SSA CFG → RVSDG structurizer (M4)
;; ============================================================
;;
;; Reads the CFG only through the port of port.rkt (`cfg` below is a
;; cv = (cons port adapter)); the Java pipeline's cfg->rvsdg wraps a
;; kernel Cfg, the GIMPLE pipeline adapts a GCfg.  Translates an SSA
;; CFG into an RVSDG Lambda.  The translation is recursive over the CFG:
;;
;;   - Straight-line chains of Term:jump blocks are inlined into a
;;     single region (parent or sub-region alike).
;;   - Term:cond blocks become `Gamma` nodes with two nested
;;     sub-regions.  Each arm may span multiple basic blocks and may
;;     itself contain nested Term:cond blocks (inner diamonds), as
;;     long as each inner cond's arms also converge before the outer
;;     join.  Each sub-region uses a synthetic `Simple '(region-arg
;;     N)` producer node at its entry to mirror the Gamma's context
;;     inputs, and a `Simple '(region-result N)` consumer node to
;;     feed the Gamma's outputs.
;;   - Term:ret / Term:throw yield synthetic `Simple 'return` /
;;     kernel `Throw` sink nodes in whichever region they appear.
;;     A Term:cond whose two arms are each a single block ending in
;;     Term:ret / Term:throw lowers to a Gamma whose sub-regions
;;     each terminate internally (no region-result, 0 Gamma outputs);
;;     translate-segment signals upward with payload
;;     'already-terminated so cfg->rvsdg knows not to install another
;;     return at the outer region.  When only one arm is such a
;;     terminal block, the cond lowers to an asymmetric early-exit
;;     Gamma: the exit sub-region owns the return / throw sink while
;;     the continue sub-region is a no-op; translate-segment resumes
;;     from the continuing branch's block in the outer region.
;;     A Term:cond whose two arms each span multiple blocks but whose
;;     reach-sets are disjoint and contain only ret/throw leaves also
;;     lowers to a terminal Gamma; each multi-block sub-region is
;;     built by recursing into translate-segment with stop-bid=#f and
;;     installing the resulting payload's ret / throw sink inside the
;;     sub-region (any inner diamonds / loops within the exit arm are
;;     materialised as nested Gamma / Theta nodes inside the sub-
;;     region just as they would be in any other region context).
;;     When the inner cond sitting INSIDE a standard diamond arm has
;;     one arm (single-block or multi-block) whose reach-set never
;;     rejoins the outer search's shared territory, the outer arm-
;;     walk skips past that exit subtree via `arm-advance` (which
;;     consults a `current-shared-set` installed by translate-gamma)
;;     and the inner cond lowers as an asymmetric early-exit Gamma
;;     inside the outer arm's sub-region.  The dispatch decides
;;     asymmetric-multi-block vs terminal vs standard by inspecting
;;     whether `stop-bid` is in exactly one arm's forward reach.
;;
;;   - A single natural loop is lowered to a Theta node.  The header
;;     must end in Term:cond; one arm leads to the latch (continue)
;;     and the other exits the loop.  The body from that cond-arm to
;;     the latch may be a mixed chain of Term:jump blocks and inner
;;     diamonds (each inner Term:cond must itself converge at an
;;     inner join before the latch).  All parent-scope vars pass
;;     through as conservatively-closed loop-carried or loop-
;;     invariant inputs.  A `Simple 'not` node flips polarity when
;;     the body sits on the else-arm.
;;
;;     An early-exit (single-block or multi-block) sitting INSIDE a
;;     Theta loop body is also tolerated: `arm-reach-set` respects
;;     `current-arm-scope` (set by translate-theta to the loop's
;;     body-blocks), so a Term:cond arm whose successor crosses the
;;     back-edge into the loop header appears as a boundary edge.
;;     The helper `reach-set-reaches-stop?` probes arm terminators
;;     to classify the arm whose immediate successor is stop-bid
;;     (= the loop header) as the continue arm; the other arm —
;;     internally contained within body-blocks and ending in
;;     ret / throw — becomes the exit sub-region of an asymmetric
;;     Gamma installed inside the Theta's body region.  The
;;     existing asymmetric-Gamma dispatch is reused verbatim; no
;;     Theta-body-specific code path is needed.
;;     A loop header with exactly two back-edges is handled by
;;     `translate-theta-two-latch-body`: the body entry is allowed
;;     to be a Term:jump chain that eventually dispatches via one
;;     Term:cond whose two arms each close to the header via
;;     exactly one distinct latch (per-arm scoped reach must be
;;     disjoint and each arm's blocks must only exit through its
;;     own latch or the header).  The diamond lowers to a merge
;;     Gamma installed inside the Theta body region: the Gamma's
;;     pred input is the dispatch Term:cond's var, the ctx inputs
;;     are the union of both arms' live ctx vars, and the Gamma's
;;     outputs (one per header phi) are produced by each sub-
;;     region's region-result nodes, wired via pick-phi-source.
;;     The resulting Gamma output VarIds then feed the Theta's
;;     latch payload for every phi, so the single-latch and multi-
;;     latch code paths share the same outer Theta shape.
;;
;;     A Term:switch (from TABLESWITCH / LOOKUPSWITCH) whose every
;;     target (default + each case in order) reaches only ret /
;;     throw leaves lowers to an (n+1)-arm Gamma via
;;     `translate-terminal-switch`: sub-regions are ordered
;;     [default, case_0, ..., case_{n-1}], each sub-region's
;;     Region.info records 'java/switch-case-key (= the symbol
;;     'default or the integer case key), preserving the key-to-arm
;;     correspondence inside the RVSDG.  Ctx inputs are the union
;;     of every arm's live parent-scope reads; the Gamma's
;;     predicate input is the raw switch value and the 0-based
;;     arm-index dispatch is deferred to a later pass.
;;
;;     A convergent Term:switch — every target eventually rejoins a
;;     common join block whose phis carry the per-arm merged values —
;;     lowers via `translate-convergent-switch`: the N+1 arms are
;;     walked from each branch-bid to the shared join-bid using the
;;     same find-branch-join / arm-last-before-join / arm-blocks-set
;;     machinery as translate-gamma's convergent path, generalised
;;     over a list of arms.  Ctx inputs are collected per-arm via
;;     `collect-switch-arms-ctx` (N-arm generalisation of
;;     collect-gamma-ctx); the join block's phis become the Gamma's
;;     outputs, each arm's sub-region ending in a region-result that
;;     picks the phi source corresponding to that arm's
;;     arm-last-before-join predecessor.  Sub-region order and the
;;     'java/switch-case-key encoding mirror the terminal variant;
;;     translate-segment resumes from join-bid in the outer region.
;;
;;     A `try { } catch { }` region — identified from
;;     `'java/exception-table` (prepared by `normalize-try-exits` so
;;     each window has at most one fall-through exit) — lowers to a
;;     `Kappa` node via `translate-kappa-group`.  The try-region is
;;     built by `translate-segment` scoped to the try's forward reach;
;;     each handler-region uses `build-handler-region-entry` so the
;;     `(+ n-ctx 1)`-th region-arg output is the exception-ref, and
;;     `translate-vfinsn` resolves any `'java/exception-ref` producer
;;     (synthesised by `jvm-method->cfg` at handler entry) to that
;;     region-arg output via the `current-exn-out` parameter.  The
;;     Kappa comes in two shapes — `'terminal` (M=0 outputs; every arm
;;     ends in ret / throw; installed via `install-kappa-terminal`) and
;;     `'convergent` (M = n_phis(join); every arm ends in `Term:jump
;;     join-bid` and its `region-result` draws the per-arm phi-source
;;     values; installed via `install-kappa-convergent`, after which
;;     translate-segment resumes from the join block in the outer
;;     region).  First-wins handler ordering is preserved: `handlers`
;;     is a pvector of `(cons catch-type Region)` in declaration order
;;     matching the exception-table's per-range entries.
;;
;; Currently unsupported:
;;   - loop headers with 3+ back-edges (only 1-latch and 2-latch
;;     diamond loops are currently recognised)
;;
;; The kappa-recovery pipeline covers single-range, multi-handler,
;; convergent-past-forwarder, method-end (end-bid=#f), nested
;; try/catch at distinct start-bids, multi-range (same-start
;; different-end) layouts, and multi-source arm merges (inner
;; diamond inside the try reunifies at the Kappa's convergent join).
;; Multi-range materialises as a stack of Kappa-Groups under the
;; shared start-bid key (outer first); the outer Kappa's try-region
;; re-encounters start-bid with the outer popped and dispatches into
;; the inner Kappa.  Multi-source: inside the Kappa try sub-region,
;; translate-gamma on the inner cond lands its own find-branch-join
;; on the outer join-bid (with the scope-boundary extension
;; described at translate-segment's standard-diamond dispatch), and
;; each join-phi's output VarId is bound to the inner Gamma's output
;; in sub-var->out.  finish-kappa-sub-region/cfg then detects
;; multi-source by counting direct Term:jump-to-join preds in the
;; arm-reach and reads the phi-output oid directly from that map,
;; rather than picking a single pred's phi-source VarId.  Each
;; remaining unsupported shape raises with a self-identifying error.
;;
;; ============================================================

(require racket/match
         racket/list
         "../../kernel/ir/cfg/types.rkt"
         "../../kernel/ir/rvsdg/rvsdg.rkt"
         "utils/builder.rkt"
         "port.rkt"
         "../../kernel/data/data.rkt"
         (except-in "structurize/ctx.rkt" translate-segment)
         "structurize/reach.rkt"
         "structurize/regions.rkt"
         "structurize/insn.rkt"
         "structurize/sinks.rkt"
         "structurize/gamma.rkt"
         "structurize/theta.rkt"
         "structurize/kappa.rkt")
(provide structurize)

;; ============================================================
;; Entry
;; ============================================================

(define (structurize cfg)
  (define local-count (cv-info cfg 'java/max-local 0))
  (define param-names
    (or (cv-info cfg 'java/ssa-param-names #f)
        (error 'cfg->rvsdg
               "ssa must publish 'java/ssa-param-names; run jvm-cfg->ssa first")))

  ;; Detect loop structure.  Each back-edge (latch . header)
  ;; contributes its latch-bid to the header's Theta-Ctx latches
  ;; set.  A reducible loop with a single latch produces a 1-entry
  ;; set; a 2-arm continue-diamond (see translate-theta) produces a
  ;; 2-entry set.  Nested loops have distinct headers and each gets
  ;; its own ctx entry.
  (define back-edges (cv-back-edges cfg))
  (define theta-ctxs
    (for/fold ([m idmap-empty])
              ([be (in-pvector back-edges)])
      (define latch (car be))
      (define header (cdr be))
      (define existing (idmap-ref m header #f))
      (define latches
        (cond
          [existing (idmap-set (Theta-Ctx-latches existing) latch #t)]
          [else (idmap-set idmap-empty
                                 latch #t)]))
      (idmap-set m header (Theta-Ctx header latches))))

  (define region0 (region-empty))

  ;; 1. Parameter provider node.
  (define-values (region1 _param-nid _param-ins param-outs)
    (region-add-node region0 (Simple 'param) 0 local-count))

  ;; 2. Seed var → output with each parameter name.
  (define var->out-init
    (for/fold ([m idmap-empty])
              ([name (in-pvector param-names)]
               [oid (in-pvector param-outs)])
      (idmap-set m name oid)))

  ;; Compute one Kappa-Group per try window in the exception-table.
  ;; Empty map when the method has no try/catch.  translate-segment
  ;; consults this map to dispatch into translate-kappa-group whenever
  ;; it lands on a try-entry BlockId.
  (define kappa-groups (compute-kappa-groups cfg))

  ;; 3. Translate starting at entry; stop-bid = #f means "until a
  ;;    terminator or unreachable".
  (define-values (region-final var->out-final payload)
    (parameterize ([current-translate-segment translate-segment]
                   [current-theta-ctxs theta-ctxs]
                   [current-kappa-groups kappa-groups])
      (translate-segment cfg (cv-entry cfg) #f region1 var->out-init)))

  ;; 4. Materialise Term:ret / Term:throw.  An 'already-terminated
  ;; payload means a terminal Gamma at the top level has installed
  ;; ret / throw inside each of its sub-regions, so the outer region
  ;; needs no further sink node.
  (define region-done
    (match payload
      [(cons 'ret vs)        (install-return region-final var->out-final vs)]
      [(cons 'throw ex)      (install-throw  region-final var->out-final ex)]
      ['already-terminated   region-final]
      [_                     region-final]))

  (Lambda region-done))


;; ============================================================
;; Core recursion: translate a segment [start-bid .. stop-bid)
;; ============================================================
;;
;; Returns (values region var->out payload).  `payload` is #f for a
;; segment that hits stop-bid, or (cons 'ret pvector) / (cons
;; 'throw VarId) for one that ran into a function-level exit.
;;
;; The active loop map lives in `current-theta-ctxs` (set by
;; cfg->rvsdg).  When `start-bid` matches one of the headers in that
;; map AND the caller did not pass stop-bid=header (which would
;; catch back-edge closure first), we dispatch into translate-theta.
;; Nested loops, sibling loops, and loops inside Gamma arms all fall
;; out for free: every recursive entry into translate-segment
;; consults the same map, so any header encountered along the way is
;; lowered into a Theta in whichever region we currently inhabit.
(define (translate-segment cfg start-bid stop-bid region var->out)
  (define ctxs (current-theta-ctxs))
  (define kappas (current-kappa-groups))
  (define ctx-here (and ctxs (idmap-ref ctxs start-bid #f)))
  (define kappa-stack-here (and kappas (idmap-ref kappas start-bid #f)))
  ;; Outer Kappa first (pvector is sorted descending by end-bid at
  ;; compute-kappa-groups time).  A single Kappa-Group is wrapped in a
  ;; length-1 pvector so this lookup is uniform across the common
  ;; single-range case and the multi-range (nested) case.
  (define kappa-here
    (and kappa-stack-here
         (> (pvector-length kappa-stack-here) 0)
         (pvector-ref kappa-stack-here 0)))
  (cond
    [(and stop-bid (equal? start-bid stop-bid))
     (values region var->out #f)]
    [kappa-here
     (define-values (region* var->out* kpayload)
       (translate-kappa-group cfg start-bid kappa-here region var->out))
     (cond
       [(eq? kpayload 'already-terminated)
        (values region* var->out* 'already-terminated)]
       [(BlockId? kpayload)
        ;; Convergent: resume translation from join-bid in the outer
        ;; (caller's) region.
        (translate-segment cfg kpayload stop-bid region* var->out*)]
       [else
        (error 'translate-segment
               "unexpected payload ~s from translate-kappa-group at ~a"
               kpayload start-bid)])]
    [ctx-here
     (translate-theta cfg ctx-here stop-bid region var->out)]
    [else
     (define blk (cv-block cfg start-bid))
     (unless blk (error 'translate-segment "missing block ~a" start-bid))

     ;; A block may carry phis; those should already be materialised
     ;; as Gamma/Theta outputs by the caller (translate-gamma seeds
     ;; var->out with each phi's output).  We verify here that every
     ;; phi output is resolved -- an unresolved phi means the segment
     ;; was re-entered through a path that skipped its handler.
     (for ([phi (in-pvector (CfgBlock-phis blk))])
       (unless (idmap-ref var->out (PhiInsn-output phi) #f)
         (error 'translate-segment
                "unresolved phi ~a at block ~a -- missing Gamma/Theta handler"
                (PhiInsn-output phi) start-bid)))

     ;; Translate the block's insns.
     (define-values (region1 var->out1)
       (translate-insns (CfgBlock-insns blk) region var->out))

     ;; Dispatch on the terminator.
     (match (CfgBlock-terminator blk)
       [(Term:jump nxt)
        (translate-segment cfg nxt stop-bid region1 var->out1)]
       [(Term:ret ret-vs)
        (values region1 var->out1 (cons 'ret ret-vs))]
       [(Term:throw ex)
        (values region1 var->out1 (cons 'throw ex))]
       [(Term:unreachable)
        (values region1 var->out1 #f)]
       [(Term:cond pred then-bid else-bid)
        (define then-blk (cv-block cfg then-bid))
        (define else-blk (cv-block cfg else-bid))
        (define then-term? (terminal-block? then-blk))
        (define else-term? (terminal-block? else-blk))
        ;; A terminal arm only qualifies as a true "exit" arm when the
        ;; other arm's forward reach does NOT flow into it.  If it
        ;; does, the terminal block is the shared convergent join —
        ;; one arm is an empty-arm diamond whose join happens to end
        ;; in ret/throw — and must go through translate-gamma proper
        ;; instead of the asymmetric-exit path.
        (define then-arm-is-exit?
          (and then-term?
               (not (idmap-ref (arm-reach-set cfg else-bid) then-bid #f))))
        (define else-arm-is-exit?
          (and else-term?
               (not (idmap-ref (arm-reach-set cfg then-bid) else-bid #f))))
        (cond
          [(and then-arm-is-exit? else-arm-is-exit?)
           ;; Both arms are single-block exits (Term:ret / Term:throw)
           ;; with disjoint reach-sets.
           (translate-terminal-gamma cfg start-bid pred then-bid else-bid
                                     region1 var->out1)]
          [(or then-arm-is-exit? else-arm-is-exit?)
           ;; Asymmetric early-exit: one arm is a single block ending
           ;; in Term:ret / Term:throw and is not shared with the
           ;; other arm.  Lower as a Gamma whose exit sub-region runs
           ;; the early return / throw and whose continue sub-region
           ;; is a no-op.  After the Gamma falls through, translate-
           ;; segment resumes from the continuing branch's first block
           ;; in the outer region.
           (define-values (region2 var->out2 continue-bid)
             (translate-asymmetric-exit-gamma cfg start-bid pred
                                              then-bid then-arm-is-exit?
                                              else-bid else-arm-is-exit?
                                              region1 var->out1))
           (translate-segment cfg continue-bid stop-bid region2 var->out2)]
          [else
           ;; Neither arm is a single-block exit.  Classify via
           ;; forward reach-sets plus the active stop-bid:
           ;;   - Exactly one arm reaches stop-bid → multi-block
           ;;     asymmetric early-exit: lower the non-continuing arm
           ;;     as the exit sub-region, resume translate-segment
           ;;     from the continuing arm.  Requires disjoint reach-
           ;;     sets so the exit arm doesn't accidentally share
           ;;     blocks with the continue arm.
           ;;   - Neither reaches stop-bid, reach-sets disjoint, all
           ;;     leaves ret/throw → multi-block terminal Gamma.
           ;;   - Otherwise → standard diamond.
           (define then-reach (arm-reach-set cfg then-bid))
           (define else-reach (arm-reach-set cfg else-bid))
           (define then-reaches-stop?
             (reach-set-reaches-stop? cfg then-reach stop-bid))
           (define else-reaches-stop?
             (reach-set-reaches-stop? cfg else-reach stop-bid))
           (cond
             [(and stop-bid
                   (not (eq? then-reaches-stop? else-reaches-stop?))
                   (arm-leaves-all-ret-throw? cfg then-reach)
                   (arm-leaves-all-ret-throw? cfg else-reach)
                   (reach-sets-disjoint? then-reach else-reach))
              ;; Multi-block asymmetric early-exit: exactly one arm
              ;; reaches stop-bid (the continue arm); the other arm's
              ;; reach is internal and ret/throw-only.
              (define-values (region2 var->out2 continue-bid)
                (translate-asymmetric-exit-gamma cfg start-bid pred
                                                 then-bid (not then-reaches-stop?)
                                                 else-bid (not else-reaches-stop?)
                                                 region1 var->out1))
              (translate-segment cfg continue-bid stop-bid region2 var->out2)]
             [(and (not then-reaches-stop?)
                   (not else-reaches-stop?)
                   (arm-leaves-all-ret-throw? cfg then-reach)
                   (arm-leaves-all-ret-throw? cfg else-reach)
                   (reach-sets-disjoint? then-reach else-reach))
              ;; Multi-block terminal Gamma: build each sub-region by
              ;; recursing into translate-segment until it hits its
              ;; own ret / throw payload.  Guard on neither-reaches-stop
              ;; so a merge diamond inside a scope (Kappa try / Theta
              ;; body) — where arm-reach is scope-clipped and may look
              ;; "terminal-shaped" even though both arms actually feed
              ;; the outer join — falls through to the standard diamond
              ;; lowering below.
              (translate-terminal-gamma cfg start-bid pred then-bid else-bid
                                        region1 var->out1)]
             [else
              ;; When both arms reach stop-bid via boundary edges — a
              ;; merge diamond whose join lives one step past the
              ;; scope boundary — extend `current-arm-scope` with
              ;; stop-bid so find-branch-join / arm-advance recognise
              ;; stop-bid itself as the convergence point.  Without
              ;; this, the scope-clipped reach walk dead-ends before
              ;; reaching the join.
              (define scope-for-gamma
                (let ([s (current-arm-scope)])
                  (cond
                    [(and s stop-bid then-reaches-stop? else-reaches-stop?)
                     (idmap-set s stop-bid #t)]
                    [else s])))
              (define-values (region2 var->out2 join-bid)
                (parameterize ([current-arm-scope scope-for-gamma])
                  (translate-gamma cfg start-bid pred then-bid else-bid
                                   region1 var->out1)))
              (cond
                [join-bid
                 (translate-segment cfg join-bid stop-bid region2 var->out2)]
                [else
                 (values region2 var->out2 #f)])])])]
       [(Term:switch value cases default-bid)
        ;; Dispatch: terminal (all arms ret/throw-only, pairwise
        ;; disjoint) vs convergent (arms rejoin a common join block
        ;; that carries the phis).  Sub-region order is always
        ;; [default, case_0, ..., case_{N-1}]; each sub-region's
        ;; Region.info carries 'java/switch-case-key.
        (define case-bids
          (for/list ([kv (in-pvector cases)]) (cdr kv)))
        (define all-bids (cons default-bid case-bids))
        (define reaches
          (for/list ([b (in-list all-bids)]) (arm-reach-set cfg b)))
        (cond
          [(and (andmap (lambda (r) (arm-leaves-all-ret-throw? cfg r)) reaches)
                (reach-sets-all-pairwise-disjoint? reaches))
           (translate-terminal-switch cfg start-bid value cases default-bid
                                      region1 var->out1)]
          [else
           (define-values (region2 var->out2 join-bid)
             (translate-convergent-switch cfg start-bid value cases default-bid
                                          region1 var->out1))
           (translate-segment cfg join-bid stop-bid region2 var->out2)])]
       [#f
        (values region1 var->out1 #f)])]))
