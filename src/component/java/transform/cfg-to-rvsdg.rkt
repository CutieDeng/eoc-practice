#lang racket/base

;; ============================================================
;; Component: SSA CFG → RVSDG (M4)
;; ============================================================
;;
;; Translate an SSA CFG (as produced by `jvm-cfg->ssa`) into an
;; RVSDG Lambda.  The translation is recursive over the CFG:
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
;;   - multi-range try windows (same try with overlapping / finally-
;;     style entries; a single-start multi-end pattern errors out)
;;   - nested try/catch (outer kappa's try-bids overlap inner try's
;;     start-bid is not yet validated; works incidentally if regions
;;     do not overlap, not yet exercised in tests)
;;   - method-end try windows (exception-table end-bid = #f) — the
;;     original ordinal range isn't recoverable post-normalize yet
;;   - loop headers with 3+ back-edges (only 1-latch and 2-latch
;;     diamond loops are currently recognised)
;;
;; Each of the above raises with a self-identifying error.
;;
;; ============================================================

(require racket/match
         "../../../kernel/ir/cfg/types.rkt"
         "../../../kernel/ir/rvsdg/rvsdg.rkt"
         "../../../component/cfg/utils/graph-ops.rkt"
         "../../../component/cfg/analysis/loops.rkt"
         "../../../component/rvsdg/utils/builder.rkt"
         (except-in "../../../kernel/data/data.rkt" integer-compare))

(provide cfg->rvsdg)

;; ============================================================
;; Loop context (single-loop scope)
;; ============================================================
;;
;; Threaded through translate-segment so that the first entry into a
;; loop header spawns a Theta.  The body from the header's body-arm
;; to any latch may mix Term:jump steps with inner Gamma diamonds
;; (each must converge before the latch).  Each latch's back-edge
;; closes to the header.  `latches` is an ordered-map BlockId -> #t;
;; a single-entry set covers the common single-latch case, a
;; 2-entry set covers the 2-latch diamond pattern (e.g. an inner if
;; whose two arms each `continue` via a distinct tail).
;;
(struct Theta-Ctx (header latches) #:prefab)

;; ============================================================
;; Kappa group (try/catch recovery scaffolding)
;; ============================================================
;;
;; A `Kappa-Group` describes one try/catch region identified on the
;; CFG side before RVSDG construction.  The actual construction is
;; staged over C2–C4 and wired via `translate-segment` once the
;; normalize-try-exits pre-pass is in place; for now the struct is
;; a scaffolding type carrying exactly the data downstream code will
;; need, without participating in the current translator.
;;
;; Fields:
;;   try-bids     : ordered-map[BlockId -> #t] — blocks inside the
;;                  half-open try window.  Handler blocks are
;;                  excluded (they see the exception as a region-arg
;;                  rather than a normal predecessor).
;;   handlers     : pvector[(cons catch-type BlockId)] — ordered
;;                  handler list in declaration order.  catch-type is
;;                  a Java class-name string, or #f for catch-all.
;;   kind         : 'terminal — every arm (try + each handler) ends
;;                               in ret / throw; Kappa has 0 outputs.
;;                  'convergent — every arm falls through to a common
;;                                join block; Kappa has join-phi-count
;;                                outputs.
;;   join-bid     : BlockId or #f — the shared post-Kappa block when
;;                  kind = 'convergent, #f when kind = 'terminal.
(struct Kappa-Group (try-bids handlers kind join-bid) #:prefab)

;; Optional restriction on which BlockIds the arm-walking helpers
;; (`arm-advance`, `find-branch-join`, `arm-blocks-set`,
;; `arm-last-before-join`) are allowed to traverse.  A successor
;; outside the scope behaves as if the arm hit a non-advanceable
;; terminator (Term:ret etc.).  Set inside `translate-theta` to the
;; loop's body-blocks so that an inner Gamma's diamond search
;; cannot wander across the back-edge into the loop header and
;; recurse forever.  #f (the default) disables the check.
(define current-arm-scope (make-parameter #f))

(define (arm-scope-contains? bid)
  (define scope (current-arm-scope))
  (or (not scope) (ordered-map-ref scope bid #f)))

;; During `translate-gamma`'s find-branch-join walk, this parameter
;; holds the forward-reach intersection of the two outer arms (i.e.
;; the set of blocks reachable from both then-bid and else-bid).
;; Any block B whose full forward-reach is disjoint from this set is
;; an "exit subtree" — from the outer walk's perspective it can
;; never rejoin the shared territory, so `arm-advance` should treat
;; it like a single-block Term:ret / Term:throw leaf and skip past
;; it.  #f (default) disables the check; translate-gamma sets it to
;; a concrete set before calling find-branch-join.
(define current-shared-set (make-parameter #f))

;; Map of every loop header in the CFG to its Theta-Ctx.  Set once
;; per `cfg->rvsdg` invocation; consulted by `translate-segment`
;; whenever it lands on a new BlockId, which is how nested loops and
;; loops inside Gamma arms get dispatched into translate-theta
;; without any caller having to thread the ctx map manually.
(define current-theta-ctxs (make-parameter #f))

;; Map of every try-entry BlockId in the CFG to its Kappa-Group.  Set
;; once per `cfg->rvsdg` invocation from `'java/exception-table`;
;; consulted by `translate-segment` so any try-entry landed on during
;; recursion dispatches into translate-kappa-group in whichever region
;; we currently inhabit (parent, Gamma arm, Theta body, ...).  #f
;; (default) means no try/catch in the method.
(define current-kappa-groups (make-parameter #f))

;; OutputId of the currently-active handler region's exception-ref
;; region-arg output.  Set by `translate-kappa-group` while walking a
;; handler sub-region; consulted by `translate-vfinsn` to resolve
;; `'java/exception-ref` producers (synthesised by `jvm-method->cfg`
;; at handler entry) into the Kappa's exn-ref output rather than
;; allocating a dead `Simple 'java/exception-ref` node.  #f means "not
;; inside a handler region" — encountering the op under that default
;; signals a structural error.
(define current-exn-out (make-parameter #f))

;; ============================================================
;; Entry
;; ============================================================

(define (cfg->rvsdg cfg)
  (define local-count (cfg-get-info cfg 'java/max-local 0))
  (define param-names
    (or (cfg-get-info cfg 'java/ssa-param-names #f)
        (error 'cfg->rvsdg
               "ssa must publish 'java/ssa-param-names; run jvm-cfg->ssa first")))

  ;; Detect loop structure.  Each back-edge (latch . header)
  ;; contributes its latch-bid to the header's Theta-Ctx latches
  ;; set.  A reducible loop with a single latch produces a 1-entry
  ;; set; a 2-arm continue-diamond (see translate-theta) produces a
  ;; 2-entry set.  Nested loops have distinct headers and each gets
  ;; its own ctx entry.
  (define back-edges (cfg-find-back-edges cfg))
  (define theta-ctxs
    (for/fold ([m (ordered-map-empty block-id-compare)])
              ([be (in-pvector back-edges)])
      (define latch (car be))
      (define header (cdr be))
      (define existing (ordered-map-ref m header #f))
      (define latches
        (cond
          [existing (ordered-map-set (Theta-Ctx-latches existing) latch #t)]
          [else (ordered-map-set (ordered-map-empty block-id-compare)
                                 latch #t)]))
      (ordered-map-set m header (Theta-Ctx header latches))))

  (define region0 (region-empty))

  ;; 1. Parameter provider node.
  (define-values (region1 _param-nid _param-ins param-outs)
    (region-add-node region0 (Simple 'param) 0 local-count))

  ;; 2. Seed var → output with each parameter name.
  (define var->out-init
    (for/fold ([m (ordered-map-empty var-id-compare)])
              ([name (in-pvector param-names)]
               [oid (in-pvector param-outs)])
      (ordered-map-set m name oid)))

  ;; Compute one Kappa-Group per try window in the exception-table.
  ;; Empty map when the method has no try/catch.  translate-segment
  ;; consults this map to dispatch into translate-kappa-group whenever
  ;; it lands on a try-entry BlockId.
  (define kappa-groups (compute-kappa-groups cfg))

  ;; 3. Translate starting at entry; stop-bid = #f means "until a
  ;;    terminator or unreachable".
  (define-values (region-final var->out-final payload)
    (parameterize ([current-theta-ctxs theta-ctxs]
                   [current-kappa-groups kappa-groups])
      (translate-segment cfg (Cfg-entry cfg) #f region1 var->out-init)))

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
  (define ctx-here (and ctxs (ordered-map-ref ctxs start-bid #f)))
  (define kappa-here (and kappas (ordered-map-ref kappas start-bid #f)))
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
     (define blk (cfg-get-block cfg start-bid))
     (unless blk (error 'translate-segment "missing block ~a" start-bid))

     ;; A block may carry phis; those should already be materialised
     ;; as Gamma/Theta outputs by the caller (translate-gamma seeds
     ;; var->out with each phi's output).  We verify here that every
     ;; phi output is resolved -- an unresolved phi means the segment
     ;; was re-entered through a path that skipped its handler.
     (for ([phi (in-pvector (CfgBlock-phis blk))])
       (unless (ordered-map-ref var->out (PhiInsn-output phi) #f)
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
        (define then-blk (cfg-get-block cfg then-bid))
        (define else-blk (cfg-get-block cfg else-bid))
        (define then-term? (terminal-block? then-blk))
        (define else-term? (terminal-block? else-blk))
        (cond
          [(and then-term? else-term?)
           ;; Both arms are single-block exits (Term:ret / Term:throw).
           (translate-terminal-gamma cfg pred then-bid else-bid
                                     region1 var->out1)]
          [(or then-term? else-term?)
           ;; Asymmetric early-exit: one arm is a single block ending
           ;; in Term:ret / Term:throw; the other arm is the
           ;; continuation.  Lower as a Gamma whose exit sub-region
           ;; runs the early return / throw and whose continue sub-
           ;; region is a no-op.  After the Gamma falls through,
           ;; translate-segment resumes from the continuing branch's
           ;; first block in the outer region.
           (define-values (region2 var->out2 continue-bid)
             (translate-asymmetric-exit-gamma cfg pred
                                              then-bid then-term?
                                              else-bid else-term?
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
                (translate-asymmetric-exit-gamma cfg pred
                                                 then-bid (not then-reaches-stop?)
                                                 else-bid (not else-reaches-stop?)
                                                 region1 var->out1))
              (translate-segment cfg continue-bid stop-bid region2 var->out2)]
             [(and (arm-leaves-all-ret-throw? cfg then-reach)
                   (arm-leaves-all-ret-throw? cfg else-reach)
                   (reach-sets-disjoint? then-reach else-reach))
              ;; Multi-block terminal Gamma: build each sub-region by
              ;; recursing into translate-segment until it hits its
              ;; own ret / throw payload.
              (translate-terminal-gamma cfg pred then-bid else-bid
                                        region1 var->out1)]
             [else
              (define-values (region2 var->out2 join-bid)
                (translate-gamma cfg start-bid pred then-bid else-bid
                                 region1 var->out1))
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
           (translate-terminal-switch cfg value cases default-bid
                                      region1 var->out1)]
          [else
           (define-values (region2 var->out2 join-bid)
             (translate-convergent-switch cfg value cases default-bid
                                          region1 var->out1))
           (translate-segment cfg join-bid stop-bid region2 var->out2)])]
       [#f
        (values region1 var->out1 #f)])]))

;; ============================================================
;; Instruction-list translation (shared by parent and sub-regions)
;; ============================================================

(define (translate-insns insns region var->out)
  (for/fold ([r region] [m var->out])
            ([insn (in-pvector insns)])
    (translate-vfinsn insn r m)))

(define (translate-vfinsn insn region var->out)
  (define op (VfInsn-op insn))
  (define inputs (VfInsn-inputs insn))
  (define outputs (VfInsn-outputs insn))
  (define n-in (pvector-length inputs))
  (define n-out (pvector-length outputs))

  ;; Special case: `'java/exception-ref` is a synthetic single-output
  ;; producer inserted by `jvm-method->cfg` at each handler block's
  ;; entry (its outputs are the stack slots holding the caught
  ;; exception reference).  Under Kappa lowering the handler runs
  ;; inside a sub-region whose `(+ n-ctx 1)`-th region-arg output IS
  ;; the exception reference, installed by translate-kappa-group via
  ;; `current-exn-out`.  Rather than allocate a fresh
  ;; `Simple 'java/exception-ref` node (dead) we rebind the insn's
  ;; output VarId to the handler-region's exn-ref OutputId.
  ;; Encountering this op outside a handler region is a structural
  ;; error — the insn should only be reachable via translate-kappa-
  ;; group's handler-region walk.
  (cond
    [(eq? op 'java/exception-ref)
     (define exn-oid (current-exn-out))
     (unless exn-oid
       (error 'translate-vfinsn
              "'java/exception-ref insn encountered outside a Kappa handler region"))
     (unless (and (= n-in 0) (= n-out 1))
       (error 'translate-vfinsn
              "'java/exception-ref insn has unexpected arity (in=~a out=~a)"
              n-in n-out))
     (define out-var (pvector-ref outputs 0))
     (unless (VarId? out-var)
       (error 'translate-vfinsn
              "'java/exception-ref output is not a VarId: ~s" out-var))
     (values region (ordered-map-set var->out out-var exn-oid))]
    [else
     (translate-vfinsn/generic region var->out op inputs outputs n-in n-out)]))

(define (translate-vfinsn/generic region var->out op inputs outputs n-in n-out)
  ;; Resolve each input: VarId lookup, or materialise a const node.
  (define-values (region1 input-outputs)
    (for/fold ([r region] [acc (pvector-empty)])
              ([x (in-pvector inputs)])
      (cond
        [(VarId? x)
         (define oid (ordered-map-ref var->out x #f))
         (unless oid
           (error 'translate-vfinsn "undefined ~a for op ~a" x op))
         (values r (pvector-cons-right acc oid))]
        [else
         (define-values (r* _nid _ins outs)
           (region-add-node r (Simple (list 'const x)) 0 1))
         (values r* (pvector-cons-right acc (pvector-ref outs 0)))])))

  ;; Main op node.
  (define-values (region2 _nid in-ids out-ids)
    (region-add-node region1 (Simple op) n-in n-out))

  ;; Wire inputs.
  (define region3
    (for/fold ([r region2])
              ([src (in-pvector input-outputs)]
               [dst (in-pvector in-ids)])
      (define-values (r* _w) (region-add-wire r src dst))
      r*))

  ;; Extend var → output.
  (define var->out*
    (for/fold ([m var->out])
              ([vout (in-pvector outputs)]
               [oid (in-pvector out-ids)]
               #:when (VarId? vout))
      (ordered-map-set m vout oid)))

  (values region3 var->out*))

;; ============================================================
;; Gamma recovery (if-then-else)
;; ============================================================
;;
;; Scope: both `then-bid` and `else-bid` must be single blocks with
;; Term:jump to a common join (or one side IS the join with an empty
;; branch).  Any deviation raises a self-identifying error.
;;
;; Returns (values region var->out join-bid) where join-bid is the
;; block the caller should continue translating from.
(define (translate-gamma cfg cond-bid pred then-bid else-bid region var->out)
  ;; Compute the pair of full forward-reaches and their intersection
  ;; once, up-front.  The intersection ("shared set") is installed on
  ;; `current-shared-set` so that every helper that walks the arms —
  ;; `find-branch-join`, `arm-last-before-join`, `arm-blocks-set` —
  ;; can recognise multi-block exit subtrees nested inside either
  ;; arm and skip past them while searching for the outer join.
  (define then-full-reach (arm-reach-set cfg then-bid))
  (define else-full-reach (arm-reach-set cfg else-bid))
  (define shared-set (reach-set-intersection then-full-reach else-full-reach))
  (define-values (join-bid then-pred else-pred then-blocks else-blocks)
    (parameterize ([current-shared-set shared-set])
      (define jb (find-branch-join cfg then-bid else-bid))
      (when (or (equal? jb then-bid) (equal? jb else-bid))
        (error 'translate-gamma
               "empty branch (then=~a else=~a join=~a) not yet supported"
               then-bid else-bid jb))
      (values jb
              (arm-last-before-join cfg then-bid jb)
              (arm-last-before-join cfg else-bid jb)
              (arm-blocks-set cfg then-bid jb)
              (arm-blocks-set cfg else-bid jb))))

  (define join-blk (cfg-get-block cfg join-bid))
  (define phis (CfgBlock-phis join-blk))
  (define n-phis (pvector-length phis))

  ;; Context vars: only parent-scope vars each sub-region actually
  ;; reads (plus the phi source contributed by each arm's pred).  We
  ;; walk every block reachable in the arm (including blocks inside
  ;; nested Gammas), subtract arm-local definitions (VfInsn outputs +
  ;; phi outputs across the arm), and union the reads across both arms
  ;; so the Gamma sub-regions agree on input shape.
  (define ctx-set
    (collect-gamma-ctx cfg var->out then-blocks then-pred
                                    else-blocks else-pred phis))
  (define ctx-vars
    (for/pvector ([kv (in-ordered-map ctx-set)]) (car kv)))
  (define n-ctx (pvector-length ctx-vars))

  ;; Build the per-branch sub-region closures.
  (define then-region
    (build-branch-region cfg then-bid then-pred join-bid ctx-vars phis 'then))
  (define else-region
    (build-branch-region cfg else-bid else-pred join-bid ctx-vars phis 'else))

  (define gamma-val (Gamma (list then-region else-region)))

  ;; Install Gamma in the parent region: 1 predicate input + N
  ;; context inputs, N_phi outputs.
  (define-values (region1 _gnid g-ins g-outs)
    (region-add-node region gamma-val (add1 n-ctx) n-phis))

  ;; Wire predicate.
  (define pred-oid
    (or (ordered-map-ref var->out pred #f)
        (error 'translate-gamma "undefined predicate ~a" pred)))
  (define-values (region2 _pw)
    (region-add-wire region1 pred-oid (pvector-ref g-ins 0)))

  ;; Wire each context input.
  (define region3
    (for/fold ([r region2])
              ([ctx-var (in-pvector ctx-vars)]
               [i (in-naturals 1)])
      (define src-oid (ordered-map-ref var->out ctx-var #f))
      (define-values (r* _w) (region-add-wire r src-oid (pvector-ref g-ins i)))
      r*))

  ;; Extend var→out: every phi at the join becomes the corresponding
  ;; Gamma output.
  (define var->out*
    (for/fold ([m var->out])
              ([phi (in-pvector phis)]
               [oid (in-pvector g-outs)])
      (ordered-map-set m (PhiInsn-output phi) oid)))

  (values region3 var->out* join-bid))

;; ============================================================
;; Terminal Gamma (gamma-early-exit, both arms terminate)
;; ============================================================
;;
;; Scope: each arm's reach-set (walked forward over Term:jump /
;; Term:cond successors) has only Term:ret / Term:throw as leaf
;; terminators, and the two reach-sets are disjoint.  Single-block
;; arms (reach-set = {arm-bid}) are the common Java case; multi-block
;; arms carry internal Term:jump chains or nested Term:cond diamonds,
;; all ultimately terminating in ret/throw.  No phis on either arm's
;; entry block.
;;
;; The Gamma has 0 outputs.  Each sub-region:
;;   - region-arg(0, n-ctx) producer mirrors ctx-vars.
;;   - translate-segment is invoked with stop-bid=#f so the arm's
;;     entire reach-set is walked recursively (inner diamonds emit
;;     nested Gamma nodes inside the sub-region; inner loops emit
;;     Theta nodes; further exit patterns stay internal).
;;   - install-return / install-throw materialises the final sink
;;     using the payload returned by translate-segment.  If the arm
;;     already ended via a nested terminal Gamma ('already-terminated
;;     payload), no additional sink is installed.

(define (terminal-block? blk)
  (define t (CfgBlock-terminator blk))
  (or (Term:ret? t) (Term:throw? t)))

;; Forward reach-set from start-bid, walking only via Term:jump and
;; Term:cond successors.  Blocks whose terminator is something else
;; (ret / throw / switch / #f) are included as leaves but not
;; expanded.  Respects `current-arm-scope`: successors outside the
;; active scope are treated as boundary edges and not included in
;; the walk (typical inside a Theta body, where scope=body-blocks
;; and a Term:jump to the loop header crosses the back-edge).
;; Returns an ordered-map used as a set of BlockId -> #t.
(define (arm-reach-set cfg start-bid)
  (let loop ([work (list start-bid)]
             [seen (ordered-map-empty block-id-compare)])
    (cond
      [(null? work) seen]
      [else
       (define b (car work))
       (define rest (cdr work))
       (cond
         [(ordered-map-ref seen b #f) (loop rest seen)]
         [(not (arm-scope-contains? b)) (loop rest seen)]
         [else
          (define seen* (ordered-map-set seen b #t))
          (define succs
            (match (CfgBlock-terminator (cfg-get-block cfg b))
              [(Term:jump n) (list n)]
              [(Term:cond _ tb eb) (list tb eb)]
              [_ '()]))
          (loop (append succs rest) seen*)])])))

;; True iff some block in `reach-set` has an immediate successor
;; equal to `stop-bid`, or the reach-set itself contains `stop-bid`.
;; Needed because `arm-reach-set` stops at scope boundaries, so under
;; a scoped walk `stop-bid` (typically outside the scope) never lands
;; in the set directly — we must probe the boundary terminators.  At
;; top level (scope=#f) the direct lookup succeeds and this degrades
;; to the classic membership check.
(define (reach-set-reaches-stop? cfg reach-set stop-bid)
  (and stop-bid
       (or (ordered-map-ref reach-set stop-bid #f)
           (for/or ([kv (in-ordered-map reach-set)])
             (match (CfgBlock-terminator (cfg-get-block cfg (car kv)))
               [(Term:jump n) (equal? n stop-bid)]
               [(Term:cond _ tb eb)
                (or (equal? tb stop-bid) (equal? eb stop-bid))]
               [_ #f])))
       #t))

;; Every block in the reach-set has a terminator that is either
;; internal to the arm (Term:jump / Term:cond) or is a ret / throw
;; leaf.  Rejects switch / unreachable / #f leaves because they
;; cannot be materialised as a Gamma-arm sink.
(define (arm-leaves-all-ret-throw? cfg reach-set)
  (for/and ([kv (in-ordered-map reach-set)])
    (define t (CfgBlock-terminator (cfg-get-block cfg (car kv))))
    (or (Term:jump? t) (Term:cond? t)
        (Term:ret? t) (Term:throw? t))))

(define (reach-sets-disjoint? s1 s2)
  (for/and ([kv (in-ordered-map s1)])
    (not (ordered-map-ref s2 (car kv) #f))))

(define (reach-sets-all-pairwise-disjoint? sets)
  (let loop ([xs sets])
    (cond
      [(or (null? xs) (null? (cdr xs))) #t]
      [else
       (define head (car xs))
       (and (for/and ([other (in-list (cdr xs))])
              (reach-sets-disjoint? head other))
            (loop (cdr xs)))])))

(define (reach-set-intersection s1 s2)
  (for/fold ([s (ordered-map-empty block-id-compare)])
            ([kv (in-ordered-map s1)]
             #:when (ordered-map-ref s2 (car kv) #f))
    (ordered-map-set s (car kv) #t)))

;; Walk every block in the arm's reach-set and collect parent-scope
;; VarIds read by any VfInsn input or by the block's terminator
;; (Term:ret values, Term:throw exception, Term:cond predicate),
;; excluding VarIds locally defined anywhere in the arm (union of phi
;; outputs + VfInsn outputs across the reach-set).  Returns an
;; ordered-map used as a set of VarId -> #t.
(define (collect-arm-ctx cfg parent-var->out reach-set)
  (define locals
    (for/fold ([s (ordered-map-empty var-id-compare)])
              ([kv (in-ordered-map reach-set)])
      (define blk (cfg-get-block cfg (car kv)))
      (define s1
        (for/fold ([s s]) ([phi (in-pvector (CfgBlock-phis blk))])
          (ordered-map-set s (PhiInsn-output phi) #t)))
      (for/fold ([s s1]) ([insn (in-pvector (CfgBlock-insns blk))])
        (for/fold ([s s]) ([o (in-pvector (VfInsn-outputs insn))]
                           #:when (VarId? o))
          (ordered-map-set s o #t)))))
  (define (consider x acc)
    (cond
      [(and (VarId? x)
            (not (ordered-map-ref locals x #f))
            (ordered-map-ref parent-var->out x #f))
       (ordered-map-set acc x #t)]
      [else acc]))
  (for/fold ([acc (ordered-map-empty var-id-compare)])
            ([kv (in-ordered-map reach-set)])
    (define blk (cfg-get-block cfg (car kv)))
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
;; the sink to install.  No phis are allowed on the arm's entry
;; block (other arm preds are filtered by the dispatch's disjointness
;; check).
(define (build-exit-arm-region cfg branch-bid ctx-vars which)
  (define n-ctx (pvector-length ctx-vars))
  (define entry-blk (cfg-get-block cfg branch-bid))
  (unless (= 0 (pvector-length (CfgBlock-phis entry-blk)))
    (error 'build-exit-arm-region
           "exit arm ~a (~a) entry block has phis, not yet supported"
           branch-bid which))
  (define sub0 (region-empty))
  (define-values (sub1 _arg-nid _arg-ins arg-outs)
    (region-add-node sub0 (Simple (list 'region-arg n-ctx)) 0 n-ctx))
  (define sub-var->out
    (for/fold ([m (ordered-map-empty var-id-compare)])
              ([v (in-pvector ctx-vars)]
               [oid (in-pvector arg-outs)])
      (ordered-map-set m v oid)))
  (define-values (sub2 sub-var->out2 payload)
    (translate-segment cfg branch-bid #f sub1 sub-var->out))
  (match payload
    [(cons 'ret vs)      (install-return sub2 sub-var->out2 vs)]
    [(cons 'throw v)     (install-throw  sub2 sub-var->out2 v)]
    ['already-terminated sub2]
    [other (error 'build-exit-arm-region
                  "exit arm ~a (~a) did not terminate with ret/throw: payload=~s"
                  branch-bid which other)]))

;; Returns (values region var->out 'already-terminated).  Caller
;; treats the terminal-Gamma as the segment's terminator and stops
;; translating further blocks.
(define (translate-terminal-gamma cfg pred then-bid else-bid region var->out)
  (define then-reach (arm-reach-set cfg then-bid))
  (define else-reach (arm-reach-set cfg else-bid))
  (define then-ctx (collect-arm-ctx cfg var->out then-reach))
  (define else-ctx (collect-arm-ctx cfg var->out else-reach))
  (define ctx-set
    (for/fold ([acc then-ctx])
              ([kv (in-ordered-map else-ctx)])
      (ordered-map-set acc (car kv) #t)))
  (define ctx-vars
    (for/pvector ([kv (in-ordered-map ctx-set)]) (car kv)))
  (define n-ctx (pvector-length ctx-vars))

  (define then-region (build-exit-arm-region cfg then-bid ctx-vars 'then))
  (define else-region (build-exit-arm-region cfg else-bid ctx-vars 'else))

  (define gamma-val (Gamma (list then-region else-region)))

  ;; Install Gamma in parent: 1 predicate input + n-ctx ctx inputs,
  ;; 0 outputs.
  (define-values (region1 _gnid g-ins _g-outs)
    (region-add-node region gamma-val (add1 n-ctx) 0))

  ;; Wire predicate.
  (define pred-oid
    (or (ordered-map-ref var->out pred #f)
        (error 'translate-terminal-gamma
               "undefined predicate ~a" pred)))
  (define-values (region2 _pw)
    (region-add-wire region1 pred-oid (pvector-ref g-ins 0)))

  ;; Wire ctx inputs.
  (define region3
    (for/fold ([r region2])
              ([ctx-var (in-pvector ctx-vars)]
               [i (in-naturals 1)])
      (define src-oid
        (or (ordered-map-ref var->out ctx-var #f)
            (error 'translate-terminal-gamma
                   "undefined ctx var ~a" ctx-var)))
      (define-values (r* _w) (region-add-wire r src-oid (pvector-ref g-ins i)))
      r*))

  (values region3 var->out 'already-terminated))

;; ============================================================
;; Terminal switch → N+1 arm Gamma
;; ============================================================
;;
;; Shape: every target of a Term:switch (the default block and each
;; case block in order) has a reach-set whose leaves are all ret /
;; throw.  Lowered to a Gamma with (1 + n-cases) sub-regions in order
;; [default, case_0, ..., case_{n-1}].  Each sub-region's Region.info
;; maps 'java/switch-case-key to either the symbol 'default or the
;; integer case key, so the key-to-arm correspondence is preserved
;; in the RVSDG.  The Gamma's predicate input is the raw switch
;; value; a later pass / backend is responsible for computing the
;; arm index from the value against the recorded keys.  Ctx inputs
;; are the union of every arm's live parent-scope reads.  0 outputs
;; (terminal).

(define (region-info-set r key val)
  (struct-copy Region r
               [info (ordered-map-set (Region-info r) key val)]))

(define (translate-terminal-switch cfg value cases default-bid region var->out)
  (define case-bids
    (for/list ([kv (in-pvector cases)]) (cdr kv)))
  (define all-bids (cons default-bid case-bids))

  ;; Per-arm reach-sets; every arm must be ret / throw leaves only.
  (define reaches
    (for/list ([b (in-list all-bids)])
      (define r (arm-reach-set cfg b))
      (unless (arm-leaves-all-ret-throw? cfg r)
        (error 'translate-terminal-switch
               "switch arm starting at ~a has non-ret/throw leaves"
               b))
      r))

  ;; Pairwise disjointness: rejects convergent switches (arms sharing
  ;; a join block).  Duplicating the join block into N sub-regions is
  ;; a future extension.
  (for ([r1 (in-list reaches)] [b1 (in-list all-bids)] [i (in-naturals)])
    (for ([r2 (in-list reaches)] [b2 (in-list all-bids)] [j (in-naturals)])
      (when (and (< i j)
                 (not (reach-sets-disjoint? r1 r2)))
        (error 'translate-terminal-switch
               "switch arms ~a and ~a share blocks (convergent switch); only terminal disjoint arms supported"
               b1 b2))))

  ;; Union of all arms' ctx-vars.
  (define ctx-set
    (for/fold ([acc (ordered-map-empty var-id-compare)])
              ([reach (in-list reaches)])
      (define arm-ctx (collect-arm-ctx cfg var->out reach))
      (for/fold ([s acc]) ([kv (in-ordered-map arm-ctx)])
        (ordered-map-set s (car kv) #t))))
  (define ctx-vars
    (for/pvector ([kv (in-ordered-map ctx-set)]) (car kv)))
  (define n-ctx (pvector-length ctx-vars))

  ;; Build each sub-region; tag its info with the switch-case-key.
  (define (build-tagged-arm bid key-tag which)
    (define sub (build-exit-arm-region cfg bid ctx-vars which))
    (region-info-set sub 'java/switch-case-key key-tag))

  (define default-region
    (build-tagged-arm default-bid 'default 'switch-default))
  (define case-regions
    (for/list ([kv (in-pvector cases)]
               [i (in-naturals 0)])
      (build-tagged-arm (cdr kv) (car kv) (list 'switch-case i))))

  (define sub-regions (cons default-region case-regions))
  (define gamma-val (Gamma sub-regions))

  ;; Install Gamma: 1 pred input + n-ctx ctx inputs, 0 outputs.
  (define-values (region1 _gnid g-ins _g-outs)
    (region-add-node region gamma-val (add1 n-ctx) 0))

  ;; Wire predicate.
  (define pred-oid
    (or (ordered-map-ref var->out value #f)
        (error 'translate-terminal-switch
               "undefined switch value ~a" value)))
  (define-values (region2 _pw)
    (region-add-wire region1 pred-oid (pvector-ref g-ins 0)))

  ;; Wire ctx inputs.
  (define region3
    (for/fold ([r region2])
              ([ctx-var (in-pvector ctx-vars)]
               [i (in-naturals 1)])
      (define src-oid
        (or (ordered-map-ref var->out ctx-var #f)
            (error 'translate-terminal-switch
                   "undefined ctx var ~a" ctx-var)))
      (define-values (r* _w) (region-add-wire r src-oid (pvector-ref g-ins i)))
      r*))

  (values region3 var->out 'already-terminated))

;; ============================================================
;; Convergent switch → N+1 arm Gamma (with phi outputs)
;; ============================================================
;;
;; Shape: every target of a Term:switch reaches a common join block,
;; reusing the same mechanics as translate-gamma's convergent case
;; generalised to N+1 arms.  Each arm is walked from its branch-bid
;; to join-bid via arm-advance; the join block's phis become the
;; Gamma's outputs, each arm contributes the phi source VarId
;; corresponding to its arm-last-before-join predecessor.  Sub-
;; region order is [default, case_0, ..., case_{n-1}]; each sub-
;; region's Region.info records 'java/switch-case-key.  Caller
;; resumes translate-segment from join-bid in the outer region.

(define (translate-convergent-switch cfg value cases default-bid region var->out)
  (define case-bids (for/list ([kv (in-pvector cases)]) (cdr kv)))
  (define all-bids (cons default-bid case-bids))
  (unless (>= (length all-bids) 2)
    (error 'translate-convergent-switch
           "switch has fewer than 2 arms; cannot form convergent join"))

  ;; Per-arm full reach-sets; install intersection as the shared set
  ;; so the arm-walking helpers recognise multi-block exit subtrees.
  (define reaches
    (for/list ([b (in-list all-bids)]) (arm-reach-set cfg b)))
  (define shared-set
    (for/fold ([s (car reaches)]) ([r (in-list (cdr reaches))])
      (reach-set-intersection s r)))

  (define-values (join-bid arm-preds arm-blocks-list)
    (parameterize ([current-shared-set shared-set])
      (define jb (find-branch-join cfg (car all-bids) (cadr all-bids)))
      (when (for/or ([b (in-list all-bids)]) (equal? b jb))
        (error 'translate-convergent-switch
               "empty arm (one of ~s == join ~a) not yet supported"
               all-bids jb))
      (define preds
        (for/list ([b (in-list all-bids)])
          (arm-last-before-join cfg b jb)))
      (define blocks-list
        (for/list ([b (in-list all-bids)])
          (arm-blocks-set cfg b jb)))
      (values jb preds blocks-list)))

  (define join-blk (cfg-get-block cfg join-bid))
  (define phis (CfgBlock-phis join-blk))
  (define n-phis (pvector-length phis))

  ;; Context vars: union over all arms of parent-scope reads + per-arm
  ;; phi source contributions.
  (define ctx-set
    (collect-switch-arms-ctx cfg var->out arm-blocks-list arm-preds phis))
  (define ctx-vars
    (for/pvector ([kv (in-ordered-map ctx-set)]) (car kv)))
  (define n-ctx (pvector-length ctx-vars))

  (define (build-tagged-branch-region bid arm-pred key-tag which)
    (define base
      (build-branch-region cfg bid arm-pred join-bid ctx-vars phis which))
    (region-info-set base 'java/switch-case-key key-tag))

  (define default-region
    (build-tagged-branch-region default-bid (car arm-preds)
                                'default 'switch-default))
  (define case-regions
    (for/list ([kv (in-pvector cases)]
               [b (in-list case-bids)]
               [p (in-list (cdr arm-preds))]
               [i (in-naturals 0)])
      (build-tagged-branch-region b p (car kv) (list 'switch-case i))))

  (define sub-regions (cons default-region case-regions))
  (define gamma-val (Gamma sub-regions))

  ;; Install Gamma: 1 pred + n-ctx inputs, n-phis outputs.
  (define-values (region1 _gnid g-ins g-outs)
    (region-add-node region gamma-val (add1 n-ctx) n-phis))

  (define pred-oid
    (or (ordered-map-ref var->out value #f)
        (error 'translate-convergent-switch
               "undefined switch value ~a" value)))
  (define-values (region2 _pw)
    (region-add-wire region1 pred-oid (pvector-ref g-ins 0)))

  (define region3
    (for/fold ([r region2])
              ([ctx-var (in-pvector ctx-vars)]
               [i (in-naturals 1)])
      (define src-oid
        (or (ordered-map-ref var->out ctx-var #f)
            (error 'translate-convergent-switch
                   "undefined ctx var ~a" ctx-var)))
      (define-values (r* _w) (region-add-wire r src-oid (pvector-ref g-ins i)))
      r*))

  ;; Bind join phi outputs to corresponding Gamma outputs.
  (define var->out*
    (for/fold ([m var->out])
              ([phi (in-pvector phis)]
               [oid (in-pvector g-outs)])
      (ordered-map-set m (PhiInsn-output phi) oid)))

  (values region3 var->out* join-bid))

;; N-arm generalisation of collect-gamma-ctx: walks each arm's
;; blocks + its phi source contribution.
(define (collect-switch-arms-ctx cfg parent-var->out
                                 arm-blocks-list arm-preds phis)
  (define (arm-local-defs blocks)
    (for/fold ([s (ordered-map-empty var-id-compare)])
              ([kv (in-ordered-map blocks)])
      (define blk (cfg-get-block cfg (car kv)))
      (define s1
        (for/fold ([s s]) ([phi (in-pvector (CfgBlock-phis blk))])
          (ordered-map-set s (PhiInsn-output phi) #t)))
      (for/fold ([s s1]) ([insn (in-pvector (CfgBlock-insns blk))])
        (for/fold ([s s]) ([o (in-pvector (VfInsn-outputs insn))]
                           #:when (VarId? o))
          (ordered-map-set s o #t)))))
  (define (walk-arm blocks arm-pred acc)
    (define locals (arm-local-defs blocks))
    (define acc*
      (for/fold ([a acc])
                ([kv (in-ordered-map blocks)])
        (define insns (CfgBlock-insns (cfg-get-block cfg (car kv))))
        (for/fold ([a a]) ([insn (in-pvector insns)])
          (for/fold ([a a]) ([x (in-pvector (VfInsn-inputs insn))])
            (cond
              [(and (VarId? x)
                    (not (ordered-map-ref locals x #f))
                    (ordered-map-ref parent-var->out x #f))
               (ordered-map-set a x #t)]
              [else a])))))
    (for/fold ([a acc*]) ([phi (in-pvector phis)])
      (define src (pick-phi-source phi arm-pred 'switch-ctx))
      (cond
        [(and (VarId? src)
              (ordered-map-ref parent-var->out src #f))
         (ordered-map-set a src #t)]
        [else a])))
  (for/fold ([acc (ordered-map-empty var-id-compare)])
            ([blocks (in-list arm-blocks-list)]
             [p (in-list arm-preds)])
    (walk-arm blocks p acc)))

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

(define (translate-asymmetric-exit-gamma cfg pred
                                         then-bid then-exits?
                                         else-bid else-exits?
                                         region var->out)
  (define exit-bid     (if then-exits? then-bid else-bid))
  (define continue-bid (if then-exits? else-bid then-bid))

  (define exit-reach (arm-reach-set cfg exit-bid))
  (define ctx-set (collect-arm-ctx cfg var->out exit-reach))
  (define ctx-vars
    (for/pvector ([kv (in-ordered-map ctx-set)]) (car kv)))
  (define n-ctx (pvector-length ctx-vars))

  (define exit-region (build-exit-arm-region cfg exit-bid ctx-vars 'exit))
  (define noop-region (build-noop-arm-region n-ctx))

  ;; Place sub-regions in [then, else] order so the raw predicate
  ;; (truthy -> region 0) keeps original Term:cond semantics.
  (define then-region (if then-exits? exit-region noop-region))
  (define else-region (if then-exits? noop-region exit-region))

  (define gamma-val (Gamma (list then-region else-region)))

  (define-values (region1 _gnid g-ins _g-outs)
    (region-add-node region gamma-val (add1 n-ctx) 0))

  (define pred-oid
    (or (ordered-map-ref var->out pred #f)
        (error 'translate-asymmetric-exit-gamma
               "undefined predicate ~a" pred)))
  (define-values (region2 _pw)
    (region-add-wire region1 pred-oid (pvector-ref g-ins 0)))

  (define region3
    (for/fold ([r region2])
              ([ctx-var (in-pvector ctx-vars)]
               [i (in-naturals 1)])
      (define src-oid
        (or (ordered-map-ref var->out ctx-var #f)
            (error 'translate-asymmetric-exit-gamma
                   "undefined ctx var ~a" ctx-var)))
      (define-values (r* _w) (region-add-wire r src-oid (pvector-ref g-ins i)))
      r*))

  (values region3 var->out continue-bid))

;; Advance one step through an arm's control flow, recursively resolving
;; nested Term:cond blocks by finding their own diamond-joins.  Returns
;; the next BlockId to visit, or #f when the terminator can't be
;; advanced (ret / throw / switch / unreachable, or — when
;; `current-arm-scope` is set — when the next bid would lie outside
;; that scope).
;;
;; Special case for early-exit-inside-region: if the Term:cond has
;; exactly one arm that is a single-block Term:ret / Term:throw (and
;; the other arm is not), advance through the non-exit arm rather
;; than trying to converge the two.  When translate-segment later
;; visits this block, its own Term:cond dispatch lowers the inner
;; cond as an asymmetric early-exit Gamma inside the outer arm's
;; sub-region, so the outer walk just needs to skip past the exit
;; arm to find the outer join.  A Term:cond where both arms are
;; single-block exits still returns #f (the outer arm is dead past
;; this point, and any attempt to use that arm in a standard diamond
;; will error later with a clear message).
(define (arm-advance cfg bid)
  (define term (CfgBlock-terminator (cfg-get-block cfg bid)))
  (match term
    [(Term:jump n) (and (arm-scope-contains? n) n)]
    [(Term:cond _ tb eb)
     (cond
       [(not (and (arm-scope-contains? tb) (arm-scope-contains? eb))) #f]
       [else
        (define tb-exit? (arm-is-exit? cfg tb))
        (define eb-exit? (arm-is-exit? cfg eb))
        (cond
          [(and tb-exit? eb-exit?) #f]
          [tb-exit? eb]
          [eb-exit? tb]
          [else (find-branch-join cfg tb eb)])])]
    [_ #f]))

;; A block B counts as an "exit" from the outer walk's perspective
;; if advancing into B's forward-reach will never rejoin the shared
;; territory.  Two cases:
;;   - Single-block terminal: Term:ret / Term:throw block.
;;   - Multi-block exit: B's full forward-reach is entirely ret/throw
;;     leaves AND the reach is disjoint from `current-shared-set`
;;     (the pair of outer-arm reaches' intersection).  The shared
;;     set must be active — outside a translate-gamma context this
;;     case degrades to `#f`, which matches the pre-existing single-
;;     block-only behavior.
(define (arm-is-exit? cfg bid)
  (or (terminal-block? (cfg-get-block cfg bid))
      (arm-is-multi-block-exit? cfg bid)))

(define (arm-is-multi-block-exit? cfg bid)
  (define shared (current-shared-set))
  (cond
    [(not shared) #f]
    [else
     (define reach (arm-reach-set cfg bid))
     (and (arm-leaves-all-ret-throw? cfg reach)
          (reach-sets-disjoint? reach shared))]))

;; Walk the then-arm forward (via arm-advance), then walk the else-arm
;; until we hit a bid visited on the then-arm.  That bid is the Gamma
;; join block.  arm-advance recurses on nested Term:conds inside an
;; arm, so diamonds-within-diamonds are allowed as long as each inner
;; cond itself converges.  Errors if an arm's terminator can't be
;; advanced (ret/throw/switch) before converging.
(define (find-branch-join cfg then-bid else-bid)
  (define then-reach
    (let loop ([b then-bid] [s (ordered-map-empty block-id-compare)])
      (cond
        [(ordered-map-ref s b #f) s]
        [else
         (define s2 (ordered-map-set s b #t))
         (define nxt (arm-advance cfg b))
         (cond [nxt (loop nxt s2)]
               [else s2])])))
  (let loop ([b else-bid])
    (cond
      [(ordered-map-ref then-reach b #f) b]
      [else
       (define nxt (arm-advance cfg b))
       (unless nxt
         (error 'find-branch-join
                "branches do not converge (then=~a else=~a dead-end at ~a: ~s)"
                then-bid else-bid b (CfgBlock-terminator (cfg-get-block cfg b))))
       (loop nxt)])))

;; Walk the arm from branch-bid, step-by-step via arm-advance, until
;; reaching join-bid.  Return the last BlockId visited just before
;; join-bid (the phi-source predecessor key).  Errors if the arm
;; never reaches join-bid.
(define (arm-last-before-join cfg branch-bid join-bid)
  (let loop ([prev #f] [b branch-bid])
    (cond
      [(equal? b join-bid)
       (or prev
           (error 'arm-last-before-join
                  "empty arm (~a == join) not yet supported" branch-bid))]
      [else
       (define nxt (arm-advance cfg b))
       (unless nxt
         (error 'arm-last-before-join
                "dead-end at ~a walking toward join ~a" b join-bid))
       (loop b nxt)])))

;; Set of BlockIds on any path from branch-bid to (but not including)
;; join-bid via arm-internal control flow (Term:jump and Term:cond
;; only).  Used to collect parent-scope reads across the arm.
;; Successors that escape the active `current-arm-scope` (when set)
;; are skipped — they are by definition outside the arm we're
;; describing.
(define (arm-blocks-set cfg branch-bid join-bid)
  (define (walk bid visited)
    (cond
      [(equal? bid join-bid) visited]
      [(not (arm-scope-contains? bid)) visited]
      [(ordered-map-ref visited bid #f) visited]
      [else
       (define v2 (ordered-map-set visited bid #t))
       (define term (CfgBlock-terminator (cfg-get-block cfg bid)))
       (match term
         [(Term:jump n) (walk n v2)]
         [(Term:cond _ tb eb) (walk eb (walk tb v2))]
         [(Term:ret _) v2]
         [(Term:throw _) v2]
         [_ (error 'arm-blocks-set
                   "unsupported arm-internal terminator ~s at ~a"
                   term bid)])]))
  (walk branch-bid (ordered-map-empty block-id-compare)))

;; Collect every BlockId reachable from `start` without ever
;; crossing the loop's back-edge.  Conceptually: run a BFS in the
;; CFG with `header` removed (so a latch→header edge is a
;; boundary).  The latch itself is included but not expanded — its
;; Term:jump target is exactly `header`, which we would skip
;; anyway.  Non-jump / non-cond terminators (ret / throw / switch)
;; are tolerated as dead-ends because this helper is also used to
;; probe the exit arm: the exit arm of a while-loop routinely ends
;; in Term:ret.  translate-segment will raise a precise error later
;; if any such terminator actually sits inside the *body* region.
;;
;; Note we deliberately avoid `arm-advance` / `find-branch-join`
;; here: those helpers were designed for Gamma arms that have no
;; loops in them, and they would recurse indefinitely if asked to
;; "advance past" the latch (whose only successor is the header,
;; which in turn has a cond arm that re-enters the body).
(define (theta-body-blocks cfg start latches header)
  (let loop ([work (list start)] [s (ordered-map-empty block-id-compare)])
    (cond
      [(null? work) s]
      [else
       (define b (car work))
       (define rest (cdr work))
       (cond
         [(equal? b header) (loop rest s)]
         [(ordered-map-ref s b #f) (loop rest s)]
         [else
          (define s* (ordered-map-set s b #t))
          (cond
            [(ordered-map-ref latches b #f)
             ;; Latch included; its only outgoing edge goes to
             ;; header, which is the back-edge we refuse to cross.
             (loop rest s*)]
            [else
             (define succs
               (match (CfgBlock-terminator (cfg-get-block cfg b))
                 [(Term:jump n) (list n)]
                 [(Term:cond _ tb eb) (list tb eb)]
                 [_ '()]))
             (loop (append succs rest) s*)])])])))

;; Body-arm check: starting from the cond-arm bid, do we reach any
;; of the loop's latches while remaining inside the body (i.e.,
;; never crossing the back-edge into `header`)?  Implemented as set-
;; membership in `theta-body-blocks`, so it naturally tolerates
;; inner Gamma diamonds just like the block-set walk.
(define (body-arm-reaches-any-latch? cfg start latches header)
  (define reach (theta-body-blocks cfg start latches header))
  (for/or ([kv (in-ordered-map latches)])
    (and (ordered-map-ref reach (car kv) #f) #t)))

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
    (for/fold ([m (ordered-map-empty var-id-compare)])
              ([v (in-pvector ctx-vars)]
               [oid (in-pvector arg-outs)])
      (ordered-map-set m v oid)))

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
      (or (ordered-map-ref sub-var->out2 src-var #f)
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
;; Returns an ordered-map used as a set of VarId -> #t.
(define (collect-gamma-ctx cfg parent-var->out
                           then-blocks then-pred
                           else-blocks else-pred phis)
  ;; Union of all locally-defined VarIds across every block in the arm
  ;; (phi outputs + VfInsn outputs).  Safe over-approximation: SSA
  ;; scoping already ensures outer-parent reads go through phi outputs
  ;; at the outer join, not arm-internal defs.
  (define (arm-local-defs blocks)
    (for/fold ([s (ordered-map-empty var-id-compare)])
              ([kv (in-ordered-map blocks)])
      (define blk (cfg-get-block cfg (car kv)))
      (define s*
        (for/fold ([s s]) ([phi (in-pvector (CfgBlock-phis blk))])
          (ordered-map-set s (PhiInsn-output phi) #t)))
      (for/fold ([s s*]) ([insn (in-pvector (CfgBlock-insns blk))])
        (for/fold ([s s]) ([o (in-pvector (VfInsn-outputs insn))]
                           #:when (VarId? o))
          (ordered-map-set s o #t)))))
  (define (walk-arm blocks arm-pred acc)
    (define locals (arm-local-defs blocks))
    (define acc*
      (for/fold ([a acc])
                ([kv (in-ordered-map blocks)])
        (define insns (CfgBlock-insns (cfg-get-block cfg (car kv))))
        (for/fold ([a a]) ([insn (in-pvector insns)])
          (for/fold ([a a]) ([x (in-pvector (VfInsn-inputs insn))])
            (cond
              [(and (VarId? x)
                    (not (ordered-map-ref locals x #f))
                    (ordered-map-ref parent-var->out x #f))
               (ordered-map-set a x #t)]
              [else a])))))
    ;; Phi source contributed at the outer join from this arm's pred.
    (for/fold ([a acc*]) ([phi (in-pvector phis)])
      (define src (pick-phi-source phi arm-pred 'gamma-ctx))
      (cond
        [(and (VarId? src)
              (ordered-map-ref parent-var->out src #f))
         (ordered-map-set a src #t)]
        [else a])))
  (walk-arm else-blocks else-pred
            (walk-arm then-blocks then-pred
                      (ordered-map-empty var-id-compare))))

;; Collect parent-scope VarIds actually read inside the loop.  Walks
;; the header block and every block on the body chain (body-entry →
;; ... → latch), tracking locally-defined outputs per block (seeded
;; with header phi outputs, which are local to the sub-region).  Any
;; VfInsn input that is a VarId, not locally-defined in the block's
;; prefix, and present in `parent-var->out` counts as a read.
;; `body-blocks` is an ordered-set of BlockIds (as produced by
;; `theta-body-blocks`); a single-block body degenerates to just the
;; latch entry.  Returns an ordered-map used as a set of VarId -> #t.
(define (collect-theta-ctx cfg parent-var->out header-bid body-blocks phis)
  (define phi-out-set
    (for/fold ([m (ordered-map-empty var-id-compare)])
              ([p (in-pvector phis)])
      (ordered-map-set m (PhiInsn-output p) #t)))
  (define (walk-block bid acc)
    (define blk (cfg-get-block cfg bid))
    (define insns (CfgBlock-insns blk))
    (define-values (_ acc*)
      (for/fold ([locals phi-out-set]
                 [a acc])
                ([insn (in-pvector insns)])
        (define a*
          (for/fold ([a a]) ([x (in-pvector (VfInsn-inputs insn))])
            (cond
              [(and (VarId? x)
                    (not (ordered-map-ref locals x #f))
                    (ordered-map-ref parent-var->out x #f))
               (ordered-map-set a x #t)]
              [else a])))
        (define locals*
          (for/fold ([s locals]) ([o (in-pvector (VfInsn-outputs insn))]
                                  #:when (VarId? o))
            (ordered-map-set s o #t)))
        (values locals* a*)))
    acc*)
  (for/fold ([acc (walk-block header-bid
                              (ordered-map-empty var-id-compare))])
            ([kv (in-ordered-map body-blocks)])
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
      (define blk (cfg-get-block cfg bid))
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
      (for/list ([kv (in-ordered-map reach)]
                 #:when (ordered-map-ref latches (car kv) #f))
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
    (for/and ([kv (in-ordered-map reach)])
      (match (CfgBlock-terminator (cfg-get-block cfg (car kv)))
        [(Term:jump n) (or (ordered-map-ref reach n #f)
                           (equal? n header-bid))]
        [(Term:cond _ tb eb) (and (ordered-map-ref reach tb #f)
                                  (ordered-map-ref reach eb #f))]
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
    (for/fold ([s then-ctx]) ([kv (in-ordered-map else-ctx)])
      (ordered-map-set s (car kv) #t)))
  (define combined-ctx-vars
    (for/pvector ([kv (in-ordered-map combined-ctx-set)]) (car kv)))
  (define n-ctx (pvector-length combined-ctx-vars))
  (define n-phis (pvector-length phis))

  (define (build-arm arm-bid arm-latch-bid which)
    (define arm0 (region-empty))
    (define-values (arm1 _arg-nid _arg-ins arg-outs)
      (region-add-node arm0 (Simple (list 'region-arg n-ctx)) 0 n-ctx))
    (define arm-var->out
      (for/fold ([m (ordered-map-empty var-id-compare)])
                ([v (in-pvector combined-ctx-vars)]
                 [oid (in-pvector arg-outs)])
        (ordered-map-set m v oid)))
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
        (or (ordered-map-ref arm-var->out2 src-var #f)
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
    (or (ordered-map-ref sub-var->out1 pred-var #f)
        (error 'translate-theta-two-latch-body
               "dispatch predicate ~a undefined" pred-var)))
  (define-values (sub-p _pw)
    (region-add-wire sub-g pred-oid (pvector-ref g-ins 0)))
  (define sub-wired
    (for/fold ([r sub-p])
              ([v (in-pvector combined-ctx-vars)]
               [i (in-naturals 1)])
      (define src-oid
        (or (ordered-map-ref sub-var->out1 v #f)
            (error 'translate-theta-two-latch-body "ctx var ~a undefined" v)))
      (define-values (r* _w) (region-add-wire r src-oid (pvector-ref g-ins i)))
      r*))

  ;; Bind each header-phi-output VarId to its Gamma output so the
  ;; outer update-oids computation can look it up uniformly.
  (define sub-var->out-final
    (for/fold ([m sub-var->out1])
              ([phi (in-pvector phis)]
               [oid (in-pvector g-outs)])
      (ordered-map-set m (PhiInsn-output phi) oid)))

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
  (define n-latches (ordered-map-count latches))
  (define header-blk (cfg-get-block cfg header-bid))
  (define phis (CfgBlock-phis header-blk))

  ;; Build phi index: phi-output VarId -> PhiInsn.  Every loop-carried
  ;; parent var has an entry here; every other parent var is
  ;; loop-invariant and passes through unchanged.
  (define phi-by-output
    (for/fold ([m (ordered-map-empty var-id-compare)])
              ([p (in-pvector phis)])
      (ordered-map-set m (PhiInsn-output p) p)))

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
              (for/list ([kv (in-ordered-map latches)]) (car kv)))]))
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
            (for/fold ([s (ordered-map-empty var-id-compare)])
                      ([v (in-pvector phi-outs)])
              (ordered-map-set s v #t))])
      (for/fold ([out phi-outs] [s seen]
                 #:result out)
                ([kv (in-ordered-map reads)])
        (define v (car kv))
        (if (ordered-map-ref s v #f)
            (values out s)
            (values (pvector-cons-right out v)
                    (ordered-map-set s v #t))))))
  (define n-carried (pvector-length carried-vars))

  ;; For each carried var, derive the "entry" source var (value on
  ;; the first iteration's input to Theta).  Loop-carried: phi's
  ;; source whose predecessor-bid is not in `latches` (i.e. the pre-
  ;; header feed).  Invariant: the var itself.
  (define entry-src-vars
    (for/pvector ([v (in-pvector carried-vars)])
      (define phi (ordered-map-ref phi-by-output v #f))
      (cond
        [phi
         (or (for/or ([s (in-pvector (PhiInsn-sources phi))])
               (and (not (ordered-map-ref latches (car s) #f)) (cdr s)))
             (error 'translate-theta
                    "phi ~a has no non-latch source" v))]
        [else v])))

  ;; ---- Build the sub-region. ----
  (define sub0 (region-empty))
  (define-values (sub1 _arg-nid _arg-ins arg-outs)
    (region-add-node sub0 (Simple (list 'region-arg n-carried)) 0 n-carried))

  ;; Sub-var->out maps each carried var to its region-arg output.
  (define sub-var->out-init
    (for/fold ([m (ordered-map-empty var-id-compare)])
              ([v (in-pvector carried-vars)]
               [oid (in-pvector arg-outs)])
      (ordered-map-set m v oid)))

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
       (define latch-bid (for/or ([kv (in-ordered-map latches)]) (car kv)))
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
    (or (ordered-map-ref sub-var->out3 pred-var #f)
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
      (define phi (ordered-map-ref phi-by-output v #f))
      (cond
        [(and phi (= n-latches 1))
         (define latch-bid (for/or ([kv (in-ordered-map latches)]) (car kv)))
         (define latch-var (pick-phi-source phi latch-bid 'latch))
         (or (ordered-map-ref sub-var->out3 latch-var #f)
             (error 'translate-theta "latch source ~a undefined" latch-var))]
        [else
         (or (ordered-map-ref sub-var->out3 v #f)
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
        (or (ordered-map-ref var->out entry-var #f)
            (error 'translate-theta "entry source ~a undefined" entry-var)))
      (define-values (r* _w) (region-add-wire r src-oid iid))
      r*))

  ;; Publish each carried var's post-loop value = Theta output.
  (define var->out*
    (for/fold ([m var->out])
              ([v (in-pvector carried-vars)]
               [oid (in-pvector t-outs)])
      (ordered-map-set m v oid)))

  ;; Continue translating from the exit-arm; the loop map lives in
  ;; the parameter so sibling / outer loops keep dispatching.
  (translate-segment cfg exit-arm-bid stop-bid region2 var->out*))

;; ============================================================
;; Kappa recovery (try/catch)
;; ============================================================
;;
;; `compute-kappa-groups` runs once at cfg->rvsdg entry, producing an
;; ordered-map[try-entry-BlockId -> Kappa-Group] keyed on each
;; exception-table entry's start-bid.  Multiple handlers for the same
;; (start-bid, end-bid) window are folded into a single group whose
;; `handlers` pvector preserves declaration order (first-wins).  Each
;; group's `try-bids` is computed from the original ordinal window
;; [start-ord, end-ord), extended with any forwarding blocks
;; normalize-try-exits inserted on exit paths; `kind` is `'terminal`
;; when the window has no fall-through exit (all paths ret / throw)
;; or `'convergent` when a single canonical exit-target exists (either
;; a direct single successor or the joiner block normalize-try-exits
;; created).  Multi-canonical-exit windows are rejected — the
;; normalize-try-exits pre-pass is the chokepoint that guarantees ≤ 1.
;;
;; translate-kappa-group installs one Kappa node in the parent region,
;; threading per-arm sub-regions through install-kappa-{terminal,
;; convergent}.  Context inputs are the union of each arm's parent-
;; scope VarId reads (collect-arm-ctx), augmented for the convergent
;; kind with each arm's phi-source contribution at the join.

(define (compute-kappa-groups cfg)
  (define table (cfg-get-info cfg 'java/exception-table #f))
  (cond
    [(or (not table) (= (pvector-length table) 0))
     (ordered-map-empty block-id-compare)]
    [else
     ;; Fold entries by start-bid, preserving declaration order of
     ;; handlers within each start-bid.  A second entry with the same
     ;; start-bid but a different end-bid would indicate a multi-range
     ;; (finally-style) layout — rejected here as not yet supported.
     (define by-start
       (for/fold ([m (ordered-map-empty block-id-compare)])
                 ([entry (in-pvector table)])
         (match-define (list start-bid end-bid handler-bid catch-type) entry)
         (define cur (ordered-map-ref m start-bid #f))
         (cond
           [cur
            (match-define (list end0 handlers0) cur)
            (unless (equal? end0 end-bid)
              (error 'compute-kappa-groups
                     "multi-range try at ~a (ends differ: ~s vs ~s) not yet supported"
                     start-bid end0 end-bid))
            (ordered-map-set m start-bid
                             (list end-bid
                                   (pvector-cons-right handlers0
                                                       (cons catch-type handler-bid))))]
           [else
            (define handlers1
              (pvector-cons-right (pvector-empty)
                                  (cons catch-type handler-bid)))
            (ordered-map-set m start-bid (list end-bid handlers1))])))
     ;; Build each Kappa-Group.  window-bids is the original ordinal
     ;; window; try-bids is window-bids extended with forwarding
     ;; blocks that sit between the window and the joiner, plus any
     ;; trivial Term:jump chain walked by refine-kappa-join when the
     ;; window's single fall-through exit is NOT itself the point
     ;; where the handler path converges.  The JVM exception table
     ;; scopes the try to the throwing instructions only, so the
     ;; normal post-try continuation may live in blocks that sit
     ;; outside the window but still on the try-success path between
     ;; the window and the real (try/handler) merge.
     (for/fold ([acc (ordered-map-empty block-id-compare)])
               ([kv (in-ordered-map by-start)])
       (define start-bid (car kv))
       (match-define (list end-bid handlers) (cdr kv))
       (define window-bids (compute-kappa-window-bids cfg start-bid end-bid))
       (define-values (kind0 join-bid0) (classify-kappa-window cfg window-bids))
       (define-values (kind join-bid chain-bids)
         (refine-kappa-join cfg kind0 join-bid0 handlers))
       (define try-bids0 (extend-with-forwarding-blocks cfg window-bids join-bid))
       (define try-bids
         (for/fold ([s try-bids0]) ([b (in-pvector chain-bids)])
           (ordered-map-set s b #t)))
       (ordered-map-set acc start-bid
                        (Kappa-Group try-bids handlers kind join-bid)))]))

;; When classify-kappa-window selects a fall-through exit X that is
;; itself a trivial Term:jump forwarder (no VfInsns, no phis) into
;; some downstream block Y, and Y happens to be reachable from every
;; handler, the real try/handler merge is Y — not X.  Walk X forward
;; through Term:jump trivials, stopping on the first block present in
;; every handler's forward-reach.  Return the promoted join plus the
;; chain of intermediate blocks (to be folded into try-bids).
;;
;; Preserves the initial classification when:
;;   - kind is 'terminal (no join to refine)
;;   - initial join is already on every handler-reach
;;   - the chain hits a non-trivial block (phi, Term:cond, etc.)
;;   - no handler-reach intersection is found within the walked chain
(define (refine-kappa-join cfg kind initial-join handlers)
  (define empty-chain (pvector-empty))
  (cond
    [(not (eq? kind 'convergent))
     (values kind initial-join empty-chain)]
    [else
     ;; Forward-reach from each handler with no stop.
     (define handler-reaches
       (for/list ([h (in-pvector handlers)])
         (kappa-forward-reach cfg (cdr h) #f)))
     (define (in-all-handler-reaches? bid)
       (for/and ([hr (in-list handler-reaches)])
         (ordered-map-ref hr bid #f)))
     (cond
       [(in-all-handler-reaches? initial-join)
        (values kind initial-join empty-chain)]
       [else
        ;; Walk forward through trivial Term:jump blocks (no phis, no
        ;; VfInsns) until we hit a common block or a non-trivial one.
        (let loop ([b initial-join]
                   [chain empty-chain]
                   [visited (ordered-map-empty block-id-compare)])
          (cond
            [(ordered-map-ref visited b #f)
             ;; Cycle — give up, keep the original join.
             (values kind initial-join empty-chain)]
            [(in-all-handler-reaches? b)
             (values kind b chain)]
            [else
             (define blk (cfg-get-block cfg b))
             (cond
               [(or (not blk)
                    (> (pvector-length (CfgBlock-phis blk)) 0)
                    (> (pvector-length (CfgBlock-insns blk)) 0))
                (values kind initial-join empty-chain)]
               [else
                (match (CfgBlock-terminator blk)
                  [(Term:jump n)
                   (loop n
                         (pvector-cons-right chain b)
                         (ordered-map-set visited b #t))]
                  [_ (values kind initial-join empty-chain)])])]))])]))

;; Re-derive the original [start-ord, end-ord) block-ordinal window
;; from 'java/block-order.  After normalize-try-exits appends
;; forwarding / joiner blocks to block-order, the first N entries are
;; still the original ones, so an end-bid that resolves to an ordinal
;; < append-point is valid.  `end-bid = #f` (try extending to method-
;; end) is rejected here — the original method-length isn't recorded
;; anywhere after the append, so we defer that case.
(define (compute-kappa-window-bids cfg start-bid end-bid)
  (define block-order
    (or (cfg-get-info cfg 'java/block-order #f)
        (error 'compute-kappa-window-bids
               "Cfg.info missing 'java/block-order (run jvm-to-cfg first)")))
  (define bid->ord
    (for/fold ([m (ordered-map-empty block-id-compare)])
              ([bid (in-pvector block-order)]
               [i (in-naturals)])
      (ordered-map-set m bid i)))
  (define start-ord
    (or (ordered-map-ref bid->ord start-bid #f)
        (error 'compute-kappa-window-bids
               "unmapped start-bid ~s" start-bid)))
  (define end-ord
    (cond
      [end-bid
       (or (ordered-map-ref bid->ord end-bid #f)
           (error 'compute-kappa-window-bids
                  "unmapped end-bid ~s" end-bid))]
      [else
       (error 'compute-kappa-window-bids
              "try window at ~s has end-bid=#f (method-end); not yet supported"
              start-bid)]))
  (for/fold ([s (ordered-map-empty block-id-compare)])
            ([bid (in-pvector block-order)]
             [i (in-naturals)]
             #:when (and (<= start-ord i) (< i end-ord)))
    (ordered-map-set s bid #t)))

;; Collect distinct out-of-window successors from every block in the
;; window, canonicalising forwarding-block successors to their joiner.
;; Returns (values kind join-bid) where kind is 'terminal (0 exits) or
;; 'convergent (exactly 1 canonical exit).  More than one canonical
;; exit is an error — normalize-try-exits should have collapsed them.
(define (classify-kappa-window cfg window-bids)
  (define direct-exits
    (for/fold ([s (ordered-map-empty block-id-compare)])
              ([kv (in-ordered-map window-bids)])
      (define t (CfgBlock-terminator (cfg-get-block cfg (car kv))))
      (define succs
        (match t
          [(Term:jump n) (list n)]
          [(Term:cond _ tb eb) (list tb eb)]
          [(Term:switch _ cases default)
           (cons default
                 (for/list ([kv (in-pvector cases)]) (cdr kv)))]
          [_ '()]))
      (for/fold ([s s]) ([n (in-list succs)]
                         #:unless (ordered-map-ref window-bids n #f))
        (ordered-map-set s n #t))))
  (define canonical-exits
    (for/fold ([s (ordered-map-empty block-id-compare)])
              ([kv (in-ordered-map direct-exits)])
      (define bid (car kv))
      (cond
        [(forwarding-block? (cfg-get-block cfg bid))
         (ordered-map-set s (Term:jump-target
                             (CfgBlock-terminator (cfg-get-block cfg bid)))
                          #t)]
        [else (ordered-map-set s bid #t)])))
  (cond
    [(= 0 (ordered-map-count canonical-exits)) (values 'terminal #f)]
    [(= 1 (ordered-map-count canonical-exits))
     (values 'convergent
             (for/or ([kv (in-ordered-map canonical-exits)]) (car kv)))]
    [else
     (error 'classify-kappa-window
            "try window has ~a distinct canonical exits (post-normalize ≤ 1 expected): ~s"
            (ordered-map-count canonical-exits)
            (for/list ([kv (in-ordered-map canonical-exits)]) (car kv)))]))

;; Structural recogniser for the selector-write forwarding block
;; normalize-try-exits synthesises on Term:cond / Term:switch exit
;; arms.  Shape: exactly one `'kappa-exit-sel` VfInsn followed by a
;; Term:jump — the jump target is the kappa-joiner block.
(define (forwarding-block? blk)
  (and blk
       (Term:jump? (CfgBlock-terminator blk))
       (= 1 (pvector-length (CfgBlock-insns blk)))
       (eq? 'kappa-exit-sel
            (VfInsn-op (pvector-ref (CfgBlock-insns blk) 0)))))

;; Extend window-bids with any forwarding block that sits on an exit
;; arm between a window block and the joiner.  These blocks live
;; outside the original ordinal window (normalize-try-exits appends
;; them to block-order) but are logically part of the try-region for
;; translation purposes — translate-segment scoped to try-bids needs
;; to walk through them to reach the join / joiner.
(define (extend-with-forwarding-blocks cfg window-bids join-bid)
  (for/fold ([s window-bids]) ([kv (in-ordered-map window-bids)])
    (define t (CfgBlock-terminator (cfg-get-block cfg (car kv))))
    (define succs
      (match t
        [(Term:jump n) (list n)]
        [(Term:cond _ tb eb) (list tb eb)]
        [(Term:switch _ cases default)
         (cons default
               (for/list ([kv (in-pvector cases)]) (cdr kv)))]
        [_ '()]))
    (for/fold ([s s]) ([n (in-list succs)])
      (cond
        [(ordered-map-ref s n #f) s]
        [(and join-bid (equal? n join-bid)) s]
        [(forwarding-block? (cfg-get-block cfg n))
         (ordered-map-set s n #t)]
        [else s]))))

;; Forward reach-set from start-bid, stopping at stop-bid (if given)
;; and at ret / throw / unreachable terminators.  Not scoped — used to
;; discover a handler's internal structure before building its sub-
;; region.  When stop-bid is #f the walk continues until every branch
;; leafs out at a non-advanceable terminator; when stop-bid is set it
;; is excluded from the result (the walk stops at but does not
;; traverse through stop-bid).
(define (kappa-forward-reach cfg start-bid stop-bid)
  (let loop ([work (list start-bid)]
             [s (ordered-map-empty block-id-compare)])
    (cond
      [(null? work) s]
      [else
       (define b (car work))
       (define rest (cdr work))
       (cond
         [(and stop-bid (equal? b stop-bid)) (loop rest s)]
         [(ordered-map-ref s b #f) (loop rest s)]
         [else
          (define s* (ordered-map-set s b #t))
          (define succs
            (match (CfgBlock-terminator (cfg-get-block cfg b))
              [(Term:jump n) (list n)]
              [(Term:cond _ tb eb) (list tb eb)]
              [(Term:switch _ cases default)
               (cons default
                     (for/list ([kv (in-pvector cases)]) (cdr kv)))]
              [_ '()]))
          (loop (append succs rest) s*)])])))

;; For a convergent Kappa arm that actually reaches the join via a
;; direct `Term:jump join-bid`, locate that predecessor block — its
;; phi-source slot supplies each join-phi's value from this arm.  C3
;; handles only arms that converge via a single direct jump; complex
;; arm merges (inner diamonds reunifying at join) are deferred.
;; Returns #f when no block in the arm jumps to join-bid (that arm is
;; terminal — the caller installs a ret / throw sink instead).  Errors
;; when multiple blocks jump to join-bid (ambiguous multi-source).
(define (find-kappa-arm-pred cfg arm-reach join-bid which)
  (define candidates
    (for/fold ([acc '()])
              ([kv (in-ordered-map arm-reach)])
      (define t (CfgBlock-terminator (cfg-get-block cfg (car kv))))
      (cond
        [(and (Term:jump? t) (equal? (Term:jump-target t) join-bid))
         (cons (car kv) acc)]
        [else acc])))
  (cond
    [(= 1 (length candidates)) (car candidates)]
    [(= 0 (length candidates)) #f]
    [else
     (error 'find-kappa-arm-pred
            "~a arm has multiple direct jumps to join ~a (~s); multi-source arm merges not yet supported"
            which join-bid candidates)]))

;; Build the try sub-region.  Walks translate-segment scoped to try-
;; bids with stop-bid=join-bid (or #f when kind is 'terminal); delegates
;; to finish-kappa-sub-region for sink vs region-result installation.
(define (build-kappa-try-region cfg start-bid try-bids ctx-vars
                                kind join-bid join-phis)
  (define n-ctx (pvector-length ctx-vars))
  (define sub0 (region-empty))
  (define-values (sub1 arg-outs) (region-add-region-arg sub0 n-ctx))
  (define sub-var->out
    (for/fold ([m (ordered-map-empty var-id-compare)])
              ([v (in-pvector ctx-vars)]
               [oid (in-pvector arg-outs)])
      (ordered-map-set m v oid)))
  (define stop (and (eq? kind 'convergent) join-bid))
  (define-values (sub2 sub-var->out2 payload)
    (parameterize ([current-arm-scope try-bids]
                   ;; Hide our own Kappa-Group so translate-segment at
                   ;; start-bid doesn't re-dispatch us (infinite loop).
                   [current-kappa-groups
                    (kappa-groups-without (current-kappa-groups) start-bid)])
      (translate-segment cfg start-bid stop sub1 sub-var->out)))
  (finish-kappa-sub-region/cfg cfg sub2 sub-var->out2 payload
                               try-bids join-bid join-phis 'kappa-try))

;; Build one handler sub-region.  The region-arg provides `n-ctx`
;; context outputs plus an extra exception-ref output (the `(N+1)`-th)
;; bound via `current-exn-out` so any `'java/exception-ref` VfInsn at
;; the handler entry resolves to it.  Arm kind (terminal vs convergent)
;; is decided from translate-segment's payload — a convergent-kind
;; Kappa tolerates a handler that ret / throws instead of joining.
;;
;; ssa-construct now augments the CFG graph with exception edges, so
;; the handler block may carry phi nodes that merge locals across all
;; try-range predecessors.  Kappa is fundamentally a throw-point-
;; collapsing abstraction — it can only present one value per local to
;; the handler — so we pick the phi source at `start-bid` (i.e., the
;; SSA name reaching the try-range entry) as the conservative ctx
;; value.  Each handler-entry phi output is pre-bound to the same
;; region-arg output that its start-bid phi source is bound to, so
;; translate-segment's phi-resolution check at the handler block
;; succeeds and subsequent reads of the phi output route through the
;; parent-scope ctx.
(define (build-kappa-handler-region cfg start-bid handler-bid handler-reach
                                    ctx-vars kind join-bid join-phis)
  (define n-ctx (pvector-length ctx-vars))
  (define-values (sub1 ctx-outs exn-out) (build-handler-region-entry n-ctx))
  (define sub-var->out-ctx
    (for/fold ([m (ordered-map-empty var-id-compare)])
              ([v (in-pvector ctx-vars)]
               [oid (in-pvector ctx-outs)])
      (ordered-map-set m v oid)))
  ;; Pre-bind handler-entry phi outputs to the ctx oid of their
  ;; start-bid-side phi source.  If the source is not a ctx var
  ;; (shouldn't happen — translate-kappa-group adds those sources to
  ;; ctx before calling us), leave it unbound; translate-segment will
  ;; surface the mismatch.
  (define handler-phi-srcs (handler-entry-phi-start-srcs cfg handler-bid start-bid))
  (define sub-var->out-init
    (for/fold ([m sub-var->out-ctx])
              ([pair (in-pvector handler-phi-srcs)])
      (define out (car pair))
      (define src (cdr pair))
      (define src-oid (ordered-map-ref m src #f))
      (cond [src-oid (ordered-map-set m out src-oid)]
            [else m])))
  (define stop (and (eq? kind 'convergent) join-bid))
  (define-values (sub2 sub-var->out2 payload)
    (parameterize ([current-arm-scope handler-reach]
                   [current-exn-out exn-out]
                   ;; Same re-entry guard as the try builder.
                   [current-kappa-groups
                    (kappa-groups-without (current-kappa-groups) start-bid)])
      (translate-segment cfg handler-bid stop sub1 sub-var->out-init)))
  (finish-kappa-sub-region/cfg cfg sub2 sub-var->out2 payload
                               handler-reach join-bid join-phis
                               (list 'kappa-handler handler-bid)))

;; Return a pvector of (cons phi-output start-bid-phi-source) pairs
;; for every phi at `handler-bid`.  Assumes ssa-construct placed
;; phis there with exception-edge predecessors, so each phi carries
;; a source slot keyed on every try-range block including start-bid.
(define (handler-entry-phi-start-srcs cfg handler-bid start-bid)
  (define blk (cfg-get-block cfg handler-bid))
  (cond
    [(not blk) (pvector-empty)]
    [else
     (for/pvector ([phi (in-pvector (CfgBlock-phis blk))])
       (cons (PhiInsn-output phi)
             (pick-phi-source phi start-bid
                              (list 'kappa-handler-entry handler-bid))))]))

;; Remove the current Kappa-Group keyed on `start-bid` from the
;; active kappa-groups map while building its own sub-regions, so
;; translate-segment landing back on `start-bid` inside the walk does
;; not re-dispatch infinitely.  Other groups (nested / sibling trys)
;; remain visible.  `gs` may be #f (no kappa-groups threading).
(define (kappa-groups-without gs start-bid)
  (cond
    [(not gs) gs]
    [(ordered-map-ref gs start-bid #f)
     (define-values (gs* _) (ordered-map-delete gs start-bid))
     gs*]
    [else gs]))

;; Wrapper that threads `cfg` into the finisher so find-kappa-arm-pred
;; can locate the Term:jump predecessor when the arm converges.
(define (finish-kappa-sub-region/cfg cfg sub2 sub-var->out2 payload
                                     arm-reach join-bid join-phis which)
  (match payload
    [(cons 'ret vs)      (install-return sub2 sub-var->out2 vs)]
    [(cons 'throw v)     (install-throw  sub2 sub-var->out2 v)]
    ['already-terminated sub2]
    [#f
     (unless join-bid
       (error 'finish-kappa-sub-region
              "~a arm reached a block without a join-bid (terminal-kind walk mis-stopped)"
              which))
     (define arm-pred
       (or (find-kappa-arm-pred cfg arm-reach join-bid which)
           (error 'finish-kappa-sub-region
                  "~a arm stopped at ~a but no block jumps to it" which join-bid)))
     (define result-src-vars
       (for/pvector ([phi (in-pvector join-phis)])
         (pick-phi-source phi arm-pred which)))
     (define-values (sub3 res-ins)
       (region-add-region-result sub2 (pvector-length join-phis)))
     (for/fold ([r sub3])
               ([src-var (in-pvector result-src-vars)]
                [iid (in-pvector res-ins)])
       (define src-oid
         (or (ordered-map-ref sub-var->out2 src-var #f)
             (error 'finish-kappa-sub-region
                    "phi source ~a undefined in ~a sub-region" src-var which)))
       (define-values (r* _w) (region-add-wire r src-oid iid))
       r*)]
    [other
     (error 'finish-kappa-sub-region
            "~a arm yielded unexpected payload ~s" which other)]))

;; Returns (values region* var->out* payload) where payload is either
;; 'already-terminated (terminal Kappa) or join-bid (convergent; caller
;; resumes from there).
(define (translate-kappa-group cfg start-bid kappa-group region var->out)
  (match-define (Kappa-Group try-bids handlers kind join-bid) kappa-group)

  ;; Each arm's forward reach (try vs each handler) drives ctx
  ;; collection.  Try-reach is try-bids itself (already expanded with
  ;; forwarding blocks); handler-reach is an unscoped walk from the
  ;; handler entry stopping at join-bid when convergent (so the phi
  ;; values live in scope but the join block itself doesn't).
  (define try-reach try-bids)
  (define handler-reaches
    (for/list ([h (in-pvector handlers)])
      (kappa-forward-reach cfg (cdr h) join-bid)))

  ;; Convergent kind: pull the join block's phis so each arm that
  ;; reaches join can wire its pred's phi-source VarId through a
  ;; region-result.  Arms that instead ret / throw inside the sub-
  ;; region install a sink and skip this path.
  (define join-phis
    (cond
      [(eq? kind 'convergent)
       (CfgBlock-phis (cfg-get-block cfg join-bid))]
      [else (pvector-empty)]))
  (define n-phis (pvector-length join-phis))

  ;; Per-arm pred-bids (or #f if the arm does not reach join via a
  ;; direct Term:jump) — only used to extend ctx with phi-source
  ;; contributions from arms that actually converge.  The builders
  ;; re-derive these internally when they finish the sub-region, so
  ;; we don't thread them through.
  (define (arm-pred-or-false reach which)
    (cond [(eq? kind 'convergent)
           (find-kappa-arm-pred cfg reach join-bid which)]
          [else #f]))
  (define try-arm-pred (arm-pred-or-false try-reach 'try))
  (define handler-arm-preds
    (for/list ([h (in-pvector handlers)]
               [reach (in-list handler-reaches)])
      (arm-pred-or-false reach (list 'handler (cdr h)))))

  ;; Ctx vars: union of parent-scope reads across every arm, plus
  ;; (for converging arms only) phi-source VarId contributions at
  ;; each arm's pred.
  (define try-ctx (collect-arm-ctx cfg var->out try-reach))
  (define handler-ctxs
    (for/list ([reach (in-list handler-reaches)])
      (collect-arm-ctx cfg var->out reach)))
  (define ctx-set-base
    (for/fold ([s try-ctx])
              ([hctx (in-list handler-ctxs)])
      (for/fold ([s s]) ([kv (in-ordered-map hctx)])
        (ordered-map-set s (car kv) #t))))
  (define (add-phi-srcs arm-pred acc)
    (cond
      [arm-pred
       (for/fold ([a acc]) ([phi (in-pvector join-phis)])
         (define src (pick-phi-source phi arm-pred 'kappa-ctx))
         (cond
           [(and (VarId? src) (ordered-map-ref var->out src #f))
            (ordered-map-set a src #t)]
           [else a]))]
      [else acc]))
  (define ctx-set-with-join
    (cond
      [(eq? kind 'convergent)
       (for/fold ([s (add-phi-srcs try-arm-pred ctx-set-base)])
                 ([ap (in-list handler-arm-preds)])
         (add-phi-srcs ap s))]
      [else ctx-set-base]))
  ;; Handler-entry phis (placed by ssa-construct on exception edges)
  ;; need their start-bid-side source value to flow into the handler
  ;; as ctx — that's the single "local at try entry" value Kappa
  ;; exposes to each handler.
  (define ctx-set
    (for/fold ([s ctx-set-with-join])
              ([h (in-pvector handlers)])
      (define pairs (handler-entry-phi-start-srcs cfg (cdr h) start-bid))
      (for/fold ([s s]) ([pair (in-pvector pairs)])
        (define src (cdr pair))
        (cond
          [(and (VarId? src) (ordered-map-ref var->out src #f))
           (ordered-map-set s src #t)]
          [else s]))))
  (define ctx-vars
    (for/pvector ([kv (in-ordered-map ctx-set)]) (car kv)))

  ;; Build each sub-region.  Arm kind is decided by each builder from
  ;; translate-segment's payload — terminal-kind Kappa sub-regions
  ;; must ret / throw; convergent-kind sub-regions may ret / throw or
  ;; jump to join-bid (whichever the CFG shape dictates).
  (define try-region
    (build-kappa-try-region cfg start-bid try-bids ctx-vars
                            kind join-bid join-phis))
  (define handlers-pv
    (for/pvector ([h (in-pvector handlers)]
                  [reach (in-list handler-reaches)])
      (define hr
        (build-kappa-handler-region cfg start-bid (cdr h) reach ctx-vars
                                    kind join-bid join-phis))
      (cons (car h) hr)))

  ;; Parent-side ctx producer outputs.
  (define ctx-oids
    (for/pvector ([v (in-pvector ctx-vars)])
      (or (ordered-map-ref var->out v #f)
          (error 'translate-kappa-group
                 "ctx var ~a undefined in parent scope" v))))

  ;; Install Kappa.
  (cond
    [(eq? kind 'terminal)
     (define-values (region* _knid)
       (install-kappa-terminal region ctx-oids try-region handlers-pv))
     (values region* var->out 'already-terminated)]
    [else
     (define-values (region* _knid out-oids)
       (install-kappa-convergent region ctx-oids try-region handlers-pv
                                 n-phis))
     (define var->out*
       (for/fold ([m var->out])
                 ([phi (in-pvector join-phis)]
                  [oid (in-pvector out-oids)])
         (ordered-map-set m (PhiInsn-output phi) oid)))
     (values region* var->out* join-bid)]))

;; ============================================================
;; Terminator materialisation
;; ============================================================

(define (install-return region var->out ret-values)
  (define n (pvector-length ret-values))
  (define-values (region1 _nid in-ids _outs)
    (region-add-node region (Simple 'return) n 0))
  (for/fold ([r region1])
            ([v (in-pvector ret-values)]
             [iid (in-pvector in-ids)])
    (define oid (or (ordered-map-ref var->out v #f)
                    (error 'install-return "undef ~a" v)))
    (define-values (r* _w) (region-add-wire r oid iid))
    r*))

(define (install-throw region var->out exn)
  (define-values (region1 _nid in-ids _outs)
    (region-add-node region (Throw #f) 1 0))
  (define oid (or (ordered-map-ref var->out exn #f)
                  (error 'install-throw "undef ~a" exn)))
  (define-values (r* _w) (region-add-wire region1 oid (pvector-ref in-ids 0)))
  r*)
