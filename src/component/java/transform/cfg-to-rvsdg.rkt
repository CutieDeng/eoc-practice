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
;; Currently unsupported:
;;   - Term:switch whose arms convergently rejoin a common
;;     successor (only fully-terminal switches are lowered; a
;;     convergent switch hits the generic Term:switch error path)
;;   - try/catch (Kappa recovery)
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

  ;; 3. Translate starting at entry; stop-bid = #f means "until a
  ;;    terminator or unreachable".
  (define-values (region-final var->out-final payload)
    (parameterize ([current-theta-ctxs theta-ctxs])
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
  (define ctx-here (and ctxs (ordered-map-ref ctxs start-bid #f)))
  (cond
    [(and stop-bid (equal? start-bid stop-bid))
     (values region var->out #f)]
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
