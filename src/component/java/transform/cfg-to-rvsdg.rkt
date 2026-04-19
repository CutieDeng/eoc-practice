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
;; Currently unsupported:
;;   - Term:switch (tablesswitch / lookupswitch)
;;   - Nested / multiple natural loops
;;   - Gamma arm with Term:ret / Term:throw / Term:switch before
;;     reaching the outer join (early-exit patterns)
;;   - try/catch (Kappa recovery)
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
;; loop header spawns a Theta.  We currently support at most one
;; natural loop per CFG; the body from the header's body-arm to the
;; latch may mix Term:jump steps with inner Gamma diamonds (each
;; must converge before the latch).  The latch's back-edge closes
;; to the header.
;;
(struct Theta-Ctx (header latch) #:prefab)

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

  ;; Detect loop structure.  Each back-edge becomes one entry in
  ;; theta-ctxs (header-bid -> Theta-Ctx).  Multiple back-edges
  ;; sharing a header (multi-latch loops, e.g. via `continue` from
  ;; several paths) are still rejected; reducible nested loops are
  ;; fine because each loop has a distinct header.
  (define back-edges (cfg-find-back-edges cfg))
  (define theta-ctxs
    (for/fold ([m (ordered-map-empty block-id-compare)])
              ([be (in-pvector back-edges)])
      (define latch (car be))
      (define header (cdr be))
      (when (ordered-map-ref m header #f)
        (error 'cfg->rvsdg
               "loop header ~a has multiple back-edges (latches), not yet supported"
               header))
      (ordered-map-set m header (Theta-Ctx header latch))))

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

  ;; 4. Materialise Term:ret / Term:throw.
  (define region-done
    (match payload
      [(cons 'ret vs)   (install-return region-final var->out-final vs)]
      [(cons 'throw ex) (install-throw  region-final var->out-final ex)]
      [_                region-final]))

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
        (define-values (region2 var->out2 join-bid)
          (translate-gamma cfg start-bid pred then-bid else-bid
                           region1 var->out1))
        (cond
          [join-bid
           (translate-segment cfg join-bid stop-bid region2 var->out2)]
          [else
           (values region2 var->out2 #f)])]
       [(Term:switch _ _ _)
        (error 'translate-segment
               "Term:switch at ~a not yet supported" start-bid)]
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
  (define join-bid (find-branch-join cfg then-bid else-bid))
  (when (or (equal? join-bid then-bid) (equal? join-bid else-bid))
    (error 'translate-gamma
           "empty branch (then=~a else=~a join=~a) not yet supported"
           then-bid else-bid join-bid))
  (define then-pred (arm-last-before-join cfg then-bid join-bid))
  (define else-pred (arm-last-before-join cfg else-bid join-bid))
  (define then-blocks (arm-blocks-set cfg then-bid join-bid))
  (define else-blocks (arm-blocks-set cfg else-bid join-bid))

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

;; Advance one step through an arm's control flow, recursively resolving
;; nested Term:cond blocks by finding their own diamond-joins.  Returns
;; the next BlockId to visit, or #f when the terminator can't be
;; advanced (ret / throw / switch / unreachable, or — when
;; `current-arm-scope` is set — when the next bid would lie outside
;; that scope).
(define (arm-advance cfg bid)
  (define term (CfgBlock-terminator (cfg-get-block cfg bid)))
  (match term
    [(Term:jump n) (and (arm-scope-contains? n) n)]
    [(Term:cond _ tb eb)
     (cond
       [(and (arm-scope-contains? tb) (arm-scope-contains? eb))
        (find-branch-join cfg tb eb)]
       [else #f])]
    [_ #f]))

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
(define (theta-body-blocks cfg start latch header)
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
            [(equal? b latch)
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

;; Body-arm check: starting from the cond-arm bid, do we reach the
;; latch while remaining inside the body (i.e., never crossing the
;; back-edge into `header`)?  Implemented as set-membership in
;; `theta-body-blocks`, so it naturally tolerates inner Gamma
;; diamonds just like the block-set walk.
(define (body-arm-reaches-latch? cfg start latch header)
  (and (ordered-map-ref
        (theta-body-blocks cfg start latch header) latch #f)
       #t))

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
;; Theta recovery (while-loop)
;; ============================================================
;;
;; Scope: exactly one natural loop.  The header must end in
;; Term:cond; one arm leads (possibly through Term:jump steps and/or
;; inner convergent diamonds) to the latch, which jumps back to the
;; header.  The other cond arm exits the loop.  Deviations raise
;; self-identifying errors.
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
  (define latch-bid (Theta-Ctx-latch theta-ctx))
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
  ;; chain of Term:jump blocks) to the latch and constitutes the loop
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
      [(body-arm-reaches-latch? cfg then-bid latch-bid header-bid)
       (values #t then-bid else-bid)]
      [(body-arm-reaches-latch? cfg else-bid latch-bid header-bid)
       (values #f else-bid then-bid)]
      [else
       (error 'translate-theta
              "neither cond arm reaches latch ~a without crossing back-edge (then=~a else=~a)"
              latch-bid then-bid else-bid)]))
  (define body-blocks
    (theta-body-blocks cfg body-entry-bid latch-bid header-bid))

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
  ;; non-latch source.  Invariant: the var itself.
  (define entry-src-vars
    (for/pvector ([v (in-pvector carried-vars)])
      (define phi (ordered-map-ref phi-by-output v #f))
      (cond
        [phi
         (or (for/or ([s (in-pvector (PhiInsn-sources phi))])
               (and (not (equal? (car s) latch-bid)) (cdr s)))
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

  ;; Translate the body block(s), stopping at the back-edge into
  ;; header.  translate-segment walks the Term:jump chain from
  ;; body-entry-bid through the latch and naturally stops at header
  ;; (= back-edge target = stop-bid).  We restrict the arm-walking
  ;; helpers to body-blocks so any inner Gamma's `find-branch-join`
  ;; cannot wander past the latch into the loop header.
  (define-values (sub3 sub-var->out3 body-payload)
    (parameterize ([current-arm-scope body-blocks])
      (translate-segment cfg body-entry-bid header-bid
                         sub2 sub-var->out2)))
  (unless (eq? body-payload #f)
    (error 'translate-theta
           "loop body payload ~s unsupported (ret/throw inside loop not yet lowered)"
           body-payload))

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
  ;; iteration / exposed as Theta's output.  Loop-carried: the phi's
  ;; latch source.  Invariant: the sub-region's own current value
  ;; (which equals the region-arg output since nothing rebound it).
  (define update-oids
    (for/pvector ([v (in-pvector carried-vars)])
      (define phi (ordered-map-ref phi-by-output v #f))
      (cond
        [phi
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
