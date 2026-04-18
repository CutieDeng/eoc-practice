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
;;     sub-regions.  Each arm may span multiple basic blocks so long
;;     as every block on the arm is Term:jump-terminated (i.e. the
;;     arm is a linear chain of jumps); both arms must eventually
;;     rejoin at a common block (Gamma's join).  Each sub-region uses
;;     a synthetic `Simple '(region-arg N)` producer node at its
;;     entry to mirror the Gamma's context inputs, and a
;;     `Simple '(region-result N)` consumer node to feed the Gamma's
;;     outputs.
;;   - Term:ret / Term:throw yield synthetic `Simple 'return` /
;;     kernel `Throw` sink nodes in whichever region they appear.
;;
;;   - A single natural loop with a one-block body is lowered to a
;;     Theta node.  The header must end in Term:cond; one arm equals
;;     the latch (continue) and the other is the exit.  All parent-
;;     scope vars pass through as conservatively-closed loop-carried
;;     or loop-invariant inputs.  A `Simple 'not` node flips polarity
;;     when the body sits on the else-arm.
;;
;; Currently unsupported:
;;   - Term:switch (tablesswitch / lookupswitch)
;;   - Nested / multiple natural loops
;;   - Loop body spanning more than one block
;;   - Nested control flow within a Gamma arm (inner Term:cond /
;;     Term:switch / Term:ret before the join)
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
;; natural loop per CFG with a single-block body whose back-edge
;; closes to the header.
;;
(struct Theta-Ctx (header latch) #:prefab)

;; ============================================================
;; Entry
;; ============================================================

(define (cfg->rvsdg cfg)
  (define local-count (cfg-get-info cfg 'java/max-local 0))
  (define param-names
    (or (cfg-get-info cfg 'java/ssa-param-names #f)
        (error 'cfg->rvsdg
               "ssa must publish 'java/ssa-param-names; run jvm-cfg->ssa first")))

  ;; Detect loop structure.  At most one back-edge supported; nested
  ;; or irreducible loops raise.
  (define back-edges (cfg-find-back-edges cfg))
  (define n-back-edges (pvector-length back-edges))
  (define theta-ctx
    (cond
      [(= n-back-edges 0) #f]
      [(= n-back-edges 1)
       (define be (pvector-ref back-edges 0))
       (Theta-Ctx (cdr be) (car be))]
      [else
       (error 'cfg->rvsdg
              "multiple back-edges not yet supported: ~s"
              (for/list ([e (in-pvector back-edges)]) e))]))

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
    (translate-segment cfg (Cfg-entry cfg) #f region1 var->out-init theta-ctx))

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
;; `theta-ctx` describes the currently-active loop (if any).  When
;; `start-bid` equals the Theta header AND the caller did not pass
;; stop-bid=header (which would catch back-edge closure first), we
;; dispatch into translate-theta to build a Theta node.
(define (translate-segment cfg start-bid stop-bid region var->out theta-ctx)
  (cond
    [(and stop-bid (equal? start-bid stop-bid))
     (values region var->out #f)]
    [(and theta-ctx (equal? start-bid (Theta-Ctx-header theta-ctx)))
     (translate-theta cfg theta-ctx stop-bid region var->out)]
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
        (translate-segment cfg nxt stop-bid region1 var->out1 theta-ctx)]
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
           (translate-segment cfg join-bid stop-bid region2 var->out2 theta-ctx)]
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
  (define then-chain (arm-chain-blocks cfg then-bid join-bid))
  (define else-chain (arm-chain-blocks cfg else-bid join-bid))
  (define then-pred
    (pvector-ref then-chain (sub1 (pvector-length then-chain))))
  (define else-pred
    (pvector-ref else-chain (sub1 (pvector-length else-chain))))

  (define join-blk (cfg-get-block cfg join-bid))
  (define phis (CfgBlock-phis join-blk))
  (define n-phis (pvector-length phis))

  ;; Context vars: only parent-scope vars each sub-region actually
  ;; reads (plus the phi source contributed by each arm's pred).  We
  ;; walk every block on each arm's chain, tracking locally-defined
  ;; outputs across the chain, and union the reads across both arms
  ;; so the Gamma sub-regions agree on input shape.
  (define ctx-set
    (collect-gamma-ctx cfg var->out then-chain then-pred
                                    else-chain else-pred phis))
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

;; Walk the then-arm's Term:jump chain forward, then walk the else-arm
;; until we hit a bid visited on the then-arm.  That bid is the Gamma
;; join block.  Errors if neither arm can reach the other.  Both arms
;; must be Term:jump-only chains -- nested conds/loops in an arm are
;; not yet supported here.
(define (find-branch-join cfg then-bid else-bid)
  (define then-reach
    (let loop ([b then-bid] [s (ordered-map-empty block-id-compare)])
      (cond
        [(ordered-map-ref s b #f) s]
        [else
         (define s2 (ordered-map-set s b #t))
         (define term (CfgBlock-terminator (cfg-get-block cfg b)))
         (match term
           [(Term:jump n) (loop n s2)]
           [_ s2])])))
  (let loop ([b else-bid])
    (cond
      [(ordered-map-ref then-reach b #f) b]
      [else
       (define term (CfgBlock-terminator (cfg-get-block cfg b)))
       (match term
         [(Term:jump n) (loop n)]
         [_ (error 'find-branch-join
                   "branches do not converge (then=~a else=~a terminator ~s at ~a)"
                   then-bid else-bid term b)])])))

;; Sequence of blocks from branch-bid up to (but not including)
;; join-bid, following Term:jump links only.  Used both for arm
;; translation and for context collection.  Errors if the chain
;; contains non-Term:jump terminators before reaching join-bid.
(define (arm-chain-blocks cfg branch-bid join-bid)
  (let loop ([b branch-bid] [acc (pvector-empty)])
    (cond
      [(equal? b join-bid) acc]
      [else
       (define blk (cfg-get-block cfg b))
       (unless blk (error 'arm-chain-blocks "missing block ~a" b))
       (define term (CfgBlock-terminator blk))
       (match term
         [(Term:jump n) (loop n (pvector-cons-right acc b))]
         [_ (error 'arm-chain-blocks
                   "arm ~a -> ~a: block ~a has non-jump terminator ~s (nested control flow inside arm not yet supported)"
                   branch-bid join-bid b term)])])))

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
  ;; from branch-bid through Term:jump edges, stopping at join-bid,
  ;; and feeds each block's var bindings forward.  theta-ctx is #f:
  ;; nested loops within a Gamma arm are not yet supported.
  (define-values (sub2 sub-var->out2 payload)
    (translate-segment cfg branch-bid join-bid sub1 sub-var->out #f))
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
;; Each arm is described by its chain (pvector of BlockIds, in order)
;; plus the block that immediately precedes the join (== last element
;; of the chain; passed separately so phi lookup can reuse it).  An
;; arm "needs" a parent var iff some VfInsn along the chain reads it
;; before any earlier insn in the chain defined it, or that var is
;; the phi source contributed by the arm's predecessor at the join.
;; Locals accumulate across all blocks in the arm since SSA scope is
;; shared along a dominance chain.  Returns an ordered-map used as a
;; set of VarId -> #t.
(define (collect-gamma-ctx cfg parent-var->out
                           then-chain then-pred
                           else-chain else-pred phis)
  (define (walk-arm chain arm-pred acc)
    (define-values (_ acc*)
      (for/fold ([locals (ordered-map-empty var-id-compare)]
                 [a acc])
                ([bid (in-pvector chain)])
        (define insns (CfgBlock-insns (cfg-get-block cfg bid)))
        (for/fold ([locals locals] [a a])
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
          (values locals* a*))))
    ;; Phi source contributed at the join from this arm's predecessor.
    (for/fold ([a acc*]) ([phi (in-pvector phis)])
      (define src (pick-phi-source phi arm-pred 'gamma-ctx))
      (cond
        [(and (VarId? src)
              (ordered-map-ref parent-var->out src #f))
         (ordered-map-set a src #t)]
        [else a])))
  (walk-arm else-chain else-pred
            (walk-arm then-chain then-pred
                      (ordered-map-empty var-id-compare))))

;; Collect parent-scope VarIds actually read inside the loop.  Walks
;; both the header block and the latch block, tracking locally-defined
;; outputs (including header phi outputs, which are local to the
;; sub-region).  Any VfInsn input that is a VarId, not locally-defined
;; in the block's prefix, and present in `parent-var->out` counts as a
;; read.  Returns an ordered-map used as a set of VarId -> #t.
(define (collect-theta-ctx cfg parent-var->out header-bid latch-bid phis)
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
  (walk-block latch-bid
              (walk-block header-bid
                          (ordered-map-empty var-id-compare))))

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
;; Scope: exactly one natural loop, with a single-block body that
;; jumps back to the header (the latch).  The header must end in
;; Term:cond; one arm leads into the body (the latch), the other
;; exits the loop.  Deviations raise self-identifying errors.
;;
;; Sub-region layout:
;;   - `Simple '(region-arg N)` producer provides one output per
;;     loop-carried phi (these mirror the Theta node's inputs).
;;   - Header's non-phi insns are translated in-region.
;;   - Body block's insns are translated in-region via a scoped
;;     translate-segment call with stop-bid=header (closing on the
;;     back-edge).
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
            (collect-theta-ctx cfg var->out header-bid latch-bid phis)]
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

  ;; Identify continue / exit arms.
  (define header-term (CfgBlock-terminator header-blk))
  (unless (Term:cond? header-term)
    (error 'translate-theta
           "header ~a must end in Term:cond for Theta lowering (got ~a)"
           header-bid header-term))
  (define pred-var (Term:cond-cond header-term))
  (define then-bid (Term:cond-then-target header-term))
  (define else-bid (Term:cond-else-target header-term))
  (define-values (body-is-then? exit-arm-bid)
    (cond
      [(equal? then-bid latch-bid) (values #t else-bid)]
      [(equal? else-bid latch-bid) (values #f then-bid)]
      [else (error 'translate-theta
                   "neither cond arm equals latch ~a (then=~a else=~a) -- multi-block loop body not yet supported"
                   latch-bid then-bid else-bid)]))

  ;; Translate the body block, stopping at the back-edge into header.
  (define-values (sub3 sub-var->out3 body-payload)
    (translate-segment cfg latch-bid header-bid sub2 sub-var->out2 #f))
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

  ;; Continue translating from the exit-arm.  Only one loop supported,
  ;; so no more theta-ctx beyond this point.
  (translate-segment cfg exit-arm-bid stop-bid region2 var->out* #f))

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
