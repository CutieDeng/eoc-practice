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
;;     sub-regions, provided both branches rejoin at a common
;;     successor with a Term:jump.  Each sub-region uses a synthetic
;;     `Simple '(region-arg N)` producer node at its entry to mirror
;;     the Gamma's context inputs, and a `Simple '(region-result N)`
;;     consumer node to feed the Gamma's outputs.
;;   - Term:ret / Term:throw yield synthetic `Simple 'return` /
;;     kernel `Throw` sink nodes in whichever region they appear.
;;
;; Currently unsupported:
;;   - Term:switch (tablesswitch / lookupswitch)
;;   - Loops (back edges / Theta recovery)
;;   - Branches where one side spans multiple blocks before the join
;;   - try/catch (Kappa recovery)
;;
;; Each of the above raises with a self-identifying error.
;;
;; ============================================================

(require racket/match
         "../../../kernel/ir/cfg/types.rkt"
         "../../../kernel/ir/rvsdg/rvsdg.rkt"
         "../../../component/cfg/utils/graph-ops.rkt"
         "../../../component/rvsdg/utils/builder.rkt"
         (except-in "../../../kernel/data/data.rkt" integer-compare))

(provide cfg->rvsdg)

;; ============================================================
;; Entry
;; ============================================================

(define (cfg->rvsdg cfg)
  (define local-count (cfg-get-info cfg 'java/max-local 0))
  (define param-names
    (or (cfg-get-info cfg 'java/ssa-param-names #f)
        (error 'cfg->rvsdg
               "ssa must publish 'java/ssa-param-names; run jvm-cfg->ssa first")))
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
    (translate-segment cfg (Cfg-entry cfg) #f region1 var->out-init))

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
(define (translate-segment cfg start-bid stop-bid region var->out)
  (cond
    [(and stop-bid (equal? start-bid stop-bid))
     (values region var->out #f)]
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
  (define-values (join-bid then-result-vars)
    (branch-jump-target cfg then-bid))
  (define-values (else-join else-result-vars)
    (branch-jump-target cfg else-bid))
  (unless (equal? join-bid else-join)
    (error 'translate-gamma
           "then/else branches converge at different joins: ~a vs ~a"
           join-bid else-join))

  (define join-blk (cfg-get-block cfg join-bid))
  (define phis (CfgBlock-phis join-blk))
  (define n-phis (pvector-length phis))

  ;; Context vars = every VarId currently mapped in parent var->out.
  ;; We can prune later via escape analysis; for now the conservative
  ;; closure is both correct and simple.
  (define ctx-vars
    (for/pvector ([kv (in-ordered-map var->out)]) (car kv)))
  (define n-ctx (pvector-length ctx-vars))

  ;; Build the per-branch sub-region closures.
  (define then-region
    (build-branch-region cfg then-bid join-bid ctx-vars phis 'then))
  (define else-region
    (build-branch-region cfg else-bid join-bid ctx-vars phis 'else))

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

;; Given a branch block, return (values join-bid result-var-list).
;; Accepts either a single-block branch ending in Term:jump, or an
;; empty branch where branch-bid's only role is to pass control on.
;; The result-vars are placeholders -- actual source varids are
;; pulled from the join's phi sources later.
(define (branch-jump-target cfg branch-bid)
  (define blk (cfg-get-block cfg branch-bid))
  (unless blk (error 'branch-jump-target "missing block ~a" branch-bid))
  (match (CfgBlock-terminator blk)
    [(Term:jump nxt) (values nxt #f)]
    [_ (error 'branch-jump-target
              "expected Term:jump at branch ~a (got ~a)"
              branch-bid (CfgBlock-terminator blk))]))

;; ============================================================
;; Sub-region builder
;; ============================================================

(define (build-branch-region cfg branch-bid join-bid ctx-vars phis which)
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

  ;; Translate branch block's insns (if it isn't trivially the join).
  (define-values (sub2 sub-var->out2)
    (cond
      [(equal? branch-bid join-bid)
       (values sub1 sub-var->out)]
      [else
       (define blk (cfg-get-block cfg branch-bid))
       (translate-insns (CfgBlock-insns blk) sub1 sub-var->out)]))

  ;; For each phi at the join, look up the source var coming from
  ;; this branch's predecessor.  The predecessor is `branch-bid` when
  ;; the branch is non-empty, otherwise it's the grandparent (cond
  ;; block) -- but in our scope cond→join direct edges aren't
  ;; supported; so branch-bid is the predecessor.
  (define result-src-vars
    (for/pvector ([phi (in-pvector phis)])
      (pick-phi-source phi branch-bid which)))

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
