#lang racket/base

;; ============================================================
;; CFG -> RVSDG structurizer: Term:cond / Term:switch -> Gamma
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
         "sinks.rkt")

(provide translate-gamma translate-terminal-gamma translate-terminal-switch translate-convergent-switch collect-switch-arms-ctx translate-asymmetric-exit-gamma)


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
  ;;
  ;; Empty-arm case: a `if (c) { body }` with no else compiles to a
  ;; Term:cond whose "else" target IS the join block (cond-bid →
  ;; then-bid, cond-bid → join-bid).  arm-pred for that empty arm is
  ;; cond-bid itself (the single direct predecessor feeding the join),
  ;; and arm-blocks is empty so collect-gamma-ctx picks up nothing
  ;; from that side beyond the phi-source contribution.
  (define then-full-reach (arm-reach-set cfg then-bid))
  (define else-full-reach (arm-reach-set cfg else-bid))
  (define shared-set (reach-set-intersection then-full-reach else-full-reach))
  (define-values (join-bid then-pred else-pred then-blocks else-blocks)
    (parameterize ([current-shared-set shared-set])
      (define jb (find-branch-join cfg then-bid else-bid))
      (define (arm-pred-for b) (if (equal? b jb) cond-bid
                                   (arm-last-before-join cfg b jb)))
      (define (arm-blocks-for b) (if (equal? b jb)
                                     idmap-empty
                                     (arm-blocks-set cfg b jb)))
      (values jb
              (arm-pred-for then-bid)
              (arm-pred-for else-bid)
              (arm-blocks-for then-bid)
              (arm-blocks-for else-bid))))

  (define join-blk (cv-block cfg join-bid))
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
    (for/pvector ([kv (in-idmap ctx-set)]) (car kv)))
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
    (or (idmap-ref var->out pred #f)
        (error 'translate-gamma "undefined predicate ~a" pred)))
  (define-values (region2 _pw)
    (region-add-wire region1 pred-oid (pvector-ref g-ins 0)))

  ;; Wire each context input.
  (define region3
    (for/fold ([r region2])
              ([ctx-var (in-pvector ctx-vars)]
               [i (in-naturals 1)])
      (define src-oid (idmap-ref var->out ctx-var #f))
      (define-values (r* _w) (region-add-wire r src-oid (pvector-ref g-ins i)))
      r*))

  ;; Extend var→out: every phi at the join becomes the corresponding
  ;; Gamma output.
  (define var->out*
    (for/fold ([m var->out])
              ([phi (in-pvector phis)]
               [oid (in-pvector g-outs)])
      (idmap-set m (PhiInsn-output phi) oid)))

  (values region3 var->out* join-bid))


;; Returns (values region var->out 'already-terminated).  Caller
;; treats the terminal-Gamma as the segment's terminator and stops
;; translating further blocks.
(define (translate-terminal-gamma cfg cond-bid pred then-bid else-bid region var->out)
  (define then-reach (arm-reach-set cfg then-bid))
  (define else-reach (arm-reach-set cfg else-bid))
  (define then-ctx (collect-arm-ctx cfg var->out then-reach))
  (define else-ctx (collect-arm-ctx cfg var->out else-reach))
  (define ctx-set0
    (for/fold ([acc then-ctx])
              ([kv (in-idmap else-ctx)])
      (idmap-set acc (car kv) #t)))
  (define ctx-set1
    (add-entry-phi-srcs-to-ctx cfg ctx-set0 then-bid cond-bid var->out 'then))
  (define ctx-set
    (add-entry-phi-srcs-to-ctx cfg ctx-set1 else-bid cond-bid var->out 'else))
  (define ctx-vars
    (for/pvector ([kv (in-idmap ctx-set)]) (car kv)))
  (define n-ctx (pvector-length ctx-vars))

  (define then-region (build-exit-arm-region cfg then-bid cond-bid ctx-vars 'then))
  (define else-region (build-exit-arm-region cfg else-bid cond-bid ctx-vars 'else))

  (define gamma-val (Gamma (list then-region else-region)))

  ;; Install Gamma in parent: 1 predicate input + n-ctx ctx inputs,
  ;; 0 outputs.
  (define-values (region1 _gnid g-ins _g-outs)
    (region-add-node region gamma-val (add1 n-ctx) 0))

  ;; Wire predicate.
  (define pred-oid
    (or (idmap-ref var->out pred #f)
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
        (or (idmap-ref var->out ctx-var #f)
            (error 'translate-terminal-gamma
                   "undefined ctx var ~a" ctx-var)))
      (define-values (r* _w) (region-add-wire r src-oid (pvector-ref g-ins i)))
      r*))

  (values region3 var->out 'already-terminated))


(define (translate-terminal-switch cfg switch-bid value cases default-bid region var->out)
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

  ;; Union of all arms' ctx-vars, plus each arm's entry-phi source
  ;; VarIds contributed from switch-bid.
  (define ctx-set-reads
    (for/fold ([acc idmap-empty])
              ([reach (in-list reaches)])
      (define arm-ctx (collect-arm-ctx cfg var->out reach))
      (for/fold ([s acc]) ([kv (in-idmap arm-ctx)])
        (idmap-set s (car kv) #t))))
  (define ctx-set
    (for/fold ([s ctx-set-reads])
              ([b (in-list all-bids)]
               [i (in-naturals 0)])
      (add-entry-phi-srcs-to-ctx cfg s b switch-bid var->out
                                 (list 'switch-arm i))))
  (define ctx-vars
    (for/pvector ([kv (in-idmap ctx-set)]) (car kv)))
  (define n-ctx (pvector-length ctx-vars))

  ;; Build each sub-region; tag its info with the switch-case-key.
  (define (build-tagged-arm bid key-tag which)
    (define sub (build-exit-arm-region cfg bid switch-bid ctx-vars which))
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
    (or (idmap-ref var->out value #f)
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
        (or (idmap-ref var->out ctx-var #f)
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

(define (translate-convergent-switch cfg switch-bid value cases default-bid region var->out)
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

  ;; Pick a seed pair for find-branch-join that is NOT already the
  ;; join (otherwise find-branch-join's else-walk starts on the join
  ;; itself and would spuriously declare start-bid as the join).  If
  ;; every arm shares the same branch-bid then that bid is the join —
  ;; all arms empty, which is degenerate.
  (define (pick-nonempty-pair all-bs)
    (let loop ([b1 #f] [bs all-bs])
      (cond
        [(null? bs) (values b1 #f)]
        [(not b1) (loop (car bs) (cdr bs))]
        [(equal? (car bs) b1) (loop b1 (cdr bs))]
        [else (values b1 (car bs))])))
  (define-values (join-bid arm-preds arm-blocks-list)
    (parameterize ([current-shared-set shared-set])
      (define-values (seed-a seed-b) (pick-nonempty-pair all-bids))
      (unless (and seed-a seed-b)
        (error 'translate-convergent-switch
               "all switch arms equal; degenerate convergent switch"))
      (define jb (find-branch-join cfg seed-a seed-b))
      ;; Empty arm: branch-bid == jb.  arm-pred = switch-bid (the
      ;; single direct predecessor of the join along that arm) and
      ;; arm-blocks is empty, matching translate-gamma's empty-arm
      ;; treatment.
      (define (arm-pred-for b)
        (if (equal? b jb) switch-bid (arm-last-before-join cfg b jb)))
      (define (arm-blocks-for b)
        (if (equal? b jb)
            idmap-empty
            (arm-blocks-set cfg b jb)))
      (define preds (for/list ([b (in-list all-bids)]) (arm-pred-for b)))
      (define blocks-list (for/list ([b (in-list all-bids)]) (arm-blocks-for b)))
      (values jb preds blocks-list)))

  (define join-blk (cv-block cfg join-bid))
  (define phis (CfgBlock-phis join-blk))
  (define n-phis (pvector-length phis))

  ;; Context vars: union over all arms of parent-scope reads + per-arm
  ;; phi source contributions.
  (define ctx-set
    (collect-switch-arms-ctx cfg var->out arm-blocks-list arm-preds phis))
  (define ctx-vars
    (for/pvector ([kv (in-idmap ctx-set)]) (car kv)))
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
    (or (idmap-ref var->out value #f)
        (error 'translate-convergent-switch
               "undefined switch value ~a" value)))
  (define-values (region2 _pw)
    (region-add-wire region1 pred-oid (pvector-ref g-ins 0)))

  (define region3
    (for/fold ([r region2])
              ([ctx-var (in-pvector ctx-vars)]
               [i (in-naturals 1)])
      (define src-oid
        (or (idmap-ref var->out ctx-var #f)
            (error 'translate-convergent-switch
                   "undefined ctx var ~a" ctx-var)))
      (define-values (r* _w) (region-add-wire r src-oid (pvector-ref g-ins i)))
      r*))

  ;; Bind join phi outputs to corresponding Gamma outputs.
  (define var->out*
    (for/fold ([m var->out])
              ([phi (in-pvector phis)]
               [oid (in-pvector g-outs)])
      (idmap-set m (PhiInsn-output phi) oid)))

  (values region3 var->out* join-bid))


;; N-arm generalisation of collect-gamma-ctx: walks each arm's
;; blocks + its phi source contribution.
(define (collect-switch-arms-ctx cfg parent-var->out
                                 arm-blocks-list arm-preds phis)
  (define (arm-local-defs blocks)
    (for/fold ([s idmap-empty])
              ([kv (in-idmap blocks)])
      (define blk (cv-block cfg (car kv)))
      (define s1
        (for/fold ([s s]) ([phi (in-pvector (CfgBlock-phis blk))])
          (idmap-set s (PhiInsn-output phi) #t)))
      (for/fold ([s s1]) ([insn (in-pvector (CfgBlock-insns blk))])
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
    (for/fold ([a acc*]) ([phi (in-pvector phis)])
      (define src (pick-phi-source phi arm-pred 'switch-ctx))
      (cond
        [(and (VarId? src)
              (idmap-ref parent-var->out src #f))
         (idmap-set a src #t)]
        [else a])))
  (for/fold ([acc idmap-empty])
            ([blocks (in-list arm-blocks-list)]
             [p (in-list arm-preds)])
    (walk-arm blocks p acc)))


(define (translate-asymmetric-exit-gamma cfg cond-bid pred
                                         then-bid then-exits?
                                         else-bid else-exits?
                                         region var->out)
  (define exit-bid     (if then-exits? then-bid else-bid))
  (define continue-bid (if then-exits? else-bid then-bid))

  (define exit-reach (arm-reach-set cfg exit-bid))
  (define ctx-set-reads (collect-arm-ctx cfg var->out exit-reach))
  (define ctx-set
    (add-entry-phi-srcs-to-ctx cfg ctx-set-reads exit-bid cond-bid
                               var->out 'exit))
  (define ctx-vars
    (for/pvector ([kv (in-idmap ctx-set)]) (car kv)))
  (define n-ctx (pvector-length ctx-vars))

  (define exit-region (build-exit-arm-region cfg exit-bid cond-bid ctx-vars 'exit))
  (define noop-region (build-noop-arm-region n-ctx))

  ;; Place sub-regions in [then, else] order so the raw predicate
  ;; (truthy -> region 0) keeps original Term:cond semantics.
  (define then-region (if then-exits? exit-region noop-region))
  (define else-region (if then-exits? noop-region exit-region))

  (define gamma-val (Gamma (list then-region else-region)))

  (define-values (region1 _gnid g-ins _g-outs)
    (region-add-node region gamma-val (add1 n-ctx) 0))

  (define pred-oid
    (or (idmap-ref var->out pred #f)
        (error 'translate-asymmetric-exit-gamma
               "undefined predicate ~a" pred)))
  (define-values (region2 _pw)
    (region-add-wire region1 pred-oid (pvector-ref g-ins 0)))

  (define region3
    (for/fold ([r region2])
              ([ctx-var (in-pvector ctx-vars)]
               [i (in-naturals 1)])
      (define src-oid
        (or (idmap-ref var->out ctx-var #f)
            (error 'translate-asymmetric-exit-gamma
                   "undefined ctx var ~a" ctx-var)))
      (define-values (r* _w) (region-add-wire r src-oid (pvector-ref g-ins i)))
      r*))

  (values region3 var->out continue-bid))
