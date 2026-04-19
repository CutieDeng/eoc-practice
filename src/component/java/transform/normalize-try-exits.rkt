#lang racket/base

;; ============================================================
;; Component: Normalize try/catch exits (Kappa-recovery prep)
;; ============================================================
;;
;; Kappa lowering in cfg-to-rvsdg requires each try region to have
;; AT MOST one fall-through exit block — the sub-region's
;; `Simple '(region-result M)` can only feed one downstream successor
;; in the parent region.  Raw javac bytecode happily produces try
;; windows with several fall-through exits (e.g. multiple early
;; `return` paths inside the guarded range, or an if-else that
;; diverges before the try ends).  This pass inserts a synthetic
;; joiner block `J` and a per-edge selector VfInsn so every exit
;; edge lands on `J`; `J`'s `Term:switch` then dispatches to the
;; original targets.
;;
;; Runs post-SSA, pre-RVSDG.  For methods without
;; `'java/exception-table` the pass is identity.  For each table
;; entry:
;;   - Compute the window W = blocks whose ordinal index lies in
;;     [start-ord, end-ord).  Handlers are excluded.
;;   - Collect exit edges (src-bid, role, target-bid) with src ∈ W
;;     and target ∉ W (targets MAY be handler blocks; those are
;;     graph-unreachable today and thus don't appear as successors,
;;     but we filter defensively just in case).
;;   - If {distinct targets} has size ≤ 1, the window is already
;;     single-exit: return the CFG unchanged.
;;   - Otherwise, allocate a fresh joiner block and rewire every
;;     exit edge through it.  Term:jump edges append a selector
;;     VfInsn to the source block and retarget the jump; Term:cond /
;;     Term:switch edges each get a 2-insn forwarding block (selector
;;     write + Term:jump to joiner) inserted between source and
;;     joiner.  Phis at each exit target have their W-source slots
;;     collapsed into a single (joiner . merged-VarId) entry and the
;;     joiner gets one merge phi per rewritten phi.
;;
;; SSA is preserved throughout: every fresh VarId is defined once
;; (each selector write and each joiner phi is unique), and joiner-
;; merge phis provide the per-predecessor fan-in required by the
;; rewritten target phi sources.
;;
;; `'java/block-order` is updated to append joiner / forwarding
;; BlockIds in insertion order.  `'java/exception-table` is
;; unchanged: the inserted blocks live outside the try window, so
;; they don't affect window semantics.  Per-block `'java/covering-
;; handlers` annotations are untouched.
;;
;; ============================================================

(require racket/match
         (only-in cutie-ftree/graph graph-add-vertex graph-add-edge
                  graph-remove-edges-between)
         "../../../kernel/ir/cfg/types.rkt"
         "../../../component/cfg/utils/graph-ops.rkt"
         (except-in "../../../kernel/data/data.rkt" integer-compare))

(provide normalize-try-exits)

;; ============================================================
;; Entry
;; ============================================================

(define (normalize-try-exits cfg)
  (define table (cfg-get-info cfg 'java/exception-table #f))
  (cond
    [(or (not table) (= (pvector-length table) 0)) cfg]
    [else
     (define block-order (cfg-get-info cfg 'java/block-order #f))
     (unless block-order
       (error 'normalize-try-exits
              "Cfg.info missing 'java/block-order (required after jvm-to-cfg)"))
     ;; Pre-compute block-ordinal map once.
     (define bid->ord (compute-bid->ord block-order))
     (define handler-set (collect-handler-bids table))
     (for/fold ([c cfg]) ([entry (in-pvector table)])
       (define window (resolve-window entry bid->ord block-order))
       (normalize-one-window c window handler-set))]))

;; ============================================================
;; Window resolution
;; ============================================================

(define (compute-bid->ord block-order)
  (for/fold ([m (ordered-map-empty block-id-compare)])
            ([bid (in-pvector block-order)]
             [i (in-naturals)])
    (ordered-map-set m bid i)))

;; `'java/exception-table` record is (list start-bid end-bid handler-bid catch-type).
;; `end-bid` may be #f when the JVM range ends at method-end.
(define (resolve-window entry bid->ord block-order)
  (match entry
    [(list start-bid end-bid _handler-bid _catch-type)
     (define start-ord (ordered-map-ref bid->ord start-bid #f))
     (unless start-ord
       (error 'normalize-try-exits "unmapped start-bid: ~s" start-bid))
     (define end-ord
       (cond
         [end-bid (ordered-map-ref bid->ord end-bid #f)]
         [else (pvector-length block-order)]))
     (unless end-ord
       (error 'normalize-try-exits "unmapped end-bid: ~s" end-bid))
     (for/fold ([s (ordered-map-empty block-id-compare)])
               ([bid (in-pvector block-order)]
                [i (in-naturals)]
                #:when (and (<= start-ord i) (< i end-ord)))
       (ordered-map-set s bid #t))]))

(define (collect-handler-bids table)
  (for/fold ([s (ordered-map-empty block-id-compare)])
            ([entry (in-pvector table)])
    (match entry
      [(list _start _end handler-bid _ct)
       (ordered-map-set s handler-bid #t)])))

(define (in-window? window bid)
  (and (ordered-map-ref window bid #f) #t))

;; ============================================================
;; Exit-edge collection
;; ============================================================
;;
;; An exit edge is (src-bid, role, target-bid) where src ∈ W and
;; target ∉ W ∧ target ∉ handlers(all windows).  `role` encodes
;; which successor slot of src's terminator produced the edge so we
;; can rewrite it precisely:
;;   'jump            — Term:jump's single target
;;   'cond-then       — Term:cond then-target
;;   'cond-else       — Term:cond else-target
;;   (cons 'case i)   — Term:switch cases[i] (0-based)
;;   'switch-default  — Term:switch default

(define (collect-exit-edges cfg window handler-set)
  (for/fold ([acc (pvector-empty)])
            ([bid (in-ordered-map-keys window)])
    (define blk (cfg-get-block cfg bid))
    (cond
      [(not blk) acc]
      [else
       (define t (CfgBlock-terminator blk))
       (match t
         [(Term:jump target)
          (if (and (not (in-window? window target))
                   (not (ordered-map-ref handler-set target #f)))
              (pvector-cons-right acc (list bid 'jump target))
              acc)]
         [(Term:cond _cv then-t else-t)
          (define a1
            (if (and (not (in-window? window then-t))
                     (not (ordered-map-ref handler-set then-t #f)))
                (pvector-cons-right acc (list bid 'cond-then then-t))
                acc))
          (if (and (not (in-window? window else-t))
                   (not (ordered-map-ref handler-set else-t #f)))
              (pvector-cons-right a1 (list bid 'cond-else else-t))
              a1)]
         [(Term:switch _v cases default)
          (define a1
            (for/fold ([a acc])
                      ([kv (in-pvector cases)]
                       [i (in-naturals)])
              (define tgt (cdr kv))
              (if (and (not (in-window? window tgt))
                       (not (ordered-map-ref handler-set tgt #f)))
                  (pvector-cons-right a (list bid (cons 'case i) tgt))
                  a)))
          (if (and (not (in-window? window default))
                   (not (ordered-map-ref handler-set default #f)))
              (pvector-cons-right a1 (list bid 'switch-default default))
              a1)]
         [_ acc])])))

;; Distinct exit targets in first-seen order.
(define (distinct-targets edges)
  (define-values (pv _seen)
    (for/fold ([pv (pvector-empty)]
               [seen (ordered-map-empty block-id-compare)])
              ([e (in-pvector edges)])
      (define tgt (caddr e))
      (cond
        [(ordered-map-ref seen tgt #f) (values pv seen)]
        [else (values (pvector-cons-right pv tgt)
                      (ordered-map-set seen tgt #t))])))
  pv)

;; ============================================================
;; One-window normalization
;; ============================================================

(define (normalize-one-window cfg window handler-set)
  (define edges (collect-exit-edges cfg window handler-set))
  (define targets (distinct-targets edges))
  (cond
    [(<= (pvector-length targets) 1) cfg]
    [else (insert-joiner cfg edges targets)]))

(define (target-index targets tgt)
  (for/or ([t (in-pvector targets)] [i (in-naturals)])
    (and (equal? t tgt) i)))

(define (insert-joiner cfg edges targets)
  ;; Phase A: allocate joiner block-id in the graph.
  (define-values (g1 joiner-bid) (graph-add-vertex (Cfg-graph cfg)))
  (define cfg1 (struct-copy Cfg cfg [graph g1]))

  ;; Phase B: for each edge, decide inline-vs-forwarding and do the
  ;; per-source rewrite.  Produces:
  ;;   edge-plans : pvector of (list edge pred-bid sel-var)
  ;;                where pred-bid is the ACTUAL predecessor of
  ;;                joiner-bid (= src for Term:jump, = forwarding bid
  ;;                otherwise), and sel-var is the fresh VarId that
  ;;                holds the selector index on that edge.
  (define-values (cfg2 edge-plans vc1)
    (for/fold ([c cfg1]
               [plans (pvector-empty)]
               [vc (Cfg-var-cnt cfg1)])
              ([e (in-pvector edges)])
      (match-define (list src-bid role tgt-bid) e)
      (define sel-idx (target-index targets tgt-bid))
      (define sel-var (VarId vc))
      (define vc* (add1 vc))
      (cond
        [(eq? role 'jump)
         ;; Inline: append selector VfInsn to src, retarget Term:jump.
         (define c* (rewrite-jump-source c src-bid sel-var sel-idx joiner-bid))
         (values c*
                 (pvector-cons-right plans (list e src-bid sel-var))
                 vc*)]
        [else
         ;; Branch / switch arm: allocate forwarding block.
         (define-values (c* fwd-bid)
           (install-forwarding-block c src-bid role tgt-bid
                                     sel-var sel-idx joiner-bid))
         (values c*
                 (pvector-cons-right plans (list e fwd-bid sel-var))
                 vc*)])))

  ;; Phase C: build joiner block's phis (selector merge + per-target
  ;; value merges) and terminator; install in cfg.
  (define cfg3 (install-joiner-block cfg2 joiner-bid edge-plans targets vc1))

  ;; Phase D: patch exit targets' phis to draw from joiner.
  (define cfg4 (patch-target-phis cfg3 joiner-bid edge-plans targets))

  ;; Phase E: publish joiner (and any forwarding bids) into block-
  ;; order and bump var-cnt.  Forwarding bids were already appended to
  ;; block-order by install-forwarding-block.
  (append-to-block-order cfg4 joiner-bid))

;; ============================================================
;; Phase B helpers: rewrite source terminators
;; ============================================================

(define (make-selector-insn sel-var sel-idx)
  (VfInsn 'kappa-exit-sel
          (pvector-cons-right (pvector-empty) sel-idx)
          (pvector-cons-right (pvector-empty) sel-var)
          #f #f))

(define (rewrite-jump-source cfg src-bid sel-var sel-idx joiner-bid)
  (define blk (cfg-get-block cfg src-bid))
  (define old-t (CfgBlock-terminator blk))
  (unless (Term:jump? old-t)
    (error 'rewrite-jump-source
           "expected Term:jump at ~s, got ~s" src-bid old-t))
  (define old-tgt (Term:jump-target old-t))
  (define insns*
    (pvector-cons-right (CfgBlock-insns blk) (make-selector-insn sel-var sel-idx)))
  (define blk*
    (struct-copy CfgBlock blk
      [insns insns*]
      [terminator (Term:jump joiner-bid)]))
  (define g1 (graph-remove-edges-between (Cfg-graph cfg) src-bid old-tgt))
  (define-values (g2 _eid) (graph-add-edge g1 src-bid joiner-bid))
  (cfg-set-block (struct-copy Cfg cfg [graph g2]) blk*))

;; Install a forwarding block F that writes the selector and jumps
;; to joiner-bid; retarget src's terminator arm (role) from tgt-bid
;; to F.  Returns (values cfg fwd-bid).
(define (install-forwarding-block cfg src-bid role tgt-bid
                                  sel-var sel-idx joiner-bid)
  (define-values (g1 fwd-bid) (graph-add-vertex (Cfg-graph cfg)))
  (define fwd-block
    (CfgBlock fwd-bid
              (pvector-empty)
              (pvector-cons-right (pvector-empty)
                                  (make-selector-insn sel-var sel-idx))
              (Term:jump joiner-bid)
              (ordered-map-empty symbol-compare)))
  (define cfg1 (cfg-set-block (struct-copy Cfg cfg [graph g1]) fwd-block))
  ;; Retarget src arm.
  (define src-blk (cfg-get-block cfg1 src-bid))
  (define old-t (CfgBlock-terminator src-blk))
  (define new-t (retarget-term old-t role tgt-bid fwd-bid))
  (define src-blk* (struct-copy CfgBlock src-blk [terminator new-t]))
  (define cfg2 (cfg-set-block cfg1 src-blk*))
  ;; Adjust graph edges: src --removed--> tgt-bid; add src->fwd, fwd->joiner.
  (define g2 (graph-remove-edges-between (Cfg-graph cfg2) src-bid tgt-bid))
  (define-values (g3 _e1) (graph-add-edge g2 src-bid fwd-bid))
  (define-values (g4 _e2) (graph-add-edge g3 fwd-bid joiner-bid))
  (define cfg3 (struct-copy Cfg cfg2 [graph g4]))
  ;; Append fwd-bid to block-order immediately so downstream passes
  ;; observe a consistent linearisation.
  (values (append-to-block-order cfg3 fwd-bid) fwd-bid))

(define (retarget-term term role old-tgt new-tgt)
  (match term
    [(Term:cond cv t e)
     (cond
       [(and (eq? role 'cond-then) (equal? t old-tgt))
        (Term:cond cv new-tgt e)]
       [(and (eq? role 'cond-else) (equal? e old-tgt))
        (Term:cond cv t new-tgt)]
       [else (error 'retarget-term "Term:cond role ~s doesn't match ~s"
                    role old-tgt)])]
    [(Term:switch v cases default)
     (cond
       [(eq? role 'switch-default)
        (Term:switch v cases new-tgt)]
       [(and (pair? role) (eq? (car role) 'case))
        (define i (cdr role))
        (define cases*
          (for/pvector ([kv (in-pvector cases)] [j (in-naturals)])
            (cond
              [(= j i) (cons (car kv) new-tgt)]
              [else kv])))
        (Term:switch v cases* default)])]))

;; ============================================================
;; Phase C: build the joiner block
;; ============================================================

(define (install-joiner-block cfg joiner-bid edge-plans targets vc)
  ;; 1. Selector merge phi: merges each edge's sel-var under
  ;;    joiner's predecessors (the pred-bid in each plan).
  (define sel-phi-output (VarId vc))
  (define vc1 (add1 vc))
  (define sel-phi-sources
    (for/pvector ([p (in-pvector edge-plans)])
      (match-define (list _e pred-bid sel-var) p)
      (cons pred-bid sel-var)))
  (define sel-phi (PhiInsn sel-phi-output sel-phi-sources))

  ;; 2. Per-target value merges: for each phi at each exit target
  ;;    that has W-source slots, build one merge phi at joiner that
  ;;    fans in per-joiner-predecessor values.  The merge is
  ;;    constructed only over phis that actually have a W-source —
  ;;    pure non-W phis are left alone.
  (define-values (merge-phis merge-bindings vc2)
    (build-target-merges cfg joiner-bid edge-plans targets vc1))

  ;; 3. Assemble joiner block.
  (define joiner-phis (pvector-cons-left merge-phis sel-phi))
  (define joiner-cases
    (for/pvector ([t (in-pvector targets)] [i (in-naturals)])
      (cons i t)))
  (define joiner-default (pvector-ref targets 0))
  (define joiner-term
    (Term:switch sel-phi-output joiner-cases joiner-default))
  (define joiner-block
    (CfgBlock joiner-bid joiner-phis (pvector-empty) joiner-term
              (let* ([m (ordered-map-empty symbol-compare)]
                     [m (ordered-map-set m 'java/kappa-joiner #t)]
                     [m (ordered-map-set m 'java/kappa-joiner-target-merges
                                         merge-bindings)])
                m)))

  (define cfg1 (cfg-set-block cfg joiner-block))

  ;; 4. Graph edges from joiner to each target.
  (define cfg2
    (for/fold ([c cfg1]) ([t (in-pvector targets)])
      (define-values (g _e) (graph-add-edge (Cfg-graph c) joiner-bid t))
      (struct-copy Cfg c [graph g])))

  (struct-copy Cfg cfg2 [var-cnt vc2]))

;; For each target T_j, for each phi at T_j whose sources include at
;; least one (src-bid . v) with src-bid ∈ joiner's predecessors,
;; allocate a merge phi at joiner that fans in one slot per edge-plan
;; entry.  Slots whose edge targets T_j copy the phi's original value
;; for that src; slots whose edge targets a different T use the phi's
;; first W-source value as a placeholder (dynamically unreachable via
;; the Term:switch dispatch).
;;
;; Returns (values merge-phis-pv merge-bindings vc').
;;   merge-phis-pv : pvector[PhiInsn]
;;   merge-bindings : pvector[(list target-bid old-output merge-output)]
;;                    — tells phase D how to rewrite each exit
;;                    target's phi sources.
(define (build-target-merges cfg joiner-bid edge-plans targets vc)
  ;; Predecessor bids of joiner, in the same order as edge-plans.
  (define pred-bids
    (for/pvector ([p (in-pvector edge-plans)]) (cadr p)))

  ;; For each edge-plan entry, remember which target was taken.
  (define plan-target-of
    (for/pvector ([p (in-pvector edge-plans)])
      (caddr (car p))))  ; edge = (list src-bid role target-bid) → target

  (define-values (phis bindings vc*)
    (for/fold ([phis-acc (pvector-empty)]
               [binds-acc (pvector-empty)]
               [vc vc])
              ([t (in-pvector targets)])
      (define t-blk (cfg-get-block cfg t))
      (cond
        [(not t-blk) (values phis-acc binds-acc vc)]
        [else
         (for/fold ([phis-acc phis-acc]
                    [binds-acc binds-acc]
                    [vc vc])
                   ([orig-phi (in-pvector (CfgBlock-phis t-blk))])
           (define w-src-map (collect-w-source-map orig-phi edge-plans t))
           (cond
             [(= 0 (ordered-map-count w-src-map))
              (values phis-acc binds-acc vc)]
             [else
              (define placeholder
                (first-value-of w-src-map))
              (define merge-out (VarId vc))
              (define merge-sources
                (for/pvector ([pb (in-pvector pred-bids)]
                              [pt (in-pvector plan-target-of)])
                  (cond
                    [(equal? pt t)
                     (cons pb (ordered-map-ref w-src-map pb placeholder))]
                    [else (cons pb placeholder)])))
              (values
                (pvector-cons-right phis-acc
                                    (PhiInsn merge-out merge-sources))
                (pvector-cons-right binds-acc
                                    (list t (PhiInsn-output orig-phi)
                                          merge-out))
                (add1 vc))]))])))
  (values phis bindings vc*))

;; For a phi in target T, return ordered-map[W-source-bid → value]
;; (renaming src-bids to their joiner-predecessor bid if they went
;; through a forwarding block).  The W-source bid in the ORIGINAL
;; phi is the CFG-side predecessor before rewriting; we look up its
;; matching edge-plan to find the new pred-bid.
(define (collect-w-source-map orig-phi edge-plans target)
  (for/fold ([m (ordered-map-empty block-id-compare)])
            ([src (in-pvector (PhiInsn-sources orig-phi))])
    (define orig-pred-bid (car src))
    (define v (cdr src))
    ;; Find the edge-plan whose source and target match.  For the
    ;; Term:jump inline case, edge-plan pred-bid == orig-pred-bid;
    ;; for the forwarding case, edge-plan pred-bid is the forwarding
    ;; block, and we detect the match via the edge's src-bid
    ;; (= orig-pred-bid) and target.
    (define maybe-plan
      (for/or ([p (in-pvector edge-plans)])
        (match-define (list edge pred-bid _sel) p)
        (match-define (list esrc _role etgt) edge)
        (and (equal? esrc orig-pred-bid)
             (equal? etgt target)
             pred-bid)))
    (cond
      [maybe-plan (ordered-map-set m maybe-plan v)]
      [else m])))

(define (first-value-of m)
  (for/or ([kv (in-ordered-map m)]) (cdr kv)))

;; ============================================================
;; Phase D: patch each exit target's phis to source from joiner
;; ============================================================

(define (patch-target-phis cfg joiner-bid edge-plans targets)
  ;; merge-bindings lives on the joiner block's info.
  (define joiner-blk (cfg-get-block cfg joiner-bid))
  (define bindings
    (ordered-map-ref (CfgBlock-info joiner-blk)
                     'java/kappa-joiner-target-merges (pvector-empty)))
  ;; Group bindings by target.
  (define binding-map
    (for/fold ([m (ordered-map-empty block-id-compare)])
              ([b (in-pvector bindings)])
      (match-define (list t old-out new-out) b)
      (define cur (ordered-map-ref m t (ordered-map-empty var-id-compare)))
      (ordered-map-set m t (ordered-map-set cur old-out new-out))))
  ;; Set of pred-bids we're consolidating into joiner, so we can
  ;; recognise which existing phi sources to strip.
  (define w-pred-set
    (for/fold ([s (ordered-map-empty block-id-compare)])
              ([p (in-pvector edge-plans)])
      (ordered-map-set s (cadr p) #t)))
  ;; Also mark the ORIGINAL src-bids as "to be stripped" — for the
  ;; Term:jump case the pred-bid IS the src, but for forwarding the
  ;; original phi still references src directly (never saw the
  ;; forwarding block).  Build a map orig-src → joiner-pred-bid.
  (define src->joiner-pred
    (for/fold ([m (ordered-map-empty block-id-compare)])
              ([p (in-pvector edge-plans)])
      (match-define (list edge pred-bid _sel) p)
      (ordered-map-set m (car edge) pred-bid)))
  (for/fold ([c cfg]) ([t (in-pvector targets)])
    (define t-blk (cfg-get-block c t))
    (cond
      [(not t-blk) c]
      [else
       (define t-merges
         (ordered-map-ref binding-map t (ordered-map-empty var-id-compare)))
       (define new-phis
         (for/pvector ([phi (in-pvector (CfgBlock-phis t-blk))])
           (rewrite-target-phi phi src->joiner-pred joiner-bid t-merges)))
       (cfg-set-block c (struct-copy CfgBlock t-blk [phis new-phis]))])))

(define (rewrite-target-phi phi src->joiner-pred joiner-bid t-merges)
  (define old-sources (PhiInsn-sources phi))
  ;; Partition into W-sources (by original src bid) and others.
  (define-values (kept joiner-added?)
    (for/fold ([kept (pvector-empty)] [added? #f])
              ([src (in-pvector old-sources)])
      (define orig-pred (car src))
      (cond
        [(ordered-map-ref src->joiner-pred orig-pred #f)
         ;; Strip this W-source; consolidate later.
         (values kept added?)]
        [else
         (values (pvector-cons-right kept src) added?)])))
  ;; If this phi had a W-source, add one (joiner . merge-out) entry.
  (define merge-out
    (ordered-map-ref t-merges (PhiInsn-output phi) #f))
  (define final-sources
    (cond
      [merge-out
       (pvector-cons-right kept (cons joiner-bid merge-out))]
      [else kept]))
  (PhiInsn (PhiInsn-output phi) final-sources))

;; ============================================================
;; Block-order bookkeeping
;; ============================================================

(define (append-to-block-order cfg bid)
  (define old (cfg-get-info cfg 'java/block-order (pvector-empty)))
  (cfg-set-info cfg 'java/block-order
                (pvector-cons-right old bid)))
