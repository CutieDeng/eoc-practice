#lang racket/base

;; ============================================================
;; Component: Java pre-SSA CFG → SSA (M3)
;; ============================================================
;;
;; Convert the pre-SSA CFG produced by `jvm-method->cfg` into SSA
;; form using the standard Cytron et al. algorithm:
;;
;;   1. Collect def-sites of each "local-slot" variable (VarId 0..
;;      local-count-1 carry JVM local semantics and may have many
;;      defs; stack-slot VarIds are fresh by construction and thus
;;      already SSA).
;;   2. Place φ nodes at the iterated dominance frontier of each
;;      local's def-set.  The entry block is treated as an implicit
;;      def of every local slot so that reads of unwritten locals
;;      bind to a phantom "param" name rather than the empty stack.
;;   3. Rename by dominator-tree DFS: each def gets a fresh VarId;
;;      uses consult a per-variable stack of current names.
;;
;; Input : Cfg produced by `jvm-method->cfg` (info carries
;;         'java/max-local, 'java/param-count).
;; Output: Cfg with φ nodes in block.phis; all VfInsn inputs/outputs
;;         and terminator VarIds renamed into strict SSA.
;;
;; ============================================================

(require racket/match
         "../../../kernel/ir/cfg/types.rkt"
         "../../../component/cfg/utils/graph-ops.rkt"
         "../../../component/cfg/analysis/dominance.rkt"
         (except-in "../../../kernel/data/data.rkt" integer-compare))

(provide jvm-cfg->ssa)

;; ============================================================
;; Entry
;; ============================================================

(define (jvm-cfg->ssa cfg)
  (define local-count (cfg-get-info cfg 'java/max-local 0))
  (cond
    [(= local-count 0)
     ;; Nothing to SSA-ify, but still publish an empty param-names
     ;; vector so downstream passes can branch on its presence.
     (cfg-set-info cfg 'java/ssa-param-names (pvector-empty))]
    [else
     (define entry-bid (Cfg-entry cfg))
     (define cfg-phis (insert-phis cfg local-count entry-bid))
     (define idom (cfg-compute-idom cfg-phis))
     (define dom-tree (cfg-compute-dominator-tree cfg-phis))
     (rename-vars cfg-phis local-count entry-bid idom dom-tree)]))

;; ============================================================
;; Phase 1: Phi placement
;; ============================================================

;; Returns ordered-map VarId -> pvector[BlockId] of def-sites
;; (one entry per slot 0..local-count-1).
(define (collect-def-sites cfg local-count entry-bid)
  ;; Start with entry-bid as a phantom def for every local slot.
  (define init
    (for/fold ([m (ordered-map-empty var-id-compare)])
              ([i (in-range local-count)])
      (ordered-map-set m (VarId i) (pvector entry-bid))))

  ;; Add real defs from every VfInsn output whose VarId is in 0..local-count-1.
  (for/fold ([m init])
            ([bid (in-list (cfg-all-block-ids cfg))])
    (define blk (cfg-get-block cfg bid))
    (cond
      [(not blk) m]
      [else
       (for/fold ([m m])
                 ([insn (in-pvector (CfgBlock-insns blk))])
         (for/fold ([m m])
                   ([out (in-pvector (VfInsn-outputs insn))]
                    #:when (and (VarId? out)
                                (< (VarId-id out) local-count)))
           (define cur (ordered-map-ref m out (pvector-empty)))
           ;; Avoid duplicate bids (two stores in same block).
           (if (pvector-member? cur bid)
               m
               (ordered-map-set m out (pvector-cons-right cur bid)))))])))

(define (pvector-member? pv x)
  (for/or ([v (in-pvector pv)]) (equal? v x)))

;; Insert empty phi nodes per iterated dominance frontier.
;; Returns a new CFG with `phis` populated.  Phi sources are empty
;; at this stage; the rename pass fills them in.
(define (insert-phis cfg local-count entry-bid)
  (define def-sites (collect-def-sites cfg local-count entry-bid))
  (define dom-frontier (cfg-compute-dominance-frontier cfg))

  ;; For each variable v, compute phi-placement blocks (IDF of defs).
  ;; Returns ordered-map BlockId -> pvector[VarId] of phis to insert.
  (define phi-plan (ordered-map-empty block-id-compare))

  (define phi-plan*
    (for/fold ([plan phi-plan])
              ([kv (in-ordered-map def-sites)])
      (define v (car kv))
      (define bids (cdr kv))
      (define has-phi (ordered-map-empty block-id-compare))
      (define in-work (ordered-map-empty block-id-compare))

      ;; Worklist seeded with def-sites.
      (define-values (_hp _iw plan*)
        (iterate-phi-placement v bids dom-frontier has-phi in-work plan))
      plan*))

  ;; Install phis into blocks.  Each phi's sources are a pvector of
  ;; (BlockId . VarId) pairs, placeholders filled during rename.  We
  ;; keep the predecessor order stable by querying it once here.
  (define preds-of (cfg-make-predecessors cfg))
  (for/fold ([c cfg])
            ([kv (in-ordered-map phi-plan*)])
    (define bid (car kv))
    (define vars (cdr kv))
    (define blk (cfg-get-block c bid))
    (cond
      [(not blk) c]
      [else
       (define preds (preds-of bid))
       (define new-phis
         (for/pvector ([v (in-pvector vars)])
           (PhiInsn v
                    ;; placeholder: (pred-bid . original-var)
                    (for/pvector ([p (in-pvector preds)])
                      (cons p v)))))
       (define merged-phis
         (pvector-append (CfgBlock-phis blk) new-phis))
       (cfg-set-block c (struct-copy CfgBlock blk [phis merged-phis]))])))

;; Worklist iteration: classical Cytron IDF using an ordered-map
;; as a set-for-seen-blocks, and a pvector worklist.
(define (iterate-phi-placement v def-bids dom-frontier has-phi in-work plan)
  ;; Mark all def-bids as "has been in worklist".
  (define in-work*
    (for/fold ([m in-work]) ([b (in-pvector def-bids)])
      (ordered-map-set m b #t)))

  (let loop ([work def-bids] [has-phi has-phi] [in-work in-work*] [plan plan])
    (cond
      [(= (pvector-length work) 0)
       (values has-phi in-work plan)]
      [else
       (define n (pvector-length work))
       (define b (pvector-ref work (- n 1)))
       (define work* (pvector-take-left work (- n 1)))
       (define df (ordered-map-ref dom-frontier b (ordered-map-empty block-id-compare)))
       (define-values (work** has-phi* plan*)
         (for/fold ([w work*] [hp has-phi] [pl plan])
                   ([d (in-ordered-map-keys df)])
           (cond
             [(ordered-map-ref hp d #f) (values w hp pl)]
             [else
              (define hp* (ordered-map-set hp d #t))
              (define cur-vars (ordered-map-ref pl d (pvector-empty)))
              (define pl* (ordered-map-set pl d (pvector-cons-right cur-vars v)))
              (define w*
                (cond
                  [(ordered-map-ref in-work d #f) w]
                  [else (pvector-cons-right w d)]))
              (values w* hp* pl*)])))
       (loop work** has-phi* in-work plan*)])))

(define (pvector-take-left pv k)
  (for/pvector ([i (in-range k)]) (pvector-ref pv i)))

;; ============================================================
;; Phase 2: Renaming
;; ============================================================
;;
;; Walk the dominator tree DFS.  Maintain a per-variable stack of
;; current names (VarIds).  For each block:
;;   - assign a fresh VarId to every φ output
;;   - rewrite instruction inputs through the stacks, allocate
;;     fresh outputs, push them
;;   - rewrite the terminator's VarId uses
;;   - fill every successor's φ source slots with the current names
;;   - recurse into dominator-tree children
;;   - pop everything pushed in this block
;; ============================================================

;; Per-var rename state: ordered-map VarId -> pvector[VarId]
;; (stack; top is last element)
(define (stk-peek stacks v)
  (define s (ordered-map-ref stacks v (pvector-empty)))
  (cond
    [(= (pvector-length s) 0) #f]
    [else (pvector-ref s (sub1 (pvector-length s)))]))

(define (stk-push stacks v name)
  (define s (ordered-map-ref stacks v (pvector-empty)))
  (ordered-map-set stacks v (pvector-cons-right s name)))

(define (stk-pop stacks v)
  (define s (ordered-map-ref stacks v (pvector-empty)))
  (cond
    [(= (pvector-length s) 0) stacks]
    [else (ordered-map-set stacks v (pvector-take-left s (sub1 (pvector-length s))))]))

(define (rename-vars cfg local-count entry-bid idom dom-tree)
  ;; Seed stacks with fresh names for each local slot at entry.
  (define vc0 (Cfg-var-cnt cfg))
  (define-values (init-stacks param-names vc1)
    (for/fold ([stk (ordered-map-empty var-id-compare)]
               [names (pvector-empty)]
               [vc vc0])
              ([i (in-range local-count)])
      (define fresh (VarId vc))
      (values (stk-push stk (VarId i) fresh)
              (pvector-cons-right names fresh)
              (add1 vc))))

  (define preds-of (cfg-make-predecessors cfg))

  ;; Run DFS; carry (cfg, stacks, vc).  Return final cfg + vc.
  (define-values (cfg-out _stk-out vc-out)
    (rename-block cfg entry-bid init-stacks vc1 preds-of dom-tree local-count))

  ;; Record the parameter VarIds so downstream passes (RVSDG lowering)
  ;; can recover which SSA names represent the entry-visible locals.
  (define cfg-with-info
    (cfg-set-info cfg-out 'java/ssa-param-names param-names))
  (struct-copy Cfg cfg-with-info [var-cnt vc-out]))

(define (rename-block cfg bid stacks vc preds-of dom-tree local-count)
  (define blk (cfg-get-block cfg bid))
  (when (not blk)
    (error 'rename-block "missing block: ~a" bid))

  ;; Track renames pushed in this block so we can pop them on exit.
  (define pushed-in-block (pvector-empty))

  ;; --- Step 1: assign fresh names to phi outputs. ---
  (define-values (phis-renamed stk1 vc1 pushed1)
    (for/fold ([acc (pvector-empty)] [stk stacks] [vc vc] [pushed pushed-in-block])
              ([phi (in-pvector (CfgBlock-phis blk))])
      (define orig (PhiInsn-output phi))
      (define fresh (VarId vc))
      (values (pvector-cons-right acc (PhiInsn fresh (PhiInsn-sources phi)))
              (stk-push stk orig fresh)
              (add1 vc)
              (pvector-cons-right pushed orig))))

  ;; --- Step 2: rewrite VfInsn list. ---
  (define-values (insns-renamed stk2 vc2 pushed2)
    (for/fold ([acc (pvector-empty)] [stk stk1] [vc vc1] [pushed pushed1])
              ([insn (in-pvector (CfgBlock-insns blk))])
      ;; Rewrite inputs: VarIds in 0..local-count-1 consult the stack;
      ;; higher VarIds stay as-is (already SSA); non-VarId literals pass through.
      (define new-inputs
        (for/pvector ([x (in-pvector (VfInsn-inputs insn))])
          (cond
            [(and (VarId? x) (< (VarId-id x) local-count))
             (or (stk-peek stk x)
                 (error 'rename-block
                        "use of undefined local ~a in block ~a" x bid))]
            [else x])))
      ;; Allocate fresh outputs; track which originals we've pushed.
      (define-values (new-outputs stk* vc* pushed*)
        (for/fold ([outs (pvector-empty)] [s stk] [vc vc] [pu pushed])
                  ([o (in-pvector (VfInsn-outputs insn))])
          (cond
            [(and (VarId? o) (< (VarId-id o) local-count))
             (define fresh (VarId vc))
             (values (pvector-cons-right outs fresh)
                     (stk-push s o fresh)
                     (add1 vc)
                     (pvector-cons-right pu o))]
            [else
             (values (pvector-cons-right outs o) s vc pu)])))
      (define new-insn
        (struct-copy VfInsn insn
          [inputs new-inputs]
          [outputs new-outputs]))
      (values (pvector-cons-right acc new-insn) stk* vc* pushed*)))

  ;; --- Step 3: rewrite terminator uses. ---
  (define new-term
    (rewrite-terminator (CfgBlock-terminator blk) stk2 local-count bid))

  (define blk-renamed
    (struct-copy CfgBlock blk
      [phis phis-renamed]
      [insns insns-renamed]
      [terminator new-term]))

  (define cfg1 (cfg-set-block cfg blk-renamed))

  ;; --- Step 4: patch successors' φ source slots. ---
  (define cfg2
    (for/fold ([c cfg1])
              ([succ (in-pvector (terminator-successors new-term))])
      (fill-phi-sources c succ bid stk2 local-count)))

  ;; --- Step 5: recurse into dominator-tree children. ---
  (define children (ordered-map-ref dom-tree bid (pvector-empty)))
  (define-values (cfg3 _stk-child vc3)
    (for/fold ([c cfg2] [s stk2] [v vc2]) ([ch (in-pvector children)])
      (rename-block c ch s v preds-of dom-tree local-count)))

  ;; --- Step 6: pop names pushed in this block. ---
  ;; `stacks` parameter is the incoming-to-this-block state; we
  ;; must return that so the caller's DFS sibling sees unchanged state.
  (values cfg3 stacks vc3))

(define (rewrite-terminator term stacks local-count bid)
  (define (rn v)
    (cond
      [(and (VarId? v) (< (VarId-id v) local-count))
       (or (stk-peek stacks v)
           (error 'rewrite-terminator
                  "use of undefined local ~a in block ~a" v bid))]
      [else v]))
  (match term
    [(Term:jump _) term]
    [(Term:cond cv t e) (Term:cond (rn cv) t e)]
    [(Term:switch v cs d) (Term:switch (rn v) cs d)]
    [(Term:ret vs)
     (Term:ret (for/pvector ([x (in-pvector vs)]) (rn x)))]
    [(Term:throw ex) (Term:throw (rn ex))]
    [(Term:unreachable) term]
    [_ term]))

(define (fill-phi-sources cfg succ-bid pred-bid stacks local-count)
  (define blk (cfg-get-block cfg succ-bid))
  (cond
    [(not blk) cfg]
    [else
     (define phis (CfgBlock-phis blk))
     (cond
       [(= (pvector-length phis) 0) cfg]
       [else
        (define new-phis
          (for/pvector ([phi (in-pvector phis)])
            (define new-sources
              (for/pvector ([src (in-pvector (PhiInsn-sources phi))])
                (match src
                  [(cons bid orig)
                   (cond
                     [(equal? bid pred-bid)
                      (cond
                        [(and (VarId? orig)
                              (< (VarId-id orig) local-count))
                         (cons bid
                               (or (stk-peek stacks orig)
                                   (error 'fill-phi-sources
                                          "undefined ~a at pred ~a"
                                          orig pred-bid)))]
                        [else src])]
                     [else src])])))
            (PhiInsn (PhiInsn-output phi) new-sources)))
        (cfg-set-block cfg (struct-copy CfgBlock blk [phis new-phis]))])]))
