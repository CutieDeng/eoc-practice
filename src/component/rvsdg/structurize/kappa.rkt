#lang racket/base

;; ============================================================
;; CFG -> RVSDG structurizer: try/catch windows -> Kappa
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

(provide compute-kappa-groups kappa-end-outer-first? refine-kappa-join compute-kappa-window-bids exception-table-handler-set classify-kappa-window forwarding-block? extend-with-forwarding-blocks kappa-forward-reach find-kappa-arm-preds build-kappa-try-region build-kappa-handler-region handler-entry-phi-start-srcs kappa-groups-without finish-kappa-sub-region/cfg translate-kappa-group)


;; ============================================================
;; Kappa recovery (try/catch)
;; ============================================================
;;
;; `compute-kappa-groups` runs once at cfg->rvsdg entry, producing an
;; idmap[try-entry-BlockId -> Kappa-Group] keyed on each
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
  (define table (cv-info cfg 'java/exception-table #f))
  (cond
    [(or (not table) (= (pvector-length table) 0))
     idmap-empty]
    [else
     ;; Fold entries by (start-bid, end-bid), preserving declaration
     ;; order of handlers within each distinct window.  Entries with
     ;; the same (start, end) are merged into one Kappa with a multi-
     ;; handler list (first-wins).  Entries with the same start but
     ;; different end materialise as SEPARATE Kappa-Groups, sorted
     ;; outer-first (descending end) under a single start-bid key —
     ;; the outer Kappa's try-region physically contains the inner
     ;; start-bid, so translate-segment re-dispatches into the inner
     ;; Kappa once the outer pops itself via `kappa-groups-without`.
     (define by-start-end
       (for/fold ([m idmap-empty])
                 ([entry (in-pvector table)])
         (match-define (list start-bid end-bid handler-bid catch-type) entry)
         (define by-end (idmap-ref m start-bid #f))
         (define by-end* (or by-end idmap-empty))
         (define handlers0 (idmap-ref by-end* end-bid (pvector-empty)))
         (define handlers1
           (pvector-cons-right handlers0 (cons catch-type handler-bid)))
         (idmap-set m start-bid
                          (idmap-set by-end* end-bid handlers1))))
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
     (for/fold ([acc idmap-empty])
               ([kv (in-idmap by-start-end)])
       (define start-bid (car kv))
       (define by-end (cdr kv))
       ;; Collect (end-bid . handlers) pairs and sort outer-first.
       ;; end-bid = #f (method-end) is the widest possible window, so
       ;; it sorts as "outermost"; otherwise descending by the end
       ;; block's ordinal.
       (define entries
         (for/list ([ekv (in-idmap by-end)])
           (cons (car ekv) (cdr ekv))))
       (define sorted-entries
         (sort entries (kappa-end-outer-first? cfg)))
       (define stack
         (for/pvector ([pair (in-list sorted-entries)])
           (define end-bid (car pair))
           (define handlers (cdr pair))
           (define window-bids (compute-kappa-window-bids cfg start-bid end-bid))
           (define-values (kind0 join-bid0) (classify-kappa-window cfg window-bids))
           (define-values (kind join-bid chain-bids)
             (refine-kappa-join cfg kind0 join-bid0 handlers))
           (define try-bids0 (extend-with-forwarding-blocks cfg window-bids join-bid))
           (define try-bids
             (for/fold ([s try-bids0]) ([b (in-pvector chain-bids)])
               (idmap-set s b #t)))
           (Kappa-Group try-bids handlers kind join-bid)))
       (idmap-set acc start-bid stack))]))


;; compute-kappa-groups' inner by-end map is keyed by end-bid or #f
;; (method-end).  idmap sorts #f before every real bid, so iteration
;; yields #f first; the final stack ordering is produced by
;; kappa-end-outer-first? below (which maps #f to +inf to put it
;; outermost).

;; Returns a comparator on (cons end-bid handlers) pairs that orders
;; the OUTER (widest) window first.  end-bid=#f (method-end) maps to
;; +inf; otherwise use the end block's ordinal in 'java/block-order.
;; Pairs that don't resolve compare equal (stable sort preserves
;; declaration order for those).
(define (kappa-end-outer-first? cfg)
  (define block-order
    (or (cv-info cfg 'java/block-order #f)
        (error 'kappa-end-outer-first? "Cfg.info missing 'java/block-order")))
  (define bid->ord
    (for/fold ([m idmap-empty])
              ([bid (in-pvector block-order)]
               [i (in-naturals)])
      (idmap-set m bid i)))
  (define (end-rank end-bid)
    (cond
      [(not end-bid) +inf.0]
      [else (or (idmap-ref bid->ord end-bid #f) -inf.0)]))
  (lambda (a b)
    (> (end-rank (car a)) (end-rank (car b)))))


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
         (idmap-ref hr bid #f)))
     (cond
       [(in-all-handler-reaches? initial-join)
        (values kind initial-join empty-chain)]
       [else
        ;; Walk forward through trivial Term:jump blocks (no phis, no
        ;; VfInsns) until we hit a common block or a non-trivial one.
        (let loop ([b initial-join]
                   [chain empty-chain]
                   [visited idmap-empty])
          (cond
            [(idmap-ref visited b #f)
             ;; Cycle — give up, keep the original join.
             (values kind initial-join empty-chain)]
            [(in-all-handler-reaches? b)
             (values kind b chain)]
            [else
             (define blk (cv-block cfg b))
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
                         (idmap-set visited b #t))]
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
    (or (cv-info cfg 'java/block-order #f)
        (error 'compute-kappa-window-bids
               "Cfg.info missing 'java/block-order (run jvm-to-cfg first)")))
  (define bid->ord
    (for/fold ([m idmap-empty])
              ([bid (in-pvector block-order)]
               [i (in-naturals)])
      (idmap-set m bid i)))
  (define start-ord
    (or (idmap-ref bid->ord start-bid #f)
        (error 'compute-kappa-window-bids
               "unmapped start-bid ~s" start-bid)))
  ;; When end-bid=#f the original JVM range extends to method-end.
  ;; jvm-to-cfg records the pre-normalize block count as
  ;; 'java/original-block-count so later passes can resolve this
  ;; without the full block-order being length-stable (normalize-try
  ;; -exits appends forwarding / joiner blocks past the original
  ;; tail).  Using the original count keeps those appended blocks
  ;; out of the try window.
  (define end-ord
    (cond
      [end-bid
       (or (idmap-ref bid->ord end-bid #f)
           (error 'compute-kappa-window-bids
                  "unmapped end-bid ~s" end-bid))]
      [else
       (or (cv-info cfg 'java/original-block-count #f)
           (error 'compute-kappa-window-bids
                  "try window at ~s has end-bid=#f but Cfg.info lacks 'java/original-block-count"
                  start-bid))]))
  ;; For method-end ranges (end-bid=#f) the raw ordinal window may
  ;; cover handler blocks that the JVM exception table excludes by
  ;; convention; filter them out.  For explicit end-bid the window
  ;; is trusted verbatim (nested try/catch: an outer window
  ;; legitimately contains inner handler blocks, because an inner
  ;; handler's code is still inside the outer try scope).
  (define filter-handlers? (not end-bid))
  (define handler-set (if filter-handlers?
                          (exception-table-handler-set cfg)
                          idmap-empty))
  (for/fold ([s idmap-empty])
            ([bid (in-pvector block-order)]
             [i (in-naturals)]
             #:when (and (<= start-ord i) (< i end-ord)
                         (not (and filter-handlers?
                                   (idmap-ref handler-set bid #f)))))
    (idmap-set s bid #t)))


;; Collect every handler-bid appearing in the exception table as an
;; idmap (used as a set) for O(log n) membership tests.  Used
;; by compute-kappa-window-bids to filter handler blocks out of
;; method-end windows.
(define (exception-table-handler-set cfg)
  (define table (cv-info cfg 'java/exception-table #f))
  (cond
    [(not table) idmap-empty]
    [else
     (for/fold ([s idmap-empty])
               ([entry (in-pvector table)])
       (idmap-set s (caddr entry) #t))]))


;; Collect distinct out-of-window successors from every block in the
;; window, canonicalising forwarding-block successors to their joiner.
;; Returns (values kind join-bid) where kind is 'terminal (0 exits) or
;; 'convergent (exactly 1 canonical exit).  More than one canonical
;; exit is an error — normalize-try-exits should have collapsed them.
(define (classify-kappa-window cfg window-bids)
  (define direct-exits
    (for/fold ([s idmap-empty])
              ([kv (in-idmap window-bids)])
      (define t (CfgBlock-terminator (cv-block cfg (car kv))))
      (define succs
        (match t
          [(Term:jump n) (list n)]
          [(Term:cond _ tb eb) (list tb eb)]
          [(Term:switch _ cases default)
           (cons default
                 (for/list ([kv (in-pvector cases)]) (cdr kv)))]
          [_ '()]))
      (for/fold ([s s]) ([n (in-list succs)]
                         #:unless (idmap-ref window-bids n #f))
        (idmap-set s n #t))))
  (define canonical-exits
    (for/fold ([s idmap-empty])
              ([kv (in-idmap direct-exits)])
      (define bid (car kv))
      (cond
        [(forwarding-block? (cv-block cfg bid))
         (idmap-set s (Term:jump-target
                             (CfgBlock-terminator (cv-block cfg bid)))
                          #t)]
        [else (idmap-set s bid #t)])))
  (cond
    [(= 0 (idmap-count canonical-exits)) (values 'terminal #f)]
    [(= 1 (idmap-count canonical-exits))
     (values 'convergent
             (for/or ([kv (in-idmap canonical-exits)]) (car kv)))]
    [else
     (error 'classify-kappa-window
            "try window has ~a distinct canonical exits (post-normalize ≤ 1 expected): ~s"
            (idmap-count canonical-exits)
            (for/list ([kv (in-idmap canonical-exits)]) (car kv)))]))


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
  (for/fold ([s window-bids]) ([kv (in-idmap window-bids)])
    (define t (CfgBlock-terminator (cv-block cfg (car kv))))
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
        [(idmap-ref s n #f) s]
        [(and join-bid (equal? n join-bid)) s]
        [(forwarding-block? (cv-block cfg n))
         (idmap-set s n #t)]
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
             [s idmap-empty])
    (cond
      [(null? work) s]
      [else
       (define b (car work))
       (define rest (cdr work))
       (cond
         [(and stop-bid (equal? b stop-bid)) (loop rest s)]
         [(idmap-ref s b #f) (loop rest s)]
         [else
          (define s* (idmap-set s b #t))
          (define succs
            (match (CfgBlock-terminator (cv-block cfg b))
              [(Term:jump n) (list n)]
              [(Term:cond _ tb eb) (list tb eb)]
              [(Term:switch _ cases default)
               (cons default
                     (for/list ([kv (in-pvector cases)]) (cdr kv)))]
              [_ '()]))
          (loop (append succs rest) s*)])])))


;; For a convergent Kappa arm, find every block in the arm's reach
;; that ends in a direct `Term:jump join-bid`.  Each such block is a
;; phi-source predecessor for this arm.  Returns a list of BlockIds
;; (possibly empty when the arm terminates via ret / throw rather
;; than reaching join).  Multi-source: when an inner diamond inside
;; the arm reunifies at the Kappa's join-bid, both inner-end blocks
;; appear here — callers consult sub-var->out2 for the phi-output
;; VarId (already materialised by the inner Gamma) rather than
;; picking one pred.
(define (find-kappa-arm-preds cfg arm-reach join-bid)
  (for/fold ([acc '()])
            ([kv (in-idmap arm-reach)])
    (define t (CfgBlock-terminator (cv-block cfg (car kv))))
    (cond
      [(and (Term:jump? t) (equal? (Term:jump-target t) join-bid))
       (cons (car kv) acc)]
      [else acc])))


;; Build the try sub-region.  Walks translate-segment scoped to try-
;; bids with stop-bid=join-bid (or #f when kind is 'terminal); delegates
;; to finish-kappa-sub-region for sink vs region-result installation.
(define (build-kappa-try-region cfg start-bid try-bids ctx-vars
                                kind join-bid join-phis)
  (define n-ctx (pvector-length ctx-vars))
  (define sub0 (region-empty))
  (define-values (sub1 arg-outs) (region-add-region-arg sub0 n-ctx))
  (define sub-var->out
    (for/fold ([m idmap-empty])
              ([v (in-pvector ctx-vars)]
               [oid (in-pvector arg-outs)])
      (idmap-set m v oid)))
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
    (for/fold ([m idmap-empty])
              ([v (in-pvector ctx-vars)]
               [oid (in-pvector ctx-outs)])
      (idmap-set m v oid)))
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
      (define src-oid (idmap-ref m src #f))
      (cond [src-oid (idmap-set m out src-oid)]
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
  (define blk (cv-block cfg handler-bid))
  (cond
    [(not blk) (pvector-empty)]
    [else
     (for/pvector ([phi (in-pvector (CfgBlock-phis blk))])
       (cons (PhiInsn-output phi)
             (pick-phi-source phi start-bid
                              (list 'kappa-handler-entry handler-bid))))]))


;; Pop the currently-active (outermost remaining) Kappa-Group from
;; `start-bid`'s stack while building its own sub-regions, so
;; translate-segment landing back on `start-bid` inside the walk does
;; not re-dispatch the same group infinitely.  Nested groups at the
;; same start-bid (multi-range case) remain visible in tail position;
;; sibling trys at other start-bids are untouched.  `gs` may be #f (no
;; kappa-groups threading).
(define (kappa-groups-without gs start-bid)
  (cond
    [(not gs) gs]
    [else
     (define stack (idmap-ref gs start-bid #f))
     (cond
       [(not stack) gs]
       [(<= (pvector-length stack) 1)
        (idmap-remove gs start-bid)]
       [else
        (idmap-set gs start-bid (pvector-drop stack 1))])]))


;; Wrapper that threads `cfg` into the finisher so find-kappa-arm-preds
;; can locate the Term:jump predecessor(s) when the arm converges.
;; Single-source (one direct jump to join) wires each region-result
;; via pick-phi-source on that pred.  Multi-source (an inner diamond
;; inside the arm reunifies at join) reads each phi's output VarId
;; directly from sub-var->out2 — translate-gamma on the inner diamond
;; has already materialised a Gamma output bound to that VarId when
;; its find-branch-join landed on the outer join-bid.
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
     (define preds (find-kappa-arm-preds cfg arm-reach join-bid))
     (define multi? (> (length preds) 1))
     (when (null? preds)
       (error 'finish-kappa-sub-region
              "~a arm stopped at ~a but no block jumps to it" which join-bid))
     (define (phi-source-oid phi)
       (cond
         [multi?
          (or (idmap-ref sub-var->out2 (PhiInsn-output phi) #f)
              (error 'finish-kappa-sub-region
                     "~a arm has multi-source merge at ~a but phi-output ~a not resolved (inner merge did not materialise)"
                     which join-bid (PhiInsn-output phi)))]
         [else
          (define arm-pred (car preds))
          (define src-var (pick-phi-source phi arm-pred which))
          (or (idmap-ref sub-var->out2 src-var #f)
              (error 'finish-kappa-sub-region
                     "phi source ~a undefined in ~a sub-region" src-var which))]))
     (define-values (sub3 res-ins)
       (region-add-region-result sub2 (pvector-length join-phis)))
     (for/fold ([r sub3])
               ([phi (in-pvector join-phis)]
                [iid (in-pvector res-ins)])
       (define-values (r* _w) (region-add-wire r (phi-source-oid phi) iid))
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
       (CfgBlock-phis (cv-block cfg join-bid))]
      [else (pvector-empty)]))
  (define n-phis (pvector-length join-phis))

  ;; Per-arm pred-bid lists — every block in the arm's reach that
  ;; directly Term:jumps to join-bid.  Lists (not singletons) so multi
  ;; -source merges (inner diamond reunifying at the Kappa join) get
  ;; all contributing preds considered for ctx collection.  Builders
  ;; re-derive these internally when they finish the sub-region.
  (define (arm-preds-or-empty reach)
    (cond [(eq? kind 'convergent) (find-kappa-arm-preds cfg reach join-bid)]
          [else '()]))
  (define try-arm-preds (arm-preds-or-empty try-reach))
  (define handler-arm-preds-list
    (for/list ([reach (in-list handler-reaches)])
      (arm-preds-or-empty reach)))

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
      (for/fold ([s s]) ([kv (in-idmap hctx)])
        (idmap-set s (car kv) #t))))
  (define (add-phi-srcs-for-preds arm-preds acc)
    (for/fold ([a acc]) ([ap (in-list arm-preds)])
      (for/fold ([a a]) ([phi (in-pvector join-phis)])
        (define src (pick-phi-source phi ap 'kappa-ctx))
        (cond
          [(and (VarId? src) (idmap-ref var->out src #f))
           (idmap-set a src #t)]
          [else a]))))
  (define ctx-set-with-join
    (cond
      [(eq? kind 'convergent)
       (for/fold ([s (add-phi-srcs-for-preds try-arm-preds ctx-set-base)])
                 ([preds (in-list handler-arm-preds-list)])
         (add-phi-srcs-for-preds preds s))]
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
          [(and (VarId? src) (idmap-ref var->out src #f))
           (idmap-set s src #t)]
          [else s]))))
  (define ctx-vars
    (for/pvector ([kv (in-idmap ctx-set)]) (car kv)))

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
      (or (idmap-ref var->out v #f)
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
         (idmap-set m (PhiInsn-output phi) oid)))
     (values region* var->out* join-bid)]))
