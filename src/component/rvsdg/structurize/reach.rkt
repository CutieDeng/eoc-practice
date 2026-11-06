#lang racket/base

;; ============================================================
;; CFG -> RVSDG structurizer: forward-reach and arm-shape queries over the port
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
         "ctx.rkt")

(provide terminal-block? arm-reach-set reach-set-reaches-stop? arm-leaves-all-ret-throw? reach-sets-disjoint? reach-sets-all-pairwise-disjoint? reach-set-intersection arm-advance arm-is-exit? arm-is-multi-block-exit? find-branch-join arm-last-before-join arm-blocks-set theta-body-blocks body-arm-reaches-any-latch?)


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
;; Returns an idmap used as a set of BlockId -> #t.
(define (arm-reach-set cfg start-bid)
  (let loop ([work (list start-bid)]
             [seen idmap-empty])
    (cond
      [(null? work) seen]
      [else
       (define b (car work))
       (define rest (cdr work))
       (cond
         [(idmap-ref seen b #f) (loop rest seen)]
         [(not (arm-scope-contains? b)) (loop rest seen)]
         [else
          (define seen* (idmap-set seen b #t))
          (define succs
            (match (CfgBlock-terminator (cv-block cfg b))
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
       (or (idmap-ref reach-set stop-bid #f)
           (for/or ([kv (in-idmap reach-set)])
             (match (CfgBlock-terminator (cv-block cfg (car kv)))
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
  (for/and ([kv (in-idmap reach-set)])
    (define t (CfgBlock-terminator (cv-block cfg (car kv))))
    (or (Term:jump? t) (Term:cond? t)
        (Term:ret? t) (Term:throw? t))))


(define (reach-sets-disjoint? s1 s2)
  (for/and ([kv (in-idmap s1)])
    (not (idmap-ref s2 (car kv) #f))))


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
  (for/fold ([s idmap-empty])
            ([kv (in-idmap s1)]
             #:when (idmap-ref s2 (car kv) #f))
    (idmap-set s (car kv) #t)))


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
  (define term (CfgBlock-terminator (cv-block cfg bid)))
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
  (or (terminal-block? (cv-block cfg bid))
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
    (let loop ([b then-bid] [s idmap-empty])
      (cond
        [(idmap-ref s b #f) s]
        [else
         (define s2 (idmap-set s b #t))
         (define nxt (arm-advance cfg b))
         (cond [nxt (loop nxt s2)]
               [else s2])])))
  (let loop ([b else-bid])
    (cond
      [(idmap-ref then-reach b #f) b]
      [else
       (define nxt (arm-advance cfg b))
       (unless nxt
         (error 'find-branch-join
                "branches do not converge (then=~a else=~a dead-end at ~a: ~s)"
                then-bid else-bid b (CfgBlock-terminator (cv-block cfg b))))
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
      [(idmap-ref visited bid #f) visited]
      [else
       (define v2 (idmap-set visited bid #t))
       (define term (CfgBlock-terminator (cv-block cfg bid)))
       (match term
         [(Term:jump n) (walk n v2)]
         [(Term:cond _ tb eb) (walk eb (walk tb v2))]
         [(Term:ret _) v2]
         [(Term:throw _) v2]
         [_ (error 'arm-blocks-set
                   "unsupported arm-internal terminator ~s at ~a"
                   term bid)])]))
  (walk branch-bid idmap-empty))


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
  (let loop ([work (list start)] [s idmap-empty])
    (cond
      [(null? work) s]
      [else
       (define b (car work))
       (define rest (cdr work))
       (cond
         [(equal? b header) (loop rest s)]
         [(idmap-ref s b #f) (loop rest s)]
         [else
          (define s* (idmap-set s b #t))
          (cond
            [(idmap-ref latches b #f)
             ;; Latch included; its only outgoing edge goes to
             ;; header, which is the back-edge we refuse to cross.
             (loop rest s*)]
            [else
             (define succs
               (match (CfgBlock-terminator (cv-block cfg b))
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
  (for/or ([kv (in-idmap latches)])
    (and (idmap-ref reach (car kv) #f) #t)))
