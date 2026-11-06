#lang racket/base

;; ============================================================
;; CFG -> RVSDG structurizer: shared context: Theta-Ctx, Kappa-Group, the dynamic parameters and the translate-segment trampoline
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
         "../../../kernel/data/data.rkt")

(provide (struct-out Theta-Ctx) (struct-out Kappa-Group) current-arm-scope arm-scope-contains? current-shared-set current-theta-ctxs current-kappa-groups current-exn-out region-info-set current-translate-segment translate-segment)

;; The recursion back into the segment translator (structurize.rkt) is
;; injected through this parameter, so the per-construct modules do not
;; require the main module.
(define current-translate-segment (make-parameter #f))
(define (translate-segment . args) (apply (current-translate-segment) args))


;; ============================================================
;; Loop context (single-loop scope)
;; ============================================================
;;
;; Threaded through translate-segment so that the first entry into a
;; loop header spawns a Theta.  The body from the header's body-arm
;; to any latch may mix Term:jump steps with inner Gamma diamonds
;; (each must converge before the latch).  Each latch's back-edge
;; closes to the header.  `latches` is an idmap BlockId -> #t;
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
;;   try-bids     : idmap[BlockId -> #t] — blocks inside the
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
  (or (not scope) (idmap-ref scope bid #f)))


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


;; Map of every try-entry BlockId in the CFG to a pvector of Kappa-
;; Groups that all share that start-bid, sorted OUTER-FIRST (descending
;; end-bid).  Set once per `cfg->rvsdg` invocation from
;; `'java/exception-table`; consulted by `translate-segment` so any
;; try-entry landed on during recursion dispatches into
;; translate-kappa-group in whichever region we currently inhabit
;; (parent, Gamma arm, Theta body, ...).  Multiple entries per start-
;; bid arise from "same start, different end" exception-table entries
;; (nested try/catch at the same bytecode position or finally-style
;; range splits): the outer Kappa is materialised first and its try-
;; region recursively encounters start-bid with the outer popped,
;; dispatching into the inner group.  #f (default) means no try/catch
;; in the method.
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
               [info (hash-set (Region-info r) key val)]))
