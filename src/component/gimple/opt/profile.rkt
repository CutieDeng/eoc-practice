#lang racket/base

;; ============================================================
;; opt: pass_profile (gcc/predict.cc tree_estimate_probability +
;; estimate_bb_frequencies), dump "profile_estimate"
;; ============================================================
;;
;; 1. loop_optimizer_init (LOOPS_NORMAL): opt/loops.rkt normalizes the
;;    CFG (preheaders, single-successor latches);
;; 2. predictions: GIMPLE_PREDICT statements / noreturn and recursive
;;    calls (tree_bb_level_predictions, paths through the post-dominator
;;    tree), the return value PHI (apply_return_prediction), loops
;;    (predict_loops: exit edges by iteration count, loop exits, iv
;;    comparisons, loop guards), per block the call heuristic and the
;;    opcode heuristics (tree_predict_by_opcode with expected values of
;;    PHIs of constants);
;; 3. combine_predictions_for_bb: even probabilities for anything but
;;    two-successor blocks, duplicate pruning, first match / Dempster-
;;    Shafer combination, every step dumped as GCC does
;;    ("Predictions for bb N", "  <name> heuristics[ of edge S->D][ (reason)]: P%");
;; 4. estimate_bb_frequencies: block frequencies by the Markov-chain
;;    propagation over loops in sreal arithmetic (opt/sreal.rkt), scaled
;;    to counts (the GCC >= 14 rounding, <= 13 truncates after adding 1/2),
;;    "cyclic probability ..." dumped.
;;
;; With loops, the iteration bounds are estimated (niter-bounds.rkt) as
;; predict_loops does with SCEV initialized.
;;
;; determine_unlikely_bbs (cold callees by the plugin's attribute list,
;; forward / backward propagation, never / always edges) runs before the
;; predictions and again at the start of estimate_bb_frequencies; an
;; unlikely block's count is the precise zero ('zero, "[count: 0]").  A
;; block with a self-recursive call has its frequency capped at 9/10
;; ("Dropping frequency of recursive call").  rebuild-frequencies is
;; pass_rebuild_frequencies (GCC >= 14).
;;
;; Not ported: fake exit edges for infinite loops, the hot attribute and
;; the cgraph frequency of analyzed callees (NODE_FREQUENCY_UNLIKELY_EXECUTED
;; when not from the cold attribute), __builtin_expect, the
;; irreducible-loop marking, counts from IPA profiles, the profile quality
;; beyond the precise zero.
;; ============================================================

(require racket/list racket/string
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../../../driver/dominance/dom.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         "../gimplify.rkt"
         "../cfg.rkt"
         "../ssa.rkt"
         "ssa-ops.rkt"
         "fold.rkt"
         "mfn.rkt"
         "sreal.rkt"
         (only-in "niter-bounds.rkt" estimate-one-loop)
         (only-in "loop-bounds.rkt" loop-bound lbnd-estimate lbnd-likely)
         "loops.rkt"
         (rename-in "niter.rkt" [simple-iv simple-iv*]))

(provide profile-estimate rebuild-frequencies)

(define (icst? t) (and (Tree? t) (eq? (tree-code t) 'integer_cst)))
(define BASE REG-BR-PROB-BASE)
(define (hitrate n) (quotient (+ (* n BASE) 50) 100))
(define PROB-ALWAYS BASE)
(define PROB-EVEN (quotient BASE 2))
(define PROB-VERY-UNLIKELY (sub1 (quotient BASE 2000)))
(define PROB-VERY-LIKELY (- BASE PROB-VERY-UNLIKELY))

;; predictor table: name hitrate first-match?
(define predictors
  (hasheq 'combined (list "combined" PROB-ALWAYS #f)
          'ds-theory (list "DS theory" PROB-ALWAYS #f)
          'first-match (list "first match" PROB-ALWAYS #f)
          'no-prediction (list "no prediction" PROB-ALWAYS #f)
          'unconditional (list "unconditional jump" PROB-ALWAYS #t)
          'loop-iterations (list "loop iterations" -1 #t)
          'loop-iterations-guessed (list "guessed loop iterations" -1 #t)
          'loop-iterations-max (list "guessed loop iterations" -1 #t)
          'continue (list "continue" (hitrate 67) #f)
          'noreturn (list "noreturn call" PROB-VERY-LIKELY #t)
          'loop-exit (list "loop exit" (hitrate 89) #t)
          'loop-exit-with-recursion (list "loop exit with recursion" (hitrate 78) #t)
          'loop-extra-exit (list "extra loop exit" (hitrate 67) #t)
          'tree-pointer (list "pointer (on trees)" (hitrate 70) #f)
          'tree-opcode-positive (list "opcode values positive (on trees)" (hitrate 59) #f)
          'tree-opcode-nonequal (list "opcode values nonequal (on trees)" (hitrate 66) #f)
          'tree-fpopcode (list "fp_opcode (on trees)" (hitrate 90) #f)
          'call (list "call" (hitrate 67) #f)
          'indir-call (list "indirect call" PROB-EVEN #f)
          'recursive-call (list "recursive call" PROB-EVEN #f)
          'tree-early-return (list "early return (on trees)" (hitrate 66) #f)
          'goto (list "goto" (hitrate 66) #f)
          'const-return (list "const return" (hitrate 65) #f)
          'negative-return (list "negative return" (hitrate 98) #f)
          'null-return (list "null return" (hitrate 71) #f)
          'loop-iv-compare-guess (list "guess loop iv compare" (hitrate 64) #f)
          'loop-iv-compare (list "loop iv compare" -1 #t)
          'loop-guard (list "loop guard" (hitrate 73) #f)
          'loop-guard-with-recursion (list "loop guard with recursion" (hitrate 85) #f)))
;; the enum order decides "best_predictor > predictor" in first match
(define predictor-order
  '(combined ds-theory first-match no-prediction unconditional loop-iterations loop-iterations-guessed
    loop-iterations-max continue noreturn loop-exit loop-exit-with-recursion loop-extra-exit tree-pointer
    tree-opcode-positive tree-opcode-nonequal tree-fpopcode call indir-call recursive-call tree-early-return
    goto const-return negative-return null-return loop-iv-compare-guess loop-iv-compare loop-guard
    loop-guard-with-recursion))
(define (pred-index p) (index-of predictor-order p))
(define (pred-name p) (car (hash-ref predictors p)))
(define (pred-hitrate p) (cadr (hash-ref predictors p)))
(define (pred-first-match? p) (caddr (hash-ref predictors p)))
;; the GIMPLE_PREDICT predictor names as the dump prints them -> our symbols
(define predict-stmt-predictors
  (hasheq 'PRED_TREE_EARLY_RETURN 'tree-early-return 'PRED_CONTINUE 'continue 'PRED_GOTO 'goto
          'PRED_NORETURN 'noreturn 'PRED_COLD_FUNCTION 'noreturn))

(struct pred (src dest predictor prob) #:transparent)

;; pass state
(struct P (st m cfg dom pdom loops back bb-preds probs never zero msgs) #:mutable)
;; bb-preds : hasheqv bb -> list of pred, newest first
;; probs    : hash (src . dest) -> m_val
;; never    : hash (src . dest) -> #t      (profile_probability::never)
;; zero     : hasheqv bb -> #t             (profile_count::zero)

(define (msg! p fmt . args) (set-P-msgs! p (cons (apply format fmt args) (P-msgs p))))
(define (dominates? d a b) (dom-dominates? d (dom-index d a) (dom-index d b)))
(define (pdominates? p a b)
  (define d (P-pdom p))
  (and (dom-reachable? d a) (dom-reachable? d b) (dom-dominates? d (dom-index d a) (dom-index d b))))
(define (succ-dests p bb) (for/list ([e (in-pvector (cfg-succs (P-cfg p) bb))]) (GEdge-dest e)))
(define (edge-true? p src dest) (not (zero? (bitwise-and (GEdge-flags (find-edge* p src dest)) EDGE-TRUE-VALUE))))
(define (find-edge* p src dest) (for/first ([e (in-pvector (cfg-succs (P-cfg p) src))] #:when (= (GEdge-dest e) dest)) e))
(define (last-stmt p bb) (bb-last-stmt (P-cfg p) bb))
(define (unlikely-edge? p src dest) (or (hash-ref (P-zero p) src #f) (hash-ref (P-never p) (cons src dest) #f)))
(define (prob-of p src dest) (hash-ref (P-probs p) (cons src dest) #f))
(define (set-prob! p src dest v) (hash-set! (P-probs p) (cons src dest) v))

;; ------------------------------------------------------------
;; recording predictions
;; ------------------------------------------------------------

;; gimple_predict_edge
(define (predict-edge! p src dest predictor prob)
  (when (and (not (= src ENTRY)) (> (pvector-length (cfg-succs (P-cfg p) src)) 1))
    (hash-update! (P-bb-preds p) src (lambda (l) (cons (pred src dest predictor prob) l)) '())))
(define (predict-edge-def! p src dest predictor taken?)
  (predict-edge! p src dest predictor (if taken? (pred-hitrate predictor) (- BASE (pred-hitrate predictor)))))
(define (edge-predicted-by? p src dest predictor taken?)
  (define prob (if taken? (pred-hitrate predictor) (- BASE (pred-hitrate predictor))))
  (for/or ([x (in-list (hash-ref (P-bb-preds p) src '()))])
    (and (eq? (pred-predictor x) predictor) (= (pred-dest x) dest) (= (pred-prob x) prob))))
(define (predicted-by? p bb predictor)
  (for/or ([x (in-list (hash-ref (P-bb-preds p) bb '()))]) (eq? (pred-predictor x) predictor)))
(define (maybe-predict-edge! p src dest predictor taken?)
  (unless (or (edge-predicted-by? p src dest predictor taken?)
              (and (eq? predictor 'loop-guard) (edge-predicted-by? p src dest 'loop-guard-with-recursion taken?)))
    (when (eq? predictor 'loop-guard-with-recursion)
      (hash-update! (P-bb-preds p) src
                    (lambda (l) (filter (lambda (x) (not (and (= (pred-dest x) dest) (eq? (pred-predictor x) 'loop-guard)))) l)) '()))
    (predict-edge-def! p src dest predictor taken?)))

;; predict_paths_for_bb
(define (predict-paths-for-bb! p cur bb predictor taken? visited in-loop)
  (define cfg (P-cfg p))
  (unless (and in-loop
               (or (not (loop-inside? in-loop cur))
                   (dominates? (P-dom p) (car (Loop-latches in-loop)) cur)))
    (for ([src (in-pvector (cfg-preds cfg cur))])
      (when (and (>= src 2) (not (pdominates? p bb src)))
        (unless (unlikely-edge? p src cur)
          (define found
            (for/or ([e2 (in-pvector (cfg-succs cfg src))])
              (define d (GEdge-dest e2))
              (and (not (= d cur))
                   (not (unlikely-edge? p src d))
                   (not (pdominates? p bb d))
                   (or (not in-loop) (not (and (loop-inside? in-loop src) (not (loop-inside? in-loop d))))))))
          (cond
            [found (maybe-predict-edge! p src cur predictor taken?)]
            [(not (hash-ref visited src #f))
             (hash-set! visited src #t)
             (predict-paths-for-bb! p src src predictor taken? visited in-loop)]))))
    ;; the post-dominator children of CUR
    (define pd (P-pdom p))
    (when (dom-reachable? pd cur)
      (for ([son (in-pvector (dom-children pd (dom-index pd cur)))])
        (define sb (dom-node pd son))
        (unless (or (= sb ENTRY) (= sb EXIT))
          (predict-paths-for-bb! p sb bb predictor taken? visited in-loop))))))

(define (predict-paths-leading-to! p bb predictor taken? [in-loop #f])
  (predict-paths-for-bb! p bb bb predictor taken? (make-hasheqv) in-loop))

(define (predict-paths-leading-to-edge! p src dest predictor taken? [in-loop #f])
  (define has-nonloop-edge
    (for/or ([e2 (in-pvector (cfg-succs (P-cfg p) src))])
      (define d (GEdge-dest e2))
      (and (not (= d src)) (not (= d dest)) (not (unlikely-edge? p src d)) (not (pdominates? p d src)))))
  (if has-nonloop-edge
      (maybe-predict-edge! p src dest predictor taken?)
      (predict-paths-for-bb! p src src predictor taken? (make-hasheqv) in-loop)))

;; ------------------------------------------------------------
;; block-level predictions
;; ------------------------------------------------------------

(define (call-noreturn? p g) (and (gcall? g) (not (zero? (bitwise-and (call-ecf (P-st p) g) ECF-NORETURN)))))

;; ------------------------------------------------------------
;; determine_unlikely_bbs (12.5 and 15.2 alike): blocks with a call to a
;; cold function are unlikely, the property is propagated forward (blocks
;; reachable only through unlikely edges) and backward (blocks all of
;; whose successors are unlikely, unless a statement may terminate the
;; block or is a noreturn call); edges into unlikely blocks become
;; never, the single other successor edge always.  Unlikely blocks get
;; the precise count zero.
;; ------------------------------------------------------------

(define (attr-names x) (let ([a (and x (tree-attr x 'attributes #f))]) (if (list? a) a '())))
;; unlikely_executed_stmt_p: a call whose function type or declaration is
;; cold, unless the current function is cold itself.  The cgraph frequency
;; of an analyzed callee (NODE_FREQUENCY_UNLIKELY_EXECUTED) is not modelled.
(define (unlikely-executed-stmt? p g)
  (define st (P-st p))
  (define cur-cold? (member "cold" (attr-names (GS-fndecl st))))
  (and (gcall? g) (not cur-cold?)
       (or (let ([ft (gcall-fntype g)])
             (and ft (let ([a (type-attr st ft 'attributes #f)]) (and (list? a) (member "cold" a)))))
           (let ([d (call-fndecl g)]) (and d (member "cold" (attr-names d)))))
       #t))
;; stmt_can_terminate_bb_p without exceptions or asm: a call that is not a
;; nothrow builtin, not a non-looping const / pure function, and returns
;; (a noreturn call has no successor edge here)
(define (stmt-can-terminate-bb? p g bb)
  (cond
    [(gcall? g)
     (define d (call-fndecl g))
     (define f (call-ecf (P-st p) g))
     (cond
       [(and d (call-fndecl-builtin? d) (member "nothrow" (attr-names d)) (zero? (bitwise-and f ECF-RETURNS-TWICE))) #f]
       [(and (not (zero? (bitwise-and f (bitwise-ior ECF-PURE ECF-CONST)))) (zero? (bitwise-and f ECF-LOOPING))) #f]
       [(zero? (bitwise-and f ECF-NORETURN)) #t]
       [else (pair? (succ-dests p bb))])]
    [else #f]))

(define (determine-unlikely-bbs! p)
  (define cfg (P-cfg p)) (define m (P-m p))
  (define zero (P-zero p)) (define never (P-never p))
  (define (zero? bb) (hash-ref zero bb #f))
  (define (never? s d) (hash-ref never (cons s d) #f))
  (define (stmts bb) (for/list ([id (in-list (mfn-stmts m bb))]) (mfn-stmt m id)))
  ;; unlikely_executed_bb_p
  (define (unlikely-bb? bb)
    (let loop ([gs (stmts bb)])
      (cond
        [(null? gs) #f]
        [(unlikely-executed-stmt? p (car gs)) #t]
        [(stmt-can-terminate-bb? p (car gs) bb) #f]
        [else (loop (cdr gs))])))
  (define bbs (pvector->list (cfg-bbs cfg)))
  (define all-bbs (append (list ENTRY) bbs (list EXIT)))
  ;; the locally unlikely blocks and edges
  (for ([bb (in-list bbs)])
    (when (and (not (zero? bb)) (unlikely-bb? bb)) (hash-set! zero bb #t))
    (for ([d (in-list (succ-dests p bb))])
      (when (and (not (never? bb d)) (or (zero? bb) (never? bb d))) (hash-set! never (cons bb d) #t))))
  ;; propagate_unlikely_bbs_forward: what ENTRY reaches through edges
  ;; whose count is not zero
  (define reached (make-hasheqv))
  (unless (zero? ENTRY)
    (hash-set! reached ENTRY #t)
    (let loop ([work (list ENTRY)])
      (unless (null? work)
        (define bb (car work))
        (define more
          (for/fold ([acc '()]) ([d (in-list (succ-dests p bb))])
            (cond
              [(or (zero? bb) (never? bb d) (zero? d) (hash-ref reached d #f)) acc]
              [else (hash-set! reached d #t) (cons d acc)])))
        (loop (append (reverse more) (cdr work))))))
  (for ([bb (in-list all-bbs)] #:unless (hash-ref reached bb #f)) (hash-set! zero bb #t))
  ;; backward: a block whose successors are all unlikely
  (define nsuccs (make-hasheqv))
  (define worklist '())
  (for ([bb (in-list all-bbs)] #:unless (or (zero? bb) (= bb EXIT)))
    (define n (for/sum ([d (in-list (succ-dests p bb))]) (if (or (never? bb d) (zero? d)) 0 1)))
    (hash-set! nsuccs bb n)
    (when (= n 0) (set! worklist (cons bb worklist))))
  (let loop ()
    (unless (null? worklist)
      (define bb (car worklist)) (set! worklist (cdr worklist))
      (unless (or (zero? bb)
                  (and (not (= bb ENTRY))
                       (for/or ([g (in-list (stmts bb))])
                         (or (stmt-can-terminate-bb? p g bb)
                             (and (gcall? g) (not (zero? (bitwise-and (call-ecf (P-st p) g) ECF-NORETURN))))))))
        (hash-set! zero bb #t)
        (for ([s (in-pvector (cfg-preds cfg bb))] #:unless (never? s bb))
          (unless (zero? s)
            (hash-update! nsuccs s sub1 0)
            (when (= 0 (hash-ref nsuccs s)) (set! worklist (cons s worklist))))))
      (loop)))
  ;; edges into unlikely blocks are never; a single remaining edge always
  (for ([bb (in-list all-bbs)])
    (unless (zero? bb)
      (for ([d (in-list (succ-dests p bb))])
        (when (and (not (never? bb d)) (zero? d)) (hash-set! never (cons bb d) #t))))
    (define others (for/list ([d (in-list (succ-dests p bb))] #:unless (never? bb d)) d))
    (when (and (= 1 (length others)) (not (eqv? (prob-of p bb (car others)) prob-always)))
      (set-prob! p bb (car others) prob-always))))

(define (tree-bb-level-predictions! p)
  (define st (P-st p)) (define cfg (P-cfg p)) (define m (P-m p))
  (define has-return-edges
    (for/or ([src (in-pvector (cfg-preds cfg EXIT))]) (not (unlikely-edge? p src EXIT))))
  (apply-return-prediction! p)
  (for* ([bb (in-pvector (cfg-bbs cfg))] [id (in-list (mfn-stmts m bb))])
    (define g (mfn-stmt m id))
    (cond
      [(gcall? g)
       (when (and (call-noreturn? p g) has-return-edges) (predict-paths-leading-to! p bb 'noreturn #f))
       (define d (call-fndecl g))
       (when (and d (decl-eq? d (GS-fndecl st))) (predict-paths-leading-to! p bb 'recursive-call #f))]
      [(gpredict? g)
       (define predictor (hash-ref predict-stmt-predictors (gpredict-predictor g) #f))
       (when predictor
         (predict-paths-leading-to! p bb predictor (eq? (gpredict-outcome g) 'TAKEN)))]
      [else (void)])))

;; return_prediction: (values predictor taken?) or (values #f #f)
(define (return-prediction p val)
  (define st (P-st p))
  (cond
    [(not val) (values #f #f)]
    [(pointer-type-p st (tree-type val))
     (if (integer-zerop val) (values 'null-return #f) (values #f #f))]
    [(integral-type-p st (tree-type val))
     (cond
       [(and (icst? val) (< (integer-cst-value val) 0)) (values 'negative-return #f)]
       [(and (tree-constant val) (not (integer-zerop val)) (not (integer-onep val))) (values 'const-return #f)]
       [else (values #f #f)])]
    [else (values #f #f)]))

(define (apply-return-prediction! p)
  (define cfg (P-cfg p)) (define m (P-m p)) (define st (P-st p))
  (define ret
    (for/first ([src (in-pvector (cfg-preds cfg EXIT))]
                #:when (let ([g (last-stmt p src)]) (and g (greturn? g))))
      (last-stmt p src)))
  (when ret
    (define rv (greturn-retval ret))
    (when (and rv (ssa-name? rv))
      (define dg (mfn-def-stmt m rv))
      (when (and dg (gphi? dg))
        (define args (map cdr (pvector->list (gphi-args dg))))
        (define-values (p0 d0) (return-prediction p (car args)))
        (define same?
          (for/and ([a (in-list (cdr args))])
            (let-values ([(px dx) (return-prediction p a)]) (eq? px p0))))
        ;; zero_one_minusone: a signed integer PHI of only 0 / 1 / -1 is a predicate, not predicted
        (define skip?
          (and (integral-type-p st (tree-type rv)) (not (type-unsigned?* st (tree-type rv)))
               (> (type-precision* st (tree-type rv)) 1)
               (let ([vals (for/list ([a (in-list args)]) (and (icst? a) (integer-cst-value a)))])
                 (and (andmap values vals) (andmap (lambda (v) (memv v '(0 1 -1))) vals)
                      (memv 1 vals) (memv -1 vals)))))
        (unless (or skip? same?)
          (for ([a (in-pvector (gphi-args dg))])
            (define-values (px dx) (return-prediction p (cdr a)))
            (when px (predict-paths-leading-to-edge! p (car a) (mfn-bb m (mfn-def m rv)) px dx))))))))

;; ------------------------------------------------------------
;; induction variables and iteration counts (the affine subset)
;; ------------------------------------------------------------

;; simple_iv through opt/niter.rkt
(define (simple-iv p l t) (simple-iv* (P-st p) (P-m p) (P-cfg p) l t))
(define (number-of-iterations p l src dest) (loop-exit-niter (P-st p) (P-m p) (P-cfg p) l src dest))
(define (swap-code c) (swap-tree-comparison* c))

;; loop_niter_by_eval: brute force over chains of constants
(define (loop-niter-by-eval p l src dest)
  (define st (P-st p)) (define m (P-m p)) (define cfg (P-cfg p))
  (define g (last-stmt p src))
  (define pre (loop-preheader-edge cfg l))
  (define latch (car (Loop-latches l)))
  (define (chain-start x)
    (define id (mfn-def m x))
    (define dg (and id (mfn-stmt m id)))
    (cond
      [(not dg) #f]
      [(not (loop-inside? l (mfn-bb m id))) #f]
      [(gphi? dg) (and (= (mfn-bb m id) (Loop-header l)) dg)]
      [(not (gassign? dg)) #f]
      [(eq? (gimple-rhs-class st (gassign-code dg)) 'ternary) #f]
      [(or (stmt-vuse dg) (eq? (tree-code-class st (gassign-code dg)) 'tcc_reference)
           (and (eq? (gassign-code dg) 'addr_expr) (not (is-gimple-min-invariant st (gassign-rhs1 dg))))) #f]
      [else (define uses (stmt-real-uses dg))
            (and (= 1 (length uses)) (chain-start (car uses)))]))
  (define (base-for x)
    (cond
      [(is-gimple-min-invariant st x) #f]
      [else
       (define ph (chain-start x))
       (and ph pre
            (let ([init (cdr (for/first ([a (in-pvector (gphi-args ph))] #:when (= (car a) (car pre))) a))]
                  [next (cdr (for/first ([a (in-pvector (gphi-args ph))] #:when (= (car a) latch)) a))])
              (and (is-gimple-min-invariant st init)
                   (or (not (ssa-name? next)) (eq? (chain-start next) ph))
                   ph)))]))
  (define (val-for x base)
    (cond
      [(not x) base]
      [(is-gimple-min-invariant st x) x]
      [else
       (define dg (mfn-def-stmt m x))
       (cond
         [(gphi? dg) base]
         [(eq? (gassign-code dg) 'ssa_name) (val-for (gassign-rhs1 dg) base)]
         [(and (eq? (gimple-rhs-class st (gassign-code dg)) 'unary) (ssa-name? (gassign-rhs1 dg)))
          (or (fold-const-unop st (gassign-code dg) (tree-type (gassign-lhs dg)) (val-for (gassign-rhs1 dg) base)) 'unknown)]
         [(eq? (gimple-rhs-class st (gassign-code dg)) 'binary)
          (define r1 (gassign-rhs1 dg)) (define r2 (gassign-rhs2 dg))
          (define a (if (ssa-name? r1) (val-for r1 base) r1))
          (define b (if (and (not (ssa-name? r1)) (ssa-name? r2)) (val-for r2 base) r2))
          (if (or (eq? a 'unknown) (eq? b 'unknown)) 'unknown
              (or (fold-const-binop st (gassign-code dg) (tree-type (gassign-lhs dg)) a b) 'unknown))]
         [else 'unknown])]))
  (cond
    [(not (and g (gcond? g))) #f]
    [else
     (define cmp0 (gcond-code g))
     (define cmp (if (edge-true? p src dest) (invert-tree-comparison cmp0) cmp0))
     (define ops (list (gcond-lhs g) (gcond-rhs g)))
     (cond
       [(not (memq cmp '(eq_expr ne_expr gt_expr ge_expr lt_expr le_expr))) #f]
       [else
        (define setup
          (for/list ([o (in-list ops)])
            (cond
              [(is-gimple-min-invariant st o) (list #f o #f)]      ; (op val next)
              [else
               (define ph (base-for o))
               (and ph (list o
                             (cdr (for/first ([a (in-pvector (gphi-args ph))] #:when (= (car a) (car pre))) a))
                             (cdr (for/first ([a (in-pvector (gphi-args ph))] #:when (= (car a) latch)) a))))])))
        (cond
          [(not (andmap values setup)) #f]
          [else
           (let loop ([i 0] [vals (map cadr setup)])
             (cond
               [(>= i 1000) #f]
               [else
                (define avals (for/list ([s (in-list setup)] [v (in-list vals)]) (val-for (car s) v)))
                (cond
                  [(ormap (lambda (x) (eq? x 'unknown)) avals) #f]
                  [else
                   (define r (fold-const-compare st cmp (boolean-type st) (car avals) (cadr avals)))
                   (cond
                     [(and r (integer-zerop r)) i]
                     [else
                      (define nvals (for/list ([s (in-list setup)] [v (in-list vals)]) (val-for (caddr s) v)))
                      (cond
                        [(ormap (lambda (x) (or (eq? x 'unknown) (not (is-gimple-min-invariant st x)))) nvals) #f]
                        [(andmap equal? nvals vals) #f]
                        [else (loop (add1 i) nvals)])])])]))])])]))

;; ------------------------------------------------------------
;; predict_loops
;; ------------------------------------------------------------

(define (predicted-by-loop-heuristics? p bb)
  (for/or ([x (in-list (hash-ref (P-bb-preds p) bb '()))])
    (memq (pred-predictor x) '(loop-iterations-guessed loop-iterations-max loop-iterations loop-exit loop-exit-with-recursion loop-extra-exit))))

;; is_comparison_with_loop_invariant_p: (values bound code step base) or #f
(define (comparison-with-loop-invariant p l g)
  (define st (P-st p))
  (define code0 (gcond-code g))
  (cond
    [(not (memq code0 '(gt_expr ge_expr ne_expr lt_expr le_expr eq_expr))) (values #f #f #f #f)]
    [else
     (define op0 (gcond-lhs g)) (define op1 (gcond-rhs g))
     (cond
       [(not (and (or (ssa-name? op0) (icst? op0)) (or (ssa-name? op1) (icst? op1)))) (values #f #f #f #f)]
       [else
        (define-values (b0 s0) (simple-iv p l op0))
        (define-values (b1 s1) (simple-iv p l op1))
        (cond
          [(not (and b0 b1)) (values #f #f #f #f)]
          [(or (and (zero? s0) (zero? s1)) (and (not (zero? s0)) (not (zero? s1)))) (values #f #f #f #f)]
          [else
           (define-values (code bound base step)
             (if (zero? s0)
                 (values (if (memq code0 '(ne_expr eq_expr)) code0 (invert-tree-comparison code0)) b0 b1 s1)
                 (values code0 b1 b0 s0)))
           (define (base-value t)
             (cond
               [(ssa-name? t) t]
               [(icst? t) t]
               [(and (eq? (tree-code-class st (tree-code t)) 'tcc_binary) (= 2 (tree-operand-length t)))
                (define a (tree-operand t 0)) (define b (tree-operand t 1))
                (cond
                  [(and (ssa-name? a) (icst? b) (<= -4 (integer-cst-value b) 4)) a]
                  [(and (icst? a) (ssa-name? b) (<= -4 (integer-cst-value a) 4)) b]
                  [else #f])]
               [else #f]))
           (define bound* (base-value bound)) (define base* (base-value base))
           (if (and bound* base*) (values bound* code step base*) (values #f #f #f #f))])])]))

;; expr_coherent_p
(define (expr-coherent? p t1 t2)
  (define m (P-m p))
  (define (single-use-of t)
    (define g (and (ssa-name? t) (mfn-def-stmt m t)))
    (and g (gassign? g) (let ([u (stmt-real-uses g)]) (and (= 1 (length u)) (car u)))))
  (cond
    [(equal? t1 t2) #t]
    [(and (icst? t1) (icst? t2)) #t]
    [(or (icst? t1) (icst? t2)) #f]
    [else
     (define s1 (single-use-of t1)) (define s2 (single-use-of t2))
     (or (and s1 (ssa-name=? s1 t2)) (and s2 (ssa-name=? s2 t1)) (and s1 s2 (ssa-name=? s1 s2)))]))

(define (predict-iv-comparison! p l bb bound-var iv-base bound-code bound-step)
  (define st (P-st p))
  (define g (last-stmt p bb))
  (when (and g (gcond? g) (not (predicted-by-loop-heuristics? p bb)))
    (define-values (cvar ccode cstep cbase) (comparison-with-loop-invariant p l g))
    (when cvar
      (define then (for/first ([e (in-pvector (cfg-succs (P-cfg p) bb))] #:when (not (zero? (bitwise-and (GEdge-flags e) EDGE-TRUE-VALUE)))) (GEdge-dest e)))
      (cond
        [(eq? ccode 'ne_expr) (predict-edge-def! p bb then 'loop-iv-compare-guess #t)]
        [(eq? ccode 'eq_expr) (predict-edge-def! p bb then 'loop-iv-compare-guess #f)]
        [(not (expr-coherent? p iv-base cbase)) (void)]
        [(and (icst? bound-var) (icst? cvar) (icst? cbase))
         (define lb (integer-cst-value bound-var)) (define cv (integer-cst-value cvar)) (define cb (integer-cst-value cbase))
         (define loop-count0 (quotient (- lb cb) cstep))
         (define compare-count0
           (if (not (eq? (not (< cstep 0)) (and (memq ccode '(lt_expr le_expr)) #t)))
               (quotient (- lb cv) cstep)
               (quotient (- cv cb) cstep)))
         (define compare-count (max 0 (+ compare-count0 (if (memq ccode '(le_expr ge_expr)) 1 0))))
         (define loop-count (max 0 (+ loop-count0 (if (memq bound-code '(le_expr ge_expr)) 1 0))))
         (define probability
           (cond [(zero? loop-count) 0]
                 [(> compare-count loop-count) BASE]
                 [else (quotient (* compare-count BASE) loop-count)]))
         (predict-edge! p bb then 'loop-iv-compare probability)]
        [(expr-coherent? p bound-var cvar)
         (cond
           [(and (memq bound-code '(lt_expr le_expr)) (memq ccode '(lt_expr le_expr))) (predict-edge-def! p bb then 'loop-iv-compare-guess #t)]
           [(and (memq bound-code '(gt_expr ge_expr)) (memq ccode '(gt_expr ge_expr))) (predict-edge-def! p bb then 'loop-iv-compare-guess #t)]
           [(eq? bound-code 'ne_expr)
            (cond
              [(and (> bound-step 0) (memq ccode '(lt_expr le_expr))) (predict-edge-def! p bb then 'loop-iv-compare-guess #t)]
              [(and (< bound-step 0) (memq ccode '(gt_expr ge_expr))) (predict-edge-def! p bb then 'loop-iv-compare-guess #t)]
              [else (predict-edge-def! p bb then 'loop-iv-compare-guess #f)])]
           [else (predict-edge-def! p bb then 'loop-iv-compare-guess #f)])]
        [(expr-coherent? p iv-base cvar)
         (cond
           [(and (> bound-step 0) (memq ccode '(gt_expr ge_expr))) (predict-edge-def! p bb then 'loop-iv-compare-guess #t)]
           [(and (< bound-step 0) (memq ccode '(lt_expr le_expr))) (predict-edge-def! p bb then 'loop-iv-compare-guess #t)]
           [else (predict-edge-def! p bb then 'loop-iv-compare-guess #f)])]
        [else (void)]))))

;; predict_extra_loop_exits
(define (predict-extra-loop-exits! p l src dest)
  (define m (P-m p)) (define cfg (P-cfg p))
  (define g (last-stmt p src))
  (when (and g (gcond? g))
    (define rhs (gcond-rhs g)) (define lhs (gcond-lhs g))
    (when (and (icst? rhs) (or (integer-zerop rhs) (integer-onep rhs)) (ssa-name? lhs))
      (define check-one (not (eq? (not (eq? (integer-onep rhs) (eq? (gcond-code g) 'eq_expr))) (edge-true? p src dest))))
      (define dg (mfn-def-stmt m lhs))
      (when (and dg (gphi? dg))
        (for ([a (in-pvector (gphi-args dg))])
          (define val (cdr a)) (define e-src (car a))
          (when (and (icst? val) (or (integer-zerop val) (integer-onep val))
                     (not (eq? check-one (not (integer-onep val)))))
            (cond
              [(not (= 1 (pvector-length (cfg-succs cfg e-src))))
               (predict-paths-leading-to-edge! p e-src (mfn-bb m (mfn-def m lhs)) 'loop-extra-exit #f l)]
              [else
               (for ([e1 (in-pvector (cfg-preds cfg e-src))])
                 (predict-paths-leading-to-edge! p e1 e-src 'loop-extra-exit #f l))])))))))

(define (predict-loops! p)
  (define cfg (P-cfg p)) (define m (P-m p)) (define st (P-st p))
  (define loops (P-loops p))
  (define fndecl (GS-fndecl st))
  (define max-iter 100)
  (define (outer l) (and (Loop-parent l) (findf (lambda (o) (= (Loop-num o) (Loop-parent l))) loops)))
  ;; with_recursion: the loops (and their outer loops) holding a recursive call
  (define with-recursion
    (for*/fold ([s (hasheqv)]) ([bb (in-pvector (cfg-bbs cfg))] [id (in-list (mfn-stmts m bb))]
                                #:when (let ([g (mfn-stmt m id)])
                                         (and (gcall? g) (let ([d (call-fndecl g)]) (and d (decl-eq? d fndecl))))))
      (let up ([l (loop-of loops bb)] [s s]) (if (and l (not (hash-ref s (Loop-num l) #f))) (up (outer l) (hash-set s (Loop-num l) #t)) s))))
  ;; estimated_stmt_executions / likely_max_stmt_executions run
  ;; estimate_numbers_of_iterations (loop) with SCEV initialized, on the
  ;; CFG as determine_unlikely_bbs left it (the unlikely blocks' count
  ;; zero, the edges into them never; the other probabilities as they
  ;; came in)
  (define estimated (make-hasheqv))              ; loop number -> loop->bounds
  (define (estimate! l)
    (hash-ref! estimated (Loop-num l)
               (lambda ()
                 (define pf (GCfg-profile cfg))
                 (define view
                   (cfg-set-profile cfg (GProf (for/fold ([c (if pf (GProf-counts pf) intmap-empty)]) ([(bb _) (in-hash (P-zero p))]) (intmap-set c bb 'zero))
                                               (for/fold ([h (if pf (GProf-probs pf) (hash))]) ([(e _) (in-hash (P-never p))]) (hash-set h e 0)))))
                 (define-values (st1 records) (estimate-one-loop (P-st p) fndecl view (Loop-header l)))
                 (set-P-st! p st1)
                 records)))
  (define (bounds l) (loop-bound (P-st p) fndecl (Loop-num l)))
  (for ([l (in-list (loops-innermost-first loops))])
    (define recursion? (hash-ref with-recursion (Loop-num l) #f))
    (define exit-predictor (if recursion? 'loop-exit-with-recursion 'loop-exit))
    (define exits (loop-exits cfg l))
    (define n-exits (for/sum ([e (in-list exits)]) (if (unlikely-edge? p (car e) (cdr e)) 0 1)))
    (when (> n-exits 0)
      (for ([e (in-list exits)])
        (define src (car e)) (define dest (cdr e))
        (unless (or (unlikely-edge? p src dest) (predicted-by-loop-heuristics? p src))
          (predict-extra-loop-exits! p l src dest)
          (define niter (or (number-of-iterations p l src dest) (loop-niter-by-eval p l src dest)))
          (define-values (nitercst predictor)
            (cond
              [niter (values (if (< niter (sub1 max-iter)) (add1 niter) max-iter) 'loop-iterations)]
              ;; one exit: the iteration estimate of the statements, + 1
              [(and (= n-exits 1) (begin (estimate! l) (lbnd-estimate (bounds l))))
               => (lambda (est) (values (min (add1 est) max-iter) 'loop-iterations-guessed))]
              ;; a likely upper bound below the loop-exit predictor's reach
              [(and (= n-exits 1) (lbnd-likely (bounds l))
                    (< (add1 (lbnd-likely (bounds l))) (rdiv BASE (- BASE (pred-hitrate exit-predictor)))))
               (values (add1 (lbnd-likely (bounds l))) 'loop-iterations-max)]
              [else (values #f #f)]))
          (when (and nitercst (not (zero? nitercst)))
            (predict-edge! p src dest predictor (rdiv BASE nitercst)))))
      ;; the bound statement: the first condition of loop->bounds, else
      ;; the header's last statement
      (define hg (or (for/first ([r (in-list (hash-ref estimated (Loop-num l) '()))] #:when (gcond? (vector-ref r 2))) (vector-ref r 2))
                     (last-stmt p (Loop-header l))))
      (define-values (bound-var bound-code bound-step iv-base)
        (if (and hg (gcond? hg)) (comparison-with-loop-invariant p l hg) (values #f #f #f #f)))
      (for ([bb (in-list (intbits->list (Loop-body l)))])
        (unless (predicted-by? p bb 'continue)
          (unless (predicted-by-loop-heuristics? p bb)
            (define probability (max (hitrate 2) (quotient (- BASE (pred-hitrate exit-predictor)) n-exits)))
            (for ([e (in-pvector (cfg-succs cfg bb))])
              (define d (GEdge-dest e))
              (when (or (< d 2) (not (loop-inside? l d)))
                (predict-edge! p bb d exit-predictor probability))))
          (when bound-var (predict-iv-comparison! p l bb bound-var iv-base bound-code bound-step))))
      ;; loop guard of a nested loop
      (when (Loop-parent l)
        (define outer (findf (lambda (o) (= (Loop-num o) (Loop-parent l))) loops))
        (define pre (loop-preheader-edge cfg l))
        (when (and pre (not (dominates? (P-dom p) (car (Loop-latches outer)) (Loop-header l))))
          (predict-paths-leading-to-edge! p (car pre) (cdr pre) 'loop-guard #f outer))))))

;; ------------------------------------------------------------
;; per-block heuristics
;; ------------------------------------------------------------

;; expr_expected_value for the opcode heuristics: a constant or #f
(define (expected-value p t depth)
  (define m (P-m p)) (define st (P-st p))
  (cond
    [(> depth 8) #f]
    [(not (Tree? t)) #f]
    [(tree-constant t) t]
    [(not (ssa-name? t)) #f]
    [else
     (define dg (mfn-def-stmt m t))
     (cond
       [(not dg) #f]
       [(gphi? dg)
        (define vals
          (for/list ([a (in-pvector (gphi-args dg))] #:unless (and (ssa-name? (cdr a)) (ssa-name=? (cdr a) t)))
            (expected-value p (cdr a) (add1 depth))))
        (and (pair? vals) (andmap values vals) (andmap (lambda (v) (equal? v (car vals))) vals) (car vals))]
       [(gassign? dg)
        (define code (gassign-code dg))
        (case (gimple-rhs-class st code)
          [(single) (expected-value p (gassign-rhs1 dg) (add1 depth))]
          [(binary)
           (define a (if (icst? (gassign-rhs1 dg)) (gassign-rhs1 dg) (expected-value p (gassign-rhs1 dg) (add1 depth))))
           (define b (if (icst? (gassign-rhs2 dg)) (gassign-rhs2 dg) (expected-value p (gassign-rhs2 dg) (add1 depth))))
           (and a b (fold-const-binop st code (tree-type (gassign-lhs dg)) a b))]
          [(unary)
           (define a (expected-value p (gassign-rhs1 dg) (add1 depth)))
           (and a (fold-const-unop st code (tree-type (gassign-lhs dg)) a))]
          [else #f])]
       [else #f])]))

(define (tree-predict-by-opcode! p bb)
  (define st (P-st p)) (define cfg (P-cfg p))
  (define g (last-stmt p bb))
  (when (and g (gcond? g))
    (define then (for/first ([e (in-pvector (cfg-succs cfg bb))] #:when (not (zero? (bitwise-and (GEdge-flags e) EDGE-TRUE-VALUE)))) (GEdge-dest e)))
    (define op0 (gcond-lhs g)) (define op1 (gcond-rhs g)) (define cmp (gcond-code g))
    (define type (tree-type op0))
    ;; expected value of the whole comparison
    (define v0 (if (icst? op0) op0 (expected-value p op0 0)))
    (define v1 (if (icst? op1) op1 (expected-value p op1 0)))
    (define val (and v0 v1 (not (and (icst? op0) (icst? op1))) (fold-const-compare st cmp (boolean-type st) v0 v1)))
    (when (and val (icst? val))
      (predict-edge! p bb then 'unconditional (if (integer-zerop val) 0 PROB-ALWAYS)))
    (cond
      [(pointer-type-p st type)
       (cond [(eq? cmp 'eq_expr) (predict-edge-def! p bb then 'tree-pointer #f)]
             [(eq? cmp 'ne_expr) (predict-edge-def! p bb then 'tree-pointer #t)])]
      [else
       (define float? (scalar-float-type-p st type))
       (define (small-cst? t) (or (integer-zerop t) (integer-onep t) (integer-minus-onep t)
                                  (and (eq? (tree-code t) 'real_cst) (memv (real-cst-value t) '(0.0 1.0 -1.0)))))
       (case cmp
         [(eq_expr uneq_expr)
          (unless (or float? (integer-zerop op0) (integer-zerop op1)) (predict-edge-def! p bb then 'tree-opcode-nonequal #f))]
         [(ne_expr ltgt_expr)
          (unless (or float? (integer-zerop op0) (integer-zerop op1)) (predict-edge-def! p bb then 'tree-opcode-nonequal #t))]
         [(ordered_expr) (predict-edge-def! p bb then 'tree-fpopcode #t)]
         [(unordered_expr) (predict-edge-def! p bb then 'tree-fpopcode #f)]
         [(le_expr lt_expr) (when (small-cst? op1) (predict-edge-def! p bb then 'tree-opcode-positive #f))]
         [(ge_expr gt_expr) (when (small-cst? op1) (predict-edge-def! p bb then 'tree-opcode-positive #t))]
         [else (void)])])))

(define (tree-estimate-probability-bb! p bb)
  (define cfg (P-cfg p)) (define m (P-m p))
  (for ([e (in-pvector (cfg-succs cfg bb))])
    (define d (GEdge-dest e))
    (when (and (not (= d EXIT)) (not (= d bb))
               (dominates? (P-dom p) bb d) (not (pdominates? p d bb)))
      (for/first ([id (in-list (mfn-stmts m d))]
                  #:when (let ([g (mfn-stmt m id)])
                           (and (gcall? g) (stmt-has-side-effects? (P-st p) g)
                                (not (call-fndecl-builtin? (call-fndecl g))))))
        (define g (mfn-stmt m id))
        (cond
          [(call-fndecl g) (predict-edge-def! p bb d 'call #f)]
          [else (predict-edge-def! p bb d 'indir-call #t)]))))
  (tree-predict-by-opcode! p bb))

;; ------------------------------------------------------------
;; combine_predictions_for_bb
;; ------------------------------------------------------------

(define (pct x) (format "~a.~a~a%" (quotient x 100) (quotient (remainder x 100) 10) (remainder x 10)))
(define (dump-prediction! p predictor prob bb [reason ""] [edge #f])
  (msg! p "  ~a heuristics~a~a: ~a" (pred-name predictor)
        (if edge (format " of edge ~a->~a" (car edge) (cdr edge)) "") reason (pct prob)))

(define (set-even-probabilities! p bb unlikely likely)
  (define cfg (P-cfg p))
  (define succs (succ-dests p bb))
  (define-values (all nedges unlikely-count)
    (for/fold ([all prob-always] [n 0] [u 0]) ([d (in-list succs)])
      (cond
        [(prob-of p bb d) (values (prob-minus all (prob-of p bb d)) n u)]
        [(not (unlikely-edge? p bb d))
         (if (member d unlikely) (values (prob-minus all prob-very-unlikely) (add1 n) (add1 u)) (values all (add1 n) u))]
        [else (values all n u)])))
  (define-values (unlikely* ucount) (if (= unlikely-count nedges) (values '() 0) (values unlikely unlikely-count)))
  (cond
    [(= 1 (length likely))
     (define lp (car likely))
     (define prob (prob-from-reg-br-prob-base (pred-prob lp)))
     (for ([d (in-list succs)])
       (cond
         [(prob-of p bb d) (void)]
         [(not (unlikely-edge? p bb d))
          (cond
            [(= (pred-dest lp) d) (set-prob! p bb d prob)]
            [(member d unlikely*) (set-prob! p bb d prob-very-unlikely)]
            [else
             (define remainder (prob-minus (prob-invert prob) (* prob-very-unlikely ucount)))
             (define count (- nedges ucount 1))
             (set-prob! p bb d (prob-div-int remainder count))])]
         [else (set-prob! p bb d prob-never)]))]
    [else
     (define scale (- nedges ucount))
     (for ([d (in-list succs)])
       (cond
         [(prob-of p bb d) (void)]
         [(not (unlikely-edge? p bb d))
          (set-prob! p bb d (if (member d unlikely*) prob-very-unlikely (prob-div-int all scale)))]
         [else (set-prob! p bb d prob-never)]))]))

(define (combine-predictions-for-bb! p bb)
  (define cfg (P-cfg p))
  (define succs (succ-dests p bb))
  (define normal (filter (lambda (d) (not (unlikely-edge? p bb d))) succs))
  (define nedges (length normal))
  (for ([d (in-list succs)] #:when (and (unlikely-edge? p bb d) (not (prob-of p bb d)))) (set-prob! p bb d prob-never))
  (define nunknown (for/sum ([d (in-list succs)]) (if (prob-of p bb d) 0 1)))
  (define nzero (for/sum ([d (in-list succs)]) (if (eqv? (prob-of p bb d) prob-never) 1 0)))
  (msg! p "Predictions for bb ~a" bb)
  (cond
    [(not (= nedges 2))
     (define preds (hash-ref (P-bb-preds p) bb '()))
     (define unlikely (for/list ([x (in-list preds)] #:when (<= (pred-prob x) PROB-VERY-UNLIKELY)) (pred-dest x)))
     (define likely (for/list ([x (in-list preds)] #:when (>= (pred-prob x) PROB-VERY-LIKELY)) x))
     (define conflict? (for/or ([x (in-list likely)]) (member (pred-dest x) unlikely)))
     (define-values (unlikely1 likely1) (if conflict? (values '() '()) (values unlikely likely)))
     (set-even-probabilities! p bb unlikely1 likely1)
     (hash-remove! (P-bb-preds p) bb)
     (cond
       [(null? unlikely1) (msg! p "~a edges in bb ~a predicted to even probabilities" nedges bb)]
       [else
        (msg! p "~a edges in bb ~a predicted with some unlikely edges" nedges bb)
        (for ([d (in-list succs)] #:unless (unlikely-edge? p bb d))
          (dump-prediction! p 'combined (prob-to-reg-br-prob-base (prob-of p bb d)) bb "" (cons bb d)))])]
    [else
     (define first (car normal)) (define second (cadr normal))
     ;; prune_predictions_for_bb
     (define preds0 (hash-ref (P-bb-preds p) bb '()))
     (define seen (make-hash))     ; (predictor . min prob) -> pred
     (define remove (make-hasheq))
     (for ([x (in-list preds0)])
       (define key (cons (pred-predictor x) (min (pred-prob x) (- BASE (pred-prob x)))))
       (define existing (hash-ref seen key #f))
       (when existing
         (cond
           [(and (= (pred-dest x) (pred-dest existing)) (= (pred-prob x) (pred-prob existing)))
            (dump-prediction! p (pred-predictor x) (pred-prob x) bb " (single edge duplicate)" (cons bb (pred-dest x)))
            (hash-set! remove x #t)]
           [(and (not (= (pred-dest x) (pred-dest existing))) (= (pred-prob x) (pred-prob existing)) (not (= (pred-prob x) PROB-EVEN)))
            (dump-prediction! p (pred-predictor x) (pred-prob x) bb " (edge pair duplicate)" (cons bb (pred-dest existing)))
            (dump-prediction! p (pred-predictor x) (pred-prob x) bb " (edge pair duplicate)" (cons bb (pred-dest x)))
            (hash-set! remove existing #t) (hash-set! remove x #t)]))
       (hash-set! seen key x))
     (define preds (filter (lambda (x) (not (hash-ref remove x #f))) preds0))
     ;; combination
     (define-values (combined best-prob best-predictor found)
       (for/fold ([combined (quotient BASE 2)] [best-prob PROB-EVEN] [best #f] [found #f]) ([x (in-list preds)])
         (define predictor (pred-predictor x))
         (define probability (if (= (pred-dest x) first) (pred-prob x) (- BASE (pred-prob x))))
         (define-values (best-prob1 best1)
           (cond
             [(and (or (not best) (> (pred-index best) (pred-index predictor))) (pred-first-match? predictor))
              ;; the best among the same predictor's predictions
              (define-values (prob conflict?)
                (for/fold ([prob probability] [conflict? #f]) ([x2 (in-list preds)] #:unless (eq? x2 x) #:when (eq? (pred-predictor x2) predictor))
                  (define p2 (if (= (pred-dest x2) first) (pred-prob x2) (- BASE (pred-prob x2))))
                  (cond
                    [conflict? (values prob #t)]
                    [(not (eq? (< probability (quotient BASE 2)) (< p2 (quotient BASE 2)))) (values prob #t)]
                    [(or (and (>= probability (quotient BASE 2)) (> p2 probability)) (and (<= probability (quotient BASE 2)) (< p2 probability))) (values p2 #f)]
                    [else (values prob #f)])))
              (if conflict? (values best-prob best) (values prob predictor))]
             [else (values best-prob best)]))
         (define d (+ (* combined probability) (* (- BASE combined) (- BASE probability))))
         (define combined1
           (if (zero? d) (quotient BASE 2)
               (if (>= (gcc-major-version (P-st p)) 15)
                   (quotient (+ (* combined probability BASE) (quotient d 2)) d)
                   ;; GCC <= 14: double arithmetic
                   (inexact->exact (truncate (/ (+ (* (exact->inexact combined) probability BASE) (quotient d 2)) d))))))
         (values combined1 best-prob1 best1 #t)))
     (define first-match (and best-predictor #t))
     (cond
       [(not found) (dump-prediction! p 'no-prediction combined bb)]
       [(not first-match) (dump-prediction! p 'ds-theory combined bb)]
       [else (dump-prediction! p 'first-match best-prob bb)])
     (define final (if first-match best-prob combined))
     (dump-prediction! p 'combined final bb)
     (for ([x (in-list preds)])
       (dump-prediction! p (pred-predictor x) (pred-prob x) bb
                         (if (or (not first-match) (eq? best-predictor (pred-predictor x))) "" " (ignored)")
                         (cons bb (pred-dest x))))
     (hash-remove! (P-bb-preds p) bb)
     (cond
       [(= nunknown 1)
        (define missing (for/first ([d (in-list succs)] #:unless (prob-of p bb d)) d))
        (define known (for/fold ([pr prob-always]) ([d (in-list succs)] #:when (prob-of p bb d)) (prob-minus pr (prob-of p bb d))))
        (set-prob! p bb missing known)]
       [(and (zero? nunknown) (not (= nzero (length succs)))) (void)]
       [else
        (set-prob! p bb first (prob-from-reg-br-prob-base final))
        (set-prob! p bb second (prob-invert (prob-from-reg-br-prob-base final)))])]))

;; ------------------------------------------------------------
;; estimate_bb_frequencies
;; ------------------------------------------------------------

(define (fmt6 r)
  ;; %f of the double value
  (define x (sreal->exact r))
  (define scaled (round (* x 1000000)))
  (define neg? (< scaled 0))
  (define a (abs scaled))
  (format "~a~a.~a" (if neg? "-" "") (quotient a 1000000) (let ([s (number->string (remainder a 1000000))]) (string-append (make-string (- 6 (string-length s)) #\0) s))))

(define (estimate-bb-frequencies! p)
  (determine-unlikely-bbs! p)
  (define cfg (P-cfg p)) (define st (P-st p))
  (define dfs-back (dfs-back-edges cfg))
  (define all-bbs (append (list ENTRY) (pvector->list (cfg-bbs cfg)) (list EXIT)))
  (define freq (make-hasheqv))             ; bb -> sreal
  (define back-edge (make-hash))           ; (src . dest) -> #t   (loop latch edges)
  (define back-edge-prob (make-hash))      ; (src . dest) -> sreal
  (for* ([bb (in-list all-bbs)] [d (in-list (succ-dests p bb))])
    (define pr (prob-of p bb d))
    (hash-set! back-edge-prob (cons bb d) (if pr (prob->sreal pr) (make-sreal 1 -1))))
  (define max-cyclic (sreal- sreal-one (sreal/ sreal-one (make-sreal 101))))
  (define (propagate-freq! head tovisit)
    (define npred (make-hasheqv))
    (for ([bb (in-list (intbits->list tovisit))])
      (hash-set! npred bb
                 (for/sum ([src (in-pvector (cfg-preds cfg bb))])
                   (if (and (intbits-ref tovisit src) (not (hash-ref dfs-back (cons src bb) #f))) 1 0)))
      (when (and (zero? (hash-ref npred bb)) (= bb EXIT)) (hash-set! (P-zero p) EXIT #t)))
    (hash-set! freq head sreal-one)
    (let loop ([queue (list head)] [visit tovisit])
      (unless (null? queue)
        (define bb (car queue))
        (unless (= bb head)
          (define-values (cyclic frequency)
            (for/fold ([cyc sreal-zero] [fr sreal-zero]) ([src (in-pvector (cfg-preds cfg bb))])
              (cond
                [(hash-ref back-edge (cons src bb) #f) (values (sreal+ cyc (hash-ref back-edge-prob (cons src bb))) fr)]
                [(not (hash-ref dfs-back (cons src bb) #f))
                 (define pr (prob-of p src bb))
                 (values cyc (sreal+ fr (sreal* (if pr (prob->sreal pr) sreal-zero) (hash-ref freq src sreal-zero))))]
                [else (values cyc fr)])))
          (cond
            [(sreal= cyclic sreal-zero) (hash-set! freq bb frequency)]
            [else
             (define cyc1
               (cond
                 [(sreal> cyclic max-cyclic)
                  (msg! p "cyclic probability of bb ~a is ~a (capped to ~a); turning freq ~a" bb (fmt6 cyclic) (fmt6 max-cyclic) (fmt6 frequency))
                  max-cyclic]
                 [else
                  (msg! p "cyclic probability of bb ~a is ~a; turning freq ~a" bb (fmt6 cyclic) (fmt6 frequency))
                  cyclic]))
             (define f (sreal/ frequency (sreal- sreal-one cyc1)))
             (hash-set! freq bb f)
             ;; the two message halves are one line in GCC
             (set-P-msgs! p (cons (string-append (car (P-msgs p)) (format " to ~a" (fmt6 f))) (cdr (P-msgs p))))]))
        (define visit1 (intbits-clear visit bb))
        (when (find-edge* p bb head)
          (define pr (prob-of p bb head))
          (hash-set! back-edge-prob (cons bb head) (sreal* (if pr (prob->sreal pr) sreal-zero) (hash-ref freq bb))))
        (define new
          (for/fold ([acc '()]) ([d (in-list (succ-dests p bb))])
            (cond
              [(and (not (hash-ref dfs-back (cons bb d) #f)) (hash-ref npred d #f) (> (hash-ref npred d) 0))
               (hash-set! npred d (sub1 (hash-ref npred d)))
               (if (zero? (hash-ref npred d)) (append acc (list d)) acc)]
              [else acc])))
        (loop (append (cdr queue) new) visit1))))
  ;; estimate_loops: inner loops first, then the whole function
  (define loops (P-loops p))
  (define (at-level ls)
    (for ([l (in-list ls)])
      (at-level (filter (lambda (o) (eqv? (Loop-parent o) (Loop-num l))) loops))
      (hash-set! back-edge (cons (car (Loop-latches l)) (Loop-header l)) #t)
      (propagate-freq! (Loop-header l) (Loop-body l))))
  (at-level (filter (lambda (l) (not (Loop-parent l))) loops))
  (propagate-freq! ENTRY (for/fold ([s intbits-empty]) ([bb (in-list all-bbs)]) (intbits-set s bb)))
  ;; scale to counts
  (define freq-max0
    (for/fold ([mx sreal-zero]) ([bb (in-pvector (cfg-bbs cfg))])
      (define f (hash-ref freq bb sreal-zero))
      (if (sreal< mx f) f mx)))
  (define freq-max1 (sreal/ (sreal-shift sreal-one 30) freq-max0))
  (define freq-max (if (sreal< freq-max1 (make-sreal 16)) (make-sreal 16) freq-max1))
  ;; a self-recursive call caps its block's frequency at 9/10
  (define (recursive-call-in? bb)
    (and (not (memv bb (list ENTRY EXIT)))
         (for/or ([id (in-list (mfn-stmts (P-m p) bb))])
           (define g (mfn-stmt (P-m p) id))
           (and (gcall? g) (let ([d (call-fndecl g)]) (and d (decl-eq? d (GS-fndecl st))))))))
  (define counts
    (for/fold ([m intmap-empty]) ([bb (in-list all-bbs)])
      (define f0 (hash-ref freq bb sreal-zero))
      (define f
        (cond
          [(and (not (sreal< f0 sreal-one)) (recursive-call-in? bb))
           (msg! p "Dropping frequency of recursive call in bb ~a from ~a" bb (fmt6 f0))
           (sreal/ (make-sreal 9) (make-sreal 10))]
          [else f0]))
      (define n
        (if (>= (gcc-major-version st) 14)
            (sreal->nearest-int (sreal* f freq-max))
            (sreal->int (sreal+ (sreal* f freq-max) (make-sreal 1 -1)))))
      ;; a block whose count is the precise zero keeps it
      (if (hash-ref (P-zero p) bb #f) (intmap-set m bb 'zero) (intmap-set m bb n))))
  counts)

;; ------------------------------------------------------------
;; the pass
;; ------------------------------------------------------------

(define (profile-estimate st fndecl cfg0)
  (define st0 (gs-set-fndecl st fndecl))
  (define cfg (normalize-loops st0 (cfg-clear-messages cfg0)))
  (define m (gcfg->mfn cfg))
  (define nodes (pvector-cons-left (pvector-cons-left (cfg-bbs cfg) EXIT) ENTRY))
  (define succs (lambda (i) (for/pvector ([e (in-pvector (cfg-succs cfg i))]) (GEdge-dest e))))
  (define dom (compute-dom nodes ENTRY succs))
  (define pdom (compute-post-dom nodes EXIT succs))
  (define-values (loops back) (find-loops cfg dom))
  (define p (P st0 m cfg dom pdom loops back (make-hasheqv) (make-hash) (make-hash) (make-hasheqv) '()))
  (determine-unlikely-bbs! p)
  (tree-bb-level-predictions! p)
  (when (pair? loops) (predict-loops! p))
  (for ([bb (in-pvector (cfg-bbs cfg))]) (tree-estimate-probability-bb! p bb))
  (for ([bb (in-pvector (cfg-bbs cfg))]) (combine-predictions-for-bb! p bb))
  (set-prob! p ENTRY (GEdge-dest (pvector-first (cfg-succs cfg ENTRY))) prob-always)
  (define counts (estimate-bb-frequencies! p))
  (define probs (for/hash ([(k v) (in-hash (P-probs p))]) (values k v)))
  (define cfg1 (for/fold ([c (cfg-set-profile cfg (GProf counts probs))]) ([mm (in-list (reverse (P-msgs p)))]) (cfg-add-message c mm)))
  ;; the bounds predict_loops estimated (without TDF_DETAILS, only for
  ;; single-exit loops whose exit count is not a constant)
  (values cfg1 (P-st p)))

;; ------------------------------------------------------------
;; pass_rebuild_frequencies (GCC >= 14, predict.cc rebuild_frequencies):
;; every block's count against the sum of its incoming edge counts
;; (profile_count::differs_from_p: off by >= 100 and by more than 1%);
;; when one differs or a count is uninitialized, estimate_bb_frequencies
;; runs again on the existing edge probabilities
;; (loop_optimizer_init without CFG changes).  The uninitialized
;; probability check only prints: GCC never sets its flag.
;; ------------------------------------------------------------

(define (rebuild-frequencies st0 fndecl cfg0)
  (define st (gs-set-fndecl st0 fndecl))
  (define cfg (cfg-clear-messages cfg0))
  (define pf (GCfg-profile cfg))
  (define msgs '())
  (define (note! fmt . args) (set! msgs (cons (apply format fmt args) msgs)))
  (define (finish c) (for/fold ([c c]) ([l (in-list (reverse msgs))]) (cfg-add-message c l)))
  (cond
    [(not pf) (values cfg st)]
    [else
     (define all-bbs (append (list ENTRY) (pvector->list (cfg-bbs cfg)) (list EXIT)))
     (define (raw-count bb) (intmap-ref (GProf-counts pf) bb #f))
     ;; profile_count values: 'zero (precise), an integer, #f (uninitialized)
     (define (edge-count s d)
       (define c (raw-count s)) (define pr (hash-ref (GProf-probs pf) (cons s d) #f))
       (cond
         [(eq? c 'zero) 'zero]
         [(eqv? pr prob-always) c]
         [(eqv? pr 0) 'zero]
         [(not (and c pr)) #f]
         [else (quotient (+ (* c pr) (quotient prob-always 2)) prob-always)]))
     (define (c+ a b)
       (cond [(eq? a 'zero) b] [(eq? b 'zero) a] [(not (and a b)) #f] [else (+ a b)]))
     (define (val x) (if (eq? x 'zero) 0 x))
     (define (differs? a b)
       (cond
         [(not (and a b)) (not (eq? (and a #t) (and b #t)))]
         [else
          (define x (val a)) (define y (val b))
          (cond
            [(or (< (abs (- x y)) 100)) #f]
            [(zero? y) #t]
            [else (let ([ratio (quotient (+ (* x 100) (quotient y 2)) y)]) (or (< ratio 99) (> ratio 101)))])]))
     (define uninitialized-count? #f)
     (define inconsistent? #f)
     (for ([bb (in-list all-bbs)])
       (unless (raw-count bb)
         (set! uninitialized-count? #t)
         (note! "BB ~a has uninitialized count" bb))
       (unless (= bb ENTRY)
         (define sum
           (for/fold ([sum 'zero]) ([s (in-pvector (cfg-preds cfg bb))])
             (unless (hash-ref (GProf-probs pf) (cons s bb) #f)
               (note! "Edge ~a->~a has uninitialized probability" s bb))
             (c+ sum (edge-count s bb))))
         (when (differs? sum (raw-count bb))
           (note! "BB ~a has invalid sum of incomming counts" bb)
           (set! inconsistent? #t))))
     (cond
       [(and (not inconsistent?) (not uninitialized-count?))
        (note! "Profile is consistent")
        (values (finish cfg) st)]
       [else
        (define m (gcfg->mfn cfg))
        (define nodes (pvector-cons-left (pvector-cons-left (cfg-bbs cfg) EXIT) ENTRY))
        (define succs (lambda (i) (for/pvector ([e (in-pvector (cfg-succs cfg i))]) (GEdge-dest e))))
        (define dom (compute-dom nodes ENTRY succs))
        (define pdom (compute-post-dom nodes EXIT succs))
        (define-values (loops back) (find-loops cfg dom))
        (define probs (make-hash))
        (for ([(k v) (in-hash (GProf-probs pf))]) (hash-set! probs k v))
        (define never (make-hash))
        (for ([(k v) (in-hash (GProf-probs pf))] #:when (eqv? v 0)) (hash-set! never k #t))
        (define zero (make-hasheqv))
        (for ([bb (in-list all-bbs)] #:when (eq? (raw-count bb) 'zero)) (hash-set! zero bb #t))
        (define p (P st m cfg dom pdom loops back (make-hasheqv) probs never zero '()))
        (define counts (estimate-bb-frequencies! p))
        (for ([l (in-list (reverse (P-msgs p)))]) (note! "~a" l))
        (note! "Rebuilt basic block counts")
        (define new-probs (for/hash ([(k v) (in-hash (P-probs p))]) (values k v)))
        (values (finish (cfg-set-profile cfg (GProf counts new-probs))) (P-st p))])]))
