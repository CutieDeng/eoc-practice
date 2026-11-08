#lang racket/base

;; ============================================================
;; opt: pass_ch (gcc/tree-ssa-loop-ch.cc), loop header copying, with the
;; incremental SSA update it requests (tree-into-ssa.cc update_ssa with
;; a name replacement table) and the value numbering of the copied
;; region (do_rpo_vn; the copied PHIs have a single argument, so they
;; are copies, and the copied condition folds)
;; ============================================================
;;
;; (ch-pass st fndecl cfg) -> (values cfg st)
;;
;; GCC 15: should_duplicate_loop_header_p classifies the header's
;; statements as loop invariant / static (constant in the first
;; iteration; the entry PHI arguments stand in for the path ranger) and
;; returns ch_win / ch_win_invariant_exit / ch_possible_zero_cost /
;; ch_possible / ch_impossible; a header is also copied to turn the
;; loop into a do-while loop; update_profile_after_ch moves the entry
;; count into the copy and, for a static exit, folds the copied
;; condition and rescales the original header's edges
;; (update_bb_profile_for_threading); only the latch edge is split when
;; the latch is not simple.  GCC 12: do-while loops are skipped, the
;; header chain is copied while should_duplicate_loop_header_p holds
;; (insn limit, no calls), the preheader and the latch edge are both
;; split, the counts are scaled as gimple_duplicate_sese_region does
;; (the copy gets entry / total, the region the rest).
;;
;; GCC >= 14 estimates every loop's iterations first (niter-bounds.rkt)
;; and, after a copy, decreases the bounds by one
;; (adjust_loop_info_after_peeling): the upper bound too when all exits
;; were copied and no other block can end the function ("Peeled all
;; exits"), only the likely bound when the copied exits take 90% of the
;; entry count ("Peeled likely exits").
;;
;; Not ported: the ";;" loop dumps of loop_optimizer_init (the harness
;; drops ";;" lines), unlooping loops that never loop (an error), inner
;; loops' finite_p in the peeling check (treated as not precise), the general value numbering of the copied
;; region (statements other than PHIs and the condition are only
;; substituted), optimize-for-size gating.
;; ============================================================

(require racket/list
         racket/string
         racket/format
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../../../driver/dominance/dom.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         (only-in "../gimplify.rkt" tree-swap-operands-p is-gimple-min-invariant)
         "../cfg.rkt"
         (only-in "../ssa.rkt" stmt-vuse stmt-vdef)
         (only-in "../print.rkt" print-expr print-cond-rhs)
         "ssa-ops.rkt"
         (only-in "sreal.rkt" rdiv max-probability prob-invert prob-minus prob-always prob-never)
         "loops.rkt"
         (only-in "ipa.rkt" estimate-num-insns)
         (only-in "mfn.rkt" stmt-use-names stmt-def-names)
         (only-in "range-info.rkt" copy-range-info)
         "update-ssa.rkt"
         (only-in "niter-bounds.rkt" estimate-all-loops)
         (only-in "loop-bounds.rkt" loop-bound lbnd-upper adjust-loop-info-after-peeling))

(provide ch-pass)

(define param-max-loop-header-insns 20)

(define (icst? t) (and (Tree? t) (eq? (tree-code t) 'integer_cst)))

;; ------------------------------------------------------------
;; the pass
;; ------------------------------------------------------------

(define (ch-pass st0 fndecl cfg0)
  (define major (gcc-major-version st0))
  (define st (gs-set-fndecl st0 fndecl))
  ;; loop_optimizer_init (LOOPS_HAVE_PREHEADERS | LOOPS_HAVE_SIMPLE_LATCHES)
  (define cfg (normalize-loops st (cfg-clear-messages cfg0)))
  (define dom (dom-of cfg))
  (define-values (loops _back) (find-loops cfg dom))
  (define stamp (cfg-next-stamp cfg))
  (define (fresh-stamp!) (begin0 stamp (set! stamp (add1 stamp))))
  (define (msg! fmt . args) (set! cfg (cfg-add-message cfg (apply format fmt args))))

  ;; ---- profile helpers ------------------------------------------------
  (define (set-count! bb n)
    (define pf (GCfg-profile cfg))
    (when (and pf n) (set! cfg (cfg-set-profile cfg (GProf (intmap-set (GProf-counts pf) bb n) (GProf-probs pf))))))
  (define (set-prob! src dest p)
    (define pf (GCfg-profile cfg))
    (when (and pf p) (set! cfg (cfg-set-profile cfg (GProf (GProf-counts pf) (hash-set (GProf-probs pf) (cons src dest) p))))))
  ;; e->count (): the source count scaled by the probability
  (define (edge-count src dest)
    (define c (prof-count cfg src)) (define p (prof-prob cfg src dest))
    (and c p (rdiv (* c p) max-probability)))
  (define (count- a b) (and a (if (and b (>= a b)) (- a b) (if b 0 a))))
  ;; profile_count::probability_in
  (define (probability-in c overall)
    (cond [(or (not c) (not overall) (zero? overall)) #f]
          [(zero? c) prob-never]
          [(< overall c) max-probability]
          [else (rdiv (* c max-probability) overall)]))
  ;; profile_probability operator/
  (define (prob-div a b)
    (cond [(zero? a) prob-never] [(>= a b) max-probability] [else (min (rdiv (* a max-probability) b) max-probability)]))
  ;; apply_scale (profile_probability num, profile_probability den)
  (define (prob-scale p num den)
    (cond [(zero? p) p] [(zero? num) num] [(= num den) p] [else (min (rdiv (* p num) den) max-probability)]))
  (define (set-edge-probability-and-rescale-others! src dest new)
    (define old (prof-prob cfg src dest))
    (when (and old new (not (= old new)))
      (define others (for/list ([e (in-pvector (cfg-succs cfg src))] #:unless (= (GEdge-dest e) dest)) (GEdge-dest e)))
      (cond
        [(= new prob-always) (for ([o (in-list others)]) (set-prob! src o prob-never))]
        [(= 1 (length others)) (set-prob! src (car others) (prob-invert new))]
        [(null? others) (void)]
        [(> (prob-invert old) 0)
         (define num (prob-invert new)) (define den (prob-invert old))
         (for ([o (in-list others)]) (set-prob! src o (prob-scale (prof-prob cfg src o) num den)))]
        [else (for ([o (in-list others)]) (set-prob! src o (min (rdiv (prob-invert new) (length others)) max-probability)))])
      (set-prob! src dest new)))
  ;; cfg.cc update_bb_profile_for_threading
  (define (update-bb-profile-for-threading! bb count0 taken)
    (define bbc (prof-count cfg bb))
    (when (and bbc count0 (not (zero? count0)))
      (define count (if (and (< bbc count0) (< bbc (rdiv (* count0 7) 8))) (rdiv bbc 2) count0))
      (when (> bbc count)
        (define taken-p (prof-prob cfg bb taken))
        (when taken-p
          (define prob0 (if (> bbc 0) (probability-in count bbc) (rdiv taken-p 2)))
          (define prob (if (> prob0 taken-p) (rdiv (* taken-p 6) 8) prob0))
          (set-edge-probability-and-rescale-others! bb taken (prob-div (prob-minus taken-p prob) (prob-invert prob)))))
      (set-count! bb (count- bbc count))))

  ;; ---- statement helpers ---------------------------------------------
  (define (block-stmts bb) (GBlock-stmts (cfg-block cfg bb)))
  (define (block-phis bb) (GBlock-phis (cfg-block cfg bb)))
  (define (set-stmts! bb v) (set! cfg (set-block cfg (block-set-stmts (cfg-block cfg bb) v))))
  (define (set-phis! bb v) (set! cfg (set-block cfg (block-set-phis (cfg-block cfg bb) v))))
  (define (set-last-stmt! bb g)
    (define v (block-stmts bb))
    (set-stmts! bb (pvector-set v (sub1 (pvector-length v)) g)))
  (define (true-edge? src dest) (edge-flag? (find-edge cfg src dest) EDGE-TRUE-VALUE))
  (define (cond-make g true?)
    (stmt-set-flag (gcond (gcond-loc g) (gcond-flags g) 'ne_expr
                          (build-int-cst st (boolean-type st) (if true? 1 0)) (build-int-cst st (boolean-type st) 0)
                          (gcond-true-label g) (gcond-false-label g))
                   'stamp (fresh-stamp!)))
  (define (constant-cond? g)
    (and (gcond? g) (icst? (gcond-lhs g)) (icst? (gcond-rhs g)) (eq? (gcond-code g) 'ne_expr)
         (integer-zerop (gcond-rhs g)) (boolean-type-p st (tree-type (gcond-lhs g)))))
  (define (icmp code a b)
    (case code
      [(lt_expr) (< a b)] [(le_expr) (<= a b)] [(gt_expr) (> a b)] [(ge_expr) (>= a b)]
      [(eq_expr) (= a b)] [(ne_expr) (not (= a b))] [else #f]))
  ;; map the SSA uses of a statement (the VUSE included)
  (define (map-uses g f)
    (define g1 (stmt-map-uses st g f))
    (define v (stmt-vuse g1))
    (if (and v (ssa-name? v)) (stmt-set-flag g1 'vuse (f v)) g1))

  ;; ---- names -----------------------------------------------------------
  (define new-names '())                 ; creation order (= version order)
  (define new->old (make-hasheqv))       ; version -> old name
  ;; create_new_def_for / duplicate_ssa_name
  (define (new-def-for! old)
    (define var (ssa-name-var old))
    (define-values (n st1) (if var (make-ssa-name st var) (make-ssa-name st (tree-type old) #:name (tree-attr old 'identifier #f))))
    ;; duplicate_ssa_name copies the range info
    (set! st (copy-range-info st1 fndecl old n))
    (set! new-names (cons n new-names))
    (hash-set! new->old (ssa-name-version n) old)
    n)

  ;; ---- loop queries ---------------------------------------------------
  ;; stmt_can_terminate_bb_p (no exceptions, no asm in the IR): a call
  ;; that is not a non-looping const / pure function and returns
  (define (stmt-can-terminate-bb? g)
    (cond
      [(gcall? g)
       (define f (call-ecf st g))
       (cond
         [(and (not (zero? (bitwise-and f (bitwise-ior ECF-PURE ECF-CONST)))) (zero? (bitwise-and f ECF-LOOPING))) #f]
         [(zero? (bitwise-and f ECF-NORETURN)) #t]
         [else #f])]
      [else #f]))
  (define (loop-exits-from-bb? l bb) (for/or ([e (in-pvector (cfg-succs cfg bb))]) (not (loop-inside? l (GEdge-dest e)))))
  (define (do-while-loop? l)
    (define latch (car (Loop-latches l)))
    (define g (bb-last-stmt cfg latch))
    (and (or (not g) (glabel? g))
         (single-pred-p cfg latch)
         (let ([pred (pvector-ref (cfg-preds cfg latch) 0)])
           (and (loop-exits-from-bb? l pred)
                (not (and (>= major 15) (constant-cond? (bb-last-stmt cfg pred))))))))
  (define (in-loop-succ l bb)
    (for/first ([e (in-pvector (cfg-succs cfg bb))] #:when (loop-inside? l (GEdge-dest e))) (GEdge-dest e)))

  ;; ---- should_duplicate_loop_header_p ---------------------------------
  ;; BITS: statement classification by SSA version of its definitions
  ;; (gimple_uid): GCC 15 1 = invariant, 2 = static, 4 = combined; GCC 12
  ;; 1 = IV, 2 = invariant.  SVALS: version -> constant of a static name.
  (define (def-site-map)
    (define m (make-hasheqv))
    (for ([bb (in-pvector (cfg-bbs cfg))])
      (for ([ph (in-pvector (block-phis bb))]) (hash-set! m (ssa-name-version (gphi-result ph)) bb))
      (for* ([g (in-pvector (block-stmts bb))] [d (in-list (stmt-def-names g))]) (hash-set! m (ssa-name-version d) bb)))
    m)
  (define (should-duplicate-15 l header defs bits svals limit-box static-exits invariant-exits)
    (define succs (cfg-succs cfg header))
    (define last (bb-last-stmt cfg header))
    (define (inv? op)
      (cond [(not (ssa-name? op)) #t]
            [(ssa-default-def? op) #t]
            [else (define d (hash-ref defs (ssa-name-version op) #f))
                  (or (not d) (not (loop-inside? l d)) (odd? (hash-ref bits (ssa-name-version op) 0)))]))
    (define (bit? op n) (and (ssa-name? op) (not (zero? (bitwise-and n (hash-ref bits (ssa-name-version op) 0))))))
    (define (static-value op)
      (cond [(icst? op) op]
            [(and (ssa-name? op) (hash-ref svals (ssa-name-version op) #f)) => values]
            [else #f]))
    (define (supports-range? t) (let ([ty (tree-type t)]) (or (integral-type-p st ty) (pointer-type-p st ty))))
    (cond
      [(= 1 (pvector-length succs)) 'impossible]
      [(and (loop-inside? l (GEdge-dest (pvector-ref succs 0))) (loop-inside? l (GEdge-dest (pvector-ref succs 1)))) 'impossible]
      [(and (not (= header (Loop-header l))) (not (single-pred-p cfg header))) 'impossible]
      [(not (gcond? last)) 'impossible]
      [else
       ;; PHIs: static when the entry argument is a constant (singleton path range)
       (define pre (for/first ([p (in-pvector (cfg-preds cfg header))] #:unless (loop-inside? l p)) p))
       (for ([ph (in-pvector (block-phis header))] #:unless (ssa-virtual? (gphi-result ph)))
         (define res (gphi-result ph))
         (define v (and pre (supports-range? res) (static-value (phi-arg-for-pred ph pre))))
         (hash-set! bits (ssa-name-version res) (if v 2 0))
         (when v (hash-set! svals (ssa-name-version res) v)))
       (define code-size-cost #f)
       (define verdict
         (let/ec return
           (for ([g (in-pvector (block-stmts header))] #:unless (glabel? g) #:break (gcond? g))
             (when (and (gcall? g) (not (gcall-internal? g))) (return 'impossible))
             (define defs-of (stmt-def-names g))
             (define (set-bits! n) (for ([d (in-list defs-of)]) (hash-set! bits (ssa-name-version d) n)))
             (set-bits! 0)
             (define classified?
               (and (not (stmt-vuse g))
                    (or (not (gcall? g)) (not (zero? (bitwise-and (call-ecf st g) ECF-CONST))))
                    (let* ([uses (stmt-real-uses g)]
                           [inv (for/and ([u (in-list uses)]) (inv? u))]
                           [static-v (and (not inv) (gassign? g) (ssa-name? (gassign-lhs g)) (supports-range? (gassign-lhs g))
                                          (static-fold g static-value))])
                      (set-bits! (bitwise-ior (if inv 1 0) (if static-v 2 0)))
                      (when static-v (for ([d (in-list defs-of)]) (hash-set! svals (ssa-name-version d) static-v)))
                      (or inv static-v))))
             (unless classified?
               (define combined?
                 (and (gassign? g) (memq (gassign-code g) '(bit_and_expr bit_ior_expr bit_xor_expr))
                      (let ([ok? (lambda (op) (or (inv? op) (bit? op 4) (bit? op 2)))])
                        (and (ok? (gassign-rhs1 g)) (ok? (gassign-rhs2 g))))))
               (cond
                 [combined? (set-bits! 4)]
                 [else
                  (define insns (estimate-num-insns st g))
                  (set-box! limit-box (- (unbox limit-box) insns))
                  (when (> insns 0) (set! code-size-cost #t))
                  (when (< (unbox limit-box) 0) (return 'impossible))])))
           #f))
       (cond
         [verdict verdict]
         [else
          (define lhs (gcond-lhs last)) (define rhs (gcond-rhs last))
          (define lhs-inv (inv? lhs)) (define rhs-inv (inv? rhs))
          (define (comb-ok? op) (or (inv? op) (bit? op 4)))
          (define exits (for/list ([e (in-pvector succs)] #:unless (loop-inside? l (GEdge-dest e))) (cons header (GEdge-dest e))))
          (cond
            [(and (not (eq? lhs-inv rhs-inv)) (comb-ok? lhs) (comb-ok? rhs)) 'win]
            [else
             ;; static_loop_exit: the condition is constant on the entry path
             ;; and takes the in-loop edge
             (define-values (te fe) (extract-true-false-edges cfg header))
             (define static-exit
               (let ([a (static-value lhs)] [b (static-value rhs)])
                 (and a b
                      (let ([val (icmp (gcond-code last) (integer-cst-value a) (integer-cst-value b))])
                        (cond
                          [(not (loop-inside? l (GEdge-dest te))) (and (not val) (cons header (GEdge-dest te)))]
                          [(not (loop-inside? l (GEdge-dest fe))) (and val (cons header (GEdge-dest fe)))]
                          [else #f])))))
             (when static-exit (set-box! static-exits (cons static-exit (unbox static-exits))))
             (cond
               [(and lhs-inv rhs-inv)
                (set-box! invariant-exits (append exits (unbox invariant-exits)))
                'win-inv]
               [static-exit (if code-size-cost 'possible 'zero-cost)]
               [else
                (set-box! limit-box (- (unbox limit-box) (estimate-num-insns st last)))
                (if (< (unbox limit-box) 0) 'impossible 'possible)])])])]))
  ;; fold an assignment whose operands are the static constants (a
  ;; stand-in for the path ranger's range_of_stmt)
  (define (static-fold g static-value)
    (define a (static-value (gassign-rhs1 g)))
    (define b (and (gassign-rhs2 g) (static-value (gassign-rhs2 g))))
    (define type (tree-type (gassign-lhs g)))
    (define r
      (cond
        [(gassign-rhs3 g) #f]
        [(and (gassign-rhs2 g) a b) (fold-build2 st (gassign-code g) type a b)]
        [(and (not (gassign-rhs2 g)) a (memq (gassign-code g) '(nop_expr convert_expr negate_expr bit_not_expr)))
         (fold-build1 st (gassign-code g) type a)]
        [(and (not (gassign-rhs2 g)) a (eq? (gassign-code g) (tree-code a))) a]
        [else #f]))
    (and r (icst? r) r))

  (define (should-duplicate-12 l header defs bits limit-box)
    (define succs (cfg-succs cfg header))
    (define last (bb-last-stmt cfg header))
    (define (in-loop-def? op)
      (and (ssa-name? op) (not (ssa-default-def? op))
           (let ([d (hash-ref defs (ssa-name-version op) #f)]) (and d (loop-inside? l d)))))
    (define (uid op) (hash-ref bits (ssa-name-version op) 0))
    (cond
      [(= 1 (pvector-length succs)) #f]
      [(and (loop-inside? l (GEdge-dest (pvector-ref succs 0))) (loop-inside? l (GEdge-dest (pvector-ref succs 1)))) #f]
      [(and (not (= header (Loop-header l))) (not (single-pred-p cfg header))) #f]
      [(not (gcond? last)) #f]
      [else
       (for ([ph (in-pvector (block-phis header))])
         (define ty (tree-type (gphi-result ph)))
         (hash-set! bits (ssa-name-version (gphi-result ph)) (if (or (integral-type-p st ty) (pointer-type-p st ty)) 1 0)))
       (define ok?
         (let/ec return
           (for ([g (in-pvector (block-stmts header))] #:unless (glabel? g))
             (when (and (gcall? g) (not (gcall-internal? g))) (return #f))
             (set-box! limit-box (- (unbox limit-box) (estimate-num-insns st g)))
             (when (< (unbox limit-box) 0) (return #f))
             (define defs-of (stmt-def-names g))
             (define n
               (cond
                 [(stmt-vuse g) 0]
                 [else
                  (define-values (inv iv)
                    (for/fold ([inv #t] [iv #f]) ([op (in-list (stmt-real-uses g))] #:when (in-loop-def? op))
                      (values (and inv (not (zero? (bitwise-and 2 (uid op))))) (or iv (odd? (uid op))))))
                  (bitwise-ior (if iv 1 0) (if inv 2 0))]))
             (for ([d (in-list defs-of)]) (hash-set! bits (ssa-name-version d) n)))
           #t))
       (and ok?
            (not (and (not (= header (Loop-header l)))
                      (let ([variant? (lambda (op) (and (in-loop-def? op) (zero? (uid op))))])
                        (or (variant? (gcond-lhs last)) (variant? (gcond-rhs last)))))))]))

  ;; ---- GCC >= 14: estimate_numbers_of_iterations of every loop with exits,
  ;; before the analysis (a loop that never loops is not ported)
  (when (>= major 14)
    (set! st (estimate-all-loops st fndecl cfg #:only (lambda (l) (pair? (loop-exits cfg l)))))
    (for ([l (in-list loops)] #:when (pair? (loop-exits cfg l)))
      (when (eqv? 0 (lbnd-upper (loop-bound st fndecl (Loop-num l))))
        (error 'ch-pass "loop ~a never loops: unlooping it is not ported" (Loop-num l)))))

  ;; ---- the candidates -------------------------------------------------
  ;; (list loop nheaders static-exits invariant-exits)
  (define defs (def-site-map))
  (define candidates
    (filter values
    (for/list ([l (in-list loops)]
               #:when (and (= 1 (length (Loop-latches l))) (pair? (loop-exits cfg l)) (loop-preheader-edge cfg l))
               #:when (or (>= major 15) (not (do-while-loop? l))))
      (define bits (make-hasheqv)) (define svals (make-hasheqv))
      (define limit (box param-max-loop-header-insns))
      (cond
        [(>= major 15)
         (define static-exits (box '())) (define invariant-exits (box '()))
         (define-values (nheaders last-win last-win-inv decisions)
           (let loop ([header (Loop-header l)] [n 0] [last-win 0] [last-win-inv #f] [decisions '()])
             (define ret (should-duplicate-15 l header defs bits svals limit static-exits invariant-exits))
             (cond
               [(eq? ret 'impossible) (values n last-win last-win-inv (reverse decisions))]
               [else
                (define n1 (add1 n))
                (define win? (memq ret '(win win-inv)))
                (loop (in-loop-succ l header) n1 (if win? n1 last-win) (if win? (eq? ret 'win-inv) last-win-inv) (cons ret decisions))])))
         (define lw1
           (cond
             [(and (> last-win 0) last-win-inv (> nheaders last-win)) (add1 last-win)]
             [(and (= last-win 0) (> nheaders 0) (not (do-while-loop? l))) 1]
             [else last-win]))
         (define lw2 (let loop ([k lw1]) (if (and (< k (length decisions)) (eq? (list-ref decisions k) 'zero-cost)) (loop (add1 k)) k)))
         (and (> lw2 0) (list l lw2 (unbox static-exits) (unbox invariant-exits)))]
        [else
         (and (should-duplicate-12 l (Loop-header l) defs bits (box param-max-loop-header-insns))
              (let ([bits (make-hasheqv)] [limit (box param-max-loop-header-insns)])
                (define n
                  (let loop ([header (Loop-header l)] [n 0])
                    (if (should-duplicate-12 l header defs bits limit) (loop (in-loop-succ l header) (add1 n)) n)))
                (and (> n 0) (list l n '() '()))))]))))

  ;; ---- copying --------------------------------------------------------
  (define (split-edge-bb-loc src dest)
    (define dest-prev (bb-prev cfg dest))
    (if (and dest-prev (find-edge cfg dest-prev dest)) src dest-prev))
  (define (phi-arg ph pred) (for/first ([a (in-pvector (gphi-args ph))] #:when (= (car a) pred)) (cdr a)))
  ;; append a PHI argument slot for a new predecessor of DEST
  (define (append-phi-args! dest value-of stamp?)
    (set-phis! dest (for/pvector ([ph (in-pvector (block-phis dest))])
                      (define stamps (or (phi-arg-stamps ph) (for/pvector ([a (in-pvector (gphi-args ph))]) #f)))
                      (gphi (gphi-loc ph) (hash-set (gphi-flags ph) 'arg-stamps (pvector-cons-right stamps (and stamp? (fresh-stamp!))))
                            (gphi-var ph) (gphi-result ph)
                            (pvector-cons-right (gphi-args ph) (value-of ph))))))
  (define (stamp-phi-arg! dest pred)
    (set-phis! dest (for/pvector ([ph (in-pvector (block-phis dest))])
                      (define k (for/first ([a (in-pvector (gphi-args ph))] [i (in-naturals)] #:when (= (car a) pred)) i))
                      (define stamps (or (phi-arg-stamps ph) (for/pvector ([a (in-pvector (gphi-args ph))]) #f)))
                      (if k (gphi (gphi-loc ph) (hash-set (gphi-flags ph) 'arg-stamps (pvector-set stamps k (fresh-stamp!))) (gphi-var ph) (gphi-result ph) (gphi-args ph)) ph))))
  (define (copy-stmt-defs! g)
    (define g1 (if (stmt-vdef g) (stmt-set-flag g 'vdef (new-def-for! (stmt-vdef g))) g))
    (cond
      [(and (gassign? g1) (ssa-name? (gassign-lhs g1)))
       (gassign (gassign-loc g1) (gassign-flags g1) (new-def-for! (gassign-lhs g1)) (gassign-code g1) (gassign-rhs1 g1) (gassign-rhs2 g1) (gassign-rhs3 g1))]
      [(and (gcall? g1) (gcall-lhs g1) (ssa-name? (gcall-lhs g1)))
       (gcall (gcall-loc g1) (gcall-flags g1) (new-def-for! (gcall-lhs g1)) (gcall-fn g1) (gcall-args g1) (gcall-fntype g1) (gcall-internal? g1) (gcall-ecf g1))]
      [else g1]))

  (define copied-regions '())   ; list of (list of copy blocks), for the value numbering
  (define changed? #f)
  (for ([cand (in-list candidates)])
    (define l (car cand)) (define nheaders (cadr cand)) (define static-exits (caddr cand)) (define invariant-exits (cadddr cand))
    (define header (Loop-header l))
    (define pre (car (loop-preheader-edge cfg l)))
    (define entry-count (for/fold ([s 0]) ([p (in-pvector (cfg-preds cfg header))] #:unless (memv p (Loop-latches l))) (+ s (or (edge-count p header) 0))))
    ;; the region: the chain of headers
    (define bbs
      (let loop ([h header] [n 0] [acc '()])
        (if (= n nheaders) (reverse acc) (loop (in-loop-succ l h) (add1 n) (cons h acc)))))
    ;; the exits of the copied headers and their count (before the copy)
    (define chain-exits (for*/list ([bb (in-list bbs)] [e (in-pvector (cfg-succs cfg bb))] #:unless (loop-inside? l (GEdge-dest e)))
                          (cons bb (GEdge-dest e))))
    (define exit-count (for/fold ([s 0]) ([e (in-list chain-exits)]) (+ s (or (edge-count (car e) (cdr e)) 0))))
    (define last-bb (last bbs))
    (define new-header0 (in-loop-succ l last-bb))
    ;; the new header must have just the latch as a predecessor inside the loop
    (define new-header
      (if (single-pred-p cfg new-header0) new-header0
          (let-values ([(nb c) (split-edge cfg last-bb new-header0)]) (set! cfg c) nb)))
    ;; gimple_duplicate_seme_region: copy_bbs
    (define copies (make-hasheqv))
    (define entry-args (for/list ([ph (in-pvector (block-phis header))]) (phi-arg ph pre)))
    (for/fold ([after (split-edge-bb-loc pre header)]) ([bb (in-list bbs)])
      (define-values (nb c) (create-bb cfg (pvector-empty) after))
      (set! cfg c)
      (hash-set! copies bb nb)
      (define phis* (for/pvector ([ph (in-pvector (block-phis bb))])
                      (gphi (gphi-loc ph) (hasheq 'arg-stamps (pvector-empty)) (gphi-var ph) (new-def-for! (gphi-result ph)) (pvector-empty))))
      (define stmts* (for/pvector ([g (in-pvector (block-stmts bb))] #:unless (glabel? g))
                       (stmt-set-flag (copy-stmt-defs! g) 'stamp (fresh-stamp!))))
      (set! cfg (set-block cfg (block-set-stmts (block-set-phis (cfg-block cfg nb) phis*) stmts*)))
      (set-count! nb (prof-count cfg bb))
      (for ([e (in-list (pvector->list (cfg-succs cfg bb)))])
        (define d (GEdge-dest e))
        (define-values (_new c) (make-edge cfg nb d (GEdge-flags e)))
        (set! cfg c)
        (set-prob! nb d (prof-prob cfg bb d))
        ;; a slot in the destination's PHIs (reserve_phi_args_for_edge), the
        ;; argument of the original edge (add_phi_args_after_copy)
        (when (cfg-block cfg d) (append-phi-args! d (lambda (ph) (cons nb (phi-arg ph bb))) #f)))
      nb)
    ;; redirect the copied edges inside the region to the copies
    (for* ([bb (in-list bbs)] [e (in-list (pvector->list (cfg-succs cfg (hash-ref copies bb))))] #:when (hash-ref copies (GEdge-dest e) #f))
      (define nb (hash-ref copies bb)) (define d (GEdge-dest e)) (define nd (hash-ref copies d))
      (set! cfg (redirect-edge-succ cfg nb d nd))
      (append-phi-args! nd (lambda (ph) (cons nb (phi-arg (for/first ([o (in-pvector (block-phis d))] #:when (ssa-name=? (hash-ref new->old (ssa-name-version (gphi-result ph))) (gphi-result o))) o) bb))) #f))
    ;; redirect the entry edge, flush_pending_stmts
    (define copy-header (hash-ref copies header))
    (set! cfg (redirect-edge-succ cfg pre header copy-header))
    (set-phis! copy-header (for/pvector ([ph (in-pvector (block-phis copy-header))] [v (in-list entry-args)])
                             (gphi (gphi-loc ph) (hash-set (gphi-flags ph) 'arg-stamps (pvector-cons-right (phi-arg-stamps ph) (fresh-stamp!)))
                                   (gphi-var ph) (gphi-result ph) (pvector-cons-right (gphi-args ph) (cons pre v)))))
    ;; add_phi_args_after_copy: the arguments are linked now
    (for* ([bb (in-list bbs)] [e (in-list (pvector->list (cfg-succs cfg (hash-ref copies bb))))])
      (when (cfg-block cfg (GEdge-dest e)) (stamp-phi-arg! (GEdge-dest e) (hash-ref copies bb))))
    ;; profile
    (when (>= major 15)
      (let loop ([bbs bbs] [entry entry-count])
        (unless (null? bbs)
          (define bb (car bbs)) (define cp (hash-ref copies bb))
          (define succs (cfg-succs cfg bb))
          (cond
            [(= 1 (pvector-length succs))
             (set-count! cp entry) (set-count! bb (count- (prof-count cfg bb) entry))
             (loop (cdr bbs) entry)]
            [else
             (define d0 (GEdge-dest (pvector-ref succs 0))) (define d1 (GEdge-dest (pvector-ref succs 1)))
             (define exit-first? (not (loop-inside? l d0)))
             (define exit-d (if exit-first? d0 d1)) (define e-d (if exit-first? d1 d0))
             (define exit-dc (hash-ref copies exit-d exit-d)) (define e-dc (hash-ref copies e-d e-d))
             (set-count! cp entry)
             (define exit-e-count (edge-count bb exit-d))
             (define was-static? (and (member (cons bb exit-d) static-exits) #t))
             (cond
               [was-static?
                (set-prob! cp e-dc prob-always) (set-prob! cp exit-dc prob-never)
                (set-last-stmt! cp (cond-make (bb-last-stmt cfg cp) (true-edge? cp e-dc)))
                (update-bb-profile-for-threading! bb entry e-d)]
               [else (set-count! bb (count- (prof-count cfg bb) entry))])
             (when (member (cons bb exit-d) invariant-exits)
               (when (and (> entry 0) (not was-static?))
                 (set-edge-probability-and-rescale-others! cp exit-dc (probability-in exit-e-count entry)))
               (set-prob! bb e-d prob-always) (set-prob! bb exit-d prob-never)
               (set-last-stmt! bb (cond-make (bb-last-stmt cfg bb) (true-edge? bb e-d))))
             (loop (cdr bbs) (edge-count cp e-dc))]))))
    ;; GCC 12 gimple_duplicate_sese_region: the region keeps (total - entry)
    ;; / total of its counts, the copy entry / total
    ;; (scale_bbs_frequencies_profile_count, profile_count::apply_scale)
    (when (< major 15)
      (define total (prof-count cfg header))
      (when (and total entry-count (> total 0))
        (define ent (min entry-count total))
        (define (apply-scale c num den) (cond [(or (not c) (zero? c)) c] [(zero? num) 0] [(= num den) c] [else (rdiv (* c num) den)]))
        (for ([bb (in-list bbs)])
          (set-count! bb (apply-scale (prof-count cfg bb) (- total ent) total))
          (set-count! (hash-ref copies bb) (apply-scale (prof-count cfg (hash-ref copies bb)) ent total)))))
    ;; simple latch (and, in GCC 12, preheader)
    (when (< major 15)
      (let-values ([(_nb c) (split-edge cfg (hash-ref copies last-bb) new-header)]) (set! cfg c)))
    (when (or (< major 15) (not (single-succ-p cfg last-bb)))
      (let-values ([(_nb c) (split-edge cfg last-bb new-header)]) (set! cfg c)))
    (set! copied-regions (cons (list (hash-ref copies header) new-header l) copied-regions))
    ;; GCC >= 14: we possibly decreased the number of iterations by 1
    (when (>= major 14)
      ;; the loop as it is after the copy (the split latch and header
      ;; blocks belong to it)
      (define-values (loops2 _b2) (find-loops cfg (dom-of cfg)))
      (define l2 (loop-of loops2 last-bb))
      (define precise
        (and l2
             (= (length chain-exits) (length (loop-exits cfg l2)))
             (for/and ([bb (in-list (intbits->list (Loop-body l2)))]
                       #:unless (for/or ([e (in-pvector (cfg-succs cfg bb))]) (not (loop-inside? l2 (GEdge-dest e)))))
               ;; an inner loop's finite_p is not modelled: not precise
               (and (eq? (loop-of loops2 bb) l2)
                    (not (for/or ([g (in-pvector (block-stmts bb))]) (stmt-can-terminate-bb? g)))))))
      (define upper (lbnd-upper (loop-bound st fndecl (Loop-num l))))
      (cond
        [(and precise (eqv? upper 1)) (error 'ch-pass "loop ~a no longer loops: unlooping it is not ported" (Loop-num l))]
        [precise (set! st (adjust-loop-info-after-peeling st fndecl (Loop-num l) 1 #t))]
        [(>= exit-count (rdiv (* entry-count 9) 10)) (set! st (adjust-loop-info-after-peeling st fndecl (Loop-num l) 1 #f))]
        [else (void)]))
    (set! changed? #t))


  ;; ---- update_ssa (TODO_update_ssa) with the name replacement table ----
  ;; update_ssa (TODO_update_ssa) with the replacement table of the copies
  (define (update-ssa!)
    (define-values (cfg1 st1)
      (update-ssa st fndecl cfg (for/list ([n (in-list (reverse new-names))]) (cons n (hash-ref new->old (ssa-name-version n))))))
    (set! cfg cfg1) (set! st st1)
    (set! stamp (max stamp (cfg-next-stamp cfg1))))

  ;; ---- do_rpo_vn over the copied region: the copied PHIs are copies,
  ;; the copied condition folds ---------------------------------------
  (define (swap-code code)
    (case code [(lt_expr) 'gt_expr] [(gt_expr) 'lt_expr] [(le_expr) 'ge_expr] [(ge_expr) 'le_expr] [else code]))
  (define (fold-cond g)
    (define a (gcond-lhs g)) (define b (gcond-rhs g))
    (cond
      [(and (icst? a) (icst? b))
       (if (constant-cond? g) g (cond-make g (icmp (gcond-code g) (integer-cst-value a) (integer-cst-value b))))]
      [(tree-swap-operands-p st a b)
       (gcond (gcond-loc g) (gcond-flags g) (swap-code (gcond-code g)) b a (gcond-true-label g) (gcond-false-label g))]
      [else g]))
  ;; do_rpo_vn (entry, exit_bbs): the blocks reachable from the copied
  ;; header without entering the loop's exit blocks or the new header
  ;; eliminate's to_remove: the copied PHIs, released last-pushed first
  (define vn-removed '())
  (define (vn-region! spec)
    (set! vn-removed '())
    (define entry (car spec)) (define new-header (cadr spec)) (define l (caddr spec))
    (define exit-bbs (cons new-header (for/list ([e (in-list (loop-exits cfg l))]) (cdr e))))
    (define region
      (let dfs ([bb entry] [seen '()])
        (cond
          [(or (memv bb seen) (memv bb exit-bbs) (= bb EXIT)) seen]
          [else (for/fold ([seen (append seen (list bb))]) ([e (in-pvector (cfg-succs cfg bb))]) (dfs (GEdge-dest e) seen))])))
    (define subst (make-hasheqv))
    (define (value t)
      (define v (and (ssa-name? t) (hash-ref subst (ssa-name-version t) #f)))
      (if v (value v) t))
    (for ([bb (in-list region)])
      (for ([ph (in-pvector (block-phis bb))] #:when (= 1 (pvector-length (gphi-args ph))))
        (hash-set! subst (ssa-name-version (gphi-result ph)) (value (cdr (pvector-ref (gphi-args ph) 0)))))
      (for ([ph (in-pvector (block-phis bb))] #:when (hash-ref subst (ssa-name-version (gphi-result ph)) #f))
        (set! vn-removed (cons (ssa-name-version (gphi-result ph)) vn-removed)))
      (set-phis! bb (for/pvector ([ph (in-pvector (block-phis bb))] #:unless (hash-ref subst (ssa-name-version (gphi-result ph)) #f)) ph))
      (set-stmts! bb (for/pvector ([g (in-pvector (block-stmts bb))])
                       (define changed #f)
                       (define g1 (map-uses g (lambda (t) (define v (value t)) (unless (eq? v t) (set! changed #t)) v)))
                       (define g2 (if (gcond? g1) (fold-cond g1) g1))
                       (if (or changed (not (eq? g2 g1))) (stmt-set-flag g2 'stamp (fresh-stamp!)) g)))
      (for ([e (in-list (pvector->list (cfg-succs cfg bb)))])
        (define d (GEdge-dest e))
        (when (and (cfg-block cfg d) (not (pvector-empty? (block-phis d))))
          (set-phis! d (for/pvector ([ph (in-pvector (block-phis d))])
                         (define k (for/first ([a (in-pvector (gphi-args ph))] [i (in-naturals)] #:when (= (car a) bb)) i))
                         (define a (and k (cdr (pvector-ref (gphi-args ph) k))))
                         (define v (and a (value a)))
                         (cond
                           [(and v (not (eq? v a)))
                            (define stamps (or (phi-arg-stamps ph) (for/pvector ([x (in-pvector (gphi-args ph))]) #f)))
                            (gphi (gphi-loc ph) (hash-set (gphi-flags ph) 'arg-stamps (pvector-set stamps k (and (ssa-name? v) (fresh-stamp!))))
                                  (gphi-var ph) (gphi-result ph) (pvector-set (gphi-args ph) k (cons bb v)))]
                           [else ph])))))))

  (cond
    [(not changed?) (values cfg st)]
    [else
     (update-ssa!)
     (for ([region (in-list (reverse copied-regions))])
       ;; do_rpo_vn: the "vn_top" temporary takes a name number
       (set! st (let-values ([(_n s) (fresh-tmp-var-id st)]) s))
       (vn-region! region)
       (set! st (for/fold ([st st]) ([v (in-list vn-removed)]) (ssa-queue-release st v))))
     (define-values (cfg1 st1) (cleanup-tree-cfg st cfg print-cond-rhs))

     (values cfg1 st1)]))

