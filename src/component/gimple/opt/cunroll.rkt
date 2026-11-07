#lang racket/base

;; ============================================================
;; opt: pass_complete_unroll (gcc/tree-ssa-loop-ivcanon.cc
;; tree_unroll_loops_completely) with the loop body duplication of
;; gcc/cfgloopmanip.cc duplicate_loop_body_to_header_edge and
;; gcc/tree-ssa-loop-manip.cc gimple_duplicate_loop_body_to_header_edge
;; ============================================================
;;
;; (cunroll-pass st fndecl cfg) -> (values cfg st)
;;
;; At -O1 flag_cunroll_grow_size is off, so every loop is tried with
;; UL_NO_GROWTH: a loop with a constant iteration count N (<= 16, the
;; max-completely-peel-times limit) whose estimated size after unrolling
;; does not exceed its size (tree_estimate_loop_size /
;; estimated_unrolled_size, the GCC 12 and 15 formulas) is peeled N
;; times along its preheader edge: the copies' exits are removed
;; (remove_path: the edge is split and the new block deleted), the
;; latch edge of the remaining iteration is cancelled (its condition
;; folded, the latch edge redirected to an unreachable block by
;; unloop), then update_ssa renames the copies and cleanup_tree_cfg
;; merges the straight-line code.  The profile follows GCC:
;; scale_loop_profile to the iteration bound (GCC >= 13),
;; DLTHE_FLAG_UPDATE_FREQ | DLTHE_FLAG_COMPLETTE_PEEL scaling of the
;; copies, force_edge_cold of the removed exits and of the cancelled
;; edge.
;;
;; Not ported: peeling (UL_ALL is never used at -O1), the bounds inferred
;; from undefined overflow (only the exit bounds are recorded), loop
;; nests (the father's do_rpo_vn), LCSSA PHIs on the exit (exit blocks
;; with PHIs are refused), irreducible regions.
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../../../driver/dominance/dom.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         "../cfg.rkt"
         (only-in "../ssa.rkt" stmt-vuse stmt-vdef)
         (only-in "../print.rkt" print-cond-rhs)
         (only-in "../gimplify.rkt" is-gimple-min-invariant)
         "ssa-ops.rkt"
         (only-in "sreal.rkt" rdiv max-probability prob-invert sreal/ sreal<= make-sreal)
         (only-in "ipa.rkt" estimate-num-insns)
         (only-in "mfn.rkt" stmt-def-names)
         (only-in "range-info.rkt" copy-range-info)
         "generic-fold.rkt"
         "scev.rkt"
         "update-ssa.rkt")

(provide cunroll-pass)

(define param-max-completely-peel-times 16)
(define param-max-completely-peeled-insns 200)
(define param-max-unroll-iterations 20)

;; ------------------------------------------------------------
;; profile arithmetic (gcc/profile-count.h, qualities ignored)
;; ------------------------------------------------------------

(define prob-always max-probability)
(define prob-never 0)
(define (count-apply-prob c p)
  (cond [(not c) c] [(or (zero? c) (= p prob-always)) c] [(= p prob-never) 0] [else (rdiv (* c p) max-probability)]))
(define (count-probability-in c overall)
  (cond [(and (zero? c) (not (zero? overall))) prob-never]
        [(zero? overall) #f]
        [(< overall c) prob-always]
        [else (rdiv (* c max-probability) overall)]))
(define (prob* a b) (rdiv (* a b) max-probability))
(define (count-apply-scale c num den)
  (cond [(zero? c) c] [(zero? num) 0] [(= num den) c] [else (rdiv (* c num) den)]))
(define (prob-apply-scale p num den)
  (cond [(zero? p) p] [(= num den) p] [else (min (rdiv (* p num) den) max-probability)]))
(define (count-differs? a b)
  (cond [(< (abs (- a b)) 100) #f]
        [(zero? b) #t]
        [else (let ([ratio (rdiv (* a 100) b)]) (or (< ratio 99) (> ratio 101)))]))

;; ------------------------------------------------------------
;; the pass
;; ------------------------------------------------------------

(define (cunroll-pass st0 fndecl cfg0)
  (define st (gs-set-fndecl st0 fndecl))
  (define cfg (cfg-clear-messages cfg0))
  (let iterate ([iteration 0])
    (define-values (plan st1) (with-loops st fndecl cfg (lambda () (unroll-plan))))
    (set! st st1)
    (when (and plan (<= iteration param-max-unroll-iterations))
      (define-values (c s) (unroll! st fndecl cfg plan))
      (set! cfg c) (set! st s)
      (iterate (add1 iteration))))
  (values cfg st))

;; tree_unroll_loops_completely_1 for the innermost loops: the first loop
;; that canonicalize_loop_induction_variables (create_iv = false,
;; UL_NO_GROWTH, no brute-force evaluation) decides to unroll, as a plan
(define (unroll-plan)
  (for/or ([l (in-list (sort (ctx-loops) > #:key Loop-depth))])
    (try-plan l)))

(define (try-plan l)
  (define exit0 (single-exit l))
  (define d (and exit0 (number-of-iterations-exit l exit0)))
  (define niter0 (if d (niter-desc-niter d) 'dont-know))
  (define may-be-zero (and d (not (integer-zerop (niter-desc-may-be-zero d)))))
  (define-values (niter exit)
    (cond
      [(and (Tree? niter0) (icst? niter0)) (values niter0 exit0)]
      [else
       (define-values (n e) (if exit0 (values niter0 exit0) (find-loop-niter l)))
       (values n (and (Tree? n) (icst? n) e))]))
  ;; max_loop_iterations_int: the exit bounds (record_estimate)
  (define maxiter
    (for/fold ([m #f]) ([ex (in-list (loop-exit-edges l))])
      (define dd (number-of-iterations-exit l ex))
      (define mx (and dd (niter-desc-max dd)))
      (cond [(not mx) m] [(not m) mx] [else (min m mx)])))
  ;; try_unroll_loop_completely (UL_NO_GROWTH, allow_peel)
  (define-values (n-unroll edge-to-cancel0 exit1)
    (cond
      [(and (Tree? niter) (icst? niter) exit)
       (values (icst-val niter) (other-succ (car exit) (cdr exit)) exit)]
      [else (values #f #f #f)]))
  (define-values (n-unroll1 edge-to-cancel1)
    (if (and maxiter (>= maxiter 0) (or (not n-unroll) (< maxiter n-unroll)))
        (values maxiter #f)
        (values n-unroll edge-to-cancel0)))
  (cond
    [(not n-unroll1) #f]
    [(> n-unroll1 param-max-completely-peel-times) #f]
    [(zero? n-unroll1) #f]                                     ; non-rolling loops: not ported
    [else
     (define edge-to-cancel (or edge-to-cancel1 (loop-edge-to-cancel l)))
     (define remove-exit (and exit1 (<= n-unroll1 (icst-val niter))))
     (define-values (large? size) (estimate-loop-size l (and remove-exit exit1) edge-to-cancel))
     (cond
       [large? #f]
       [else
        (define ninsns (hash-ref size 'overall))
        (define-values (unr est-elim) (estimated-unrolled-size size n-unroll1))
        (define ok
          (if (>= (major) 13)
              (<= (- unr est-elim) (+ ninsns (if (hash-ref size 'constant-iv) 1 0)))
              (<= unr (+ ninsns (if (hash-ref size 'constant-iv) 1 0)))))
        (and ok
             (let ([cfg (ctx-cfg)])
               (hash 'header (Loop-header l) 'latch (loop-latch l) 'body (Loop-body l)
                     'exit exit1 'edge-to-cancel edge-to-cancel 'n-unroll n-unroll1
                     'niter (and (Tree? niter) (icst? niter) (icst-val niter)) 'may-be-zero may-be-zero
                     'bbs (loop-body-in-dom-order l))))])]))
(define (major) (gcc-major-version (gs)))

(define (other-succ src exit-dest)
  (define d (for/first ([e (in-pvector (cfg-succs (ctx-cfg) src))] #:unless (= (GEdge-dest e) exit-dest)) (GEdge-dest e)))
  (and d (cons src d)))

;; loop_edge_to_cancel
(define (loop-edge-to-cancel l)
  (define cfg (ctx-cfg))
  (define latch (loop-latch l))
  (cond
    [(> (pvector-length (cfg-preds cfg latch)) 1) #f]
    [else
     (for/or ([ex (in-list (loop-exit-edges l))])
       (define src (car ex))
       (define ss (cfg-succs cfg src))
       (and (= 2 (pvector-length ss))
            (let* ([other (for/first ([e (in-pvector ss)] #:unless (= (GEdge-dest e) (cdr ex))) e)])
              (and (or (edge-flag? (find-edge cfg src (GEdge-dest other)) EDGE-TRUE-VALUE)
                       (edge-flag? (find-edge cfg src (GEdge-dest other)) EDGE-FALSE-VALUE))
                   (= (GEdge-dest other) latch)
                   (not (for/or ([g (in-pvector (GBlock-stmts (cfg-block cfg latch)))]) (stmt-has-side-effects? (gs) g)))
                   (cons src latch)))))]))

;; get_loop_body_in_dom_order: the dominator tree inside the loop, the
;; son dominating the latch last (dominator sons: newest first)
(define (loop-body-in-dom-order l)
  (define cfg (ctx-cfg))
  (define dom (cfg-dom cfg))
  (define chain-pos (for/hasheqv ([b (in-pvector (cfg-bbs cfg))] [i (in-naturals)]) (values b i)))
  (define (sons bb)
    (sort (for/list ([c (in-pvector (dom-children dom (dom-index dom bb)))]) (dom-node dom c)) > #:key (lambda (b) (hash-ref chain-pos b -1))))
  (define latch (loop-latch l))
  (define (dominates? a b) (dom-dominates? dom (dom-index dom a) (dom-index dom b)))
  (let fill ([bb (Loop-header l)])
    (define kids (filter (lambda (s) (loop-inside? l s)) (sons bb)))
    (define postpone (for/last ([s (in-list kids)] #:when (dominates? s latch)) s))
    (append (list bb)
            (append* (for/list ([s (in-list kids)] #:unless (dominates? s latch)) (fill s)))
            (if postpone (fill postpone) '()))))

;; constant_after_peeling
(define (constant-after-peeling op stmt-bb l)
  (cond
    [(and (Tree? op) (memq (tree-code op) '(integer_cst real_cst))) #t]
    [(not (and (Tree? op) (eq? (tree-code op) 'ssa_name))) #f]
    [(not (eqv? (loop-num (bb-loop stmt-bb)) (loop-num l))) #f]
    [else
     (define ev (analyze-scalar-evolution l op))
     (cond
       [(or (chrec-contains-undetermined? ev) (chrec-contains-symbols? ev)) #f]
       [else #t])]))
;; chrec_contains_symbols: an SSA name or a decl in the chrec
(define (chrec-contains-symbols? c)
  (let walk ([c c])
    (cond [(not (Tree? c)) #f]
          [(memq (tree-code c) '(ssa_name var_decl parm_decl)) #t]
          [else (for/or ([i (in-range (tree-operand-length c))]) (walk (tree-operand c i)))])))

;; tree_estimate_loop_size: (values large? size-hash)
(define (estimate-loop-size l exit edge-to-cancel)
  (define cfg (ctx-cfg))
  (define dom (cfg-dom cfg))
  (define (dominated-by? a b) (dom-dominates? dom (dom-index dom b) (dom-index dom a)))
  (define body (loop-body l))
  (define size (make-hasheq))
  (for ([k '(overall eliminated not-elim last last-elim last-not-elim)]) (hash-set! size k 0))
  (hash-set! size 'constant-iv #f)
  (define large?
    (for*/or ([bb (in-list body)]
              [after-exit (in-value (and edge-to-cancel (not (= bb (car edge-to-cancel))) (dominated-by? bb (car edge-to-cancel))))]
              [g (in-pvector (GBlock-stmts (cfg-block cfg bb)))])
      (define num (estimate-num-insns (gs) g))
      (define side (stmt-has-side-effects? (gs) g))
      (define last? (eq? g (bb-last-stmt cfg bb)))
      (define elim-peeled (and (not side) exit (= bb (car exit)) last?))
      (define elim-last (and (not side) edge-to-cancel (= bb (car edge-to-cancel)) last?))
      (define likely-eliminated
        (and (not side)
             (cond
               [(and (gassign? g) (constant-after-peeling (gassign-lhs g) bb l)) #t]
               [(and (gassign? g) (ssa-name? (gassign-lhs g))
                     (constant-after-peeling (gassign-rhs1 g) bb l)
                     (or (not (eq? (gimple-rhs-class (gs) (gassign-code g)) 'binary)) (constant-after-peeling (gassign-rhs2 g) bb l))
                     (not (eq? (gimple-rhs-class (gs) (gassign-code g)) 'ternary)))
                (hash-set! size 'constant-iv #t) #t]
               [(and (gcond? g) (constant-after-peeling (gcond-lhs g) bb l) (constant-after-peeling (gcond-rhs g) bb l)
                     (not (and (is-gimple-min-invariant (gs) (gcond-lhs g)) (is-gimple-min-invariant (gs) (gcond-rhs g)))))
                #t]
               [else #f])))
      (hash-update! size 'overall (lambda (v) (+ v num)))
      (when (or likely-eliminated elim-peeled) (hash-update! size 'eliminated (lambda (v) (+ v num))))
      (when side (hash-update! size 'not-elim (lambda (v) (+ v num))))
      (unless after-exit
        (hash-update! size 'last (lambda (v) (+ v num)))
        (when (or likely-eliminated elim-last) (hash-update! size 'last-elim (lambda (v) (+ v num))))
        (when side (hash-update! size 'last-not-elim (lambda (v) (+ v num)))))
      (> (- (quotient (* (hash-ref size 'overall) 3) 2) (hash-ref size 'eliminated) (hash-ref size 'last-elim))
         param-max-completely-peeled-insns)))
  (values large? size))

;; get_loop_body: the blocks of the loop, header first, in a DFS from the header
(define (loop-body l)
  (define cfg (ctx-cfg))
  (let walk ([bb (Loop-header l)] [seen '()])
    (if (memv bb seen)
        seen
        (for/fold ([seen (append seen (list bb))]) ([e (in-pvector (cfg-succs cfg bb))] #:when (loop-inside? l (GEdge-dest e)))
          (walk (GEdge-dest e) seen)))))

;; estimated_unrolled_size: (values unr-insns est-eliminated)
(define (estimated-unrolled-size size n)
  (cond
    [(>= (major) 13)
     (define unr (+ (* n (- (hash-ref size 'overall) (hash-ref size 'eliminated)))
                    (- (hash-ref size 'last) (hash-ref size 'last-elim))))
     (define not-elim (+ (* n (hash-ref size 'not-elim)) (hash-ref size 'last-not-elim)))
     (values unr (- unr not-elim (quotient (* (- unr not-elim) 2) 3)))]
    [else
     (define u0 (+ (if (zero? n) 0 (* n (- (hash-ref size 'overall) (hash-ref size 'eliminated))))
                   (- (hash-ref size 'last) (hash-ref size 'last-elim))))
     (define u (quotient (* u0 2) 3))
     (values (if (<= u 0) 1 u) 0)]))

;; ------------------------------------------------------------
;; the transformation
;; ------------------------------------------------------------

(define (unroll! st0 fndecl cfg0 plan)
  (define major (gcc-major-version st0))
  (define st st0)
  (define cfg cfg0)
  (define stamp (cfg-next-stamp cfg0))
  (define (fresh-stamp!) (begin0 stamp (set! stamp (add1 stamp))))
  (define h (hash-ref plan 'header)) (define latch (hash-ref plan 'latch))
  (define body-set (hash-ref plan 'body))
  (define (in-loop? bb) (intbits-ref body-set bb))
  (define exit (hash-ref plan 'exit)) (define edge-to-cancel (hash-ref plan 'edge-to-cancel))
  (define n (hash-ref plan 'n-unroll)) (define niter (hash-ref plan 'niter))
  (define bbs (hash-ref plan 'bbs))
  ;; ---- CFG / profile helpers ----------------------------------------
  (define (block bb) (cfg-block cfg bb))
  (define (phis bb) (GBlock-phis (block bb)))
  (define (set-phis! bb v) (set! cfg (set-block cfg (block-set-phis (block bb) v))))
  (define (count bb) (or (prof-count cfg bb) 0))
  (define (set-count! bb c)
    (define pf (GCfg-profile cfg))
    (when pf (set! cfg (cfg-set-profile cfg (GProf (intmap-set (GProf-counts pf) bb c) (GProf-probs pf))))))
  (define (prob src dest) (or (prof-prob cfg src dest) 0))
  (define (set-prob! src dest p)
    (define pf (GCfg-profile cfg))
    (when pf (set! cfg (cfg-set-profile cfg (GProf (GProf-counts pf) (hash-set (GProf-probs pf) (cons src dest) p))))))
  (define (edge-count src dest) (count-apply-prob (count src) (prob src dest)))
  (define (succ-dests bb) (for/list ([e (in-pvector (cfg-succs cfg bb))]) (GEdge-dest e)))
  ;; a PHI argument slot for a new predecessor (reserve_phi_args_for_new_edge)
  (define (append-phi-slot! dest src val)
    (when (cfg-block cfg dest)
      (set-phis! dest (for/pvector ([ph (in-pvector (phis dest))])
                        (define stamps (or (phi-arg-stamps ph) (for/pvector ([a (in-pvector (gphi-args ph))]) #f)))
                        (gphi (gphi-loc ph) (hash-set (gphi-flags ph) 'arg-stamps (pvector-cons-right stamps #f))
                              (gphi-var ph) (gphi-result ph) (pvector-cons-right (gphi-args ph) (cons src val)))))))
  (define (set-phi-arg! dest src val-of)
    (set-phis! dest (for/pvector ([ph (in-pvector (phis dest))])
                      (define k (for/first ([a (in-pvector (gphi-args ph))] [i (in-naturals)] #:when (= (car a) src)) i))
                      (cond
                        [(not k) ph]
                        [else
                         (define v (val-of ph))
                         (define stamps (or (phi-arg-stamps ph) (for/pvector ([a (in-pvector (gphi-args ph))]) #f)))
                         (gphi (gphi-loc ph) (hash-set (gphi-flags ph) 'arg-stamps (pvector-set stamps k (and (ssa-name? v) (fresh-stamp!))))
                               (gphi-var ph) (gphi-result ph) (pvector-set (gphi-args ph) k (cons src v)))]))))
  (define (phi-arg ph pred) (for/first ([a (in-pvector (gphi-args ph))] #:when (= (car a) pred)) (cdr a)))
  (define (make-edge! src dest flags p)
    (define-values (_ok c) (make-edge cfg src dest flags))
    (set! cfg c)
    (set-prob! src dest p)
    (append-phi-slot! dest src #f))
  ;; redirect_edge_and_branch_force: (values) ; the PHI arguments of the
  ;; old destination for the edge are returned (ssa_redirect_edge)
  (define (redirect! src old new)
    (define saved (for/list ([ph (in-pvector (phis old))]) (phi-arg ph src)))
    (set! cfg (redirect-edge-succ cfg src old new))
    (append-phi-slot! new src #f)
    saved)
  ;; force_edge_cold (E, true)
  (define (force-edge-impossible! src dest)
    (define others (for/list ([d (in-list (succ-dests src))] #:unless (= d dest)) d))
    (define prob-sum (for/sum ([d (in-list others)]) (prob src d)))
    (cond
      [(> prob-sum 0) (set-edge-probability-and-rescale-others! src dest prob-never)]
      [else (set-prob! src dest (if (zero? prob-sum) prob-always prob-never))]))
  (define (set-edge-probability-and-rescale-others! src dest new)
    (define old (prob src dest))
    (unless (= old new)
      (define others (for/list ([d (in-list (succ-dests src))] #:unless (= d dest)) d))
      (cond
        [(= new prob-always) (for ([o (in-list others)]) (set-prob! src o prob-never))]
        [(= 1 (length others)) (set-prob! src (car others) (prob-invert new))]
        [(null? others) (void)]
        [(> (prob-invert old) 0)
         (for ([o (in-list others)]) (set-prob! src o (prob-apply-scale (prob src o) (prob-invert new) (prob-invert old))))]
        [else (for ([o (in-list others)]) (set-prob! src o (quotient (prob-invert new) (length others))))])
      (set-prob! src dest new)))
  ;; ---- names -----------------------------------------------------------
  (define table '())
  (define (new-def-for! old)
    (define var (ssa-name-var old))
    (define-values (nn st1) (if var (make-ssa-name st var) (make-ssa-name st (tree-type old) #:name (tree-attr old 'identifier #f))))
    (set! st (copy-range-info st1 fndecl old nn))
    (set! table (cons (cons nn old) table))
    nn)
  (define (copy-stmt g)
    (define g1 (if (stmt-vdef g) (stmt-set-flag g 'vdef (new-def-for! (stmt-vdef g))) g))
    (define g2
      (cond
        [(and (gassign? g1) (ssa-name? (gassign-lhs g1)))
         (gassign (gassign-loc g1) (gassign-flags g1) (new-def-for! (gassign-lhs g1)) (gassign-code g1) (gassign-rhs1 g1) (gassign-rhs2 g1) (gassign-rhs3 g1))]
        [(and (gcall? g1) (gcall-lhs g1) (ssa-name? (gcall-lhs g1)))
         (gcall (gcall-loc g1) (gcall-flags g1) (new-def-for! (gcall-lhs g1)) (gcall-fn g1) (gcall-args g1) (gcall-fntype g1) (gcall-internal? g1) (gcall-ecf g1))]
        [else g1]))
    (stmt-set-flag g2 'stamp (fresh-stamp!)))

  ;; ---- scale_loop_profile (loop, always, N) (GCC >= 13) ----------------
  (define (loop-count-in)
    (for/sum ([p (in-pvector (cfg-preds cfg h))] #:unless (= p latch)) (edge-count p h)))
  (when (>= major 13)
    (define hc (count h))
    (define cin (loop-count-in))
    (when (and (> hc 0) (> cin 0)
               (not (sreal<= (sreal/ (make-sreal (- hc cin)) (make-sreal cin)) (make-sreal n))))
      (define scale-prob (count-probability-in (* cin (add1 n)) hc))
      (define unadjusted (edge-count (car exit) (cdr exit)))
      ;; scale_loop_frequencies
      (for ([bb (in-list bbs)]) (set-count! bb (count-apply-prob (count bb) scale-prob)))
      ;; update_loop_exit_probability_scale_dom_bbs (exit, unadjusted)
      (define src (car exit))
      (define old-exit-count (edge-count src (cdr exit)))
      (unless (or (= unadjusted old-exit-count)
                  (and (> unadjusted (count src)) (count-differs? (count src) unadjusted))
                  (zero? (count src)))
        (set-edge-probability-and-rescale-others! src (cdr exit) (count-probability-in unadjusted (count src)))
        (define others (for/list ([d (in-list (succ-dests src))] #:when (in-loop? d)) d))
        (cond
          [(and (= 1 (length others)) (= (car others) latch))
           (when (= 1 (pvector-length (cfg-preds cfg latch)))
             (set-count! latch (- (+ (count latch) old-exit-count) (edge-count src (cdr exit)))))]
          [else (void)]))))                                     ; scale_dominated_blocks_in_loop: not ported

  ;; ---- duplicate_loop_body_to_header_edge -----------------------------
  (define pre (for/first ([p (in-pvector (cfg-preds cfg h))] #:unless (in-loop? p)) p))
  (define first-new-block (last-basic-block cfg))
  (define count-in (count h))
  (define count-le (edge-count latch h))
  (define count-out-orig (edge-count (car exit) (cdr exit)))
  (define prob-pass-thru (count-probability-in count-le count-in))
  (define new-count-le (+ count-le count-out-orig))
  (define exit-prob (prob (car exit) (cdr exit)))
  (define-values (after-exit-num after-exit-den bbs-to-scale)
    (cond
      [(not (= exit-prob prob-always))
       (define dom (cfg-dom cfg))
       (define num (count (car exit)))
       (values num (- num count-out-orig)
               (for/list ([bb (in-list bbs)] [i (in-naturals)]
                          #:when (and (not (= bb (car exit))) (dom-dominates? dom (dom-index dom (car exit)) (dom-index dom bb))))
                 i))]
      [else (values #f #f '())]))
  (define prob-pass-wont-exit (count-probability-in new-count-le count-in))
  (define scale-step (for/list ([i (in-range n)]) prob-pass-wont-exit))
  (define-values (scale-act0 scale-main)
    (let* ([wanted0 (edge-count pre h)]
           [act (count-probability-in wanted0 count-in)]
           [wanted (for/fold ([w wanted0]) ([s (in-list scale-step)]) (count-apply-prob w s))])
      (values act (count-probability-in wanted count-in))))
  (define latch-pos (sub1 (length bbs)))
  (define orig-of (make-hasheqv))         ; copy -> original
  (define to-remove '())
  (define pending #f)                     ; ssa_redirect_edge of the preheader edge
  (define e-src pre)                      ; the edge entering the next copy: E-SRC -> (h or the previous copy header)
  (define e-dest h)
  (define place-after pre)
  (define scale-act scale-act0)
  (for ([j (in-range n)])
    ;; copy_bbs
    (define new-bbs
      (for/list ([bb (in-list bbs)])
        (define-values (nb c) (create-bb cfg (pvector-empty) place-after))
        (set! cfg c)
        (set! place-after nb)
        (hash-set! orig-of nb bb)
        (set-phis! nb (for/pvector ([ph (in-pvector (phis bb))])
                        (gphi (gphi-loc ph) (hasheq 'arg-stamps (pvector-empty)) (gphi-var ph) (new-def-for! (gphi-result ph)) (pvector-empty))))
        (set! cfg (set-block cfg (block-set-stmts (cfg-block cfg nb)
                                                  (for/pvector ([g (in-pvector (GBlock-stmts (block bb)))] #:unless (glabel? g)) (copy-stmt g)))))
        (set-count! nb (count bb))
        (for ([e (in-list (pvector->list (cfg-succs cfg bb)))])
          (make-edge! nb (GEdge-dest e) (GEdge-flags e) (prob bb (GEdge-dest e))))
        nb))
    (define copy (for/hasheqv ([bb (in-list bbs)] [nb (in-list new-bbs)]) (values bb nb)))
    (for ([nb (in-list new-bbs)])
      (for ([d (in-list (succ-dests nb))] #:when (hash-ref copy d #f))
        (redirect! nb d (hash-ref copy d))))
    (define new-h (hash-ref copy h)) (define new-latch (hash-ref copy latch))
    ;; redirect the special edges (GCC 12 redirects the latch copy first)
    (define (redirect-entry!)
      (define saved (redirect! e-src e-dest new-h))
      (unless pending (set! pending saved)))
    (define (redirect-latch!) (redirect! new-latch new-h h))
    (if (>= major 13) (begin (redirect-entry!) (redirect-latch!)) (begin (redirect-latch!) (redirect-entry!)))
    ;; the exit of this copy is removed later
    (define new-exit-src (hash-ref copy (car exit)))
    (set! to-remove (cons (cons new-exit-src (cdr exit)) to-remove))
    (force-edge-impossible! new-exit-src (cdr exit))
    (when after-exit-num
      (for ([i (in-list bbs-to-scale)])
        (define nb (list-ref new-bbs i))
        (set-count! nb (count-apply-scale (count nb) after-exit-num after-exit-den))))
    (for ([nb (in-list new-bbs)]) (set-count! nb (count-apply-prob (count nb) scale-act)))
    (set! scale-act (prob* scale-act (list-ref scale-step j)))
    (set! e-src new-latch) (set! e-dest h))
  (for ([bb (in-list bbs)]) (set-count! bb (count-apply-prob (count bb) scale-main)))
  ;; flush_pending_stmts: the preheader arguments go to the first copy's header
  (define first-h (for/first ([(c o) (in-hash orig-of)] #:when (and (= o h) (memv pre (pvector->list (cfg-preds cfg c))))) c))
  (when (and first-h pending)
    (set-phis! first-h (for/pvector ([ph (in-pvector (phis first-h))] [v (in-list pending)])
                         (define k (for/first ([a (in-pvector (gphi-args ph))] [i (in-naturals)] #:when (= (car a) pre)) i))
                         (define stamps (or (phi-arg-stamps ph) (for/pvector ([a (in-pvector (gphi-args ph))]) #f)))
                         (gphi (gphi-loc ph) (hash-set (gphi-flags ph) 'arg-stamps (pvector-set stamps k (and (ssa-name? v) (fresh-stamp!))))
                               (gphi-var ph) (gphi-result ph) (pvector-set (gphi-args ph) k (cons pre v))))))
  ;; copy_phi_node_args: the arguments of edges out of the new blocks
  (for ([nb (in-range first-new-block (last-basic-block cfg))] #:when (cfg-block cfg nb))
    (for ([d (in-list (succ-dests nb))] #:unless (pvector-empty? (phis d)))
      (define bb (hash-ref orig-of nb nb))
      (define dest (hash-ref orig-of d d))
      (define e-dest (if (find-edge cfg bb dest) dest
                         (for/first ([x (in-list (succ-dests bb))] #:when (= (hash-ref orig-of x x) dest)) x)))
      (define orig-phis (phis e-dest))
      (set-phis! d (for/pvector ([ph (in-pvector (phis d))] [o (in-pvector orig-phis)])
                     (define k (for/first ([a (in-pvector (gphi-args ph))] [i (in-naturals)] #:when (= (car a) nb)) i))
                     (define v (phi-arg o bb))
                     (define stamps (or (phi-arg-stamps ph) (for/pvector ([a (in-pvector (gphi-args ph))]) #f)))
                     (gphi (gphi-loc ph) (hash-set (gphi-flags ph) 'arg-stamps (pvector-set stamps k (and (ssa-name? v) (fresh-stamp!))))
                           (gphi-var ph) (gphi-result ph) (pvector-set (gphi-args ph) k (cons nb v)))))))

  ;; ---- the last iteration: cancel the latch edge ------------------------
  (when edge-to-cancel
    (force-edge-impossible! (car edge-to-cancel) (cdr edge-to-cancel))
    (define src (car edge-to-cancel))
    (define g (bb-last-stmt cfg src))
    (define true? (edge-flag? (find-edge cfg src (cdr edge-to-cancel)) EDGE-TRUE-VALUE))
    (set! cfg (bb-set-last-stmt cfg src (cond-make st g (not true?) (fresh-stamp!)))))

  ;; ---- unloop_loops ------------------------------------------------------
  ;; remove_exits_and_undefined_stmts: the exit bound N <= N peeled
  (when (and niter (<= niter n))
    (define src (car exit))
    (define g (bb-last-stmt cfg src))
    (set-prob! src (cdr exit) prob-always)
    (define exit-true? (edge-flag? (find-edge cfg src (cdr exit)) EDGE-TRUE-VALUE))
    (set! cfg (bb-set-last-stmt cfg src (cond-make st g exit-true? (fresh-stamp!)))))
  ;; unloop: the latch edge goes away; a new block for its destination
  (define latch-flags (GEdge-flags (find-edge cfg latch h)))
  (set! cfg (remove-edge cfg latch h))
  (define-values (ub c-ub) (create-bb cfg (pvector-empty) latch))
  (set! cfg c-ub)
  (make-edge! latch ub latch-flags prob-never)
  (set-count! ub 0)
  ;; remove_path for each removed exit, in the order they were recorded
  (for ([e (in-list (reverse to-remove))] #:when (cfg-block cfg (car e)))
    (define src (car e)) (define dest (cdr e))
    (define-values (src2 dest2)
      (if (> (pvector-length (cfg-preds cfg dest)) 1)
          (let-values ([(nb c) (split-edge cfg src dest)])
            (set! cfg c)
            (set-count! nb (edge-count src nb))
            (values src nb))
          (values src dest)))
    ;; remove_branch: the edge is redirected to the other successor's
    ;; destination, where it merges with the existing edge
    (define other (for/first ([d (in-list (succ-dests src2))] #:unless (= d dest2)) d))
    (define p-other (prob src2 other)) (define p-e (prob src2 dest2))
    (set! cfg (remove-edge cfg src2 dest2))
    (set-prob! src2 other (min prob-always (+ p-other p-e)))
    ;; remove_bbs
    (set! cfg (delete-basic-block cfg dest2)))

  ;; ---- update_ssa (TODO_update_ssa) ------------------------------------
  (define-values (cfg1 st1) (update-ssa st fndecl cfg (reverse table)))
  (set! cfg cfg1) (set! st st1)
  ;; scev_reset; cleanup_tree_cfg
  (cleanup-tree-cfg st cfg print-cond-rhs))

;; gimple_cond_make_true / _false
(define (cond-make st g true? stamp)
  (stmt-set-flag (gcond (gcond-loc g) (gcond-flags g) 'ne_expr
                        (build-int-cst st (boolean-type st) (if true? 1 0)) (build-int-cst st (boolean-type st) 0)
                        (gcond-true-label g) (gcond-false-label g))
                 'stamp stamp))
