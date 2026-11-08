#lang racket/base

;; ============================================================
;; opt: pass_iv_canon (gcc/tree-ssa-loop-ivcanon.cc
;; canonicalize_induction_variables): a loop whose iteration count is
;; a known constant gets a canonical down-counting induction variable
;; that controls its exit (create_canonical_iv / create_iv)
;; ============================================================
;;
;; (ivcanon-pass st fndecl cfg) -> (values cfg st)
;;
;; Per loop, innermost first: the iteration count of the single exit
;; (number_of_iterations_exit; with several exits find_loop_niter;
;; then find_loop_niter_by_eval); a constant count N on an exit tested
;; once per iteration gets ivtmp = PHI <N + 1 (preheader), ivtmp' (latch)>,
;; ivtmp' = ivtmp - 1 before the exit test, and the test becomes
;; ivtmp' != 0 (== 0 when the exit is the true edge).
;;
;; estimate_numbers_of_iterations (niter-bounds.rkt) runs first.
;;
;; Not ported: try_unroll_loop_completely (UL_SINGLE_ITER only removes
;; loops that never iterate), remove_redundant_iv_tests.
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         "../cfg.rkt"
         (only-in "loops.rkt" loops-innermost-first loops-preorder)
         "ssa-ops.rkt"
         "generic-fold.rkt"
         "scev.rkt"
         (only-in "niter-bounds.rkt" estimate-all-loops record-constant-niter!))

(provide ivcanon-pass create-iv)

(define (ivcanon-pass st0 fndecl cfg0)
  ;; canonicalize_induction_variables: estimate_numbers_of_iterations (cfun) first
  (define st (estimate-all-loops (gs-set-fndecl st0 fndecl) fndecl cfg0))
  (define cfg (cfg-clear-messages cfg0))
  (define headers
    (let-values ([(r s) (with-loops st fndecl cfg
                          (lambda () (for/list ([l (in-list (loops-innermost-first (ctx-loops)))]) (Loop-header l))))])
      r))
  (for ([h (in-list headers)])
    (define-values (plan st1)
      (with-loops st fndecl cfg
        (lambda ()
          (define l (findf (lambda (l) (= (Loop-header l) h)) (ctx-loops)))
          (and l (canonical-iv-plan l)))))
    (set! st st1)
    (when plan
      (define-values (c s) (apply (lambda (l exit niter) (create-canonical-iv st cfg l exit niter)) plan))
      (set! cfg c) (set! st s)))
  (values cfg st))

;; canonicalize_loop_induction_variables up to the create_iv decision:
;; (list loop exit niter) or #f
(define (canonical-iv-plan l)
  (define exit0 (single-exit l))
  (define d (and exit0 (number-of-iterations-exit l exit0)))
  (define niter0 (if d (niter-desc-niter d) 'dont-know))
  (define may-be-zero (and d (not (integer-zerop (niter-desc-may-be-zero d)))))
  (define-values (niter exit)
    (cond
      [(and (Tree? niter0) (icst? niter0)) (values niter0 exit0)]
      [else
       (define n1
         (cond
           [(not may-be-zero) niter0]
           [(comparison-code? (tree-code (niter-desc-may-be-zero d)))
            (gf-build3 'cond_expr (tree-type niter0) (niter-desc-may-be-zero d) (icst (tree-type niter0) 0) niter0)]
           [else 'dont-know]))
       (define-values (n2 e2) (if exit0 (values n1 exit0) (find-loop-niter l)))
       (define-values (n3 e3)
         (if (or (chrec-contains-undetermined? n2) (not (icst? n2)))
             (let-values ([(n e) (find-loop-niter-by-eval l)]) (if (icst? n) (values n e) (values n e2)))
             (values n2 e2)))
       (values n3 (and (icst? n3) e3))]))
  ;; a constant count is kept as a bound (record_niter_bound)
  (when (and (Tree? niter) (icst? niter)) (record-constant-niter! l (icst-val niter) exit))
  ;; try_unroll_loop_completely (UL_SINGLE_ITER) is a no-op for N > 0
  (and exit (icst? niter) (not (and may-be-zero (icst? niter0)))
       (just-once-each-iteration? l (car exit))
       (list l exit niter)))

;; create_canonical_iv
(define (create-canonical-iv st0 cfg0 l exit niter)
  (define-values (cfg st)
    (let* ([src (car exit)]
           [in (for/first ([e (in-pvector (cfg-succs cfg0 src))] #:unless (= (GEdge-dest e) (cdr exit))) (GEdge-dest e))]
           [type (tree-type niter)]
           [base (let-values ([(b s) (with-gs st0 (lambda () (gf-build2 'plus_expr type niter (icst type 1))))]) b)])
      (define-values (cfg1 st1 vb va)
        (create-iv st0 cfg0 l base 'plus_expr (build-int-cst st0 type -1) src #f))
      ;; the exit test
      (define exit-true? (edge-flag? (find-edge cfg1 src (cdr exit)) EDGE-TRUE-VALUE))
      (define g (bb-last-stmt cfg1 src))
      (define g2 (stmt-set-flag (gcond (gcond-loc g) (gcond-flags g) (if exit-true? 'eq_expr 'ne_expr) va (build-int-cst st1 type 0)
                                       (gcond-true-label g) (gcond-false-label g))
                                'stamp (cfg-next-stamp cfg1)))
      (values (bb-set-last-stmt cfg1 src g2) st1)))
  (values cfg st))

;; create_iv (BASE, INCR-OP, STEP, NULL, LOOP, &gsi_last_bb (INCR-BB), AFTER):
;; (values cfg st var-before var-after); the increment goes before the
;; last statement of INCR-BB (AFTER = #f) or at its end
(define (create-iv st0 cfg0 l base incr-op0 step0 incr-bb after?)
  (define type (tree-type base))
  (define-values (vb st1) (make-ssa-name st0 type #:name "ivtmp"))
  (define-values (va st2) (make-ssa-name st1 type #:name "ivtmp"))
  ;; MINUS_EXPRs for readability
  (define-values (incr-op step)
    (cond
      [(and (icst? step0) (type-attr st2 (tree-type step0) 'unsigned #f))
       (define step1 (build-int-cst st2 (tree-type step0) (- (integer-cst-value step0))))
       (if (< (integer-cst-value step1) (integer-cst-value step0))
           (values (if (eq? incr-op0 'plus_expr) 'minus_expr 'plus_expr) step1)
           (values incr-op0 step0))]
      [(and (icst? step0) (< (integer-cst-value step0) 0))
       (values (if (eq? incr-op0 'plus_expr) 'minus_expr 'plus_expr) (build-int-cst st2 (tree-type step0) (- (integer-cst-value step0))))]
      [else (values incr-op0 step0)]))
  (define stamp (cfg-next-stamp cfg0))
  (define incr (stmt-set-flag (gassign #f (hasheq) va incr-op vb step #f) 'stamp stamp))
  (define ss (GBlock-stmts (cfg-block cfg0 incr-bb)))
  (define k (if after? (pvector-length ss) (max 0 (sub1 (pvector-length ss)))))
  (define cfg1 (set-block cfg0 (block-set-stmts (cfg-block cfg0 incr-bb)
                                                (pvector-append (pvector-cons-right (pvector-take ss k) incr) (pvector-drop ss k)))))
  ;; the PHI in the header: the base on the preheader edge, VA on the latch edge
  (define h (Loop-header l))
  (define latch (car (Loop-latches l)))
  (define preds (cfg-preds cfg1 h))
  (define args (for/pvector ([p (in-pvector preds)]) (cons p (if (= p latch) va base))))
  (define stamps (for/pvector ([p (in-pvector preds)]) (and (= p latch) (add1 stamp))))
  (define b (cfg-block cfg1 h))
  (define phi (gphi #f (hasheq 'arg-stamps stamps) #f vb args))
  (values (set-block cfg1 (block-set-phis b (pvector-cons-right (GBlock-phis b) phi))) st2 vb va))
