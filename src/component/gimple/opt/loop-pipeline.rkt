#lang racket/base

;; ============================================================
;; opt: the frame of the loop optimizer pipeline (gcc/tree-ssa-loop.cc
;; pass_tree_loop / pass_tree_loop_init / pass_tree_no_loop) and the
;; loop-closed SSA form (gcc/tree-ssa-loop-manip.cc)
;; ============================================================
;;
;; Loop state between passes is a decl flag of the function:
;;   'loop-pipeline : #t once pass_tree_loop's gate held (the function
;;                    had a loop); the loop sub-passes and pass_tree_no_loop
;;                    gate on it
;;   'loops-state   : list of the loops_state flags that are set beyond
;;                    LOOPS_MAY_HAVE_MULTIPLE_LATCHES:
;;                    'preheaders 'simple-latches 'lcssa 'scev
;;                    (cleanup_tree_cfg protects preheaders / latches by
;;                    it and does not compact blocks while 'scev is set)
;;
;;   (function-has-loops? cfg)            gate_loop: number_of_loops > 1
;;   (in-loop-pipeline? st decl cfg)      gate of the sub-passes
;;   (not-in-loop-pipeline? st decl cfg)  gate of pass_tree_no_loop
;;   (loop-pass st decl cfg)              pass_tree_loop (no execute)
;;   (loop-init-pass st decl cfg)         pass_tree_loop_init:
;;     loop_optimizer_init (LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS),
;;     rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa), scev_initialize
;;   (rewrite-into-loop-closed-ssa st decl cfg) -> (values cfg st)
;;
;; Not ported: TODO_update_address_taken at the start of loopinit.
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../../../driver/dominance/dom.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         "../cfg.rkt"
         "ssa-ops.rkt"
         (only-in "mfn.rkt" stmt-use-names stmt-def-names)
         (only-in "range-info.rkt" copy-range-info)
         "loops.rkt"
         "update-ssa.rkt")

(provide function-has-loops? in-loop-pipeline? not-in-loop-pipeline? loop-pass loop-init-pass
         rewrite-into-loop-closed-ssa loops-state set-loops-state)

(define (loops-state st decl) (decl-flag st decl 'loops-state '()))
(define (set-loops-state st decl flags) (set-decl-flag st decl 'loops-state flags))

(define (function-has-loops? cfg)
  (define-values (loops _b) (find-loops cfg (dom-of cfg)))
  (pair? loops))
(define (in-loop-pipeline? st decl cfg) (decl-flag st decl 'loop-pipeline #f))
(define (not-in-loop-pipeline? st decl cfg) (not (in-loop-pipeline? st decl cfg)))

(define (loop-pass st decl cfg)
  (values (cfg-clear-messages cfg) (set-decl-flag st decl 'loop-pipeline #t)))

(define (loop-init-pass st0 decl cfg0)
  (define st (gs-set-fndecl st0 decl))
  (define cfg1 (normalize-loops st (cfg-clear-messages cfg0)))
  (define-values (cfg2 st2) (rewrite-into-loop-closed-ssa st decl cfg1))
  (values cfg2 (set-loops-state st2 decl '(preheaders simple-latches recorded-exits lcssa scev))))

;; ------------------------------------------------------------
;; rewrite_into_loop_closed_ssa_1 (NULL, TODO_update_ssa, SSA_OP_ALL_USES)
;; ------------------------------------------------------------

(define (rewrite-into-loop-closed-ssa st0 decl cfg)
  (define st st0)
  (define dom (dom-of cfg))
  (define-values (loops _b) (find-loops cfg dom))
  (define (block-phis bb) (GBlock-phis (cfg-block cfg bb)))
  (define (block-stmts bb) (GBlock-stmts (cfg-block cfg bb)))
  ;; definition block of every SSA name
  (define def-bb (make-hasheqv))
  (for ([bb (in-pvector (cfg-bbs cfg))])
    (for ([ph (in-pvector (block-phis bb))]) (hash-set! def-bb (ssa-name-version (gphi-result ph)) bb))
    (for* ([g (in-pvector (block-stmts bb))] [d (in-list (stmt-def-names g))]) (hash-set! def-bb (ssa-name-version d) bb)))
  (define (loop-father bb) (loop-of loops bb))        ; #f = the root
  (define (loop-by-num n) (findf (lambda (l) (= (Loop-num l) n)) loops))
  (define (depth l) (if l (Loop-depth l) 0))
  (define (outer l) (and l (Loop-parent l) (loop-by-num (Loop-parent l))))
  (define (inside? l bb) (or (not l) (loop-inside? l bb)))
  ;; flow_loop_nested_p (OUTER, LOOP): LOOP is strictly inside OUTER
  (define (nested? o l) (and l (let up ([x (outer l)]) (cond [(not x) (not o)] [(and o (= (Loop-num x) (Loop-num o))) #t] [else (up (outer x))]))))
  (define (same? a b) (or (and (not a) (not b)) (and a b (= (Loop-num a) (Loop-num b)))))
  (define (superloop-at-depth l d) (if (<= (depth l) d) l (superloop-at-depth (outer l) d)))
  ;; find_sibling_superloop
  (define (sibling-superloop use-loop def-loop)
    (define ud (depth use-loop)) (define dd (depth def-loop))
    (define u (if (> ud dd) (superloop-at-depth use-loop dd) use-loop))
    (define d (if (> dd ud) (superloop-at-depth def-loop ud) def-loop))
    (let loop ([u u] [d d])
      (if (same? (outer u) (outer d)) u (loop (outer u) (outer d)))))
  ;; find_uses_to_rename: blocks in chain order, the PHI arguments of the
  ;; successors first, then the statements
  (define use-blocks (make-hasheqv))
  (define names (make-hasheqv))
  (define (use! bb t)
    (when (ssa-name? t)
      (define d (hash-ref def-bb (ssa-name-version t) #f))
      (define dl (and d (loop-father d)))
      (when (and dl (not (inside? dl bb)))
        (hash-set! names (ssa-name-version t) t)
        (hash-set! use-blocks (ssa-name-version t) (intbits-set (hash-ref use-blocks (ssa-name-version t) intbits-empty) bb)))))
  (for ([bb (in-pvector (cfg-bbs cfg))])
    (for* ([e (in-pvector (cfg-succs cfg bb))]
           #:when (cfg-block cfg (GEdge-dest e))
           [ph (in-pvector (block-phis (GEdge-dest e)))])
      (define a (for/first ([a (in-pvector (gphi-args ph))] #:when (= (car a) bb)) (cdr a)))
      (when a (use! bb a)))
    (for* ([g (in-pvector (block-stmts bb))] [u (in-list (stmt-use-names g))]) (use! bb u)))
  (cond
    [(zero? (hash-count names)) (values cfg st)]
    [else
     ;; add_exit_phis: the names sorted by the number of their definition loop
     (define sorted
       (sort (sort (hash-keys names) <) < #:key (lambda (v) (Loop-num (loop-father (hash-ref def-bb v))))))
     (define table '())
     (define multiple? #f)
     (define stamp (cfg-next-stamp cfg))
     (define (fresh-stamp!) (begin0 stamp (set! stamp (add1 stamp))))
     (for ([v (in-list sorted)])
       (define var (hash-ref names v))
       (define d (hash-ref def-bb v))
       (define def-loop (loop-father d))
       (define def-loop-exits
         (let up ([l def-loop] [s intbits-empty])
           (if (not l) s (up (outer l) (for/fold ([s s]) ([e (in-list (loop-exits cfg l))]) (intbits-set s (cdr e)))))))
       ;; compute_live_loop_exits
       (define live intbits-empty)
       (define work '())
       (for ([u (in-list (intbits->list (hash-ref use-blocks v)))])
         (define ul (loop-father u))
         (define ub (if (nested? ul def-loop) u (Loop-header (sibling-superloop ul def-loop))))
         (unless (intbits-ref live ub) (set! live (intbits-set live ub)) (set! work (cons ub work))))
       (let loop ()
         (unless (null? work)
           (define bb (car work)) (set! work (cdr work))
           (for ([p0 (in-pvector (cfg-preds cfg bb))])
             (define pl (loop-father p0))
             (define stop?
               (and (>= (depth pl) (depth def-loop))
                    (same? (if (> (depth pl) (depth def-loop)) (superloop-at-depth pl (depth def-loop)) pl) def-loop)))
             (unless stop?
               (define p (if (and (< (depth pl) (depth def-loop)) (not (nested? pl def-loop)))
                             (Loop-header (sibling-superloop pl def-loop))
                             p0))
               (define visited (intbits-ref live p))
               (set! live (intbits-set live p))
               (unless (or visited (dominates? dom bb p))
                 (set! work (cons p work)))))
           (loop)))
       (define exits (for/list ([b (in-list (intbits->list live))] #:when (intbits-ref def-loop-exits b)) b))
       (when (> (length exits) 1) (set! multiple? #t))
       ;; add_exit_phi: create_phi_node + create_new_def_for, one argument per predecessor
       (for ([x (in-list exits)])
         (define-values (n st1)
           (if (ssa-name-var var) (make-ssa-name st (ssa-name-var var)) (make-ssa-name st (tree-type var) #:name (tree-attr var 'identifier #f))))
         (set! st (copy-range-info st1 decl var n))
         (set! table (cons (cons n var) table))
         (define b (cfg-block cfg x))
         (define preds (cfg-preds cfg x))
         (set! cfg (set-block cfg (block-set-phis b (pvector-cons-right
                                                     (GBlock-phis b)
                                                     (gphi #f (hasheq 'arg-stamps (for/pvector ([p (in-pvector preds)]) (fresh-stamp!)))
                                                           (ssa-name-var var) n (for/pvector ([p (in-pvector preds)]) (cons p var)))))))))
     (update-ssa st decl cfg (reverse table) #:phis? multiple?)]))
