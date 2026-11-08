#lang racket/base

;; ============================================================
;; opt: pass_scev_cprop (gcc/tree-scalar-evolution.cc
;; final_value_replacement_loop): the loop-closed PHIs on the single
;; exit of a loop whose iteration count is known are replaced by the
;; value of their scalar evolution after the last iteration
;; ============================================================
;;
;; (sccp-pass st fndecl cfg) -> (values cfg st)
;;
;; Per loop, innermost first: the PHIs of the exit block whose argument
;; has an evolution without symbols defined in the loop and a cheap
;; overall effect are removed; the value is gimplified
;; (force_gimple_operand) and assigned to the PHI result after the
;; labels of the exit block.  GCC >= 13 also replaces the uses of a
;; constant value (replace_uses_by).  The pass asks for
;; TODO_cleanup_cfg when anything was replaced.
;;
;; Not ported: split_loop_exit_edge (an exit block with several
;; predecessors is skipped), the bit-operation inductions of GCC 15,
;; rewrite_to_defined_overflow for folded casts, the re-folding of the
;; uses of a non-constant value.
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         "../cfg.rkt"
         (only-in "loops.rkt" loops-innermost-first loops-preorder)
         (only-in "../ssa.rkt" virtual-operand?)
         (only-in "../gimplify.rkt" is-gimple-val)
         (only-in "../print.rkt" print-expr print-cond-rhs stmt->string)
         "ssa-ops.rkt"
         "generic-fold.rkt"
         "scev.rkt")

(provide sccp-pass force-gimple-operand)

(define (sccp-pass st0 fndecl cfg0)
  (define st (gs-set-fndecl st0 fndecl))
  (define major (gcc-major-version st))
  (define cfg (cfg-clear-messages cfg0))
  (define any #f)
  ;; the loops, innermost first, by header (the loop tree is rebuilt
  ;; after each replacement)
  (define headers
    (let-values ([(r s) (with-loops st fndecl cfg
                          (lambda () (for/list ([l (in-list (loops-innermost-first (ctx-loops)))]) (Loop-header l))))])
      r))
  (for ([h (in-list headers)])
    (define-values (plan st1)
      (with-loops st fndecl cfg
        (lambda ()
          (define l (findf (lambda (l) (= (Loop-header l) h)) (ctx-loops)))
          (and l (final-value-plan l)))))
    (set! st st1)
    (when (and plan (pair? (cdr plan)))
      (set! any #t)
      (define-values (c s) (apply-plan st cfg major (car plan) (cdr plan)))
      (set! cfg c) (set! st s)))
  (if any
      (cleanup-tree-cfg st cfg print-cond-rhs)
      (values cfg st)))

;; (cons exit-dest (list (list phi value) ...)) or #f
(define (final-value-plan l)
  (define exit (single-exit l))
  (cond
    [(not exit) #f]
    [(chrec-dont-know? (number-of-latch-executions l)) #f]
    [(not (single-pred-p (ctx-cfg) (cdr exit))) #f]           ; split_loop_exit_edge not ported
    [else
     (define dest (cdr exit))
     (define ex-loop (let up ([x l]) (if (= (loop-depth x) (add1 (loop-depth (bb-loop dest)))) x (up (loop-outer x)))))
     (define phis (GBlock-phis (cfg-block (ctx-cfg) dest)))
     (cons dest
           (for*/list ([ph (in-pvector phis)]
                       [def0 (in-value (phi-arg-from ph (car exit)))]
                       #:unless (virtual-operand? (gphi-var ph))
                       #:when (or (ty-int? (tree-type def0)) (ty-ptr? (tree-type def0)))
                       [v (in-value (let-values ([(ev _folded) (analyze-scalar-evolution-in-loop ex-loop l def0)])
                                      (if (chrec-dont-know? ev) ev (compute-overall-effect-of-inner-loop ex-loop ev))))]
                       #:unless (or (auto-chrec? v)
                                    (tree-contains-chrecs? v)
                                    (chrec-contains-symbols-defined-in-loop? v (loop-num ex-loop))
                                    (let-values ([(exp? _c) (expression-expensive-p v)]) exp?)))
             (list ph v)))]))

(define (phi-arg-from ph src)
  (for/first ([a (in-pvector (gphi-args ph))] #:when (= (car a) src)) (cdr a)))

;; remove the PHIs, emit the final values
(define (apply-plan st0 cfg0 major dest items)
  (define st st0) (define cfg cfg0)
  (define (msg! fmt . args) (set! cfg (cfg-add-message cfg (apply format fmt args))))
  (for ([it (in-list items)])
    (define ph (car it)) (define def (cadr it))
    (define rslt (gphi-result ph))
    (msg! "") (msg! "final value replacement:")
    (msg! "  ~a" (stmt->string st cfg dest ph))
    (msg! " with expr: ~a" (print-expr st def))
    ;; remove_phi_node
    (define b (cfg-block cfg dest))
    (set! cfg (set-block cfg (block-set-phis b (for/pvector ([p (in-pvector (GBlock-phis b))]
                                                             #:unless (= (ssa-name-version (gphi-result p)) (ssa-name-version rslt)))
                                                 p))))
    ;; propagate constants immediately (GCC >= 13)
    (when (and (>= major 13) (constant-class-p st def))
      (set! cfg (replace-uses-by st cfg rslt def)))
    (define-values (rhs pre st1) (force-gimple-operand st def))
    (set! st st1)
    (define stamp (cfg-next-stamp cfg))
    (define ass (stmt-set-flag (build-assign rslt rhs) 'stamp (+ stamp (length pre))))
    (define new (append (for/list ([g (in-list pre)] [i (in-naturals)]) (stmt-set-flag g 'stamp (+ stamp i))) (list ass)))
    ;; gsi_after_labels (exit->dest)
    (define b2 (cfg-block cfg dest))
    (define ss (GBlock-stmts b2))
    (define k (or (for/first ([g (in-pvector ss)] [i (in-naturals)] #:unless (glabel? g)) i) (pvector-length ss)))
    (set! cfg (set-block cfg (block-set-stmts b2 (pvector-append (pvector-append (pvector-take ss k) (list->pvector new)) (pvector-drop ss k)))))
    (msg! " final stmt:")
    (msg! "  ~a" (stmt->string st cfg dest ass))
    (msg! ""))
  (values cfg st))

;; gimple_build_assign (LHS, EXPR)
(define (build-assign lhs e)
  (define code (tree-code e))
  (cond
    [(memq code '(ssa_name integer_cst real_cst var_decl parm_decl addr_expr)) (gassign #f (hasheq) lhs code e #f #f)]
    [(= (tree-operand-length e) 1) (gassign #f (hasheq) lhs code (tree-operand e 0) #f #f)]
    [(= (tree-operand-length e) 2) (gassign #f (hasheq) lhs code (tree-operand e 0) (tree-operand e 1) #f)]
    [else (gassign #f (hasheq) lhs code (tree-operand e 0) (tree-operand e 1) (tree-operand e 2))]))

;; force_gimple_operand (EXPR, &stmts, false, NULL): (values rhs stmts st)
;; with RHS a valid GIMPLE right-hand side; the operands that are not
;; GIMPLE values are computed into new SSA names first, operand 0 first
(define (force-gimple-operand st0 e)
  (define st st0)
  (define pre '())
  (define (val! x)
    (cond
      [(is-gimple-val st x) x]
      [else
       (define r (rhs! x))
       (define-values (n st1) (make-ssa-name st (tree-type x)))
       (set! st st1)
       (set! pre (cons (build-assign n r) pre))
       n]))
  (define (rhs! x)
    (cond
      [(is-gimple-val st x) x]
      [else (make-tree (tree-code x) #:type (tree-type x)
                       #:ops (for/list ([i (in-range (tree-operand-length x))]) (val! (tree-operand x i))))]))
  (define r (rhs! e))
  (values r (reverse pre) st))
