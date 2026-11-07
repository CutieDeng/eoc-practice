#lang racket/base

;; ============================================================
;; opt: pass_cd_dce / pass_dce (gcc/tree-ssa-dce.cc) at -O1, where
;; pass_cd_dce runs perform_tree_ssa_dce (aggressive = optimize >= 2)
;; = plain dead code elimination
;; ============================================================
;;
;;   1. obviously necessary statements: labels, predicts, returns,
;;      conditions and switches (non-aggressive), statements with side
;;      effects or that alter control flow, stores that may clobber
;;      global memory (stmt_may_clobber_global_p), i.e. every store
;;      through a pointer (no points-to information);
;;   2. propagate_necessity: the SSA operands of a necessary statement,
;;      and for a necessary load / call / return with a VUSE the
;;      reaching stores: an unaliased declaration's load marks the
;;      clobbering definitions up to a covering one
;;      (mark_aliased_reaching_defs_necessary), anything else marks all
;;      reaching definitions except stores to unaliased declarations
;;      (mark_all_reaching_defs_necessary);
;;   3. eliminate_unnecessary_stmts: unnecessary statements removed
;;      (clobbers stay unless an operand died), unused call results
;;      dropped, dead PHIs removed, degenerate virtual PHIs propagated.
;; Something removed -> TODO_cleanup_cfg; GCC >= 15 also asks for
;; TODO_remove_unused_locals (the pass table carries that).
;;
;; Not ported: the aggressive (control dependence) mode, allocation
;; pairs (malloc/free, new/delete), atomics, the chain-overflow
;; heuristic, debug statements.
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../../../driver/dominance/dom.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         "../gimplify.rkt"
         "../cfg.rkt"
         "../ssa.rkt"
         "../print.rkt"
         "ssa-ops.rkt"
         "mfn.rkt"
         "alias.rkt")

(provide dce)

(define (dce st fndecl cfg0)
  (define st0 (gs-set-fndecl st fndecl))
  (define cfg (cfg-clear-messages cfg0))
  (define m (gcfg->mfn cfg))
  (define dom (compute-dom (pvector-cons-left (pvector-cons-left (cfg-bbs cfg) EXIT) ENTRY) ENTRY
                           (lambda (i) (for/pvector ([e (in-pvector (cfg-succs cfg i))]) (GEdge-dest e)))))
  (define necessary (make-hasheqv))       ; id -> #t
  (define processed (make-hasheqv))       ; version -> #t
  (define worklist '())
  (define (mark-necessary! id add?)
    (unless (hash-ref necessary id #f)
      (hash-set! necessary id #t)
      (when add? (set! worklist (cons id worklist)))))
  (define (mark-operand-necessary! op)
    (define v (ssa-name-version op))
    (unless (hash-ref processed v #f)
      (hash-set! processed v #t)
      (define id (mfn-def m op))
      (when (and id (not (hash-ref necessary id #f)))
        (hash-set! necessary id #t)
        (set! worklist (cons id worklist)))))
  (define (clobber? g) (and (gassign? g) (tree-clobber-p (gassign-rhs1 g))))

  ;; find_obviously_necessary_stmts
  (for* ([bb (in-pvector (cfg-bbs cfg))] [id (in-list (mfn-stmts m bb))])
    (define g (mfn-stmt m id))
    (cond
      [(or (gpredict? g) (glabel? g)) (mark-necessary! id #f)]
      [(greturn? g) (mark-necessary! id #t)]
      [(and (gcall? g) (stmt-flag g 'ctrl-altering #f)) (mark-necessary! id #t)]
      [(or (gcond? g) (gswitch? g)) (mark-necessary! id #t)]
      [(and (clobber? g) (pair? (stmt-real-uses g))) (void)]
      [(or (stmt-has-side-effects? st0 g) (stmt-ends-bb? g)) (mark-necessary! id #t)]
      [(stmt-may-clobber-global? st0 g) (mark-necessary! id #t)]
      [else (void)]))

  ;; propagate_necessity (aggressive = false)
  (define (mark-aliased-reaching-defs! g ref)
    (define r (make-ao-ref st0 ref))
    (define bb (mfn-bb m (mfn-def m (stmt-vuse g))))
    (walk-aliased-vdefs st0 m r (stmt-vuse g)
                        (lambda (vdef)
                          (define did (mfn-def m vdef))
                          (define dg (mfn-stmt m did))
                          (unless (clobber? dg) (mark-operand-necessary! vdef))
                          (define lhs (stmt-get-lhs dg))
                          (cond
                            [(and lhs (not (ssa-name? lhs)))
                             (define-values (base off size max-size) (get-ref-base-and-extent st0 lhs))
                             (and (equal? base (ao-ref-base r))
                                  (or (and (= size max-size) (known-subrange? (ao-ref-offset r) (ao-ref-max-size r) off size))
                                      (equal? (ao-ref-ref r) lhs)))]
                            [else #f]))
                        1000))
  (define (mark-all-reaching-defs! g)
    (walk-aliased-vdefs st0 m #f (stmt-vuse g)
                        (lambda (vdef)
                          (define dg (mfn-stmt m (mfn-def m vdef)))
                          (cond
                            [(and (gassign? dg) (gassign-single-p st0 dg) (not (ref-may-be-aliased? st0 (gassign-lhs dg)))) #f]
                            [else (unless (clobber? dg) (mark-operand-necessary! vdef)) #f]))
                        1000))
  (define (memory-operand? t)
    (and t (not (ssa-name? t)) (not (is-gimple-min-invariant st0 t)) (not (eq? (tree-code t) 'constructor))))
  (let loop ()
    (unless (null? worklist)
      (define id (car worklist))
      (set! worklist (cdr worklist))
      (define g (mfn-stmt m id))
      (cond
        [(and (gphi? g) (not (ssa-virtual? (gphi-result g))))
         (for ([a (in-list (phi-ssa-args g))]) (mark-operand-necessary! (cdr a)))]
        [(gphi? g) (void)]
        [else
         (for ([u (in-list (stmt-real-uses g))]) (mark-operand-necessary! u))
         (when (stmt-vuse g)
           (cond
             [(gcall? g)
              (for ([a (in-pvector (gcall-args g))] #:when (memory-operand? a))
                (unless (ref-may-be-aliased? st0 a) (mark-aliased-reaching-defs! g a)))
              (mark-all-reaching-defs! g)]
             [(and (gassign? g) (gassign-single-p st0 g))
              (define rhs (gassign-rhs1 g))
              (when (memory-operand? rhs)
                (if (ref-may-be-aliased? st0 rhs) (mark-all-reaching-defs! g) (mark-aliased-reaching-defs! g rhs)))]
             [(greturn? g)
              (define rhs (greturn-retval g))
              (when (memory-operand? rhs)
                (if (ref-may-be-aliased? st0 rhs) (mark-all-reaching-defs! g) (mark-aliased-reaching-defs! g rhs)))]
             [else (void)]))])
      (loop)))

  ;; eliminate_unnecessary_stmts: blocks dominated by the entry block,
  ;; last pushed first (a post-order of the dominator tree)
  (define something-changed #f)
  (define blocks
    (let walk ([idx (dom-index dom (GEdge-dest (pvector-first (cfg-succs cfg ENTRY))))] [acc '()])
      (define bb (dom-node dom idx))
      (for/fold ([acc (cons bb acc)]) ([c (in-pvector (dom-children dom idx))]) (walk c acc))))
  (for ([bb (in-list blocks)] #:unless (= bb EXIT))
    (let loop ([id (mfn-last m bb)])
      (when id
        (define prev (mfn-prev m id))
        (define g (mfn-stmt m id))
        (cond
          [(not (hash-ref necessary id #f))
           (define keep-clobber?
             (and (clobber? g)
                  (not (for/or ([u (in-list (stmt-real-uses g))])
                         (and (not (ssa-default-def? u)) (not (hash-ref processed (ssa-name-version u) #f)))))))
           (unless keep-clobber?
             (set! something-changed #t)
             (mfn-unlink-vdef! m id)
             (mfn-remove! m id))]
          [(and (gcall? g) (ssa-name? (gcall-lhs g)) (not (hash-ref processed (ssa-name-version (gcall-lhs g)) #f)))
           (set! something-changed #t)
           (mfn-replace! m id (gcall (gcall-loc g) (gcall-flags g) #f (gcall-fn g) (gcall-args g) (gcall-fntype g) (gcall-internal? g) (gcall-ecf g)))]
          [else (void)])
        (loop prev)))
    ;; remove_dead_phis
    (for ([pid (in-list (mfn-phis m bb))])
      (define ph (mfn-stmt m pid))
      (define res (gphi-result ph))
      (define nec?
        (cond
          [(ssa-virtual? res)
           (define args (map cdr (pvector->list (gphi-args ph))))
           (cond
             [(for/and ([a (in-list (cdr args))]) (equal? a (car args)))
              ;; degenerate: its uses take the argument
              (define vuse (car args))
              (for ([uid (in-list (mfn-use-ids m res))])
                (define ug (mfn-stmt m uid))
                (define (f t) (if (and (ssa-name? t) (ssa-name=? t res)) vuse t))
                (mfn-replace! m uid
                              (cond
                                [(gphi? ug) (phi-set-args ug (for/pvector ([a (in-pvector (gphi-args ug))]) (cons (car a) (f (cdr a)))))]
                                [(stmt-vuse ug) (stmt-set-flag ug 'vuse (f (stmt-vuse ug)))]
                                [else ug])))
              (hash-ref necessary pid #f)]
             [else #t])]
          [else (hash-ref necessary pid #f)]))
      (unless nec?
        (set! something-changed #t)
        (mfn-remove! m pid))))
  (define cfg1 (mfn->gcfg m))
  (if something-changed (cleanup-tree-cfg st0 cfg1 print-cond-rhs) (values cfg1 st0)))
