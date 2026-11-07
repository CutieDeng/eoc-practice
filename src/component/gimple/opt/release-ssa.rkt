#lang racket/base

;; ============================================================
;; opt: pass_strip_predict_hints (early) + pass_release_ssa_names,
;; dump "release_ssa"
;; ============================================================
;;
;; strip_predict_hints (early_p): GIMPLE_PREDICT statements of
;; PRED_TREE_EARLY_RETURN are removed (the other predictors stay until
;; the late instance), TODO_cleanup_cfg when anything went.  The pass
;; has no dump file ("*strip_predict_hints").
;; release_free_names_and_compact_live_names: the SSA version space is
;; compacted keeping the relative order ("Released N names, P%, removed
;; H holes": N = the free list, P = N / the compacted count, H = the
;; empty slots).  GCC reuses released versions for later names
;; (make_ssa_name_fn pops the free list); the port's versions are never
;; reused, so N and H are the created-minus-live difference.
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         "../cfg.rkt"
         "../ssa.rkt"
         "../print.rkt"
         "ssa-ops.rkt")

(provide release-ssa)

(define (strip-predict-hints st cfg)
  (define changed #f)
  (define cfg1
    (for/fold ([c cfg]) ([bb (in-pvector (cfg-bbs cfg))])
      (define b (cfg-block c bb))
      (define stmts
        (for/pvector ([g (in-pvector (GBlock-stmts b))]
                      #:unless (and (gpredict? g) (eq? (gpredict-predictor g) 'PRED_TREE_EARLY_RETURN) (begin (set! changed #t) #t)))
          g))
      (if (= (pvector-length stmts) (pvector-length (GBlock-stmts b))) c (set-block c (block-set-stmts b stmts)))))
  (if changed (cleanup-tree-cfg st cfg1 print-cond-rhs) (values cfg1 st)))

(define (release-ssa st fndecl cfg0)
  (define st0 (gs-set-fndecl st fndecl))
  (define-values (cfg1 st1) (strip-predict-hints st0 cfg0))
  (define cfg (cfg-clear-messages cfg1))
  ;; default definitions stay allocated after their last use
  (define live
    (let* ([in-fn (ssa-names-in-version-order cfg)]
           [seen (for/fold ([s intmap-empty]) ([n (in-pvector in-fn)]) (intmap-set s (ssa-name-version n) n))]
           [all (for/fold ([s seen]) ([p (in-list (decl-flag st1 fndecl 'default-defs '()))])
                  (if (intmap-ref s (ssa-name-version (cdr p)) #f) s (intmap-set s (ssa-name-version (cdr p)) (cdr p))))])
      (for/pvector ([p (in-list (intmap->literal-datum all))]) (cdr p))))
  (define n-live (pvector-length live))
  (define created (GS-ssa-version st1))
  (define holes (- created n-live))
  (define pct (/ (* holes 100) (add1 n-live)))
  (define remap
    (for/fold ([m intmap-empty]) ([name (in-pvector live)] [j (in-naturals 1)])
      (intmap-set m (ssa-name-version name) j)))
  (define (rename-tree t)
    (cond
      [(not t) #f]
      [(eq? (tree-code t) 'ssa_name) (tree-set-attr t 'version (intmap-ref remap (ssa-name-version t)))]
      [(pvector-empty? (tree-ops t)) t]
      [else (tree-set-ops t (for/pvector ([o (in-pvector (tree-ops t))]) (rename-tree o)))]))
  (define cfg2
    (for/fold ([c cfg]) ([bb (in-pvector (cfg-bbs cfg))])
      (define b (cfg-block c bb))
      (set-block c (block-set-stmts
                    (block-set-phis b (for/pvector ([ph (in-pvector (GBlock-phis b))])
                                        (gphi (gphi-loc ph) (gphi-flags ph) (gphi-var ph) (rename-tree (gphi-result ph))
                                              (for/pvector ([a (in-pvector (gphi-args ph))]) (cons (car a) (rename-tree (cdr a)))))))
                    (for/pvector ([g (in-pvector (GBlock-stmts b))]) (map-stmt-trees g rename-tree))))))
  (define dd (for/list ([p (in-list (decl-flag st1 fndecl 'default-defs '()))])
               (cons (car p) (rename-tree (cdr p)))))
  (define st2 (gs-set-ssa-version (set-decl-flag st1 fndecl 'default-defs dd) n-live))
  (values (cfg-add-message cfg2 (format "Released ~a names, ~a%, removed ~a holes" holes (real->decimal-string pct 2) holes)) st2))
