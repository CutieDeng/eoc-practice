#lang racket/base

;; ============================================================
;; opt: pass_dse (gcc/tree-ssa-dse.cc), first instance "dse1"
;; ============================================================
;;
;; Blocks in reverse RPO, statements last to first:
;;   - a statement with a VDEF: dse_optimize_stmt -> dse_classify_store
;;     walks the uses of the store's virtual definition (through PHIs);
;;     a use that may read the stored bytes keeps the store live, a
;;     later store covering it (stmt_kills_ref_p) kills it, a VDEF
;;     without uses whose memory is not global ends the walk dead;
;;     dead stores are removed (clobbers only when killed by a real
;;     store), dead calls with a VDEF lose their lhs;
;;   - a statement with one SSA definition without uses and without
;;     side effects is removed;
;;   - PHIs without uses are removed.
;; TODO_cleanup_cfg when a block lost all its PHIs.
;;
;; Not ported: byte tracking / partial store trimming
;; (live_bytes), memset/memcpy/calloc built-ins, redundant zero
;; stores (dse_optimize_redundant_stores), data-reference
;; disambiguation, the back-edge index check of dse_classify_store.
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

(provide dse)

;; initialize_ao_ref_for_dse (may_def_ok): the written reference or #f
(define (store-ref st g)
  (define lhs (stmt-get-lhs g))
  (and lhs (not (ssa-name? lhs)) (make-ao-ref st lhs)))

;; dse_classify_store: 'dead or 'live (no byte tracking); (values status by-clobber?)
(define (classify-store st m dom r id)
  (define limit 256)           ; param_dse_max_alias_queries_per_store
  (define cnt 0)
  (define visited (make-hasheqv))
  (define (vdef-of id) (let ([g (mfn-stmt m id)]) (if (gphi? g) (gphi-result g) (stmt-vdef g))))
  (define (dominates? a b) (dom-dominates? dom (dom-index dom a) (dom-index dom b)))
  (let loop ([temp id] [by-clobber #t])
    (define defvar (vdef-of temp))
    (when (gphi? (mfn-stmt m temp)) (hash-set! visited (ssa-name-version defvar) #t))
    ;; the uses of the definition
    (define-values (defs fail?)
      (let scan ([work (list defvar)] [defs '()])
        (cond
          [(null? work) (values (reverse defs) #f)]
          [else
           (define dv (car work))
           (define-values (defs1 fail? more)
             (for/fold ([defs defs] [fail? #f] [more '()]) ([uid (in-list (mfn-use-ids m dv))])
               #:break fail?
               (set! cnt (add1 cnt))
               (define ug (mfn-stmt m uid))
               (cond
                 [(> cnt limit) (values defs #t more)]
                 [(gphi? ug)
                  (cond
                    [(= 1 (pvector-length (gphi-args ug))) (values defs #f (cons (gphi-result ug) more))]
                    [(hash-ref visited (ssa-name-version (gphi-result ug)) #f) (values defs #f more)]
                    [else (values (cons uid defs) #f more)])]
                 [(ref-maybe-used-by-stmt? st ug r) (values defs #t more)]
                 [(eqv? uid id) (values defs #f more)]
                 [(stmt-vdef ug) (values (cons uid defs) #f more)]
                 [else (values defs #f more)])))
           (if fail? (values defs1 #t) (scan (append (cdr work) more) defs1))])))
    (cond
      [fail? (values 'live by-clobber)]
      [(null? defs)
       (if (ref-may-alias-global? st r) (values 'live by-clobber) (values 'dead #f))]
      [else
       ;; definitions that kill the store, or whose vdef is unused, are done
       (define-values (rest by-clobber1)
         (for/fold ([rest '()] [bc by-clobber]) ([d (in-list defs)])
           (define dg (mfn-stmt m d))
           (define dv (vdef-of d))
           (cond
             [(and (not (gphi? dg)) (stmt-kills-ref? st dg r))
              (values rest (and bc (tree-clobber-p (gassign-rhs1* dg))))]
             [(mfn-zero-uses? m dv) (values rest bc)]
             [else (values (cons d rest) bc)])))
       (define rest1 (reverse rest))
       (cond
         [(and (not (null? rest)) (for/or ([d (in-list rest)]) (and (mfn-zero-uses? m (vdef-of d)) (ref-may-alias-global? st r))))
          (values 'live by-clobber1)]
         [(null? rest1) (values 'dead by-clobber1)]
         [(> (length rest1) 1) (values 'live by-clobber1)]
         [else (loop (car rest1) by-clobber1)])])))

(define (gassign-rhs1* g) (if (gassign? g) (gassign-rhs1 g) #f))

(define (dse st fndecl cfg0)
  (define st0 (gs-set-fndecl st fndecl))
  (define cfg (cfg-clear-messages cfg0))
  (define m (gcfg->mfn cfg))
  (define dom (compute-dom (pvector-cons-left (pvector-cons-left (cfg-bbs cfg) EXIT) ENTRY) ENTRY
                           (lambda (i) (for/pvector ([e (in-pvector (cfg-succs cfg i))]) (GEdge-dest e)))))
  (define rpo (for/list ([b (in-pvector (dom-rpo dom))] #:unless (or (= b ENTRY) (= b EXIT))) b))
  (define cfg-changed #f)
  (for ([bb (in-list (reverse rpo))])
    (let loop ([id (mfn-last m bb)])
      (when id
        (define prev (mfn-prev m id))
        (define g (mfn-stmt m id))
        (cond
          [(stmt-vdef g)
           ;; dse_optimize_stmt
           (define r (and (not (and (stmt-has-volatile-ops? g) (not (and (gassign? g) (tree-clobber-p (gassign-rhs1 g))
                                                                            (eq? (tree-code (gassign-lhs g)) 'mem_ref)))))
                          (store-ref st0 g)))
           (when r
             (define self-copy? (and (gassign? g) (equal? (gassign-rhs1 g) (gassign-lhs g))))
             (define-values (status by-clobber)
               (if self-copy? (values 'dead #f) (classify-store st0 m dom r id)))
             (when (eq? status 'dead)
               (cond
                 [(and (gassign? g) (tree-clobber-p (gassign-rhs1 g)) (not by-clobber)) (void)]
                 [(and (gcall? g) (stmt-has-side-effects? st0 g))
                  (mfn-replace! m id (gcall (gcall-loc g) (gcall-flags g) #f (gcall-fn g) (gcall-args g) (gcall-fntype g) (gcall-internal? g) (gcall-ecf g)))]
                 [else
                  (mfn-unlink-vdef! m id)
                  (mfn-remove! m id)])))]
          [else
           (define defs (stmt-def-names g))
           (when (and (= 1 (length defs)) (mfn-zero-uses? m (car defs))
                      (not (stmt-has-side-effects? st0 g)) (not (stmt-ends-bb? g)))
             (mfn-remove! m id))])
        (loop prev)))
    ;; PHIs without uses
    (define removed?
      (for/fold ([removed? #f]) ([pid (in-list (mfn-phis m bb))])
        (cond
          [(mfn-zero-uses? m (gphi-result (mfn-stmt m pid))) (mfn-remove! m pid) #t]
          [else removed?])))
    (when (and removed? (null? (mfn-phis m bb))) (set! cfg-changed #t)))
  (define cfg1 (mfn->gcfg m))
  (if cfg-changed (cleanup-tree-cfg st0 cfg1 print-cond-rhs) (values cfg1 st0)))
