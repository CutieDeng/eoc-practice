#lang racket/base

;; ============================================================
;; ssa: pass_build_ssa (gcc/tree-into-ssa.cc) over GCfg, pure
;; ============================================================
;;
;; The GIMPLE port of the generic SSA driver (driver/ssa/construct.rkt):
;;   - variables: the is_gimple_reg declarations (VAR/PARM/RESULT_DECL
;;     that are scalar, not addressable, not volatile) mentioned by the
;;     function, in DECL_UID order (insert_phi_nodes sorts by uid);
;;   - defs: the lhs of an assignment or call when it is such a decl;
;;     uses: every such decl inside an operand tree (lhs memory
;;     references included);
;;   - names: make_ssa_name on the decl; default definitions carry the
;;     'default-def flag (printed x_N(D));
;;   - after renaming, SSA names of DECL_IGNORED_P variables are made
;;     anonymous (SSA_NAME_VAR cleared, identifier kept), as
;;     pass_build_ssa::execute does.
;; Policies: pruned, children in RPO, PHI results named at insertion.
;;
;; (build-ssa st fndecl cfg) -> (values cfg st)
;; ============================================================

(require racket/list
         "../../kernel/data/data.rkt"
         "../../kernel/ir/gimple/types.rkt"
         "../../driver/ssa/construct.rkt"
         "state.rkt"
         "tree.rkt"
         "gimple.rkt"
         "gimplify.rkt"
         "cfg.rkt")

(provide build-ssa ssa-names-in-version-order)

;; adapter value
(struct ga (st cfg))

(define (reg-decl? st t)
  (and t (memq (tree-code t) '(var_decl parm_decl result_decl)) (is-gimple-reg st t)))

;; every is_gimple_reg decl occurrence inside an operand tree, in operand order
(define (tree-reg-uses st t)
  (cond
    [(not t) '()]
    [(reg-decl? st t) (list t)]
    [(eq? (tree-code t) 'ssa_name) '()]
    [else (append* (for/list ([o (in-pvector (tree-ops t))]) (tree-reg-uses st o)))]))

;; replace reg decl occurrences through F : decl -> name
(define (subst-reg-uses st t f)
  (cond
    [(not t) #f]
    [(reg-decl? st t) (f t)]
    [(eq? (tree-code t) 'ssa_name) t]
    [(pvector-empty? (tree-ops t)) t]
    [else (tree-set-ops t (for/pvector ([o (in-pvector (tree-ops t))]) (subst-reg-uses st o f)))]))

;; the statement's definition (a reg decl) or #f
(define (stmt-def st g)
  (cond
    [(gassign? g) (and (reg-decl? st (gassign-lhs g)) (gassign-lhs g))]
    [(gcall? g) (and (reg-decl? st (gcall-lhs g)) (gcall-lhs g))]
    [else #f]))

;; operand trees that may contain uses, in GCC's operand-scan order
;; (a memory lhs first, then the rhs operands)
(define (stmt-use-trees st g)
  (cond
    [(gassign? g) (append (if (stmt-def st g) '() (list (gassign-lhs g)))
                          (list (gassign-rhs1 g) (gassign-rhs2 g) (gassign-rhs3 g)))]
    [(gcall? g) (append (if (or (not (gcall-lhs g)) (stmt-def st g)) '() (list (gcall-lhs g)))
                        (cons (gcall-fn g) (pvector->list (gcall-args g))))]
    [(gcond? g) (list (gcond-lhs g) (gcond-rhs g))]
    [(gswitch? g) (list (gswitch-index g))]
    [(greturn? g) (list (greturn-retval g))]
    [else '()]))

(define (stmt-uses st g) (append* (for/list ([t (in-list (stmt-use-trees st g))]) (tree-reg-uses st t))))

;; rewrite the statement: uses through USE-F, the def (if any) to DEF-NAME
(define (rewrite-stmt st g use-f def-name)
  (define (u t) (subst-reg-uses st t use-f))
  (cond
    [(gassign? g)
     (gassign (gassign-loc g) (gassign-flags g) (or def-name (u (gassign-lhs g))) (gassign-code g)
              (u (gassign-rhs1 g)) (u (gassign-rhs2 g)) (u (gassign-rhs3 g)))]
    [(gcall? g)
     (gcall (gcall-loc g) (gcall-flags g) (or def-name (u (gcall-lhs g))) (u (gcall-fn g))
            (for/pvector ([x (in-pvector (gcall-args g))]) (u x)) (gcall-fntype g) (gcall-internal? g) (gcall-ecf g))]
    [(gcond? g) (gcond (gcond-loc g) (gcond-flags g) (gcond-code g) (u (gcond-lhs g)) (u (gcond-rhs g))
                       (gcond-true-label g) (gcond-false-label g))]
    [(gswitch? g) (gswitch (gswitch-loc g) (gswitch-flags g) (u (gswitch-index g)) (gswitch-default-case g) (gswitch-cases g))]
    [(greturn? g) (greturn (greturn-loc g) (greturn-flags g) (u (greturn-retval g)))]
    [else g]))

;; ------------------------------------------------------------
;; the port
;; ------------------------------------------------------------

(define (block-stmts a i) (GBlock-stmts (cfg-block (ga-cfg a) i)))
(define (block-phis a i) (GBlock-phis (cfg-block (ga-cfg a) i)))
(define (update-block a i f) (ga (ga-st a) (set-block (ga-cfg a) (f (cfg-block (ga-cfg a) i)))))

(define (variables a)
  ;; every reg decl defined or used anywhere, by DECL_UID
  (define st (ga-st a))
  (define found
    (for*/fold ([m intmap-empty]) ([i (in-pvector (cfg-bbs (ga-cfg a)))] [g (in-pvector (block-stmts a i))])
      (define d (stmt-def st g))
      (for/fold ([m (if d (intmap-set m (decl-uid d) d) m)]) ([u (in-list (stmt-uses st g))])
        (intmap-set m (decl-uid u) u))))
  (for/pvector ([p (in-list (intmap->literal-datum found))]) (cdr p)))

;; kills and upward-exposed uses of a block (mark_def_sites)
(define (block-summary a i)
  (define st (ga-st a))
  (define-values (kills ups _seen)
    (for/fold ([kills (pvector-empty)] [ups (pvector-empty)] [killed intbits-empty]) ([g (in-pvector (block-stmts a i))])
      (define ups*
        (for/fold ([ups ups]) ([u (in-list (stmt-uses st g))])
          (if (or (intbits-ref killed (decl-uid u)) (pvector-index-of ups u)) ups (pvector-cons-right ups u))))
      (define d (stmt-def st g))
      (cond
        [(and d (not (intbits-ref killed (decl-uid d))))
         (values (pvector-cons-right kills d) ups* (intbits-set killed (decl-uid d)))]
        [else (values kills ups* killed)])))
  (values kills ups))

(define (new-name a var kind)
  (define-values (name st) (make-ssa-name (ga-st a) var))
  (values (if (eq? kind 'default) (tree-set-attr name 'default-def #t) name) (ga st (ga-cfg a))))

(define (insert-phi a i var name)
  (define args (for/pvector ([p (in-pvector (cfg-preds (ga-cfg a) i))]) (cons p #f)))
  (update-block a i (lambda (b) (block-set-phis b (pvector-cons-right (GBlock-phis b) (gphi #f (hasheq) var name args))))))

(define (phis a i) (for/pvector ([ph (in-pvector (block-phis a i))]) (cons (gphi-var ph) (gphi-result ph))))

(define (update-phi a i var f)
  (update-block a i (lambda (b)
                      (block-set-phis b (for/pvector ([ph (in-pvector (GBlock-phis b))])
                                          (if (decl-eq? (gphi-var ph) var) (f ph) ph))))))

(define (set-phi-result a i var name)
  (update-phi a i var (lambda (ph) (gphi (gphi-loc ph) (gphi-flags ph) (gphi-var ph) name (gphi-args ph)))))

(define (set-phi-arg a i var pred name)
  (update-phi a i var (lambda (ph)
                        (gphi (gphi-loc ph) (gphi-flags ph) (gphi-var ph) (gphi-result ph)
                              (for/pvector ([arg (in-pvector (gphi-args ph))])
                                (if (= (car arg) pred) (cons pred name) arg))))))

;; rewrite_stmt for every statement of the block
(define (rename-stmts a i acc use def)
  (define-values (stmts* a* acc*)
    (for/fold ([out (pvector-empty)] [a a] [acc acc] #:result (values out a acc)) ([g (in-pvector (block-stmts a i))])
      (define st (ga-st a))
      ;; uses, in operand order, each resolved once
      (define-values (map1 a1 acc1)
        (for/fold ([m intmap-empty] [a a] [acc acc]) ([u (in-list (stmt-uses st g))])
          (cond
            [(intmap-has-key? m (decl-uid u)) (values m a acc)]
            [else (let-values ([(n a acc) (use a acc u)]) (values (intmap-set m (decl-uid u) n) a acc))])))
      (define d (stmt-def st g))
      (define-values (dn a2 acc2) (if d (def a1 acc1 d) (values #f a1 acc1)))
      (values (pvector-cons-right out (rewrite-stmt st g (lambda (t) (intmap-ref map1 (decl-uid t))) dn)) a2 acc2)))
  (values (update-block a* i (lambda (b) (block-set-stmts b stmts*))) acc*))

;; pass_build_ssa::execute's tail: SSA names of DECL_IGNORED_P variables
;; become anonymous (the identifier survives: iftmp.0_5, _23)
(define (anonymize st name)
  (define v (ssa-name-var name))
  (cond
    [(and v (not (tree-attr name 'default-def #f)) (var-p v) (decl-flag st v 'ignored))
     (tree-set-attr (tree-set-attr name 'var #f) 'identifier (decl-name v))]
    [else name]))

(define (anonymize-tree st t)
  (cond
    [(not t) #f]
    [(eq? (tree-code t) 'ssa_name) (anonymize st t)]
    [(pvector-empty? (tree-ops t)) t]
    [else (tree-set-ops t (for/pvector ([o (in-pvector (tree-ops t))]) (anonymize-tree st o)))]))

(define (anonymize-cfg st cfg)
  (define (an t) (anonymize-tree st t))
  (for/fold ([c cfg]) ([i (in-pvector (cfg-bbs cfg))])
    (define b (cfg-block c i))
    (define phis* (for/pvector ([ph (in-pvector (GBlock-phis b))])
                    (gphi (gphi-loc ph) (gphi-flags ph) (gphi-var ph) (an (gphi-result ph))
                          (for/pvector ([arg (in-pvector (gphi-args ph))]) (cons (car arg) (an (cdr arg)))))))
    (define stmts* (for/pvector ([g (in-pvector (GBlock-stmts b))]) (rewrite-stmt st g an #f)))
    (set-block c (block-set-stmts (block-set-phis b phis*) stmts*))))

(define (finish a) (ga (ga-st a) (anonymize-cfg (ga-st a) (ga-cfg a))))

(define gimple-port
  (ssa-port (lambda (a) ENTRY)
            (lambda (a) (pvector-cons-left (pvector-cons-left (cfg-bbs (ga-cfg a)) EXIT) ENTRY))
            (lambda (a i) (for/pvector ([e (in-pvector (cfg-succs (ga-cfg a) i))]) (GEdge-dest e)))
            (lambda (a i) (cfg-preds (ga-cfg a) i))
            values
            variables
            decl-uid
            block-summary
            (lambda (a) (pvector-empty))
            new-name insert-phi phis set-phi-result set-phi-arg rename-stmts finish
            #t 'rpo 'insert))

(define (build-ssa st fndecl cfg)
  (define a (ssa-construct gimple-port (ga (gs-set-fndecl st fndecl) cfg)))
  (values (ga-cfg a) (ga-st a)))

;; all SSA names of the function in version order (for the dump's declaration list)
(define (ssa-names-in-version-order cfg)
  (define (collect-tree t acc)
    (cond
      [(not t) acc]
      [(eq? (tree-code t) 'ssa_name) (intmap-set acc (ssa-name-version t) t)]
      [else (for/fold ([acc acc]) ([o (in-pvector (tree-ops t))]) (collect-tree o acc))]))
  (define (collect-stmt g acc)
    (cond
      [(gassign? g) (for/fold ([acc acc]) ([t (list (gassign-lhs g) (gassign-rhs1 g) (gassign-rhs2 g) (gassign-rhs3 g))]) (collect-tree t acc))]
      [(gcall? g) (for/fold ([acc acc]) ([t (append (list (gcall-lhs g) (gcall-fn g)) (pvector->list (gcall-args g)))]) (collect-tree t acc))]
      [(gcond? g) (collect-tree (gcond-rhs g) (collect-tree (gcond-lhs g) acc))]
      [(gswitch? g) (collect-tree (gswitch-index g) acc)]
      [(greturn? g) (collect-tree (greturn-retval g) acc)]
      [else acc]))
  (define m
    (for*/fold ([acc intmap-empty]) ([i (in-pvector (cfg-bbs cfg))])
      (define b (cfg-block cfg i))
      (define acc1 (for/fold ([acc acc]) ([ph (in-pvector (GBlock-phis b))])
                     (for/fold ([acc (collect-tree (gphi-result ph) acc)]) ([arg (in-pvector (gphi-args ph))])
                       (collect-tree (cdr arg) acc))))
      (for/fold ([acc acc1]) ([g (in-pvector (GBlock-stmts b))]) (collect-stmt g acc))))
  (for/pvector ([p (in-list (intmap->literal-datum m))]) (cdr p)))
