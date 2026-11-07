#lang racket/base

;; ============================================================
;; ssa: pass_build_ssa (gcc/tree-into-ssa.cc) over GCfg, pure
;; ============================================================
;;
;; The GIMPLE port of the generic SSA driver (driver/ssa/construct.rkt):
;;   - variables: the is_gimple_reg declarations (VAR/PARM/RESULT_DECL
;;     that are scalar, not addressable, not volatile) mentioned by the
;;     function, in DECL_UID order (insert_phi_nodes sorts by uid), plus
;;     the virtual operand `.MEM` (gimple_vop) last;
;;   - real defs: the lhs of an assignment or call when it is such a
;;     decl; real uses: every such decl inside an operand tree;
;;   - virtual operands (tree-ssa-operands.cc): a statement that reads
;;     memory (a memory reference or a non-register object outside an
;;     ADDR_EXPR, every GIMPLE_RETURN, a pure call) has a VUSE; one that
;;     writes memory (memory lhs, a call that is neither const nor pure,
;;     a clobber) has a VDEF (which also uses the old state).  The
;;     renamer stores the names in the statement flags 'vuse / 'vdef;
;;     the order of allocation is uses, then the VDEF, then the real def
;;     (that is the version order GCC produces);
;;   - names: make_ssa_name on the decl; default definitions carry the
;;     'default-def flag (printed x_N(D));
;;   - after renaming, SSA names of DECL_IGNORED_P variables are made
;;     anonymous (SSA_NAME_VAR cleared, identifier kept), as
;;     pass_build_ssa::execute does; `.MEM` keeps its variable.
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

(provide build-ssa update-ssa-vars ssa-names-in-version-order virtual-operand? stmt-vuse stmt-vdef remove-unused-locals map-stmt-trees)

;; adapter value
(struct ga (st cfg vop stamp [only #:auto #:mutable]) #:auto-value #f)     ; vop: the .MEM declaration; stamp: next use-list stamp; only: uids of the variables to rename (update_ssa) or #f
(define (ga-only? a d) (or (not (ga-only a)) (intbits-ref (ga-only a) (decl-uid d))))
(define (ga* st cfg vop stamp only) (let ([a (ga st cfg vop stamp)]) (set-ga-only! a only) a))

(define (reg-decl? st t)
  (and t (memq (tree-code t) '(var_decl parm_decl result_decl)) (is-gimple-reg st t)))

(define (virtual-operand? d) (and d (tree-attr d 'virtual #f) #t))
(define (stmt-vuse g) (stmt-flag g 'vuse #f))
(define (stmt-vdef g) (stmt-flag g 'vdef #f))

;; ---- real operands ----------------------------------------------------

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

;; the statement's real definition (a reg decl) or #f
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

;; ---- virtual operands ---------------------------------------------------

;; does T access memory (get_expr_operands adds a virtual operand)?
;; Objects under an ADDR_EXPR are not accessed.
(define (tree-touches-memory? st t)
  (cond
    [(not t) #f]
    [else
     (case (tree-code t)
       [(ssa_name integer_cst real_cst addr_expr) #f]
       [(var_decl parm_decl result_decl) (not (is-gimple-reg st t))]
       [(string_cst const_decl) #t]
       [(mem_ref target_mem_ref array_ref array_range_ref component_ref realpart_expr imagpart_expr bit_field_ref) #t]
       [else (for/or ([o (in-pvector (tree-ops t))]) (tree-touches-memory? st o))])]))

;; (values vuse? vdef?) for a statement (parse_ssa_operands / maybe_add_call_vops)
(define (stmt-vops st g)
  (cond
    [(gassign? g)
     (define lhs-mem (and (not (stmt-def st g)) (tree-touches-memory? st (gassign-lhs g))))
     (define rhs-mem (for/or ([t (list (gassign-rhs1 g) (gassign-rhs2 g) (gassign-rhs3 g))]) (tree-touches-memory? st t)))
     (values (or lhs-mem rhs-mem) lhs-mem)]
    [(gcall? g)
     (define ecf (gcall-ecf g))
     (define const? (flag? ecf ECF-CONST))
     (define pure? (flag? ecf ECF-PURE))
     (define call-vdef (not (or const? pure?)))
     (define lhs-mem (and (gcall-lhs g) (not (stmt-def st g)) (tree-touches-memory? st (gcall-lhs g))))
     (define args-mem (for/or ([t (in-pvector (gcall-args g))]) (tree-touches-memory? st t)))
     (values (or call-vdef pure? lhs-mem args-mem) (or call-vdef lhs-mem))]
    [(greturn? g) (values #t #f)]
    [else (values #f #f)]))

(define (flag? bits b) (not (zero? (bitwise-and bits b))))

;; rewrite the statement: real uses through USE-F, the real def (if any)
;; to DEF-NAME, the virtual operands into the flags
(define (rewrite-stmt st g use-f def-name vuse vdef)
  (define (u t) (subst-reg-uses st t use-f))
  (define (vops g*)
    (let* ([g* (if vuse (stmt-set-flag g* 'vuse vuse) g*)]
           [g* (if vdef (stmt-set-flag g* 'vdef vdef) g*)])
      g*))
  (vops
   (cond
     [(gassign? g)
      (define rhs1 (u (gassign-rhs1 g)))
      ;; gimple_assign_rhs_code: for a GIMPLE_SINGLE_RHS the code is TREE_CODE (rhs1)
      (define code (if (eq? (gimple-rhs-class st (gassign-code g)) 'single) (tree-code rhs1) (gassign-code g)))
      (gassign (gassign-loc g) (gassign-flags g) (or def-name (u (gassign-lhs g))) code
               rhs1 (u (gassign-rhs2 g)) (u (gassign-rhs3 g)))]
     [(gcall? g)
      (gcall (gcall-loc g) (gcall-flags g) (or def-name (u (gcall-lhs g))) (u (gcall-fn g))
             (for/pvector ([x (in-pvector (gcall-args g))]) (u x)) (gcall-fntype g) (gcall-internal? g) (gcall-ecf g))]
     [(gcond? g) (gcond (gcond-loc g) (gcond-flags g) (gcond-code g) (u (gcond-lhs g)) (u (gcond-rhs g))
                        (gcond-true-label g) (gcond-false-label g))]
     [(gswitch? g) (gswitch (gswitch-loc g) (gswitch-flags g) (u (gswitch-index g)) (gswitch-default-case g) (gswitch-cases g))]
     [(greturn? g) (greturn (greturn-loc g) (greturn-flags g) (u (greturn-retval g)))]
     [else g])))

;; ------------------------------------------------------------
;; the port
;; ------------------------------------------------------------

(define (block-stmts a i) (GBlock-stmts (cfg-block (ga-cfg a) i)))
(define (block-phis a i) (GBlock-phis (cfg-block (ga-cfg a) i)))
(define (update-block a i f) (ga* (ga-st a) (set-block (ga-cfg a) (f (cfg-block (ga-cfg a) i))) (ga-vop a) (ga-stamp a) (ga-only a)))
(define (ga-set-stamp a n) (ga* (ga-st a) (ga-cfg a) (ga-vop a) n (ga-only a)))

;; every reg decl defined or used anywhere, by DECL_UID; then .MEM when
;; any statement has a virtual operand
(define (variables a)
  (define st (ga-st a))
  (define-values (found any-vop)
    (for*/fold ([m intmap-empty] [any #f]) ([i (in-pvector (cfg-bbs (ga-cfg a)))] [g (in-pvector (block-stmts a i))])
      (define d (stmt-def st g))
      (define-values (vu vd) (stmt-vops st g))
      (values (for/fold ([m (if d (intmap-set m (decl-uid d) d) m)]) ([u (in-list (stmt-uses st g))])
                (intmap-set m (decl-uid u) u))
              (or any vu vd))))
  (define regs (for/pvector ([p (in-list (intmap->literal-datum found))] #:when (ga-only? a (cdr p))) (cdr p)))
  (if (and any-vop (not (ga-only a))) (pvector-cons-right regs (ga-vop a)) regs))

;; kills and upward-exposed uses of a block (mark_def_sites), virtual
;; operand included
(define (block-summary a i)
  (define st (ga-st a))
  (define vop (ga-vop a))
  (define-values (kills ups _seen)
    (for/fold ([kills (pvector-empty)] [ups (pvector-empty)] [killed intbits-empty]) ([g (in-pvector (block-stmts a i))])
      (define-values (vu vd) (if (ga-only a) (values #f #f) (stmt-vops st g)))
      (define uses (append (filter (lambda (u) (ga-only? a u)) (stmt-uses st g)) (if vu (list vop) '())))
      (define ups*
        (for/fold ([ups ups]) ([u (in-list uses)])
          (if (or (intbits-ref killed (decl-uid u)) (pvector-index-of ups u)) ups (pvector-cons-right ups u))))
      (define defs (append (if vd (list vop) '()) (let ([d (stmt-def st g)]) (if (and d (ga-only? a d)) (list d) '()))))
      (for/fold ([kills kills] [ups ups*] [killed killed]) ([d (in-list defs)])
        (if (intbits-ref killed (decl-uid d))
            (values kills ups killed)
            (values (pvector-cons-right kills d) ups (intbits-set killed (decl-uid d)))))))
  (values kills ups))

(define (new-name a var kind)
  (define-values (name st) (make-ssa-name (ga-st a) var))
  (values (if (eq? kind 'default) (tree-set-attr name 'default-def #t) name) (ga* st (ga-cfg a) (ga-vop a) (ga-stamp a) (ga-only a))))

(define (insert-phi a i var name)
  (define args (for/pvector ([p (in-pvector (cfg-preds (ga-cfg a) i))]) (cons p #f)))
  (define stamps (for/pvector ([p (in-pvector args)]) #f))
  (update-block a i (lambda (b) (block-set-phis b (pvector-cons-right (GBlock-phis b) (gphi #f (hasheq 'arg-stamps stamps) var name args))))))

(define (phis a i) (for/pvector ([ph (in-pvector (block-phis a i))] #:when (ga-only? a (gphi-var ph))) (cons (gphi-var ph) (gphi-result ph))))

(define (update-phi a i var f)
  (update-block a i (lambda (b)
                      (block-set-phis b (for/pvector ([ph (in-pvector (GBlock-phis b))])
                                          (if (decl-eq? (gphi-var ph) var) (f ph) ph))))))

(define (set-phi-result a i var name)
  (update-phi a i var (lambda (ph) (gphi (gphi-loc ph) (gphi-flags ph) (gphi-var ph) name (gphi-args ph)))))

(define (set-phi-arg a i var pred name)
  ;; add_phi_arg links the argument's use at the head of NAME's use list
  (define n (ga-stamp a))
  (ga-set-stamp
   (update-phi a i var (lambda (ph)
                         (define stamps (or (phi-arg-stamps ph) (for/pvector ([x (in-pvector (gphi-args ph))]) #f)))
                         (define k (for/first ([arg (in-pvector (gphi-args ph))] [j (in-naturals)] #:when (= (car arg) pred)) j))
                         (gphi (gphi-loc ph) (hash-set (gphi-flags ph) 'arg-stamps (if k (pvector-set stamps k n) stamps)) (gphi-var ph) (gphi-result ph)
                               (for/pvector ([arg (in-pvector (gphi-args ph))])
                                 (if (= (car arg) pred) (cons pred name) arg)))))
   (add1 n)))

;; rewrite_stmt for every statement of the block: real uses, VUSE, VDEF,
;; real def
(define (rename-stmts a i acc use def)
  (define vop (ga-vop a))
  (define-values (stmts* a* acc*)
    (for/fold ([out (pvector-empty)] [a a] [acc acc] #:result (values out a acc)) ([g (in-pvector (block-stmts a i))])
      (define st (ga-st a))
      (define-values (map1 a1 acc1)
        (for/fold ([m intmap-empty] [a a] [acc acc]) ([u (in-list (stmt-uses st g))] #:when (ga-only? a u))
          (cond
            [(intmap-has-key? m (decl-uid u)) (values m a acc)]
            [else (let-values ([(n a acc) (use a acc u)]) (values (intmap-set m (decl-uid u) n) a acc))])))
      (define-values (vu? vd?) (if (ga-only a) (values #f #f) (stmt-vops st g)))
      (define-values (vuse a2 acc2) (if vu? (use a1 acc1 vop) (values #f a1 acc1)))
      (define-values (vdef a3 acc3) (if vd? (def a2 acc2 vop) (values #f a2 acc2)))
      (define d (let ([d (stmt-def st g)]) (and d (ga-only? a d) d)))
      (define-values (dn a4 acc4) (if d (def a3 acc3 d) (values #f a3 acc3)))
      (cond
        [(and (ga-only a) (not d) (intmap-empty? map1))
         ;; update_ssa leaves untouched statements alone
         (values (pvector-cons-right out g) a4 acc4)]
        [else
         ;; rewrite_stmt relinks every use operand: the statement takes a fresh stamp
         (define g0 (rewrite-stmt st g (lambda (t) (if (intmap-has-key? map1 (decl-uid t)) (intmap-ref map1 (decl-uid t)) t)) dn
                                  (or vuse (and (ga-only a) (stmt-vuse g))) (or vdef (and (ga-only a) (stmt-vdef g)))))
         (define g* (stmt-set-flag g0 'stamp (ga-stamp a4)))
         (values (pvector-cons-right out g*) (ga-set-stamp a4 (add1 (ga-stamp a4))) acc4)])))
  (values (update-block a* i (lambda (b) (block-set-stmts b stmts*))) acc*))

;; pass_build_ssa::execute's tail: SSA names of DECL_IGNORED_P variables
;; become anonymous (the identifier survives: iftmp.0_5, _23); the
;; virtual operand keeps its variable
(define (anonymize st name)
  (define v (ssa-name-var name))
  (cond
    [(and v (not (tree-attr name 'default-def #f)) (var-p v) (not (virtual-operand? v)) (decl-flag st v 'ignored))
     (tree-set-attr (tree-set-attr name 'var #f) 'identifier (decl-name v))]
    [else name]))

(define (anonymize-tree st t)
  (cond
    [(not t) #f]
    [(eq? (tree-code t) 'ssa_name) (anonymize st t)]
    [(pvector-empty? (tree-ops t)) t]
    [else (tree-set-ops t (for/pvector ([o (in-pvector (tree-ops t))]) (anonymize-tree st o)))]))

;; apply F to every operand tree of a statement (and its virtual operands)
(define (map-stmt-trees g f)
  (define g*
    (cond
      [(gassign? g) (gassign (gassign-loc g) (gassign-flags g) (f (gassign-lhs g)) (gassign-code g)
                             (f (gassign-rhs1 g)) (f (gassign-rhs2 g)) (f (gassign-rhs3 g)))]
      [(gcall? g) (gcall (gcall-loc g) (gcall-flags g) (f (gcall-lhs g)) (f (gcall-fn g))
                         (for/pvector ([x (in-pvector (gcall-args g))]) (f x)) (gcall-fntype g) (gcall-internal? g) (gcall-ecf g))]
      [(gcond? g) (gcond (gcond-loc g) (gcond-flags g) (gcond-code g) (f (gcond-lhs g)) (f (gcond-rhs g))
                         (gcond-true-label g) (gcond-false-label g))]
      [(gswitch? g) (gswitch (gswitch-loc g) (gswitch-flags g) (f (gswitch-index g)) (gswitch-default-case g) (gswitch-cases g))]
      [(greturn? g) (greturn (greturn-loc g) (greturn-flags g) (f (greturn-retval g)))]
      [else g]))
  (let* ([g* (if (stmt-vuse g*) (stmt-set-flag g* 'vuse (f (stmt-vuse g*))) g*)]
         [g* (if (stmt-vdef g*) (stmt-set-flag g* 'vdef (f (stmt-vdef g*))) g*)])
    g*))

(define (anonymize-cfg st cfg)
  (define (an t) (anonymize-tree st t))
  (for/fold ([c cfg]) ([i (in-pvector (cfg-bbs cfg))])
    (define b (cfg-block c i))
    (define phis* (for/pvector ([ph (in-pvector (GBlock-phis b))])
                    (gphi (gphi-loc ph) (gphi-flags ph) (gphi-var ph) (an (gphi-result ph))
                          (for/pvector ([arg (in-pvector (gphi-args ph))]) (cons (car arg) (an (cdr arg)))))))
    (define stmts* (for/pvector ([g (in-pvector (GBlock-stmts b))]) (map-stmt-trees g an)))
    (set-block c (block-set-stmts (block-set-phis b phis*) stmts*))))

;; pass_build_ssa anonymizes the names of DECL_IGNORED_P variables; update_ssa does not
(define (finish a) (ga* (ga-st a) (if (ga-only a) (ga-cfg a) (anonymize-cfg (ga-st a) (ga-cfg a))) (ga-vop a) (ga-stamp a) (ga-only a)))

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

;; create_vop_var: the ".MEM" VAR_DECL (artificial, ignored, virtual);
;; its uid is fresh, so it sorts after every real declaration
(define (make-vop st)
  (let/st st ([uid (fresh-uid st)])
    (values (make-tree 'var_decl #:type (void-type st)
                       #:attrs (list (cons 'uid uid) (cons 'name ".MEM") (cons 'artificial #t) (cons 'ignored #t)
                                     (cons 'virtual #t) (cons 'used #t)))
            st)))

(define (build-ssa st fndecl cfg)
  (let/st st ([vop (make-vop (gs-set-fndecl st fndecl))])
    ;; a new pass, a new dump_file: the cfg pass's messages are not repeated
    (define a (ssa-construct gimple-port (ga st (cfg-clear-messages cfg) vop 1)))
    ;; ssa_default_def: the default definitions survive their last use
    (define dd
      (for/fold ([acc '()]) ([n (in-pvector (ssa-names-in-version-order (ga-cfg a)))])
        (if (and (tree-attr n 'default-def #f) (ssa-name-var n)) (cons (cons (decl-uid (ssa-name-var n)) n) acc) acc)))
    (values (ga-cfg a) (set-decl-flag (ga-st a) fndecl 'default-defs (reverse dd)))))

;; update_ssa for VARS (declarations with non-SSA definitions inserted
;; by inlining): only those variables are renamed, virtual operands and
;; existing SSA names stay; new names in dominator-walk order
(define (update-ssa-vars st fndecl cfg vars)
  (cond
    [(null? vars) (values cfg st)]
    [else
     (define only (for/fold ([s intbits-empty]) ([v (in-list vars)]) (intbits-set s (decl-uid v))))
     (define a (ssa-construct gimple-port (ga* (gs-set-fndecl st fndecl) cfg #f (cfg-next-stamp cfg) only)))
     (values (ga-cfg a) (ga-st a))]))

;; all SSA names of the function in version order (for the dump's
;; declaration list), virtual operands included
(define (ssa-names-in-version-order cfg)
  (define (collect-tree t acc)
    (cond
      [(not t) acc]
      [(eq? (tree-code t) 'ssa_name) (intmap-set acc (ssa-name-version t) t)]
      [else (for/fold ([acc acc]) ([o (in-pvector (tree-ops t))]) (collect-tree o acc))]))
  (define (collect-stmt g acc0)
    (define acc (collect-tree (stmt-vdef g) (collect-tree (stmt-vuse g) acc0)))
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

;; TODO_remove_unused_locals (tree-ssa-live.cc remove_unused_locals), the
;; part that affects the dumps: when optimizing, a local declaration
;; survives only if some statement or PHI mentions it, directly or as
;; the variable of an SSA name.  Nothing is removed at -O0.
(define (remove-unused-locals st cfg locals)
  (cond
    [(not (optimize? st)) locals]
    [else
     (define (mark t acc)
       (cond
         [(not t) acc]
         [(eq? (tree-code t) 'ssa_name) (let ([v (ssa-name-var t)]) (if v (intbits-set acc (decl-uid v)) acc))]
         [(tree-attr t 'uid #f) (intbits-set acc (tree-attr t 'uid))]
         [else (for/fold ([acc acc]) ([o (in-pvector (tree-ops t))]) (mark o acc))]))
     (define (mark-stmt g acc0)
       (define acc (mark (stmt-vdef g) (mark (stmt-vuse g) acc0)))
       (cond
         [(gassign? g) (for/fold ([acc acc]) ([t (list (gassign-lhs g) (gassign-rhs1 g) (gassign-rhs2 g) (gassign-rhs3 g))]) (mark t acc))]
         [(gcall? g) (for/fold ([acc acc]) ([t (append (list (gcall-lhs g) (gcall-fn g)) (pvector->list (gcall-args g)))]) (mark t acc))]
         [(gcond? g) (mark (gcond-rhs g) (mark (gcond-lhs g) acc))]
         [(gswitch? g) (mark (gswitch-index g) acc)]
         [(greturn? g) (mark (greturn-retval g) acc)]
         [else acc]))
     (define used
       (for*/fold ([acc intbits-empty]) ([i (in-pvector (cfg-bbs cfg))])
         (define b (cfg-block cfg i))
         (define acc1 (for/fold ([acc acc]) ([ph (in-pvector (GBlock-phis b))])
                        (for/fold ([acc (mark (gphi-result ph) acc)]) ([arg (in-pvector (gphi-args ph))]) (mark (cdr arg) acc))))
         (for/fold ([acc acc1]) ([g (in-pvector (GBlock-stmts b))]) (mark-stmt g acc))))
     (for/pvector ([d (in-pvector locals)] #:when (intbits-ref used (decl-uid d))) d)]))
