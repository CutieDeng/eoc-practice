#lang racket/base

;; ============================================================
;; opt: pass_forwprop (gcc/tree-ssa-forwprop.cc), first instance
;; "forwprop1", pure at the boundary (GCfg in, GCfg out)
;; ============================================================
;;
;; pass_forwprop::execute over an mfn working copy, in RPO:
;;   - (>= 13) blocks without an executable (or not yet decided back
;;     edge) predecessor edge are skipped; the taken edge of a constant
;;     condition is the only executable successor;
;;   - PHIs whose arguments are all the same value enter the copy
;;     lattice and are queued for removal;
;;   - first statement walk: &DECL / p+ CST definitions are propagated
;;     into their dereferences (forward_propagate_addr_expr, the
;;     MEM_REF lhs/rhs and p+ cases) and removed when every use went;
;;   - second walk: lattice values substituted into the uses
;;     (may_propagate_copy), then fold_stmt (opt/simplify.rkt) and the
;;     per-code combiners repeated while something changes:
;;     comparisons / conditions with their defining statements
;;     (forward_propagate_into_comparison), switch index casts; the
;;     lattice is filled from single-rhs assignments, copies queued for
;;     removal;
;;   - successor PHI arguments substituted;
;;   - (>= 14) simple_dce_from_worklist on the names whose uses were
;;     substituted away, then the queued removals; (<= 13) only the
;;     queued statements, in reverse order;
;;   - cleanup_tree_cfg when a condition became constant.
;;
;; Not ported: complex/vector/CONSTRUCTOR/VEC_PERM rewrites, aggregate
;; copy/zero propagation and DSE, builtin call simplification, rotate /
;; count-zeroes / long-multiply recognition, __builtin_unreachable
;; handling, abnormal and EH edges.
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
         "fold.rkt"
         "mfn.rkt"
         "simplify.rkt")

(provide forwprop)

(define (icst? t) (and t (eq? (tree-code t) 'integer_cst)))

;; pass context
(struct F (st m lattice dce to-remove-defs [cfg-changed #:mutable]))   ; lattice: hasheqv version -> tree; dce: box intbits

(define (fwprop-ssa-val f name)
  (if (ssa-name? name) (hash-ref (F-lattice f) (ssa-name-version name) name) name))
(define (fwprop-set-lattice-val! f name val) (when (ssa-name? name) (hash-set! (F-lattice f) (ssa-name-version name) val)))
(define (fwprop-invalidate-lattice! f name) (when (and name (ssa-name? name)) (hash-remove! (F-lattice f) (ssa-name-version name))))
(define (dce-add! f name) (when (and (ssa-name? name) (not (ssa-default-def? name)))
                            (set-box! (F-dce f) (intbits-set (unbox (F-dce f)) (ssa-name-version name)))))

;; ------------------------------------------------------------
;; propagation sources
;; ------------------------------------------------------------

;; get_prop_source_stmt: (values def-id single-use?) or (values #f #f)
(define (get-prop-source-stmt f name single-use-only?)
  (define m (F-m f))
  (let loop ([name name] [single-use #t])
    (define su (and (mfn-single-use? m name) single-use))
    (cond
      [(and (not su) single-use-only?) (values #f #f)]
      [else
       (define id (mfn-def m name))
       (define g (and id (mfn-stmt m id)))
       (cond
         [(not (and g (gassign? g))) (values #f #f)]
         [(eq? (gassign-code g) 'ssa_name) (loop (gassign-rhs1 g) su)]
         [else (values id su)])])))

;; can_propagate_from
(define (can-propagate-from f g)
  (define st (F-st f))
  (define code (gassign-code g))
  (cond
    [(stmt-has-volatile-ops? g) #f]
    [(memq (tree-code-class st code) '(tcc_reference tcc_declaration)) #f]
    [(and (gassign-single-p st g) (is-gimple-min-invariant st (gassign-rhs1 g))) #t]
    [(and (memq code '(nop_expr convert_expr))
          (let ([t (tree-type (gassign-rhs1 g))])
            (and (pointer-type-p st t) (func-or-method-type-p st (type-type st t)))))
     #f]
    [else #t]))

;; remove_prop_source_from_use
(define (remove-prop-source-from-use! f name)
  (define m (F-m f))
  (let loop ([name name])
    (when (and name (ssa-name? name) (not (mfn-released? m name)) (not (ssa-default-def? name)) (mfn-zero-uses? m name))
      (define id (mfn-def m name))
      (define g (and id (mfn-stmt m id)))
      (when (and g (not (gphi? g)) (not (stmt-has-side-effects? (F-st f) g)))
        (fwprop-invalidate-lattice! f (stmt-get-lhs g))
        (mfn-remove! m id)
        (loop (and (gassign? g) (gassign-rhs1 g)))))))

;; rhs_to_tree: the defining rhs as a GENERIC tree of TYPE
(define (rhs-to-tree st type g)
  (define code (gassign-code g))
  (case (gimple-rhs-class st code)
    [(ternary) (fold-build3 st code type (gassign-rhs1 g) (gassign-rhs2 g) (gassign-rhs3 g))]
    [(binary) (fold-build2 st code type (gassign-rhs1 g) (gassign-rhs2 g))]
    [(unary) (build1 st code type (gassign-rhs1 g))]
    [else (gassign-rhs1 g)]))

;; ------------------------------------------------------------
;; forward_propagate_into_comparison
;; ------------------------------------------------------------

;; combine_cond_expr_cond: fold_binary (code type op0 op1) restricted to the
;; forms the port knows, canonicalized; #f otherwise
(define (combine-cond-expr-cond f code type op0 op1 invariant-only?)
  (define st (F-st f))
  (define t
    (cond
      [(fold-const-compare st code type op0 op1) => values]
      ;; (a cmp b) != 0 etc. with a comparison tree operand
      [(and (memq code '(eq_expr ne_expr)) (icst? op1) (or (integer-zerop op1) (integer-onep op1))
            (comparison-class-p st op0) (integral-type-p st (tree-type (tree-operand op0 0))))
       (define keep? (eq? (eq? code 'ne_expr) (integer-zerop op1)))
       (cond
         [keep? (build2 st (tree-code op0) type (tree-operand op0 0) (tree-operand op0 1))]
         [(invert-tree-comparison (tree-code op0))
          => (lambda (ic) (build2 st ic type (tree-operand op0 0) (tree-operand op0 1)))]
         [else #f])]
      ;; ((T) b) cmp CST where b is boolean: compare b itself
      [(and (memq code '(eq_expr ne_expr)) (icst? op1) (memq (tree-code op0) '(nop_expr convert_expr))
            (boolean-type-p st (tree-type (tree-operand op0 0))))
       (define b (tree-operand op0 0))
       (define v (integer-cst-value op1))
       (cond
         [(or (= v 0) (= v 1)) (build2 st code type b (build-int-cst st (tree-type b) v))]
         [else (build-int-cst st type (if (eq? code 'ne_expr) 1 0))])]
      [else #f]))
  (and t (or (not invariant-only?) (is-gimple-min-invariant st t)) t))

;; forward_propagate_into_comparison_1: a replacement condition tree or #f
(define (forward-propagate-into-comparison-1 f code type op0 op1)
  (define st (F-st f)) (define m (F-m f))
  (define (source op)
    (cond
      [(ssa-name? op)
       (define-values (id su) (get-prop-source-stmt f op #f))
       (define g (and id (mfn-stmt m id)))
       (if (and g (can-propagate-from f g)) (values g su) (values #f #f))]
      [else (values #f #f)]))
  (define-values (def0 su0) (source op0))
  (define-values (def1 su1) (source op1))
  (define rhs0 (and def0 (rhs-to-tree st (tree-type op1) def0)))
  (define rhs1 (and def1 (rhs-to-tree st (tree-type op0) def1)))
  (or (and rhs0
           (let* ([def-code (gassign-code def0)]
                  [invariant-only (and (not su0)
                                       (not (and (icst? op1)
                                                 (or (and (memq def-code '(nop_expr convert_expr))
                                                          (boolean-type-p st (tree-type (tree-operand rhs0 0))))
                                                     (comparison-code? st def-code)))))])
             (combine-cond-expr-cond f code type rhs0 op1 invariant-only)))
      (and rhs1 (combine-cond-expr-cond f code type op0 rhs1 (not su1)))
      (and rhs0 rhs1 (combine-cond-expr-cond f code type rhs0 rhs1 (not (and su0 su1))))))

(define (comparison-code? st code) (eq? (tree-code-class st code) 'tcc_comparison))

;; the comparison assignment at ID: changed?
(define (forward-propagate-into-comparison! f id)
  (define st (F-st f)) (define m (F-m f))
  (define g (mfn-stmt m id))
  (define type (tree-type (gassign-lhs g)))
  (define rhs1 (gassign-rhs1 g)) (define rhs2 (gassign-rhs2 g))
  (define tmp (forward-propagate-into-comparison-1 f (gassign-code g) type rhs1 rhs2))
  (cond
    [(and tmp (useless-type-conversion-p st type (tree-type tmp)))
     (define-values (code a b c) (extract-ops-from-tree st tmp))
     (mfn-replace! m id (gassign (gassign-loc g) (gassign-flags g) (gassign-lhs g) code a b c))
     (fold-stmt st m id values)
     (when (ssa-name? rhs1) (remove-prop-source-from-use! f rhs1))
     (when (ssa-name? rhs2) (remove-prop-source-from-use! f rhs2))
     #t]
    [else #f]))

;; forward_propagate_into_gimple_cond: 0 / 1 / 2 (2 = became constant)
(define (forward-propagate-into-gimple-cond! f id)
  (define st (F-st f)) (define m (F-m f))
  (define g (mfn-stmt m id))
  (define rhs1 (gcond-lhs g)) (define rhs2 (gcond-rhs g))
  (define tmp (forward-propagate-into-comparison-1 f (gcond-code g) (boolean-type st) rhs1 rhs2))
  (cond
    [(and tmp (is-gimple-condexpr-for-cond st tmp))
     (define-values (code a b) (gimple-cond-get-ops-from-tree st tmp))
     (mfn-replace! m id (gcond (gcond-loc g) (gcond-flags g) code a b (gcond-true-label g) (gcond-false-label g)))
     (when (ssa-name? rhs1) (remove-prop-source-from-use! f rhs1))
     (when (ssa-name? rhs2) (remove-prop-source-from-use! f rhs2))
     (if (is-gimple-min-invariant st tmp) 2 1)]
    [else 0]))

;; ------------------------------------------------------------
;; forward_propagate_addr_expr
;; ------------------------------------------------------------

;; rewrite a MEM_REF [NAME, off] found at the base of REF through DEF-RHS (= &obj):
;; (values new-ref ok?) where ok? means the base was rewritten
(define (rewrite-mem-ref-base f ref name def-rhs)
  (define st (F-st f))
  (define-values (base rebuild)
    (let loop ([x ref] [k (lambda (y) y)])
      (if (handled-component-p x)
          (loop (tree-operand x 0) (lambda (y) (k (tree-set-operand x 0 y))))
          (values x k))))
  (cond
    [(and (eq? (tree-code base) 'mem_ref) (ssa-name=? (tree-operand base 0) name))
     (define-values (def-base def-off) (get-addr-base-and-unit-offset st (tree-operand def-rhs 0)))
     (cond
       [def-base
        (define off0 (+ (mem-ref-offset base) def-off))
        (define-values (new-ptr off)
          (if (eq? (tree-code def-base) 'mem_ref)
              (values (tree-operand def-base 0) (+ off0 (mem-ref-offset def-base)))
              (values (build-fold-addr-expr st def-base) off0)))
        (values (rebuild (tree-set-operand (tree-set-operand base 0 new-ptr) 1
                                           (build-int-cst st (tree-type (tree-operand base 1)) off)))
                #t)]
       [else (values ref #f)])]
    [else (values ref #f)]))

;; forward_propagate_addr_expr_1 on the use statement USE-ID: ok?
(define (forward-propagate-addr-expr-1! f name def-rhs use-id single-use?)
  (define st (F-st f)) (define m (F-m f))
  (define g (mfn-stmt m use-id))
  (define lhs (gassign-lhs g))
  (define code (gassign-code g))
  (define rhs (gassign-rhs1 g))
  (cond
    ;; a copy: propagate into its uses
    [(and (ssa-name? lhs) (eq? code 'ssa_name)) (forward-propagate-addr-expr! f lhs def-rhs single-use?)]
    [(and (ssa-name? lhs) (memq code '(nop_expr convert_expr)))
     (cond
       [(and single-use? (useless-type-conversion-p st (tree-type lhs) (tree-type def-rhs)))
        (mfn-replace! m use-id (gassign (gassign-loc g) (gassign-flags g) lhs 'addr_expr def-rhs #f #f))
        #t]
       [(and (or (integral-type-p st (tree-type lhs)) (pointer-type-p st (tree-type lhs)))
             (>= (type-precision* st (tree-type lhs)) (type-precision* st (tree-type def-rhs))))
        (forward-propagate-addr-expr! f lhs def-rhs single-use?)]
       [else #f])]
    [(not (types-compatible-p st (tree-type name) (tree-type def-rhs))) #f]
    ;; lhs = name p+ CST  ->  lhs = &MEM[obj + CST]
    [(and (ssa-name? lhs) (eq? code 'pointer_plus_expr) (ssa-name=? rhs name) (icst? (gassign-rhs2 g)))
     (define inner (make-tree 'mem_ref #:type (type-type st (tree-type rhs))
                              #:ops (list def-rhs (fold-convert st (ptr-type st) (gassign-rhs2 g)))))
     (define-values (new-def-rhs _ch)
       (canonicalize-mem-ref-addr st (make-tree 'addr_expr #:type (tree-type rhs) #:ops (list inner))))
     (cond
       [(forward-propagate-addr-expr! f lhs new-def-rhs single-use?) #t]
       [(useless-type-conversion-p st (tree-type lhs) (tree-type new-def-rhs))
        (mfn-replace! m use-id (gassign (gassign-loc g) (gassign-flags g) lhs 'addr_expr new-def-rhs #f #f))
        #t]
       [else #f])]
    [else
     ;; a dereference of NAME on the lhs and/or the rhs
     (define-values (lhs1 lhs-ok?) (if (ssa-name? lhs) (values lhs #f) (rewrite-mem-ref-base f lhs name def-rhs)))
     (define res (if (and (not (ssa-name? lhs)) (not lhs-ok?) (mem-ref-of? lhs name)) #f #t))
     (define-values (rhs1 rhs-ok?) (rewrite-mem-ref-base f rhs name def-rhs))
     (cond
       [(or lhs-ok? rhs-ok?)
        (mfn-replace! m use-id (gassign (gassign-loc g) (gassign-flags g) lhs1 (if (ssa-name? lhs1) (tree-code rhs1) code) rhs1
                                        (gassign-rhs2 g) (gassign-rhs3 g)))
        (when rhs-ok? (fold-stmt st m use-id values))
        (if rhs-ok? res (and single-use? res))]
       [else #f])]))

(define (mem-ref-of? t name)
  (let loop ([x t])
    (cond
      [(handled-component-p x) (loop (tree-operand x 0))]
      [else (and (eq? (tree-code x) 'mem_ref) (ssa-name=? (tree-operand x 0) name))])))

;; forward_propagate_addr_expr: all uses rewritten and NAME dead?
(define (forward-propagate-addr-expr! f name rhs parent-single-use?)
  (define m (F-m f))
  (define single-use? (and parent-single-use? (mfn-single-use? m name)))
  (define all
    (for/fold ([all #t]) ([use-id (in-list (mfn-use-ids m name))])
      (define g (mfn-stmt m use-id))
      (cond
        [(not (gassign? g)) #f]
        [else
         (define result (forward-propagate-addr-expr-1! f name rhs use-id single-use?))
         (define g* (mfn-stmt m use-id))
         ;; a copy whose lhs became dead goes
         (when (and result (ssa-name? (gassign-lhs g*)) (ssa-name? (gassign-rhs1 g*)) (mfn-zero-uses? m (gassign-lhs g*)))
           (fwprop-invalidate-lattice! f (gassign-lhs g*))
           (mfn-remove! m use-id))
         (and all result)])))
  (and all (mfn-zero-uses? m name)))

;; ------------------------------------------------------------
;; simplify_gimple_switch: a cast index
;; ------------------------------------------------------------

(define (simplify-gimple-switch! f id)
  (define st (F-st f)) (define m (F-m f))
  (define g (mfn-stmt m id))
  (define idx (gswitch-index g))
  (define dg (and (ssa-name? idx) (mfn-def-stmt m idx)))
  (cond
    [(and dg (gassign? dg) (memq (gassign-code dg) '(nop_expr convert_expr)) (ssa-name? (gassign-rhs1 dg)))
     (define def (gassign-rhs1 dg))
     (define ti (tree-type def))
     (define cases (gswitch-cases g))
     (define (fits? v) (let ([p (type-precision* st ti)] [n (integer-cst-value v)])
                         (if (type-unsigned?* st ti) (<= 0 n (sub1 (arithmetic-shift 1 p)))
                             (<= (- (arithmetic-shift 1 (sub1 p))) n (sub1 (arithmetic-shift 1 (sub1 p)))))))
     (define mn (and (not (pvector-empty? cases)) (case-low (pvector-first cases))))
     (define mx (and (not (pvector-empty? cases)) (let ([l (pvector-last cases)]) (or (case-high l) (case-low l)))))
     (cond
       [(and (integral-type-p st ti) (<= (type-precision* st ti) (type-precision* st (tree-type idx)))
             (or (not mn) (fits? mn)) (or (not mx) (fits? mx)))
        (dce-add! f idx)
        ;; the case labels keep their values (preprocess_case_label_vec_for_gimple drops none here)
        (mfn-replace! m id (gswitch (gswitch-loc g) (gswitch-flags g) def (gswitch-default-case g) cases))
        #t]
       [else #f])]
    [else #f]))

;; ------------------------------------------------------------
;; the pass
;; ------------------------------------------------------------

(define (forwprop st fndecl cfg0)
  ;; fold_stmt may build new statements (gimple_build): their SSA names
  ;; come from the state in this box
  (parameterize ([simplify-state-box (box (gs-set-fndecl st fndecl))])
    (forwprop* st fndecl cfg0)))

(define (forwprop* st fndecl cfg0)
  (define st0 (gs-set-fndecl st fndecl))
  (define major (gcc-major-version st0))
  (define cfg (cfg-clear-messages cfg0))
  (define m (gcfg->mfn cfg))
  (define dom (compute-dom (pvector-cons-left (pvector-cons-left (cfg-bbs cfg) EXIT) ENTRY) ENTRY
                           (lambda (i) (for/pvector ([e (in-pvector (cfg-succs cfg i))]) (GEdge-dest e)))))
  (define rpo (for/list ([b (in-pvector (dom-rpo dom))] #:unless (or (= b ENTRY) (= b EXIT))) b))
  (define bb->rpo (for/hasheqv ([b (in-list rpo)] [i (in-naturals)]) (values b i)))
  (define exec (make-hash))
  (define (exec? src dest) (hash-ref exec (cons src dest) #f))
  (hash-set! exec (cons ENTRY (GEdge-dest (pvector-first (cfg-succs cfg ENTRY)))) #t)
  (define f (F st0 m (make-hasheqv) (box intbits-empty) '() #f))
  (define to-remove-defs '())           ; versions, in queue order
  (define (queue-def! name) (set! to-remove-defs (cons (ssa-name-version name) to-remove-defs)))
  (define exec-walk? (>= major 13))
  (define dce-walk? (>= major 14))

  (for ([bb (in-list rpo)] [i (in-naturals)])
    (define reachable?
      (or (not exec-walk?)
          (for/or ([src (in-pvector (cfg-preds cfg bb))])
            (or (exec? src bb)
                (and (> (hash-ref bb->rpo src -1) i)
                     (not (dom-dominates? dom (dom-index dom bb) (dom-index dom src))))))))
    (when reachable?
      ;; PHIs with all-same arguments
      (for ([pid (in-list (mfn-phis m bb))])
        (define ph (mfn-stmt m pid))
        (define res (gphi-result ph))
        (unless (ssa-virtual? res)
          (define-values (first all-same)
            (let loop ([args (pvector->list (gphi-args ph))] [first #f] [all-same #t])
              (cond
                [(or (null? args) (not all-same)) (values first all-same)]
                [else
                 (define src (car (car args))) (define use (cdr (car args)))
                 (cond
                   [(and exec-walk? (not (exec? src bb)) (< (hash-ref bb->rpo src -1) i)) (loop (cdr args) first all-same)]
                   [else
                    (define same0 (if (and exec-walk? (not (exec? src bb))) #f all-same))
                    (cond
                      [(and (ssa-name? use) (ssa-name=? use res)) (loop (cdr args) first same0)]
                      [(not first) (loop (cdr args) use same0)]
                      [(equal? first use) (loop (cdr args) first same0)]
                      [else (loop (cdr args) first #f)])])])))
          (when (and all-same first)
            (when (may-propagate-copy st0 res first) (queue-def! res))
            (fwprop-set-lattice-val! f res first))))
      ;; first walk: addresses
      (let loop ([id (mfn-first m bb)])
        (when id
          (define next (mfn-next m id))
          (define g (mfn-stmt m id))
          (when (gassign? g)
            (define lhs (gassign-lhs g)) (define rhs (gassign-rhs1 g)) (define code (gassign-code g))
            (when (and (ssa-name? lhs) (not (mfn-zero-uses? m lhs)))
              (cond
                [(or (eq? code 'addr_expr)
                     (and (memq code '(nop_expr convert_expr)) (eq? (tree-code rhs) 'addr_expr) (pointer-type-p st0 (tree-type lhs))))
                 (define base (let loop ([x (tree-operand rhs 0)]) (if (handled-component-p x) (loop (tree-operand x 0)) x)))
                 (define base-decl (if (eq? (tree-code base) 'mem_ref) #f base))
                 (when (and (or (not base-decl) (not (decl-p st0 base-decl)) (decl-address-invariant-p st0 base-decl))
                            (forward-propagate-addr-expr! f lhs rhs #t))
                   (fwprop-invalidate-lattice! f lhs)
                   (mfn-remove! m id))]
                [(eq? code 'pointer_plus_expr)
                 (define off (gassign-rhs2 g))
                 (when (and (icst? off) (can-propagate-from f g) (not (simple-iv-increment? m g)))
                   (define inner (make-tree 'mem_ref #:type (type-type st0 (tree-type rhs))
                                            #:ops (list rhs (fold-convert st0 (ptr-type st0) off))))
                   (define def-rhs (make-tree 'addr_expr #:type (tree-type rhs) #:ops (list inner)))
                   (when (forward-propagate-addr-expr! f lhs def-rhs #t)
                     (fwprop-invalidate-lattice! f lhs)
                     (mfn-remove! m id)))]
                [else (void)])))
          (loop next)))
      ;; second walk: substitution, folding, combining
      (let loop ([id (mfn-first m bb)])
        (when id
          (define g (mfn-stmt m id))
          ;; lattice values into the uses
          (define substituted #f)
          (define g1
            (stmt-map-uses st0 g (lambda (u)
                                   (define val (fwprop-ssa-val f u))
                                   (cond
                                     [(not (equal? val u))
                                      (dce-add! f u)
                                      (cond [(may-propagate-copy st0 u val) (set! substituted #t) val] [else u])]
                                     [else u]))))
          (when substituted (mfn-replace! m id g1))
          ;; fold and combine until nothing changes
          (let fold-loop ()
            (define g2 (mfn-stmt m id))
            (define uses (stmt-real-uses g2))
            (define changed
              (cond
                [(fold-stmt st0 m id (lambda (t) (fwprop-ssa-val f t)))
                 (define g3 (mfn-stmt m id))
                 (when (and (gcond? g3) (gimple-cond-constant? g3)) (set-F-cfg-changed! f #t))
                 (for ([u (in-list uses)]) (dce-add! f u))
                 #t]
                [else #f]))
            (define g4 (mfn-stmt m id))
            (define changed2
              (cond
                [(gassign? g4)
                 (define code (gassign-code g4))
                 (cond
                   [(and (comparison-code? st0 code) (ssa-name? (gassign-lhs g4))) (forward-propagate-into-comparison! f id)]
                   [else #f])]
                [(gswitch? g4) (simplify-gimple-switch! f id)]
                [(gcond? g4)
                 (define r (forward-propagate-into-gimple-cond! f id))
                 (when (= r 2) (set-F-cfg-changed! f #t))
                 (not (zero? r))]
                [else #f]))
            (when (or changed changed2) (fold-loop)))
          ;; fill the lattice
          (define g5 (mfn-stmt m id))
          (when (and (gassign? g5) (gassign-single-p st0 g5) (ssa-name? (gassign-lhs g5)))
            (define lhs (gassign-lhs g5)) (define rhs (gassign-rhs1 g5))
            (define val (cond [(ssa-name? rhs) (fwprop-ssa-val f rhs)] [(is-gimple-min-invariant st0 rhs) rhs] [else lhs]))
            (when (and (not (equal? val lhs)) (may-propagate-copy st0 lhs val)) (queue-def! lhs))
            (fwprop-set-lattice-val! f lhs val))
          (loop (mfn-next m id))))
      ;; successor PHI arguments
      (for* ([e (in-pvector (cfg-succs cfg bb))] [pid (in-list (mfn-phis m (GEdge-dest e)))])
        (define ph (mfn-stmt m pid))
        (define args*
          (for/pvector ([arg (in-pvector (gphi-args ph))])
            (define a (cdr arg))
            (cond
              [(and (= (car arg) bb) (ssa-name? a) (not (ssa-virtual? a)))
               (define val (fwprop-ssa-val f a))
               (if (and (not (equal? val a)) (may-propagate-copy st0 a val)) (cons (car arg) val) arg)]
              [else arg])))
        (unless (equal? args* (gphi-args ph)) (mfn-replace! m pid (phi-set-args ph args*))))
      ;; executable successors
      (when exec-walk?
        (define last (mfn-last m bb))
        (define taken
          (and last (let ([g (mfn-stmt m last)])
                      (cond
                        [(gcond? g) (and (gimple-cond-constant? g)
                                         (find-taken-edge st0 (block-view m cfg bb) bb (cond-value st0 g)))]
                        [(gswitch? g) (and (icst? (gswitch-index g)) (find-taken-edge st0 (block-view m cfg bb) bb (gswitch-index g)))]
                        [else (and (= 1 (pvector-length (cfg-succs cfg bb))) (pvector-first (cfg-succs cfg bb)))]))))
        (cond
          [taken
           (hash-set! exec (cons bb (GEdge-dest taken)) #t)
           (when (> (pvector-length (cfg-succs cfg bb)) 1) (set-F-cfg-changed! f #t))]
          [else (for ([e (in-pvector (cfg-succs cfg bb))]) (hash-set! exec (cons bb (GEdge-dest e)) #t))]))))

  ;; removals
  (when dce-walk? (simple-dce! (F-st f) m (unbox (F-dce f))))
  (for ([v (in-list to-remove-defs)])          ; LIFO = GCC's pop order
    (define id (mfn-def-of-version m v))
    (when id (mfn-remove! m id)))
  (define cfg1 (mfn->gcfg m))
  (define st-out (unbox (simplify-state-box)))
  (if (F-cfg-changed f) (cleanup-tree-cfg st-out cfg1 print-cond-rhs) (values cfg1 st-out)))

;; the CFG with block BB's statements as they are in the working copy
(define (block-view m cfg bb)
  (set-block cfg (block-set-stmts (cfg-block cfg bb) (for/pvector ([id (in-list (mfn-stmts m bb))]) (mfn-stmt m id)))))

;; gimple_cond_true_p || gimple_cond_false_p
(define (gimple-cond-constant? g)
  (and (icst? (gcond-lhs g)) (icst? (gcond-rhs g))))
(define (cond-value st g)
  (fold-const-compare st (gcond-code g) (boolean-type st) (gcond-lhs g) (gcond-rhs g)))

;; simple_iv_increment_p: lhs feeds a PHI that defines rhs1 (an induction variable step)
(define (simple-iv-increment? m g)
  (define lhs (gassign-lhs g)) (define rhs (gassign-rhs1 g))
  (and (ssa-name? rhs)
       (let ([d (mfn-def-stmt m rhs)])
         (and d (gphi? d)
              (for/or ([a (in-pvector (gphi-args d))]) (and (ssa-name? (cdr a)) (ssa-name=? (cdr a) lhs)))))))

;; simple_dce_from_worklist over the working copy
(define (simple-dce! st m worklist0)
  (let loop ([wl worklist0])
    (unless (intbits-empty? wl)
      (define v (intbits-first wl))
      (define wl1 (intbits-clear wl v))
      (define id (mfn-def-of-version m v))
      (cond
        [(not id) (loop wl1)]
        [else
         (define g (mfn-stmt m id))
         (define def (car (stmt-def-names g)))
         (define n (mfn-num-uses m def))
         (define self-only?
           (and (gphi? g) (for/and ([u (in-list (mfn-use-ids m def))]) (eqv? u id))))
         (cond
           [(and (> n 0) (not self-only?)) (loop wl1)]
           [(and (not (gphi? g)) (stmt-has-side-effects? st g))
            (when (gcall? g)
              (mfn-replace! m id (gcall (gcall-loc g) (gcall-flags g) #f (gcall-fn g) (gcall-args g) (gcall-fntype g) (gcall-internal? g) (gcall-ecf g)))
              (mfn-release-name! m def))
            (loop wl1)]
           [else
            (define used (if (gphi? g) (map cdr (phi-ssa-args g)) (stmt-real-uses g)))
            (mfn-remove! m id)
            (loop (for/fold ([wl wl1]) ([u (in-list used)])
                    (if (ssa-default-def? u) wl (intbits-set wl (ssa-name-version u)))))])]))))
