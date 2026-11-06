#lang racket/base

;; ============================================================
;; gimple: GENERIC tree helpers (pure functions over kernel/ir/gimple)
;; ============================================================
;;
;; The functional counterpart of the former (deleted) component/gccast/generic/
;; tree.rkt: builders, fold subset, predicates, type compatibility and
;; statement-list helpers.  Trees are values; type arguments are uids
;; resolved through the state; functions that allocate (fresh uids,
;; tmp names, SSA versions, new types) return the new state last.
;;
;; Portions follow gcc/tree.cc, gcc/gimple-expr.cc, gcc/fold-const.cc
;; and gcc/tree-iterator.cc (GPLv3+) function by function; see
;; doc/roadmap.md 6.3 for the licensing note.
;; ============================================================

(require racket/list racket/string
         "../../kernel/data/data.rkt"
         "../../kernel/ir/gimple/types.rkt"
         "state.rkt")

(provide build0 build1 build2 build3 build4
         build-int-cst build-zero-cst build-real-cst build-clobber tree-clobber-p
         build-empty-stmt build-label-expr build-goto
         create-artificial-label create-tmp-var-name create-tmp-var-raw make-ssa-name
         pointer-to type-name-string
         gimple-rhs-class
         ;; predicates
         type-p decl-p var-p expr-p constant-class-p comparison-class-p convert-expr-p
         handled-component-p indirect-ref-p
         integral-type-p pointer-type-p aggregate-type-p void-type-p scalar-float-type-p
         boolean-type-p record-or-union-type-p func-or-method-type-p complex-type-p vector-type-p
         integer-zerop integer-onep integer-nonzerop integer-minus-onep
         is-empty-stmt is-empty-type truth-value-p
         useless-type-conversion-p types-compatible-p tree-ssa-useless-type-conversion
         strip-useless-type-conversion strip-nops strip-type-nops strip-sign-nops
         ;; statement lists
         make-statement-list append-to-statement-list append-to-statement-list-force expr-first expr-last
         ;; side effects and fold
         recalculate-side-effects
         fold-convert fold-build1 fold-build2 fold-build3 convert-to-ptrofftype)

;; ------------------------------------------------------------
;; Builders
;; ------------------------------------------------------------

;; build0..4 (gcc/tree.cc): statements, MODIFY/INIT/CALL carry
;; TREE_SIDE_EFFECTS unconditionally; other expressions inherit it from
;; their operands.
(define (build* st code type ops)
  (define opv (if (pvector? ops) ops (list->pvector ops)))
  (define cls (tree-code-class st code))
  (define se
    (or (eq? cls 'tcc_statement)
        (and (memq code '(modify_expr init_expr call_expr bind_expr target_expr)) #t)
        (for/or ([o (in-pvector opv)]) (and o (tree-side-effects o)))))
  (define attrs
    (if (memq code '(integer_cst real_cst string_cst))
        (hasheq 'side-effects (and se #t) 'constant #t)
        (hasheq 'side-effects (and se #t))))
  (Tree code type #f opv attrs))
(define (build0 st code type) (build* st code type '()))
(define (build1 st code type a) (build* st code type (list a)))
(define (build2 st code type a b) (build* st code type (list a b)))
(define (build3 st code type a b c) (build* st code type (list a b c)))
(define (build4 st code type a b c d) (build* st code type (list a b c d)))

;; sign/zero-extend n to the type's precision (fold_convert_const semantics)
(define (wrap-int st type n)
  (define prec (or (type-attr st type 'precision) 64))
  (define m (bitwise-and n (sub1 (arithmetic-shift 1 prec))))
  (if (and (not (type-attr st type 'unsigned)) (bitwise-bit-set? m (sub1 prec)))
      (- m (arithmetic-shift 1 prec))
      m))

(define (build-int-cst st type n)
  (make-tree 'integer_cst #:type type #:attrs (list (cons 'value (wrap-int st type n)) (cons 'constant #t))))
(define (build-real-cst type x)
  (make-tree 'real_cst #:type type #:attrs (list (cons 'real x) (cons 'constant #t))))
(define (build-zero-cst st type)
  (cond
    [(or (integral-type-p st type) (pointer-type-p st type)) (build-int-cst st type 0)]
    [(scalar-float-type-p st type) (build-real-cst type 0.0)]
    [else (make-tree 'constructor #:type type #:attrs (list (cons 'elts (pvector-empty)) (cons 'constant #t)))]))

;; build_clobber (tree.cc): an empty CONSTRUCTOR marked as clobber
(define (build-clobber type [kind 'storage-end])
  (make-tree 'constructor #:type type
             #:attrs (list (cons 'elts (pvector-empty)) (cons 'clobber kind) (cons 'volatile #t) (cons 'constant #f))))
(define (tree-clobber-p t) (and (eq? (tree-code t) 'constructor) (tree-attr t 'clobber #f) #t))

;; build_empty_stmt: NOP_EXPR of void with integer_zero operand
(define (build-empty-stmt st)
  (build1 st 'nop_expr (void-type st) (build-int-cst st (size-type st) 0)))
(define (build-label-expr st label) (build1 st 'label_expr (void-type st) label))
(define (build-goto st label) (build1 st 'goto_expr (void-type st) label))

(define (fn-context st)
  (cons 'function_decl (and (GS-fndecl st) (decl-uid (GS-fndecl st)))))

;; create_artificial_label (loc): LABEL_DECL, artificial, ignored, context = current fn
(define (create-artificial-label st [loc #f])
  (let/st st ([uid (fresh-uid st)])
    (values (make-tree 'label_decl #:type (void-type st) #:loc loc
                       #:attrs (list (cons 'uid uid) (cons 'name #f) (cons 'artificial #t) (cons 'ignored #t)
                                     (cons 'context (fn-context st))))
            st)))

;; ASM_FORMAT_PRIVATE_NAME on ELF/Mach-O: "prefix.N" (remove_suffix strips a trailing ".N")
(define (create-tmp-var-name st prefix)
  (define p (if prefix (regexp-replace #rx"[.][0-9]+$" prefix "") "T"))
  (let/st st ([n (fresh-tmp-var-id st)])
    (values (format "~a.~a" p n) st)))

(define (create-tmp-var-raw st type prefix)
  (let/st st ([uid (fresh-uid st)]
              [name (if prefix (create-tmp-var-name st prefix) (values #f st))])
    (values (make-tree 'var_decl #:type type
                       #:attrs (list (cons 'uid uid) (cons 'name name)
                                     (cons 'artificial #t) (cons 'ignored #t) (cons 'nameless #t)
                                     (cons 'readonly #f) (cons 'external #f) (cons 'static #f) (cons 'used #t)
                                     (cons 'addressable #f) (cons 'public #f)
                                     (cons 'context (fn-context st))))
            st)))

;; SSA names: TYPE-OR-VAR is a type uid or a decl value
(define (make-ssa-name st type-or-var #:name [name #f])
  (let/st st ([v (fresh-ssa-version st)])
    (define type (if (exact-integer? type-or-var) (type-main-variant st type-or-var) (tree-type type-or-var)))
    (values (make-tree 'ssa_name #:type type
                       #:attrs (list (cons 'version v)
                                     (cons 'var (if (exact-integer? type-or-var) #f type-or-var))
                                     (cons 'identifier name)))
            st)))

;; TYPE_NAME as a string (identifier_node / type_decl / string)
(define (type-name-string st uid)
  (define nm (type-attr st uid 'name #f))
  (cond
    [(not nm) #f]
    [(string? nm) nm]
    [(eq? (tree-code nm) 'identifier_node) (tree-attr nm 'id)]
    [(eq? (tree-code nm) 'type_decl) (tree-attr nm 'name)]
    [else #f]))

;; build_pointer_type: an existing unqualified pointer to TYPE, else a new one
(define (pointer-to st type)
  (define found
    (for/first ([p (in-list (intmap->literal-datum (GS-types st)))]
                #:when (let ([t (cdr p)])
                         (and (eq? (tree-code t) 'pointer_type) (eqv? (tree-type t) type) (zero? (type-quals t)))))
      (car p)))
  (cond
    [found (values found st)]
    [else
     (let/st st ([uid (fresh-uid st)])
       (add-type st (make-tree 'pointer_type #:type type
                               #:attrs (list (cons 'uid uid) (cons 'name #f) (cons 'quals 0) (cons 'align 64)
                                             (cons 'mode "DI") (cons 'size (build-int-cst st (sizetype st) 64))
                                             (cons 'size-unit (build-int-cst st (sizetype st) 8))))))]))

;; gimple_rhs_class_table (gcc/gimple.cc) as a function.
(define (gimple-rhs-class st code)
  (case code
    [(truth_and_expr truth_or_expr truth_xor_expr) 'binary]
    [(truth_not_expr) 'unary]
    [(cond_expr widen_mult_plus_expr widen_mult_minus_expr dot_prod_expr sad_expr
      realign_load_expr vec_cond_expr vec_perm_expr bit_insert_expr) 'ternary]
    [(constructor obj_type_ref assert_expr addr_expr with_size_expr ssa_name) 'single]
    [else
     (case (tree-code-class st code)
       [(tcc_unary) 'unary]
       [(tcc_binary tcc_comparison) 'binary]
       [(tcc_constant tcc_declaration tcc_reference) 'single]
       [else 'invalid])]))

;; ------------------------------------------------------------
;; Predicates
;; ------------------------------------------------------------

(define (type-p st t) (and t (eq? (tree-code-class st (tree-code t)) 'tcc_type)))
(define (decl-p st t) (and t (eq? (tree-code-class st (tree-code t)) 'tcc_declaration)))
(define (var-p t) (and t (eq? (tree-code t) 'var_decl)))
(define (expr-p st t)
  (and t (memq (tree-code-class st (tree-code t))
               '(tcc_expression tcc_unary tcc_binary tcc_comparison tcc_reference tcc_statement tcc_vl_exp))
       #t))
(define (constant-class-p st t) (and t (eq? (tree-code-class st (tree-code t)) 'tcc_constant)))
(define (comparison-class-p st t) (and t (eq? (tree-code-class st (tree-code t)) 'tcc_comparison)))
(define (convert-expr-p t) (and t (memq (tree-code t) '(nop_expr convert_expr)) #t))
(define (handled-component-p t)
  (and t (memq (tree-code t) '(component_ref bit_field_ref array_ref array_range_ref realpart_expr imagpart_expr view_convert_expr)) #t))
(define (indirect-ref-p t) (and t (eq? (tree-code t) 'indirect_ref)))

;; type predicates take a type uid (or #f)
(define (integral-type-p st ty) (and ty (memq (type-code st ty) '(enumeral_type boolean_type integer_type bitint_type)) #t))
(define (pointer-type-p st ty) (and ty (memq (type-code st ty) '(pointer_type reference_type)) #t))
(define (aggregate-type-p st ty) (and ty (memq (type-code st ty) '(array_type record_type union_type qual_union_type)) #t))
(define (void-type-p st ty) (and ty (eq? (type-code st ty) 'void_type)))
(define (scalar-float-type-p st ty) (and ty (eq? (type-code st ty) 'real_type)))
(define (boolean-type-p st ty) (and ty (eq? (type-code st ty) 'boolean_type)))
(define (record-or-union-type-p st ty) (and ty (memq (type-code st ty) '(record_type union_type qual_union_type)) #t))
(define (func-or-method-type-p st ty) (and ty (memq (type-code st ty) '(function_type method_type)) #t))
(define (complex-type-p st ty) (and ty (eq? (type-code st ty) 'complex_type)))
(define (vector-type-p st ty) (and ty (eq? (type-code st ty) 'vector_type)))

(define (integer-zerop t) (and t (eq? (tree-code t) 'integer_cst) (eqv? (integer-cst-value t) 0)))
(define (integer-onep t) (and t (eq? (tree-code t) 'integer_cst) (eqv? (integer-cst-value t) 1)))
(define (integer-nonzerop t) (and t (eq? (tree-code t) 'integer_cst) (not (eqv? (integer-cst-value t) 0))))
(define (integer-minus-onep t) (and t (eq? (tree-code t) 'integer_cst) (eqv? (integer-cst-value t) -1)))

;; IS_EMPTY_STMT: NOP_EXPR of void type whose operand is integer zero
(define (is-empty-stmt st t)
  (and t (eq? (tree-code t) 'nop_expr) (void-type-p st (tree-type t)) (integer-zerop (tree-operand t 0))))

(define (is-empty-type st ty)
  (cond
    [(record-or-union-type-p st ty)
     (for/and ([f (in-pvector (type-attr st ty 'fields (pvector-empty)))])
       (or (not (eq? (tree-code f) 'field_decl)) (tree-attr f 'padding #f) (is-empty-type st (tree-type f))))]
    [(eq? (type-code st ty) 'array_type)
     (define dom (type-attr st ty 'domain))
     (or (not dom)
         (let ([mx (type-attr st dom 'max)] [mn (type-attr st dom 'min)])
           (and mx mn (eq? (tree-code mx) 'integer_cst) (eq? (tree-code mn) 'integer_cst)
                (< (integer-cst-value mx) (integer-cst-value mn))))
         (is-empty-type st (type-type st ty)))]
    [else #f]))

(define (truth-value-p code)
  (and (memq code '(truth_and_expr truth_or_expr truth_xor_expr truth_andif_expr truth_orif_expr truth_not_expr)) #t))

;; ------------------------------------------------------------
;; useless_type_conversion_p (gcc/gimple-expr.cc), C subset; uids
;; ------------------------------------------------------------

(define (useless-type-conversion-p st outer inner)
  (define (mode ty) (type-attr st ty 'mode))
  (define (icst v) (and v (eq? (tree-code v) 'integer_cst) v))
  (let/ec return
    (when (and (pointer-type-p st inner) (pointer-type-p st outer))
      (when (and (func-or-method-type-p st (type-type st outer)) (not (func-or-method-type-p st (type-type st inner))))
        (return #f)))
    (define in (type-main-variant st inner))
    (define out (type-main-variant st outer))
    (when (eqv? in out) (return #t))
    (unless (equal? (mode in) (mode out)) (return #f))
    (define in-code (type-code st in)) (define out-code (type-code st out))
    (cond
      [(and (integral-type-p st in) (integral-type-p st out))
       (when (or (not (eq? (type-attr st in 'unsigned) (type-attr st out 'unsigned)))
                 (not (eqv? (type-attr st in 'precision) (type-attr st out 'precision))))
         (return #f))
       (when (and (not (eq? (boolean-type-p st in) (boolean-type-p st out)))
                  (not (eqv? (type-attr st out 'precision) 1)))
         (return #f))
       (when (not (eq? (eq? in-code 'bitint_type) (eq? out-code 'bitint_type)))
         (return #f))
       #t]
      [(and (scalar-float-type-p st in) (scalar-float-type-p st out)) #t]
      [(and (pointer-type-p st in) (pointer-type-p st out)) #t]
      [(and (complex-type-p st in) (complex-type-p st out))
       (useless-type-conversion-p st (type-type st out) (type-type st in))]
      [(and (eq? in-code 'array_type) (eq? out-code 'array_type))
       (when (not (eq? (type-attr st in 'string-flag) (type-attr st out 'string-flag))) (return #f))
       (define idom (type-attr st in 'domain)) (define odom (type-attr st out 'domain))
       (when (and (not idom) odom) (return #f))
       (define os (icst (type-attr st out 'size))) (define is (icst (type-attr st in 'size)))
       (when (and os (or (not is) (not (eqv? (integer-cst-value os) (integer-cst-value is)))))
         (return #f))
       (when (and idom odom (not (eqv? idom odom)))
         (define imin (icst (type-attr st idom 'min))) (define omin (icst (type-attr st odom 'min)))
         (define imax (icst (type-attr st idom 'max))) (define omax (icst (type-attr st odom 'max)))
         (when (and omin (or (not imin) (not (eqv? (integer-cst-value imin) (integer-cst-value omin))))) (return #f))
         (when (and omax (or (not imax) (not (eqv? (integer-cst-value imax) (integer-cst-value omax))))) (return #f)))
       (useless-type-conversion-p st (type-type st out) (type-type st in))]
      [(and (func-or-method-type-p st in) (eq? in-code out-code))
       (unless (useless-type-conversion-p st (type-type st out) (type-type st in)) (return #f))
       (unless (type-attr st out 'prototype) (return #t))
       (define oargs (type-attr st out 'arg-types)) (define iargs (type-attr st in 'arg-types))
       (when (equal? oargs iargs) (return #t))
       (let loop ([op oargs] [ip iargs])
         (cond
           [(and op ip)
            (unless (useless-type-conversion-p st (type-main-variant st (tree-value op)) (type-main-variant st (tree-value ip)))
              (return #f))
            (loop (tree-chain op) (tree-chain ip))]
           [(or op ip) #f]
           [else #t]))]
      [(and (aggregate-type-p st in) (eq? in-code out-code))
       (and (type-attr st in 'canonical-uid) (eqv? (type-attr st in 'canonical-uid) (type-attr st out 'canonical-uid)))]
      [else #f])))

(define (types-compatible-p st a b)
  (or (eqv? a b) (and (useless-type-conversion-p st a b) (useless-type-conversion-p st b a))))

(define (tree-ssa-useless-type-conversion st expr)
  (and (or (convert-expr-p expr) (memq (tree-code expr) '(view_convert_expr non_lvalue_expr)))
       (useless-type-conversion-p st (tree-type expr) (tree-type (tree-operand expr 0)))))

;; STRIP_USELESS_TYPE_CONVERSION
(define (strip-useless-type-conversion st e)
  (let loop ([e e])
    (if (and e (tree-ssa-useless-type-conversion st e)) (loop (tree-operand e 0)) e)))
;; STRIP_NOPS-like: strip nop/convert with the same mode
(define (strip-nops st e)
  (let loop ([e e])
    (if (and e (or (convert-expr-p e) (eq? (tree-code e) 'non_lvalue_expr))
             (equal? (type-attr st (tree-type e) 'mode) (type-attr st (tree-type (tree-operand e 0)) 'mode)))
        (loop (tree-operand e 0))
        e)))
;; STRIP_TYPE_NOPS: strip NOP/CONVERT/NON_LVALUE whose operand has the same type
(define (strip-type-nops e)
  (let loop ([e e])
    (if (and e (or (convert-expr-p e) (eq? (tree-code e) 'non_lvalue_expr))
             (tree-operand e 0) (eqv? (tree-type (tree-operand e 0)) (tree-type e)))
        (loop (tree-operand e 0))
        e)))
;; STRIP_SIGN_NOPS: like STRIP_NOPS but keep sign/precision-changing conversions
(define (strip-sign-nops st e)
  (let loop ([e e])
    (cond
      [(and e (or (convert-expr-p e) (eq? (tree-code e) 'non_lvalue_expr)))
       (define o (tree-operand e 0))
       (define ot (and o (tree-type o))) (define et (tree-type e))
       (if (and o (equal? (type-attr st ot 'mode) (type-attr st et 'mode))
                (eq? (type-attr st ot 'unsigned) (type-attr st et 'unsigned))
                (eqv? (type-attr st ot 'precision) (type-attr st et 'precision))
                (eq? (pointer-type-p st ot) (pointer-type-p st et)))
           (loop o)
           e)]
      [else e])))

;; ------------------------------------------------------------
;; Statement lists (gcc/tree-iterator.cc)
;; ------------------------------------------------------------

(define (expr-first e)
  (cond [(not e) #f]
        [(eq? (tree-code e) 'statement_list)
         (define l (statement-list-stmts e))
         (if (pvector-empty? l) #f (expr-first (pvector-first l)))]
        [else e]))
(define (expr-last e)
  (cond [(not e) #f]
        [(eq? (tree-code e) 'statement_list)
         (define l (statement-list-stmts e))
         (if (pvector-empty? l) #f (expr-last (pvector-last l)))]
        [else e]))

(define (make-statement-list st [stmts (pvector-empty)])
  (make-tree 'statement_list #:type (void-type st) #:attrs (list (cons 'stmts stmts) (cons 'side-effects #t))))

;; append_to_statement_list_1 (list_p is LST, may be #f): returns the new list
(define (append-to-statement-list-1 st t lst)
  (cond
    [(not lst)
     (if (and t (eq? (tree-code t) 'statement_list))
         t
         (make-statement-list st (pvector t)))]
    [(eq? (tree-code lst) 'statement_list)
     (tree-set-attr lst 'stmts (pvector-cons-right (statement-list-stmts lst) t))]
    [else (make-statement-list st (pvector lst t))]))

;; append_to_statement_list: skip statements without side effects
(define (append-to-statement-list st t lst)
  (if (and t (tree-side-effects t)) (append-to-statement-list-1 st t lst) lst))
(define (append-to-statement-list-force st t lst)
  (if t (append-to-statement-list-1 st t lst) lst))

;; ------------------------------------------------------------
;; Side effects (gcc/gimplify.cc recalculate_side_effects)
;; ------------------------------------------------------------

(define (recompute-from-operands st t)
  (define se
    (or (and (eq? (tree-code-class st (tree-code t)) 'tcc_reference) (tree-this-volatile t))
        (eq? (tree-code t) 'call_expr)
        (for/or ([o (in-pvector (tree-ops t))]) (and o (tree-side-effects o)))))
  (tree-set-attr t 'side-effects (and se #t)))

(define (recalculate-side-effects st t)
  (define code (tree-code t))
  (case (tree-code-class st code)
    [(tcc_expression)
     (case code
       [(init_expr modify_expr va_arg_expr bind_expr compound_expr cond_expr truth_andif_expr truth_orif_expr
         call_expr loop_expr exit_expr switch_expr label_expr goto_expr return_expr
         predict_expr try_finally_expr try_catch_expr cleanup_point_expr target_expr with_cleanup_expr)
        t]
       [else (recompute-from-operands st t)])]
    [(tcc_comparison tcc_unary tcc_binary tcc_reference tcc_vl_exp) (recompute-from-operands st t)]
    [else t]))

;; ------------------------------------------------------------
;; A small fold (the subset gimplification relies on at -O0)
;; ------------------------------------------------------------

;; fold_convert: no-op if useless; constant-fold INTEGER_CST; else NOP_EXPR
(define (fold-convert st type e)
  (cond
    [(or (eqv? type (tree-type e)) (useless-type-conversion-p st type (tree-type e))) e]
    [(and (eq? (tree-code e) 'integer_cst) (or (integral-type-p st type) (pointer-type-p st type)))
     (build-int-cst st type (integer-cst-value e))]
    [(and (eq? (tree-code e) 'integer_cst) (scalar-float-type-p st type))
     (build-real-cst type (exact->inexact (integer-cst-value e)))]
    [(and (eq? (tree-code e) 'real_cst) (real-cst-value e) (integral-type-p st type))
     (build-int-cst st type (inexact->exact (truncate (real-cst-value e))))]
    [(aggregate-type-p st type) (build1 st 'view_convert_expr type e)]
    [else (recalculate-side-effects st (build1 st 'nop_expr type e))]))

(define (fold-build1 st code type a)
  (cond
    [(and (eq? code 'negate_expr) (eq? (tree-code a) 'integer_cst))
     (build-int-cst st type (- (integer-cst-value a)))]
    [(and (eq? code 'negate_expr) (eq? (tree-code a) 'negate_expr)) (tree-operand a 0)]
    [(memq code '(nop_expr convert_expr)) (fold-convert st type a)]
    [else (recalculate-side-effects st (build1 st code type a))]))

(define (fold-build2 st code type a b)
  (define (icst? x) (eq? (tree-code x) 'integer_cst))
  (define (ret t) (recalculate-side-effects st t))
  (cond
    [(and (icst? a) (icst? b) (memq code '(plus_expr minus_expr mult_expr bit_and_expr bit_ior_expr bit_xor_expr pointer_plus_expr)))
     (define x (integer-cst-value a)) (define y (integer-cst-value b))
     (build-int-cst st type (case code [(plus_expr pointer_plus_expr) (+ x y)] [(minus_expr) (- x y)] [(mult_expr) (* x y)]
                              [(bit_and_expr) (bitwise-and x y)] [(bit_ior_expr) (bitwise-ior x y)] [(bit_xor_expr) (bitwise-xor x y)]))]
    [(and (icst? a) (icst? b) (memq code '(lt_expr le_expr gt_expr ge_expr eq_expr ne_expr)))
     (define x (integer-cst-value a)) (define y (integer-cst-value b))
     (build-int-cst st type (if (case code [(lt_expr) (< x y)] [(le_expr) (<= x y)] [(gt_expr) (> x y)] [(ge_expr) (>= x y)] [(eq_expr) (= x y)] [(ne_expr) (not (= x y))]) 1 0))]
    ;; A - C -> A + (-C)   (fold-const.cc, MINUS_EXPR with negatable constant)
    [(and (eq? code 'minus_expr) (icst? b) (integral-type-p st type))
     (ret (build2 st 'plus_expr type a (build-int-cst st type (- (integer-cst-value b)))))]
    ;; x + 0, x * 1
    [(and (eq? code 'plus_expr) (integer-zerop b)) (fold-convert st type a)]
    [(and (eq? code 'mult_expr) (integer-onep b)) (fold-convert st type a)]
    [else (ret (build2 st code type a b))]))

(define (fold-build3 st code type a b c)
  (recalculate-side-effects st (build3 st code type a b c)))

;; convert_to_ptrofftype: convert to sizetype
(define (convert-to-ptrofftype st e) (fold-convert st (sizetype st) e))
