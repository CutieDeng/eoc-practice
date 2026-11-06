#lang racket/base

;; ============================================================
;; Kernel IR: gimple -- persistent GENERIC / GIMPLE / GIMPLE-CFG data
;; ============================================================
;;
;; Decision 13 (doc/gimple-functional-ir.md).  Everything here is a
;; prefab value: pvector for sequences, intmap for id-keyed tables,
;; immutable hasheq for attribute sets.  Nothing in this module is
;; mutable and nothing needs state; the state-aware operations live in
;; component/gimple/state.rkt.
;;
;; Tree nodes
;;   #s(Tree code type loc ops attrs)
;;     code  : GCC tree code name, lowercase symbol ('plus_expr)
;;     type  : type uid (exact integer) or #f
;;     loc   : (list file line col) or #f
;;     ops   : pvector of operands (Tree values)
;;     attrs : immutable hasheq
;;   Expressions, constants, STATEMENT_LIST, BIND_EXPR are plain values
;;   (no identity).  Declarations are values identified by their 'uid
;;   attribute (compare with decl-eq?); their mutable bits are kept in
;;   the state's decl-flags table.  Types live in the state's type table
;;   and are referenced by uid everywhere (TREE_TYPE of a node is a uid).
;;
;; Locations
;;   #s(gloc spot block discr)   spot : (list file line col) or #f
;;   #f is UNKNOWN_LOCATION.  Two locations are equal only if all three
;;   components are (equal?); that is what -O0 cleanup_tree_cfg compares.
;;
;; Statements: one prefab per GIMPLE code, fields 1-2 are always
;;   loc (gloc or #f) and flags (immutable hasheq: do-not-emit,
;;   ctrl-altering).  A sequence (gimple_seq) is a pvector of statements.
;;
;; State #s(GS ...) and CFG #s(GCfg ...) are documented at their structs.
;; ============================================================

(require "../../data/data.rkt")

(provide (struct-out Tree)
         ;; stateless tree access
         tree-code tree-type tree-loc tree-ops tree-attr tree-has-attr? tree-operand tree-operand-length
         tree-set-code tree-set-type tree-set-loc tree-set-ops tree-set-operand tree-set-attr tree-remove-attr
         make-tree
         ;; flags and fields (GCC macro names, lowercase)
         tree-side-effects tree-this-volatile tree-readonly tree-constant tree-static
         decl-external decl-uid decl-name decl-ignored decl-arguments decl-result decl-context-uid
         decl-register decl-initial-attr decl-eq?
         type-uid type-main-variant-attr type-precision type-unsigned type-mode type-quals
         type-size type-size-unit type-fields type-domain type-arg-types type-min-value type-max-value
         type-canonical-uid type-prototype? type-string-flag type-name
         integer-cst-value real-cst-hex real-cst-value string-cst-bytes
         bind-expr-vars bind-expr-body bind-expr-block statement-list-stmts
         tree-purpose tree-value tree-chain constructor-elts
         case-low case-high case-label label-expr-label goto-destination decl-expr-decl
         call-expr-fn call-expr-nargs call-expr-arg call-expr-args call-expr-internal?
         ssa-name-version ssa-name-var ssa-name-identifier
         ;; locations
         (struct-out gloc) make-loc loc-spot loc-has-location? set-block-loc location-with-discriminator
         ;; statements
         (struct-out gassign) (struct-out gcall) (struct-out gcond) (struct-out glabel) (struct-out ggoto)
         (struct-out greturn) (struct-out gswitch) (struct-out gbind) (struct-out gpredict) (struct-out gnop)
         (struct-out gtry)
         stmt-loc stmt-flags stmt-set-loc stmt-flag stmt-set-flag stmt-has-location?
         ;; state and cfg records
         (struct-out GS) (struct-out GCfg) (struct-out GBlock) (struct-out GEdge))

;; ------------------------------------------------------------
;; Tree
;; ------------------------------------------------------------

(struct Tree (code type loc ops attrs) #:prefab)

(define (make-tree code #:type [type #f] #:loc [loc #f] #:ops [ops (pvector-empty)] #:attrs [attrs (hasheq)])
  (Tree code type loc
        (if (pvector? ops) ops (list->pvector ops))
        (cond [(hash? attrs) attrs]
              [else (for/hasheq ([p (in-list attrs)]) (values (car p) (cdr p)))])))

(define tree-code Tree-code)
(define tree-type Tree-type)
(define tree-loc Tree-loc)
(define tree-ops Tree-ops)
(define (tree-attr t key [default #f]) (hash-ref (Tree-attrs t) key default))
(define (tree-has-attr? t key) (hash-has-key? (Tree-attrs t) key))
(define (tree-operand t i)
  (define ops (Tree-ops t))
  (if (< i (pvector-length ops)) (pvector-ref ops i) #f))
(define (tree-operand-length t) (pvector-length (Tree-ops t)))

(define (tree-set-code t c) (Tree c (Tree-type t) (Tree-loc t) (Tree-ops t) (Tree-attrs t)))
(define (tree-set-type t ty) (Tree (Tree-code t) ty (Tree-loc t) (Tree-ops t) (Tree-attrs t)))
(define (tree-set-loc t l) (Tree (Tree-code t) (Tree-type t) l (Tree-ops t) (Tree-attrs t)))
(define (tree-set-ops t ops) (Tree (Tree-code t) (Tree-type t) (Tree-loc t) ops (Tree-attrs t)))
(define (tree-set-operand t i v)
  (define ops
    (let grow ([ops (Tree-ops t)])
      (if (>= i (pvector-length ops)) (grow (pvector-cons-right ops #f)) ops)))
  (tree-set-ops t (pvector-set ops i v)))
(define (tree-set-attr t key v)
  (Tree (Tree-code t) (Tree-type t) (Tree-loc t) (Tree-ops t) (hash-set (Tree-attrs t) key v)))
(define (tree-remove-attr t key)
  (Tree (Tree-code t) (Tree-type t) (Tree-loc t) (Tree-ops t) (hash-remove (Tree-attrs t) key)))

;; ---- flags / fields ----------------------------------------------------

(define (tree-side-effects t) (tree-attr t 'side-effects #f))
(define (tree-this-volatile t) (tree-attr t 'volatile #f))
(define (tree-readonly t) (tree-attr t 'readonly #f))
(define (tree-constant t) (tree-attr t 'constant #f))
(define (tree-static t) (tree-attr t 'static #f))
(define (decl-external t) (tree-attr t 'external #f))
(define (decl-uid t) (tree-attr t 'uid))
(define (decl-name t) (tree-attr t 'name #f))
(define (decl-ignored t) (tree-attr t 'ignored #f))
(define (decl-arguments t) (tree-attr t 'arguments (pvector-empty)))
(define (decl-result t) (tree-attr t 'result #f))
(define (decl-context-uid t) (let ([c (tree-attr t 'context #f)]) (and c (cdr c))))
(define (decl-register t) (tree-attr t 'register #f))
;; the DECL_INITIAL carried by the value; the state may override it
(define (decl-initial-attr t) (tree-attr t 'initial #f))
(define (decl-eq? a b) (and a b (eqv? (decl-uid a) (decl-uid b))))

;; type attributes on a type *value* (the state resolves uids)
(define (type-uid t) (tree-attr t 'uid))
(define (type-main-variant-attr t) (tree-attr t 'main-variant #f))   ; uid or #f
(define (type-precision t) (tree-attr t 'precision #f))
(define (type-unsigned t) (tree-attr t 'unsigned #f))
(define (type-mode t) (tree-attr t 'mode #f))
(define (type-quals t) (tree-attr t 'quals 0))
(define (type-size t) (tree-attr t 'size #f))
(define (type-size-unit t) (tree-attr t 'size-unit #f))
(define (type-fields t) (tree-attr t 'fields (pvector-empty)))
(define (type-domain t) (tree-attr t 'domain #f))                      ; uid or #f
(define (type-arg-types t) (tree-attr t 'arg-types #f))
(define (type-min-value t) (tree-attr t 'min #f))
(define (type-max-value t) (tree-attr t 'max #f))
(define (type-canonical-uid t) (tree-attr t 'canonical-uid #f))
(define (type-prototype? t) (tree-attr t 'prototype #f))
(define (type-string-flag t) (tree-attr t 'string-flag #f))
(define (type-name t) (tree-attr t 'name #f))

(define (integer-cst-value t) (tree-attr t 'value))
(define (real-cst-hex t) (tree-attr t 'hex #f))
(define (real-cst-value t) (tree-attr t 'real #f))
(define (string-cst-bytes t) (tree-attr t 'bytes))

(define (bind-expr-vars t) (tree-attr t 'vars (pvector-empty)))
(define (bind-expr-body t) (tree-operand t 1))
(define (bind-expr-block t) (tree-operand t 2))
(define (statement-list-stmts t) (tree-attr t 'stmts (pvector-empty)))
(define (tree-purpose t) (tree-attr t 'purpose #f))
(define (tree-value t) (tree-attr t 'value #f))
(define (tree-chain t) (tree-attr t 'chain #f))
(define (constructor-elts t) (tree-attr t 'elts (pvector-empty)))
(define (case-low t) (tree-operand t 0))
(define (case-high t) (tree-operand t 1))
(define (case-label t) (tree-operand t 2))
(define (label-expr-label t) (tree-operand t 0))
(define (goto-destination t) (tree-operand t 0))
(define (decl-expr-decl t) (tree-operand t 0))

;; CALL_EXPR operands: 0 = nargs (INTEGER_CST), 1 = fn, 2 = static chain, 3.. = args
(define (call-expr-fn t) (tree-operand t 1))
(define (call-expr-nargs t) (max 0 (- (tree-operand-length t) 3)))
(define (call-expr-arg t i) (tree-operand t (+ 3 i)))
(define (call-expr-args t) (pvector-drop (Tree-ops t) (min 3 (tree-operand-length t))))
(define (call-expr-internal? t) (tree-attr t 'internal #f))

(define (ssa-name-version t) (tree-attr t 'version))
(define (ssa-name-var t) (tree-attr t 'var #f))
(define (ssa-name-identifier t) (tree-attr t 'identifier #f))

;; ------------------------------------------------------------
;; location_t
;; ------------------------------------------------------------

(struct gloc (spot block discr) #:prefab)
(define (make-loc spot) (and spot (gloc spot #f 0)))
(define (loc-spot l) (and l (gloc-spot l)))
;; LOCATION_LOCUS (l) != UNKNOWN_LOCATION
(define (loc-has-location? l) (and l (gloc-spot l) #t))
;; set_block: a block-only location is "nonzero" even without a spot
(define (set-block-loc l block)
  (cond
    [(and (not l) (not block)) #f]
    [(not l) (gloc #f block 0)]
    [else (gloc (gloc-spot l) block (gloc-discr l))]))
;; location_with_discriminator: UNKNOWN when there is no spot
(define (location-with-discriminator l d)
  (and l (gloc-spot l) (gloc (gloc-spot l) (gloc-block l) d)))

;; ------------------------------------------------------------
;; GIMPLE statements
;; ------------------------------------------------------------

(struct gassign  (loc flags lhs code rhs1 rhs2 rhs3) #:prefab)
(struct gcall    (loc flags lhs fn args fntype internal? ecf) #:prefab)   ; ecf: call_expr_flags bits
(struct gcond    (loc flags code lhs rhs true-label false-label) #:prefab)
(struct glabel   (loc flags label) #:prefab)
(struct ggoto    (loc flags dest) #:prefab)
(struct greturn  (loc flags retval) #:prefab)
(struct gswitch  (loc flags index default-case cases) #:prefab)          ; cases: pvector of CASE_LABEL_EXPR
(struct gbind    (loc flags vars body block) #:prefab)                   ; body: pvector (seq)
(struct gpredict (loc flags predictor outcome) #:prefab)
(struct gnop     (loc flags) #:prefab)
(struct gtry     (loc flags eval cleanup kind) #:prefab)                 ; kind: 'finally | 'catch

;; Generic access to the two leading fields of any statement prefab.
(define (stmt-loc g) (vector-ref (struct->vector g) 1))
(define (stmt-flags g) (vector-ref (struct->vector g) 2))
(define (stmt-replace-field g i v)
  (define vec (struct->vector g))
  (apply make-prefab-struct (prefab-struct-key g)
         (for/list ([x (in-vector vec 1)] [j (in-naturals 1)]) (if (= i j) v x))))
(define (stmt-set-loc g l) (stmt-replace-field g 1 l))
(define (stmt-flag g key [default #f]) (hash-ref (stmt-flags g) key default))
(define (stmt-set-flag g key v) (stmt-replace-field g 2 (hash-set (stmt-flags g) key v)))
(define (stmt-has-location? g) (loc-has-location? (stmt-loc g)))

;; ------------------------------------------------------------
;; State
;; ------------------------------------------------------------
;;
;;   version     : GCC version string of the dump ("15.2.0")
;;   fndecl      : current_function_decl (FUNCTION_DECL value) or #f
;;   types       : intmap uid -> Tree (type values; append-only)
;;   decl-flags  : intmap uid -> hasheq (overrides of a declaration's
;;                 mutable bits: addressable used initial seen-in-bind-expr
;;                 context not-gimple-reg readonly artificial label-uid
;;                 forced label-break label-continue switch-break-label)
;;   next-uid    : next fresh DECL/TYPE uid
;;   tmp-var-id  : tmp_var_id_num (translation-unit global)
;;   ssa-version : SSA version counter (per function, 0 reserved)
;;   globals     : hasheq 'void 'boolean 'size 'sizetype 'ptr -> type uid
;;   loc         : gimplify's input_location (gloc or #f)
;;   ctx         : gimplify context stack (list of gctx values)

(struct GS (version fndecl types decl-flags next-uid tmp-var-id ssa-version globals loc ctx) #:prefab)

;; ------------------------------------------------------------
;; CFG
;; ------------------------------------------------------------
;;
;;   blocks         : intmap index -> GBlock   (0 = ENTRY, 1 = EXIT)
;;   edges          : intmap src-index -> pvector of GEdge (GCC succ order)
;;   label->bb      : intmap label uid -> block index
;;   next-label-uid : cfg->last_label_uid
;;   messages       : pvector of dump lines ("Removing basic block 4")
;;   altered        : cfgcleanup_altered_bbs (intbits)
;;   touched        : touched_switch_bbs (intbits)
;; GBlock preds is a pvector of predecessor indices (GCC pred order);
;; an edge is identified by (src, dest) and stored once, in `edges`.

(struct GCfg (blocks edges label->bb next-label-uid messages altered touched) #:prefab)
(struct GBlock (index stmts prev next preds) #:prefab)
(struct GEdge (dest flags goto-locus) #:prefab)
