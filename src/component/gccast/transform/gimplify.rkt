#lang racket/base

;; ============================================================
;; gimplify: GENERIC -> high GIMPLE
;; ============================================================
;;
;; Function-by-function port of the C subset of gcc/gimplify.cc and
;; the operand predicates of gcc/gimple-expr.cc (GPLv3+), trunk
;; 17.0.0 (352a0b5fc55).  Names follow GCC's; see each function for
;; the original.  Not covered: OpenMP/OpenACC, EH (TRY_*), VLA sizes,
;; asm, va_arg, TARGET_EXPR cleanups, full gimplify_init_constructor,
;; fold_stmt beyond readonly-symbol substitution.
;;
;; Entry: (gimplify-function fndecl body) -> gbind
;; ============================================================

(require racket/list racket/match
         "../generic/tree.rkt"
         "../generic/gimple.rkt")

(provide gimplify-function
         (struct-out gctx)
         is-gimple-reg is-gimple-val is-gimple-lvalue is-gimple-reg-type)

;; ------------------------------------------------------------
;; Status codes (enum gimplify_status)
;; ------------------------------------------------------------
(define GS-ERROR -2) (define GS-UNHANDLED -1) (define GS-OK 0) (define GS-ALL-DONE 1)

;; fallback_t
(define fb-none 0) (define fb-rvalue 1) (define fb-lvalue 2) (define fb-either 3) (define fb-mayfail 4)
(define (fb? fallback bit) (not (zero? (bitwise-and fallback bit))))

;; ------------------------------------------------------------
;; Context
;; ------------------------------------------------------------
(struct gctx (prev
              [bind-stack #:mutable]
              [temps #:mutable]              ; newest first
              [conditions #:mutable]
              [conditional-cleanups #:mutable]
              [exit-label #:mutable]
              [return-temp #:mutable]
              [case-labels #:mutable]        ; #f or list (newest first)
              [in-switch-expr #:mutable]
              into-ssa
              allow-rhs-cond-expr
              [in-cleanup-point-expr #:mutable]
              [in-handler-expr #:mutable]
              [save-stack #:mutable]
              [keep-stack #:mutable]))

(define the-ctx (make-parameter #f))
(define (ctx) (the-ctx))

(define (push-gimplify-context! into-ssa rhs-cond-ok)
  (the-ctx (gctx (the-ctx) '() '() 0 (make-seq) #f #f #f #f into-ssa rhs-cond-ok #f #f #f #f)))

;; pop_gimplify_context: temporaries go to BODY's GIMPLE_BIND.
(define (pop-gimplify-context! body)
  (define c (ctx))
  (unless (null? (gctx-bind-stack c)) (error 'pop-gimplify-context "bind stack not empty"))
  (the-ctx (gctx-prev c))
  (when body (declare-vars! (gctx-temps c) body)))

;; declare_vars (debug_info = false): temps (newest first) reversed, then existing vars
(define (declare-vars! temps bind)
  (unless (null? temps)
    (set-gbind-vars! bind (append (reverse temps) (gbind-vars bind)))))

(define (gimple-push-bind-expr! b) (set-gctx-bind-stack! (ctx) (cons b (gctx-bind-stack (ctx)))))
(define (gimple-pop-bind-expr!) (set-gctx-bind-stack! (ctx) (cdr (gctx-bind-stack (ctx)))))
(define (gimple-conditional-context) (> (gctx-conditions (ctx)) 0))
(define (gimple-push-condition!) (set-gctx-conditions! (ctx) (add1 (gctx-conditions (ctx)))))
(define (gimple-pop-condition! pre)
  (define c (ctx))
  (set-gctx-conditions! c (sub1 (gctx-conditions c)))
  (when (zero? (gctx-conditions c))
    (seq-add-seq! pre (gctx-conditional-cleanups c))
    (set-gctx-conditional-cleanups! c (make-seq))))

;; gimple_add_tmp_var
(define (gimple-add-tmp-var! tmp)
  (set-attr! tmp 'context (cons 'function_decl (decl-uid (current-function-decl))))
  (set-decl-seen-in-bind-expr! tmp #t)
  (set-gctx-temps! (ctx) (cons tmp (gctx-temps (ctx)))))

(define (create-tmp-var type [prefix #f])
  (define v (create-tmp-var-raw type prefix))
  (gimple-add-tmp-var! v)
  v)
(define create-tmp-reg create-tmp-var)

;; ------------------------------------------------------------
;; Predicates (gcc/gimple-expr.cc, gimple-expr.h, tree.cc)
;; ------------------------------------------------------------

(define (is-gimple-reg-type type) (not (aggregate-type-p type)))

(define (is-gimple-variable t)
  (and t (memq (tree-code t) '(var_decl parm_decl result_decl ssa_name)) #t))

(define (is-gimple-id t)
  (or (is-gimple-variable t)
      (and (memq (tree-code t) '(function_decl label_decl const_decl string_cst)) #t)))

(define (is-global-var t) (or (tree-static t) (decl-external t)))

;; needs_to_live_in_memory (tree-ssa.cc)
(define (needs-to-live-in-memory t)
  (or (tree-addressable t)
      (and (decl-p t) (is-global-var t))
      (and (eq? (tree-code t) 'result_decl) (aggregate-value-p (tree-type t)))))

;; aggregate_value_p: target dependent; AArch64 AAPCS64 returns
;; aggregates larger than 16 bytes in memory.
(define aggregate-return-byte-limit (make-parameter 16))
(define (aggregate-value-p type)
  (and (aggregate-type-p type)
       (let ([sz (type-size-unit type)])
         (or (not sz) (not (eq? (tree-code sz) 'integer_cst))
             (> (integer-cst-value sz) (aggregate-return-byte-limit))))))

(define (is-gimple-reg t)
  (cond
    [(not t) #f]
    [(eq? (tree-code t) 'ssa_name) #t]
    [(not (is-gimple-variable t)) #f]
    [(not (is-gimple-reg-type (tree-type t))) #f]
    [(tree-this-volatile t) #f]
    [(needs-to-live-in-memory t) #f]
    [else (not (decl-not-gimple-reg t))]))

(define (is-gimple-constant t)
  (and t (memq (tree-code t) '(integer_cst poly_int_cst real_cst fixed_cst complex_cst vector_cst string_cst)) #t))

(define (decl-function-context-is-current? d)
  (define c (attr d 'context #f))
  (and c (eq? (car c) 'function_decl) (eqv? (cdr c) (decl-uid (current-function-decl)))))

;; decl_address_invariant_p (tree.cc)
(define (decl-address-invariant-p op)
  (case (tree-code op)
    [(parm_decl result_decl label_decl function_decl) #t]
    [(var_decl) (or (tree-static op) (decl-external op) (decl-function-context-is-current? op))]
    [(const_decl) (or (tree-static op) (decl-external op) (decl-function-context-is-current? op))]
    [else #f]))

(define (strip-invariant-refs op)
  (let loop ([op op])
    (cond
      [(not (handled-component-p op)) op]
      [else
       (case (tree-code op)
         [(array_ref array_range_ref)
          (if (or (not (is-gimple-constant (tree-operand op 1))) (tree-operand op 2) (tree-operand op 3))
              #f
              (loop (tree-operand op 0)))]
         [(component_ref)
          (if (tree-operand op 2) #f (loop (tree-operand op 0)))]
         [else (loop (tree-operand op 0))])])))

(define (is-gimple-invariant-address t)
  (and (eq? (tree-code t) 'addr_expr)
       (let ([op (strip-invariant-refs (tree-operand t 0))])
         (and op
              (if (eq? (tree-code op) 'mem_ref)
                  (let ([op0 (tree-operand op 0)])
                    (and (eq? (tree-code op0) 'addr_expr)
                         (or (constant-class-p (tree-operand op0 0)) (decl-address-invariant-p (tree-operand op0 0)))))
                  (or (constant-class-p op) (decl-address-invariant-p op)))))))

(define (is-gimple-min-invariant t)
  (if (eq? (tree-code t) 'addr_expr) (is-gimple-invariant-address t) (is-gimple-constant t)))

(define (is-gimple-val t)
  (cond
    [(and (is-gimple-variable t) (is-gimple-reg-type (tree-type t)) (not (is-gimple-reg t))) #f]
    [else (or (is-gimple-variable t) (is-gimple-min-invariant t))]))

(define (is-gimple-addressable t)
  (or (is-gimple-id t) (handled-component-p t) (memq (tree-code t) '(target_mem_ref mem_ref)) #t))
(define (is-gimple-lvalue t)
  (or (is-gimple-addressable t) (and (memq (tree-code t) '(with_size_expr bit_field_ref)) #t)))
(define (is-gimple-min-lval t)
  (define s (strip-invariant-refs t))
  (and s (or (is-gimple-id s) (eq? (tree-code s) 'mem_ref))))
(define (is-gimple-call-addr t) (or (eq? (tree-code t) 'obj_type_ref) (is-gimple-val t)))
(define (is-gimple-mem-ref-addr t)
  (or (is-gimple-reg t)
      (eq? (tree-code t) 'integer_cst)
      (and (eq? (tree-code t) 'addr_expr)
           (or (constant-class-p (tree-operand t 0)) (decl-address-invariant-p (tree-operand t 0))))))
;; is_gimple_condexpr_for_cond: no traps allowed; C without -fexceptions never "throws"
(define (is-gimple-condexpr-for-cond t)
  (or (is-gimple-val t)
      (and (comparison-class-p t)
           (is-gimple-val (tree-operand t 0))
           (is-gimple-val (tree-operand t 1)))))
(define (is-gimple-reg-rhs t) (not (eq? (gimple-rhs-class (tree-code t)) 'invalid)))
(define (is-gimple-reg-rhs-or-call t) (or (is-gimple-reg-rhs t) (eq? (tree-code t) 'call_expr)))
(define (is-gimple-mem-rhs t)
  (if (is-gimple-reg-type (tree-type t)) (is-gimple-val t) (or (is-gimple-val t) (is-gimple-lvalue t))))
(define (is-gimple-mem-rhs-or-call t)
  (if (is-gimple-reg-type (tree-type t))
      (is-gimple-val t)
      (or (is-gimple-val t) (is-gimple-lvalue t)
          (and (eq? (tree-code t) 'constructor) (null? (constructor-elts t)))
          (eq? (tree-code t) 'call_expr))))
(define (rhs-predicate-for lhs) (if (is-gimple-reg lhs) is-gimple-reg-rhs-or-call is-gimple-mem-rhs-or-call))
(define (initial-rhs-predicate-for lhs) (if (is-gimple-reg-type (tree-type lhs)) is-gimple-reg-rhs-or-call is-gimple-mem-rhs-or-call))

;; is_gimple_stmt (gimplify.cc)
(define (is-gimple-stmt t)
  (case (tree-code t)
    [(nop_expr) (is-empty-stmt t)]
    [(bind_expr cond_expr) (or (not (tree-type t)) (void-type-p (tree-type t)))]
    [(switch_expr goto_expr return_expr label_expr case_label_expr try_catch_expr try_finally_expr
      eh_filter_expr catch_expr asm_expr statement_list) #t]
    [(call_expr modify_expr predict_expr) #t]
    [else #f]))

;; get_name (tree.cc)
(define (get-name t)
  (define s (strip-nops t))
  (cond
    [(eq? (tree-code s) 'ssa_name)
     (or (attr s 'identifier #f) (let ([v (ssa-name-var s)]) (and v (decl-name v))))]
    [(and (decl-p s) (decl-name s)) (decl-name s)]
    [(eq? (tree-code s) 'addr_expr) (get-name (tree-operand s 0))]
    [else #f]))

(define (mark-addressable! x)
  (let loop ([x x])
    (cond
      [(eq? (tree-code x) 'with_size_expr) (loop (tree-operand x 0))]
      [(handled-component-p x) (loop (tree-operand x 0))]
      [(and (memq (tree-code x) '(mem_ref target_mem_ref)) (eq? (tree-code (tree-operand x 0)) 'addr_expr))
       (loop (tree-operand (tree-operand x 0) 0))]
      [(memq (tree-code x) '(var_decl parm_decl result_decl)) (set-tree-addressable! x #t)]
      [else (void)])))

;; ------------------------------------------------------------
;; Sequences helpers
;; ------------------------------------------------------------

(define (gimplify-and-add t seq) (gimplify-stmt (box-ref (box t)) seq))

;; Returns #t if statements were added.
(define (gimplify-stmt r seq)
  (define n (seq-length seq))
  (gimplify-expr r seq #f is-gimple-stmt fb-none)
  (not (= n (seq-length seq))))

;; ------------------------------------------------------------
;; Temporaries
;; ------------------------------------------------------------

(define (create-tmp-from-val val)
  (create-tmp-var (type-main-variant (tree-type val)) (get-name val)))

;; lookup_tmp_var: !optimize -> never reuse
(define (lookup-tmp-var val is-formal not-gimple-reg)
  (define v (create-tmp-from-val val))
  (when not-gimple-reg (set-attr! v 'not-gimple-reg #t))
  v)

(define (internal-get-tmp-var val pre post is-formal allow-ssa not-gimple-reg)
  (define vb (box val))
  (gimplify-expr (box-ref vb) pre post is-gimple-reg-rhs-or-call fb-rvalue)
  (define val* (unbox vb))
  (define t
    (cond
      [(and allow-ssa (gctx-into-ssa (ctx)) (is-gimple-reg-type (tree-type val*)))
       (define name (get-name val*))
       (make-ssa-name (type-main-variant (tree-type val*)) #:name (and name (create-tmp-var-name name)))]
      [else (lookup-tmp-var val* is-formal not-gimple-reg)]))
  (define mod (build2 'init_expr (tree-type t) t val*))
  (gimplify-and-add mod pre)
  t)

(define (get-formal-tmp-var val pre) (internal-get-tmp-var val pre #f #t #t #f))
(define (get-initialized-tmp-var val pre [post #f] [allow-ssa #t]) (internal-get-tmp-var val pre post #f allow-ssa #f))

;; ------------------------------------------------------------
;; voidify_wrapper_expr
;; ------------------------------------------------------------
(define (voidify-wrapper-expr wrapper temp)
  (define type (tree-type wrapper))
  (cond
    [(and type (not (void-type-p type)))
     (define p (box-ref (box wrapper)))
     (let loop ()
       (define x (ref-get p))
       (when (and p x)
         (case (tree-code x)
           [(bind_expr)
            (set-tree-side-effects! x #t) (set-tree-type! x (void-type))
            (set! p (op-ref x 1)) (loop)]
           [(cleanup_point_expr try_finally_expr try_catch_expr)
            (set-tree-side-effects! x #t) (set-tree-type! x (void-type))
            (set! p (op-ref x 0)) (loop)]
           [(statement_list)
            (set-tree-side-effects! x #t) (set-tree-type! x (void-type))
            (define l (statement-list-stmts x))
            (cond
              [(null? l) (set! p #f)]
              [else
               (define i (sub1 (length l)))
               (set! p (ref (lambda () (list-ref (statement-list-stmts x) i))
                            (lambda (v) (set-statement-list-stmts! x (list-set (statement-list-stmts x) i v)))))
               (loop)])]
           [(compound_expr)
            (let cloop ([q p])
              (define y (ref-get q))
              (cond
                [(eq? (tree-code y) 'compound_expr)
                 (set-tree-side-effects! y #t) (set-tree-type! y (void-type))
                 (cloop (op-ref y 1))]
                [else (set! p q)]))
            (loop)]
           [else
            (cond
              [(eq? x wrapper)
               (set-tree-side-effects! x #t) (set-tree-type! x (void-type))
               (set! p (op-ref x 0)) (loop)]
              [else (void)])])))
     (cond
       [(or (not p) (is-empty-stmt (ref-get p))) #f]
       [temp
        (set-tree-operand! temp 1 (ref-get p))
        (ref-set! p temp)
        temp]
       [else
        (define tmp (create-tmp-var type "retval"))
        (ref-set! p (build2 'init_expr type tmp (ref-get p)))
        tmp])]
    [else #f]))

;; ------------------------------------------------------------
;; gimplify_bind_expr (no clobbers/stack save in this port yet)
;; ------------------------------------------------------------
(define (gimplify-bind-expr r pre)
  (define bind-expr (ref-get r))
  (define temp (voidify-wrapper-expr bind-expr #f))
  (for ([t (bind-expr-vars bind-expr)])
    (when (var-p t) (set-decl-seen-in-bind-expr! t #t)))
  (define bind-stmt (gimple-build-bind (bind-expr-vars bind-expr) #f (bind-expr-block bind-expr)))
  (gimple-push-bind-expr! bind-stmt)
  (define body (make-seq))
  (gimplify-stmt (op-ref bind-expr 1) body)
  (set-gbind-body! bind-stmt body)
  ;; TODO(roadmap 6.3): CLOBBER cleanup for non-register locals (-fstack-reuse) -> GIMPLE_TRY_FINALLY
  (gimple-pop-bind-expr!)
  (seq-add! pre bind-stmt)
  (cond
    [temp (ref-set! r temp) GS-OK]
    [else (ref-set! r #f) GS-ALL-DONE]))

;; ------------------------------------------------------------
;; gimplify_return_expr
;; ------------------------------------------------------------
(define (maybe-add-early-return-predict-stmt! pre)
  (when (gimple-conditional-context)
    (seq-add! pre (gimple-build-predict 'PRED_TREE_EARLY_RETURN 'NOT_TAKEN))))

(define (gimplify-return-expr stmt pre)
  (define ret-expr (tree-operand stmt 0))
  (cond
    [(or (not ret-expr) (eq? (tree-code ret-expr) 'result_decl))
     (maybe-add-early-return-predict-stmt! pre)
     (seq-add! pre (gimple-build-return ret-expr))
     GS-ALL-DONE]
    [else
     (define fn (current-function-decl))
     (define result-decl
       (cond
         [(void-type-p (tree-type (tree-type fn))) #f]
         [(eq? (tree-code ret-expr) 'compound_expr) (decl-result fn)]
         [else
          (define rd (tree-operand ret-expr 0))
          (if (indirect-ref-p rd) (tree-operand rd 0) rd)]))
     (define result
       (cond
         [(not result-decl) #f]
         [(aggregate-value-p (tree-type result-decl)) result-decl]
         [(gctx-return-temp (ctx)) (gctx-return-temp (ctx))]
         [else
          (define t (create-tmp-reg (tree-type result-decl)))
          (set-gctx-return-temp! (ctx) t)
          t]))
     (when (and result (not (eq? result result-decl)))
       (set-tree-operand! ret-expr 0 result))
     (gimplify-and-add (tree-operand stmt 0) pre)
     (maybe-add-early-return-predict-stmt! pre)
     (seq-add! pre (gimple-build-return result))
     GS-ALL-DONE]))

;; ------------------------------------------------------------
;; gimplify_decl_expr
;; ------------------------------------------------------------
(define (gimplify-decl-expr r seq)
  (define stmt (ref-get r))
  (define decl (decl-expr-decl stmt))
  (ref-set! r #f)
  (when (and (var-p decl) (not (decl-external decl)))
    (define init (decl-initial decl))
    (when (and (not (decl-seen-in-bind-expr decl)) (decl-artificial decl) (not (decl-name decl)))
      (gimple-add-tmp-var! decl))
    (when init
      (cond
        [(not (tree-static decl))
         (set-decl-initial! decl #f)
         (define ie (build2 'init_expr (void-type) decl init))
         (gimplify-and-add ie seq)
         (unless (decl-initial decl) (set-attr! decl 'readonly #f))]
        [else (void)])))
  GS-ALL-DONE)

;; ------------------------------------------------------------
;; LOOP_EXPR / STATEMENT_LIST / SWITCH / labels / EXIT
;; ------------------------------------------------------------
(define (gimplify-loop-expr r pre)
  (define saved (gctx-exit-label (ctx)))
  (define start (create-artificial-label))
  (seq-add! pre (gimple-build-label start))
  (set-gctx-exit-label! (ctx) #f)
  (gimplify-and-add (tree-operand (ref-get r) 0) pre)
  (seq-add! pre (gimple-build-goto start))
  (when (gctx-exit-label (ctx)) (seq-add! pre (gimple-build-label (gctx-exit-label (ctx)))))
  (set-gctx-exit-label! (ctx) saved)
  (ref-set! r #f)
  GS-ALL-DONE)

(define (gimplify-statement-list r pre)
  (define sl (ref-get r))
  (define temp (voidify-wrapper-expr sl #f))
  (let loop ()
    (define l (statement-list-stmts sl))
    (unless (null? l)
      (define b (box (car l)))
      (gimplify-stmt (box-ref b) pre)
      (set-statement-list-stmts! sl (cdr (statement-list-stmts sl)))
      (loop)))
  (cond [temp (ref-set! r temp) GS-OK]
        [else GS-ALL-DONE]))

;; preprocess_case_label_vec_for_gimple (subset): sort by CASE_LOW, split out default
(define (preprocess-case-labels labels)
  (define default #f)
  (define cases
    (for/list ([l labels] #:unless (and (not (case-low l)) (begin (set! default l) #t))) l))
  (values (sort cases < #:key (lambda (c) (integer-cst-value (case-low c)))) default))

(define (gimplify-switch-expr r pre)
  (define sw (ref-get r))
  (define ret (gimplify-expr (op-ref sw 0) pre #f is-gimple-val fb-rvalue))
  (cond
    [(or (= ret GS-ERROR) (= ret GS-UNHANDLED)) ret]
    [else
     (define body-seq (make-seq))
     (define saved-labels (gctx-case-labels (ctx)))
     (set-gctx-case-labels! (ctx) '())
     (define old-in-switch (gctx-in-switch-expr (ctx)))
     (set-gctx-in-switch-expr! (ctx) #t)
     (gimplify-stmt (op-ref sw 1) body-seq)
     (set-gctx-in-switch-expr! (ctx) old-in-switch)
     (define labels (reverse (gctx-case-labels (ctx))))
     (set-gctx-case-labels! (ctx) saved-labels)
     (define-values (cases default-case) (preprocess-case-labels labels))
     (define add-bind #f)
     (define default*
       (cond
         [default-case
          (when old-in-switch
            (define lastg (seq-last body-seq))
            (when (and lastg (glabel? lastg) (attr (glabel-label lastg) 'switch-break-label #f))
              (set! add-bind #t)))
          default-case]
         [else
          (define lbl (create-artificial-label))
          (define dc (build3 'case_label_expr (void-type) #f #f lbl))
          (when old-in-switch (set-attr! lbl 'switch-break-label #t) (set! add-bind #t))
          (seq-add! body-seq (gimple-build-label lbl))
          dc]))
     (define switch-stmt (gimple-build-switch (tree-operand sw 0) default* cases))
     (cond
       [add-bind
        (define bb (make-seq))
        (seq-add! bb switch-stmt)
        (seq-add-seq! bb body-seq)
        (seq-add! pre (gimple-build-bind '() bb #f))]
       [else
        (seq-add! pre switch-stmt)
        (seq-add-seq! pre body-seq)])
     GS-ALL-DONE]))

(define (gimplify-label-expr r pre)
  (seq-add! pre (gimple-build-label (label-expr-label (ref-get r))))
  GS-ALL-DONE)

(define (gimplify-case-label-expr r pre)
  (define e (ref-get r))
  (let loop ([c (ctx)])
    (cond
      [(not c) (error 'gimplify-case-label-expr "case label outside switch")]
      [(gctx-case-labels c) (set-gctx-case-labels! c (cons e (gctx-case-labels c)))]
      [else (loop (gctx-prev c))]))
  (seq-add! pre (gimple-build-label (case-label e)))
  GS-ALL-DONE)

;; build_and_jump: label-ref is a ref to a label slot (or #f)
(define (build-and-jump label-ref)
  (cond
    [(not label-ref) #f]
    [else
     (unless (ref-get label-ref) (ref-set! label-ref (create-artificial-label)))
     (build-goto (ref-get label-ref))]))

(define (gimplify-exit-expr r)
  (define cnd (tree-operand (ref-get r) 0))
  (define lb (box (gctx-exit-label (ctx))))
  (define e (build-and-jump (box-ref lb)))
  (set-gctx-exit-label! (ctx) (unbox lb))
  (ref-set! r (build3 'cond_expr (void-type) cnd e #f))
  GS-OK)

;; ------------------------------------------------------------
;; Conversions, decls, compound lvalues, self-modify, args
;; ------------------------------------------------------------
(define (canonicalize-component-ref! r)
  (define expr (ref-get r))
  (define field (tree-operand expr 1))
  (define type (tree-type field))
  (unless (eq? (tree-type expr) type)
    ;; bit-fields keep their narrowed type in GCC; plain fields take the field type
    (unless (attr field 'bit-field #f)
      (set-tree-type! expr type))))

(define (canonicalize-addr-expr! r)
  (define expr (ref-get r))
  (define addr (tree-operand expr 0))
  (when (and (pointer-type-p (tree-type expr)) (eq? (tree-code addr) 'addr_expr))
    (define datype (tree-type (tree-type addr)))
    (when (eq? (tree-code datype) 'array_type)
      (define ddatype (tree-type datype))
      (define dom (type-domain datype))
      (when (and dom (type-min-value dom) (eq? (tree-code (type-min-value dom)) 'integer_cst)
                 (useless-type-conversion-p (type-main-variant (tree-type expr)) (pointer-to ddatype)))
        (define aref (build4 'array_ref ddatype (tree-operand addr 0) (type-min-value dom) #f #f))
        (define n (build1 'addr_expr (pointer-to ddatype) aref))
        (ref-set! r (if (useless-type-conversion-p (tree-type expr) (tree-type n)) n (fold-convert (tree-type expr) n)))))))

;; build_pointer_type (synthesized pointer types are compared through useless_type_conversion_p)
(define pointer-cache (make-hasheq))
(define (pointer-to type)
  (hash-ref! pointer-cache type
             (lambda () (make-tree 'pointer_type #:type type #:attrs (list (cons 'uid (next-uid!)) (cons 'mode "DI") (cons 'quals 0) (cons 'align 64))))))

(define (gimplify-conversion r)
  (define e (ref-get r))
  (strip-sign-nops! (op-ref e 0))
  (when (tree-ssa-useless-type-conversion (ref-get r))
    (ref-set! r (tree-operand (ref-get r) 0)))
  (let ([e (ref-get r)])
    (when (convert-expr-p e)
      (define sub (tree-operand e 0))
      (cond
        [(eq? (tree-code sub) 'component_ref) (canonicalize-component-ref! (op-ref e 0))]
        [(eq? (tree-code sub) 'addr_expr) (canonicalize-addr-expr! r)])))
  (let ([e (ref-get r)])
    (when (and (convert-expr-p e) (not (is-gimple-reg-type (tree-type e))))
      (ref-set! r (build1 'view_convert_expr (tree-type e) (tree-operand e 0)))))
  (let ([e (ref-get r)])
    (when (eq? (tree-code e) 'convert_expr) (set-tree-code! e 'nop_expr)))
  GS-OK)

(define (gimplify-var-or-parm-decl r)
  (define decl (ref-get r))
  (cond
    [(attr decl 'value-expr #f) (ref-set! r (attr decl 'value-expr)) GS-OK]
    [else GS-ALL-DONE]))

;; fold_indirect_ref_loc (subset): *&x -> x
(define (fold-indirect-ref t)
  (define sub (tree-operand t 0))
  (define subtype (tree-type sub))
  (cond
    [(and (eq? (tree-code sub) 'addr_expr) (eq? (tree-code (tree-operand sub 0)) 'array_type)) t]
    [(eq? (tree-code sub) 'addr_expr)
     (define op (tree-operand sub 0))
     (cond
       [(types-compatible-p (tree-type t) (tree-type op)) op]
       [else t])]
    [else t]))

(define (prepare-gimple-addressable! r pre)
  (let loop ([r r])
    (define e (ref-get r))
    (cond
      [(handled-component-p e) (loop (op-ref e 0))]
      [(is-gimple-reg e) (ref-set! r (internal-get-tmp-var e pre #f #f #f #t))]
      [else (void)])))

(define (gimplify-compound-lval r pre post fallback)
  (define loc (tree-loc (ref-get r)))
  (define expr-stack '())
  (define p r)
  (let loop ()
    (define x (ref-get p))
    (when (eq? (tree-code x) 'indirect_ref)
      (ref-set! p (fold-indirect-ref x)))
    (define x* (ref-get p))
    (cond
      [(handled-component-p x*)
       (set! expr-stack (cons x* expr-stack))
       (set! p (op-ref x* 0))
       (loop)]
      [(and (memq (tree-code x*) '(var_decl parm_decl)) (= (gimplify-var-or-parm-decl p) GS-OK)) (loop)]
      [else (void)]))
  ;; expr-stack is innermost-first here; GCC's vector is outermost-first.
  (define stack-outer-first (reverse expr-stack))
  (define need-non-reg #f)
  (for ([t stack-outer-first])
    (case (tree-code t)
      [(array_ref array_range_ref) (set! need-non-reg #t)]
      [(component_ref) (set! need-non-reg #t)]
      [else (unless (is-gimple-reg-type (tree-type t)) (set! need-non-reg #t))]))
  (define ret (gimplify-expr p pre post is-gimple-min-lval (bitwise-ior fallback fb-lvalue)))
  (cond
    [(= ret GS-ERROR) GS-ERROR]
    [else
     (when (and need-non-reg (fb? fallback fb-rvalue)) (prepare-gimple-addressable! p pre))
     (define reg-operations (is-gimple-reg (ref-get p)))
     (define ret2
       (for/fold ([ret ret]) ([t (in-list expr-stack)])     ; innermost first (pop order)
         (define r1
           (case (tree-code t)
             [(array_ref array_range_ref)
              (define a (gimplify-expr (op-ref t 2) pre post is-gimple-reg fb-rvalue))
              (define b (gimplify-expr (op-ref t 3) pre post is-gimple-reg fb-rvalue))
              (define c (gimplify-expr (op-ref t 1) pre post is-gimple-val fb-rvalue))
              (min a b c)]
             [(component_ref) (gimplify-expr (op-ref t 2) pre post is-gimple-reg fb-rvalue)]
             [else (if reg-operations (gimplify-expr (op-ref t 0) pre post is-gimple-val fb-rvalue) GS-ALL-DONE)]))
         (strip-useless-type-conversion! (op-ref t 0))
         (recalculate-side-effects! t)
         (min ret r1)))
     (when (and (fb? fallback fb-rvalue) (eq? (tree-code (ref-get r)) 'component_ref))
       (canonicalize-component-ref! r))
     ret2]))

(define (gimplify-self-mod-expr r pre post want-value arith-type)
  (define e (ref-get r))
  (define code (tree-code e))
  (define postfix (and (memq code '(postincrement_expr postdecrement_expr)) want-value))
  (define post* (if postfix (make-seq) post))
  (define arith-code (if (memq code '(preincrement_expr postincrement_expr)) 'plus_expr 'minus_expr))
  (define lvb (box (tree-operand e 0)))
  (define ret (gimplify-expr (box-ref lvb) pre post* is-gimple-lvalue fb-lvalue))
  (cond
    [(= ret GS-ERROR) ret]
    [else
     (define lvalue (unbox lvb))
     (define lhs lvalue)
     (define rhs (tree-operand e 1))
     (when postfix
       (define lb (box lhs))
       (gimplify-expr (box-ref lb) pre post* is-gimple-val fb-rvalue)
       (set! lhs (get-initialized-tmp-var (unbox lb) pre)))
     (define t1
       (cond
         [(pointer-type-p (tree-type lhs))
          (define rhs1 (convert-to-ptrofftype rhs))
          (define rhs2 (if (eq? arith-code 'minus_expr) (fold-build1 'negate_expr (tree-type rhs1) rhs1) rhs1))
          (fold-build2 'pointer_plus_expr (tree-type e) lhs rhs2)]
         [else
          (fold-convert (tree-type e)
                        (fold-build2 arith-code arith-type (fold-convert arith-type lhs) (fold-convert arith-type rhs)))]))
     (cond
       [postfix
        (gimplify-and-add (build2 'modify_expr (tree-type lvalue) lvalue t1) pre)
        (seq-add-seq! post post*)
        (ref-set! r lhs)
        GS-ALL-DONE]
       [else
        (ref-set! r (build2 'modify_expr (tree-type lvalue) lvalue t1))
        GS-OK])]))

(define (gimplify-arg r pre [allow-ssa #t])
  (define a (ref-get r))
  (define-values (test fb)
    (if (is-gimple-reg-type (tree-type a))
        (values is-gimple-val fb-rvalue)
        (values is-gimple-lvalue fb-either)))
  (gimplify-expr r pre #f test fb))

;; ------------------------------------------------------------
;; gimplify_call_expr (no builtin folding)
;; ------------------------------------------------------------
(define ECF-CONST 1) (define ECF-PURE 2) (define ECF-LOOPING 4) (define ECF-NORETURN 8)
(define (call-expr-flags c) (attr c 'flags 0))

(define (get-callee-fndecl c)
  (define fn (call-expr-fn c))
  (and fn (eq? (tree-code fn) 'addr_expr) (eq? (tree-code (tree-operand fn 0)) 'function_decl) (tree-operand fn 0)))

(define (gimplify-call-expr r pre fallback)
  (define want-value (not (= fallback fb-none)))
  (define e (ref-get r))
  (cond
    [(call-expr-internal? e)
     (cond
       [want-value GS-ALL-DONE]
       [else
        (for ([i (in-range (call-expr-nargs e))]) (gimplify-arg (op-ref e (+ 3 i)) pre))
        (seq-add! pre (gimple-build-call-from-tree e #f))
        GS-ALL-DONE])]
    [else
     (define fnptrtype (tree-type (call-expr-fn e)))
     (define ret (gimplify-expr (op-ref e 1) pre #f is-gimple-call-addr fb-rvalue))
     (cond
       [(= ret GS-ERROR) GS-ERROR]
       [else
        (define nargs (call-expr-nargs e))
        ;; PUSH_ARGS_REVERSED: gimplify the last argument first
        (define ret2
          (for/fold ([ret ret]) ([i (in-range (sub1 nargs) -1 -1)])
            (define t (gimplify-arg (op-ref e (+ 3 i)) pre))
            (if (= t GS-ERROR) GS-ERROR ret)))
        (cond
          [(= ret2 GS-ERROR) (ref-set! r #f) GS-ERROR]
          [else
           (define flags (call-expr-flags e))
           (when (and (not (zero? (bitwise-and flags (bitwise-ior ECF-CONST ECF-PURE))))
                      (zero? (bitwise-and flags ECF-LOOPING)))
             (set-tree-side-effects! e #f))
           (cond
             [(not want-value)
              (define call (gimple-build-call-from-tree e fnptrtype))
              (seq-add! pre call)
              (ref-set! r #f)]
             [else
              (set-tree-operand! e 1 (build1 'nop_expr fnptrtype (call-expr-fn e)))])
           ret2])])]))

;; ------------------------------------------------------------
;; Short-circuit conditions
;; ------------------------------------------------------------
(define (find-goto e)
  (cond
    [(not e) #f]
    [(eq? (tree-code e) 'goto_expr) e]
    [(not (eq? (tree-code e) 'statement_list)) #f]
    [else
     (define l (filter (lambda (s) (not (eq? (tree-code s) 'debug_begin_stmt))) (statement-list-stmts e)))
     (and (= (length l) 1) (find-goto (car l)))]))
(define (find-goto-label e)
  (define d (find-goto e))
  (and d (eq? (tree-code (goto-destination d)) 'label_decl) d))

;; rexpr_location: location of e, looking through a one-statement list
(define (rexpr-location e)
  (cond
    [(not e) #f]
    [(eq? (tree-code e) 'statement_list)
     (define l (filter (lambda (s) (not (eq? (tree-code s) 'debug_begin_stmt))) (statement-list-stmts e)))
     (and (= (length l) 1) (rexpr-location (car l)))]
    [else (tree-loc e)]))

;; block_may_fallthru (tree.cc), C subset
(define (block-may-fallthru e)
  (cond
    [(not e) #t]
    [else
     (case (tree-code e)
       [(goto_expr return_expr) #f]
       [(cond_expr) (or (block-may-fallthru (tree-operand e 1)) (block-may-fallthru (tree-operand e 2)))]
       [(switch_expr) #t]
       [(loop_expr) #f]
       [(bind_expr) (block-may-fallthru (bind-expr-body e))]
       [(statement_list) (block-may-fallthru (expr-last e))]
       [(cleanup_point_expr) (block-may-fallthru (tree-operand e 0))]
       [(call_expr) (zero? (bitwise-and (call-expr-flags e) ECF-NORETURN))]
       [(modify_expr) (if (eq? (tree-code (tree-operand e 1)) 'call_expr) (block-may-fallthru (tree-operand e 1)) #t)]
       [else #t])]))

(define (shortcut-cond-r pred true-label-ref false-label-ref locus)
  (define local-label (box #f))
  (define expr (box #f))
  (define code (tree-code pred))
  (cond
    [(eq? code 'truth_andif_expr)
     (when (not false-label-ref) (set! false-label-ref (box-ref local-label)))
     (append-to-statement-list! (shortcut-cond-r (tree-operand pred 0) #f false-label-ref locus) (box-ref expr))
     (append-to-statement-list! (shortcut-cond-r (tree-operand pred 1) true-label-ref false-label-ref (or (tree-loc pred) locus)) (box-ref expr))]
    [(eq? code 'truth_orif_expr)
     (when (not true-label-ref) (set! true-label-ref (box-ref local-label)))
     (append-to-statement-list! (shortcut-cond-r (tree-operand pred 0) true-label-ref #f locus) (box-ref expr))
     (append-to-statement-list! (shortcut-cond-r (tree-operand pred 1) true-label-ref false-label-ref (or (tree-loc pred) locus)) (box-ref expr))]
    [(and (eq? code 'cond_expr)
          (not (void-type-p (tree-type (tree-operand pred 1))))
          (not (void-type-p (tree-type (tree-operand pred 2)))))
     (set-box! expr (build3 'cond_expr (void-type) (tree-operand pred 0)
                            (shortcut-cond-r (tree-operand pred 1) true-label-ref false-label-ref locus)
                            (shortcut-cond-r (tree-operand pred 2) true-label-ref false-label-ref (or (tree-loc pred) locus))))]
    [else
     (define c (build3 'cond_expr (void-type) pred (build-and-jump true-label-ref) (build-and-jump false-label-ref)))
     (set-tree-loc! c locus)
     (set-box! expr c)])
  (when (unbox local-label)
    (append-to-statement-list! (build-label-expr (unbox local-label)) (box-ref expr)))
  (unbox expr))

(define (shortcut-cond-expr expr)
  (define pred (tree-operand expr 0))
  (define then_ (tree-operand expr 1))
  (define else_ (tree-operand expr 2))
  (define then-se (and then_ (tree-side-effects then_)))
  (define else-se (and else_ (tree-side-effects else_)))
  (when (not else-se)
    (let loop ()
      (when (eq? (tree-code pred) 'truth_andif_expr)
        (define locus (tree-loc expr))
        (set-tree-operand! expr 0 (tree-operand pred 1))
        (when (tree-loc pred) (set-tree-loc! expr (tree-loc pred)))
        (set! then_ (shortcut-cond-expr expr))
        (set! then-se (and then_ (tree-side-effects then_)))
        (set! pred (tree-operand pred 0))
        (set! expr (build3 'cond_expr (void-type) pred then_ #f))
        (set-tree-loc! expr locus)
        (loop))))
  (when (not then-se)
    (let loop ()
      (when (eq? (tree-code pred) 'truth_orif_expr)
        (define locus (tree-loc expr))
        (set-tree-operand! expr 0 (tree-operand pred 1))
        (when (tree-loc pred) (set-tree-loc! expr (tree-loc pred)))
        (set! else_ (shortcut-cond-expr expr))
        (set! else-se (and else_ (tree-side-effects else_)))
        (set! pred (tree-operand pred 0))
        (set! expr (build3 'cond_expr (void-type) pred #f else_))
        (set-tree-loc! expr locus)
        (loop))))
  (cond
    [(not (memq (tree-code pred) '(truth_andif_expr truth_orif_expr))) expr]
    [else
     (define true-label (box #f)) (define false-label (box #f)) (define end-label (box #f))
     (let ([g (find-goto-label then_)])
       (when g (set-box! true-label (goto-destination g)) (set! then_ #f) (set! then-se #f)))
     (let ([g (find-goto-label else_)])
       (when g (set-box! false-label (goto-destination g)) (set! else_ #f) (set! else-se #f)))
     (define true-label-ref (if (unbox true-label) (box-ref true-label) #f))
     (define false-label-ref (if (or (unbox false-label) else-se) (box-ref false-label) #f))
     (cond
       [(and (not then-se) (not else-se))
        (shortcut-cond-r pred true-label-ref false-label-ref (tree-loc expr))]
       [else
        (define t (cond [else-se (expr-last else_)] [then-se (expr-last then_)] [else #f]))
        (when (and t (eq? (tree-code t) 'label_expr)) (set-box! end-label (label-expr-label t)))
        (unless false-label-ref (set! false-label-ref (box-ref end-label)))
        (define emit-end (not (unbox end-label)))
        (define emit-false (not (unbox false-label)))
        (define jump-over-else (block-may-fallthru then_))
        (define pred* (shortcut-cond-r pred true-label-ref false-label-ref (tree-loc expr)))
        (define out (box #f))
        (append-to-statement-list! pred* (box-ref out))
        (append-to-statement-list! then_ (box-ref out))
        (when else-se
          (when jump-over-else
            (append-to-statement-list! (build-and-jump (box-ref end-label)) (box-ref out)))
          (when emit-false
            (append-to-statement-list! (build-label-expr (unbox false-label)) (box-ref out)))
          (append-to-statement-list! else_ (box-ref out)))
        (when (and emit-end (unbox end-label))
          (append-to-statement-list! (build-label-expr (unbox end-label)) (box-ref out)))
        (unbox out)])]))

(define (gimple-boolify expr)
  (define type (tree-type expr))
  (case (tree-code expr)
    [(truth_and_expr truth_or_expr truth_xor_expr truth_andif_expr truth_orif_expr)
     (set-tree-operand! expr 1 (gimple-boolify (tree-operand expr 1)))
     (set-tree-operand! expr 0 (gimple-boolify (tree-operand expr 0)))
     (unless (boolean-type-p type) (set-tree-type! expr (boolean-type)))
     expr]
    [(truth_not_expr)
     (set-tree-operand! expr 0 (gimple-boolify (tree-operand expr 0)))
     (unless (boolean-type-p type) (set-tree-type! expr (boolean-type)))
     expr]
    [else
     (cond
       [(comparison-class-p expr)
        (unless (boolean-type-p type) (set-tree-type! expr (boolean-type)))
        expr]
       [(boolean-type-p type) expr]
       [else (fold-convert (boolean-type) expr)])]))

;; generic_expr_could_trap_p: conservative
(define (generic-expr-could-trap-p e)
  (cond
    [(or (not e) (is-gimple-val e)) #f]
    [(not (expr-p e)) #t]
    [(memq (tree-code e) '(mem_ref indirect_ref array_ref component_ref trunc_div_expr trunc_mod_expr
                           exact_div_expr floor_div_expr ceil_div_expr round_div_expr rdiv_expr call_expr)) #t]
    [else (for/or ([i (in-range (tree-operand-length e))]) (generic-expr-could-trap-p (tree-operand e i)))]))

(define (gimplify-pure-cond-expr r pre)
  (define expr (ref-get r))
  (define cnd (gimple-boolify (tree-operand expr 0)))
  (case (tree-code cnd)
    [(truth_andif_expr) (set-tree-code! cnd 'truth_and_expr)]
    [(truth_orif_expr) (set-tree-code! cnd 'truth_or_expr)]
    [else (void)])
  (define cb (box cnd))
  (define ret (gimplify-expr (box-ref cb) pre #f is-gimple-val fb-rvalue))
  (set-tree-operand! expr 0 (unbox cb))
  (define t1 (gimplify-expr (op-ref expr 1) pre #f is-gimple-val fb-rvalue))
  (define t2 (gimplify-expr (op-ref expr 2) pre #f is-gimple-val fb-rvalue))
  (min ret t1 t2))

(define (gimplify-cond-expr r pre fallback)
  (define expr (ref-get r))
  (define type (tree-type expr))
  (cond
    ;; valued COND_EXPR: copy the arms into a temporary
    [(not (void-type-p type))
     (define then_ (tree-operand expr 1)) (define else_ (tree-operand expr 2))
     (define-values (tmp result expr*)
       (cond
         [(and (or (fb? fallback fb-rvalue) (not (fb? fallback fb-lvalue))) (not (tree-addressable type)))
          (cond
            [(and (gctx-allow-rhs-cond-expr (ctx))
                  (not (tree-side-effects then_)) (not (generic-expr-could-trap-p then_))
                  (not (tree-side-effects else_)) (not (generic-expr-could-trap-p else_)))
             (values 'pure #f #f)]
            [else (define t (create-tmp-var type "iftmp")) (values t t expr)])]
         [else
          (define ptype (pointer-to type))
          (define then* (if (void-type-p (tree-type then_)) then_ (build1 'addr_expr ptype then_)))
          (define else* (if (void-type-p (tree-type else_)) else_ (build1 'addr_expr ptype else_)))
          (define e2 (build3 'cond_expr ptype (tree-operand expr 0) then* else*))
          (define t (create-tmp-var ptype "iftmp"))
          (values t (build2 'mem_ref type t (build-int-cst ptype 0)) e2)]))
     (cond
       [(eq? tmp 'pure) (gimplify-pure-cond-expr r pre)]
       [else
        (define e expr*)
        (define ty (tree-type tmp))
        (unless (void-type-p (tree-type (tree-operand e 1)))
          (set-tree-operand! e 1 (build2 'init_expr ty tmp (tree-operand e 1))))
        (unless (void-type-p (tree-type (tree-operand e 2)))
          (set-tree-operand! e 2 (build2 'init_expr ty tmp (tree-operand e 2))))
        (set-tree-type! e (void-type))
        (recalculate-side-effects! e)
        (gimplify-stmt (box-ref (box e)) pre)
        (ref-set! r result)
        GS-ALL-DONE])]
    [else
     (strip-type-nops! (op-ref expr 0))
     (when (eq? (tree-code (tree-operand expr 0)) 'compound_expr)
       (gimplify-compound-expr (op-ref expr 0) pre #t))
     (set-tree-operand! expr 0 (gimple-boolify (tree-operand expr 0)))
     (define c0 (tree-operand expr 0))
     (cond
       [(and (memq (tree-code c0) '(truth_andif_expr truth_orif_expr))
             (let ([e2 (shortcut-cond-expr expr)]) (and (not (eq? e2 expr)) e2)))
        => (lambda (e2)
             (ref-set! r e2)
             (define s (make-seq))
             (gimple-push-condition!)
             (gimplify-stmt r s)
             (gimple-pop-condition! pre)
             (seq-add-seq! pre s)
             GS-ALL-DONE)]
       [else
        (define ret (gimplify-expr (op-ref expr 0) pre #f is-gimple-condexpr-for-cond fb-rvalue))
        (cond
          [(= ret GS-ERROR) GS-ERROR]
          [else
           (gimple-push-condition!)
           (define seq (make-seq))
           (define have-then #f) (define have-else #f)
           (define (hijack? g)
             (and g
                  ;; -O0: only when the COND_EXPR and the GOTO share a location (or have none)
                  (let ([el (tree-loc expr)] [gl (rexpr-location g)])
                    (or (not el) (not gl) (equal? el gl)))))
           (define label-true
             (let ([g (find-goto-label (tree-operand expr 1))])
               (cond [(hijack? g) (set! have-then #t) (goto-destination g)]
                     [else (create-artificial-label)])))
           (define label-false
             (let ([g (find-goto-label (tree-operand expr 2))])
               (cond [(hijack? g) (set! have-else #t) (goto-destination g)]
                     [else (create-artificial-label)])))
           (define-values (pred-code arm1 arm2) (gimple-cond-get-ops-from-tree (tree-operand expr 0)))
           (define cond-stmt (gimple-build-cond pred-code arm1 arm2 label-true label-false))
           (seq-add! seq cond-stmt)
           (maybe-fold-stmt! seq)
           (define label-cont #f)
           (unless have-then
             (cond
               [(and (not (tree-operand expr 1)) (not have-else) (tree-operand expr 2))
                (set! label-cont label-true)]
               [else
                (seq-add! seq (gimple-build-label label-true))
                (set! have-then (gimplify-stmt (op-ref expr 1) seq))
                (when (and (not have-else) (tree-operand expr 2) (gimple-seq-may-fallthru seq))
                  (set! label-cont (create-artificial-label))
                  (seq-add! seq (gimple-build-goto label-cont)))]))
           (unless have-else
             (seq-add! seq (gimple-build-label label-false))
             (set! have-else (gimplify-stmt (op-ref expr 2) seq)))
           (when label-cont (seq-add! seq (gimple-build-label label-cont)))
           (gimple-pop-condition! pre)
           (seq-add-seq! pre seq)
           (define ret*
             (cond
               [(or have-then have-else) GS-ALL-DONE]
               [else
                (define c (tree-operand expr 0))
                (gimplify-stmt (box-ref (box c)) pre)
                ret]))
           (ref-set! r #f)
           ret*])])]))

;; ------------------------------------------------------------
;; MODIFY_EXPR
;; ------------------------------------------------------------
(define (gimplify-modify-expr-rhs expr-ref from-ref to-ref pre post want-value)
  (let loop ([ret GS-UNHANDLED])
    (define from (ref-get from-ref))
    (define changed #f)
    (define ret*
      (case (tree-code from)
        [(indirect_ref)
         (define t (fold-indirect-ref from))
         (cond
           [(and (not (eq? t from)) (not (tree-addressable (tree-type from))))
            (ref-set! from-ref t) (set! changed #t) GS-OK]
           [else ret])]
        [(target_expr)
         (define init (tree-operand from 1))
         (cond
           [(and init (not (void-type-p (tree-type init))))
            (ref-set! from-ref init) (set! changed #t) GS-OK]
           [else ret])]
        [(compound_expr)
         (gimplify-compound-expr from-ref pre #t) (set! changed #t) GS-OK]
        [(constructor)
         (if (not (= ret GS-UNHANDLED))
             ret
             (gimplify-init-constructor expr-ref pre post want-value))]
        [(cond_expr)
         (define to (ref-get to-ref))
         (cond
           [(or (not (is-gimple-reg-type (tree-type from)))
                (and (is-gimple-reg to) (not (gctx-allow-rhs-cond-expr (ctx)))))
            (define code (tree-code (ref-get expr-ref)))
            (define cnd from)
            (define rb (box to))
            (define r1 (gimplify-expr (box-ref rb) pre post is-gimple-lvalue fb-lvalue))
            (define result (unbox rb))
            (define r2 (if (= r1 GS-ERROR) r1 GS-OK))
            (when (and (var-p result)
                       (not (void-type-p (tree-type (tree-operand cnd 1))))
                       (not (void-type-p (tree-type (tree-operand cnd 2)))))
              (set-attr! result 'readonly #f))
            (unless (void-type-p (tree-type (tree-operand cnd 1)))
              (set-tree-operand! cnd 1 (build2 code (void-type) result (tree-operand cnd 1))))
            (unless (void-type-p (tree-type (tree-operand cnd 2)))
              (set-tree-operand! cnd 2 (build2 code (void-type) result (tree-operand cnd 2))))
            (set-tree-type! cnd (void-type))
            (recalculate-side-effects! cnd)
            (cond
              [want-value (gimplify-and-add cnd pre) (ref-set! expr-ref result)]
              [else (ref-set! expr-ref cnd)])
            (cons 'return r2)]
           [else ret])]
        [(cleanup_point_expr bind_expr statement_list)
         (define wrap from)
         (define r1 (gimplify-expr to-ref pre post is-gimple-min-lval fb-lvalue))
         (define t (voidify-wrapper-expr wrap (ref-get expr-ref)))
         (cond
           [want-value (gimplify-and-add wrap pre) (ref-set! expr-ref (ref-get to-ref))]
           [else (ref-set! expr-ref wrap)])
         (cons 'return GS-OK)]
        [(nop_expr)
         (cond
           [(and (eq? (tree-code (tree-operand from 0)) 'compound_literal_expr) (tree-ssa-useless-type-conversion from))
            (ref-set! from-ref (tree-operand from 0)) (set! changed #t) GS-OK]
           [else ret])]
        [(compound_literal_expr)
         (define complit from)
         (define decl-s (tree-operand complit 0))
         (define decl (decl-expr-decl decl-s))
         (define init (decl-initial decl))
         (cond
           [(and (not (tree-addressable complit)) (not (tree-addressable decl)) init)
            (define e (copy-node (ref-get expr-ref)))
            (set-tree-operand! e 1 init)
            (ref-set! expr-ref e)
            (cons 'return GS-OK)]
           [else ret])]
        [else ret]))
    (cond
      [(and (pair? ret*) (eq? (car ret*) 'return)) (cdr ret*)]
      [changed (loop ret*)]
      [else ret*])))

;; gimplify_init_constructor: only the trivial forms for now
(define (gimplify-init-constructor expr-ref pre post want-value)
  (define e (ref-get expr-ref))
  (define ctor (tree-operand e 1))
  (define object (tree-operand e 0))
  (cond
    [(null? (constructor-elts ctor))
     ;; zero-initialization: object = {}  (kept as a CONSTRUCTOR rhs)
     GS-UNHANDLED]
    [else
     ;; element-wise stores (GCC does this for non-constant or small ctors at -O0 via gimplify_init_ctor_eval)
     (define ob (box object))
     (gimplify-expr (box-ref ob) pre post is-gimple-lvalue fb-lvalue)
     (define obj (unbox ob))
     (for ([p (constructor-elts ctor)])
       (define index (car p)) (define val (cdr p))
       (define lhs
         (cond
           [(and index (eq? (tree-code index) 'field_decl)) (build3 'component_ref (tree-type index) obj index #f)]
           [index (build4 'array_ref (tree-type val) obj index #f #f)]
           [else (error 'gimplify-init-constructor "unsupported CONSTRUCTOR element")]))
       (gimplify-and-add (build2 'modify_expr (tree-type lhs) lhs val) pre))
     (cond [want-value (ref-set! expr-ref object) GS-OK]
           [else (ref-set! expr-ref #f) GS-ALL-DONE])]))

;; get_symbol_constant_value (gimple-fold.cc), via maybe_fold_stmt on a fresh assignment
(define (get-symbol-constant-value sym)
  (and (var-p sym) (tree-readonly sym) (not (tree-this-volatile sym))
       (or (tree-static sym) (decl-external sym))
       (let ([val (decl-initial sym)])
         (and val (is-gimple-min-invariant val)
              (useless-type-conversion-p (tree-type sym) (tree-type val))
              val))))

;; tree_swap_operands_p (fold-const.cc): should ARG0 and ARG1 be swapped?
(define (tree-swap-operands-p arg0 arg1)
  (cond
    [(constant-class-p arg1) #f]
    [(constant-class-p arg0) #t]
    [else
     (define a0 (strip-nops arg0)) (define a1 (strip-nops arg1))
     (cond
       [(tree-constant a1) #f]
       [(tree-constant a0) #t]
       [(and (eq? (tree-code a0) 'ssa_name) (eq? (tree-code a1) 'ssa_name))
        (> (ssa-name-version a0) (ssa-name-version a1))]
       [(eq? (tree-code a1) 'ssa_name) #f]
       [(eq? (tree-code a0) 'ssa_name) #t]
       [(decl-p a1) #f]
       [(decl-p a0) #t]
       [else #f])]))

(define (commutative-tree-code code)
  (and (memq code '(plus_expr mult_expr mult_highpart_expr min_expr max_expr bit_ior_expr bit_xor_expr bit_and_expr
                    ne_expr eq_expr unordered_expr ordered_expr uneq_expr ltgt_expr truth_and_expr truth_xor_expr truth_or_expr
                    widen_mult_expr vec_widen_mult_hi_expr vec_widen_mult_lo_expr)) #t))

(define (swap-tree-comparison code)
  (case code [(lt_expr) 'gt_expr] [(gt_expr) 'lt_expr] [(le_expr) 'ge_expr] [(ge_expr) 'le_expr]
    [(unlt_expr) 'ungt_expr] [(ungt_expr) 'unlt_expr] [(unle_expr) 'unge_expr] [(unge_expr) 'unle_expr] [else code]))

;; maybe_fold_stmt -> fold_stmt_1: the parts observable at -O0 on plain C:
;;   - canonical operand order of commutative assigns and of conditions
;;   - loads from readonly symbols with constant initializers
(define (maybe-fold-stmt! pre)
  (define g (seq-last pre))
  (define g*
    (cond
      [(gassign? g)
       (define code (gassign-code g))
       (cond
         [(and (eq? (gimple-rhs-class code) 'single) (get-symbol-constant-value (gassign-rhs1 g)))
          => (lambda (v)
               (define-values (c a b d) (extract-ops-from-tree v))
               (gassign (gassign-lhs g) c a b d))]
         [(and (eq? (gimple-rhs-class code) 'binary) (commutative-tree-code code)
               (tree-swap-operands-p (gassign-rhs1 g) (gassign-rhs2 g)))
          (gassign (gassign-lhs g) code (gassign-rhs2 g) (gassign-rhs1 g) #f)]
         [else g])]
      [(gcond? g)
       (if (tree-swap-operands-p (gcond-lhs g) (gcond-rhs g))
           (gcond (swap-tree-comparison (gcond-code g)) (gcond-rhs g) (gcond-lhs g) (gcond-true-label g) (gcond-false-label g))
           g)]
      [else g]))
  (unless (eq? g g*)
    (set-seq-items! pre (cons g* (cdr (seq-items pre))))))

(define (gimplify-modify-expr r pre post want-value)
  (define e (ref-get r))
  (define from-ref (op-ref e 1))
  (define to-ref (op-ref e 0))
  ;; pointer conversions required by the middle end
  (when (pointer-type-p (tree-type (ref-get to-ref)))
    (strip-useless-type-conversion! from-ref)
    (unless (useless-type-conversion-p (tree-type (ref-get to-ref)) (tree-type (ref-get from-ref)))
      (ref-set! from-ref (fold-convert (tree-type (ref-get to-ref)) (ref-get from-ref)))))
  (define ret0 (gimplify-modify-expr-rhs r from-ref to-ref pre post want-value))
  (cond
    [(not (= ret0 GS-UNHANDLED)) ret0]
    [(and (is-empty-type (tree-type (ref-get from-ref))) (not want-value))
     (gimplify-stmt from-ref pre) (gimplify-stmt to-ref pre) (ref-set! r #f) GS-ALL-DONE]
    [else
     (define initial-pred (initial-rhs-predicate-for (ref-get to-ref)))
     (define r1 (gimplify-expr from-ref pre post initial-pred fb-rvalue))
     (cond
       [(= r1 GS-ERROR) r1]
       [else
        (define r2 (gimplify-expr to-ref pre post is-gimple-lvalue fb-lvalue))
        (cond
          [(= r2 GS-ERROR) r2]
          [else
           (define final-pred (rhs-predicate-for (ref-get to-ref)))
           (define r3 (if (eq? final-pred initial-pred) GS-OK (gimplify-expr from-ref pre post final-pred fb-rvalue)))
           (cond
             [(= r3 GS-ERROR) r3]
             [else
              (define r4 (gimplify-modify-expr-rhs r from-ref to-ref pre post want-value))
              (cond
                [(not (= r4 GS-UNHANDLED)) r4]
                [else
                 (define to (ref-get to-ref))
                 (when (and want-value (tree-this-volatile to))
                   (ref-set! from-ref (get-initialized-tmp-var (ref-get from-ref) pre post)))
                 (define from (ref-get from-ref))
                 (define assign
                   (cond
                     [(eq? (tree-code from) 'call_expr)
                      (cond
                        [(call-expr-internal? from)
                         (for ([i (in-range (call-expr-nargs from))]) (gimplify-arg (op-ref from (+ 3 i)) pre))
                         (define c (gimple-build-call-from-tree from #f))
                         (set-gcall-lhs! c to)
                         c]
                        [else
                         (define fnptrtype (tree-type (call-expr-fn from)))
                         (set-tree-operand! from 1 (tree-operand (call-expr-fn from) 0))
                         (strip-useless-type-conversion! (op-ref from 1))
                         (define c (gimple-build-call-from-tree from fnptrtype))
                         (set-gcall-lhs! c to)
                         c])]
                     [else (gimple-build-assign to from)]))
                 (seq-add! pre assign)
                 (maybe-fold-stmt! pre)
                 (cond
                   [want-value (ref-set! r (if (tree-this-volatile to) from to)) GS-OK]
                   [else (ref-set! r #f) GS-ALL-DONE])])])])])]))

;; ------------------------------------------------------------
;; COMPOUND_EXPR, SAVE_EXPR, ADDR_EXPR, CLEANUP_POINT, TARGET_EXPR
;; ------------------------------------------------------------
(define (gimplify-compound-expr r pre want-value)
  (let loop ([t (ref-get r)])
    (define sub-ref (op-ref t 0))
    (if (eq? (tree-code (ref-get sub-ref)) 'compound_expr)
        (gimplify-compound-expr sub-ref pre #f)
        (gimplify-stmt sub-ref pre))
    (define next (tree-operand t 1))
    (cond
      [(eq? (tree-code next) 'compound_expr) (loop next)]
      [else (ref-set! r next)]))
  (cond
    [want-value GS-OK]
    [else (gimplify-stmt r pre) GS-ALL-DONE]))

(define (gimplify-save-expr r pre post)
  (define e (ref-get r))
  (define val (tree-operand e 0))
  (cond
    [(attr e 'resolved #f) (ref-set! r val) GS-ALL-DONE]
    [else
     (define v
       (cond
         [(void-type-p (tree-type val))
          (gimplify-expr (op-ref e 0) pre post is-gimple-stmt fb-none) #f]
         [else (get-initialized-tmp-var val pre post #f)]))
     (set-tree-operand! e 0 v)
     (set-attr! e 'resolved #t)
     (ref-set! r v)
     GS-ALL-DONE]))

(define (gimplify-addr-expr r pre post)
  (define expr (ref-get r))
  (define op0 (tree-operand expr 0))
  (define (do-indirect-ref op0)
    (define op00 (tree-operand op0 0))
    (ref-set! r (if (useless-type-conversion-p (tree-type expr) (tree-type op00)) op00 (fold-convert (tree-type expr) op00)))
    GS-OK)
  (case (tree-code op0)
    [(indirect_ref) (do-indirect-ref op0)]
    [(view_convert_expr)
     (define inner (if (tree-ssa-useless-type-conversion (tree-operand op0 0)) (tree-operand (tree-operand op0 0) 0) (tree-operand op0 0)))
     (ref-set! r (fold-convert (tree-type expr) (build1 'addr_expr (pointer-to (tree-type inner)) inner)))
     GS-OK]
    [else
     (cond
       [(and (eq? (tree-code op0) 'mem_ref) (integer-zerop (tree-operand op0 1))) (do-indirect-ref op0)]
       [else
        (define ret (gimplify-expr (op-ref expr 0) pre post is-gimple-addressable fb-either))
        (cond
          [(= ret GS-ERROR) ret]
          [else
           (prepare-gimple-addressable! (op-ref expr 0) pre)
           (define op0* (tree-operand expr 0))
           (cond
             [(or (indirect-ref-p op0*) (and (eq? (tree-code op0*) 'mem_ref) (integer-zerop (tree-operand op0* 1))))
              (do-indirect-ref op0*)]
             [else
              (mark-addressable! op0*)
              (set-attr! expr 'constant (is-gimple-invariant-address expr))
              (recalculate-side-effects! expr)
              ret])])])]))

(define (gimplify-cleanup-point-expr r pre)
  (define e (ref-get r))
  (define temp (voidify-wrapper-expr e #f))
  (define body (make-seq))
  (define old-conds (gctx-conditions (ctx)))
  (define old-cleanups (gctx-conditional-cleanups (ctx)))
  (define old-in (gctx-in-cleanup-point-expr (ctx)))
  (set-gctx-conditions! (ctx) 0) (set-gctx-conditional-cleanups! (ctx) (make-seq)) (set-gctx-in-cleanup-point-expr! (ctx) #t)
  (gimplify-stmt (op-ref e 0) body)
  (set-gctx-conditions! (ctx) old-conds) (set-gctx-conditional-cleanups! (ctx) old-cleanups) (set-gctx-in-cleanup-point-expr! (ctx) old-in)
  (seq-add-seq! pre body)
  (cond [temp (ref-set! r temp) GS-OK]
        [else (ref-set! r #f) GS-ALL-DONE]))

(define (gimplify-target-expr r pre post)
  (define targ (ref-get r))
  (define temp (tree-operand targ 0))
  (define init (tree-operand targ 1))
  (when init
    (gimple-add-tmp-var! temp)
    (define ib (box (if (void-type-p (tree-type init)) init (build2 'init_expr (void-type) temp init))))
    (gimplify-expr (box-ref ib) pre post is-gimple-stmt fb-none)
    (set-tree-operand! targ 1 #f))
  (ref-set! r temp)
  GS-OK)

;; ------------------------------------------------------------
;; c_gimplify_expr (lang hook)
;; ------------------------------------------------------------
(define int-precision 32)
(define (c-promoting-integer-type-p type)
  (and (integral-type-p type) (< (type-precision type) int-precision)))
(define unsigned-cache (make-hasheq))
(define (unsigned-type-for type)
  (if (type-unsigned type)
      type
      (hash-ref! unsigned-cache (type-main-variant type)
                 (lambda ()
                   (define t (copy-node (type-main-variant type)))
                   (set-attr! t 'uid (next-uid!))
                   (set-attr! t 'unsigned #t)
                   (hash-remove! (T-attrs t) 'main-variant)
                   (define nm (attr t 'name #f))
                   (define nstr (and nm (if (eq? (tree-code nm) 'type_decl) (decl-name nm) (attr nm 'id #f))))
                   (set-attr! t 'name (make-tree 'identifier_node #:attrs (list (cons 'id (format "unsigned ~a" (or nstr "int"))))))
                   t))))

(define (c-gimplify-expr r pre post)
  (define e (ref-get r))
  (case (tree-code e)
    [(lshift_expr rshift_expr lrotate_expr rrotate_expr)
     (define op1 (tree-operand e 1))
     (define t (type-main-variant (tree-type op1)))
     (unless (or (vector-type-p t) (and (integral-type-p t) (eqv? (type-precision t) int-precision) (not (boolean-type-p t))))
       (set-tree-operand! e 1 (fold-convert (unsigned-type-for (size-type-int)) op1)))
     GS-UNHANDLED]
    [(preincrement_expr predecrement_expr postincrement_expr postdecrement_expr)
     (define type (tree-type (tree-operand e 0)))
     (cond
       [(and (integral-type-p type) (c-promoting-integer-type-p type))
        (gimplify-self-mod-expr r pre post #t (if (type-unsigned type) type (unsigned-type-for type)))]
       [else GS-UNHANDLED])]
    [else GS-UNHANDLED]))

;; an `int`-like type for shift counts: reuse the first 32-bit signed integer type we can find
(define (size-type-int) (size-type))

;; ------------------------------------------------------------
;; gimplify_expr
;; ------------------------------------------------------------
(define (gimplify-expr r pre post test fallback)
  (define save-expr0 (ref-get r))
  (cond
    [(not save-expr0) GS-ALL-DONE]
    [else
     (define is-statement (eq? test is-gimple-stmt))
     (define internal-pre (make-seq))
     (define internal-post (make-seq))
     (define pre* (or pre internal-pre))
     (define post* (or post internal-post))
     (define ret
       (let loop ()
         (strip-useless-type-conversion! r)
         (define save-expr (ref-get r))
         (define lang-ret (c-gimplify-expr r pre* post*))
         (cond
           [(and (= lang-ret GS-OK) (not (ref-get r))) GS-OK]
           [(and (= lang-ret GS-OK) (not (eq? (ref-get r) save-expr))) (loop)]
           [(and (not (= lang-ret GS-UNHANDLED)) (not (= lang-ret GS-OK))) lang-ret]
           [else
            (define e (ref-get r))
            (define code (tree-code e))
            (define ret
              (case code
                [(postincrement_expr postdecrement_expr preincrement_expr predecrement_expr)
                 (gimplify-self-mod-expr r pre* post* (not (= fallback fb-none)) (tree-type e))]
                [(view_convert_expr)
                 (cond
                   [(and (fb? fallback fb-rvalue) (is-gimple-reg-type (tree-type e)) (is-gimple-reg-type (tree-type (tree-operand e 0))))
                    (define rr (gimplify-expr (op-ref e 0) pre* post* is-gimple-val fb-rvalue))
                    (recalculate-side-effects! e)
                    rr]
                   [else (gimplify-compound-lval r pre* post* (if (zero? fallback) fb-rvalue fallback))])]
                [(array_ref array_range_ref realpart_expr imagpart_expr component_ref)
                 (gimplify-compound-lval r pre* post* (if (zero? fallback) fb-rvalue fallback))]
                [(cond_expr)
                 (define rr (gimplify-cond-expr r pre* fallback))
                 (cond
                   [(= fallback fb-lvalue)
                    (ref-set! r (get-initialized-tmp-var (ref-get r) pre* post* #f))
                    (mark-addressable! (ref-get r))
                    GS-OK]
                   [else rr])]
                [(call_expr)
                 (define rr (gimplify-call-expr r pre* fallback))
                 (cond
                   [(= fallback fb-lvalue)
                    (ref-set! r (get-initialized-tmp-var (ref-get r) pre* post* #f))
                    (mark-addressable! (ref-get r))
                    GS-OK]
                   [else rr])]
                [(compound_expr) (gimplify-compound-expr r pre* (not (= fallback fb-none)))]
                [(modify_expr init_expr) (gimplify-modify-expr r pre* post* (not (= fallback fb-none)))]
                [(truth_andif_expr truth_orif_expr)
                 (define org-type (tree-type e))
                 (define b (gimple-boolify e))
                 (ref-set! r (build3 'cond_expr org-type b
                                     (fold-convert org-type (build-int-cst (boolean-type) 1))
                                     (fold-convert org-type (build-int-cst (boolean-type) 0))))
                 GS-OK]
                [(truth_not_expr)
                 (define type (tree-type e))
                 (define b (gimple-boolify e))
                 (define n
                   (if (eqv? (type-precision (tree-type b)) 1)
                       (build1 'bit_not_expr (tree-type b) (tree-operand b 0))
                       (build2 'bit_xor_expr (tree-type b) (tree-operand b 0) (build-int-cst (tree-type b) 1))))
                 (ref-set! r (if (useless-type-conversion-p type (tree-type n)) n (fold-convert type n)))
                 GS-OK]
                [(addr_expr) (gimplify-addr-expr r pre* post*)]
                [(nop_expr convert_expr)
                 (cond
                   [(is-empty-stmt e) GS-ALL-DONE]
                   [(or (void-type-p (tree-type e)) (= fallback fb-none))
                    (ref-set! r (tree-operand e 0)) GS-OK]
                   [else
                    (define rr (gimplify-conversion r))
                    (cond
                      [(= rr GS-ERROR) rr]
                      [(not (eq? (ref-get r) save-expr)) rr]
                      [else
                       (define rr2 (gimplify-expr (op-ref (ref-get r) 0) pre* post* is-gimple-val fb-rvalue))
                       (recalculate-side-effects! (ref-get r))
                       rr2])])]
                [(fix_trunc_expr)
                 (define rr (gimplify-expr (op-ref e 0) pre* post* is-gimple-val fb-rvalue))
                 (recalculate-side-effects! e) rr]
                [(indirect_ref)
                 (define volatilep (tree-this-volatile e))
                 (define saved-ptr-type (tree-type (tree-operand e 0)))
                 (define f (fold-indirect-ref e))
                 (cond
                   [(not (eq? f e)) (ref-set! r f) GS-OK]
                   [else
                    (define rr (gimplify-expr (op-ref e 0) pre* post* is-gimple-reg fb-rvalue))
                    (cond
                      [(= rr GS-ERROR) rr]
                      [else
                       (recalculate-side-effects! e)
                       (define m (build2 'mem_ref (tree-type e) (tree-operand e 0) (build-int-cst saved-ptr-type 0)))
                       (when volatilep (set-attr! m 'volatile #t))
                       (recalculate-side-effects! m)
                       (ref-set! r m)
                       GS-OK])])]
                [(mem_ref)
                 ;; fold MEM[&x, 0] -> x
                 (define a (tree-operand e 0)) (define off (tree-operand e 1))
                 (cond
                   [(and (eq? (tree-code a) 'addr_expr) (integer-zerop off)
                         (types-compatible-p (tree-type e) (tree-type (tree-operand a 0))))
                    (ref-set! r (tree-operand a 0)) GS-OK]
                   [else
                    (define rr (gimplify-expr (op-ref e 0) pre* post* is-gimple-mem-ref-addr fb-rvalue))
                    (cond [(= rr GS-ERROR) rr]
                          [else (recalculate-side-effects! e) GS-ALL-DONE])])]
                [(integer_cst real_cst fixed_cst string_cst complex_cst vector_cst) GS-ALL-DONE]
                [(const_decl)
                 (cond [(fb? fallback fb-lvalue) GS-ALL-DONE]
                       [else (ref-set! r (decl-initial e)) GS-OK])]
                [(decl_expr) (gimplify-decl-expr r pre*)]
                [(bind_expr) (gimplify-bind-expr r pre*)]
                [(loop_expr) (gimplify-loop-expr r pre*)]
                [(switch_expr) (gimplify-switch-expr r pre*)]
                [(exit_expr) (gimplify-exit-expr r)]
                [(goto_expr)
                 (define rr
                   (if (eq? (tree-code (goto-destination e)) 'label_decl)
                       GS-OK
                       (gimplify-expr (op-ref e 0) pre* #f is-gimple-val fb-rvalue)))
                 (cond [(= rr GS-ERROR) rr]
                       [else (seq-add! pre* (gimple-build-goto (goto-destination e))) GS-ALL-DONE])]
                [(predict_expr)
                 (seq-add! pre* (gimple-build-predict (attr e 'predictor) (attr e 'outcome))) GS-ALL-DONE]
                [(label_expr) (gimplify-label-expr r pre*)]
                [(case_label_expr) (gimplify-case-label-expr r pre*)]
                [(return_expr) (gimplify-return-expr e pre*)]
                [(constructor)
                 (cond
                   [(= fallback fb-none)
                    (define temp (box #f))
                    (for ([p (constructor-elts e)])
                      (when (tree-side-effects (cdr p)) (append-to-statement-list! (cdr p) (box-ref temp))))
                    (ref-set! r (unbox temp))
                    (if (unbox temp) GS-OK GS-ALL-DONE)]
                   [(= fallback fb-lvalue)
                    (ref-set! r (get-initialized-tmp-var e pre* post* #f))
                    (mark-addressable! (ref-get r))
                    GS-OK]
                   [else GS-ALL-DONE])]
                [(save_expr) (gimplify-save-expr r pre* post*)]
                [(bit_field_ref)
                 (define rr (gimplify-expr (op-ref e 0) pre* post* is-gimple-lvalue fb-either))
                 (recalculate-side-effects! e) rr]
                [(cleanup_point_expr) (gimplify-cleanup-point-expr r pre*)]
                [(target_expr) (gimplify-target-expr r pre* post*)]
                [(try_finally_expr try_catch_expr)
                 ;; EH not modelled: evaluate body, then cleanup, in sequence
                 (gimplify-and-add (tree-operand e 0) pre*)
                 (gimplify-and-add (tree-operand e 1) pre*)
                 (ref-set! r #f) GS-ALL-DONE]
                [(label_decl) (set-attr! e 'forced #t) GS-ALL-DONE]
                [(statement_list) (gimplify-statement-list r pre*)]
                [(var_decl parm_decl) (gimplify-var-or-parm-decl r)]
                [(result_decl ssa_name) GS-ALL-DONE]
                [(debug_begin_stmt) (ref-set! r #f) GS-ALL-DONE]
                [(truth_and_expr truth_or_expr truth_xor_expr)
                 (define orig-type (tree-type e))
                 (define b (gimple-boolify e))
                 (define new-type (tree-type b))
                 (cond
                   [(not (useless-type-conversion-p orig-type new-type))
                    (ref-set! r (fold-convert orig-type b)) GS-OK]
                   [else
                    (set-tree-code! b (case (tree-code b) [(truth_and_expr) 'bit_and_expr] [(truth_or_expr) 'bit_ior_expr] [else 'bit_xor_expr]))
                    (for ([i '(0 1)])
                      (unless (useless-type-conversion-p new-type (tree-type (tree-operand b i)))
                        (set-tree-operand! b i (fold-convert new-type (tree-operand b i)))))
                    (expr-2 b pre* post*)])]
                [(pointer_plus_expr) (expr-2 e pre* post*)]
                [else
                 (case (tree-code-class code)
                   [(tcc_comparison)
                    (define type (tree-type (tree-operand e 1)))
                    (cond
                      [(vector-type-p type) (expr-2 e pre* post*)]
                      [(not (aggregate-type-p type))
                       (define org-type (tree-type e))
                       (define b (gimple-boolify e))
                       (cond
                         [(not (useless-type-conversion-p org-type (tree-type b)))
                          (ref-set! r (fold-convert org-type b)) GS-OK]
                         [else (expr-2 b pre* post*)])]
                      [else (error 'gimplify-expr "aggregate comparison not supported")])]
                   [(tcc_unary)
                    (define rr (gimplify-expr (op-ref e 0) pre* post* is-gimple-val fb-rvalue))
                    (recalculate-side-effects! e) rr]
                   [(tcc_binary) (expr-2 e pre* post*)]
                   [(tcc_declaration tcc_constant) GS-ALL-DONE]
                   [else (error 'gimplify-expr "unhandled tree code ~a (~a)" code (tree-code-class code))])]))
            (if (= ret GS-OK) (if (ref-get r) (loop) GS-OK) ret)])))
     (cond
       [(= ret GS-ERROR) (when is-statement (ref-set! r #f)) ret]
       [else
        (define e (ref-get r))
        ;; statement context: discard a non-statement without side effects
        (when (and (= fallback fb-none) e (not (is-gimple-stmt e)))
          (cond
            [(or (eq? (tree-code e) 'label_decl) (not (tree-side-effects e))) (ref-set! r #f)]
            [(not (tree-this-volatile e))
             (case (tree-code e)
               [(component_ref realpart_expr imagpart_expr view_convert_expr)
                (gimplify-expr (op-ref e 0) pre* post* test fallback)]
               [(array_ref array_range_ref)
                (gimplify-expr (op-ref e 0) pre* post* test fallback)
                (gimplify-expr (op-ref e 1) pre* post* test fallback)]
               [else (error 'gimplify-expr "side-effecting non-statement ~a" (tree-code e))])
             (ref-set! r #f)]
            [else
             (define tmp (create-tmp-var-raw (type-main-variant (tree-type e)) "vol"))
             (gimple-add-tmp-var! tmp)
             (gimplify-and-add (build2 'modify_expr (tree-type tmp) tmp e) pre*)
             (ref-set! r #f)]))
        (cond
          [(or (= fallback fb-none) is-statement)
           (ref-set! r #f)
           (when (or (not (seq-empty? internal-pre)) (not (seq-empty? internal-post)))
             (seq-add-seq! internal-pre internal-post)
             (seq-add-seq! pre* internal-pre))
           ret]
          [(and (seq-empty? internal-post) (test (ref-get r))) ret]
          [else
           (define e (ref-get r))
           (cond
             [(and (fb? fallback fb-lvalue) (seq-empty? internal-post) (is-gimple-addressable e))
              (define ref-type (tree-type e))
              (define tb (box (build1 'addr_expr (pointer-to ref-type) e)))
              (gimplify-expr (box-ref tb) pre* post* is-gimple-reg fb-rvalue)
              (ref-set! r (build2 'mem_ref ref-type (unbox tb) (build-int-cst (pointer-to ref-type) 0)))]
             [(and (fb? fallback fb-rvalue) (is-gimple-reg-rhs-or-call e))
              (ref-set! r (get-formal-tmp-var e pre*))]
             [else (error 'gimplify-expr "gimplification failed for ~a" (tree-code e))])
           (unless (seq-empty? internal-post) (seq-add-seq! pre* internal-post))
           ret])])]))

(define (expr-2 e pre post)
  (define r0 (gimplify-expr (op-ref e 0) pre post is-gimple-val fb-rvalue))
  (define r1 (gimplify-expr (op-ref e 1) pre post is-gimple-val fb-rvalue))
  (recalculate-side-effects! e)
  (min r0 r1))

;; ------------------------------------------------------------
;; gimplify_body / gimplify_function_tree
;; ------------------------------------------------------------
(define (gimplify-function fndecl body)
  (parameterize ([current-function-decl fndecl]
                 [ssa-version-counter (box 0)]
                 [the-ctx #f])
    (push-gimplify-context! #t #f)
    (define seq (make-seq))
    (define b (box body))
    (gimplify-stmt (box-ref b) seq)
    (define outer-bind
      (let ([first (seq-first seq)])
        (cond
          [(and first (gbind? first) (= (seq-length seq) 1)) first]
          [(not first) (define nb (gimple-build-bind '() (make-seq) #f)) nb]
          [else (gimple-build-bind '() seq #f)])))
    (pop-gimplify-context! outer-bind)
    outer-bind))
