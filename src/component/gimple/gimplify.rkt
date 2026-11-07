#lang racket/base

;; ============================================================
;; gimplify: GENERIC -> high GIMPLE (pure, state-threaded)
;; ============================================================
;;
;; Function-by-function port of the C subset of gcc/gimplify.cc and
;; the operand predicates of gcc/gimple-expr.cc (GPLv3+), trunk
;; 17.0.0 (352a0b5fc55).  Names follow GCC's.  Not covered: OpenMP/
;; OpenACC, EH (TRY_*), VLA sizes, asm, va_arg, TARGET_EXPR cleanups,
;; full gimplify_init_constructor, fold_stmt beyond readonly-symbol
;; substitution.
;;
;; Conventions (doc/gimple-functional-ir.md 3):
;;   (gimplify-expr st e pre post test fallback)
;;        -> (values e* ret pre* post* st*)
;;   GCC's `tree *expr_p` is "return the new expression"; the caller
;;   stores it (gimplify-operand does that for an operand slot).
;;   `pre`/`post` are pvectors; a #f `post` means GCC's NULL post_p
;;   (an internal queue is used and folded into `pre`).  Helpers that
;;   only take pre return (values e* ret pre* st*).
;;   GCC's input_location is GS-loc; the gimplify context stack is
;;   GS-ctx (a list of immutable gctx records).
;;
;; Entry: (gimplify-function st fndecl body) -> (values gbind st)
;; ============================================================

(require racket/list racket/match
         "../../kernel/data/data.rkt"
         "../../kernel/ir/gimple/types.rkt"
         "state.rkt"
         "tree.rkt"
         "gimple.rkt")

(provide gimplify-function
         (struct-out gctx)
         is-gimple-reg is-gimple-val is-gimple-lvalue is-gimple-reg-type
         is-gimple-constant is-gimple-min-invariant get-symbol-constant-value tree-swap-operands-p
         is-gimple-condexpr-for-cond decl-address-invariant-p unsigned-type-for)

;; ------------------------------------------------------------
;; Status codes (enum gimplify_status) and fallback_t
;; ------------------------------------------------------------
(define GS-ERROR -2) (define GS-UNHANDLED -1) (define GS-OK 0) (define GS-ALL-DONE 1)
(define fb-none 0) (define fb-rvalue 1) (define fb-lvalue 2) (define fb-either 3) (define fb-mayfail 4)
(define (fb? fallback bit) (not (zero? (bitwise-and fallback bit))))

;; ------------------------------------------------------------
;; Context (struct gimplify_ctx), immutable; the stack is GS-ctx
;; ------------------------------------------------------------
(struct gctx (bind-depth temps conditions conditional-cleanups exit-label return-temp
              case-labels in-switch-expr into-ssa allow-rhs-cond-expr in-cleanup-point-expr temp-table)
  #:prefab)

(define (ctx st) (car (GS-ctx st)))
(define (ctx-update st f) (gs-set-ctx st (cons (f (car (GS-ctx st))) (cdr (GS-ctx st)))))

(define (push-gimplify-context st into-ssa rhs-cond-ok)
  (gs-set-ctx st (cons (gctx 0 (pvector-empty) 0 (pvector-empty) #f #f #f #f into-ssa rhs-cond-ok #f (hash)) (GS-ctx st))))

;; pop_gimplify_context: temporaries go to BODY's GIMPLE_BIND (declare_vars,
;; debug_info = false: temps in creation order, then the existing vars)
(define (pop-gimplify-context st bind)
  (define c (ctx st))
  (unless (zero? (gctx-bind-depth c)) (error 'pop-gimplify-context "bind stack not empty"))
  (define st1 (gs-set-ctx st (cdr (GS-ctx st))))
  (define temps (gctx-temps c))
  (values (if (or (not bind) (pvector-empty? temps))
              bind
              (gbind (gbind-loc bind) (gbind-flags bind) (pvector-append temps (gbind-vars bind)) (gbind-body bind) (gbind-block bind)))
          st1))

(define (gimple-push-bind-expr st) (ctx-update st (lambda (c) (struct-copy gctx c [bind-depth (add1 (gctx-bind-depth c))]))))
(define (gimple-pop-bind-expr st) (ctx-update st (lambda (c) (struct-copy gctx c [bind-depth (sub1 (gctx-bind-depth c))]))))
(define (gimple-conditional-context st) (> (gctx-conditions (ctx st)) 0))
(define (gimple-push-condition st) (ctx-update st (lambda (c) (struct-copy gctx c [conditions (add1 (gctx-conditions c))]))))
;; gimple_pop_condition: conditional cleanups are flushed to PRE
(define (gimple-pop-condition st pre)
  (define c (ctx st))
  (define n (sub1 (gctx-conditions c)))
  (cond
    [(zero? n)
     (values (pvector-append pre (gctx-conditional-cleanups c))
             (ctx-update st (lambda (c) (struct-copy gctx c [conditions 0] [conditional-cleanups (pvector-empty)]))))]
    [else (values pre (ctx-update st (lambda (c) (struct-copy gctx c [conditions n]))))]))

;; gimple_add_tmp_var
(define (gimple-add-tmp-var st tmp)
  (let* ([st (set-decl-flag st tmp 'context (cons 'function_decl (decl-uid (GS-fndecl st))))]
         [st (set-decl-flag st tmp 'seen-in-bind-expr #t)])
    (ctx-update st (lambda (c) (struct-copy gctx c [temps (pvector-cons-right (gctx-temps c) tmp)])))))

(define (create-tmp-var st type [prefix #f])
  (let/st st ([v (create-tmp-var-raw st type prefix)])
    (values v (gimple-add-tmp-var st v))))
(define create-tmp-reg create-tmp-var)

;; ------------------------------------------------------------
;; Predicates (gcc/gimple-expr.cc, gimple-expr.h, tree.cc)
;; ------------------------------------------------------------

(define (is-gimple-reg-type st type) (not (aggregate-type-p st type)))

(define (is-gimple-variable t)
  (and t (memq (tree-code t) '(var_decl parm_decl result_decl ssa_name)) #t))

(define (is-gimple-id t)
  (or (is-gimple-variable t)
      (and (memq (tree-code t) '(function_decl label_decl const_decl string_cst)) #t)))

(define (is-global-var t) (or (tree-static t) (decl-external t)))

;; needs_to_live_in_memory (tree-ssa.cc)
(define (needs-to-live-in-memory st t)
  (or (decl-flag st t 'addressable)
      (and (decl-p st t) (is-global-var t))
      (and (eq? (tree-code t) 'result_decl) (aggregate-value-p st (tree-type t)))))

;; aggregate_value_p: target dependent; AArch64 AAPCS64 returns
;; aggregates larger than 16 bytes in memory.
(define aggregate-return-byte-limit 16)
(define (aggregate-value-p st type)
  (and (aggregate-type-p st type)
       (let ([sz (type-attr st type 'size-unit)])
         (or (not sz) (not (eq? (tree-code sz) 'integer_cst))
             (> (integer-cst-value sz) aggregate-return-byte-limit)))))

(define (is-gimple-reg st t)
  (cond
    [(not t) #f]
    [(eq? (tree-code t) 'ssa_name) #t]
    [(not (is-gimple-variable t)) #f]
    [(not (is-gimple-reg-type st (tree-type t))) #f]
    [(tree-this-volatile t) #f]
    [(needs-to-live-in-memory st t) #f]
    [else (not (decl-flag st t 'not-gimple-reg))]))

(define (is-gimple-constant t)
  (and t (memq (tree-code t) '(integer_cst poly_int_cst real_cst fixed_cst complex_cst vector_cst string_cst)) #t))

(define (decl-function-context-is-current? st d)
  (define c (decl-flag st d 'context #f))
  (and c (eq? (car c) 'function_decl) (eqv? (cdr c) (decl-uid (GS-fndecl st)))))

;; decl_address_invariant_p (tree.cc)
(define (decl-address-invariant-p st op)
  (case (tree-code op)
    [(parm_decl result_decl label_decl function_decl) #t]
    [(var_decl const_decl) (or (tree-static op) (decl-external op) (decl-function-context-is-current? st op))]
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
         [(component_ref) (if (tree-operand op 2) #f (loop (tree-operand op 0)))]
         [else (loop (tree-operand op 0))])])))

(define (is-gimple-invariant-address st t)
  (and (eq? (tree-code t) 'addr_expr)
       (let ([op (strip-invariant-refs (tree-operand t 0))])
         (and op
              (if (eq? (tree-code op) 'mem_ref)
                  (let ([op0 (tree-operand op 0)])
                    (and (eq? (tree-code op0) 'addr_expr)
                         (or (constant-class-p st (tree-operand op0 0)) (decl-address-invariant-p st (tree-operand op0 0)))))
                  (or (constant-class-p st op) (decl-address-invariant-p st op)))))))

(define (is-gimple-min-invariant st t)
  (if (eq? (tree-code t) 'addr_expr) (is-gimple-invariant-address st t) (is-gimple-constant t)))

(define (is-gimple-val st t)
  (cond
    [(and (is-gimple-variable t) (is-gimple-reg-type st (tree-type t)) (not (is-gimple-reg st t))) #f]
    [else (or (is-gimple-variable t) (is-gimple-min-invariant st t))]))

(define (is-gimple-addressable st t)
  (or (is-gimple-id t) (handled-component-p t) (and (memq (tree-code t) '(target_mem_ref mem_ref)) #t)))
(define (is-gimple-lvalue st t)
  (or (is-gimple-addressable st t) (and (memq (tree-code t) '(with_size_expr bit_field_ref)) #t)))
(define (is-gimple-min-lval st t)
  (define s (strip-invariant-refs t))
  (and s (or (is-gimple-id s) (eq? (tree-code s) 'mem_ref))))
(define (is-gimple-call-addr st t) (or (eq? (tree-code t) 'obj_type_ref) (is-gimple-val st t)))
(define (is-gimple-mem-ref-addr st t)
  (or (is-gimple-reg st t)
      (eq? (tree-code t) 'integer_cst)
      (and (eq? (tree-code t) 'addr_expr)
           (or (constant-class-p st (tree-operand t 0)) (decl-address-invariant-p st (tree-operand t 0))))))
;; is_gimple_condexpr_for_cond: no traps allowed; C without -fexceptions never "throws"
(define (is-gimple-condexpr-for-cond st t)
  (or (is-gimple-val st t)
      (and (comparison-class-p st t)
           (is-gimple-val st (tree-operand t 0))
           (is-gimple-val st (tree-operand t 1)))))
(define (is-gimple-reg-rhs st t) (not (eq? (gimple-rhs-class st (tree-code t)) 'invalid)))
(define (is-gimple-reg-rhs-or-call st t) (or (is-gimple-reg-rhs st t) (eq? (tree-code t) 'call_expr)))
(define (is-gimple-mem-rhs st t)
  (if (is-gimple-reg-type st (tree-type t)) (is-gimple-val st t) (or (is-gimple-val st t) (is-gimple-lvalue st t))))
(define (is-gimple-mem-rhs-or-call st t)
  (if (is-gimple-reg-type st (tree-type t))
      (is-gimple-val st t)
      (or (is-gimple-val st t) (is-gimple-lvalue st t)
          (and (eq? (tree-code t) 'constructor) (pvector-empty? (constructor-elts t)))
          (eq? (tree-code t) 'call_expr))))
(define (rhs-predicate-for st lhs) (if (is-gimple-reg st lhs) is-gimple-reg-rhs-or-call is-gimple-mem-rhs-or-call))
(define (initial-rhs-predicate-for st lhs) (if (is-gimple-reg-type st (tree-type lhs)) is-gimple-reg-rhs-or-call is-gimple-mem-rhs-or-call))

;; is_gimple_stmt (gimplify.cc)
(define (is-gimple-stmt st t)
  (case (tree-code t)
    [(nop_expr) (is-empty-stmt st t)]
    [(bind_expr cond_expr) (or (not (tree-type t)) (void-type-p st (tree-type t)))]
    [(switch_expr goto_expr return_expr label_expr case_label_expr try_catch_expr try_finally_expr
      eh_filter_expr catch_expr asm_expr statement_list) #t]
    [(call_expr modify_expr predict_expr) #t]
    [else #f]))

;; get_name (tree.cc)
(define (get-name st t)
  (define s (strip-nops st t))
  (cond
    [(eq? (tree-code s) 'ssa_name)
     (or (ssa-name-identifier s) (let ([v (ssa-name-var s)]) (and v (decl-name v))))]
    [(and (decl-p st s) (decl-name s)) (decl-name s)]
    [(eq? (tree-code s) 'addr_expr) (get-name st (tree-operand s 0))]
    [else #f]))

;; mark_addressable: sets TREE_ADDRESSABLE on the base declaration
(define (mark-addressable st x)
  (let loop ([x x])
    (cond
      [(eq? (tree-code x) 'with_size_expr) (loop (tree-operand x 0))]
      [(handled-component-p x) (loop (tree-operand x 0))]
      [(and (memq (tree-code x) '(mem_ref target_mem_ref)) (eq? (tree-code (tree-operand x 0)) 'addr_expr))
       (loop (tree-operand (tree-operand x 0) 0))]
      [(memq (tree-code x) '(var_decl parm_decl result_decl)) (set-decl-flag st x 'addressable #t)]
      [else st])))

;; ------------------------------------------------------------
;; Sequence helpers
;; ------------------------------------------------------------

;; gimplify_stmt: (values added? seq st)
(define (gimplify-stmt st t seq)
  (define n (pvector-length seq))
  (define-values (e ret seq1 post st1) (gimplify-expr st t seq #f is-gimple-stmt fb-none))
  (values (not (= n (pvector-length seq1))) seq1 st1))

;; gimplify_and_add: (values seq st)
(define (gimplify-and-add st t seq)
  (define-values (added seq1 st1) (gimplify-stmt st t seq))
  (values seq1 st1))

;; gimplify operand I of E: (values e* ret pre post st)
(define (gimplify-operand st e i pre post test fb)
  (define-values (o ret pre1 post1 st1) (gimplify-expr st (tree-operand e i) pre post test fb))
  (values (tree-set-operand e i o) ret pre1 post1 st1))

;; ------------------------------------------------------------
;; Temporaries
;; ------------------------------------------------------------

(define (create-tmp-from-val st val)
  (create-tmp-var st (type-main-variant st (tree-type val)) (get-name st val)))

;; lookup_tmp_var: when optimizing, a formal temporary for a side-effect
;; free value is reused for an equal value (GCC hashes the tree; values
;; compare with equal? here); otherwise a fresh one
(define (lookup-tmp-var st val is-formal not-gimple-reg)
  (cond
    [(or (not (optimize? st)) (not is-formal) (tree-side-effects val))
     (let/st st ([v (create-tmp-from-val st val)])
       (values v (if not-gimple-reg (set-decl-flag st v 'not-gimple-reg #t) st)))]
    [(hash-ref (gctx-temp-table (ctx st)) val #f) => (lambda (v) (values v st))]
    [else
     (let/st st ([v (create-tmp-from-val st val)])
       (values v (ctx-update st (lambda (c) (struct-copy gctx c [temp-table (hash-set (gctx-temp-table c) val v)])))))]))

;; internal_get_tmp_var: (values tmp pre post st)
(define (internal-get-tmp-var st val pre post is-formal allow-ssa not-gimple-reg)
  (let/st st ([(val* ret pre post) (gimplify-expr st val pre post is-gimple-reg-rhs-or-call fb-rvalue)]
              [t (cond
                   [(and allow-ssa (gctx-into-ssa (ctx st)) (is-gimple-reg-type st (tree-type val*)))
                    (define name (get-name st val*))
                    (let/st st ([nm (if name (create-tmp-var-name st name) (values #f st))])
                      (make-ssa-name st (type-main-variant st (tree-type val*)) #:name nm))]
                   [else (lookup-tmp-var st val* is-formal not-gimple-reg)])])
    (define mod (tree-set-loc (build2 st 'init_expr (tree-type t) t val*)
                              (or (tree-loc val*) (loc-spot (GS-loc st)))))
    (let/st st ([pre (gimplify-and-add st mod pre)])
      (values t pre post st))))

(define (get-formal-tmp-var st val pre)
  (define-values (t pre1 post1 st1) (internal-get-tmp-var st val pre #f #t #t #f))
  (values t pre1 st1))
(define (get-initialized-tmp-var st val pre [post #f] [allow-ssa #t])
  (internal-get-tmp-var st val pre post #f allow-ssa #f))

;; ------------------------------------------------------------
;; voidify_wrapper_expr: (values wrapper* temp st)
;; ------------------------------------------------------------
(define (voidify-wrapper-expr st wrapper temp)
  (define type (tree-type wrapper))
  (cond
    [(and type (not (void-type-p st type)))
     (define (void! x) (tree-set-type (tree-set-attr x 'side-effects #t) (void-type st)))
     ;; the terminal position holds X (or #f)
     (define (at-p st x)
       (cond
         [(or (not x) (is-empty-stmt st x)) (values x #f st)]
         [temp (define t* (tree-set-operand temp 1 x)) (values t* t* st)]
         [else
          (let/st st ([tmp (create-tmp-var st type "retval")])
            (values (build2 st 'init_expr type tmp x) tmp st))]))
     (define (descend st x root?)
       (cond
         [(not x) (values x #f st)]
         [else
          (define (through x i)
            (define x1 (void! x))
            (define-values (b r st1) (descend st (tree-operand x1 i) #f))
            (values (tree-set-operand x1 i b) r st1))
          (case (tree-code x)
            [(bind_expr) (through x 1)]
            [(cleanup_point_expr try_finally_expr try_catch_expr) (through x 0)]
            [(statement_list)
             (define x1 (void! x))
             (define l (statement-list-stmts x1))
             (cond
               [(pvector-empty? l) (values x1 #f st)]
               [else
                (define i (sub1 (pvector-length l)))
                (define-values (b r st1) (descend st (pvector-ref l i) #f))
                (values (tree-set-attr x1 'stmts (pvector-set l i b)) r st1)])]
            [(compound_expr) (through x 1)]
            [else (if root? (through x 0) (at-p st x))])]))
     (descend st wrapper #t)]
    [else (values wrapper #f st)]))

;; ------------------------------------------------------------
;; gimplify_bind_expr: (values e* ret pre st)
;; ------------------------------------------------------------
(define (gimplify-bind-expr st bind-expr0 pre)
  (let/st st ([(bind-expr temp) (voidify-wrapper-expr st bind-expr0 #f)])
    (define vars (bind-expr-vars bind-expr))
    (define st1
      (for/fold ([st st]) ([t (in-pvector vars)])
        (if (var-p t) (set-decl-flag st t 'seen-in-bind-expr #t) st)))
    (define st2 (gimple-push-bind-expr st1))
    (let/st st ([(_added body) (gimplify-stmt st2 (tree-operand bind-expr 1) (pvector-empty))])
      ;; Add clobbers for all variables that go out of scope (-fstack-reuse=all
      ;; is the default at every optimization level): only variables that have
      ;; to live in memory, i.e. !is_gimple_reg.
      (define cleanup
        (for/fold ([acc (pvector-empty)]) ([t (in-pvector vars)])
          (if (and (var-p t) (not (is-global-var t)) (decl-function-context-is-current? st t)
                   (not (tree-attr t 'hard-register #f)) (not (tree-this-volatile t)) (not (tree-attr t 'value-expr #f))
                   (not (is-gimple-reg st t)))
              (pvector-cons-right acc (gimple-build-assign st t (build-clobber (tree-type t))))
              acc)))
      (define body* (if (pvector-empty? cleanup) body (pvector (gimple-build-try body cleanup 'finally))))
      (define bind-stmt (gimple-build-bind vars body* (bind-expr-block bind-expr)))
      (define st3 (gimple-pop-bind-expr st))
      (define pre* (pvector-cons-right pre bind-stmt))
      (if temp
          (values temp GS-OK pre* st3)
          (values #f GS-ALL-DONE pre* st3)))))

;; ------------------------------------------------------------
;; gimplify_return_expr: (values #f GS-ALL-DONE pre st)
;; ------------------------------------------------------------
(define (maybe-add-early-return-predict-stmt st pre)
  (if (gimple-conditional-context st)
      (pvector-cons-right pre (gimple-build-predict 'PRED_TREE_EARLY_RETURN 'NOT_TAKEN))
      pre))

(define (gimplify-return-expr st stmt pre)
  (define ret-expr (tree-operand stmt 0))
  (cond
    [(or (not ret-expr) (eq? (tree-code ret-expr) 'result_decl))
     (values #f GS-ALL-DONE
             (pvector-cons-right (maybe-add-early-return-predict-stmt st pre) (gimple-build-return ret-expr (tree-loc stmt)))
             st)]
    [else
     (define fn (GS-fndecl st))
     (define result-decl
       (cond
         [(void-type-p st (type-type st (tree-type fn))) #f]
         [(eq? (tree-code ret-expr) 'compound_expr) (decl-result fn)]
         [else
          (define rd (tree-operand ret-expr 0))
          (if (indirect-ref-p rd) (tree-operand rd 0) rd)]))
     (let/st st ([result
                  (cond
                    [(not result-decl) (values #f st)]
                    [(aggregate-value-p st (tree-type result-decl)) (values result-decl st)]
                    [(gctx-return-temp (ctx st)) (values (gctx-return-temp (ctx st)) st)]
                    [else
                     (let/st st ([t (create-tmp-reg st (tree-type result-decl))])
                       (values t (ctx-update st (lambda (c) (struct-copy gctx c [return-temp t])))))])])
       (define ret-expr* (if (and result (not (decl-eq? result result-decl))) (tree-set-operand ret-expr 0 result) ret-expr))
       (let/st st ([pre (gimplify-and-add st ret-expr* pre)])
         (values #f GS-ALL-DONE
                 (pvector-cons-right (maybe-add-early-return-predict-stmt st pre) (gimple-build-return result (tree-loc stmt)))
                 st)))]))

;; ------------------------------------------------------------
;; gimplify_decl_expr: (values #f GS-ALL-DONE seq st)
;; ------------------------------------------------------------
(define (gimplify-decl-expr st stmt seq)
  (define decl (decl-expr-decl stmt))
  (cond
    [(and (var-p decl) (not (decl-external decl)))
     (define init (decl-initial st decl))
     (define st1
       (if (and (not (decl-flag st decl 'seen-in-bind-expr)) (decl-flag st decl 'artificial) (not (decl-name decl)))
           (gimple-add-tmp-var st decl)
           st))
     (cond
       [(and init (not (tree-static decl)))
        (define st2 (set-decl-flag st1 decl 'initial #f))
        (define ie (build2 st2 'init_expr (void-type st2) decl init))
        (let/st st ([seq (gimplify-and-add st2 ie seq)])
          (values #f GS-ALL-DONE seq (if (decl-initial st decl) st (set-decl-flag st decl 'readonly #f))))]
       [else (values #f GS-ALL-DONE seq st1)])]
    [else (values #f GS-ALL-DONE seq st)]))

;; ------------------------------------------------------------
;; LOOP_EXPR / STATEMENT_LIST / SWITCH / labels / EXIT
;; ------------------------------------------------------------
(define (gimplify-loop-expr st e pre)
  (define saved (gctx-exit-label (ctx st)))
  (let/st st ([start (create-artificial-label st)])
    (define st1 (ctx-update st (lambda (c) (struct-copy gctx c [exit-label #f]))))
    (let/st st ([pre (gimplify-and-add st1 (tree-operand e 0) (pvector-cons-right pre (gimple-build-label start)))])
      (define pre1 (pvector-cons-right pre (gimple-build-goto start)))
      (define exit (gctx-exit-label (ctx st)))
      (define pre2 (if exit (pvector-cons-right pre1 (gimple-build-label exit)) pre1))
      (values #f GS-ALL-DONE pre2 (ctx-update st (lambda (c) (struct-copy gctx c [exit-label saved])))))))

(define (gimplify-statement-list st sl0 pre)
  (let/st st ([(sl temp) (voidify-wrapper-expr st sl0 #f)])
    (let/st st ([pre (for/fold ([pre pre] [st st] #:result (values pre st)) ([s (in-pvector (statement-list-stmts sl))])
                       (gimplify-and-add st s pre))])
      (if temp (values temp GS-OK pre st) (values sl GS-ALL-DONE pre st)))))

;; preprocess_case_label_vec_for_gimple (subset): sort by CASE_LOW, split out default
(define (preprocess-case-labels labels)
  (define default (for/first ([l (in-pvector labels)] #:unless (case-low l)) l))
  (define cases (for/list ([l (in-pvector labels)] #:when (case-low l)) l))
  (values (list->pvector (sort cases < #:key (lambda (c) (integer-cst-value (case-low c))))) default))

(define (gimplify-switch-expr st sw pre)
  (let/st st ([(sw ret pre _post) (gimplify-operand st sw 0 pre #f is-gimple-val fb-rvalue)])
    (cond
      [(or (= ret GS-ERROR) (= ret GS-UNHANDLED)) (values sw ret pre st)]
      [else
       (define saved-labels (gctx-case-labels (ctx st)))
       (define old-in-switch (gctx-in-switch-expr (ctx st)))
       (define st1 (ctx-update st (lambda (c) (struct-copy gctx c [case-labels (pvector-empty)] [in-switch-expr #t]))))
       (let/st st ([(_added body-seq) (gimplify-stmt st1 (tree-operand sw 1) (pvector-empty))])
         (define labels (gctx-case-labels (ctx st)))
         (define st2 (ctx-update st (lambda (c) (struct-copy gctx c [case-labels saved-labels] [in-switch-expr old-in-switch]))))
         (define-values (cases default-case) (preprocess-case-labels labels))
         (let/st st ([(default* body-seq add-bind)
                      (cond
                        [default-case
                         (define lastg (seq-last body-seq))
                         (values default-case body-seq
                                 (and old-in-switch lastg (glabel? lastg) (decl-flag st2 (glabel-label lastg) 'switch-break-label #f) #t)
                                 st2)]
                        [else
                         (let/st st ([lbl (create-artificial-label st2)])
                           (define dc (build3 st 'case_label_expr (void-type st) #f #f lbl))
                           (define st3 (if old-in-switch (set-decl-flag st lbl 'switch-break-label #t) st))
                           (values dc (pvector-cons-right body-seq (gimple-build-label lbl)) (and old-in-switch #t) st3))])])
           (define switch-stmt (gimple-build-switch (tree-operand sw 0) default* cases (make-loc (tree-loc sw))))
           (define pre*
             (if add-bind
                 (pvector-cons-right pre (gimple-build-bind (pvector-empty) (pvector-cons-left body-seq switch-stmt) #f))
                 (pvector-append (pvector-cons-right pre switch-stmt) body-seq)))
           (values #f GS-ALL-DONE pre* st)))])))

(define (gimplify-label-expr st e pre)
  (values #f GS-ALL-DONE (pvector-cons-right pre (gimple-build-label (label-expr-label e) (make-loc (tree-loc e)))) st))

;; gimplify_case_label_expr: record the label in the nearest context with case labels
(define (gimplify-case-label-expr st e pre)
  (define stack (GS-ctx st))
  (define st1
    (let loop ([cs stack] [acc '()])
      (cond
        [(null? cs) (error 'gimplify-case-label-expr "case label outside switch")]
        [(gctx-case-labels (car cs))
         (gs-set-ctx st (append (reverse acc)
                                (cons (struct-copy gctx (car cs) [case-labels (pvector-cons-right (gctx-case-labels (car cs)) e)])
                                      (cdr cs))))]
        [else (loop (cdr cs) (cons (car cs) acc))])))
  (values #f GS-ALL-DONE (pvector-cons-right pre (gimple-build-label (case-label e) (make-loc (tree-loc e)))) st1))

;; build_and_jump over a label slot: #f = no slot (NULL pointer), 'empty =
;; slot without a label yet, otherwise the label.  (values goto slot* st)
(define (build-and-jump st slot)
  (cond
    [(not slot) (values #f #f st)]
    [(eq? slot 'empty)
     (let/st st ([l (create-artificial-label st)])
       (values (build-goto st l) l st))]
    [else (values (build-goto st slot) slot st)]))

(define (gimplify-exit-expr st e)
  (define cnd (tree-operand e 0))
  (define slot (or (gctx-exit-label (ctx st)) 'empty))
  (let/st st ([(jump slot*) (build-and-jump st slot)])
    (define st1 (ctx-update st (lambda (c) (struct-copy gctx c [exit-label slot*]))))
    (values (build3 st1 'cond_expr (void-type st1) cnd jump #f) GS-OK st1)))

;; ------------------------------------------------------------
;; Conversions, decls, compound lvalues, self-modify, args
;; ------------------------------------------------------------
(define (canonicalize-component-ref st expr)
  (define field (tree-operand expr 1))
  (define type (tree-type field))
  (cond
    [(eqv? (tree-type expr) type) expr]
    ;; bit-fields keep their narrowed type in GCC; plain fields take the field type
    [(tree-attr field 'bit-field #f) expr]
    [else (tree-set-type expr type)]))

;; canonicalize_addr_expr: &array -> &array[0] when the pointer types agree
(define (canonicalize-addr-expr st expr)
  (define addr (tree-operand expr 0))
  (cond
    [(and (pointer-type-p st (tree-type expr)) (eq? (tree-code addr) 'addr_expr))
     (define datype (type-type st (tree-type addr)))
     (cond
       [(eq? (type-code st datype) 'array_type)
        (define ddatype (type-type st datype))
        (define dom (type-attr st datype 'domain))
        (define mn (and dom (type-attr st dom 'min)))
        (let/st st ([pt (pointer-to st ddatype)])
          (cond
            [(and mn (eq? (tree-code mn) 'integer_cst)
                  (useless-type-conversion-p st (type-main-variant st (tree-type expr)) pt))
             (define aref (build4 st 'array_ref ddatype (tree-operand addr 0) mn #f #f))
             (define n (build1 st 'addr_expr pt aref))
             (values (if (useless-type-conversion-p st (tree-type expr) (tree-type n)) n (fold-convert st (tree-type expr) n)) st)]
            [else (values expr st)]))]
       [else (values expr st)])]
    [else (values expr st)]))

;; gimplify_conversion: (values e* GS-OK st)
(define (gimplify-conversion st e0)
  (define e1 (tree-set-operand e0 0 (strip-sign-nops st (tree-operand e0 0))))
  (define e2 (if (tree-ssa-useless-type-conversion st e1) (tree-operand e1 0) e1))
  (let/st st ([e3 (cond
                    [(convert-expr-p e2)
                     (define sub (tree-operand e2 0))
                     (cond
                       [(eq? (tree-code sub) 'component_ref) (values (tree-set-operand e2 0 (canonicalize-component-ref st sub)) st)]
                       [(eq? (tree-code sub) 'addr_expr) (canonicalize-addr-expr st e2)]
                       [else (values e2 st)])]
                    [else (values e2 st)])])
    (define e4
      (if (and (convert-expr-p e3) (not (is-gimple-reg-type st (tree-type e3))))
          (build1 st 'view_convert_expr (tree-type e3) (tree-operand e3 0))
          e3))
    (define e5 (if (eq? (tree-code e4) 'convert_expr) (tree-set-code e4 'nop_expr) e4))
    (values e5 GS-OK st)))

;; gimplify_var_or_parm_decl: DECL_VALUE_EXPR substitution; (values e* ret st)
(define (gimplify-var-or-parm-decl st decl)
  (cond
    [(tree-attr decl 'value-expr #f) (values (tree-attr decl 'value-expr) GS-OK st)]
    [else (values decl GS-ALL-DONE st)]))

;; fold_indirect_ref_loc (subset): *&x -> x
(define (fold-indirect-ref st t)
  (define sub (tree-operand t 0))
  (cond
    [(eq? (tree-code sub) 'addr_expr)
     (define op (tree-operand sub 0))
     (if (types-compatible-p st (tree-type t) (tree-type op)) op t)]
    [else t]))

;; prepare_gimple_addressable on a base (no handled components left): (values base* pre st)
(define (prepare-gimple-addressable st e pre)
  (cond
    [(is-gimple-reg st e)
     (define-values (t pre1 post1 st1) (internal-get-tmp-var st e pre #f #f #f #t))
     (values t pre1 st1)]
    [else (values e pre st)]))

;; gimplify_compound_lval: (values e* ret pre post st)
(define (gimplify-compound-lval st e pre post fallback)
  ;; descend to the base; STACK is innermost-first (GCC's pop order)
  (define-values (stack base st1)
    (let loop ([x e] [stack '()] [st st])
      (define x1 (if (eq? (tree-code x) 'indirect_ref) (fold-indirect-ref st x) x))
      (cond
        [(handled-component-p x1) (loop (tree-operand x1 0) (cons x1 stack) st)]
        [(memq (tree-code x1) '(var_decl parm_decl))
         (define-values (x2 r st2) (gimplify-var-or-parm-decl st x1))
         (if (= r GS-OK) (loop x2 stack st2) (values stack x2 st2))]
        [else (values stack x1 st)])))
  (define need-non-reg
    (for/or ([t (in-list stack)])
      (case (tree-code t)
        [(array_ref array_range_ref component_ref) #t]
        [else (not (is-gimple-reg-type st1 (tree-type t)))])))
  (let/st st ([(base* ret pre post) (gimplify-expr st1 base pre post is-gimple-min-lval (bitwise-ior fallback fb-lvalue))])
    (cond
      [(= ret GS-ERROR) (values e GS-ERROR pre post st)]
      [else
       (let/st st ([(base2 pre) (if (and need-non-reg (fb? fallback fb-rvalue))
                                    (prepare-gimple-addressable st base* pre)
                                    (values base* pre st))])
         (define reg-operations (is-gimple-reg st base2))
         ;; rebuild innermost first, gimplifying index operands
         (let/st st ([(top ret2 pre post)
                      (for/fold ([inner base2] [ret ret] [pre pre] [post post] [st st] #:result (values inner ret pre post st))
                                ([t (in-list stack)])
                        (define t0 (tree-set-operand t 0 inner))
                        (define-values (t1 r1 pre1 post1 st1)
                          (case (tree-code t0)
                            [(array_ref array_range_ref)
                             (let/st st ([(a ra pre post) (gimplify-operand st t0 2 pre post is-gimple-reg fb-rvalue)]
                                         [(b rb pre post) (gimplify-operand st a 3 pre post is-gimple-reg fb-rvalue)]
                                         [(c rc pre post) (gimplify-operand st b 1 pre post is-gimple-val fb-rvalue)])
                               (values c (min ra rb rc) pre post st))]
                            [(component_ref) (gimplify-operand st t0 2 pre post is-gimple-reg fb-rvalue)]
                            [else
                             (if reg-operations
                                 (gimplify-operand st t0 0 pre post is-gimple-val fb-rvalue)
                                 (values t0 GS-ALL-DONE pre post st))]))
                        (define t2 (recalculate-side-effects
                                    st1 (tree-set-operand t1 0 (strip-useless-type-conversion st1 (tree-operand t1 0)))))
                        (values t2 (min ret r1) pre1 post1 st1))])
           (define top* (if (and (fb? fallback fb-rvalue) (eq? (tree-code top) 'component_ref))
                            (canonicalize-component-ref st top)
                            top))
           (values top* ret2 pre post st)))])))

;; gimplify_self_mod_expr: (values e* ret pre post st)
(define (gimplify-self-mod-expr st e pre post want-value arith-type)
  (define code (tree-code e))
  (define postfix (and (memq code '(postincrement_expr postdecrement_expr)) want-value))
  (define arith-code (if (memq code '(preincrement_expr postincrement_expr)) 'plus_expr 'minus_expr))
  ;; postfix: side effects of the lvalue go to a local post queue
  (let/st st ([(lvalue ret pre lpost) (gimplify-expr st (tree-operand e 0) pre (if postfix (pvector-empty) post)
                                                     is-gimple-lvalue fb-lvalue)])
    (cond
      [(= ret GS-ERROR) (values e ret pre (if postfix post lpost) st)]
      [else
       (define rhs (tree-operand e 1))
       (let/st st ([(lhs pre lpost)
                    (cond
                      [postfix
                       (let/st st ([(lv r pre lpost) (gimplify-expr st lvalue pre lpost is-gimple-val fb-rvalue)]
                                   [(tmp pre lpost) (get-initialized-tmp-var st lv pre lpost)])
                         (values tmp pre lpost st))]
                      [else (values lvalue pre lpost st)])])
         (define t1
           (cond
             [(pointer-type-p st (tree-type lhs))
              (define rhs1 (convert-to-ptrofftype st rhs))
              (define rhs2 (if (eq? arith-code 'minus_expr) (fold-build1 st 'negate_expr (tree-type rhs1) rhs1) rhs1))
              (fold-build2 st 'pointer_plus_expr (tree-type e) lhs rhs2)]
             [else
              (fold-convert st (tree-type e)
                            (fold-build2 st arith-code arith-type (fold-convert st arith-type lhs) (fold-convert st arith-type rhs)))]))
         (cond
           [postfix
            (let/st st ([pre (gimplify-and-add st (build2 st 'modify_expr (tree-type lvalue) lvalue t1) pre)])
              (values lhs GS-ALL-DONE pre (pvector-append post lpost) st))]
           [else
            (values (build2 st 'modify_expr (tree-type lvalue) lvalue t1) GS-OK pre lpost st)]))])))

;; gimplify_arg: (values a* ret pre st)
(define (gimplify-arg st a pre [allow-ssa #t])
  (define-values (test fb)
    (if (is-gimple-reg-type st (tree-type a))
        (values is-gimple-val fb-rvalue)
        (values is-gimple-lvalue fb-either)))
  (define-values (a* ret pre1 post1 st1) (gimplify-expr st a pre #f test fb))
  (values a* ret pre1 st1))

;; ------------------------------------------------------------
;; gimplify_call_expr (no builtin folding): (values e* ret pre st)
;; ------------------------------------------------------------
;; call_expr_flags: the front end's bits, plus what later analysis
;; recorded on the callee
(define (call-expr-flags c [st #f])
  (define fn (call-expr-fn c))
  (define d (and st fn (eq? (tree-code fn) 'addr_expr) (eq? (tree-code (tree-operand fn 0)) 'function_decl) (tree-operand fn 0)))
  (bitwise-ior (tree-attr c 'flags 0) (if d (decl-ecf-flags st d) 0)))

;; gimplify the arguments, last first (PUSH_ARGS_REVERSED): (values e* ret pre st)
(define (gimplify-call-args st e pre)
  (for/fold ([e e] [ret GS-ALL-DONE] [pre pre] [st st] #:result (values e ret pre st))
            ([i (in-range (sub1 (call-expr-nargs e)) -1 -1)])
    (define-values (a r pre1 st1) (gimplify-arg st (call-expr-arg e i) pre))
    (values (tree-set-operand e (+ 3 i) a) (if (= r GS-ERROR) GS-ERROR ret) pre1 st1)))

(define (gimplify-call-expr st e0 pre fallback)
  (define want-value (not (= fallback fb-none)))
  (define e (if (tree-loc e0) e0 (tree-set-loc e0 (loc-spot (GS-loc st)))))
  (cond
    [(call-expr-internal? e)
     (cond
       [want-value (values e GS-ALL-DONE pre st)]
       [else
        (let/st st ([(e* _ret pre) (gimplify-call-args st e pre)])
          (values #f GS-ALL-DONE (pvector-cons-right pre (gimple-build-call-from-tree e* #f)) st))])]
    [else
     (define fnptrtype (tree-type (call-expr-fn e)))
     (let/st st ([(e1 ret pre _post) (gimplify-operand st e 1 pre #f is-gimple-call-addr fb-rvalue)])
       (cond
         [(= ret GS-ERROR) (values e1 GS-ERROR pre st)]
         [else
          (let/st st ([(e2 ret2 pre) (gimplify-call-args st e1 pre)])
            (cond
              [(= ret2 GS-ERROR) (values #f GS-ERROR pre st)]
              [else
               (define flags (call-expr-flags e2 st))
               (define e2* (if (= flags (tree-attr e2 'flags 0)) e2 (tree-set-attr e2 'flags flags)))
               (define e3
                 (if (and (not (zero? (bitwise-and flags (bitwise-ior ECF-CONST ECF-PURE))))
                          (zero? (bitwise-and flags ECF-LOOPING)))
                     (tree-set-attr e2* 'side-effects #f)
                     e2*))
               (cond
                 [(not want-value)
                  (values #f ret (pvector-cons-right pre (gimple-build-call-from-tree e3 fnptrtype)) st)]
                 [else
                  (values (tree-set-operand e3 1 (build1 st 'nop_expr fnptrtype (call-expr-fn e3))) ret pre st)])]))]))]))

;; ------------------------------------------------------------
;; Short-circuit conditions
;; ------------------------------------------------------------
(define (find-goto e)
  (cond
    [(not e) #f]
    [(eq? (tree-code e) 'goto_expr) e]
    [(not (eq? (tree-code e) 'statement_list)) #f]
    [else
     (define l (pvector-filter (statement-list-stmts e) (lambda (s) (not (eq? (tree-code s) 'debug_begin_stmt)))))
     (and (= (pvector-length l) 1) (find-goto (pvector-ref l 0)))]))
(define (find-goto-label e)
  (define d (find-goto e))
  (and d (eq? (tree-code (goto-destination d)) 'label_decl) d))

;; rexpr_location: location of e, looking through a one-statement list
(define (rexpr-location e)
  (cond
    [(not e) #f]
    [(eq? (tree-code e) 'statement_list)
     (define l (pvector-filter (statement-list-stmts e) (lambda (s) (not (eq? (tree-code s) 'debug_begin_stmt)))))
     (and (= (pvector-length l) 1) (rexpr-location (pvector-ref l 0)))]
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

;; shortcut_cond_r with label slots (see build-and-jump): (values expr tslot fslot st)
(define (shortcut-cond-r st pred tslot fslot locus)
  (define code (tree-code pred))
  (define (app st t lst) (append-to-statement-list st t lst))
  (cond
    [(eq? code 'truth_andif_expr)
     (define local? (not fslot))
     (define f0 (if local? 'empty fslot))
     (let/st st ([(e1 _t f1) (shortcut-cond-r st (tree-operand pred 0) #f f0 locus)]
                 [(e2 t2 f2) (shortcut-cond-r st (tree-operand pred 1) tslot f1 (or (tree-loc pred) locus))])
       (define expr (app st e2 (app st e1 #f)))
       (finish-local st expr local? f2 t2 fslot))]
    [(eq? code 'truth_orif_expr)
     (define local? (not tslot))
     (define t0 (if local? 'empty tslot))
     (let/st st ([(e1 t1 _f) (shortcut-cond-r st (tree-operand pred 0) t0 #f locus)]
                 [(e2 t2 f2) (shortcut-cond-r st (tree-operand pred 1) t1 fslot (or (tree-loc pred) locus))])
       (define expr (app st e2 (app st e1 #f)))
       (finish-local st expr local? t2 f2 tslot #:true-local #t))]
    [(and (eq? code 'cond_expr)
          (not (void-type-p st (tree-type (tree-operand pred 1))))
          (not (void-type-p st (tree-type (tree-operand pred 2)))))
     (let/st st ([(e1 t1 f1) (shortcut-cond-r st (tree-operand pred 1) tslot fslot locus)]
                 [(e2 t2 f2) (shortcut-cond-r st (tree-operand pred 2) t1 f1 (or (tree-loc pred) locus))])
       (values (build3 st 'cond_expr (void-type st) (tree-operand pred 0) e1 e2) t2 f2 st))]
    [else
     (let/st st ([(tj t1) (build-and-jump st tslot)]
                 [(fj f1) (build-and-jump st fslot)])
       (values (tree-set-loc (build3 st 'cond_expr (void-type st) pred tj fj) locus) t1 f1 st))]))

;; emit the local label (if one was created) and hand the caller's slots back
(define (finish-local st expr local? local-slot other-slot caller-slot #:true-local [true-local #f])
  (define expr*
    (if (and local? (not (eq? local-slot 'empty)) local-slot)
        (append-to-statement-list st (build-label-expr st local-slot) expr)
        expr))
  (if true-local
      (values expr* (if local? caller-slot local-slot) other-slot st)
      (values expr* other-slot (if local? caller-slot local-slot) st)))

;; shortcut_cond_expr: (values expr* st)
(define (shortcut-cond-expr st expr0)
  ;; pull ANDIF/ORIF out of the predicate when the corresponding arm is empty
  (define-values (expr pred then_ else_ then-se else-se st1)
    (let ()
      (define expr expr0)
      (define pred (tree-operand expr 0))
      (define then_ (tree-operand expr 1))
      (define else_ (tree-operand expr 2))
      (define then-se (and then_ (tree-side-effects then_)))
      (define else-se (and else_ (tree-side-effects else_)))
      (define-values (expr1 pred1 then1 then-se1 st1)
        (if (not else-se)
            (let loop ([expr expr] [pred pred] [then_ then_] [then-se then-se] [st st])
              (cond
                [(eq? (tree-code pred) 'truth_andif_expr)
                 (define locus (tree-loc expr))
                 (define inner (let ([e (tree-set-operand expr 0 (tree-operand pred 1))])
                                 (if (tree-loc pred) (tree-set-loc e (tree-loc pred)) e)))
                 (define-values (then* st*) (shortcut-cond-expr st inner))
                 (define pred* (tree-operand pred 0))
                 (loop (tree-set-loc (build3 st* 'cond_expr (void-type st*) pred* then* #f) locus)
                       pred* then* (and then* (tree-side-effects then*)) st*)]
                [else (values expr pred then_ then-se st)]))
            (values expr pred then_ then-se st)))
      (define-values (expr2 pred2 else2 else-se2 st2)
        (if (not then-se1)
            (let loop ([expr expr1] [pred pred1] [else_ else_] [else-se else-se] [st st1])
              (cond
                [(eq? (tree-code pred) 'truth_orif_expr)
                 (define locus (tree-loc expr))
                 (define inner (let ([e (tree-set-operand expr 0 (tree-operand pred 1))])
                                 (if (tree-loc pred) (tree-set-loc e (tree-loc pred)) e)))
                 (define-values (else* st*) (shortcut-cond-expr st inner))
                 (define pred* (tree-operand pred 0))
                 (loop (tree-set-loc (build3 st* 'cond_expr (void-type st*) pred* #f else*) locus)
                       pred* else* (and else* (tree-side-effects else*)) st*)]
                [else (values expr pred else_ else-se st)]))
            (values expr1 pred1 else_ else-se st1)))
      (values expr2 pred2 then1 else2 then-se1 else-se2 st2)))
  (cond
    [(not (memq (tree-code pred) '(truth_andif_expr truth_orif_expr))) (values expr st1)]
    [else
     ;; hijack gotos that are the whole arm
     (define-values (true-label then2 then-se2)
       (let ([g (find-goto-label then_)]) (if g (values (goto-destination g) #f #f) (values #f then_ then-se))))
     (define-values (false-label else2 else-se2)
       (let ([g (find-goto-label else_)]) (if g (values (goto-destination g) #f #f) (values #f else_ else-se))))
     (define tslot (or true-label #f))
     (define fslot (cond [false-label false-label] [else-se2 'empty] [else #f]))
     (cond
       [(and (not then-se2) (not else-se2))
        (let/st st1 ([(e _t _f) (shortcut-cond-r st1 pred tslot fslot (tree-loc expr))])
          (values e st1))]
       [else
        (define t (cond [else-se2 (expr-last else2)] [then-se2 (expr-last then2)] [else #f]))
        (define end-label0 (and t (eq? (tree-code t) 'label_expr) (label-expr-label t)))
        ;; without a false slot, the false label is the end label (the slots alias)
        (define alias? (not fslot))
        (define fslot* (if alias? (or end-label0 'empty) fslot))
        (define emit-end (not end-label0))
        (define emit-false (not false-label))
        (define jump-over-else (block-may-fallthru then2))
        (let/st st1 ([(pred* _t f1) (shortcut-cond-r st1 pred tslot fslot* (tree-loc expr))])
          (define end-label1 (if alias? (and (not (eq? f1 'empty)) f1) end-label0))
          (define false-label1 (if alias? false-label (and (not (eq? f1 'empty)) f1)))
          (define (app st t lst) (append-to-statement-list st t lst))
          (define out0 (app st1 then2 (app st1 pred* #f)))
          (let/st st1 ([(out1 end-label2)
                        (cond
                          [else-se2
                           (let/st st1 ([(out end-slot)
                                         (cond
                                           [jump-over-else
                                            (let/st st1 ([(j slot) (build-and-jump st1 (or end-label1 'empty))])
                                              (values (app st1 j out0) (and (not (eq? slot 'empty)) slot) st1))]
                                           [else (values out0 end-label1 st1)])])
                             (define out2 (if emit-false (app st1 (build-label-expr st1 false-label1) out) out))
                             (values (app st1 else2 out2) end-slot st1))]
                          [else (values out0 end-label1 st1)])])
            (define out3 (if (and emit-end end-label2) (app st1 (build-label-expr st1 end-label2) out1) out1))
            (values out3 st1)))])]))

;; gimple_boolify: (values expr* st)  (pure except for the global boolean type)
(define (gimple-boolify st expr)
  (define type (tree-type expr))
  (define (bool! e) (if (boolean-type-p st (tree-type e)) e (tree-set-type e (boolean-type st))))
  (case (tree-code expr)
    [(truth_and_expr truth_or_expr truth_xor_expr truth_andif_expr truth_orif_expr)
     (define e1 (tree-set-operand expr 1 (gimple-boolify st (tree-operand expr 1))))
     (define e2 (tree-set-operand e1 0 (gimple-boolify st (tree-operand e1 0))))
     (bool! e2)]
    [(truth_not_expr)
     (bool! (tree-set-operand expr 0 (gimple-boolify st (tree-operand expr 0))))]
    [else
     (cond
       [(comparison-class-p st expr) (bool! expr)]
       [(boolean-type-p st type) expr]
       [else (fold-convert st (boolean-type st) expr)])]))

;; generic_expr_could_trap_p: conservative
(define (generic-expr-could-trap-p st e)
  (cond
    [(or (not e) (is-gimple-val st e)) #f]
    [(not (expr-p st e)) #t]
    [(memq (tree-code e) '(mem_ref indirect_ref array_ref component_ref trunc_div_expr trunc_mod_expr
                           exact_div_expr floor_div_expr ceil_div_expr round_div_expr rdiv_expr call_expr)) #t]
    [else (for/or ([i (in-range (tree-operand-length e))]) (generic-expr-could-trap-p st (tree-operand e i)))]))

;; gimplify_pure_cond_expr: (values expr* ret pre st)
(define (gimplify-pure-cond-expr st expr pre)
  (define cnd0 (gimple-boolify st (tree-operand expr 0)))
  (define cnd
    (case (tree-code cnd0)
      [(truth_andif_expr) (tree-set-code cnd0 'truth_and_expr)]
      [(truth_orif_expr) (tree-set-code cnd0 'truth_or_expr)]
      [else cnd0]))
  (let/st st ([(e0 r0 pre _p0) (gimplify-operand st (tree-set-operand expr 0 cnd) 0 pre #f is-gimple-val fb-rvalue)]
              [(e1 r1 pre _p1) (gimplify-operand st e0 1 pre #f is-gimple-val fb-rvalue)]
              [(e2 r2 pre _p2) (gimplify-operand st e1 2 pre #f is-gimple-val fb-rvalue)])
    (values e2 (min r0 r1 r2) pre st)))

;; gimplify_cond_expr: (values e* ret pre st)
(define (gimplify-cond-expr st expr pre fallback)
  (define type (tree-type expr))
  (cond
    ;; valued COND_EXPR: copy the arms into a temporary
    [(not (void-type-p st type))
     (define then_ (tree-operand expr 1)) (define else_ (tree-operand expr 2))
     (let/st st ([(tmp result expr*)
                  (cond
                    [(and (or (fb? fallback fb-rvalue) (not (fb? fallback fb-lvalue))) (not (type-attr st type 'addressable)))
                     (cond
                       [(and (gctx-allow-rhs-cond-expr (ctx st))
                             (not (tree-side-effects then_)) (not (generic-expr-could-trap-p st then_))
                             (not (tree-side-effects else_)) (not (generic-expr-could-trap-p st else_)))
                        (values 'pure #f #f st)]
                       [else (let/st st ([t (create-tmp-var st type "iftmp")]) (values t t expr st))])]
                    [else
                     (let/st st ([ptype (pointer-to st type)])
                       (define then* (if (void-type-p st (tree-type then_)) then_ (build1 st 'addr_expr ptype then_)))
                       (define else* (if (void-type-p st (tree-type else_)) else_ (build1 st 'addr_expr ptype else_)))
                       (define e2 (build3 st 'cond_expr ptype (tree-operand expr 0) then* else*))
                       (let/st st ([t (create-tmp-var st ptype "iftmp")])
                         (values t (build2 st 'mem_ref type t (build-int-cst st ptype 0)) e2 st)))])])
       (cond
         [(eq? tmp 'pure) (gimplify-pure-cond-expr st expr pre)]
         [else
          (define ty (tree-type tmp))
          (define e1 (if (void-type-p st (tree-type (tree-operand expr* 1))) expr*
                         (tree-set-operand expr* 1 (build2 st 'init_expr ty tmp (tree-operand expr* 1)))))
          (define e2 (if (void-type-p st (tree-type (tree-operand e1 2))) e1
                         (tree-set-operand e1 2 (build2 st 'init_expr ty tmp (tree-operand e1 2)))))
          (define e3 (recalculate-side-effects st (tree-set-type e2 (void-type st))))
          (let/st st ([pre (gimplify-and-add st e3 pre)])
            (values result GS-ALL-DONE pre st))]))]
    [else
     (define e0 (tree-set-operand expr 0 (strip-type-nops (tree-operand expr 0))))
     (let/st st ([(e1 pre) (if (eq? (tree-code (tree-operand e0 0)) 'compound_expr)
                               (let/st st ([(c _r pre) (gimplify-compound-expr st (tree-operand e0 0) pre #t)])
                                 (values (tree-set-operand e0 0 c) pre st))
                               (values e0 pre st))])
       (define e2 (tree-set-operand e1 0 (gimple-boolify st (tree-operand e1 0))))
       (define c0 (tree-operand e2 0))
       (define-values (short st1)
         (if (memq (tree-code c0) '(truth_andif_expr truth_orif_expr))
             (let-values ([(s st*) (shortcut-cond-expr st e2)]) (values (and (not (eq? s e2)) s) st*))
             (values #f st)))
       (cond
         [short
          (define st2 (gimple-push-condition st1))
          (let/st st ([(_added s) (gimplify-stmt st2 short (pvector-empty))]
                      [pre (gimple-pop-condition st pre)])
            (values #f GS-ALL-DONE (pvector-append pre s) st))]
         [else
          (let/st st ([(e3 ret pre _post) (gimplify-operand st1 e2 0 pre #f is-gimple-condexpr-for-cond fb-rvalue)])
            (cond
              [(= ret GS-ERROR) (values e3 GS-ERROR pre st)]
              [else
               (define st2 (gimple-push-condition st))
               ;; -O0: hijack a goto arm only when the COND_EXPR and the GOTO share a
               ;; location (or have none)
               (define (hijack? g)
                 (and g (or (optimize? st2)
                            (let ([el (tree-loc e3)] [gl (rexpr-location g)])
                              (or (not el) (not gl) (equal? el gl))))))
               (define gt (find-goto-label (tree-operand e3 1)))
               (define ge (find-goto-label (tree-operand e3 2)))
               (define have-then0 (hijack? gt))
               (define have-else0 (hijack? ge))
               (let/st st2 ([label-true (if have-then0 (values (goto-destination gt) st2) (create-artificial-label st2))]
                            [label-false (if have-else0 (values (goto-destination ge) st2) (create-artificial-label st2))])
                 (define-values (pred-code arm1 arm2) (gimple-cond-get-ops-from-tree st2 (tree-operand e3 0)))
                 (define cond-stmt (gimple-build-cond pred-code arm1 arm2 label-true label-false (make-loc (tree-loc e3))))
                 (define seq0 (maybe-fold-stmt st2 (pvector cond-stmt)))
                 ;; then arm
                 (let/st st2 ([(seq1 have-then label-cont)
                               (cond
                                 [have-then0 (values seq0 #t #f st2)]
                                 [(and (not (tree-operand e3 1)) (not have-else0) (tree-operand e3 2))
                                  (values seq0 #f label-true st2)]
                                 [else
                                  (let/st st2 ([(added seq) (gimplify-stmt st2 (tree-operand e3 1) (pvector-cons-right seq0 (gimple-build-label label-true)))])
                                    (cond
                                      [(and (not have-else0) (tree-operand e3 2) (gimple-seq-may-fallthru seq))
                                       (let/st st2 ([lc (create-artificial-label st2)])
                                         (values (pvector-cons-right seq (stmt-set-do-not-emit-location (gimple-build-goto lc))) added lc st2))]
                                      [else (values seq added #f st2)]))])])
                   ;; else arm
                   (let/st st2 ([(seq2 have-else)
                                 (cond
                                   [have-else0 (values seq1 #t st2)]
                                   [else
                                    (let/st st2 ([(added seq) (gimplify-stmt st2 (tree-operand e3 2) (pvector-cons-right seq1 (gimple-build-label label-false)))])
                                      (values seq added st2))])])
                     (define seq3 (if label-cont (pvector-cons-right seq2 (gimple-build-label label-cont)) seq2))
                     (let/st st2 ([pre (gimple-pop-condition st2 pre)])
                       (define pre1 (pvector-append pre seq3))
                       (cond
                         [(or have-then have-else) (values #f GS-ALL-DONE pre1 st2)]
                         [else
                          (let/st st2 ([pre (gimplify-and-add st2 (tree-operand e3 0) pre1)])
                            (values #f ret pre st2))])))))]))]))]))

;; ------------------------------------------------------------
;; MODIFY_EXPR
;; ------------------------------------------------------------

;; gimplify_modify_expr_rhs: (values e* ret pre post st), ret GS-UNHANDLED when
;; nothing applied.  E is the whole MODIFY/INIT_EXPR.
(define (gimplify-modify-expr-rhs st e pre post want-value)
  (let loop ([e e] [ret GS-UNHANDLED] [pre pre] [post post] [st st])
    (define from (tree-operand e 1))
    (define to (tree-operand e 0))
    (define (again e* pre post st) (loop e* GS-OK pre post st))
    (case (tree-code from)
      [(indirect_ref)
       (define t (fold-indirect-ref st from))
       (if (and (not (eq? t from)) (not (type-attr st (tree-type from) 'addressable)))
           (again (tree-set-operand e 1 t) pre post st)
           (values e ret pre post st))]
      [(target_expr)
       (define init (tree-operand from 1))
       (if (and init (not (void-type-p st (tree-type init))))
           (again (tree-set-operand e 1 init) pre post st)
           (values e ret pre post st))]
      [(compound_expr)
       (let/st st ([(f _r pre) (gimplify-compound-expr st from pre #t)])
         (again (tree-set-operand e 1 f) pre post st))]
      [(constructor)
       (if (not (= ret GS-UNHANDLED))
           (values e ret pre post st)
           (gimplify-init-constructor st e pre post want-value))]
      [(cond_expr)
       (cond
         [(or (not (is-gimple-reg-type st (tree-type from)))
              (and (is-gimple-reg st to) (not (gctx-allow-rhs-cond-expr (ctx st)))))
          (define code (tree-code e))
          (let/st st ([(result r1 pre post) (gimplify-expr st to pre post is-gimple-lvalue fb-lvalue)])
            (define r2 (if (= r1 GS-ERROR) r1 GS-OK))
            (define st1
              (if (and (var-p result)
                       (not (void-type-p st (tree-type (tree-operand from 1))))
                       (not (void-type-p st (tree-type (tree-operand from 2)))))
                  (set-decl-flag st result 'readonly #f)
                  st))
            (define c1 (if (void-type-p st1 (tree-type (tree-operand from 1))) from
                           (tree-set-operand from 1 (build2 st1 code (void-type st1) result (tree-operand from 1)))))
            (define c2 (if (void-type-p st1 (tree-type (tree-operand c1 2))) c1
                           (tree-set-operand c1 2 (build2 st1 code (void-type st1) result (tree-operand c1 2)))))
            (define cnd (recalculate-side-effects st1 (tree-set-type c2 (void-type st1))))
            (cond
              [want-value
               (let/st st1 ([pre (gimplify-and-add st1 cnd pre)])
                 (values result r2 pre post st1))]
              [else (values cnd r2 pre post st1)]))]
         [else (values e ret pre post st)])]
      [(cleanup_point_expr bind_expr statement_list)
       (let/st st ([(to* r1 pre post) (gimplify-expr st to pre post is-gimple-min-lval fb-lvalue)])
         (define e1 (tree-set-operand e 0 to*))
         (let/st st ([(wrap _t) (voidify-wrapper-expr st from e1)])
           (cond
             [want-value
              (let/st st ([pre (gimplify-and-add st wrap pre)])
                (values to* GS-OK pre post st))]
             [else (values wrap GS-OK pre post st)])))]
      [(nop_expr)
       (if (and (eq? (tree-code (tree-operand from 0)) 'compound_literal_expr) (tree-ssa-useless-type-conversion st from))
           (again (tree-set-operand e 1 (tree-operand from 0)) pre post st)
           (values e ret pre post st))]
      [(compound_literal_expr)
       (define decl (decl-expr-decl (tree-operand from 0)))
       (define init (decl-initial st decl))
       (if (and (not (tree-attr from 'addressable #f)) (not (decl-flag st decl 'addressable)) init)
           (values (tree-set-operand e 1 init) GS-OK pre post st)
           (values e ret pre post st))]
      [else (values e ret pre post st)])))

;; gimplify_init_constructor: only the trivial forms for now
(define (gimplify-init-constructor st e pre post want-value)
  (define ctor (tree-operand e 1))
  (define object (tree-operand e 0))
  (cond
    [(pvector-empty? (constructor-elts ctor))
     ;; zero-initialization: object = {}  (kept as a CONSTRUCTOR rhs)
     (values e GS-UNHANDLED pre post st)]
    [else
     ;; element-wise stores (gimplify_init_ctor_eval)
     (let/st st ([(obj _r pre post) (gimplify-expr st object pre post is-gimple-lvalue fb-lvalue)])
       (let/st st ([pre (for/fold ([pre pre] [st st] #:result (values pre st)) ([p (in-pvector (constructor-elts ctor))])
                          (define index (car p)) (define val (cdr p))
                          (define lhs
                            (cond
                              [(and index (eq? (tree-code index) 'field_decl)) (build3 st 'component_ref (tree-type index) obj index #f)]
                              [index (build4 st 'array_ref (tree-type val) obj index #f #f)]
                              [else (error 'gimplify-init-constructor "unsupported CONSTRUCTOR element")]))
                          (gimplify-and-add st (build2 st 'modify_expr (tree-type lhs) lhs val) pre))])
         (if want-value
             (values object GS-OK pre post st)
             (values #f GS-ALL-DONE pre post st))))]))

;; get_symbol_constant_value (gimple-fold.cc), via maybe_fold_stmt on a fresh assignment
(define (get-symbol-constant-value st sym)
  (and (var-p sym) (decl-flag st sym 'readonly) (not (tree-this-volatile sym))
       (or (tree-static sym) (decl-external sym))
       (let ([val (let ([v (decl-initial st sym)]) (and v (strip-useless-type-conversion st v)))])   ; canonicalize_constructor_val
         (and val (is-gimple-min-invariant st val)
              (useless-type-conversion-p st (tree-type sym) (tree-type val))
              val))))

;; tree_swap_operands_p (fold-const.cc): should ARG0 and ARG1 be swapped?
(define (tree-swap-operands-p st arg0 arg1)
  (cond
    [(constant-class-p st arg1) #f]
    [(constant-class-p st arg0) #t]
    [else
     (define a0 (strip-nops st arg0)) (define a1 (strip-nops st arg1))
     (cond
       [(tree-constant a1) #f]
       [(tree-constant a0) #t]
       [(and (eq? (tree-code a0) 'ssa_name) (eq? (tree-code a1) 'ssa_name))
        (> (ssa-name-version a0) (ssa-name-version a1))]
       [(eq? (tree-code a1) 'ssa_name) #f]
       [(eq? (tree-code a0) 'ssa_name) #t]
       [(decl-p st a1) #f]
       [(decl-p st a0) #t]
       [else #f])]))

(define (commutative-tree-code code)
  (and (memq code '(plus_expr mult_expr mult_highpart_expr min_expr max_expr bit_ior_expr bit_xor_expr bit_and_expr
                    ne_expr eq_expr unordered_expr ordered_expr uneq_expr ltgt_expr truth_and_expr truth_xor_expr truth_or_expr
                    widen_mult_expr vec_widen_mult_hi_expr vec_widen_mult_lo_expr)) #t))

(define (swap-tree-comparison code)
  (case code [(lt_expr) 'gt_expr] [(gt_expr) 'lt_expr] [(le_expr) 'ge_expr] [(ge_expr) 'le_expr]
    [(unlt_expr) 'ungt_expr] [(ungt_expr) 'unlt_expr] [(unle_expr) 'unge_expr] [(unge_expr) 'unle_expr] [else code]))

;; maybe_fold_stmt -> fold_stmt_1 on the last statement of SEQ: the parts
;; observable at -O0 on plain C: canonical operand order of commutative
;; assigns and of conditions, loads from readonly symbols with constant
;; initializers.  Returns the new sequence.
(define (maybe-fold-stmt st seq)
  (define g (seq-last seq))
  (define g*
    (cond
      [(gassign? g)
       (define code (gassign-code g))
       (cond
         [(and (eq? (gimple-rhs-class st code) 'single) (get-symbol-constant-value st (gassign-rhs1 g)))
          => (lambda (v)
               (define-values (c a b d) (extract-ops-from-tree st v))
               (gassign (gassign-loc g) (gassign-flags g) (gassign-lhs g) c a b d))]
         [(and (eq? (gimple-rhs-class st code) 'binary) (commutative-tree-code code)
               (tree-swap-operands-p st (gassign-rhs1 g) (gassign-rhs2 g)))
          (gassign (gassign-loc g) (gassign-flags g) (gassign-lhs g) code (gassign-rhs2 g) (gassign-rhs1 g) #f)]
         [else g])]
      [(gcond? g)
       (if (tree-swap-operands-p st (gcond-lhs g) (gcond-rhs g))
           (gcond (gcond-loc g) (gcond-flags g) (swap-tree-comparison (gcond-code g)) (gcond-rhs g) (gcond-lhs g)
                  (gcond-true-label g) (gcond-false-label g))
           g)]
      [else g]))
  (if (eq? g g*) seq (seq-replace-last seq g*)))

;; gimplify_modify_expr: (values e* ret pre post st)
(define (gimplify-modify-expr st e0 pre post want-value)
  ;; pointer conversions required by the middle end
  (define e
    (cond
      [(pointer-type-p st (tree-type (tree-operand e0 0)))
       (define from (strip-useless-type-conversion st (tree-operand e0 1)))
       (define to-type (tree-type (tree-operand e0 0)))
       (tree-set-operand e0 1 (if (useless-type-conversion-p st to-type (tree-type from)) from (fold-convert st to-type from)))]
      [else e0]))
  (let/st st ([(e1 ret0 pre post) (gimplify-modify-expr-rhs st e pre post want-value)])
    (cond
      [(not (= ret0 GS-UNHANDLED)) (values e1 ret0 pre post st)]
      [(and (is-empty-type st (tree-type (tree-operand e1 1))) (not want-value))
       (let/st st ([(_a pre) (gimplify-stmt st (tree-operand e1 1) pre)]
                   [(_b pre) (gimplify-stmt st (tree-operand e1 0) pre)])
         (values #f GS-ALL-DONE pre post st))]
      [else
       (define initial-pred (initial-rhs-predicate-for st (tree-operand e1 0)))
       (let/st st ([(e2 r1 pre post) (gimplify-operand st e1 1 pre post initial-pred fb-rvalue)])
         (cond
           [(= r1 GS-ERROR) (values e2 r1 pre post st)]
           [else
            (let/st st ([(e3 r2 pre post) (gimplify-operand st e2 0 pre post is-gimple-lvalue fb-lvalue)])
              (cond
                [(= r2 GS-ERROR) (values e3 r2 pre post st)]
                [else
                 (define final-pred (rhs-predicate-for st (tree-operand e3 0)))
                 (let/st st ([(e4 r3 pre post) (if (eq? final-pred initial-pred)
                                                   (values e3 GS-OK pre post st)
                                                   (gimplify-operand st e3 1 pre post final-pred fb-rvalue))])
                   (cond
                     [(= r3 GS-ERROR) (values e4 r3 pre post st)]
                     [else
                      (let/st st ([(e5 r4 pre post) (gimplify-modify-expr-rhs st e4 pre post want-value)])
                        (cond
                          [(not (= r4 GS-UNHANDLED)) (values e5 r4 pre post st)]
                          [else
                           (define to (tree-operand e5 0))
                           (let/st st ([(e6 pre post)
                                        (cond
                                          [(and want-value (tree-this-volatile to))
                                           (let/st st ([(tmp pre post) (get-initialized-tmp-var st (tree-operand e5 1) pre post)])
                                             (values (tree-set-operand e5 1 tmp) pre post st))]
                                          [else (values e5 pre post st)])])
                             (define from (tree-operand e6 1))
                             (let/st st ([(assign pre)
                                          (cond
                                            [(eq? (tree-code from) 'call_expr)
                                             (cond
                                               [(call-expr-internal? from)
                                                (let/st st ([(from* _r pre) (gimplify-call-args st from pre)])
                                                  (values (gimple-build-call-from-tree from* #f to) pre st))]
                                               [else
                                                (define fnptrtype (tree-type (call-expr-fn from)))
                                                (define from1 (tree-set-operand from 1 (strip-useless-type-conversion st (tree-operand (call-expr-fn from) 0))))
                                                (values (gimple-build-call-from-tree from1 fnptrtype to) pre st)])]
                                            [else (values (gimple-build-assign st to from (make-loc (tree-loc e6))) pre st)])])
                               (define pre1 (maybe-fold-stmt st (pvector-cons-right pre assign)))
                               (cond
                                 [want-value (values (if (tree-this-volatile to) from to) GS-OK pre1 post st)]
                                 [else (values #f GS-ALL-DONE pre1 post st)])))]))]))]))]))])))

;; ------------------------------------------------------------
;; COMPOUND_EXPR, SAVE_EXPR, ADDR_EXPR, CLEANUP_POINT, TARGET_EXPR
;; ------------------------------------------------------------

;; gimplify_compound_expr: (values e* ret pre st)
(define (gimplify-compound-expr st e pre want-value)
  (let/st st ([(next pre)
               (let loop ([t e] [pre pre] [st st])
                 (define sub (tree-operand t 0))
                 (let/st st ([pre (if (eq? (tree-code sub) 'compound_expr)
                                      (let/st st ([(_s _r pre) (gimplify-compound-expr st sub pre #f)]) (values pre st))
                                      (gimplify-and-add st sub pre))])
                   (define next (tree-operand t 1))
                   (if (eq? (tree-code next) 'compound_expr) (loop next pre st) (values next pre st))))])
    (cond
      [want-value (values next GS-OK pre st)]
      [else
       (let/st st ([pre (gimplify-and-add st next pre)])
         (values #f GS-ALL-DONE pre st))])))

;; gimplify_save_expr: (values e* ret pre post st).  Values have no
;; sharing, so a SAVE_EXPR is evaluated where it stands.
(define (gimplify-save-expr st e pre post)
  (define val (tree-operand e 0))
  (cond
    [(tree-attr e 'resolved #f) (values val GS-ALL-DONE pre post st)]
    [(void-type-p st (tree-type val))
     (let/st st ([(_v _r pre post) (gimplify-expr st val pre post is-gimple-stmt fb-none)])
       (values #f GS-ALL-DONE pre post st))]
    [else
     (let/st st ([(v pre post) (get-initialized-tmp-var st val pre post #f)])
       (values v GS-ALL-DONE pre post st))]))

;; gimplify_addr_expr: (values e* ret pre post st)
(define (gimplify-addr-expr st expr pre post)
  (define op0 (tree-operand expr 0))
  (define (do-indirect-ref st op0 pre post)
    (define op00 (tree-operand op0 0))
    (values (if (useless-type-conversion-p st (tree-type expr) (tree-type op00)) op00 (fold-convert st (tree-type expr) op00))
            GS-OK pre post st))
  (case (tree-code op0)
    [(indirect_ref) (do-indirect-ref st op0 pre post)]
    [(view_convert_expr)
     (define inner (if (tree-ssa-useless-type-conversion st (tree-operand op0 0)) (tree-operand (tree-operand op0 0) 0) (tree-operand op0 0)))
     (let/st st ([pt (pointer-to st (tree-type inner))])
       (values (fold-convert st (tree-type expr) (build1 st 'addr_expr pt inner)) GS-OK pre post st))]
    [else
     (cond
       [(and (eq? (tree-code op0) 'mem_ref) (integer-zerop (tree-operand op0 1))) (do-indirect-ref st op0 pre post)]
       [else
        (let/st st ([(e1 ret pre post) (gimplify-operand st expr 0 pre post is-gimple-addressable fb-either)])
          (cond
            [(= ret GS-ERROR) (values e1 ret pre post st)]
            [else
             (let/st st ([(base pre) (prepare-gimple-addressable st (tree-operand e1 0) pre)])
               (define e2 (tree-set-operand e1 0 base))
               (cond
                 [(or (indirect-ref-p base) (and (eq? (tree-code base) 'mem_ref) (integer-zerop (tree-operand base 1))))
                  (do-indirect-ref st base pre post)]
                 [else
                  (define st1 (mark-addressable st base))
                  (define e3 (recalculate-side-effects st1 (tree-set-attr e2 'constant (is-gimple-invariant-address st1 e2))))
                  (values e3 ret pre post st1)]))]))])]))

;; gimplify_cleanup_point_expr: (values e* ret pre st)
(define (gimplify-cleanup-point-expr st e0 pre)
  (let/st st ([(e temp) (voidify-wrapper-expr st e0 #f)])
    (define c (ctx st))
    (define old-conds (gctx-conditions c))
    (define old-cleanups (gctx-conditional-cleanups c))
    (define old-in (gctx-in-cleanup-point-expr c))
    (define st1 (ctx-update st (lambda (c) (struct-copy gctx c [conditions 0] [conditional-cleanups (pvector-empty)] [in-cleanup-point-expr #t]))))
    (let/st st1 ([(_added body) (gimplify-stmt st1 (tree-operand e 0) (pvector-empty))])
      (define st2 (ctx-update st1 (lambda (c) (struct-copy gctx c [conditions old-conds] [conditional-cleanups old-cleanups] [in-cleanup-point-expr old-in]))))
      (define pre* (pvector-append pre body))
      (if temp (values temp GS-OK pre* st2) (values #f GS-ALL-DONE pre* st2)))))

;; gimplify_target_expr: (values e* ret pre post st)
(define (gimplify-target-expr st targ pre post)
  (define temp (tree-operand targ 0))
  (define init (tree-operand targ 1))
  (cond
    [init
     (define st1 (gimple-add-tmp-var st temp))
     (define ie (if (void-type-p st1 (tree-type init)) init (build2 st1 'init_expr (void-type st1) temp init)))
     (let/st st1 ([(_e _r pre post) (gimplify-expr st1 ie pre post is-gimple-stmt fb-none)])
       (values temp GS-OK pre post st1))]
    [else (values temp GS-OK pre post st)]))

;; ------------------------------------------------------------
;; c_gimplify_expr (lang hook): (values e* ret pre post st)
;; ------------------------------------------------------------
(define int-precision 32)
(define (c-promoting-integer-type-p st type)
  (and (integral-type-p st type) (< (type-attr st type 'precision) int-precision)))

;; c_common_unsigned_type: an unsigned integer type of the same precision
;; and mode; synthesized (and registered) when the dump has none
(define (unsigned-type-for st type)
  (cond
    [(type-attr st type 'unsigned) (values type st)]
    [else
     (define mv (type-main-variant st type))
     (define prec (type-attr st mv 'precision))
     (define mode (type-attr st mv 'mode))
     (define found
       (for/first ([p (in-list (intmap->literal-datum (GS-types st)))]
                   #:when (let ([t (cdr p)])
                            (and (eq? (tree-code t) 'integer_type) (tree-attr t 'unsigned)
                                 (eqv? (tree-attr t 'precision) prec) (equal? (tree-attr t 'mode) mode)
                                 (zero? (tree-attr t 'quals 0)) (not (tree-attr t 'main-variant #f))
                                 (not (tree-attr t 'nonstandard #f)))))
         (car p)))
     (cond
       [found (values found st)]
       [else
        (let/st st ([uid (fresh-type-uid st)])
          (define base (type-ref st mv))
          (define nstr (type-name-string st mv))
          (define t (make-tree 'integer_type
                               #:attrs (hash-set* (hash-remove (Tree-attrs base) 'main-variant)
                                                  'uid uid 'unsigned #t
                                                  'name (make-tree 'identifier_node #:attrs (list (cons 'id (format "unsigned ~a" (or nstr "int"))))))))
          (add-type st t))])]))

(define (c-gimplify-expr st e pre post)
  (case (tree-code e)
    [(lshift_expr rshift_expr lrotate_expr rrotate_expr)
     (define op1 (tree-operand e 1))
     (define t (type-main-variant st (tree-type op1)))
     (cond
       [(or (vector-type-p st t) (and (integral-type-p st t) (eqv? (type-attr st t 'precision) int-precision) (not (boolean-type-p st t))))
        (values e GS-UNHANDLED pre post st)]
       [else
        (let/st st ([ut (unsigned-type-for st (size-type st))])
          (values (tree-set-operand e 1 (fold-convert st ut op1)) GS-UNHANDLED pre post st))])]
    [(preincrement_expr predecrement_expr postincrement_expr postdecrement_expr)
     (define type (tree-type (tree-operand e 0)))
     (cond
       [(and (integral-type-p st type) (c-promoting-integer-type-p st type))
        (let/st st ([at (if (type-attr st type 'unsigned) (values type st) (unsigned-type-for st type))])
          (gimplify-self-mod-expr st e pre post #t at))]
       [else (values e GS-UNHANDLED pre post st)])]
    [else (values e GS-UNHANDLED pre post st)]))

;; ------------------------------------------------------------
;; gimplify_expr
;; ------------------------------------------------------------

;; One pass of the dispatch: (values e* ret pre post st).  POST is never #f here.
(define (gimplify-expr-1 st e pre post test fallback save-expr)
  (define code (tree-code e))
  (define (with-post f) (let-values ([(e* ret pre* st*) (f)]) (values e* ret pre* post st*)))
  (case code
    [(postincrement_expr postdecrement_expr preincrement_expr predecrement_expr)
     (gimplify-self-mod-expr st e pre post (not (= fallback fb-none)) (tree-type e))]
    [(view_convert_expr)
     (cond
       [(and (fb? fallback fb-rvalue) (is-gimple-reg-type st (tree-type e)) (is-gimple-reg-type st (tree-type (tree-operand e 0))))
        (let/st st ([(e1 rr pre post) (gimplify-operand st e 0 pre post is-gimple-val fb-rvalue)])
          (values (recalculate-side-effects st e1) rr pre post st))]
       [else (gimplify-compound-lval st e pre post (if (zero? fallback) fb-rvalue fallback))])]
    [(array_ref array_range_ref realpart_expr imagpart_expr component_ref)
     (gimplify-compound-lval st e pre post (if (zero? fallback) fb-rvalue fallback))]
    [(cond_expr)
     (let/st st ([(e1 rr pre) (gimplify-cond-expr st e pre fallback)])
       (cond
         [(= fallback fb-lvalue)
          (let/st st ([(tmp pre post) (get-initialized-tmp-var st e1 pre post #f)])
            (values tmp GS-OK pre post (mark-addressable st tmp)))]
         [else (values e1 rr pre post st)]))]
    [(call_expr)
     (let/st st ([(e1 rr pre) (gimplify-call-expr st e pre fallback)])
       (cond
         [(= fallback fb-lvalue)
          (let/st st ([(tmp pre post) (get-initialized-tmp-var st e1 pre post #f)])
            (values tmp GS-OK pre post (mark-addressable st tmp)))]
         [else (values e1 rr pre post st)]))]
    [(compound_expr) (with-post (lambda () (gimplify-compound-expr st e pre (not (= fallback fb-none)))))]
    [(modify_expr init_expr) (gimplify-modify-expr st e pre post (not (= fallback fb-none)))]
    [(truth_andif_expr truth_orif_expr)
     (define org-type (tree-type e))
     (define b (gimple-boolify st e))
     (values (build3 st 'cond_expr org-type b
                     (fold-convert st org-type (build-int-cst st (boolean-type st) 1))
                     (fold-convert st org-type (build-int-cst st (boolean-type st) 0)))
             GS-OK pre post st)]
    [(truth_not_expr)
     (define type (tree-type e))
     (define b (gimple-boolify st e))
     (define n
       (if (eqv? (type-attr st (tree-type b) 'precision) 1)
           (build1 st 'bit_not_expr (tree-type b) (tree-operand b 0))
           (build2 st 'bit_xor_expr (tree-type b) (tree-operand b 0) (build-int-cst st (tree-type b) 1))))
     (values (if (useless-type-conversion-p st type (tree-type n)) n (fold-convert st type n)) GS-OK pre post st)]
    [(addr_expr) (gimplify-addr-expr st e pre post)]
    [(nop_expr convert_expr)
     (cond
       [(is-empty-stmt st e) (values e GS-ALL-DONE pre post st)]
       [(or (void-type-p st (tree-type e)) (= fallback fb-none)) (values (tree-operand e 0) GS-OK pre post st)]
       [else
        (let/st st ([(e1 rr) (gimplify-conversion st e)])
          (cond
            [(not (eq? (tree-code e1) code)) (values e1 rr pre post st)]              ; replaced by something else
            [(not (and (convert-expr-p e1) (eqv? (tree-type e1) (tree-type e)))) (values e1 rr pre post st)]
            [else
             (let/st st ([(e2 rr2 pre post) (gimplify-operand st e1 0 pre post is-gimple-val fb-rvalue)])
               (values (recalculate-side-effects st e2) rr2 pre post st))]))])]
    [(fix_trunc_expr)
     (let/st st ([(e1 rr pre post) (gimplify-operand st e 0 pre post is-gimple-val fb-rvalue)])
       (values (recalculate-side-effects st e1) rr pre post st))]
    [(indirect_ref)
     (define volatilep (tree-this-volatile e))
     (define saved-ptr-type (tree-type (tree-operand e 0)))
     (define f (fold-indirect-ref st e))
     (cond
       [(not (eq? f e)) (values f GS-OK pre post st)]
       [else
        (let/st st ([(e1 rr pre post) (gimplify-operand st e 0 pre post is-gimple-reg fb-rvalue)])
          (cond
            [(= rr GS-ERROR) (values e1 rr pre post st)]
            [else
             (define m0 (build2 st 'mem_ref (tree-type e1) (tree-operand e1 0) (build-int-cst st saved-ptr-type 0)))
             (define m (recalculate-side-effects st (if volatilep (tree-set-attr m0 'volatile #t) m0)))
             (values m GS-OK pre post st)]))])]
    [(mem_ref)
     ;; fold MEM[&x, 0] -> x
     (define a (tree-operand e 0)) (define off (tree-operand e 1))
     (cond
       [(and (eq? (tree-code a) 'addr_expr) (integer-zerop off)
             (types-compatible-p st (tree-type e) (tree-type (tree-operand a 0))))
        (values (tree-operand a 0) GS-OK pre post st)]
       [else
        (let/st st ([(e1 rr pre post) (gimplify-operand st e 0 pre post is-gimple-mem-ref-addr fb-rvalue)])
          (if (= rr GS-ERROR)
              (values e1 rr pre post st)
              (values (recalculate-side-effects st e1) GS-ALL-DONE pre post st)))])]
    [(integer_cst real_cst fixed_cst string_cst complex_cst vector_cst) (values e GS-ALL-DONE pre post st)]
    [(const_decl)
     (if (fb? fallback fb-lvalue)
         (values e GS-ALL-DONE pre post st)
         (values (decl-initial st e) GS-OK pre post st))]
    [(decl_expr) (with-post (lambda () (gimplify-decl-expr st e pre)))]
    [(bind_expr) (with-post (lambda () (gimplify-bind-expr st e pre)))]
    [(loop_expr) (with-post (lambda () (gimplify-loop-expr st e pre)))]
    [(switch_expr) (with-post (lambda () (gimplify-switch-expr st e pre)))]
    [(exit_expr) (let-values ([(e1 rr st1) (gimplify-exit-expr st e)]) (values e1 rr pre post st1))]
    [(goto_expr)
     (let/st st ([(e1 rr pre post)
                  (if (eq? (tree-code (goto-destination e)) 'label_decl)
                      (values e GS-OK pre post st)
                      (gimplify-operand st e 0 pre post is-gimple-val fb-rvalue))])
       (if (= rr GS-ERROR)
           (values e1 rr pre post st)
           (values #f GS-ALL-DONE (pvector-cons-right pre (gimple-build-goto (goto-destination e1))) post st)))]
    [(predict_expr)
     (values #f GS-ALL-DONE (pvector-cons-right pre (gimple-build-predict (tree-attr e 'predictor) (tree-attr e 'outcome))) post st)]
    [(label_expr) (with-post (lambda () (gimplify-label-expr st e pre)))]
    [(case_label_expr) (with-post (lambda () (gimplify-case-label-expr st e pre)))]
    [(return_expr) (with-post (lambda () (gimplify-return-expr st e pre)))]
    [(constructor)
     (cond
       [(= fallback fb-none)
        (define temp
          (for/fold ([lst #f]) ([p (in-pvector (constructor-elts e))])
            (if (tree-side-effects (cdr p)) (append-to-statement-list st (cdr p) lst) lst)))
        (values temp (if temp GS-OK GS-ALL-DONE) pre post st)]
       [(= fallback fb-lvalue)
        (let/st st ([(tmp pre post) (get-initialized-tmp-var st e pre post #f)])
          (values tmp GS-OK pre post (mark-addressable st tmp)))]
       [else (values e GS-ALL-DONE pre post st)])]
    [(save_expr) (gimplify-save-expr st e pre post)]
    [(bit_field_ref)
     (let/st st ([(e1 rr pre post) (gimplify-operand st e 0 pre post is-gimple-lvalue fb-either)])
       (values (recalculate-side-effects st e1) rr pre post st))]
    [(cleanup_point_expr) (with-post (lambda () (gimplify-cleanup-point-expr st e pre)))]
    [(target_expr) (gimplify-target-expr st e pre post)]
    [(try_finally_expr try_catch_expr)
     ;; EH not modelled: evaluate body, then cleanup, in sequence
     (let/st st ([pre (gimplify-and-add st (tree-operand e 0) pre)]
                 [pre (gimplify-and-add st (tree-operand e 1) pre)])
       (values #f GS-ALL-DONE pre post st))]
    [(label_decl) (values e GS-ALL-DONE pre post (set-decl-flag st e 'forced #t))]
    [(statement_list) (with-post (lambda () (gimplify-statement-list st e pre)))]
    [(var_decl parm_decl) (let-values ([(e1 rr st1) (gimplify-var-or-parm-decl st e)]) (values e1 rr pre post st1))]
    [(result_decl ssa_name) (values e GS-ALL-DONE pre post st)]
    [(debug_begin_stmt) (values #f GS-ALL-DONE pre post st)]
    [(truth_and_expr truth_or_expr truth_xor_expr)
     (define orig-type (tree-type e))
     (define b (gimple-boolify st e))
     (define new-type (tree-type b))
     (cond
       [(not (useless-type-conversion-p st orig-type new-type))
        (values (fold-convert st orig-type b) GS-OK pre post st)]
       [else
        (define b1 (tree-set-code b (case (tree-code b) [(truth_and_expr) 'bit_and_expr] [(truth_or_expr) 'bit_ior_expr] [else 'bit_xor_expr])))
        (define b2
          (for/fold ([b b1]) ([i '(0 1)])
            (if (useless-type-conversion-p st new-type (tree-type (tree-operand b i)))
                b
                (tree-set-operand b i (fold-convert st new-type (tree-operand b i))))))
        (expr-2 st b2 pre post)])]
    [(pointer_plus_expr) (expr-2 st e pre post)]
    [else
     (case (tree-code-class st code)
       [(tcc_comparison)
        (define type (tree-type (tree-operand e 1)))
        (cond
          [(vector-type-p st type) (expr-2 st e pre post)]
          [(not (aggregate-type-p st type))
           (define org-type (tree-type e))
           (define b (gimple-boolify st e))
           (cond
             [(not (useless-type-conversion-p st org-type (tree-type b)))
              (values (fold-convert st org-type b) GS-OK pre post st)]
             [else (expr-2 st b pre post)])]
          [else (error 'gimplify-expr "aggregate comparison not supported")])]
       [(tcc_unary)
        (let/st st ([(e1 rr pre post) (gimplify-operand st e 0 pre post is-gimple-val fb-rvalue)])
          (values (recalculate-side-effects st e1) rr pre post st))]
       [(tcc_binary) (expr-2 st e pre post)]
       [(tcc_declaration tcc_constant) (values e GS-ALL-DONE pre post st)]
       [else (error 'gimplify-expr "unhandled tree code ~a (~a)" code (tree-code-class st code))])]))

;; the two-operand default: (values e* ret pre post st)
(define (expr-2 st e pre post)
  (let/st st ([(e1 r0 pre post) (gimplify-operand st e 0 pre post is-gimple-val fb-rvalue)]
              [(e2 r1 pre post) (gimplify-operand st e1 1 pre post is-gimple-val fb-rvalue)])
    (values (recalculate-side-effects st e2) (min r0 r1) pre post st)))

;; gimplify_expr: (values e* ret pre post st); POST #f means NULL post_p
(define (gimplify-expr st e0 pre post test fallback)
  (cond
    [(not e0) (values #f GS-ALL-DONE pre post st)]
    [else
     (define is-statement (eq? test is-gimple-stmt))
     (define internal? (not post))
     (define pre-last (pvector-length pre))
     (define post0 (or post (pvector-empty)))
     (define post-last (pvector-length post0))
     (define saved-location (GS-loc st))
     (define st0 (if (tree-loc e0) (gs-set-loc st (make-loc (tree-loc e0))) st))
     ;; the do { ... } while (ret == GS_OK) loop; GS_OK with a replaced
     ;; expression gimplifies again (a bounded number of times)
     (define-values (e ret pre1 post1 st1)
       (let loop ([e e0] [pre pre] [post post0] [st st0] [n 0])
         (when (> n 64) (error 'gimplify-expr "no convergence on ~a" (tree-code e)))
         (define e1 (strip-useless-type-conversion st e))
         (define-values (e2 lang-ret pre2 post2 st2) (c-gimplify-expr st e1 pre post))
         (cond
           [(and (= lang-ret GS-OK) (not e2)) (values #f GS-OK pre2 post2 st2)]
           [(and (= lang-ret GS-OK) (not (eq? e2 e1))) (loop e2 pre2 post2 st2 (add1 n))]
           [(and (not (= lang-ret GS-UNHANDLED)) (not (= lang-ret GS-OK))) (values e2 lang-ret pre2 post2 st2)]
           [else
            (define-values (e3 ret pre3 post3 st3) (gimplify-expr-1 st2 e2 pre2 post2 test fallback e1))
            (cond
              [(= ret GS-OK) (if e3 (loop e3 pre3 post3 st3 (add1 n)) (values #f GS-OK pre3 post3 st3))]
              [else (values e3 ret pre3 post3 st3)])])))
     (define loc (GS-loc st1))
     (define (finish e ret pre post st)
       (values e ret pre (if internal? #f post) (gs-set-loc st saved-location)))
     (cond
       [(= ret GS-ERROR) (finish (if is-statement #f e) ret pre1 post1 st1)]
       [else
        ;; statement context: discard a non-statement without side effects
        (define-values (e4 pre4 post4 st4)
          (cond
            [(and (= fallback fb-none) e (not (is-gimple-stmt st1 e)))
             (cond
               [(or (eq? (tree-code e) 'label_decl) (not (tree-side-effects e))) (values #f pre1 post1 st1)]
               [(not (tree-this-volatile e))
                (case (tree-code e)
                  [(component_ref realpart_expr imagpart_expr view_convert_expr)
                   (let/st st1 ([(_e _r pre post) (gimplify-expr st1 (tree-operand e 0) pre1 post1 test fallback)])
                     (values #f pre post st1))]
                  [(array_ref array_range_ref)
                   (let/st st1 ([(_a _r pre post) (gimplify-expr st1 (tree-operand e 0) pre1 post1 test fallback)]
                                [(_b _r2 pre post) (gimplify-expr st1 (tree-operand e 1) pre post test fallback)])
                     (values #f pre post st1))]
                  [else (error 'gimplify-expr "side-effecting non-statement ~a" (tree-code e))])]
               [else
                (let/st st1 ([tmp (create-tmp-var-raw st1 (type-main-variant st1 (tree-type e)) "vol")])
                  (define st2 (gimple-add-tmp-var st1 tmp))
                  (let/st st2 ([pre (gimplify-and-add st2 (build2 st2 'modify_expr (tree-type tmp) tmp e) pre1)])
                    (values #f pre post1 st2)))])]
            [else (values e pre1 post1 st1)]))
        (cond
          [(or (= fallback fb-none) is-statement)
           ;; internal post goes to pre; annotate everything that was added
           (define pre5 (if internal? (pvector-append pre4 post4) pre4))
           (define pre6 (annotate-all-with-location-after pre5 pre-last loc))
           (define post6 (if internal? post4 (annotate-all-with-location-after post4 post-last loc)))
           (finish #f ret pre6 post6 st4)]
          [(and (or (not internal?) (pvector-empty? post4)) (test st4 e4))
           (finish e4 ret pre4 post4 st4)]
          [else
           (let/st st4 ([(e5 pre post)
                         (cond
                           [(and (fb? fallback fb-lvalue) (or (not internal?) (pvector-empty? post4)) (is-gimple-addressable st4 e4))
                            (define ref-type (tree-type e4))
                            (let/st st4 ([pt (pointer-to st4 ref-type)]
                                         [(tmp _r pre post) (gimplify-expr st4 (build1 st4 'addr_expr pt e4) pre4 post4 is-gimple-reg fb-rvalue)])
                              (values (build2 st4 'mem_ref ref-type tmp (build-int-cst st4 pt 0)) pre post st4))]
                           [(and (fb? fallback fb-rvalue) (is-gimple-reg-rhs-or-call st4 e4))
                            (let/st st4 ([(tmp pre) (get-formal-tmp-var st4 e4 pre4)])
                              (values tmp pre post4 st4))]
                           [else (error 'gimplify-expr "gimplification failed for ~a" (tree-code e4))])])
             ;; annotate_all_with_location on the internal post, then move it to pre
             (cond
               [(and internal? (not (pvector-empty? post)))
                (define post* (for/pvector ([g (in-pvector post)]) (annotate-one-with-location g loc)))
                (finish e5 ret (pvector-append pre post*) (pvector-empty) st4)]
               [else (finish e5 ret pre post st4)]))])])]))

;; ------------------------------------------------------------
;; gimplify_body / gimplify_function_tree: (values gbind st)
;; ------------------------------------------------------------
(define (gimplify-function st fndecl body)
  (define st1 (gs-set-ctx (gs-set-loc (gs-reset-ssa (gs-set-fndecl st fndecl)) (make-loc (tree-loc fndecl))) '()))
  (define st2 (push-gimplify-context st1 #t #f))
  (let/st st2 ([(_added seq) (gimplify-stmt st2 body (pvector-empty))])
    (define outer-bind
      (let ([first (seq-first seq)])
        (cond
          [(and first (gbind? first) (= (pvector-length seq) 1)) first]
          [(not first) (gimple-build-bind (pvector-empty) (pvector-empty) #f)]
          [else (gimple-build-bind (pvector-empty) seq #f)])))
    (let/st st2 ([bind (pop-gimplify-context st2 outer-bind)])
      (values bind (gs-set-loc st2 #f)))))
