#lang racket/base

;; ============================================================
;; opt: pass_local_pure_const (gcc/ipa-pure-const.cc, local mode),
;; dump "local-pure-const1"
;; ============================================================
;;
;; analyze_function: every statement of every block is scanned
;; ("  scanning: STMT"); volatile operands, loads and stores
;; (walk_stmt_load_store_ops with check_decl / check_op) and calls
;; (check_call: the callee's ECF flags through state_from_flags, self
;; recursion) lower the state const > pure > neither and may set
;; "looping".  A const/pure function with loops stays non-looping only
;; when every loop is proved finite (opt/niter.rkt loop-finite?).  The
;; result is dumped ("Function is locally looping/const/pure.") and the
;; declaration updated ("Function found to be const: NAME/N",
;; "Declaration updated to be const: NAME/N"): the flags live in the GS
;; declaration overrides ('readonly, 'pure, 'looping-const-or-pure,
;; 'noreturn) and reach later functions' calls through
;; gimplify-call-expr.  N is the cgraph uid from GCC 15, the symbol
;; order before (symtab_node::get_dump_name); both come from the
;; gccast plugin.
;;
;; Not ported: exceptions (stmt_could_throw_p, nothrow discovery: the C
;; front end marks every function nothrow already), malloc discovery,
;; the builtin tables (builtin_safe_for_const_function_p, setjmp,
;; longjmp), points-to information for pointer dereferences (a pointer
;; is local only when it is derived from the address of an automatic
;; variable), execute_fixup_cfg beyond cleanup_tree_cfg.
;; ============================================================

(require racket/list racket/string racket/port
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
         "loops.rkt"
         "niter.rkt")

(provide local-pure-const)

;; pure_const_state_e: IPA_CONST < IPA_PURE < IPA_NEITHER
(define (state-rank s) (case s [(const) 0] [(pure) 1] [else 2]))
(define (state-max a b) (if (> (state-rank b) (state-rank a)) b a))

(define (dump-name st d)
  (define n (if (>= (gcc-major-version st) 15) (decl-flag st d 'cgraph-uid #f) (decl-flag st d 'symbol-order #f)))
  (format "~a/~a" (decl-print-name st d) (or n "?")))

(define (local-pure-const st fndecl cfg0)
  (define st0 (gs-set-fndecl st fndecl))
  (define cfg (cfg-clear-messages cfg0))
  (define m (gcfg->mfn cfg))
  (define msgs '())
  (define (msg! fmt . args) (set! msgs (cons (apply format fmt args) msgs)))
  (define state 'const)
  (define looping #f)
  (define (set-state! s) (set! state (state-max state s)))
  (define first-scan #t)

  ;; -------- check_decl / check_op (local mode)
  (define (check-decl! t write?)
    (cond
      [(tree-this-volatile t)
       (set-state! 'neither)
       (msg! "    Volatile operand is not const/pure")]
      [(and (not (tree-static t)) (not (decl-external t))) (void)]
      [write?
       (set-state! 'neither)
       (msg! "    static/global memory write is not const/pure")]
      [(or (decl-external t) (tree-attr t 'public #f))
       (unless (decl-flag st0 t 'readonly #f)
         (msg! "    global memory read is not const")
         (set-state! 'pure))]
      [else
       (unless (decl-flag st0 t 'readonly #f)
         (msg! "    static memory read is not const")
         (set-state! 'pure))]))
  ;; auto_var_in_fn_p
  (define (auto-var? d)
    (and (memq (tree-code d) '(var_decl parm_decl result_decl)) (not (tree-static d)) (not (decl-external d))))
  ;; refs_local_or_readonly_memory_p / points_to_local_or_readonly_memory_p
  (define (refs-local-or-readonly? t)
    (define b (get-base-address* t))
    (cond
      [(and (Tree? b) (memq (tree-code b) '(mem_ref target_mem_ref))) (points-to-local-or-readonly? (tree-operand b 0) 0)]
      [(and (Tree? b) (decl-p st0 b) (auto-var? b)) #t]
      [(and (Tree? b) (decl-p st0 b) (decl-flag st0 b 'readonly #f)) #t]
      [else #f]))
  (define (points-to-local-or-readonly? t depth)
    (cond
      [(> depth 16) #f]
      [(integer-zerop t) #t]                                   ; flag_delete_null_pointer_checks
      [(ssa-name? t)
       (define g (mfn-def-stmt m t))
       (cond
         [(not g) #f]                                          ; a parameter / undefined: points to nonlocal memory
         [(gphi? g) (for/and ([a (in-pvector (gphi-args g))]) (points-to-local-or-readonly? (cdr a) (add1 depth)))]
         [(gassign? g)
          (case (gassign-code g)
            [(addr_expr ssa_name nop_expr convert_expr pointer_plus_expr)
             (points-to-local-or-readonly? (gassign-rhs1 g) (add1 depth))]
            [else #f])]
         [else #f])]
      [(eq? (tree-code t) 'addr_expr) (refs-local-or-readonly? (tree-operand t 0))]
      [else #f]))
  (define (get-base-address* t)
    (let loop ([t t])
      (cond
        [(handled-component-p t) (loop (tree-operand t 0))]
        [(and (eq? (tree-code t) 'mem_ref) (eq? (tree-code (tree-operand t 0)) 'addr_expr))
         (loop (tree-operand (tree-operand t 0) 0))]
        [else t])))
  (define (check-op! t write?)
    (define b (get-base-address* t))
    (cond
      [(and (Tree? b) (tree-this-volatile b))
       (set-state! 'neither)
       (msg! "    Volatile indirect ref is not const/pure")]
      [(refs-local-or-readonly? t) (msg! "    Indirect ref to local or readonly memory is OK")]
      [write?
       (set-state! 'neither)
       (msg! "    Indirect ref write is not const/pure")]
      [else
       (msg! "    Indirect ref read is not const")
       (set-state! 'pure)]))
  ;; get_base_loadstore
  (define (base-loadstore t)
    (define b (let loop ([t t]) (if (handled-component-p t) (loop (tree-operand t 0)) t)))
    (and (Tree? b)
         (or (decl-p st0 b) (memq (tree-code b) '(indirect_ref mem_ref target_mem_ref)))
         b))
  (define (visit! t write?)
    (define b (and t (base-loadstore t)))
    (when b (if (decl-p st0 b) (check-decl! b write?) (check-op! b write?))))
  ;; walk_stmt_load_store_ops
  (define (walk-loads-stores! g)
    (cond
      [(and (gassign? g) (gassign-single-p st0 g))
       (visit! (gassign-lhs g) #t)
       (visit! (gassign-rhs1 g) #f)]
      [(gcall? g)
       (when (gcall-lhs g) (visit! (gcall-lhs g) #t))
       (for ([a (in-pvector (gcall-args g))] #:unless (eq? (tree-code a) 'addr_expr)) (visit! a #f))]
      [(greturn? g)
       (define v (greturn-retval g))
       (when (and v (not (eq? (tree-code v) 'addr_expr))) (visit! v #f))]
      [else (void)]))

  ;; -------- state_from_flags / worse_state
  (define (state-from-flags flags cannot-return?)
    (define (f? b) (not (zero? (bitwise-and flags b))))
    (define l (f? ECF-LOOPING))
    (cond
      [(f? ECF-CONST) (values 'const l)]
      [(f? ECF-PURE) (values 'pure l)]
      [cannot-return? (values 'pure #t)]
      [else (values 'neither #t)]))
  (define (worse-state! s l) (set-state! s) (set! looping (or looping l)))

  ;; -------- check_call
  (define (check-call! g)
    (define flags (gcall-ecf g))
    (define callee (call-fndecl g))
    (cond
      [(and callee (decl-eq? callee fndecl))
       (msg! "    Recursive call can loop.")
       (set! looping #t)]
      [else
       (define noreturn? (not (zero? (bitwise-and flags ECF-NORETURN))))
       (define-values (s l) (state-from-flags flags noreturn?))   ; !flag_exceptions && ECF_NORETURN
       (worse-state! s l)]))

  ;; -------- check_stmt
  (define (stmt-string g bb)
    (string-trim
     (parameterize ([print-cfg-mode #t] [current-print-block (cons cfg bb)])
       (with-output-to-string (lambda () (print-stmt st0 g 0 (current-output-port)))))
     "\n" #:left? #f))
  (define (check-stmt! g bb)
    (msg! "~a  scanning: ~a" (if first-scan " " "") (stmt-string g bb))
    (set! first-scan #f)
    (when (and (stmt-has-volatile-ops? g) (not (and (gassign? g) (tree-clobber-p (gassign-rhs1 g)))))
      (set-state! 'neither)
      (msg! "    Volatile stmt is not const/pure"))
    (walk-loads-stores! g)
    (cond
      [(gcall? g) (check-call! g)]
      [else (void)]))

  (msg! " local analysis of ~a" (dump-name st0 fndecl))
  (for* ([bb (in-pvector (cfg-bbs cfg))] [id (in-list (mfn-stmts m bb))])
    (check-stmt! (mfn-stmt m id) bb))

  ;; finite_function_p
  (when (and (not (eq? state 'neither)) (not looping))
    (define dom (compute-dom (pvector-cons-left (pvector-cons-left (cfg-bbs cfg) EXIT) ENTRY) ENTRY
                             (lambda (i) (for/pvector ([e (in-pvector (cfg-succs cfg i))]) (GEdge-dest e)))))
    (define-values (loops _back) (find-loops cfg dom))
    (define own-flags (decl-ecf-flags st0 fndecl))
    (define previously-finite?
      (and (not (zero? (bitwise-and own-flags (bitwise-ior ECF-CONST ECF-PURE))))
           (zero? (bitwise-and own-flags ECF-LOOPING))))
    (unless previously-finite?
      (for/first ([l (in-list loops)] #:unless (loop-finite? st0 m cfg l))
        (msg! "    cannot prove finiteness of loop ~a" (Loop-num l))
        (set! looping #t))))

  ;; better_state with the previously known state: a plain function adds nothing
  (define-values (known-state known-looping)
    (state-from-flags (decl-ecf-flags st0 fndecl) (zero? (pvector-length (cfg-preds cfg EXIT)))))
  (when (< (state-rank known-state) (state-rank state))
    (set! looping (if (eq? state 'neither) known-looping (and looping known-looping)))
    (set! state known-state))
  (when (and (= (state-rank known-state) (state-rank state)) (not (eq? known-state 'neither)))
    (set! looping (and looping known-looping)))

  (when looping (msg! "Function is locally looping."))
  (when (eq? state 'const) (msg! "Function is locally const."))
  (when (eq? state 'pure) (msg! "Function is locally pure."))

  ;; pass_local_pure_const::execute
  (define changed #f)
  (define st1
    (let* ([st st0]
           [st (cond
                 [(and (zero? (pvector-length (cfg-preds cfg EXIT))) (not (tree-this-volatile fndecl))
                       (not (decl-flag st fndecl 'noreturn #f)))
                  (msg! "Function found to be noreturn: ~a" (decl-print-name st fndecl))
                  (set! changed #t)
                  (set-decl-flag st fndecl 'noreturn #t)]
                 [else st])]
           [st (case state
                 [(const)
                  (cond
                    [(and (decl-flag st fndecl 'readonly #f)
                          (or looping (not (decl-flag st fndecl 'looping-const-or-pure #f))))
                     st]
                    [else
                     (msg! "Function found to be ~aconst: ~a" (if looping "looping " "") (dump-name st fndecl))
                     (msg! "Declaration updated to be ~aconst: ~a" (if looping "looping " "") (dump-name st fndecl))
                     (set! changed #t)
                     (set-decl-flag (set-decl-flag (set-decl-flag st fndecl 'readonly #t) fndecl 'looping-const-or-pure looping) fndecl 'pure #f)])]
                 [(pure)
                  (cond
                    [(or (decl-flag st fndecl 'readonly #f)
                         (and (decl-flag st fndecl 'pure #f)
                              (or looping (not (decl-flag st fndecl 'looping-const-or-pure #f)))))
                     st]
                    [else
                     (msg! "Function found to be ~apure: ~a" (if looping "looping " "") (dump-name st fndecl))
                     (msg! "Declaration updated to be ~apure: ~a" (if looping "looping " "") (dump-name st fndecl))
                     (set! changed #t)
                     (set-decl-flag (set-decl-flag st fndecl 'pure #t) fndecl 'looping-const-or-pure looping)])]
                 [else st])])
      st))
  (define cfg1 (for/fold ([c cfg]) ([mm (in-list (reverse msgs))]) (cfg-add-message c mm)))
  ;; execute_fixup_cfg: TODO_cleanup_cfg when the function calls a const/pure function
  (define needs-cleanup?
    (and changed
         (for*/or ([bb (in-pvector (cfg-bbs cfg1))] [g (in-pvector (GBlock-stmts (cfg-block cfg1 bb)))])
           (and (gcall? g) (call-fndecl g)
                (not (zero? (bitwise-and (gcall-ecf g) (bitwise-ior ECF-CONST ECF-PURE ECF-LOOPING))))))))
  (if needs-cleanup?
      (cleanup-tree-cfg st1 cfg1 print-cond-rhs)
      (values cfg1 st1)))
