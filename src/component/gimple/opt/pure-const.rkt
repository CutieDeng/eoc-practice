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
;; when every loop is proved finite (finite_function_p below, with
;; opt/niter.rkt loop-finite? for max_loop_iterations).  The result is
;; dumped ("Function is locally looping/const/pure.") and the
;; declaration updated ("Function found to be const: NAME/N",
;; "Declaration updated to be const: NAME/N"): the flags live in the GS
;; declaration overrides ('readonly, 'pure, 'looping-const-or-pure,
;; 'noreturn) and reach later functions' calls through
;; gimplify-call-expr.  N is the cgraph uid from GCC 15, the symbol
;; order before (symtab_node::get_dump_name); both come from the
;; gccast plugin.
;;
;; Shared with opt/modref.rkt: dump-name, walk-stmt-loads-stores,
;; refs-local-or-readonly?, points-to-local-or-readonly?,
;; ipa-make-function-const / -pure, finite-function?, fixup-cfg.
;; From GCC 15 finite_loop_p records a proved loop in loop->finite_p
;; (loops persist between passes), and a later query prints "Assume
;; loop N to be finite: ... loop was previously finite."; the port keeps
;; the proved headers in the 'finite-loops declaration flag.
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

(provide local-pure-const
         dump-name walk-stmt-loads-stores refs-local-or-readonly? points-to-local-or-readonly? auto-var? get-base-address*
         ipa-make-function-const ipa-make-function-pure finite-function? fixup-cfg
         binds-to-current-def? function-available?)

;; pure_const_state_e: IPA_CONST < IPA_PURE < IPA_NEITHER
(define (state-rank s) (case s [(const) 0] [(pure) 1] [else 2]))
(define (state-max a b) (if (> (state-rank b) (state-rank a)) b a))

;; symtab_node::dump_name: name/order up to GCC 14, name/uid from 15
(define (dump-name st d)
  (define n (if (>= (gcc-major-version st) 15) (decl-flag st d 'cgraph-uid #f) (decl-flag st d 'symbol-order #f)))
  (format "~a/~a" (decl-print-name st d) (or n "?")))

;; decl_binds_to_current_def_p / availability for the fixtures' model: a
;; function defined in this unit binds (no -shared), an external
;; declaration is not available
(define (binds-to-current-def? st d) (not (decl-external d)))
(define (function-available? st d) (not (decl-external d)))

;; auto_var_in_fn_p
(define (auto-var? d)
  (and (memq (tree-code d) '(var_decl parm_decl result_decl)) (not (tree-static d)) (not (decl-external d))))

(define (get-base-address* t)
  (let loop ([t t])
    (cond
      [(handled-component-p t) (loop (tree-operand t 0))]
      [(and (eq? (tree-code t) 'mem_ref) (eq? (tree-code (tree-operand t 0)) 'addr_expr))
       (loop (tree-operand (tree-operand t 0) 0))]
      [else t])))

;; refs_local_or_readonly_memory_p / points_to_local_or_readonly_memory_p
;; (ipa-fnsummary.cc) without points-to: a pointer is local when it is
;; derived from the address of an automatic variable
(define (refs-local-or-readonly? st m t)
  (define b (get-base-address* t))
  (cond
    [(and (Tree? b) (memq (tree-code b) '(mem_ref target_mem_ref))) (points-to-local-or-readonly? st m (tree-operand b 0))]
    [(and (Tree? b) (decl-p st b) (auto-var? b)) #t]
    [(and (Tree? b) (decl-p st b) (decl-flag st b 'readonly #f)) #t]
    [else #f]))
(define (points-to-local-or-readonly? st m t [depth 0])
  (cond
    [(> depth 16) #f]
    [(integer-zerop t) #t]                                   ; flag_delete_null_pointer_checks
    [(ssa-name? t)
     (define g (mfn-def-stmt m t))
     (cond
       [(not g) #f]                                          ; a parameter / undefined: points to nonlocal memory
       [(gphi? g) (for/and ([a (in-pvector (gphi-args g))]) (points-to-local-or-readonly? st m (cdr a) (add1 depth)))]
       [(gassign? g)
        (case (gassign-code g)
          [(addr_expr ssa_name nop_expr convert_expr pointer_plus_expr)
           (points-to-local-or-readonly? st m (gassign-rhs1 g) (add1 depth))]
          [else #f])]
       [else #f])]
    [(eq? (tree-code t) 'addr_expr) (refs-local-or-readonly? st m (tree-operand t 0))]
    [else #f]))

;; walk_stmt_load_store_ops: ON-LOAD / ON-STORE receive the base
;; (get_base_loadstore: a declaration or an indirect reference) and the
;; full operand
(define (walk-stmt-loads-stores st g on-load on-store)
  (define (base-loadstore t)
    (define b (let loop ([t t]) (if (handled-component-p t) (loop (tree-operand t 0)) t)))
    (and (Tree? b)
         (or (decl-p st b) (memq (tree-code b) '(indirect_ref mem_ref target_mem_ref)))
         b))
  (define (visit t f) (define b (and t (base-loadstore t))) (when b (f b t)))
  (cond
    [(and (gassign? g) (gassign-single-p st g))
     (visit (gassign-lhs g) on-store)
     (visit (gassign-rhs1 g) on-load)]
    [(gcall? g)
     (when (gcall-lhs g) (visit (gcall-lhs g) on-store))
     (for ([a (in-pvector (gcall-args g))] #:unless (eq? (tree-code a) 'addr_expr)) (visit a on-load))]
    [(greturn? g)
     (define v (greturn-retval g))
     (when (and v (not (eq? (tree-code v) 'addr_expr))) (visit v on-load))]
    [else (void)]))

;; ------------------------------------------------------------
;; finite_function_p (shared with modref): (values finite? st)
;; ------------------------------------------------------------

(define (finite-function? st m cfg fndecl msg!)
  (define dom (compute-dom (pvector-cons-left (pvector-cons-left (cfg-bbs cfg) EXIT) ENTRY) ENTRY
                           (lambda (i) (for/pvector ([e (in-pvector (cfg-succs cfg i))]) (GEdge-dest e)))))
  (define-values (loops _back) (find-loops cfg dom))
  (define major (gcc-major-version st))
  (define own-flags (decl-ecf-flags st fndecl))
  (define flags-finite?
    (and (not (zero? (bitwise-and own-flags (bitwise-ior ECF-CONST ECF-PURE))))
         (zero? (bitwise-and own-flags ECF-LOOPING))))
  (define (record st l)
    (if (>= major 15)
        (set-decl-flag st fndecl 'finite-loops (cons (Loop-header l) (decl-flag st fndecl 'finite-loops '())))
        st))
  (let loop ([ls loops] [st st])
    (cond
      [(null? ls) (values #t st)]
      [else
       (define l (car ls))
       (define previously (and (>= major 15) (memv (Loop-header l) (decl-flag st fndecl 'finite-loops '()))))
       (cond
         [(and previously (pair? (loop-exits cfg l)))
          (msg! "Assume loop ~a to be finite: it has an exit and -ffinite-loops is on or loop was previously finite." (Loop-num l))
          (loop (cdr ls) st)]
         [flags-finite? (loop (cdr ls) (record st l))]
         [(loop-finite? st m cfg l) (loop (cdr ls) (record st l))]
         [else
          (msg! "    cannot prove finiteness of loop ~a" (Loop-num l))
          (values #f st)])])))

;; ------------------------------------------------------------
;; ipa_make_function_const / ipa_make_function_pure (local = true):
;; (values changed? st)
;; ------------------------------------------------------------

(define (ipa-make-function-const st fndecl looping msg!)
  (cond
    [(and (decl-flag st fndecl 'readonly #f)
          (or looping (not (decl-flag st fndecl 'looping-const-or-pure #f))))
     (values #f st)]
    [else
     (msg! "Function found to be ~aconst: ~a" (if looping "looping " "") (dump-name st fndecl))
     (msg! "Declaration updated to be ~aconst: ~a" (if looping "looping " "") (dump-name st fndecl))
     (values #t (set-decl-flag (set-decl-flag (set-decl-flag st fndecl 'readonly #t) fndecl 'looping-const-or-pure looping) fndecl 'pure #f))]))

(define (ipa-make-function-pure st fndecl looping msg!)
  (cond
    [(or (decl-flag st fndecl 'readonly #f)
         (and (decl-flag st fndecl 'pure #f)
              (or looping (not (decl-flag st fndecl 'looping-const-or-pure #f)))))
     (values #f st)]
    [else
     (msg! "Function found to be ~apure: ~a" (if looping "looping " "") (dump-name st fndecl))
     (msg! "Declaration updated to be ~apure: ~a" (if looping "looping " "") (dump-name st fndecl))
     (values #t (set-decl-flag (set-decl-flag st fndecl 'pure #t) fndecl 'looping-const-or-pure looping))]))

;; execute_fixup_cfg: calls to const/pure functions get update_stmt (a
;; fresh use-list stamp) and TODO_cleanup_cfg
(define (fixup-cfg st cfg)
  (define n0 (cfg-next-stamp cfg))
  (define-values (cfg1 n1 any?)
    (for/fold ([c cfg] [n n0] [any? #f]) ([bb (in-pvector (cfg-bbs cfg))])
      (define b (cfg-block c bb))
      (define-values (stmts n* any*)
        (for/fold ([acc (pvector-empty)] [n n] [any? any?]) ([g (in-pvector (GBlock-stmts b))])
          (cond
            [(and (gcall? g) (call-fndecl g)
                  (not (zero? (bitwise-and (gcall-ecf g) (bitwise-ior ECF-CONST ECF-PURE ECF-LOOPING)))))
             (values (pvector-cons-right acc (stmt-set-flag g 'stamp n)) (add1 n) #t)]
            [else (values (pvector-cons-right acc g) n any?)])))
      (values (if any* (set-block c (block-set-stmts b stmts)) c) n* any*)))
  (if any? (cleanup-tree-cfg st cfg1 print-cond-rhs) (values cfg1 st)))

;; ------------------------------------------------------------
;; the pass
;; ------------------------------------------------------------

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
  (define (check-op! t write?)
    (define b (get-base-address* t))
    (cond
      [(and (Tree? b) (tree-this-volatile b))
       (set-state! 'neither)
       (msg! "    Volatile indirect ref is not const/pure")]
      [(refs-local-or-readonly? st0 m t) (msg! "    Indirect ref to local or readonly memory is OK")]
      [write?
       (set-state! 'neither)
       (msg! "    Indirect ref write is not const/pure")]
      [else
       (msg! "    Indirect ref read is not const")
       (set-state! 'pure)]))
  (define (visit! b t write?) (if (decl-p st0 b) (check-decl! b write?) (check-op! b write?)))

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
  (define (check-stmt! g bb)
    (msg! "~a  scanning: ~a" (if first-scan " " "") (stmt->string st0 cfg bb g))
    (set! first-scan #f)
    (when (and (stmt-has-volatile-ops? g) (not (and (gassign? g) (tree-clobber-p (gassign-rhs1 g)))))
      (set-state! 'neither)
      (msg! "    Volatile stmt is not const/pure"))
    (walk-stmt-loads-stores st0 g (lambda (b t) (visit! b t #f)) (lambda (b t) (visit! b t #t)))
    (cond
      [(gcall? g) (check-call! g)]
      [else (void)]))

  (msg! " local analysis of ~a" (dump-name st0 fndecl))
  (for* ([bb (in-pvector (cfg-bbs cfg))] [id (in-list (mfn-stmts m bb))])
    (check-stmt! (mfn-stmt m id) bb))

  ;; finite_function_p
  (define st-f
    (cond
      [(and (not (eq? state 'neither)) (not looping))
       (define-values (finite? st1) (finite-function? st0 m cfg fndecl msg!))
       (unless finite? (set! looping #t))
       st1]
      [else st0]))

  ;; better_state with the previously known state: a plain function adds nothing
  (define-values (known-state known-looping)
    (state-from-flags (decl-ecf-flags st-f fndecl) (zero? (pvector-length (cfg-preds cfg EXIT)))))
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
    (let* ([st st-f]
           [st (cond
                 [(and (zero? (pvector-length (cfg-preds cfg EXIT))) (not (tree-this-volatile fndecl))
                       (not (decl-flag st fndecl 'noreturn #f)))
                  (msg! "Function found to be noreturn: ~a" (decl-print-name st fndecl))
                  (set! changed #t)
                  (set-decl-flag st fndecl 'noreturn #t)]
                 [else st])]
           [st (case state
                 [(const) (let-values ([(ch st) (ipa-make-function-const st fndecl looping msg!)]) (when ch (set! changed #t)) st)]
                 [(pure) (let-values ([(ch st) (ipa-make-function-pure st fndecl looping msg!)]) (when ch (set! changed #t)) st)]
                 [else st])])
      st))
  (define cfg1 (for/fold ([c cfg]) ([mm (in-list (reverse msgs))]) (cfg-add-message c mm)))
  (if changed (fixup-cfg st1 cfg1) (values cfg1 st1)))
