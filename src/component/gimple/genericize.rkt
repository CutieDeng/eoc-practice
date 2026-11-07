#lang racket/base

;; ============================================================
;; c_genericize: lower C front-end statement nodes to GENERIC (pure)
;; ============================================================
;;
;; Port of gcc/c-family/c-gimplify.cc (GPLv3+), C subset:
;;   FOR_STMT / WHILE_STMT / DO_STMT  -> labels + gotos + COND_EXPR
;;   SWITCH_STMT                      -> SWITCH_EXPR (+ break label)
;;   BREAK_STMT / CONTINUE_STMT       -> GOTO_EXPR (+ PREDICT_EXPR)
;;   STATEMENT_LIST                   -> side-effect flag maintenance
;;
;; walk_tree becomes a pure pre-order rewrite: the callback receives a
;; node and returns its replacement plus whether to visit the children.
;; No pointer set is needed because expression nodes are values (the
;; import rejects shared expressions).  The break/continue label stacks
;; and the named-loop map travel in a `gz` record together with the
;; state.
;; ============================================================

(require racket/list
         "../../kernel/data/data.rkt"
         "../../kernel/ir/gimple/types.rkt"
         "state.rkt"
         "tree.rkt")

(provide c-genericize)

;; ---- threaded record --------------------------------------------------

(struct gz (st break-stack continue-stack names))    ; stacks: pvector of labels; names: immutable hash
(define (gz-set-st z st) (gz st (gz-break-stack z) (gz-continue-stack z) (gz-names z)))
(define bc-break 0)
(define bc-continue 1)
(define (bc-stack z bc) (if (= bc bc-break) (gz-break-stack z) (gz-continue-stack z)))
(define (set-bc-stack z bc v)
  (if (= bc bc-break) (gz (gz-st z) v (gz-continue-stack z) (gz-names z)) (gz (gz-st z) (gz-break-stack z) v (gz-names z))))
(define (set-names z n) (gz (gz-st z) (gz-break-stack z) (gz-continue-stack z) n))

;; ---- break / continue labels -------------------------------------------

(define (begin-bc-block z bc [loc #f])
  (define-values (label st) (create-artificial-label (gz-st z) loc))
  (define st* (set-decl-flag st label (if (= bc bc-break) 'label-break 'label-continue) #t))
  (values label (set-bc-stack (gz-set-st z st*) bc (pvector-cons-right (bc-stack z bc) label))))

;; finish_bc_block: append the label if it was used; returns the new list
(define (finish-bc-block z block bc label)
  (unless (decl-eq? label (pvector-last (bc-stack z bc)))
    (error 'finish-bc-block "label stack mismatch"))
  (define block*
    (if (decl-flag (gz-st z) label 'used)
        (append-to-statement-list (gz-st z) (build-label-expr (gz-st z) label) block)
        block))
  (values block* (set-bc-stack z bc (pvector-drop-right (bc-stack z bc) 1))))

(define (get-bc-label z bc)
  (define stack (bc-stack z bc))
  (when (pvector-empty? stack) (error 'get-bc-label "break/continue outside of a loop or switch"))
  (define label (pvector-last stack))
  (values label (gz-set-st z (set-decl-flag (gz-st z) label 'used #t))))

(define (mark-used z label) (gz-set-st z (set-decl-flag (gz-st z) label 'used #t)))

;; ---- operand access by GCC accessor name ---------------------------------

(define (stmt-op st t accessor)
  (define pos (tree-code-name->accessor-pos st (tree-code t) accessor))
  (and pos (tree-operand t pos)))

;; ---- walk_tree -------------------------------------------------------------

;; (func z t) -> (values t* walk-subtrees? z)
(define (walk-tree z t func)
  (cond
    [(not t) (values #f z)]
    [else
     (define-values (t* sub? z1) (func z t))
     (if (and sub? t*) (walk-children z1 t* func) (values t* z1))]))

(define (walk-children z t func)
  (case (tree-code t)
    [(bind_expr)
     ;; DECL_INITIAL of the bound variables, then the body
     (define z1
       (for/fold ([z z]) ([d (in-pvector (bind-expr-vars t))])
         (define init (decl-initial (gz-st z) d))
         (cond
           [init
            (define-values (init* z*) (walk-tree z init func))
            (gz-set-st z* (set-decl-flag (gz-st z*) d 'initial init*))]
           [else z])))
     (define-values (body z2) (walk-tree z1 (tree-operand t 1) func))
     (values (tree-set-operand t 1 body) z2)]
    [(statement_list)
     (define-values (stmts z1)
       (for/fold ([acc (pvector-empty)] [z z] #:result (values acc z)) ([s (in-pvector (statement-list-stmts t))])
         (define-values (s* z*) (walk-tree z s func))
         (values (pvector-cons-right acc s*) z*)))
     (values (tree-set-attr t 'stmts stmts) z1)]
    [else
     (cond
       [(expr-p (gz-st z) t)
        (for/fold ([t t] [z z] #:result (values t z)) ([i (in-range (tree-operand-length t))])
          (define-values (o z*) (walk-tree z (tree-operand t i) func))
          (values (tree-set-operand t i o) z*))]
       [else (values t z)])]))

;; ---- genericize_c_loop -------------------------------------------------------

;; Returns (values stmt-list z).  COND-IS-FIRST? selects while/for vs do.
(define (genericize-c-loop z cnd body incr name cond-is-first? func [start-locus #f])
  (define st0 (gz-st z))
  (define-values (cnd1 z1) (walk-tree z cnd func))
  (define-values (incr1 z2) (walk-tree z1 incr func))
  (define-values (blab z3) (begin-bc-block z2 bc-break start-locus))
  (define-values (clab z4) (begin-bc-block z3 bc-continue start-locus))
  (define z5 (if name (set-names z4 (hash-set (gz-names z4) name (cons blab clab))) z4))
  (define-values (body1 z6) (walk-tree z5 body func))
  (define z7 (if name (set-names z6 (hash-remove (gz-names z6) name)) z6))
  (define st (gz-st z7))
  ;; expand to gotos
  (define-values (list0 entry exit z8)
    (cond
      [(and cnd1 (integer-zerop cnd1))
       ;; If condition is zero don't generate a loop construct.
       (cond
         [cond-is-first?
          (define-values (bl z*) (get-bc-label z7 bc-break))
          (values (append-to-statement-list st (build-goto st bl) #f) #f #f z*)]
         [else (values #f #f #f z7)])]
      [else
       (define-values (top-label st1) (create-artificial-label st start-locus))
       (define top (build-label-expr st1 top-label))
       (define exit0 (build-goto st1 top-label))
       (define z* (gz-set-st z7 st1))
       (define-values (lst entry exit z**)
         (cond
           [(and cnd1 (not (integer-nonzerop cnd1)))
            ;; Canonicalize the loop condition to the end: branch to it first.
            (define-values (lst entry z1)
              (cond
                [cond-is-first?
                 (cond
                   [incr1
                    (define-values (el st2) (create-artificial-label (gz-st z*) start-locus))
                    (define entry (build-label-expr st2 el))
                    (values (append-to-statement-list st2 (build-goto st2 el) #f) entry (gz-set-st z* st2))]
                   [else
                    (define-values (cl z2) (get-bc-label z* bc-continue))
                    (values (append-to-statement-list (gz-st z2) (build-goto (gz-st z2) cl) #f) #f z2)])]
                [else (values #f #f z*)]))
            (define-values (bl z2) (get-bc-label z1 bc-break))
            (define st2 (gz-st z2))
            (define exit1 (fold-build3 st2 'cond_expr (void-type st2) cnd1 exit0 (build-goto st2 bl)))
            (define exit2 (tree-set-loc exit1 (or (tree-loc cnd1) start-locus)))
            (values lst entry exit2 z2)]
           [else (values #f #f exit0 z*)]))
       (values (append-to-statement-list (gz-st z**) top lst) entry exit z**)]))
  (define st9 (gz-st z8))
  (define list1 (append-to-statement-list st9 body1 list0))
  (define-values (list2 z9) (finish-bc-block z8 list1 bc-continue clab))
  (define list3 (if incr1 (append-to-statement-list st9 incr1 list2) list2))
  (define list4 (append-to-statement-list st9 entry list3))
  (define list5 (append-to-statement-list st9 exit list4))
  (define-values (list6 z10) (finish-bc-block z9 list5 bc-break blab))
  (values (or list6 (build-empty-stmt (gz-st z10))) z10))

(define (genericize-for-stmt z stmt func)
  (define st (gz-st z))
  (define init (stmt-op st stmt 'FOR_INIT_STMT))
  (define-values (expr z1)
    (cond
      [init
       (define-values (init* z*) (walk-tree z init func))
       (values (append-to-statement-list (gz-st z*) init* #f) z*)]
      [else (values #f z)]))
  (define-values (loop z2)
    (genericize-c-loop z1 (stmt-op st stmt 'FOR_COND) (stmt-op st stmt 'FOR_BODY) (stmt-op st stmt 'FOR_EXPR)
                       (stmt-op st stmt 'FOR_NAME) #t func (tree-loc stmt)))
  (define expr* (append-to-statement-list (gz-st z2) loop expr))
  (values (or expr* loop) z2))

(define (genericize-while-stmt z stmt func)
  (define st (gz-st z))
  (genericize-c-loop z (stmt-op st stmt 'WHILE_COND) (stmt-op st stmt 'WHILE_BODY) #f (stmt-op st stmt 'WHILE_NAME) #t func (tree-loc stmt)))

(define (genericize-do-stmt z stmt func)
  (define st (gz-st z))
  (genericize-c-loop z (stmt-op st stmt 'DO_COND) (stmt-op st stmt 'DO_BODY) #f (stmt-op st stmt 'DO_NAME) #f func (tree-loc stmt)))

(define (genericize-switch-stmt z stmt func)
  (define st (gz-st z))
  (define body0 (or (stmt-op st stmt 'SWITCH_STMT_BODY) (build-empty-stmt st)))
  (define type (stmt-op st stmt 'SWITCH_STMT_TYPE))
  (define name (stmt-op st stmt 'SWITCH_STMT_NAME))
  (define-values (cnd z1) (walk-tree z (stmt-op st stmt 'SWITCH_STMT_COND) func))
  (define-values (blab z2) (begin-bc-block z1 bc-break (tree-loc stmt)))
  (define z3 (if name (set-names z2 (hash-set (gz-names z2) name (cons blab #f))) z2))
  (define-values (body1 z4) (walk-tree z3 body0 func))
  (define z5 (if name (set-names z4 (hash-remove (gz-names z4) name)) z4))
  (define z6 (if (decl-flag (gz-st z5) blab 'used)
                 (gz-set-st z5 (set-decl-flag (gz-st z5) blab 'switch-break-label #t))
                 z5))
  (define-values (body2 z7) (finish-bc-block z6 body1 bc-break blab))
  ;; build2_loc (stmt_locus, SWITCH_EXPR, ...)
  (values (tree-set-loc (build2 (gz-st z7) 'switch_expr type cnd body2) (tree-loc stmt)) z7))

(define (genericize-continue-stmt z stmt)
  (define st (gz-st z))
  (define name (stmt-op st stmt 'CONTINUE_NAME))
  (define-values (label z1)
    (cond
      [name (define p (hash-ref (gz-names z) name)) (values (cdr p) (mark-used z (cdr p)))]
      [else (get-bc-label z bc-continue)]))
  (define st1 (gz-st z1))
  (define pred (make-tree 'predict_expr #:type (void-type st1) #:attrs (list (cons 'predictor 'PRED_CONTINUE) (cons 'outcome 'NOT_TAKEN))))
  (define jump (tree-set-loc (build-goto st1 label) (tree-loc stmt)))
  (values (append-to-statement-list st1 jump (append-to-statement-list-force st1 pred #f)) z1))

(define (genericize-break-stmt z stmt)
  (define st (gz-st z))
  (define name (stmt-op st stmt 'BREAK_NAME))
  (define-values (label z1)
    (cond
      [name (define p (hash-ref (gz-names z) name)) (values (car p) (mark-used z (car p)))]
      [else (get-bc-label z bc-break)]))
  (values (tree-set-loc (build-goto (gz-st z1) label) (tree-loc stmt)) z1))

;; c_genericize_control_stmt: returns (values stmt* walk-subtrees? z)
(define (c-genericize-control-stmt z stmt func)
  (case (tree-code stmt)
    [(for_stmt) (let-values ([(s z) (genericize-for-stmt z stmt func)]) (values s #f z))]
    [(while_stmt) (let-values ([(s z) (genericize-while-stmt z stmt func)]) (values s #f z))]
    [(do_stmt) (let-values ([(s z) (genericize-do-stmt z stmt func)]) (values s #f z))]
    [(switch_stmt) (let-values ([(s z) (genericize-switch-stmt z stmt func)]) (values s #f z))]
    [(continue_stmt) (let-values ([(s z) (genericize-continue-stmt z stmt)]) (values s #t z))]
    [(break_stmt) (let-values ([(s z) (genericize-break-stmt z stmt)]) (values s #t z))]
    [(statement_list)
     (cond
       [(tree-side-effects stmt)
        ;; walk the statements; clear TREE_SIDE_EFFECTS when at most one
        ;; non-debug statement remains and it has none
        (define-values (stmts nondebug clear? z1)
          (for/fold ([acc (pvector-empty)] [nondebug 0] [clear? #t] [z z] #:result (values acc nondebug clear? z))
                    ([t (in-pvector (statement-list-stmts stmt))])
            (define nd (if (and (not (eq? (tree-code t) 'debug_begin_stmt)) (< nondebug 2)) (add1 nondebug) nondebug))
            (define-values (t* z*) (walk-tree z t func))
            (define clear*
              (if (and (not (eq? (tree-code t*) 'debug_begin_stmt)) (or (> nd 1) (tree-side-effects t*))) #f clear?))
            (values (pvector-cons-right acc t*) nd clear* z*)))
        (define s1 (tree-set-attr stmt 'stmts stmts))
        (values (if clear? (tree-set-attr s1 'side-effects #f) s1) #f z1)]
       [else (values stmt #f z)])]
    [else (values stmt #t z)]))

;; c_genericize (C only): (values body* st)
(define (c-genericize st fndecl body)
  (define z0 (gz (gs-set-fndecl st fndecl) (pvector-empty) (pvector-empty) (hash)))
  (define (func z t) (c-genericize-control-stmt z t func))
  (define-values (body* z1) (walk-tree z0 body func))
  (values body* (gz-st z1)))
