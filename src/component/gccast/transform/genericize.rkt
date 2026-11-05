#lang racket/base

;; ============================================================
;; c_genericize: lower C front-end statement nodes to GENERIC
;; ============================================================
;;
;; Port of gcc/c-family/c-gimplify.cc (GPLv3+), C subset:
;;   FOR_STMT / WHILE_STMT / DO_STMT  -> labels + gotos + COND_EXPR
;;   SWITCH_STMT                      -> SWITCH_EXPR (+ break label)
;;   BREAK_STMT / CONTINUE_STMT       -> GOTO_EXPR (+ PREDICT_EXPR)
;;   STATEMENT_LIST                   -> side-effect flag maintenance
;; Named loops (C2y) are supported through the bc hash map; C++ cond
;; declarations (cond_prep) are not.
;;
;; Driven by a walk_tree-style pre-order traversal with a visited set,
;; exactly like c_genericize's walk_tree (&DECL_SAVED_TREE, ...).
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../generic/tree.rkt")

(provide c-genericize!)

;; ---- break / continue label stacks ----------------------------------

(define bc-break 0)
(define bc-continue 1)
;; two label stacks (pvectors, top = last element)
(define bc-stacks (pvector (pvector-empty) (pvector-empty)))
(define (bc-stack bc) (pvector-ref bc-stacks bc))
(define (set-bc-stack! bc v) (set! bc-stacks (pvector-set bc-stacks bc v)))
(define bc-hash-map (make-parameter (make-hasheq)))

(define (begin-bc-block bc)
  (define label (create-artificial-label))
  (set-bc-stack! bc (pvector-cons-right (bc-stack bc) label))
  (set-attr! label (if (= bc bc-break) 'label-break 'label-continue) #t)
  label)

(define (finish-bc-block! block-ref bc label)
  (unless (eq? label (pvector-last (bc-stack bc)))
    (error 'finish-bc-block "label stack mismatch"))
  (when (tree-used label)
    (append-to-statement-list! (build-label-expr label) block-ref))
  (set-bc-stack! bc (pvector-drop-right (bc-stack bc) 1)))

(define (get-bc-label bc)
  (define stack (bc-stack bc))
  (when (pvector-empty? stack) (error 'get-bc-label "break/continue outside of a loop or switch"))
  (define label (pvector-last stack))
  (set-tree-used! label #t)
  label)

;; ---- operand access by GCC accessor name ----------------------------

(define (stmt-op t accessor)
  (define pos (tree-code-name->accessor-pos (tree-code t) accessor))
  (and pos (tree-operand t pos)))

;; ---- walk_tree ------------------------------------------------------

;; Pre-order walk.  FUNC receives (ref . walk-subtrees-box); if it sets
;; the box to #f the operands are not visited.  PSET (or #f) prevents
;; visiting a node twice.
(define (walk-tree! r func pset)
  (define t (ref-get r))
  (when (and t (not (and pset (hash-ref pset t #f))))
    (when pset (hash-set! pset t #t))
    (define walk-subtrees (box #t))
    (func r walk-subtrees)
    (define t* (ref-get r))
    (when (and (unbox walk-subtrees) t*)
      (walk-children! t* func pset))))

(define (walk-children! t func pset)
  (case (tree-code t)
    [(bind_expr)
     (for ([d (in-pvector (bind-expr-vars t))])
       (when (decl-initial d) (walk-tree! (attr-ref d 'initial) func pset)))
     (walk-tree! (op-ref t 1) func pset)]
    [(statement_list)
     (for ([i (in-range (pvector-length (statement-list-stmts t)))])
       (walk-tree! (stmt-ref t i) func pset))]
    [else
     (when (or (expr-p t))
       (for ([i (in-range (tree-operand-length t))])
         (walk-tree! (op-ref t i) func pset)))]))

(define (stmt-ref sl i)
  (ref (lambda () (pvector-ref (statement-list-stmts sl) i))
       (lambda (v) (set-statement-list-stmts! sl (pvector-set (statement-list-stmts sl) i v)))))

;; ---- genericize_c_loop ----------------------------------------------

(define (genericize-c-loop! stmt-ref cnd body incr name cond-is-first? walk-subtrees func [start-locus #f])
  (define stmt-list (box #f))
  (define entry #f)
  (define exit #f)
  (define cond-box (box cnd)) (define incr-box (box incr)) (define body-box (box body))
  (walk-tree! (box-ref cond-box) func #f)
  (walk-tree! (box-ref incr-box) func #f)
  (define blab (begin-bc-block bc-break))
  (define clab (begin-bc-block bc-continue))
  (when name (hash-set! (bc-hash-map) name (cons blab clab)))
  (walk-tree! (box-ref body-box) func #f)
  (set-box! walk-subtrees #f)
  (when name (hash-remove! (bc-hash-map) name))
  (set! cnd (unbox cond-box)) (set! incr (unbox incr-box)) (set! body (unbox body-box))
  (cond
    [(and cnd (integer-zerop cnd))
     ;; If condition is zero don't generate a loop construct.
     (when cond-is-first?
       (append-to-statement-list! (build-goto (get-bc-label bc-break)) (box-ref stmt-list)))]
    [else
     ;; Expand to gotos.
     (define top (build-label-expr (create-artificial-label)))
     (set! exit (build-goto (label-expr-label top)))
     (cond
       [(and cnd (not (integer-nonzerop cnd)))
        ;; Canonicalize the loop condition to the end: branch to it first.
        (when cond-is-first?
          (define t
            (cond
              [incr
               (set! entry (build-label-expr (create-artificial-label)))
               (build-goto (label-expr-label entry))]
              [else (build-goto (get-bc-label bc-continue))]))
          (append-to-statement-list! t (box-ref stmt-list)))
        (define t (build-goto (get-bc-label bc-break)))
        (set! exit (fold-build3 'cond_expr (void-type) cnd exit t))
        (set-tree-loc! exit (or (tree-loc cnd) start-locus))]
       [else (void)])
     (append-to-statement-list! top (box-ref stmt-list))])
  (append-to-statement-list! body (box-ref stmt-list))
  (finish-bc-block! (box-ref stmt-list) bc-continue clab)
  (when incr (append-to-statement-list! incr (box-ref stmt-list)))
  (append-to-statement-list! entry (box-ref stmt-list))
  (append-to-statement-list! exit (box-ref stmt-list))
  (finish-bc-block! (box-ref stmt-list) bc-break blab)
  (ref-set! stmt-ref (or (unbox stmt-list) (build-empty-stmt))))

(define (genericize-for-stmt! r walk-subtrees func)
  (define stmt (ref-get r))
  (define expr (box #f))
  (define init (stmt-op stmt 'FOR_INIT_STMT))
  (when init
    (define ib (box init))
    (walk-tree! (box-ref ib) func #f)
    (append-to-statement-list! (unbox ib) (box-ref expr)))
  (define loop (box #f))
  (genericize-c-loop! (box-ref loop) (stmt-op stmt 'FOR_COND) (stmt-op stmt 'FOR_BODY) (stmt-op stmt 'FOR_EXPR)
                      (stmt-op stmt 'FOR_NAME) #t walk-subtrees func (tree-loc stmt))
  (append-to-statement-list! (unbox loop) (box-ref expr))
  (ref-set! r (or (unbox expr) (unbox loop))))

(define (genericize-while-stmt! r walk-subtrees func)
  (define stmt (ref-get r))
  (genericize-c-loop! r (stmt-op stmt 'WHILE_COND) (stmt-op stmt 'WHILE_BODY) #f (stmt-op stmt 'WHILE_NAME) #t walk-subtrees func (tree-loc stmt)))

(define (genericize-do-stmt! r walk-subtrees func)
  (define stmt (ref-get r))
  (genericize-c-loop! r (stmt-op stmt 'DO_COND) (stmt-op stmt 'DO_BODY) #f (stmt-op stmt 'DO_NAME) #f walk-subtrees func (tree-loc stmt)))

(define (genericize-switch-stmt! r walk-subtrees func)
  (define stmt (ref-get r))
  (define body-box (box (or (stmt-op stmt 'SWITCH_STMT_BODY) (build-empty-stmt))))
  (define cond-box (box (stmt-op stmt 'SWITCH_STMT_COND)))
  (define type (stmt-op stmt 'SWITCH_STMT_TYPE))
  (define name (stmt-op stmt 'SWITCH_STMT_NAME))
  (walk-tree! (box-ref cond-box) func #f)
  (define blab (begin-bc-block bc-break))
  (when name (hash-set! (bc-hash-map) name (cons blab #f)))
  (walk-tree! (box-ref body-box) func #f)
  (when name (hash-remove! (bc-hash-map) name))
  (set-box! walk-subtrees #f)
  (when (tree-used blab) (set-attr! blab 'switch-break-label #t))
  (finish-bc-block! (box-ref body-box) bc-break blab)
  (define sw (build2 'switch_expr type (unbox cond-box) (unbox body-box)))
  (ref-set! r sw))

(define (genericize-continue-stmt! r)
  (define stmt (ref-get r))
  (define name (stmt-op stmt 'CONTINUE_NAME))
  (define label
    (cond
      [name (define p (hash-ref (bc-hash-map) name)) (set-tree-used! (cdr p) #t) (cdr p)]
      [else (get-bc-label bc-continue)]))
  (define pred (make-tree 'predict_expr #:type (void-type) #:attrs (list (cons 'predictor 'PRED_CONTINUE) (cons 'outcome 'NOT_TAKEN))))
  (define sl (box #f))
  (append-to-statement-list-force! pred (box-ref sl))
  (define jump (build-goto label))
  (set-tree-loc! jump (tree-loc stmt))
  (append-to-statement-list! jump (box-ref sl))
  (ref-set! r (unbox sl)))

(define (genericize-break-stmt! r)
  (define stmt (ref-get r))
  (define name (stmt-op stmt 'BREAK_NAME))
  (define label
    (cond
      [name (define p (hash-ref (bc-hash-map) name)) (set-tree-used! (car p) #t) (car p)]
      [else (get-bc-label bc-break)]))
  (define g (build-goto label))
  (set-tree-loc! g (tree-loc stmt))
  (ref-set! r g))

;; c_genericize_control_stmt
(define (c-genericize-control-stmt! r walk-subtrees func pset)
  (define stmt (ref-get r))
  (case (tree-code stmt)
    [(for_stmt) (genericize-for-stmt! r walk-subtrees func)]
    [(while_stmt) (genericize-while-stmt! r walk-subtrees func)]
    [(do_stmt) (genericize-do-stmt! r walk-subtrees func)]
    [(switch_stmt) (genericize-switch-stmt! r walk-subtrees func)]
    [(continue_stmt) (genericize-continue-stmt! r)]
    [(break_stmt) (genericize-break-stmt! r)]
    [(statement_list)
     (when (tree-side-effects stmt)
       (define nondebug 0)
       (define clear? #t)
       (for ([i (in-range (pvector-length (statement-list-stmts stmt)))])
         (define t (pvector-ref (statement-list-stmts stmt) i))
         (unless (eq? (tree-code t) 'debug_begin_stmt)
           (when (< nondebug 2) (set! nondebug (add1 nondebug))))
         (walk-tree! (stmt-ref stmt i) func pset)
         (define t* (pvector-ref (statement-list-stmts stmt) i))
         (when (and (not (eq? (tree-code t*) 'debug_begin_stmt))
                    (or (> nondebug 1) (tree-side-effects t*)))
           (set! clear? #f)))
       (when clear? (set-tree-side-effects! stmt #f))
       (set-box! walk-subtrees #f))]
    [else (void)]))

;; c_genericize (C only): walk the body with a pset.
(define (c-genericize! fndecl body-ref)
  (set-bc-stack! bc-break (pvector-empty))
  (set-bc-stack! bc-continue (pvector-empty))
  (parameterize ([bc-hash-map (make-hasheq)]
                 [current-function-decl fndecl])
    (define pset (make-hasheq))
    (define (func r walk-subtrees)
      (c-genericize-control-stmt! r walk-subtrees func pset))
    (walk-tree! body-ref func pset)))
