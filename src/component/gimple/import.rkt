#lang racket/base

;; ============================================================
;; gimple: import a gccast dump (kernel/ir/gccast) into Tree values
;; ============================================================
;;
;; (import-tu tu)        -> st        types of every function registered,
;;                                   global type nodes located/synthesized,
;;                                   fresh uids start above the dump's maximum
;; (import-fn st fn)     -> (values fndecl body st)
;;
;; Every Node of a function's table becomes one value: type nodes go
;; into the state's type table and are referenced by uid; declarations
;; and expressions become Tree values (a declaration referenced from
;; several places is the same value each time).  Expression-class
;; nodes and STATEMENT_LISTs must be referenced exactly once (no
;; sharing); the import checks this and fails otherwise.
;; ============================================================

(require racket/list
         "../../kernel/data/data.rkt"
         "../../kernel/ir/gimple/types.rkt"
         "../../kernel/ir/gccast/types.rkt"
         "state.rkt"
         "tree.rkt")

(provide import-tu import-fn)

;; ------------------------------------------------------------
;; Node conversion (memo threaded with the state)
;; ------------------------------------------------------------

(struct cv (memo st))     ; memo: intmap node-index -> value (Tree) or uid (types)

(define (cv-set-st c st) (cv (cv-memo c) st))
(define (cv-remember c i v) (cv (intmap-set (cv-memo c) i v) (cv-st c)))

;; resolve a dump field value
(define (res c nodes v)
  (cond
    [(TreeId? v) (node-value c nodes (TreeId-id v))]
    [(Wide? v) (values (wide->integer v) c)]
    [(null? v) (values (pvector-empty) c)]          ; empty vars/arguments/fields/elts/stmts
    [(list? v)
     (for/fold ([acc (pvector-empty)] [c c] #:result (values acc c)) ([x (in-list v)])
       (define-values (x* c*) (res c nodes x))
       (values (pvector-cons-right acc x*) c*))]
    [(pair? v)
     (let*-values ([(a c) (res c nodes (car v))] [(d c) (res c nodes (cdr v))])
       (values (cons a d) c))]
    [else (values v c)]))

(define (build-tree c nodes nd)
  (define st (cv-st c))
  (define-values (type c1) (res c nodes (Node-type nd)))
  (define-values (ops attrs c2)
    (for/fold ([ops (pvector-empty)] [attrs (hasheq)] [c c1] #:result (values ops attrs c))
              ([p (in-list (Node-fields nd))])
      (define key (car p)) (define val (cdr p))
      (case key
        [(ops) (let-values ([(v c*) (res c nodes val)]) (values v attrs c*))]
        [(context mode) (values ops (hash-set attrs key val) c)]     ; (code . uid) / "DI": not refs
        [else (let-values ([(v c*) (res c nodes val)]) (values ops (hash-set attrs key v) c*))])))
  (define attrs*
    (if (eq? (tree-code-class st (Node-code nd)) 'tcc_constant) (hash-set attrs 'constant #t) attrs))
  (values (Tree (Node-code nd) type (Node-loc nd) ops attrs*) c2))

(define (node-value c nodes i)
  (cond
    [(intmap-has-key? (cv-memo c) i) (values (intmap-ref (cv-memo c) i) c)]
    [else
     (define nd (vector-ref nodes i))
     (cond
       [(eq? (tree-code-class (cv-st c) (Node-code nd)) 'tcc_type)
        ;; register the uid before converting the body: types are cyclic
        (define uid (node-field nd 'uid))
        (define-values (t c1) (build-tree (cv-remember c i uid) nodes nd))
        (define-values (_ st1) (add-type (cv-st c1) t))
        (values uid (cv-set-st c1 st1))]
       [else
        (define-values (t c1) (build-tree c nodes nd))
        (values t (cv-remember c1 i t))])]))

;; expression-class nodes and statement lists may be referenced only once
(define (check-no-sharing st fn)
  (define nodes (Fn-nodes fn))
  (define counts (make-vector (vector-length nodes) 0))
  (define (bump! v)
    (cond
      [(TreeId? v) (vector-set! counts (TreeId-id v) (add1 (vector-ref counts (TreeId-id v))))]
      [(pair? v) (bump! (car v)) (bump! (cdr v))]
      [else (void)]))
  (for ([nd (in-vector nodes)])
    (for ([p (in-list (Node-fields nd))])
      (when (memq (car p) '(ops stmts elts initial)) (bump! (cdr p)))))
  (for ([nd (in-vector nodes)] [i (in-naturals)])
    (define cls (tree-code-class st (Node-code nd)))
    (when (and (> (vector-ref counts i) 1)
               (or (eq? (Node-code nd) 'statement_list)
                   (memq cls '(tcc_expression tcc_unary tcc_binary tcc_comparison tcc_reference tcc_statement tcc_vl_exp))))
      (error 'import-fn "~a: node ~a (~a) is referenced ~a times; shared expressions are not supported"
             (Fn-name fn) i (Node-code nd) (vector-ref counts i)))))

(define (convert-fn st fn)
  (check-no-sharing st fn)
  (define nodes (Fn-nodes fn))
  (define c0 (cv intmap-empty st))
  (define-values (decl c1) (node-value c0 nodes (TreeId-id (Fn-decl fn))))
  (define-values (body c2)
    (if (Fn-body fn) (node-value c1 nodes (TreeId-id (Fn-body fn))) (values #f c1)))
  (values decl body (cv-st c2)))

;; ------------------------------------------------------------
;; Entry points
;; ------------------------------------------------------------

(define (max-uid tu)
  (for*/fold ([m 0]) ([fn (in-list (TU-fns tu))] [n (in-vector (Fn-nodes fn))])
    (define u (node-field n 'uid #f))
    (if (and u (> u m)) u m)))

(define (import-tu tu #:optimize [optimize 0])
  (define st0 (make-gs (TU-gcc-version tu) #:next-uid (add1 (max-uid tu)) #:optimize optimize))
  (define st1
    (for/fold ([st st0]) ([fn (in-list (TU-fns tu))])
      (define-values (_d _b st*) (convert-fn st fn))
      st*))
  (install-global-types st1))

(define (import-fn st fn)
  (define-values (decl body st1) (convert-fn st fn))
  (values decl body (gs-reset-ssa (gs-set-fndecl st1 decl))))

;; ------------------------------------------------------------
;; Global type nodes (void, _Bool, sizetype, size_t, void *)
;; ------------------------------------------------------------

(define (find-type st pred)
  (for/first ([p (in-list (intmap->literal-datum (GS-types st)))] #:when (pred (car p) (cdr p))) (car p)))

(define (synth-type st code name #:precision [prec #f] #:unsigned [uns #f] #:mode [mode #f] #:pointee [pointee #f])
  (let/st st ([uid (fresh-uid st)])
    (define name-node (and name (make-tree 'identifier_node #:attrs (list (cons 'id name)))))
    (add-type st (make-tree code #:type pointee
                            #:attrs (append (list (cons 'uid uid) (cons 'name name-node) (cons 'quals 0) (cons 'align 8))
                                            (if mode (list (cons 'mode mode)) '())
                                            (if prec (list (cons 'precision prec) (cons 'unsigned uns)) '()))))))

(define (install-global-types st)
  (define (find-or st pred thunk)
    (define f (find-type st pred))
    (if f (values f st) (thunk st)))
  (let/st st ([void-t (find-or st (lambda (u t) (eq? (tree-code t) 'void_type))
                               (lambda (st) (synth-type st 'void_type "void" #:mode "VOID")))]
              [bool-t (find-or st (lambda (u t) (eq? (tree-code t) 'boolean_type))
                               (lambda (st) (synth-type st 'boolean_type "_Bool" #:precision 1 #:unsigned #t #:mode "QI")))]
              [sizetype-t (find-or st (lambda (u t) (equal? (type-name-string st u) "sizetype"))
                                   (lambda (st) (synth-type st 'integer_type "sizetype" #:precision 64 #:unsigned #t #:mode "DI")))]
              [size-t (find-or st (lambda (u t) (and (integral-type-p st u) (equal? (type-name-string st u) "long unsigned int")))
                               (lambda (st) (synth-type st 'integer_type "long unsigned int" #:precision 64 #:unsigned #t #:mode "DI")))]
              [ptr-t (find-or st (lambda (u t) (and (eq? (tree-code t) 'pointer_type) (eqv? (tree-type t) void-t)))
                              (lambda (st) (synth-type st 'pointer_type #f #:mode "DI" #:pointee void-t)))])
    (gs-set-globals st (hasheq 'void void-t 'boolean bool-t 'size size-t 'sizetype sizetype-t 'ptr ptr-t))))
