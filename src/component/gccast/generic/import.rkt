#lang racket/base

;; ============================================================
;; Import a gccast Fn (prefab dump) into the mutable GENERIC tree
;; ============================================================
;;
;; (import-fn tu fn) -> (values fndecl body)
;;   Every Node of the function's table becomes exactly one T, so
;;   sharing (types, decls referenced many times) and cycles
;;   (TREE_TYPE back-pointers) are preserved.  The decode table for
;;   the TU's GCC version must be installed in current-tree-table.
;;
;; Also locates/synthesizes the global type nodes the gimplifier
;; builds with (void, _Bool, sizetype, size_t, void *).
;; ============================================================

(require racket/list racket/match
         "tree.rkt"
         "../../../kernel/ir/gccast/types.rkt"
         "../../../frontend/gccast/reader.rkt")

(provide import-fn
         install-tu-context!
         max-decl-uid)

(define (install-tu-context! tu)
  (current-tree-table (load-decode-table (TU-gcc-version tu))))

;; Largest DECL_UID/TYPE_UID seen in a function (fresh uids start above it).
(define (max-decl-uid fn)
  (for/fold ([m 0]) ([n (in-vector (Fn-nodes fn))])
    (define u (node-field n 'uid #f))
    (if (and u (> u m)) u m)))

(define (import-fn tu fn)
  (define nodes (Fn-nodes fn))
  (define n (vector-length nodes))
  ;; pass 1: shells
  (define shells
    (for/vector ([nd (in-vector nodes)])
      (T (Node-code nd) #f (Node-loc nd) (vector) (make-hasheq))))
  (define (res v)
    (cond
      [(TreeId? v) (vector-ref shells (TreeId-id v))]
      [(Wide? v) (wide->integer v)]
      [(pair? v)
       (if (list? v) (map res v) (cons (res (car v)) (res (cdr v))))]
      [else v]))
  ;; pass 2: fill
  (for ([nd (in-vector nodes)] [t (in-vector shells)])
    (set-tree-type! t (res (Node-type nd)))
    (for ([p (in-list (Node-fields nd))])
      (define key (car p)) (define val (cdr p))
      (case key
        [(ops) (set-T-ops! t (list->vector (map res val)))]
        [(context) (set-attr! t 'context val)]        ; (code . uid), not a ref
        [(mode) (set-attr! t 'mode val)]
        [else (set-attr! t key (res val))])))
  ;; TREE_CONSTANT on constants
  (for ([t (in-vector shells)])
    (when (eq? (tree-code-class (tree-code t)) 'tcc_constant) (set-attr! t 'constant #t)))
  (install-global-types! shells)
  (values (vector-ref shells (TreeId-id (Fn-decl fn)))
          (and (Fn-body fn) (vector-ref shells (TreeId-id (Fn-body fn))))))

;; ------------------------------------------------------------
;; Global type nodes
;; ------------------------------------------------------------

(define (type-name-string t)
  (define nm (attr t 'name #f))
  (cond
    [(not nm) #f]
    [(string? nm) nm]
    [(eq? (tree-code nm) 'identifier_node) (attr nm 'id)]
    [(eq? (tree-code nm) 'type_decl) (attr nm 'name)]
    [else #f]))

(define (find-type shells pred)
  (for/first ([t (in-vector shells)] #:when (and (type-p t) (pred t))) t))

(define (synth-type code name #:precision [prec #f] #:unsigned [uns #f] #:mode [mode #f] #:size [size #f] #:pointee [pointee #f])
  (define name-node (and name (make-tree 'identifier_node #:attrs (list (cons 'id name)))))
  (define t (make-tree code #:type pointee
                       #:attrs (append (list (cons 'uid (next-uid!)) (cons 'name name-node) (cons 'quals 0) (cons 'align 8))
                                       (if mode (list (cons 'mode mode)) '())
                                       (if prec (list (cons 'precision prec) (cons 'unsigned uns)) '()))))
  (when size (set-attr! t 'size (build-int-cst t size)))
  t)

(define (install-global-types! shells)
  (define void-t (or (find-type shells void-type-p) (synth-type 'void_type "void" #:mode "VOID")))
  (define bool-t (or (find-type shells boolean-type-p) (synth-type 'boolean_type "_Bool" #:precision 1 #:unsigned #t #:mode "QI")))
  (define sizetype-t (or (find-type shells (lambda (t) (equal? (type-name-string t) "sizetype")))
                         (synth-type 'integer_type "sizetype" #:precision 64 #:unsigned #t #:mode "DI")))
  (define size-t (or (find-type shells (lambda (t) (and (integral-type-p t) (equal? (type-name-string t) "long unsigned int"))))
                     (synth-type 'integer_type "long unsigned int" #:precision 64 #:unsigned #t #:mode "DI")))
  (define ptr-t (or (find-type shells (lambda (t) (and (pointer-type-p t) (void-type-p (tree-type t)))))
                    (synth-type 'pointer_type #f #:mode "DI" #:pointee void-t)))
  (set-global-types! #:void void-t #:boolean bool-t #:size size-t #:sizetype sizetype-t #:ptr ptr-t))
