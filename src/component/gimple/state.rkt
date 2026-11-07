#lang racket/base

;; ============================================================
;; gimple: the threaded state (GS) and its operations
;; ============================================================
;;
;; Convention (doc/gimple-functional-ir.md 3): a function that changes
;; state takes `st` first and returns it last, `(values result ... st)`;
;; a function that only reads state takes `st` first and returns the
;; result alone.  `let/st` sequences state-changing calls:
;;
;;   (let/st st ([x (fresh-uid st)]
;;               [(d st2) ...]            ; not needed: st is rebound
;;               [_ (set-decl-flag st d 'used #t)])
;;     body ...)                          ; sees the final st
;;
;; Each right-hand side must return its new state as the last value;
;; `st` is rebound before the next binding and inside the body.
;;
;; The per-GCC-version tree-code decode table is not part of GS (it is
;; a static resource, like the source text); it is looked up by version
;; through a module-level memo.
;; ============================================================

(require racket/list
         (for-syntax racket/base)
         "../../kernel/data/data.rkt"
         "../../kernel/ir/gimple/types.rkt"
         "../../frontend/gccast/reader.rkt")

(provide let/st
         make-gs optimize? gs-set-fndecl gs-set-loc gs-set-ctx gs-reset-ssa gs-set-ssa-version gs-set-globals
         gcc-major-version
         fresh-uid fresh-type-uid fresh-tmp-var-id fresh-ssa-version ssa-free-queue ssa-queue-release ssa-flush-free ssa-clear-free
         ssa-pass-log log-ssa-release!
         decl-flag set-decl-flag decl-initial gimple-add-tmp-var take-added-locals
         type-ref type-code type-attr add-type type-type type-main-variant
         global-type void-type boolean-type size-type sizetype ptr-type
         tree-code-class tree-code-length tree-code-name->accessor-pos)

;; ------------------------------------------------------------
;; let/st
;; ------------------------------------------------------------

(define-syntax (let/st stx)
  (syntax-case stx ()
    [(_ st () body ...) #'(let () body ...)]
    [(_ st ([(id ...) e] rest ...) body ...)
     #'(let-values ([(id ... st) e]) (let/st st (rest ...) body ...))]
    [(_ st ([id e] rest ...) body ...)
     #'(let-values ([(id st) e]) (let/st st (rest ...) body ...))]))

;; ------------------------------------------------------------
;; Construction / field updates
;; ------------------------------------------------------------

(define (make-gs version #:next-uid [next-uid 100000] #:optimize [optimize 0])
  (GS version #f intmap-empty intmap-empty next-uid 0 0 (hasheq) #f '() optimize))
(define (optimize? st) (> (GS-optimize st) 0))

(define (gs-set-fndecl st d)
  (GS (GS-version st) d (GS-types st) (GS-decl-flags st) (GS-next-uid st) (GS-tmp-var-id st)
      (GS-ssa-version st) (GS-globals st) (GS-loc st) (GS-ctx st) (GS-optimize st)))
(define (gs-set-loc st l)
  (GS (GS-version st) (GS-fndecl st) (GS-types st) (GS-decl-flags st) (GS-next-uid st) (GS-tmp-var-id st)
      (GS-ssa-version st) (GS-globals st) l (GS-ctx st) (GS-optimize st)))
(define (gs-set-ctx st c)
  (GS (GS-version st) (GS-fndecl st) (GS-types st) (GS-decl-flags st) (GS-next-uid st) (GS-tmp-var-id st)
      (GS-ssa-version st) (GS-globals st) (GS-loc st) c (GS-optimize st)))
(define (gs-set-ssa-version st n)
  (GS (GS-version st) (GS-fndecl st) (GS-types st) (GS-decl-flags st) (GS-next-uid st) (GS-tmp-var-id st)
      n (GS-globals st) (GS-loc st) (GS-ctx st) (GS-optimize st)))
(define (gs-reset-ssa st)
  (GS (GS-version st) (GS-fndecl st) (GS-types st) (GS-decl-flags st) (GS-next-uid st) (GS-tmp-var-id st)
      0 (GS-globals st) (GS-loc st) (GS-ctx st) (GS-optimize st)))
(define (gs-set-types st types)
  (GS (GS-version st) (GS-fndecl st) types (GS-decl-flags st) (GS-next-uid st) (GS-tmp-var-id st)
      (GS-ssa-version st) (GS-globals st) (GS-loc st) (GS-ctx st) (GS-optimize st)))
(define (gs-set-globals st g)
  (GS (GS-version st) (GS-fndecl st) (GS-types st) (GS-decl-flags st) (GS-next-uid st) (GS-tmp-var-id st)
      (GS-ssa-version st) g (GS-loc st) (GS-ctx st) (GS-optimize st)))
(define (gs-set-decl-flags st f)
  (GS (GS-version st) (GS-fndecl st) (GS-types st) f (GS-next-uid st) (GS-tmp-var-id st)
      (GS-ssa-version st) (GS-globals st) (GS-loc st) (GS-ctx st) (GS-optimize st)))

(define (gcc-major-version st)
  (string->number (car (regexp-split #rx"[.]" (GS-version st)))))

;; ------------------------------------------------------------
;; Counters
;; ------------------------------------------------------------

;; TYPE_UID: types have their own counter in GCC (the decl uids, which
;; the dumps print as D.N, are not consumed by new types)
(define (fresh-type-uid st)
  (values (add1 (for/fold ([m 0]) ([p (in-list (intmap->literal-datum (GS-types st)))]) (max m (car p)))) st))

(define (fresh-uid st)
  (define n (GS-next-uid st))
  (values n (GS (GS-version st) (GS-fndecl st) (GS-types st) (GS-decl-flags st) (add1 n) (GS-tmp-var-id st)
                (GS-ssa-version st) (GS-globals st) (GS-loc st) (GS-ctx st) (GS-optimize st))))

;; tmp_var_id_num: returns the current number, then increments
(define (fresh-tmp-var-id st)
  (define n (GS-tmp-var-id st))
  (values n (GS (GS-version st) (GS-fndecl st) (GS-types st) (GS-decl-flags st) (GS-next-uid st) (add1 n)
                (GS-ssa-version st) (GS-globals st) (GS-loc st) (GS-ctx st) (GS-optimize st))))

;; the SSA versions a pass allocates and the ones the CFG cleanup releases
;; (merge_blocks' PHIs, removed blocks), in order: a box of
;; (hash 'allocated list 'released list), newest first, set by run-pass
(define ssa-pass-log (make-parameter #f))
(define (log-ssa! key v)
  (define b (ssa-pass-log))
  (when b (set-box! b (hash-update (unbox b) key (lambda (l) (cons v l)) '()))))
(define (log-ssa-release! v) (log-ssa! 'released v))

;; SSA versions start at 1 (0 is reserved).  make_ssa_name_fn takes the
;; last released version of the current function first (FREE_SSANAMES is a
;; LIFO), else a new one.
(define (fresh-ssa-version st)
  (define fn (GS-fndecl st))
  (define free (if fn (decl-flag st fn 'ssa-free '()) '()))
  (cond
    [(pair? free) (log-ssa! 'allocated (car free)) (values (car free) (set-decl-flag st fn 'ssa-free (cdr free)))]
    [else
     (define n (add1 (GS-ssa-version st)))
     (log-ssa! 'allocated n)
     (values n (GS (GS-version st) (GS-fndecl st) (GS-types st) (GS-decl-flags st) (GS-next-uid st) (GS-tmp-var-id st)
                   n (GS-globals st) (GS-loc st) (GS-ctx st) (GS-optimize st)))]))

;; release_ssa_name_fn queues a version (FREE_SSANAMES_QUEUE); execute_todo's
;; flush_ssaname_freelist appends the queue to the free list after a pass;
;; release_ssa (compaction) empties both.  Lists are kept with the most
;; recently released version first.
(define (ssa-free-queue st) (let ([fn (GS-fndecl st)]) (if fn (decl-flag st fn 'ssa-free-queue '()) '())))
(define (ssa-queue-release st v)
  (define fn (GS-fndecl st))
  (if fn (set-decl-flag st fn 'ssa-free-queue (cons v (ssa-free-queue st))) st))
(define (ssa-flush-free st)
  (define fn (GS-fndecl st))
  (if fn
      (set-decl-flag (set-decl-flag st fn 'ssa-free (append (ssa-free-queue st) (decl-flag st fn 'ssa-free '())))
                     fn 'ssa-free-queue '())
      st))
(define (ssa-clear-free st)
  (define fn (GS-fndecl st))
  (if fn (set-decl-flag (set-decl-flag st fn 'ssa-free '()) fn 'ssa-free-queue '()) st))

;; ------------------------------------------------------------
;; Declaration flags (mutable bits of a decl, keyed by uid)
;; ------------------------------------------------------------

;; The value carried by the declaration itself is the default; the
;; state's override wins when present.
(define (decl-flag st d key [default #f])
  (define uid (tree-attr d 'uid #f))
  (define over (and uid (intmap-ref (GS-decl-flags st) uid #f)))
  (cond
    [(and over (hash-has-key? over key)) (hash-ref over key)]
    [else (tree-attr d key default)]))

(define (set-decl-flag st d key v)
  (define uid (decl-uid d))
  (define over (intmap-ref (GS-decl-flags st) uid (hasheq)))
  (gs-set-decl-flags st (intmap-set (GS-decl-flags st) uid (hash-set over key v))))

;; gimple_add_tmp_var inside a pass: the variable is recorded on the
;; current function; the driver appends it to the local decls
;; (add_local_decl) with take-added-locals
(define (gimple-add-tmp-var st var)
  (define fn (GS-fndecl st))
  (set-decl-flag st fn 'added-locals (append (decl-flag st fn 'added-locals '()) (list var))))
(define (take-added-locals st fn locals)
  (define added (decl-flag st fn 'added-locals '()))
  (if (null? added)
      (values locals st)
      (values (for/fold ([l locals]) ([v (in-list added)]) (pvector-cons-right l v))
              (set-decl-flag st fn 'added-locals '()))))

;; DECL_INITIAL (gimplify_decl_expr clears it after moving the init out)
(define (decl-initial st d) (decl-flag st d 'initial #f))

;; ------------------------------------------------------------
;; Types
;; ------------------------------------------------------------

(define (type-ref st uid)
  (or (intmap-ref (GS-types st) uid #f)
      (error 'type-ref "unknown type uid ~a" uid)))
(define (type-code st uid) (and uid (tree-code (type-ref st uid))))
(define (type-attr st uid key [default #f]) (tree-attr (type-ref st uid) key default))
;; TREE_TYPE of a type (pointee / element / return type): a uid or #f
(define (type-type st uid) (tree-type (type-ref st uid)))
(define (type-main-variant st uid) (or (type-attr st uid 'main-variant #f) uid))

;; register a type value (its 'uid attribute is the key); existing
;; entries are kept
(define (add-type st t)
  (define uid (type-uid t))
  (values uid
          (if (intmap-has-key? (GS-types st) uid)
              st
              (gs-set-types st (intmap-set (GS-types st) uid t)))))

(define (global-type st key)
  (or (hash-ref (GS-globals st) key #f) (error 'global-type "global type ~a not installed" key)))
(define (void-type st) (global-type st 'void))
(define (boolean-type st) (global-type st 'boolean))
(define (size-type st) (global-type st 'size))        ; size_type_node (long unsigned int)
(define (sizetype st) (global-type st 'sizetype))     ; sizetype
(define (ptr-type st) (global-type st 'ptr))          ; ptr_type_node (void *)

;; ------------------------------------------------------------
;; Per-version tree code table
;; ------------------------------------------------------------

(define decode-tables (make-hash))   ; version -> hash code -> decode  (memo of a static resource)
(define (decode-table st)
  (define v (GS-version st))
  (hash-ref! decode-tables v (lambda () (load-decode-table v))))
(define (table-entry st code) (hash-ref (decode-table st) code #f))

(define (tree-code-class st code)
  (define d (table-entry st code))
  (cond
    [d (decode-class d)]
    [(eq? code 'ssa_name) 'tcc_exceptional]
    [else (error 'tree-code-class "unknown tree code ~a" code)]))

(define (tree-code-length st code)
  (define d (table-entry st code))
  (if d (decode-nargs d) 0))

;; Position of an accessor (e.g. 'FOR_BODY) among a code's operands.
(define (tree-code-name->accessor-pos st code accessor)
  (define d (table-entry st code))
  (and d (index-of (decode-operands d) accessor)))
