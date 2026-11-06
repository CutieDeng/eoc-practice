#lang racket/base

;; ============================================================
;; gimple-low: pass_lower_cf (gcc/gimple-low.cc), C subset
;; ============================================================
;;
;; - GIMPLE_BIND is flattened; its variables are recorded in
;;   local-decls (record_vars: VAR_DECLs that are not DECL_EXTERNAL).
;; - Every GIMPLE_RETURN becomes a goto to a per-retval return label;
;;   the label + the first such return are emitted at the end of the
;;   function, LIFO over the distinct retvals.  At -O0 a return label
;;   whose return had a location is made non-artificial (this is what
;;   keeps it alive as <L%d> in the CFG dump).
;; - A return that cannot be reached (right after a goto/cond/switch/
;;   noreturn call/return) is dropped.
;; - A body that may fall through gets `return;` appended (or reuses a
;;   registered void return).
;; - GIMPLE_TRY is kept (pass_lower_eh expands it later); its eval and
;;   cleanup sequences are lowered in place.
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../generic/tree.rkt"
         "../generic/gimple.rkt")

(provide lower-function (struct-out lowered))

(struct lowered (body local-decls))       ; body: seq ; local-decls: pvector (record order)

(define optimize? (make-parameter #f))

(define (lower-function fndecl outer-bind)
  (parameterize ([current-function-decl fndecl])
    (define local-decls (box (pvector-empty)))
    (define return-statements (box (pvector-empty)))   ; of (cons label greturn), push right
    (define cannot-fallthru (box #f))
    (define current-block (box #f))                      ; lower_data.block (outermost BLOCK = NULL)
    (define end-locus (make-loc (attr fndecl 'end-locus #f)))

    (define (record-vars! vars)
      (for ([v (in-pvector vars)])
        (when (and (var-p v) (not (decl-external v)))
          (set-box! local-decls (pvector-cons-right (unbox local-decls) v)))))

    ;; lower_sequence: returns a new seq
    (define (lower-sequence s)
      (define out (make-seq))
      (for ([g (in-seq s)]) (lower-stmt! g out))
      out)

    ;; lower_gimple_bind: a nested BIND with a BLOCK scopes the block of its statements
    (define (lower-gimple-bind! b out [outermost? #f])
      (define old (unbox current-block))
      (define nb (and (not outermost?) (gbind-block b)))
      (when nb (set-box! current-block nb))
      (record-vars! (gbind-vars b))
      (seq-add-seq! out (lower-sequence (gbind-body b)))
      (when nb (set-box! current-block old)))

    ;; lower_gimple_return
    (define (lower-gimple-return! g out)
      (define rs (unbox return-statements))
      (define found
        (for/first ([i (in-range (sub1 (pvector-length rs)) -1 -1)]
                    #:when (eq? (greturn-retval g) (greturn-retval (cdr (pvector-ref rs i)))))
          (pvector-ref rs i)))
      (define entry
        (cond
          [found (gimple-set-location! (cdr found) #f) found]
          [else
           (define label (create-artificial-label (loc-spot end-locus)))
           (define e (cons label g))
           (set-box! return-statements (pvector-cons-right rs e))
           e]))
      (when (and (not (optimize?)) (gimple-has-location? g))
        (set-attr! (car entry) 'artificial #f))
      (define t (gimple-build-goto (car entry)))
      (gimple-set-location! t (gimple-location g))
      (seq-add! out t))

    (define (lower-stmt! g out)
      (gimple-set-block! g (unbox current-block))
      (cond
        [(gbind? g) (lower-gimple-bind! g out)]
        [(or (gcond? g) (ggoto? g) (gswitch? g))
         (seq-add! out g) (set-box! cannot-fallthru #t)]
        [(greturn? g)
         (cond
           [(unbox cannot-fallthru) (void)]                 ; unreachable return: removed
           [else (lower-gimple-return! g out) (set-box! cannot-fallthru #t)])]
        [(gtry? g)
         (define ev (lower-sequence (gtry-eval g)))
         (define c1 (unbox cannot-fallthru))
         (set-box! cannot-fallthru #f)
         (define cl (lower-sequence (gtry-cleanup g)))
         (set-box! cannot-fallthru (or (unbox cannot-fallthru) c1))
         (seq-add! out (gtry ev cl (gtry-kind g)))]
        [(gcall? g)
         (seq-add! out g)
         (set-box! cannot-fallthru (gimple-call-noreturn-p g))]
        [else (seq-add! out g) (set-box! cannot-fallthru #f)]))   ; nop/assign/predict/label

    ;; lower_function_body
    (define body (make-seq))
    (lower-gimple-bind! outer-bind body #t)
    (define may (gimple-seq-may-fallthru body))
    (define rs (unbox return-statements))
    (when (and may (or (pvector-empty? rs) (greturn-retval (cdr (pvector-last rs)))))
      (seq-add! body (gimple-build-return #f end-locus))
      (set! may #f))
    (for ([e (in-pvector (pvector-reverse (unbox return-statements)))])
      (seq-add! body (gimple-build-label (car e)))
      (seq-add! body (cdr e))
      (when may (gimple-set-location! (cdr e) #f) (set! may #f)))
    (lowered body (unbox local-decls))))
