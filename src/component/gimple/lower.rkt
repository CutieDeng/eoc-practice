#lang racket/base

;; ============================================================
;; lower: pass_lower_cf (gcc/gimple-low.cc), C subset, pure
;; ============================================================
;;
;; - GIMPLE_BIND is flattened; its variables are recorded in
;;   local-decls (record_vars: VAR_DECLs that are not DECL_EXTERNAL);
;;   every statement gets the BLOCK of its innermost BIND
;;   (gimple_set_block; the outermost BIND's BLOCK is NULL).
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
;;
;; (lower-function st fndecl outer-bind) -> (values body local-decls st)
;; ============================================================

(require racket/list
         "../../kernel/data/data.rkt"
         "../../kernel/ir/gimple/types.rkt"
         "state.rkt"
         "tree.rkt"
         "gimple.rkt")

(provide lower-function)

;; struct lower_data
(struct ld (local-decls return-statements cannot-fallthru block))
;;   return-statements: pvector of (cons label greturn), pushed right

(define (ld-set-decls d v) (ld v (ld-return-statements d) (ld-cannot-fallthru d) (ld-block d)))
(define (ld-set-returns d v) (ld (ld-local-decls d) v (ld-cannot-fallthru d) (ld-block d)))
(define (ld-set-cannot d v) (ld (ld-local-decls d) (ld-return-statements d) v (ld-block d)))
(define (ld-set-block d v) (ld (ld-local-decls d) (ld-return-statements d) (ld-cannot-fallthru d) v))

;; return_stmt retvals are compared by identity in GCC: decls by uid, else by value
(define (retval-same? a b)
  (cond
    [(and (not a) (not b)) #t]
    [(and a b (tree-attr a 'uid #f) (tree-attr b 'uid #f)) (decl-eq? a b)]
    [else (equal? a b)]))

(define (lower-function st fndecl outer-bind)
  (define st0 (gs-set-fndecl st fndecl))
  (define end-locus (make-loc (tree-attr fndecl 'end-locus #f)))

  ;; record_vars
  (define (record-vars d vars)
    (ld-set-decls d (for/fold ([acc (ld-local-decls d)]) ([v (in-pvector vars)])
                      (if (and (var-p v) (not (decl-external v))) (pvector-cons-right acc v) acc))))

  ;; lower_sequence: (values out d st)
  (define (lower-sequence st d seq)
    (for/fold ([out (pvector-empty)] [d d] [st st] #:result (values out d st)) ([g (in-pvector seq)])
      (lower-stmt st d g out)))

  ;; lower_gimple_bind: a nested BIND with a BLOCK scopes the block of its statements
  (define (lower-gimple-bind st d b out [outermost? #f])
    (define old (ld-block d))
    (define nb (and (not outermost?) (gbind-block b)))
    (define d1 (record-vars (if nb (ld-set-block d nb) d) (gbind-vars b)))
    (define-values (body d2 st1) (lower-sequence st d1 (gbind-body b)))
    (values (pvector-append out body) (if nb (ld-set-block d2 old) d2) st1))

  ;; lower_gimple_return: (values out d st)
  (define (lower-gimple-return st d g out)
    (define rs (ld-return-statements d))
    (define found-i
      (for/first ([i (in-range (sub1 (pvector-length rs)) -1 -1)]
                  #:when (retval-same? (greturn-retval g) (greturn-retval (cdr (pvector-ref rs i)))))
        i))
    (define-values (label d1 st1)
      (cond
        [found-i
         (define e (pvector-ref rs found-i))
         (values (car e)
                 (ld-set-returns d (pvector-set rs found-i (cons (car e) (stmt-set-loc (cdr e) #f))))
                 st)]
        [else
         (let/st st ([label (create-artificial-label st (loc-spot end-locus))])
           (values label (ld-set-returns d (pvector-cons-right rs (cons label g))) st))]))
    (define st2 (if (and (not (optimize? st1)) (stmt-has-location? g)) (set-decl-flag st1 label 'artificial #f) st1))
    (values (pvector-cons-right out (gimple-build-goto label (stmt-loc g))) d1 st2))

  ;; lower_stmt: (values out d st)
  (define (lower-stmt st d g0 out)
    (define g (stmt-set-block g0 (ld-block d)))
    (cond
      [(gbind? g) (lower-gimple-bind st d g out)]
      [(or (gcond? g) (ggoto? g) (gswitch? g))
       (values (pvector-cons-right out g) (ld-set-cannot d #t) st)]
      [(greturn? g)
       (cond
         [(ld-cannot-fallthru d) (values out d st)]            ; unreachable return: removed
         [else
          (define-values (out1 d1 st1) (lower-gimple-return st d g out))
          (values out1 (ld-set-cannot d1 #t) st1)])]
      [(gtry? g)
       (define-values (ev d1 st1) (lower-sequence st d (gtry-eval g)))
       (define c1 (ld-cannot-fallthru d1))
       (define-values (cl d2 st2) (lower-sequence st1 (ld-set-cannot d1 #f) (gtry-cleanup g)))
       (values (pvector-cons-right out (gtry (gtry-loc g) (gtry-flags g) ev cl (gtry-kind g)))
               (ld-set-cannot d2 (or (ld-cannot-fallthru d2) c1))
               st2)]
      [(gcall? g) (values (pvector-cons-right out g) (ld-set-cannot d (gimple-call-noreturn-p g)) st)]
      [else (values (pvector-cons-right out g) (ld-set-cannot d #f) st)]))   ; nop/assign/predict/label

  ;; lower_function_body
  (define d0 (ld (pvector-empty) (pvector-empty) #f #f))
  (define-values (body d1 st1) (lower-gimple-bind st0 d0 outer-bind (pvector-empty) #t))
  (define rs (ld-return-statements d1))
  (define may0 (gimple-seq-may-fallthru body))
  (define-values (body1 may1)
    (if (and may0 (or (pvector-empty? rs) (greturn-retval (cdr (pvector-last rs)))))
        (values (pvector-cons-right body (gimple-build-return #f end-locus)) #f)
        (values body may0)))
  (define body2
    (for/fold ([body body1] [may may1] #:result body) ([e (in-pvector (pvector-reverse rs))])
      (define ret (if may (stmt-set-loc (cdr e) #f) (cdr e)))
      (values (pvector-cons-right (pvector-cons-right body (gimple-build-label (car e))) ret) #f)))
  (values body2 (ld-local-decls d1) st1))
