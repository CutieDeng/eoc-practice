#lang racket/base

;; ============================================================
;; opt: pass_copy_prop (gcc/tree-ssa-copy.cc), GCC 15 with the GCC 12
;; difference of copy_prop_visit_assignment
;; ============================================================
;;
;; (copyprop st fndecl cfg) -> (values cfg st)
;;
;; init_copy_prop: statements that end a block or may generate a copy
;; (stmt_may_generate_copy: a PHI, an assignment without memory
;; operands from a constant or with a single SSA use) are simulated,
;; the definitions of the others are copies of themselves; the SSA
;; propagator (propagate.rkt) visits assignments
;; (copy_prop_visit_assignment: GCC >= 13 gimple_fold_stmt_to_constant_1
;; through the copy-of values, GCC 12 the valueized rhs1), conditions
;; (fold_binary of the valueized operands) and PHIs (all executable
;; arguments with the same copy-of value).  fini_copy_prop:
;; substitute_and_fold with value_of_expr = the copy-of value: PHIs and
;; statements whose result has a value are queued for removal, uses
;; replaced (and the statement folded), successor PHI arguments get
;; constants; then the queued dead code is removed.
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../../../driver/dominance/dom.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         (only-in "../gimplify.rkt" is-gimple-min-invariant)
         "../cfg.rkt"
         (only-in "../ssa.rkt" stmt-vuse)
         (only-in "../print.rkt" print-cond-rhs)
         "ssa-ops.rkt"
         "propagate.rkt"
         "mfn.rkt"
         (only-in "simplify.rkt" fold-stmt gimple-fold-stmt-to-constant may-propagate-copy follow-ssa-edges?)
         (only-in "fold.rkt" fold-const-compare)
         (only-in "ccp.rkt" dce-from-worklist))

(provide copyprop)

(define (icst? t) (and (Tree? t) (eq? (tree-code t) 'integer_cst)))

(define (copyprop st0 fndecl cfg0)
  (define st (gs-set-fndecl st0 fndecl))
  (define major (gcc-major-version st))
  (define cfg (cfg-clear-messages cfg0))
  (define dom (compute-dom (pvector-cons-left (pvector-cons-left (cfg-bbs cfg) EXIT) ENTRY) ENTRY
                           (lambda (i) (for/pvector ([e (in-pvector (cfg-succs cfg i))]) (GEdge-dest e)))))
  (define copy-of (make-hasheqv))                    ; version -> tree
  (define eng #f)
  (define m (gcfg->mfn cfg))                          ; definitions for the folder

  (define (may-generate-copy? g)
    (cond
      [(gphi? g) #t]
      [(not (gassign? g)) #f]
      [(stmt-has-volatile-ops? g) #f]
      [(stmt-vuse g) #f]
      [(and (gassign-single-p st g) (is-gimple-min-invariant st (gassign-rhs1 g))) #t]
      [else (= 1 (length (remove-duplicates (map ssa-name-version (stmt-real-uses g)))))]))
  (define (copy-of-val v)
    (define x (hash-ref copy-of (ssa-name-version v) #f))
    (cond
      [x x]
      [else
       (define g (and eng (eng-def-stmt eng v)))
       (if (and (not (ssa-default-def? v)) g (may-generate-copy? g))
           #f
           (begin (hash-set! copy-of (ssa-name-version v) v) v))]))
  (define (valueize t) (if (ssa-name? t) (or (copy-of-val t) t) t))
  (define (same? a b) (and a b (or (eq? a b) (equal? a b))))
  (define (set-copy-of! v val)
    (define old (hash-ref copy-of (ssa-name-version v) #f))
    (hash-set! copy-of (ssa-name-version v) val)
    (not (same? old val)))

  (define (visit-assignment g)
    (define lhs (gassign-lhs g))
    (define rhs0
      (if (>= major 13)
          (parameterize ([follow-ssa-edges? #f]) (gimple-fold-stmt-to-constant st m g valueize))
          (and (gassign-single-p st g) (valueize (gassign-rhs1 g)))))
    (define rhs
      (if (and rhs0 (or (ssa-name? rhs0) (is-gimple-min-invariant st rhs0)))
          (if (may-propagate-copy st lhs rhs0) rhs0 lhs)
          lhs))
    (cond
      [(set-copy-of! lhs rhs) (values 'interesting lhs)]
      [(not (same? rhs lhs)) (values 'not-interesting lhs)]
      [else (values 'varying lhs)]))

  (define (visit-stmt e uid g)
    (define-values (res taken out)
      (cond
        [(and (gassign? g) (ssa-name? (gassign-lhs g)))
         (define-values (r o) (visit-assignment g)) (values r #f o)]
        [(gcond? g)
         (define a (valueize (gcond-lhs g))) (define b (valueize (gcond-rhs g)))
         (define v (and (icst? a) (icst? b) (fold-const-compare st (gcond-code g) (boolean-type st) a b)))
         (cond
           [(and v (icst? v))
            (define bb (eng-uid-bb e uid))
            (define-values (te fe) (extract-true-false-edges cfg bb))
            (values 'interesting (GEdge-dest (if (integer-zerop v) fe te)) #f)]
           [else (values 'varying #f #f)])]
        [else (values 'varying #f #f)]))
    (when (eq? res 'varying)
      (for ([d (in-list (stmt-all-defs g))]) (set-copy-of! d d)))
    (values res taken out))

  (define (visit-phi e uid ph)
    (define lhs (gphi-result ph))
    (define bb (eng-uid-bb e uid))
    (define val
      (let loop ([args (pvector->list (gphi-args ph))] [val #f])
        (cond
          [(null? args) val]
          [(not (eng-edge-executable? e (car (car args)) bb)) (loop (cdr args) val)]
          [else
           (define arg (cdr (car args)))
           (define av (if (ssa-name? arg) (copy-of-val arg) (valueize arg)))
           (cond
             [(not av) (loop (cdr args) val)]
             [(not val) (loop (cdr args) av)]
             [(not (same? val av)) lhs]
             [else (loop (cdr args) val)])])))
    (cond
      [(and val (may-propagate-copy st lhs val) (set-copy-of! lhs val))
       (values (if (same? val lhs) 'varying 'interesting) lhs)]
      [else (values 'not-interesting #f)]))

  (set! eng
        (ssa-propagate cfg dom
                       #:backedge-worklists? (<= major 12)
                       #:init (lambda (e)
                                (set! eng e)
                                (for* ([b (in-pvector (cfg-bbs cfg))])
                                  (for ([uid (in-list (cdr (eng-uids-of-block e b)))])
                                    (define g (eng-stmt e uid))
                                    (define again (or (stmt-ends-bb? g) (may-generate-copy? g)))
                                    (eng-set-simulate-again! e uid again)
                                    (unless again (for ([d (in-list (stmt-all-defs g))]) (set-copy-of! d d))))
                                  (for ([uid (in-list (car (eng-uids-of-block e b)))])
                                    (define ph (eng-stmt e uid))
                                    (define again (not (ssa-virtual? (gphi-result ph))))
                                    (eng-set-simulate-again! e uid again)
                                    (unless again (set-copy-of! (gphi-result ph) (gphi-result ph))))))
                       #:visit-stmt visit-stmt
                       #:visit-phi visit-phi))

  ;; fini_copy_prop: substitute_and_fold
  (define (value-of t)
    (define v (and (ssa-name? t) (hash-ref copy-of (ssa-name-version t) #f)))
    (and v (not (same? v t)) v))
  (define dce intbits-empty)
  (define changed #f)
  (let walk ([idx 0])
    (define bb (dom-node dom idx))
    (unless (or (= bb ENTRY) (= bb EXIT))
      (for ([pid (in-list (mfn-phis m bb))])
        (define ph (mfn-stmt m pid))
        (define res (gphi-result ph))
        (unless (ssa-virtual? res)
          (define sp (value-of res))
          (cond
            [(and sp (may-propagate-copy st res sp)) (set! dce (intbits-set dce (ssa-name-version res)))]
            [else
             (define args* (for/pvector ([a (in-pvector (gphi-args ph))])
                             (define v (value-of (cdr a)))
                             (if (and v (may-propagate-copy st (cdr a) v)) (cons (car a) v) a)))
             (unless (equal? args* (gphi-args ph)) (set! changed #t) (mfn-replace! m pid (phi-set-args ph args*)))])))
      (for ([id (in-list (mfn-stmts m bb))])
        (define g (mfn-stmt m id))
        (define lhs (stmt-ssa-lhs g))
        (define sp (and lhs (value-of lhs)))
        (cond
          [(and sp (may-propagate-copy st lhs sp) (not (stmt-has-side-effects? st g)))
           (set! dce (intbits-set dce (ssa-name-version lhs)))]
          [else
           (define replaced #f)
           (define g1 (stmt-map-uses st g (lambda (t) (define v (value-of t))
                                            (if (and v (may-propagate-copy st t v)) (begin (set! replaced #t) v) t))))
           (when replaced
             (set! changed #t)
             (mfn-replace! m id g1)
             (fold-stmt st m id (lambda (t) t)))]))
      ;; propagate_into_phi_args
      (for ([e (in-pvector (cfg-succs cfg bb))])
        (define d (GEdge-dest e))
        (for ([pid (in-list (mfn-phis m d))])
          (define ph (mfn-stmt m pid))
          (define args* (for/pvector ([a (in-pvector (gphi-args ph))])
                          (define arg (cdr a))
                          (define v (and (= (car a) bb) (ssa-name? arg) (not (ssa-virtual? arg)) (value-of arg)))
                          (if (and v (is-gimple-min-invariant st v) (may-propagate-copy st arg v)) (cons (car a) v) a)))
          (unless (equal? args* (gphi-args ph)) (set! changed #t) (mfn-replace! m pid (phi-set-args ph args*))))))
    (for ([k (in-pvector (dom-children dom idx))]) (walk k)))
  (define cfg1 (dce-from-worklist st (mfn->gcfg m) dce))
  (if (or changed (not (intbits-empty? dce)))
      (cleanup-tree-cfg st cfg1 print-cond-rhs)
      (values cfg1 st)))
