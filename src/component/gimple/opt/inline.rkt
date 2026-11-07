#lang racket/base

;; ============================================================
;; opt: tree-inline (gcc/tree-inline.cc optimize_inline_calls /
;; expand_call_inline) for the inline decisions of opt/ipa.rkt
;; ============================================================
;;
;; Blocks are walked forward and their statements backward
;; (gimple_expand_calls_inline); every call whose callee is in the plan
;; is expanded: the block is split before the call, the callee's blocks
;; are copied right after it in order (counts scaled by call count /
;; callee entry count, probabilities kept), SSA names of the body get
;; fresh versions (parameter default definitions become the arguments:
;; setup_one_parameter's SSA case), local declarations and labels are
;; copied, `return x` becomes `retvar = x` for an anonymous variable of
;; the return type (declare_return_variable) and the call is replaced
;; by `lhs = retvar`; return edges fall through to the block after the
;; call.  After all calls of the function, update_ssa renames the new
;; variables (one dominator walk: their SSA versions follow the copied
;; bodies' versions).  Virtual operands of the body are threaded from
;; the call's VUSE; the call's VDEF users take the memory state at the
;; callee's exit (single-exit callees).
;;
;; Not ported: statement folding of the copied body, debug binds,
;; non-register parameters needing initialization statements beyond a
;; plain copy, aggregate / by-reference returns, EH, abnormal edges,
;; static chains, multi-exit callees with memory side effects.
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         "../gimplify.rkt"
         "../cfg.rkt"
         "../ssa.rkt"
         "../print.rkt"
         "ssa-ops.rkt")

(provide inline-calls)

(define (scale-count v num den)
  (if (or (not v) (not num) (not den) (zero? den)) v (quotient (+ (* v num) (quotient den 2)) den)))

;; the memory state live at the end of callee block BB (an SSA name of
;; the callee), following single predecessors; #f when unknown
(define (exit-memory-state ccfg bb entry-vuse)
  (let loop ([bb bb] [depth 0])
    (cond
      [(> depth 64) #f]
      [else
       (define b (cfg-block ccfg bb))
       (define last-vdef (for/last ([g (in-pvector (GBlock-stmts b))] #:when (stmt-vdef g)) (stmt-vdef g)))
       (cond
         [last-vdef last-vdef]
         [(for/first ([ph (in-pvector (GBlock-phis b))] #:when (ssa-virtual? (gphi-result ph))) (gphi-result ph)) => values]
         [(= 1 (pvector-length (GBlock-preds b)))
          (define p (pvector-first (GBlock-preds b)))
          (if (= p ENTRY) entry-vuse (loop p (add1 depth)))]
         [else #f])])))

;; (values cfg locals st): inline every call to a callee in PLAN (hash
;; callee-uid -> (vector decl cfg locals)) within FNDECL's CFG
(define (inline-calls st0 fndecl cfg0 locals0 plan)
  (define st (gs-set-fndecl st0 fndecl))
  (define new-vars '())                        ; declarations to put into SSA form afterwards, in creation order
  (define locals locals0)
  (define cfg cfg0)
  (define stamp (cfg-next-stamp cfg0))
  (define (fresh-stamp!) (begin0 stamp (set! stamp (add1 stamp))))
  (define (stamped g) (stmt-set-flag g 'stamp (fresh-stamp!)))
  (define inlined? #f)

  (define (expand! bb k call)
    (define callee (gcall-fndecl call))
    (define rec (hash-ref plan (decl-uid callee)))
    (define ccfg (vector-ref rec 1))
    (define clocals (vector-ref rec 2))
    (define cdefault-defs (decl-flag st callee 'default-defs '()))
    (set! inlined? #t)
    ;; split before the call: RB starts with the call
    (define-values (rb c1) (split-block cfg bb k))
    (set! cfg (let-values ([(c) (remove-edge c1 bb rb)]) c))
    ;; the memory operands of the call
    (define call-vuse (stmt-vuse call))
    (define call-vdef (stmt-vdef call))
    ;; ---- declaration / SSA name maps
    (define decl-map (make-hasheqv))           ; callee decl uid -> caller tree
    (define name-map (make-hasheqv))           ; callee ssa version -> caller tree
    (define (callee-vop-var) (for/first ([p (in-list cdefault-defs)] #:when (ssa-virtual? (cdr p))) (ssa-name-var (cdr p))))
    (define caller-vop-var (and (or call-vuse call-vdef) (ssa-name-var (or call-vuse call-vdef))))
    ;; parameters: setup_one_parameter, SSA register case
    (define params (pvector->list (decl-arguments callee)))
    (for ([p (in-list params)] [i (in-naturals)])
      (define arg (and (< i (pvector-length (gcall-args call))) (pvector-ref (gcall-args call) i)))
      (define dd (let ([x (assv (decl-uid p) cdefault-defs)]) (and x (cdr x))))
      (cond
        [(and dd arg (is-gimple-reg st p) (or (ssa-name? arg) (is-gimple-min-invariant st arg)))
         (hash-set! name-map (ssa-name-version dd) arg)]
        [else
         ;; a variable copy initialized from the argument
         (define-values (uid st1) (fresh-uid st))
         (set! st st1)
         (define var (make-tree 'var_decl #:type (tree-type p)
                                #:attrs (hash-set* (Tree-attrs p) 'uid uid 'context (cons 'function_decl (decl-uid fndecl)) 'used #t)))
         (hash-set! decl-map (decl-uid p) var)
         (set! locals (pvector-cons-right locals var))
         (set! new-vars (append new-vars (list var)))
         (when arg
           (define init (stamped (gassign (gcall-loc call) (hasheq) var (tree-code arg) arg #f #f)))
           (set! cfg (set-block cfg (block-set-stmts (cfg-block cfg bb) (pvector-cons-right (GBlock-stmts (cfg-block cfg bb)) init)))))
         (when dd (hash-set! name-map (ssa-name-version dd) var))]))
    ;; return variable
    (define lhs (gcall-lhs call))
    (define ret-type (type-type st (tree-type callee)))
    (define retvar
      (cond
        [(or (not lhs) (void-type-p st ret-type)) #f]
        [else
         (define-values (uid st1) (fresh-uid st))
         (set! st st1)
         (define v (make-tree 'var_decl #:type ret-type
                              #:attrs (hasheq 'uid uid 'name #f 'artificial #t 'ignored #t 'used #t
                                              'context (cons 'function_decl (decl-uid fndecl)))))
         (set! locals (pvector-cons-right locals v))
         (set! new-vars (append new-vars (list v)))
         v]))
    ;; callee locals (add_local_variables: FOR_EACH_LOCAL_DECL is reversed)
    (for ([d (in-pvector (pvector-reverse clocals))])
      (unless (tree-static d)
        (define-values (uid st1) (fresh-uid st))
        (set! st st1)
        (define copy (make-tree 'var_decl #:type (tree-type d)
                                #:attrs (hash-set* (Tree-attrs d) 'uid uid 'context (cons 'function_decl (decl-uid fndecl)))))
        (hash-set! decl-map (decl-uid d) copy)
        (set! locals (pvector-cons-right locals copy))))
    ;; ---- block map
    (define cbbs (pvector->list (cfg-bbs ccfg)))
    (define bb-map (make-hasheqv))
    (hash-set! bb-map ENTRY bb)
    (hash-set! bb-map EXIT rb)
    (define num (prof-count cfg bb))
    (define den (prof-count ccfg ENTRY))
    (define rbs (box '()))                     ; callee return blocks (preds of EXIT)
    (for/fold ([after bb]) ([cb (in-list cbbs)])
      (define-values (nb c) (create-bb cfg (pvector-empty) after))
      (set! cfg c)
      (hash-set! bb-map cb nb)
      (define cnt (scale-count (prof-count ccfg cb) num den))
      (when (and cnt (GCfg-profile cfg))
        (define pf (GCfg-profile cfg))
        (set! cfg (cfg-set-profile cfg (GProf (intmap-set (GProf-counts pf) nb cnt) (GProf-probs pf)))))
      nb)
    ;; ---- tree remapping
    (define (remap-decl d)
      (define uid (decl-uid d))
      (cond
        [(hash-ref decl-map uid #f) => values]
        [(eq? (tree-code d) 'label_decl)
         (define-values (nuid st1) (fresh-uid st))
         (set! st st1)
         (define l (make-tree 'label_decl #:type (tree-type d)
                              #:attrs (hash-set* (hash-remove (Tree-attrs d) 'label-uid) 'uid nuid 'context (cons 'function_decl (decl-uid fndecl)))))
         (hash-set! decl-map uid l)
         l]
        [else d]))
    (define (remap t)
      (cond
        [(not t) #f]
        [(eq? (tree-code t) 'ssa_name)
         (define v (ssa-name-version t))
         (cond
           [(hash-ref name-map v #f) => values]
           [(ssa-virtual? t)
            (cond
              [(tree-attr t 'default-def #f) (or call-vuse t)]
              [else
               (define-values (n st1) (make-ssa-name st (or caller-vop-var (ssa-name-var t))))
               (set! st st1)
               (hash-set! name-map v n)
               n])]
           [else
            (define var (ssa-name-var t))
            (define-values (n st1)
              (cond
                [(and var (not (and (var-p var) (decl-flag st var 'artificial #f) (decl-flag st var 'ignored #f) (not (decl-name var)))))
                 (make-ssa-name st (remap-decl var))]
                [else (make-ssa-name st (tree-type t) #:name (tree-attr t 'identifier #f))]))
            (set! st st1)
            (hash-set! name-map v n)
            n])]
        [(decl-p st t) (remap-decl t)]
        [(pvector-empty? (tree-ops t)) t]
        [else (tree-set-ops t (for/pvector ([o (in-pvector (tree-ops t))]) (remap o)))]))
    (define (remap-stmt g)
      (cond
        [(greturn? g)
         (define rv (greturn-retval g))
         (cond
           [(and retvar rv (not (eq? (tree-code rv) 'result_decl))
                 (not (and (ssa-name? rv) (ssa-name-var rv) (eq? (tree-code (ssa-name-var rv)) 'result_decl))))
            (define r (remap rv))
            (stamped (gassign (greturn-loc g) (hasheq) retvar (tree-code r) r #f #f))]
           [else #f])]
        [(glabel? g) (stamped (glabel (glabel-loc g) (hasheq) (remap-decl (glabel-label g))))]
        [else
         (define g1 (map-stmt-trees g remap))
         (define g2 (cond
                      [(gassign? g1)
                       ;; a GIMPLE_SINGLE_RHS keeps gimple_assign_rhs_code = TREE_CODE (rhs1)
                       (if (eq? (gimple-rhs-class st (gassign-code g1)) 'single)
                           (gassign (gassign-loc g1) (gassign-flags g1) (gassign-lhs g1) (tree-code (gassign-rhs1 g1)) (gassign-rhs1 g1) #f #f)
                           g1)]
                      [else g1]))
         (stamped g2)]))
    ;; ---- copy blocks: statements and PHIs
    (for ([cb (in-list cbbs)])
      (define nb (hash-ref bb-map cb))
      (define b (cfg-block ccfg cb))
      (define phis
        (for/pvector ([ph (in-pvector (GBlock-phis b))])
          (define res (remap (gphi-result ph)))
          (define args (for/pvector ([a (in-pvector (gphi-args ph))]) (cons (hash-ref bb-map (car a)) (remap (cdr a)))))
          (define-values (g* n) (restamp #f (gphi (gphi-loc ph) (hasheq) (gphi-var ph) res args) stamp))
          (set! stamp n)
          g*))
      ;; remap-stmt allocates names: evaluate once per statement
      (define stmts* (for/fold ([acc (pvector-empty)]) ([g (in-pvector (GBlock-stmts b))])
                       (define g* (remap-stmt g))
                       (if g* (pvector-cons-right acc g*) acc)))
      (set! cfg (set-block cfg (block-set-stmts (block-set-phis (cfg-block cfg nb) phis) stmts*)))
      (for ([g (in-pvector stmts*)] #:when (glabel? g))
        (define-values (st1 c1) (assign-label-uid st cfg (glabel-label g)))
        (set! st st1)
        (set! cfg (register-label c1 (glabel-label g) nb))))
    ;; ---- edges
    (define (copy-edges src)
      (for ([e (in-pvector (cfg-succs ccfg src))])
        (define d (GEdge-dest e))
        (define nsrc (hash-ref bb-map src))
        (define ndest (hash-ref bb-map d))
        (define flags0 (GEdge-flags e))
        (define flags (if (and (= d EXIT) (zero? (bitwise-and flags0 (bitwise-ior EDGE-TRUE-VALUE EDGE-FALSE-VALUE))))
                          (bitwise-ior flags0 EDGE-FALLTHRU) flags0))
        (define-values (_new c) (make-edge cfg nsrc ndest flags))
        (set! cfg c)
        (define pr (if (= src ENTRY) 134217728 (prof-prob ccfg src d)))
        (when (and pr (GCfg-profile cfg))
          (define pf (GCfg-profile cfg))
          (set! cfg (cfg-set-profile cfg (GProf (GProf-counts pf) (hash-set (GProf-probs pf) (cons nsrc ndest) pr)))))))
    (copy-edges ENTRY)
    (for ([cb (in-list cbbs)]) (copy-edges cb))
    ;; ---- the call becomes lhs = retvar (or disappears)
    (define rb-block (cfg-block cfg rb))
    (define rest (GBlock-stmts rb-block))      ; the call is first
    (define replacement
      (and lhs retvar (stamped (gassign (gcall-loc call) (hasheq) lhs 'var_decl retvar #f #f))))
    (set! cfg (set-block cfg (block-set-stmts rb-block
                                              (let ([tail (for/pvector ([g (in-pvector rest)] [i (in-naturals)] #:when (> i 0)) g)])
                                                (if replacement (pvector-cons-left tail replacement) tail)))))
    ;; the call's VDEF users take the memory state at the callee's exit
    (when call-vdef
      (define exit-preds (cfg-preds ccfg EXIT))
      (define state
        (cond
          [(= 1 (pvector-length exit-preds))
           (define s (exit-memory-state ccfg (pvector-first exit-preds) #f))
           (cond [(not s) call-vuse] [else (remap s)])]
          [else call-vuse]))
      (when state
        (define (f t) (if (and t (ssa-name? t) (ssa-name=? t call-vdef)) state t))
        (set! cfg
              (for/fold ([c cfg]) ([i (in-pvector (cfg-bbs cfg))])
                (define b (cfg-block c i))
                (set-block c (block-set-stmts
                              (block-set-phis b (for/pvector ([ph (in-pvector (GBlock-phis b))])
                                                  (gphi (gphi-loc ph) (gphi-flags ph) (gphi-var ph) (gphi-result ph)
                                                        (for/pvector ([a (in-pvector (gphi-args ph))]) (cons (car a) (f (cdr a)))))))
                              (for/pvector ([g (in-pvector (GBlock-stmts b))])
                                (let* ([g (if (stmt-vuse g) (stmt-set-flag g 'vuse (f (stmt-vuse g))) g)]) g)))))))))

  ;; optimize_inline_calls: blocks forward (new blocks follow the split block), statements backward
  (let walk ([bb (bb-next cfg ENTRY)])
    (when (and bb (not (= bb EXIT)))
      (let stmts-loop ([k (sub1 (pvector-length (GBlock-stmts (cfg-block cfg bb))))])
        (when (>= k 0)
          (define g (pvector-ref (GBlock-stmts (cfg-block cfg bb)) k))
          (when (and (gcall? g) (gcall-fndecl g) (hash-ref plan (decl-uid (gcall-fndecl g)) #f))
            (expand! bb k g))
          (stmts-loop (sub1 k))))
      (walk (bb-next cfg bb))))
  (cond
    [(not inlined?) (values cfg0 locals0 st0)]
    [else
     ;; TODO_cleanup_cfg | TODO_update_ssa
     (define-values (cfg1 st1) (cleanup-tree-cfg st cfg print-cond-rhs))
     (define-values (cfg2 st2) (update-ssa-vars st1 fndecl cfg1 new-vars))
     (values cfg2 locals st2)]))
