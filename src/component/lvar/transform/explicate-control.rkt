#lang racket/base

;; ============================================================
;; Component: L-var Explicate Control Transform
;; ============================================================
;;
;; Convert high-level AST to C-var with explicit control flow.
;; Creates basic blocks and explicit jumps.
;;
;; Input:  AST in A-normal form with Program info containing 'counter
;; Output: CProgram with explicit control flow
;;
;; Design: Functional style - counter and blocks are threaded
;; through all recursive calls as state. No global mutable state.
;;
;; ============================================================

(require racket/match
         "../../../kernel/ir/ast/types.rkt"
         "../../../kernel/data/data.rkt"
         "../../../pipeline/x86/ir/cvar.rkt")

(provide explicate-control)

;; ============================================================
;; State Management
;; ============================================================

;; State: (counter . blocks)
;; counter: integer for generating unique label IDs
;; blocks: ordered-map from label (symbol) to CBlock

(define (make-state counter)
  (cons counter (ordered-map-empty symbol-compare)))

(define (state-counter st) (car st))
(define (state-blocks st) (cdr st))

(define (state-set-counter st counter)
  (cons counter (state-blocks st)))

(define (state-set-blocks st blocks)
  (cons (state-counter st) blocks))

;; Ensure info is an ordered-map
(define (ensure-ordered-map-info info)
  (cond
    [(ordered-map? info) info]
    [(null? info) (ordered-map-empty symbol-compare)]
    [(list? info)
     (for/fold ([m (ordered-map-empty symbol-compare)])
               ([pair info])
       (ordered-map-set m (car pair) (cdr pair)))]
    [else (error 'ensure-ordered-map-info "invalid info: ~a" info)]))

;; Get counter from info (default 0)
(define (info-counter info)
  (if (ordered-map? info)
      (ordered-map-ref info 'counter 0)
      0))

;; Set counter in info
(define (info-set-counter info counter)
  (define om (ensure-ordered-map-info info))
  (ordered-map-set om 'counter counter))

;; Generate fresh label, returns (values label new-state)
(define (fresh-label st base)
  (define counter (state-counter st))
  (define label (string->symbol (format "~a.~a" base counter)))
  (values label (state-set-counter st (add1 counter))))

;; Generate fresh variable ID, returns (values var-id new-state)
(define (fresh-var-id st)
  (define counter (state-counter st))
  (values counter (state-set-counter st (add1 counter))))

;; Add block to state, returns new-state
(define (add-block st label tail)
  (state-set-blocks st
    (ordered-map-set (state-blocks st) label (CBlock '() tail))))

;; ============================================================
;; Main Pass
;; ============================================================

(define (explicate-control prog)
  (match prog
    [(Program info body)
     (define counter (info-counter info))
     (define init-state (make-state counter))
     (define-values (start-tail final-state) (explicate-tail init-state body))
     (define final-state* (add-block final-state 'start start-tail))
     ;; Convert ordered-map blocks to hash for compatibility with later passes
     (define blocks-hash
       (for/hash ([kv (in-ordered-map (state-blocks final-state*))])
         (values (car kv) (cdr kv))))
     (CProgram (info-set-counter info (state-counter final-state*))
               blocks-hash)]))

;; Convert AST expression to atomic C-var expression
(define (ast->catom exp)
  (match exp
    [(Int n) (CInt n)]
    [(Bool b) (CBool b)]
    [(Var id) (CVar id)]
    [_ (error 'ast->catom "not atomic: ~a" exp)]))

;; ============================================================
;; Explicate Tail (expression in tail position)
;; ============================================================

;; explicate-tail: state exp -> (values tail new-state)
(define (explicate-tail st exp)
  (match exp
    ;; Atomic expressions - return directly
    [(Int n) (values (CReturn (CInt n)) st)]
    [(Bool b) (values (CReturn (CBool b)) st)]
    [(Void) (values (CReturn (CBool #f)) st)]  ; Void -> #f
    [(Var id) (values (CReturn (CVar id)) st)]

    ;; Primitive operations
    [(Prim op args)
     (values (CReturn (CPrim op (map ast->catom args))) st)]

    ;; Let binding
    [(Let (Var id) rhs body)
     (define-values (body-tail st1) (explicate-tail st body))
     (explicate-assign st1 id rhs body-tail)]

    ;; Conditional
    [(If cond then else)
     (define-values (then-label st1) (fresh-label st 'then))
     (define-values (else-label st2) (fresh-label st1 'else))
     (define-values (then-tail st3) (explicate-tail st2 then))
     (define-values (else-tail st4) (explicate-tail st3 else))
     (define st5 (add-block st4 then-label then-tail))
     (define st6 (add-block st5 else-label else-tail))
     (explicate-pred st6 cond then-label else-label)]

    ;; Sequence
    [(Begin exprs body)
     (define-values (body-tail st1) (explicate-tail st body))
     (explicate-effect-list st1 exprs body-tail)]

    ;; Type annotation
    [(HasType exp _)
     (explicate-tail st exp)]

    [_ (error 'explicate-tail "unhandled expression: ~a" exp)]))

;; ============================================================
;; Explicate Assign (assignment context)
;; ============================================================

;; explicate-assign: state var-id rhs cont -> (values tail new-state)
(define (explicate-assign st name rhs cont)
  (match rhs
    ;; Atomic expressions
    [(Int n)
     (values (CSeq (CAssign (CVar name) (CInt n)) cont) st)]
    [(Bool b)
     (values (CSeq (CAssign (CVar name) (CBool b)) cont) st)]
    [(Void)
     (values (CSeq (CAssign (CVar name) (CBool #f)) cont) st)]
    [(Var id)
     (values (CSeq (CAssign (CVar name) (CVar id)) cont) st)]

    ;; Primitive operations
    [(Prim op args)
     (values (CSeq (CAssign (CVar name) (CPrim op (map ast->catom args))) cont) st)]

    ;; Let binding
    [(Let (Var inner-id) inner-rhs inner-body)
     (define-values (inner-cont st1) (explicate-assign st name inner-body cont))
     (explicate-assign st1 inner-id inner-rhs inner-cont)]

    ;; Conditional - create continuation block
    [(If cond then else)
     (define-values (cont-label st1) (fresh-label st 'cont))
     (define st2 (add-block st1 cont-label cont))
     (define-values (then-label st3) (fresh-label st2 'then))
     (define-values (else-label st4) (fresh-label st3 'else))
     (define-values (then-tail st5) (explicate-assign st4 name then (CGoto cont-label)))
     (define-values (else-tail st6) (explicate-assign st5 name else (CGoto cont-label)))
     (define st7 (add-block st6 then-label then-tail))
     (define st8 (add-block st7 else-label else-tail))
     (explicate-pred st8 cond then-label else-label)]

    ;; Sequence
    [(Begin exprs body)
     (define-values (assign-tail st1) (explicate-assign st name body cont))
     (explicate-effect-list st1 exprs assign-tail)]

    ;; Type annotation
    [(HasType exp _)
     (explicate-assign st name exp cont)]

    [_ (error 'explicate-assign "unhandled expression: ~a" rhs)]))

;; ============================================================
;; Explicate Effect (expression for side effects only)
;; ============================================================

;; explicate-effect: state exp cont -> (values tail new-state)
(define (explicate-effect st exp cont)
  (match exp
    ;; Atomic - no effect, continue
    [(Int _) (values cont st)]
    [(Bool _) (values cont st)]
    [(Void) (values cont st)]
    [(Var _) (values cont st)]

    ;; Primitive with side effects
    [(Prim 'read '())
     ;; Discard result - use fresh variable ID
     (define-values (tmp-id st1) (fresh-var-id st))
     (values (CSeq (CAssign (CVar tmp-id) (CPrim 'read '())) cont) st1)]
    [(Prim _ _) (values cont st)]  ; Other prims have no side effects

    ;; Let binding
    [(Let (Var id) rhs body)
     (define-values (body-cont st1) (explicate-effect st body cont))
     (explicate-assign st1 id rhs body-cont)]

    ;; Conditional
    [(If cond then else)
     (define-values (cont-label st1) (fresh-label st 'cont))
     (define st2 (add-block st1 cont-label cont))
     (define-values (then-label st3) (fresh-label st2 'then))
     (define-values (else-label st4) (fresh-label st3 'else))
     (define-values (then-tail st5) (explicate-effect st4 then (CGoto cont-label)))
     (define-values (else-tail st6) (explicate-effect st5 else (CGoto cont-label)))
     (define st7 (add-block st6 then-label then-tail))
     (define st8 (add-block st7 else-label else-tail))
     (explicate-pred st8 cond then-label else-label)]

    ;; Sequence
    [(Begin exprs body)
     (define-values (effect-tail st1) (explicate-effect st body cont))
     (explicate-effect-list st1 exprs effect-tail)]

    ;; Mutation
    [(SetBang (Var id) rhs)
     (explicate-assign st id rhs cont)]

    ;; Type annotation
    [(HasType exp _)
     (explicate-effect st exp cont)]

    [_ (error 'explicate-effect "unhandled expression: ~a" exp)]))

;; explicate-effect-list: state exprs cont -> (values tail new-state)
;; Process effects from right to left (foldr pattern)
(define (explicate-effect-list st exprs cont)
  (foldr (lambda (exp acc)
           (define-values (tail state) acc)
           (explicate-effect state exp tail))
         (values cont st)
         exprs))

;; ============================================================
;; Explicate Predicate (conditional context)
;; ============================================================

;; explicate-pred: state exp then-label else-label -> (values tail new-state)
(define (explicate-pred st exp then-label else-label)
  (match exp
    ;; Boolean constant
    [(Bool #t) (values (CGoto then-label) st)]
    [(Bool #f) (values (CGoto else-label) st)]

    ;; Variable - compare to #f
    [(Var id)
     (values (CIf (CPrim 'eq? (list (CVar id) (CBool #f)))
                  else-label then-label)
             st)]

    ;; Comparison primitives
    [(Prim (and op (or 'eq? '< '<= '> '>= 'not)) args)
     (values (CIf (CPrim op (map ast->catom args)) then-label else-label) st)]

    ;; not
    [(Prim 'not (list arg))
     (explicate-pred st arg else-label then-label)]

    ;; Let binding
    [(Let (Var id) rhs body)
     (define-values (body-pred st1) (explicate-pred st body then-label else-label))
     (explicate-assign st1 id rhs body-pred)]

    ;; Nested conditional
    [(If cond then else)
     (define-values (then-pred-label st1) (fresh-label st 'then))
     (define-values (else-pred-label st2) (fresh-label st1 'else))
     (define-values (then-pred st3) (explicate-pred st2 then then-label else-label))
     (define-values (else-pred st4) (explicate-pred st3 else then-label else-label))
     (define st5 (add-block st4 then-pred-label then-pred))
     (define st6 (add-block st5 else-pred-label else-pred))
     (explicate-pred st6 cond then-pred-label else-pred-label)]

    ;; Type annotation
    [(HasType exp _)
     (explicate-pred st exp then-label else-label)]

    ;; Other expressions - evaluate and compare to #f
    [_
     (define-values (tmp-id st1) (fresh-var-id st))
     (explicate-assign st1 tmp-id exp
                       (CIf (CPrim 'eq? (list (CVar tmp-id) (CBool #f)))
                            else-label then-label))]))
