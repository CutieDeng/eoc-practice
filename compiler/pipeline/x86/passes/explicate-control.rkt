#lang racket/base

;; ============================================================
;; Pass: Explicate Control
;; ============================================================
;;
;; Convert high-level AST to C-var with explicit control flow.
;; Creates basic blocks and explicit jumps.
;;
;; Input:  AST in A-normal form
;; Output: CProgram with explicit control flow
;;
;; ============================================================

(require racket/match
         "../../../kernel/ir/ast/types.rkt"
         "../ir/cvar.rkt")

(provide explicate-control)

;; Mutable state for collecting blocks
(define blocks (make-hash))
(define block-counter 0)

(define (fresh-label base)
  (set! block-counter (add1 block-counter))
  (string->symbol (format "~a.~a" base block-counter)))

(define (reset-blocks!)
  (set! blocks (make-hash))
  (set! block-counter 0))

(define (add-block! label tail)
  (hash-set! blocks label (CBlock '() tail)))

;; ============================================================
;; Main Pass
;; ============================================================

(define (explicate-control prog)
  (reset-blocks!)
  (match prog
    [(Program info body)
     (define start-tail (explicate-tail body))
     (add-block! 'start start-tail)
     (CProgram info (hash-copy blocks))]))

;; Convert AST expression to atomic C-var expression
(define (ast->catom exp)
  (match exp
    [(Int n) (CInt n)]
    [(Bool b) (CBool b)]
    [(Var:named name) (CVar name)]
    [_ (error 'ast->catom "not atomic: ~a" exp)]))

;; ============================================================
;; Explicate Tail (expression in tail position)
;; ============================================================

(define (explicate-tail exp)
  (match exp
    ;; Atomic expressions - return directly
    [(Int n) (CReturn (CInt n))]
    [(Bool b) (CReturn (CBool b))]
    [(Void) (CReturn (CBool #f))]  ; Void -> #f
    [(Var:named name) (CReturn (CVar name))]

    ;; Primitive operations
    [(Prim op args)
     (CReturn (CPrim op (map ast->catom args)))]

    ;; Let binding
    [(Let (Var:named name) rhs body)
     (explicate-assign name rhs (explicate-tail body))]

    ;; Conditional
    [(If cond then else)
     (define then-label (fresh-label 'then))
     (define else-label (fresh-label 'else))
     (add-block! then-label (explicate-tail then))
     (add-block! else-label (explicate-tail else))
     (explicate-pred cond then-label else-label)]

    ;; Sequence
    [(Begin exprs body)
     (define tail (explicate-tail body))
     (foldr (lambda (e t) (explicate-effect e t)) tail exprs)]

    ;; Type annotation
    [(HasType exp _)
     (explicate-tail exp)]

    [_ (error 'explicate-tail "unhandled expression: ~a" exp)]))

;; ============================================================
;; Explicate Assign (assignment context)
;; ============================================================

(define (explicate-assign name rhs cont)
  (match rhs
    ;; Atomic expressions
    [(Int n)
     (CSeq (CAssign (CVar name) (CInt n)) cont)]
    [(Bool b)
     (CSeq (CAssign (CVar name) (CBool b)) cont)]
    [(Void)
     (CSeq (CAssign (CVar name) (CBool #f)) cont)]
    [(Var:named var)
     (CSeq (CAssign (CVar name) (CVar var)) cont)]

    ;; Primitive operations
    [(Prim op args)
     (CSeq (CAssign (CVar name) (CPrim op (map ast->catom args))) cont)]

    ;; Let binding
    [(Let (Var:named inner-name) inner-rhs inner-body)
     (explicate-assign inner-name inner-rhs
                       (explicate-assign name inner-body cont))]

    ;; Conditional - create continuation block
    [(If cond then else)
     (define cont-label (fresh-label 'cont))
     (add-block! cont-label cont)
     (define then-label (fresh-label 'then))
     (define else-label (fresh-label 'else))
     (add-block! then-label
                 (explicate-assign name then (CGoto cont-label)))
     (add-block! else-label
                 (explicate-assign name else (CGoto cont-label)))
     (explicate-pred cond then-label else-label)]

    ;; Sequence
    [(Begin exprs body)
     (define assign-tail (explicate-assign name body cont))
     (foldr (lambda (e t) (explicate-effect e t)) assign-tail exprs)]

    ;; Type annotation
    [(HasType exp _)
     (explicate-assign name exp cont)]

    [_ (error 'explicate-assign "unhandled expression: ~a" rhs)]))

;; ============================================================
;; Explicate Effect (expression for side effects only)
;; ============================================================

(define (explicate-effect exp cont)
  (match exp
    ;; Atomic - no effect, continue
    [(Int _) cont]
    [(Bool _) cont]
    [(Void) cont]
    [(Var:named _) cont]

    ;; Primitive with side effects
    [(Prim 'read '())
     ;; Discard result
     (define tmp (fresh-label 'tmp))
     (CSeq (CAssign (CVar tmp) (CPrim 'read '())) cont)]
    [(Prim _ _) cont]  ; Other prims have no side effects

    ;; Let binding
    [(Let (Var:named name) rhs body)
     (explicate-assign name rhs (explicate-effect body cont))]

    ;; Conditional
    [(If cond then else)
     (define cont-label (fresh-label 'cont))
     (add-block! cont-label cont)
     (define then-label (fresh-label 'then))
     (define else-label (fresh-label 'else))
     (add-block! then-label (explicate-effect then (CGoto cont-label)))
     (add-block! else-label (explicate-effect else (CGoto cont-label)))
     (explicate-pred cond then-label else-label)]

    ;; Sequence
    [(Begin exprs body)
     (define effect-tail (explicate-effect body cont))
     (foldr (lambda (e t) (explicate-effect e t)) effect-tail exprs)]

    ;; Mutation
    [(SetBang (Var:named name) rhs)
     (explicate-assign name rhs cont)]

    ;; Type annotation
    [(HasType exp _)
     (explicate-effect exp cont)]

    [_ (error 'explicate-effect "unhandled expression: ~a" exp)]))

;; ============================================================
;; Explicate Predicate (conditional context)
;; ============================================================

(define (explicate-pred exp then-label else-label)
  (match exp
    ;; Boolean constant
    [(Bool #t) (CGoto then-label)]
    [(Bool #f) (CGoto else-label)]

    ;; Variable - compare to #f
    [(Var:named name)
     (CIf (CPrim 'eq? (list (CVar name) (CBool #f)))
          else-label then-label)]

    ;; Comparison primitives
    [(Prim (and op (or 'eq? '< '<= '> '>= 'not)) args)
     (CIf (CPrim op (map ast->catom args)) then-label else-label)]

    ;; not
    [(Prim 'not (list arg))
     (explicate-pred arg else-label then-label)]

    ;; Let binding
    [(Let (Var:named name) rhs body)
     (explicate-assign name rhs
                       (explicate-pred body then-label else-label))]

    ;; Nested conditional
    [(If cond then else)
     (define then-pred-label (fresh-label 'then))
     (define else-pred-label (fresh-label 'else))
     (add-block! then-pred-label (explicate-pred then then-label else-label))
     (add-block! else-pred-label (explicate-pred else then-label else-label))
     (explicate-pred cond then-pred-label else-pred-label)]

    ;; Type annotation
    [(HasType exp _)
     (explicate-pred exp then-label else-label)]

    ;; Other expressions - evaluate and compare to #f
    [_
     (define tmp (fresh-label 'pred))
     (explicate-assign tmp exp
                       (CIf (CPrim 'eq? (list (CVar tmp) (CBool #f)))
                            else-label then-label))]))
