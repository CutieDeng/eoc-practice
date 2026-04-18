#lang racket/base

;; ============================================================
;; Component: L-var Remove Complex Operands (ANF Transformation)
;; ============================================================
;;
;; Ensure all primitive operations only have atomic operands.
;; Complex operands are bound to temporary variables.
;;
;; Input:  AST with unique variable IDs (Var) from uniquify
;; Output: AST in A-normal form
;;
;; Design: Functional style - counter is threaded through all
;; recursive calls as state. No global mutable state.
;; Counter is stored in Program's info under 'counter key.
;;
;; ============================================================

(require racket/match
         "../../../kernel/ir/ast/types.rkt"
         "../../../kernel/data/data.rkt")

(provide remove-complex-opera*)

;; ============================================================
;; State Management
;; ============================================================

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

;; Generate fresh temporary variable, returns (values new-var new-counter)
(define (fresh-temp counter)
  (values (Var counter) (add1 counter)))

;; ============================================================
;; Main Pass
;; ============================================================

(define (remove-complex-opera* prog)
  (match prog
    [(Program info body)
     (define counter (info-counter info))
     (define-values (new-body new-counter) (rco-exp counter body))
     (Program (info-set-counter info new-counter) new-body)]))

;; Is expression atomic?
(define (atomic-exp? e)
  (match e
    [(Int _) #t]
    [(Bool _) #t]
    [(Void) #t]
    [(Var _) #t]
    [_ #f]))

;; rco-atom: ensure expression is atomic
;; Returns (values atom bindings counter)
;; bindings is pvector of (var . rhs) pairs
(define (rco-atom counter exp)
  (if (atomic-exp? exp)
      (values exp (pvector-empty) counter)
      (let ()
        (define-values (new-exp bindings counter1) (rco-exp/bindings counter exp))
        (define-values (tmp counter2) (fresh-temp counter1))
        (values tmp
                (pvector-cons-right bindings (cons tmp new-exp))
                counter2))))

;; rco-exp/bindings: returns (values exp bindings counter)
(define (rco-exp/bindings counter exp)
  (match exp
    ;; Atomic - no bindings needed
    [(Int n) (values (Int n) (pvector-empty) counter)]
    [(Bool b) (values (Bool b) (pvector-empty) counter)]
    [(Void) (values (Void) (pvector-empty) counter)]
    [(Var id) (values (Var id) (pvector-empty) counter)]

    ;; Primitive operations - make arguments atomic
    [(Prim op args)
     (define-values (rev-atoms combined-bindings counter1)
       (for/fold ([atoms '()]
                  [bindings (pvector-empty)]
                  [c counter])
                 ([arg args])
         (define-values (atom bs new-c) (rco-atom c arg))
         (values (cons atom atoms)
                 (pvector-append bindings bs)
                 new-c)))
     (values (Prim op (reverse rev-atoms))
             combined-bindings
             counter1)]

    ;; Let binding
    [(Let var rhs body)
     (define-values (new-rhs rhs-bindings counter1) (rco-exp/bindings counter rhs))
     (define-values (new-body body-bindings counter2) (rco-exp/bindings counter1 body))
     (values (Let var new-rhs new-body)
             (pvector-append rhs-bindings body-bindings)
             counter2)]

    ;; Conditional
    [(If cond then else)
     (define-values (cond-atom cond-bindings counter1) (rco-atom counter cond))
     (define-values (new-then counter2) (rco-exp counter1 then))
     (define-values (new-else counter3) (rco-exp counter2 else))
     (values (If cond-atom new-then new-else) cond-bindings counter3)]

    ;; Sequence
    [(Begin exprs body)
     (define-values (new-exprs counter1) (rco-exp-list counter exprs))
     (define-values (new-body counter2) (rco-exp counter1 body))
     (values (Begin new-exprs new-body) (pvector-empty) counter2)]

    ;; Mutation
    [(SetBang var rhs)
     (define-values (new-rhs bindings counter1) (rco-exp/bindings counter rhs))
     (values (SetBang var new-rhs) bindings counter1)]
    [(GetBang var)
     (values (GetBang var) (pvector-empty) counter)]

    ;; While loop
    [(WhileLoop cond body)
     (define-values (new-cond counter1) (rco-exp counter cond))
     (define-values (new-body counter2) (rco-exp counter1 body))
     (values (WhileLoop new-cond new-body) (pvector-empty) counter2)]

    ;; Type annotations
    [(HasType exp type)
     (define-values (new-exp bindings counter1) (rco-exp/bindings counter exp))
     (values (HasType new-exp type) bindings counter1)]

    [_ (error 'rco-exp/bindings "unhandled expression: ~a" exp)]))

;; Wrap expression with bindings (pvector, outermost first).
;; Iterate right-to-left so bindings[0] ends up as the outermost Let.
(define (wrap-bindings bindings exp)
  (pvector-foldr (lambda (body binding)
                   (Let (car binding) (cdr binding) body))
                 exp
                 bindings))

;; rco-exp: main entry point, returns (values new-exp counter)
(define (rco-exp counter exp)
  (define-values (new-exp bindings new-counter) (rco-exp/bindings counter exp))
  (values (wrap-bindings bindings new-exp) new-counter))

;; rco-exp-list: process list of expressions
(define (rco-exp-list counter exprs)
  (define-values (rev-acc new-counter)
    (for/fold ([acc '()] [c counter])
              ([e exprs])
      (define-values (new-e new-c) (rco-exp c e))
      (values (cons new-e acc) new-c)))
  (values (reverse rev-acc) new-counter))
