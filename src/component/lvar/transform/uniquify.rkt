#lang racket/base

;; ============================================================
;; Component: L-var Uniquify Transform
;; ============================================================
;;
;; Make all variable names unique by using integer IDs.
;; This simplifies later passes by ensuring no shadowing.
;;
;; Input:  AST (Lvar) with Program info containing 'counter key
;; Output: AST with unique variable IDs (Var instead of Var:named)
;;
;; Design: Functional style - counter is threaded through all
;; recursive calls as state. No global mutable state.
;;
;; ============================================================

(require racket/match
         "../../../kernel/ir/ast/types.rkt"
         "../../../kernel/data/data.rkt")

(provide uniquify)

;; ============================================================
;; State Management
;; ============================================================

;; Ensure info is an immutable hash
(define (ensure-info-hash info)
  (cond
    [(hash? info) info]
    [(null? info) (hash)]
    [(list? info)
     ;; Convert association list to hash
     (for/fold ([m (hash)])
               ([pair info])
       (hash-set m (car pair) (cdr pair)))]
    [else (error 'ensure-info-hash "invalid info: ~a" info)]))

;; Get counter from info (default 0)
(define (info-counter info)
  (if (hash? info)
      (hash-ref info 'counter 0)
      0))

;; Set counter in info
(define (info-set-counter info counter)
  (define om (ensure-info-hash info))
  (hash-set om 'counter counter))

;; Generate fresh variable ID, returns (values new-var new-counter)
(define (fresh-var counter)
  (values (Var counter) (add1 counter)))

;; Environment: hash from original name to Var
(define (empty-env)
  (hash))

(define (extend-env env name var)
  (hash-set env name var))

(define (lookup-env env name)
  (define result (hash-ref env name #f))
  (unless result
    (error 'uniquify "unbound variable: ~a" name))
  result)

;; ============================================================
;; Main Pass
;; ============================================================

(define (uniquify prog)
  (match prog
    [(Program info body)
     (define counter (info-counter info))
     (define-values (new-body new-counter)
       (uniquify-exp (empty-env) counter body))
     (Program (info-set-counter info new-counter) new-body)]))

;; uniquify-exp: env counter exp -> (values new-exp new-counter)
(define (uniquify-exp env counter exp)
  (match exp
    ;; Literals - unchanged
    [(Int n) (values (Int n) counter)]
    [(Bool b) (values (Bool b) counter)]
    [(Void) (values (Void) counter)]

    ;; Variable reference
    [(Var:named name)
     (values (lookup-env env name) counter)]

    ;; Let binding
    [(Let (Var:named name) rhs body)
     (define-values (new-var counter1) (fresh-var counter))
     (define new-env (extend-env env name new-var))
     (define-values (new-rhs counter2) (uniquify-exp env counter1 rhs))
     (define-values (new-body counter3) (uniquify-exp new-env counter2 body))
     (values (Let new-var new-rhs new-body) counter3)]

    ;; Conditional
    [(If cond then else)
     (define-values (new-cond counter1) (uniquify-exp env counter cond))
     (define-values (new-then counter2) (uniquify-exp env counter1 then))
     (define-values (new-else counter3) (uniquify-exp env counter2 else))
     (values (If new-cond new-then new-else) counter3)]

    ;; Sequence
    [(Begin exprs body)
     (define-values (new-exprs counter1)
       (uniquify-exp-list env counter exprs))
     (define-values (new-body counter2) (uniquify-exp env counter1 body))
     (values (Begin new-exprs new-body) counter2)]

    ;; Mutation
    [(SetBang (Var:named name) rhs)
     (define-values (new-rhs counter1) (uniquify-exp env counter rhs))
     (values (SetBang (lookup-env env name) new-rhs) counter1)]
    [(GetBang (Var:named name))
     (values (GetBang (lookup-env env name)) counter)]

    ;; While loop
    [(WhileLoop cond body)
     (define-values (new-cond counter1) (uniquify-exp env counter cond))
     (define-values (new-body counter2) (uniquify-exp env counter1 body))
     (values (WhileLoop new-cond new-body) counter2)]

    ;; Primitive operations
    [(Prim op args)
     (define-values (new-args counter1) (uniquify-exp-list env counter args))
     (values (Prim op new-args) counter1)]

    ;; Type annotations
    [(HasType exp type)
     (define-values (new-exp counter1) (uniquify-exp env counter exp))
     (values (HasType new-exp type) counter1)]

    [_ (error 'uniquify-exp "unhandled expression: ~a" exp)]))

;; uniquify-exp-list: env counter list -> (values new-list new-counter)
(define (uniquify-exp-list env counter exprs)
  (define-values (rev-acc new-counter)
    (for/fold ([acc '()] [c counter])
              ([e exprs])
      (define-values (new-e new-c) (uniquify-exp env c e))
      (values (cons new-e acc) new-c)))
  (values (reverse rev-acc) new-counter))
