#lang racket/base

;; ============================================================
;; Pass: Assign Homes
;; ============================================================
;;
;; Assign each variable to a stack location.
;; Simple version without register allocation.
;;
;; Input:  X86Program with pseudo-registers (Var)
;; Output: X86Program with stack locations (Deref)
;;
;; ============================================================

(require racket/match
         racket/set
         "../ir/x86var.rkt")

(provide assign-homes)

;; ============================================================
;; Main Pass
;; ============================================================

(define (assign-homes prog)
  (match prog
    [(X86Program info blocks)
     ;; Collect all variables
     (define all-vars (collect-vars blocks))
     ;; Create variable -> stack offset mapping
     (define var-homes (make-homes all-vars))
     (define stack-size (* 8 (set-count all-vars)))
     ;; Align stack to 16 bytes
     (define aligned-size
       (if (= 0 (modulo stack-size 16))
           stack-size
           (+ stack-size (- 16 (modulo stack-size 16)))))
     ;; Transform blocks
     (define new-blocks
       (for/hash ([(label block) (in-hash blocks)])
         (values label (assign-block var-homes block))))
     (X86Program (cons (cons 'stack-size aligned-size) info)
                 new-blocks)]))

;; ============================================================
;; Collect Variables
;; ============================================================

(define (collect-vars blocks)
  (define vars (mutable-set))
  (for ([(label block) (in-hash blocks)])
    (collect-block-vars block vars))
  vars)

(define (collect-block-vars block vars)
  (match block
    [(X86Block info instrs)
     (for ([instr instrs])
       (collect-instr-vars instr vars))]))

(define (collect-instr-vars instr vars)
  (match instr
    [(Instr op args)
     (for ([arg args])
       (collect-arg-vars arg vars))]
    [(Callq _ _) (void)]
    [(Retq) (void)]
    [(Jmp _) (void)]
    [(JmpIf _ _) (void)]
    [(IndirectCallq arg _)
     (collect-arg-vars arg vars)]))

(define (collect-arg-vars arg vars)
  (match arg
    [(Var name) (set-add! vars name)]
    [_ (void)]))

;; ============================================================
;; Create Homes (Variable -> Stack Offset)
;; ============================================================

(define (make-homes vars)
  (define homes (make-hash))
  (define offset -8)
  (for ([var (in-set vars)])
    (hash-set! homes var offset)
    (set! offset (- offset 8)))
  homes)

;; ============================================================
;; Assign Homes to Block
;; ============================================================

(define (assign-block homes block)
  (match block
    [(X86Block info instrs)
     (X86Block info (map (lambda (i) (assign-instr homes i)) instrs))]))

(define (assign-instr homes instr)
  (match instr
    [(Instr op args)
     (Instr op (map (lambda (a) (assign-arg homes a)) args))]
    [(Callq label arity)
     (Callq label arity)]
    [(Retq)
     (Retq)]
    [(Jmp label)
     (Jmp label)]
    [(JmpIf cc label)
     (JmpIf cc label)]
    [(IndirectCallq arg arity)
     (IndirectCallq (assign-arg homes arg) arity)]))

(define (assign-arg homes arg)
  (match arg
    [(Var name)
     (define offset (hash-ref homes name
                              (lambda () (error 'assign-arg "unknown var: ~a" name))))
     (Deref 'rbp offset)]
    [_ arg]))
