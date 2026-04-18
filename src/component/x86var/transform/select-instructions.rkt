#lang racket/base

;; ============================================================
;; Component: X86-var Select Instructions
;; ============================================================
;;
;; Convert C-var to x86-var instructions.
;; Chooses appropriate x86 instructions for each operation.
;;
;; Input:  CProgram with info containing 'counter
;; Output: X86Program (with pseudo-registers)
;;
;; Design: Functional style - counter is threaded through for
;; generating fresh temp variables. No global mutable state.
;;
;; ============================================================

(require racket/match
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/cvar/types.rkt"
         "../../../kernel/ir/x86/x86var.rkt")

(provide select-instructions)

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

;; Generate fresh variable ID
(define (fresh-var-id counter)
  (values counter (add1 counter)))

;; ============================================================
;; Main Pass
;; ============================================================

(define (select-instructions prog)
  (match prog
    [(CProgram info blocks)
     (define counter (info-counter info))
     (define-values (x86-blocks new-counter)
       (for/fold ([result (hash)] [c counter])
                 ([(label block) (in-hash blocks)])
         (define-values (new-block c*) (select-block c block))
         (values (hash-set result label new-block) c*)))
     (X86Program (info-set-counter info new-counter) x86-blocks)]))

;; select-block: counter block -> (values x86-block new-counter)
(define (select-block counter block)
  (match block
    [(CBlock info tail)
     (define-values (instrs new-counter) (select-tail counter tail))
     (values (X86Block info instrs) new-counter)]))

;; ============================================================
;; Select Tail
;; ============================================================

;; select-tail: counter tail -> (values instrs new-counter)
(define (select-tail counter tail)
  (match tail
    ;; Return
    [(CReturn exp)
     (values (append (select-exp (Reg 'rax) exp)
                     (list (Jmp 'conclusion)))
             counter)]

    ;; Sequence
    [(CSeq stmt tail)
     (define-values (tail-instrs new-counter) (select-tail counter tail))
     (values (append (select-stmt stmt) tail-instrs)
             new-counter)]

    ;; Goto
    [(CGoto label)
     (values (list (Jmp label)) counter)]

    ;; Conditional
    [(CIf cond then-label else-label)
     (select-pred counter cond then-label else-label)]

    [_ (error 'select-tail "unhandled tail: ~a" tail)]))

;; ============================================================
;; Select Statement
;; ============================================================

(define (select-stmt stmt)
  (match stmt
    [(CAssign (CVar name) exp)
     (select-exp (Var name) exp)]))

;; ============================================================
;; Select Expression (assignment target)
;; ============================================================

(define (select-exp dest exp)
  (match exp
    ;; Atomic expressions
    [(CInt n)
     (list (Instr 'movq (list (Imm n) dest)))]
    [(CBool #t)
     (list (Instr 'movq (list (Imm 1) dest)))]
    [(CBool #f)
     (list (Instr 'movq (list (Imm 0) dest)))]
    [(CVar name)
     (if (equal? dest (Var name))
         '()
         (list (Instr 'movq (list (Var name) dest))))]

    ;; Primitive operations
    [(CPrim 'read '())
     (list (Callq 'read_int 0)
           (Instr 'movq (list (Reg 'rax) dest)))]

    [(CPrim '- (list arg))
     (define src (select-atom arg))
     (list (Instr 'movq (list src dest))
           (Instr 'negq (list dest)))]

    [(CPrim '+ (list arg1 arg2))
     (define src1 (select-atom arg1))
     (define src2 (select-atom arg2))
     (list (Instr 'movq (list src1 dest))
           (Instr 'addq (list src2 dest)))]

    [(CPrim '- (list arg1 arg2))
     (define src1 (select-atom arg1))
     (define src2 (select-atom arg2))
     (list (Instr 'movq (list src1 dest))
           (Instr 'subq (list src2 dest)))]

    [(CPrim 'not (list arg))
     (define src (select-atom arg))
     (list (Instr 'movq (list src dest))
           (Instr 'xorq (list (Imm 1) dest)))]

    [(CPrim (and op (or 'eq? '< '<= '> '>=)) (list arg1 arg2))
     (define src1 (select-atom arg1))
     (define src2 (select-atom arg2))
     (define cc (prim->cc op))
     (list (Instr 'cmpq (list src2 src1))
           (Instr 'set (list cc (ByteReg 'al)))
           (Instr 'movzbq (list (ByteReg 'al) dest)))]

    [_ (error 'select-exp "unhandled expression: ~a" exp)]))

;; ============================================================
;; Select Predicate (for conditional jumps)
;; ============================================================

;; select-pred: counter cond then-label else-label -> (values instrs new-counter)
(define (select-pred counter cond then-label else-label)
  (match cond
    [(CPrim (and op (or 'eq? '< '<= '> '>=)) (list arg1 arg2))
     (define src1 (select-atom arg1))
     (define src2 (select-atom arg2))
     (define cc (prim->cc op))
     (values (list (Instr 'cmpq (list src2 src1))
                   (JmpIf cc then-label)
                   (Jmp else-label))
             counter)]

    [(CPrim 'not (list arg))
     ;; Flip branches
     (select-pred counter arg else-label then-label)]

    [_
     ;; General case: evaluate to temp and compare
     (define-values (tmp-id new-counter) (fresh-var-id counter))
     (define temp (Var tmp-id))
     (values (append (select-exp temp cond)
                     (list (Instr 'cmpq (list (Imm 0) temp))
                           (JmpIf 'ne then-label)
                           (Jmp else-label)))
             new-counter)]))

;; ============================================================
;; Helpers
;; ============================================================

(define (select-atom atom)
  (match atom
    [(CInt n) (Imm n)]
    [(CBool #t) (Imm 1)]
    [(CBool #f) (Imm 0)]
    [(CVar name) (Var name)]))

(define (prim->cc op)
  (case op
    [(eq?) 'e]
    [(<) 'l]
    [(<=) 'le]
    [(>) 'g]
    [(>=) 'ge]
    [else (error 'prim->cc "unknown comparison: ~a" op)]))
