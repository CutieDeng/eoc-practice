#lang racket/base

;; ============================================================
;; Component: JVM → pre-SSA CFG (M2)
;; ============================================================
;;
;; Translate the BB-partitioned JVM bytecode of a single method
;; into a kernel `Cfg`.  The result is PRE-SSA: each JVM local
;; slot shares a single VarId across the whole method, so stores
;; may redefine the same output.  Phi placement is M3's job.
;;
;; Input : JvmMethod
;; Output: Cfg   -- one VfInsn per non-terminator JVM insn;
;;                  terminators become Term:* records and
;;                  consume operand-stack values directly.
;;
;; Stack model:
;;   Each block begins with an empty operand stack (height 0)
;;   except declared exception handlers, which begin with one
;;   implicit exception-ref slot at height 1.  Cross-block
;;   stack-height consistency is NOT verified here -- the JVM
;;   verifier guarantees it; M3/SSA will consume the information.
;;
;; Locals:
;;   Each JVM local slot N is modelled as VarId(N).  `load`-style
;;   insns emit a VfInsn with inputs=[VarId(N)] (read) and
;;   outputs=[fresh]; `store`-style insns emit inputs=[value]
;;   outputs=[VarId(N)] -- clearly pre-SSA, intended to be
;;   renamed by a later pass.
;;
;; Unsupported opcodes raise with a self-identifying error; add
;; handlers incrementally as fixtures demand.
;;
;; ============================================================

(require (only-in racket/set set set-member? set-add)
         (only-in cutie-ftree/graph graph-empty graph-add-vertex graph-add-edge)
         "bbs.rkt"
         "../../../kernel/ir/jvm/types.rkt"
         "../../../kernel/ir/cfg/types.rkt"
         (except-in "../../../kernel/data/data.rkt" integer-compare))

(provide jvm-method->cfg
         parse-method-descriptor)

;; ============================================================
;; JVM method descriptor parser
;; ============================================================
;;
;; Returns (values param-count has-return?).
;;
;; Param count here uses ONE stack slot per parameter -- we do
;; not apply the historical "J/D consume two slots" rule.  Fine
;; for the int/ref-heavy fixtures currently in tree.  Revisit
;; when a double/long-bearing test shows up.
(define (parse-method-descriptor desc)
  (define chars (string->list desc))
  (unless (and (pair? chars) (char=? (car chars) #\())
    (error 'parse-method-descriptor "bad descriptor: ~s" desc))
  (let loop ([cs (cdr chars)] [n 0])
    (cond
      [(null? cs) (error 'parse-method-descriptor "unterminated: ~s" desc)]
      [(char=? (car cs) #\)) (values n (not (and (pair? (cdr cs))
                                                 (char=? (cadr cs) #\V))))]
      [else
       (define-values (rest _) (skip-one-type cs))
       (loop rest (add1 n))])))

;; Skip one type descriptor; returns (values rest-chars consumed-chars)
(define (skip-one-type cs)
  (case (car cs)
    [(#\B #\C #\D #\F #\I #\J #\S #\Z #\V) (values (cdr cs) 1)]
    [(#\L)
     (let scan ([rest (cdr cs)] [consumed 1])
       (cond
         [(null? rest) (error 'skip-one-type "unterminated class ref")]
         [(char=? (car rest) #\;) (values (cdr rest) (add1 consumed))]
         [else (scan (cdr rest) (add1 consumed))]))]
    [(#\[)
     (define-values (rest inner) (skip-one-type (cdr cs)))
     (values rest (add1 inner))]
    [else (error 'skip-one-type "unexpected type char: ~a" (car cs))]))

;; ============================================================
;; Main entry
;; ============================================================

(define (jvm-method->cfg method)
  (define mbb (jvm-method->bbs method))
  (define handler-labels
    (for/fold ([s (set)]) ([e (in-pvector (JvmMethodBBs-exceptions mbb))])
      (set-add s (JvmExceptionEntry-handler e))))

  ;; Step 1: allocate a vertex-id per BB
  (define-values (graph0 label->bid)
    (for/fold ([g graph-empty]
               [m (ordered-map-empty string-compare)])
              ([lbl (in-pvector (JvmMethodBBs-order mbb))])
      (define-values (g* vid) (graph-add-vertex g))
      (values g* (ordered-map-set m lbl vid))))

  ;; Step 2: reserve VarIds 0..max-local for JVM local slots,
  ;; then allocate fresh VarIds starting above that range.
  (define max-local (scan-max-local method))
  (define vc0 (add1 max-local))

  ;; Step 3: translate each BB
  (define-values (blocks-map vc-final)
    (for/fold ([bm (ordered-map-empty block-id-compare)] [vc vc0])
              ([lbl (in-pvector (JvmMethodBBs-order mbb))])
      (define jvmbb (ordered-map-ref (JvmMethodBBs-blocks mbb) lbl #f))
      (define bid (ordered-map-ref label->bid lbl #f))
      (define entry-h (if (set-member? handler-labels lbl) 1 0))
      (define-values (cfg-block vc*)
        (translate-block jvmbb bid entry-h vc label->bid))
      (values (ordered-map-set bm bid cfg-block) vc*)))

  ;; Step 4: wire edges
  (define graph-final
    (for/fold ([g graph0]) ([lbl (in-pvector (JvmMethodBBs-order mbb))])
      (define jvmbb (ordered-map-ref (JvmMethodBBs-blocks mbb) lbl #f))
      (define src (ordered-map-ref label->bid lbl #f))
      (for/fold ([g g]) ([s (in-pvector (JvmBB-successors jvmbb))])
        (define dst (ordered-map-ref label->bid s #f))
        (cond
          [dst
           (define-values (g* _) (graph-add-edge g src dst))
           g*]
          [else g]))))

  (define entry-bid
    (ordered-map-ref label->bid (JvmMethodBBs-entry mbb) #f))

  (Cfg graph-final
       blocks-map
       entry-bid
       #f
       vc-final
       0
       (ordered-map-empty symbol-compare)))

;; ============================================================
;; Local-index scan
;; ============================================================

;; Walk the insn stream; return the largest local index referenced.
(define local-op->index-pos
  ;; For each local-indexed opcode, the position in `operands`
  ;; where the index lives (here, always 0).
  (hash 'ILOAD 0 'LLOAD 0 'FLOAD 0 'DLOAD 0 'ALOAD 0
        'ISTORE 0 'LSTORE 0 'FSTORE 0 'DSTORE 0 'ASTORE 0
        'IINC 0))

(define (scan-max-local method)
  (for/fold ([mx -1]) ([i (in-list (JvmMethod-instructions method))])
    (cond
      [(hash-ref local-op->index-pos (JvmInsn-opcode i) #f)
       => (lambda (pos)
            (define idx (list-ref (JvmInsn-operands i) pos))
            (if (and (integer? idx) (> idx mx)) idx mx))]
      [else mx])))

;; ============================================================
;; Block translation
;; ============================================================

;; Translate a single JvmBB into a CfgBlock.
;; Returns (values cfg-block new-var-cnt).
(define (translate-block jvmbb bid entry-h vc label->bid)
  ;; Allocate entry-stack VarIds
  (define-values (init-stack vc1) (alloc-stack entry-h vc))

  (define insns-pv (JvmBB-insns jvmbb))
  (define n (pvector-length insns-pv))

  ;; Determine if the last insn is a terminator
  (define term?
    (and (> n 0)
         (terminator-opcode? (JvmInsn-opcode (pvector-ref insns-pv (- n 1))))))
  (define body-count (if term? (- n 1) n))

  ;; Walk body instructions
  (define-values (vf-pv stack-out vc2)
    (for/fold ([pv (pvector-empty)] [stack init-stack] [vc vc1])
              ([i (in-range body-count)])
      (define insn (pvector-ref insns-pv i))
      (define-values (vf stack* vc*) (translate-insn insn stack vc))
      (values (if vf (pvector-cons-right pv vf) pv) stack* vc*)))

  ;; Build terminator
  (define-values (term vf-extra-pv vc3)
    (cond
      [term?
       (translate-terminator (pvector-ref insns-pv (- n 1))
                             stack-out vc2 jvmbb label->bid)]
      [else
       (values (make-fallthrough-term jvmbb label->bid)
               (pvector-empty)
               vc2)]))

  (define all-insns (pvector-append vf-pv vf-extra-pv))

  (values (CfgBlock bid (pvector-empty) all-insns term (ordered-map-empty symbol-compare))
          vc3))

(define (alloc-stack h vc)
  (let loop ([i 0] [pv (pvector-empty)] [vc vc])
    (if (= i h)
        (values pv vc)
        (loop (add1 i) (pvector-cons-right pv (VarId vc)) (add1 vc)))))

(define (stack-pop-n stack n)
  (define h (pvector-length stack))
  (when (< h n)
    (error 'stack-pop-n "stack underflow: want ~a, have ~a" n h))
  (define popped
    (for/pvector ([i (in-range (- h n) h)])
      (pvector-ref stack i)))
  (values popped (pvector-take stack (- h n))))

(define (pvector-take pv k)
  (for/pvector ([i (in-range k)]) (pvector-ref pv i)))

(define (stack-push stack v)
  (pvector-cons-right stack v))

(define (fresh-var vc)
  (values (VarId vc) (add1 vc)))

;; ============================================================
;; Terminator detection
;; ============================================================

(define terminator-opcodes
  (set 'GOTO 'GOTO_W
       'IFEQ 'IFNE 'IFLT 'IFGE 'IFGT 'IFLE
       'IF_ICMPEQ 'IF_ICMPNE 'IF_ICMPLT 'IF_ICMPGE 'IF_ICMPGT 'IF_ICMPLE
       'IF_ACMPEQ 'IF_ACMPNE 'IFNULL 'IFNONNULL
       'RETURN 'IRETURN 'LRETURN 'FRETURN 'DRETURN 'ARETURN
       'ATHROW))

(define (terminator-opcode? op)
  (set-member? terminator-opcodes op))

(define (make-fallthrough-term jvmbb label->bid)
  (define succs (JvmBB-successors jvmbb))
  (cond
    [(= (pvector-length succs) 0) (Term:unreachable)]
    [else
     (define lbl (pvector-ref succs 0))
     (Term:jump (ordered-map-ref label->bid lbl #f))]))

;; ============================================================
;; Instruction translation
;; ============================================================

;; translate-insn : JvmInsn × pvector[VarId] × Integer
;;                -> (values (or #f VfInsn) pvector[VarId] Integer)
(define (translate-insn insn stack vc)
  (define op (JvmInsn-opcode insn))
  (define args (JvmInsn-operands insn))
  (case op
    ;; ---- Constants ----
    [(ACONST_NULL)
     (push-const op 'null stack vc)]
    [(ICONST_M1) (push-const op -1 stack vc)]
    [(ICONST_0)  (push-const op 0 stack vc)]
    [(ICONST_1)  (push-const op 1 stack vc)]
    [(ICONST_2)  (push-const op 2 stack vc)]
    [(ICONST_3)  (push-const op 3 stack vc)]
    [(ICONST_4)  (push-const op 4 stack vc)]
    [(ICONST_5)  (push-const op 5 stack vc)]
    [(BIPUSH SIPUSH)
     (push-const op (car args) stack vc)]
    [(LDC LDC_W LDC2_W)
     (push-const op (car args) stack vc)]

    ;; ---- Local loads ----
    [(ILOAD LLOAD FLOAD DLOAD ALOAD)
     (define idx (car args))
     (define-values (out vc*) (fresh-var vc))
     (values
       (VfInsn op
               (pvector-cons-right (pvector-empty) (VarId idx))
               (pvector-cons-right (pvector-empty) out)
               #f #f)
       (stack-push stack out)
       vc*)]

    ;; ---- Local stores ----
    [(ISTORE LSTORE FSTORE DSTORE ASTORE)
     (define idx (car args))
     (define-values (popped stack*) (stack-pop-n stack 1))
     (values
       (VfInsn op popped
               (pvector-cons-right (pvector-empty) (VarId idx))
               #f #f)
       stack*
       vc)]

    ;; ---- Stack manipulation ----
    [(POP)
     (define-values (_ stack*) (stack-pop-n stack 1))
     (values #f stack* vc)]
    [(POP2)
     (define-values (_ stack*) (stack-pop-n stack 2))
     (values #f stack* vc)]
    [(DUP)
     (define h (pvector-length stack))
     (when (= h 0) (error 'translate-insn "DUP on empty stack"))
     (values #f (pvector-cons-right stack (pvector-ref stack (- h 1))) vc)]
    [(SWAP)
     (define h (pvector-length stack))
     (when (< h 2) (error 'translate-insn "SWAP needs 2"))
     (define a (pvector-ref stack (- h 2)))
     (define b (pvector-ref stack (- h 1)))
     (values #f
             (pvector-cons-right
               (pvector-cons-right (pvector-take stack (- h 2)) b)
               a)
             vc)]

    ;; ---- Integer binary arithmetic (pop 2, push 1) ----
    [(IADD ISUB IMUL IDIV IREM
       ISHL ISHR IUSHR IAND IOR IXOR)
     (binop op stack vc)]

    ;; ---- Integer unary (pop 1, push 1) ----
    [(INEG)
     (unop op stack vc)]

    ;; ---- Field access ----
    [(GETSTATIC)
     ;; args: (owner name desc)
     (define-values (out vc*) (fresh-var vc))
     (values
       (VfInsn op
               (pvector-empty)
               (pvector-cons-right (pvector-empty) out)
               (hash 'owner (list-ref args 0)
                     'name  (list-ref args 1)
                     'desc  (list-ref args 2))
               #f)
       (stack-push stack out)
       vc*)]
    [(PUTSTATIC)
     (define-values (popped stack*) (stack-pop-n stack 1))
     (values
       (VfInsn op popped (pvector-empty)
               (hash 'owner (list-ref args 0)
                     'name  (list-ref args 1)
                     'desc  (list-ref args 2))
               #f)
       stack*
       vc)]
    [(GETFIELD)
     ;; pops objref, pushes value
     (define-values (popped stack*) (stack-pop-n stack 1))
     (define-values (out vc*) (fresh-var vc))
     (values
       (VfInsn op popped
               (pvector-cons-right (pvector-empty) out)
               (hash 'owner (list-ref args 0)
                     'name  (list-ref args 1)
                     'desc  (list-ref args 2))
               #f)
       (stack-push stack* out)
       vc*)]
    [(PUTFIELD)
     (define-values (popped stack*) (stack-pop-n stack 2))
     (values
       (VfInsn op popped (pvector-empty)
               (hash 'owner (list-ref args 0)
                     'name  (list-ref args 1)
                     'desc  (list-ref args 2))
               #f)
       stack*
       vc)]

    ;; ---- Method invocation ----
    [(INVOKESTATIC)
     (translate-invoke op args stack vc #:receiver? #f)]
    [(INVOKEVIRTUAL INVOKESPECIAL INVOKEINTERFACE)
     (translate-invoke op args stack vc #:receiver? #t)]

    [else
     (error 'translate-insn "unhandled opcode: ~a (args ~s)" op args)]))

(define (push-const op literal stack vc)
  (define-values (out vc*) (fresh-var vc))
  (values
    (VfInsn op
            (pvector-cons-right (pvector-empty) literal)
            (pvector-cons-right (pvector-empty) out)
            #f #f)
    (stack-push stack out)
    vc*))

(define (binop op stack vc)
  (define-values (popped stack*) (stack-pop-n stack 2))
  (define-values (out vc*) (fresh-var vc))
  (values
    (VfInsn op popped
            (pvector-cons-right (pvector-empty) out)
            #f #f)
    (stack-push stack* out)
    vc*))

(define (unop op stack vc)
  (define-values (popped stack*) (stack-pop-n stack 1))
  (define-values (out vc*) (fresh-var vc))
  (values
    (VfInsn op popped
            (pvector-cons-right (pvector-empty) out)
            #f #f)
    (stack-push stack* out)
    vc*))

(define (translate-invoke op args stack vc #:receiver? recv?)
  ;; args: (owner name desc . extra)  -- ASM reader may append
  ;; a boolean for INVOKEINTERFACE or INVOKEVIRTUAL/SPECIAL; just
  ;; ignore anything past the descriptor.
  (define owner (list-ref args 0))
  (define name  (list-ref args 1))
  (define desc  (list-ref args 2))
  (define-values (param-n has-ret?) (parse-method-descriptor desc))
  (define pop-n (if recv? (add1 param-n) param-n))
  (define-values (popped stack*) (stack-pop-n stack pop-n))
  (define info (hash 'owner owner 'name name 'desc desc))
  (cond
    [has-ret?
     (define-values (out vc*) (fresh-var vc))
     (values
       (VfInsn op popped
               (pvector-cons-right (pvector-empty) out)
               info #f)
       (stack-push stack* out)
       vc*)]
    [else
     (values
       (VfInsn op popped (pvector-empty) info #f)
       stack*
       vc)]))

;; ============================================================
;; Terminator translation
;; ============================================================

;; Returns (values terminator pre-terminator-vfinsns new-vc).
;; Some terminators emit a preceding VfInsn (e.g. IFEQ lowers to
;; an `ieq cond 0` VfInsn followed by Term:cond on the boolean).
(define (translate-terminator insn stack vc jvmbb label->bid)
  (define op (JvmInsn-opcode insn))
  (define args (JvmInsn-operands insn))
  (define succs (JvmBB-successors jvmbb))

  (define (bid-of lbl) (ordered-map-ref label->bid lbl #f))

  (case op
    ;; Unconditional jump: pvector[0] is the only successor.
    [(GOTO GOTO_W)
     (values (Term:jump (bid-of (car args))) (pvector-empty) vc)]

    ;; Unary comparison against 0: IFEQ/IFNE/IFLT/IFGE/IFGT/IFLE.
    [(IFEQ IFNE IFLT IFGE IFGT IFLE IFNULL IFNONNULL)
     (define-values (popped stack*) (stack-pop-n stack 1))
     (define-values (cond-var vc*) (fresh-var vc))
     (define pre
       (VfInsn op popped
               (pvector-cons-right (pvector-empty) cond-var)
               #f #f))
     (values (Term:cond cond-var
                        (bid-of (pvector-ref succs 0))
                        (bid-of (pvector-ref succs 1)))
             (pvector-cons-right (pvector-empty) pre)
             vc*)]

    ;; Binary comparison: IF_ICMP*, IF_ACMP*.
    [(IF_ICMPEQ IF_ICMPNE IF_ICMPLT IF_ICMPGE IF_ICMPGT IF_ICMPLE
      IF_ACMPEQ IF_ACMPNE)
     (define-values (popped stack*) (stack-pop-n stack 2))
     (define-values (cond-var vc*) (fresh-var vc))
     (define pre
       (VfInsn op popped
               (pvector-cons-right (pvector-empty) cond-var)
               #f #f))
     (values (Term:cond cond-var
                        (bid-of (pvector-ref succs 0))
                        (bid-of (pvector-ref succs 1)))
             (pvector-cons-right (pvector-empty) pre)
             vc*)]

    ;; Returns.
    [(RETURN)
     (values (Term:ret (pvector-empty)) (pvector-empty) vc)]
    [(IRETURN LRETURN FRETURN DRETURN ARETURN)
     (define-values (popped _) (stack-pop-n stack 1))
     (values (Term:ret popped) (pvector-empty) vc)]

    ;; Throw.
    [(ATHROW)
     (define-values (popped _) (stack-pop-n stack 1))
     (values (Term:throw (pvector-ref popped 0)) (pvector-empty) vc)]

    [else
     (error 'translate-terminator "unhandled terminator: ~a" op)]))
