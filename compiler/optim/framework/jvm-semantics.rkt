#lang racket/base

;; ============================================================
;; JVM Bytecode Semantics Provider
;; ============================================================
;;
;; Concrete implementation of InsnSemantics for JVM bytecode
;; operations in CFG form.
;;
;; This module encapsulates all JVM-specific knowledge, keeping
;; the optimization passes abstract.
;; ============================================================

(require racket/match racket/list racket/set racket/function)
(require "semantics.rkt")
(require "../../ir/cfg/types.rkt")

;; ============================================================
;; JVM Opcode Classification
;; ============================================================

;; Local variable load operations
(define jvm-local-loads
  '(iload lload fload dload aload
    iload_0 iload_1 iload_2 iload_3
    lload_0 lload_1 lload_2 lload_3
    fload_0 fload_1 fload_2 fload_3
    dload_0 dload_1 dload_2 dload_3
    aload_0 aload_1 aload_2 aload_3
    load-local))

;; Local variable store operations
(define jvm-local-stores
  '(istore lstore fstore dstore astore
    istore_0 istore_1 istore_2 istore_3
    lstore_0 lstore_1 lstore_2 lstore_3
    fstore_0 fstore_1 fstore_2 fstore_3
    dstore_0 dstore_1 dstore_2 dstore_3
    astore_0 astore_1 astore_2 astore_3
    store-local))

;; Array load operations
(define jvm-array-loads
  '(iaload laload faload daload aaload baload caload saload
    vector-ref))

;; Array store operations
(define jvm-array-stores
  '(iastore lastore fastore dastore aastore bastore castore sastore
    vector-set!))

;; Field access operations
(define jvm-field-loads
  '(getfield getstatic))

(define jvm-field-stores
  '(putfield putstatic))

;; All memory read operations
(define jvm-memory-reads
  (append jvm-local-loads jvm-array-loads jvm-field-loads '(load)))

;; All memory write operations
(define jvm-memory-writes
  (append jvm-local-stores jvm-array-stores jvm-field-stores '(store)))

;; Pure arithmetic operations
(define jvm-pure-arithmetic
  '(iadd isub imul idiv irem ineg
    ladd lsub lmul ldiv lrem lneg
    fadd fsub fmul fdiv frem fneg
    dadd dsub dmul ddiv drem dneg
    add sub mul div rem neg
    iand ior ixor ishl ishr iushr
    land lor lxor lshl lshr lushr
    and or xor shl shr ushr
    i2l i2f i2d l2i l2f l2d
    f2i f2l f2d d2i d2l d2f
    i2b i2c i2s
    lcmp fcmpl fcmpg dcmpl dcmpg
    not eq ne lt le gt ge
    instanceof checkcast))

;; Constant operations
(define jvm-constants
  '(iconst_m1 iconst_0 iconst_1 iconst_2 iconst_3 iconst_4 iconst_5
    lconst_0 lconst_1
    fconst_0 fconst_1 fconst_2
    dconst_0 dconst_1
    bipush sipush
    ldc ldc_w ldc2_w
    aconst_null
    const))

;; Call operations (may have side effects)
(define jvm-calls
  '(invokevirtual invokespecial invokestatic invokeinterface invokedynamic
    call invoke))

;; Object creation
(define jvm-allocations
  '(new newarray anewarray multianewarray))

;; Exception related
(define jvm-exception-ops
  '(athrow monitorenter monitorexit))

;; Commutative operations
(define jvm-commutative
  '(iadd ladd fadd dadd add
    imul lmul fmul dmul mul
    iand land and
    ior lor or
    ixor lxor xor
    eq ne))

;; Associative operations
(define jvm-associative
  '(iadd ladd add
    imul lmul mul
    iand land and
    ior lor or
    ixor lxor xor))

;; ============================================================
;; Memory Effect Analysis
;; ============================================================

(define (jvm-memory-effect insn)
  (match insn
    [(VfInsn op _ _ _ _)
     (cond
       [(and (memq op jvm-memory-reads)
             (memq op jvm-memory-writes))
        'read-write]
       [(memq op jvm-memory-reads) 'read]
       [(memq op jvm-memory-writes) 'write]
       [(memq op jvm-calls) 'read-write]  ; Conservative for calls
       [(memq op jvm-allocations) 'write]
       [(memq op jvm-exception-ops) 'read-write]
       [else 'none])]
    [_ 'unknown]))

;; ============================================================
;; Memory Location Extraction
;; ============================================================

(define (jvm-read-locations insn)
  (match insn
    ;; Local variable load
    [(VfInsn (? (lambda (op) (memq op jvm-local-loads))) (list idx) _ _ _)
     (list (MemLoc 'local 'locals idx 1))]

    [(VfInsn 'load-local (list idx) _ _ _)
     (list (MemLoc 'local 'locals idx 1))]

    ;; Array load
    [(VfInsn (? (lambda (op) (memq op jvm-array-loads)))
             (list base-var idx-var) _ _ _)
     (list (MemLoc 'heap base-var 'unknown 1))]

    [(VfInsn 'vector-ref (list base-var idx) _ _ _)
     (if (integer? idx)
         (list (MemLoc 'heap base-var idx 1))
         (list (MemLoc 'heap base-var 'unknown 1)))]

    ;; Field load
    [(VfInsn 'getfield (list obj-var field-name) _ _ _)
     (list (MemLoc 'heap obj-var field-name 1))]

    [(VfInsn 'getstatic (list class-name field-name) _ _ _)
     (list (MemLoc 'static (list class-name field-name) 0 1))]

    ;; Generic load
    [(VfInsn 'load inputs _ _ _)
     (if (and (pair? inputs) (VarId? (car inputs)))
         (list (MemLoc 'heap (car inputs) 'unknown 'unknown))
         (list mem-unknown))]

    ;; Calls may read anything
    [(VfInsn (? (lambda (op) (memq op jvm-calls))) _ _ _ _)
     (list mem-unknown)]

    [_ '()]))

(define (jvm-write-locations insn)
  (match insn
    ;; Local variable store
    [(VfInsn (? (lambda (op) (memq op jvm-local-stores))) (list _ idx) _ _ _)
     (list (MemLoc 'local 'locals idx 1))]

    [(VfInsn 'store-local (list _ idx) _ _ _)
     (list (MemLoc 'local 'locals idx 1))]

    ;; Array store
    [(VfInsn (? (lambda (op) (memq op jvm-array-stores)))
             (list base-var idx-var val-var) _ _ _)
     (list (MemLoc 'heap base-var 'unknown 1))]

    [(VfInsn 'vector-set! (list base-var idx val) _ _ _)
     (if (integer? idx)
         (list (MemLoc 'heap base-var idx 1))
         (list (MemLoc 'heap base-var 'unknown 1)))]

    ;; Field store
    [(VfInsn 'putfield (list obj-var field-name val-var) _ _ _)
     (list (MemLoc 'heap obj-var field-name 1))]

    [(VfInsn 'putstatic (list class-name field-name val-var) _ _ _)
     (list (MemLoc 'static (list class-name field-name) 0 1))]

    ;; Generic store
    [(VfInsn 'store inputs _ _ _)
     (if (and (pair? inputs) (VarId? (car inputs)))
         (list (MemLoc 'heap (car inputs) 'unknown 'unknown))
         (list mem-unknown))]

    ;; Calls may write anything
    [(VfInsn (? (lambda (op) (memq op jvm-calls))) _ _ _ _)
     (list mem-unknown)]

    ;; Allocations write to heap
    [(VfInsn (? (lambda (op) (memq op jvm-allocations))) _ outputs _ _)
     (if (pair? outputs)
         (list (MemLoc 'heap (car outputs) 0 'unknown))
         '())]

    [_ '()]))

;; ============================================================
;; Purity Analysis
;; ============================================================

(define (jvm-purity insn)
  (match insn
    [(VfInsn op _ _ _ _)
     (cond
       [(memq op jvm-pure-arithmetic) 'pure]
       [(memq op jvm-constants) 'pure]
       [(memq op jvm-memory-reads)
        (if (memq op jvm-local-loads)
            'read-only  ; Local loads are read-only
            'read-only)]
       [(memq op jvm-memory-writes) 'side-effecting]
       [(memq op jvm-calls) 'side-effecting]
       [(memq op jvm-allocations) 'side-effecting]
       [(memq op jvm-exception-ops) 'side-effecting]
       [else 'pure])]  ; Unknown ops assumed pure
    [_ 'unknown]))

;; ============================================================
;; Control Flow Effect
;; ============================================================

(define (jvm-control-effect insn)
  (match insn
    [(VfInsn op _ _ _ _)
     (cond
       [(memq op jvm-calls) 'call]
       [(memq op '(athrow)) 'throw]
       [else 'none])]
    [_ 'none]))

;; ============================================================
;; Value Properties
;; ============================================================

(define (jvm-is-constant? insn)
  (match insn
    [(VfInsn op _ _ _ _)
     (memq op jvm-constants)]
    [_ #f]))

(define (jvm-is-copy? insn)
  (match insn
    ;; Direct assignment: v1 = v0
    [(VfInsn 'copy (list (? VarId?)) (list (? VarId?)) _ _) #t]
    [(VfInsn 'mov (list (? VarId?)) (list (? VarId?)) _ _) #t]
    ;; iload/aload followed by istore/astore with same index could be copy
    [_ #f]))

(define (jvm-is-commutative? insn)
  (match insn
    [(VfInsn op _ _ _ _)
     (and (memq op jvm-commutative) #t)]
    [_ #f]))

(define (jvm-is-associative? insn)
  (match insn
    [(VfInsn op _ _ _ _)
     (and (memq op jvm-associative) #t)]
    [_ #f]))

;; ============================================================
;; Expression Key for GVN
;; ============================================================

(define (jvm-expr-key insn)
  (match insn
    [(VfInsn op inputs _ _ _)
     ;; For commutative ops, sort inputs for canonical form
     (if (memq op jvm-commutative)
         (list op (sort inputs expr-input<?))
         (list op inputs))]
    [_ insn]))

;; Helper: compare expression inputs
(define (expr-input<? a b)
  (cond
    [(and (VarId? a) (VarId? b))
     (< (VarId-id a) (VarId-id b))]
    [(VarId? a) #t]
    [(VarId? b) #f]
    [(and (number? a) (number? b)) (< a b)]
    [(number? a) #t]
    [(number? b) #f]
    [else (string<? (format "~a" a) (format "~a" b))]))

;; ============================================================
;; Constant Evaluation
;; ============================================================

(define (jvm-const-eval insn var->const)
  (match insn
    ;; Constant producers
    [(VfInsn 'const (list val) _ _ _) val]
    [(VfInsn 'iconst_0 _ _ _ _) 0]
    [(VfInsn 'iconst_1 _ _ _ _) 1]
    [(VfInsn 'iconst_2 _ _ _ _) 2]
    [(VfInsn 'iconst_3 _ _ _ _) 3]
    [(VfInsn 'iconst_4 _ _ _ _) 4]
    [(VfInsn 'iconst_5 _ _ _ _) 5]
    [(VfInsn 'iconst_m1 _ _ _ _) -1]
    [(VfInsn 'lconst_0 _ _ _ _) 0]
    [(VfInsn 'lconst_1 _ _ _ _) 1]

    ;; Binary arithmetic
    [(VfInsn (or 'add 'iadd 'ladd) (list a b) _ _ _)
     (let ([va (get-const a var->const)]
           [vb (get-const b var->const)])
       (and va vb (+ va vb)))]

    [(VfInsn (or 'sub 'isub 'lsub) (list a b) _ _ _)
     (let ([va (get-const a var->const)]
           [vb (get-const b var->const)])
       (and va vb (- va vb)))]

    [(VfInsn (or 'mul 'imul 'lmul) (list a b) _ _ _)
     (let ([va (get-const a var->const)]
           [vb (get-const b var->const)])
       (and va vb (* va vb)))]

    [(VfInsn (or 'div 'idiv 'ldiv) (list a b) _ _ _)
     (let ([va (get-const a var->const)]
           [vb (get-const b var->const)])
       (and va vb (not (zero? vb)) (quotient va vb)))]

    [(VfInsn (or 'rem 'irem 'lrem) (list a b) _ _ _)
     (let ([va (get-const a var->const)]
           [vb (get-const b var->const)])
       (and va vb (not (zero? vb)) (remainder va vb)))]

    ;; Bitwise
    [(VfInsn (or 'and 'iand 'land) (list a b) _ _ _)
     (let ([va (get-const a var->const)]
           [vb (get-const b var->const)])
       (and va vb (bitwise-and va vb)))]

    [(VfInsn (or 'or 'ior 'lor) (list a b) _ _ _)
     (let ([va (get-const a var->const)]
           [vb (get-const b var->const)])
       (and va vb (bitwise-ior va vb)))]

    [(VfInsn (or 'xor 'ixor 'lxor) (list a b) _ _ _)
     (let ([va (get-const a var->const)]
           [vb (get-const b var->const)])
       (and va vb (bitwise-xor va vb)))]

    [(VfInsn (or 'shl 'ishl 'lshl) (list a b) _ _ _)
     (let ([va (get-const a var->const)]
           [vb (get-const b var->const)])
       (and va vb (arithmetic-shift va vb)))]

    [(VfInsn (or 'shr 'ishr 'lshr) (list a b) _ _ _)
     (let ([va (get-const a var->const)]
           [vb (get-const b var->const)])
       (and va vb (arithmetic-shift va (- vb))))]

    ;; Unary
    [(VfInsn (or 'neg 'ineg 'lneg) (list a) _ _ _)
     (let ([va (get-const a var->const)])
       (and va (- va)))]

    [(VfInsn 'not (list a) _ _ _)
     (let ([va (get-const a var->const)])
       (and va (if (zero? va) 1 0)))]

    ;; Comparisons
    [(VfInsn 'eq (list a b) _ _ _)
     (let ([va (get-const a var->const)]
           [vb (get-const b var->const)])
       (and va vb (if (= va vb) 1 0)))]

    [(VfInsn 'ne (list a b) _ _ _)
     (let ([va (get-const a var->const)]
           [vb (get-const b var->const)])
       (and va vb (if (not (= va vb)) 1 0)))]

    [(VfInsn 'lt (list a b) _ _ _)
     (let ([va (get-const a var->const)]
           [vb (get-const b var->const)])
       (and va vb (if (< va vb) 1 0)))]

    [(VfInsn 'le (list a b) _ _ _)
     (let ([va (get-const a var->const)]
           [vb (get-const b var->const)])
       (and va vb (if (<= va vb) 1 0)))]

    [(VfInsn 'gt (list a b) _ _ _)
     (let ([va (get-const a var->const)]
           [vb (get-const b var->const)])
       (and va vb (if (> va vb) 1 0)))]

    [(VfInsn 'ge (list a b) _ _ _)
     (let ([va (get-const a var->const)]
           [vb (get-const b var->const)])
       (and va vb (if (>= va vb) 1 0)))]

    [_ #f]))

;; Helper: get constant value for an input
(define (get-const input var->const)
  (cond
    [(integer? input) input]
    [(VarId? input) (var->const input)]
    [else #f]))

;; ============================================================
;; Algebraic Simplification
;; ============================================================

(define (jvm-simplify insn var->def)
  ;; Try various algebraic simplifications
  (match insn
    ;; x + 0 = x
    [(VfInsn (or 'add 'iadd 'ladd) (list x (? (curry equal? 0))) outputs info id)
     (if (VarId? x)
         (VfInsn 'copy (list x) outputs info id)
         #f)]

    ;; 0 + x = x
    [(VfInsn (or 'add 'iadd 'ladd) (list (? (curry equal? 0)) x) outputs info id)
     (if (VarId? x)
         (VfInsn 'copy (list x) outputs info id)
         #f)]

    ;; x - 0 = x
    [(VfInsn (or 'sub 'isub 'lsub) (list x (? (curry equal? 0))) outputs info id)
     (if (VarId? x)
         (VfInsn 'copy (list x) outputs info id)
         #f)]

    ;; x * 0 = 0
    [(VfInsn (or 'mul 'imul 'lmul) (list _ (? (curry equal? 0))) outputs info id)
     (VfInsn 'const (list 0) outputs info id)]

    ;; 0 * x = 0
    [(VfInsn (or 'mul 'imul 'lmul) (list (? (curry equal? 0)) _) outputs info id)
     (VfInsn 'const (list 0) outputs info id)]

    ;; x * 1 = x
    [(VfInsn (or 'mul 'imul 'lmul) (list x (? (curry equal? 1))) outputs info id)
     (if (VarId? x)
         (VfInsn 'copy (list x) outputs info id)
         #f)]

    ;; 1 * x = x
    [(VfInsn (or 'mul 'imul 'lmul) (list (? (curry equal? 1)) x) outputs info id)
     (if (VarId? x)
         (VfInsn 'copy (list x) outputs info id)
         #f)]

    ;; x * 2 = x << 1 (strength reduction)
    [(VfInsn (or 'mul 'imul 'lmul) (list x (? power-of-2?)) outputs info id)
     (define shift (integer-length (- (if (number? (cadr (VfInsn-inputs insn)))
                                          (cadr (VfInsn-inputs insn))
                                          2) 1)))
     (VfInsn 'shl (list x shift) outputs info id)]

    ;; x / 1 = x
    [(VfInsn (or 'div 'idiv 'ldiv) (list x (? (curry equal? 1))) outputs info id)
     (if (VarId? x)
         (VfInsn 'copy (list x) outputs info id)
         #f)]

    ;; x & 0 = 0
    [(VfInsn (or 'and 'iand 'land) (list _ (? (curry equal? 0))) outputs info id)
     (VfInsn 'const (list 0) outputs info id)]

    ;; x | 0 = x
    [(VfInsn (or 'or 'ior 'lor) (list x (? (curry equal? 0))) outputs info id)
     (if (VarId? x)
         (VfInsn 'copy (list x) outputs info id)
         #f)]

    ;; x ^ 0 = x
    [(VfInsn (or 'xor 'ixor 'lxor) (list x (? (curry equal? 0))) outputs info id)
     (if (VarId? x)
         (VfInsn 'copy (list x) outputs info id)
         #f)]

    ;; x << 0 = x
    [(VfInsn (or 'shl 'ishl 'lshl) (list x (? (curry equal? 0))) outputs info id)
     (if (VarId? x)
         (VfInsn 'copy (list x) outputs info id)
         #f)]

    ;; x >> 0 = x
    [(VfInsn (or 'shr 'ishr 'lshr) (list x (? (curry equal? 0))) outputs info id)
     (if (VarId? x)
         (VfInsn 'copy (list x) outputs info id)
         #f)]

    [_ #f]))

;; Helper: check if number is power of 2
(define (power-of-2? n)
  (and (integer? n)
       (> n 0)
       (zero? (bitwise-and n (- n 1)))))

;; ============================================================
;; Complete JVM Semantics Provider
;; ============================================================

(define jvm-semantics
  (InsnSemantics
   jvm-memory-effect
   jvm-read-locations
   jvm-write-locations
   jvm-purity
   jvm-control-effect
   jvm-is-constant?
   jvm-is-copy?
   jvm-is-commutative?
   jvm-is-associative?
   jvm-expr-key
   jvm-const-eval
   jvm-simplify))

(provide jvm-semantics)

;; ============================================================
;; Backward Compatibility Exports
;; ============================================================

;; These provide the old interface for gradual migration
(define (memory-read-op? op)
  (and (memq op jvm-memory-reads) #t))

(define (memory-write-op? op)
  (and (memq op jvm-memory-writes) #t))

(define (has-side-effect? op)
  (or (memq op jvm-memory-writes)
      (memq op jvm-calls)
      (memq op jvm-allocations)
      (memq op jvm-exception-ops)))

(provide memory-read-op? memory-write-op? has-side-effect?)
