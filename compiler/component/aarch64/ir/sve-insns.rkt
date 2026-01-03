#lang racket/base

;; SVE/SVE2 Instruction Definitions
;;
;; Complete instruction set definitions for ARM Scalable Vector Extension
;; and SVE2, organized by category.

(require racket/match
         "../../../../cutie-ftree/ordered-map.rkt"
         "../../../../cutie-ftree/comparator.rkt")

(provide
 ;; Instruction categories (lists of symbols)
 sve-arith-ops
 sve-fp-arith-ops
 sve-logical-ops
 sve-shift-ops
 sve-reduce-ops
 sve-cmp-ops
 sve-fp-cmp-ops
 sve-mem-ops
 sve-gather-scatter-ops
 sve-pred-ops
 sve-permute-ops
 sve-misc-ops

 ;; SVE2 extension categories
 sve2-complex-ops
 sve2-histogram-ops
 sve2-wide-ops
 sve2-narrow-ops
 sve2-bit-ops
 sve2-crypto-ops
 sve2-tbl-ops
 sve2-match-ops

 ;; All SVE ops
 all-sve-ops
 all-sve2-ops

 ;; Instruction classification
 sve-op?
 sve2-op?
 sve-arith-op?
 sve-mem-op?
 sve-pred-op?

 ;; Instruction format info
 InsnFormat InsnFormat? InsnFormat-mnemonic InsnFormat-format InsnFormat-operands InsnFormat-sve2-only?
 get-insn-format

 ;; Encoding table
 sve-insn-formats)

;; ============================================================================
;; SVE Instruction Categories
;; ============================================================================

;; Integer arithmetic (predicated)
(define sve-arith-ops
  '(add sub mul
    sdiv udiv
    smax smin umax umin
    sabd uabd                     ; Absolute difference
    sqadd sqsub uqadd uqsub       ; Saturating
    abs neg))

;; Floating-point arithmetic (predicated)
(define sve-fp-arith-ops
  '(fadd fsub fmul fdiv
    fmax fmin fmaxnm fminnm
    fabs fneg
    frecpe frecps                 ; Reciprocal estimate
    frsqrte frsqrts               ; Reciprocal square root estimate
    fsqrt
    fmla fmls                     ; Fused multiply-add/sub
    fmad fmsb fnmla fnmls         ; FMA variants
    fscale
    ftmad                         ; Trigonometric multiply-add
    ))

;; Logical operations
(define sve-logical-ops
  '(and orr eor bic
    not
    orn nor nand))

;; Shift operations
(define sve-shift-ops
  '(asr lsl lsr
    asrd                          ; Arithmetic shift right for divide
    asrr lslr lsrr                ; Reversed shifts
    sqshl sqshlu uqshl            ; Saturating shifts
    srshl srshr urshl urshr       ; Rounding shifts
    ))

;; Reduction operations
(define sve-reduce-ops
  '(;; Integer reductions
    addv uaddv saddv
    andv orv eorv
    smaxv sminv umaxv uminv

    ;; FP reductions
    faddv
    fmaxv fminv fmaxnmv fminnmv))

;; Comparison operations (produce predicate)
(define sve-cmp-ops
  '(cmpeq cmpne
    cmpge cmpgt cmple cmplt       ; Signed
    cmphi cmphs cmplo cmpls))     ; Unsigned

(define sve-fp-cmp-ops
  '(fcmeq fcmne
    fcmge fcmgt fcmle fcmlt
    fcmuo                         ; Unordered
    facge facgt                   ; Absolute compare
    ))

;; Memory operations
(define sve-mem-ops
  '(;; Contiguous loads
    ld1b ld1h ld1w ld1d
    ld1sb ld1sh ld1sw             ; Sign-extending loads

    ;; Contiguous stores
    st1b st1h st1w st1d

    ;; Non-temporal
    ldnt1b ldnt1h ldnt1w ldnt1d
    stnt1b stnt1h stnt1w stnt1d

    ;; First-fault loads
    ldff1b ldff1h ldff1w ldff1d
    ldff1sb ldff1sh ldff1sw

    ;; Non-fault loads
    ldnf1b ldnf1h ldnf1w ldnf1d
    ldnf1sb ldnf1sh ldnf1sw

    ;; Replicate
    ld1rb ld1rh ld1rw ld1rd
    ld1rsb ld1rsh ld1rsw
    ld1rqb ld1rqh ld1rqw ld1rqd   ; Replicate quadword

    ;; Load/store multiple structures
    ld2b ld2h ld2w ld2d
    ld3b ld3h ld3w ld3d
    ld4b ld4h ld4w ld4d
    st2b st2h st2w st2d
    st3b st3h st3w st3d
    st4b st4h st4w st4d))

;; Gather/scatter operations
(define sve-gather-scatter-ops
  '(;; Gather loads
    ld1b-gather ld1h-gather ld1w-gather ld1d-gather
    ld1sb-gather ld1sh-gather ld1sw-gather
    ldff1b-gather ldff1h-gather ldff1w-gather ldff1d-gather

    ;; Scatter stores
    st1b-scatter st1h-scatter st1w-scatter st1d-scatter))

;; Predicate operations
(define sve-pred-ops
  '(;; Predicate creation
    ptrue pfalse
    whilelt whilele whilegt whilege
    whilelo whilels whilehi whilehs
    whilerw whilewr

    ;; Predicate manipulation
    pnext pfirst
    brka brkb brkn brkpa brkpb
    brkas brkbs brkns brkpas brkpbs

    ;; Predicate counting
    cntp

    ;; Predicate read/write
    rdffr rdffrs wrffr setffr

    ;; Predicate logical
    ands orrs eors bics nors nands))

;; Permute operations
(define sve-permute-ops
  '(;; Element permutation
    compact
    splice
    rev revb revh revw
    tbl tbx
    zip1 zip2
    uzp1 uzp2
    trn1 trn2

    ;; Duplicate
    dup dupm
    insr

    ;; Extract/insert
    ext
    lastb lasta
    clastb clasta

    ;; Index generation
    index))

;; Miscellaneous
(define sve-misc-ops
  '(;; Move
    mov movprfx
    sel

    ;; Increment/decrement by element count
    incb inch incw incd
    decb dech decw decd
    sqincb sqinch sqincw sqincd
    sqincw-scalar sqincd-scalar
    uqincb uqinch uqincw uqincd

    ;; Count
    cntb cnth cntw cntd

    ;; Convert
    fcvt fcvtzs fcvtzu
    scvtf ucvtf

    ;; Predicate test
    ptest))

;; ============================================================================
;; SVE2 Extension Categories
;; ============================================================================

;; Complex arithmetic
(define sve2-complex-ops
  '(cadd csub                     ; Complex add/sub
    cmla cmla-indexed             ; Complex multiply-add
    sqcadd                        ; Saturating complex add
    cdot cdot-indexed))           ; Complex dot product

;; Histogram operations
(define sve2-histogram-ops
  '(histcnt histseg))

;; Widening operations
(define sve2-wide-ops
  '(saddwb saddwt uaddwb uaddwt
    ssubwb ssubwt usubwb usubwt
    smullb smullt umullb umullt
    sqdmullb sqdmullt
    pmullb pmullt))               ; Polynomial multiply

;; Narrowing operations
(define sve2-narrow-ops
  '(addhnb addhnt subhnb subhnt
    raddhnb raddhnt rsubhnb rsubhnt
    sqxtnb sqxtnt uqxtnb uqxtnt sqxtunb sqxtunt
    sqshrunb sqshrunt uqshrunb uqshrunt
    sqrshrunb sqrshrunt))

;; Bit manipulation
(define sve2-bit-ops
  '(bdep bext bgrp                ; Bit deposit/extract/group
    sli sri))                     ; Shift and insert

;; Crypto extensions
(define sve2-crypto-ops
  '(;; AES
    aesd aese aesimc aesmc

    ;; SM4
    sm4e sm4ekey

    ;; SHA512
    rax1
    sha512h sha512h2 sha512su0 sha512su1))

;; Table lookup
(define sve2-tbl-ops
  '(tbl-multi tbx-multi))         ; Multi-register table lookup

;; Match operations
(define sve2-match-ops
  '(match nmatch))

;; ============================================================================
;; Combined Lists
;; ============================================================================

(define all-sve-ops
  (append sve-arith-ops
          sve-fp-arith-ops
          sve-logical-ops
          sve-shift-ops
          sve-reduce-ops
          sve-cmp-ops
          sve-fp-cmp-ops
          sve-mem-ops
          sve-gather-scatter-ops
          sve-pred-ops
          sve-permute-ops
          sve-misc-ops))

(define all-sve2-ops
  (append sve2-complex-ops
          sve2-histogram-ops
          sve2-wide-ops
          sve2-narrow-ops
          sve2-bit-ops
          sve2-crypto-ops
          sve2-tbl-ops
          sve2-match-ops))

;; ============================================================================
;; Instruction Classification
;; ============================================================================

(define (sve-op? op)
  (memq op all-sve-ops))

(define (sve2-op? op)
  (memq op all-sve2-ops))

(define (sve-arith-op? op)
  (or (memq op sve-arith-ops)
      (memq op sve-fp-arith-ops)))

(define (sve-mem-op? op)
  (or (memq op sve-mem-ops)
      (memq op sve-gather-scatter-ops)))

(define (sve-pred-op? op)
  (memq op sve-pred-ops))

;; ============================================================================
;; Instruction Format Information
;; ============================================================================

;; InsnFormat describes how an instruction is encoded/emitted
;; - mnemonic: string for assembly output
;; - format: symbol describing operand layout
;; - operands: list of operand types
;; - sve2-only?: #t if this is SVE2 only
(struct InsnFormat (mnemonic format operands sve2-only?) #:prefab)

;; Instruction formats table (using ordered-map for efficient lookup)
(define sve-insn-formats
  (make-hasheq
   ;; Arithmetic
   `((add . ,(InsnFormat "add" 'zpzz '(z p z z) #f))
     (sub . ,(InsnFormat "sub" 'zpzz '(z p z z) #f))
     (mul . ,(InsnFormat "mul" 'zpzz '(z p z z) #f))
     (fadd . ,(InsnFormat "fadd" 'zpzz '(z p z z) #f))
     (fsub . ,(InsnFormat "fsub" 'zpzz '(z p z z) #f))
     (fmul . ,(InsnFormat "fmul" 'zpzz '(z p z z) #f))
     (fdiv . ,(InsnFormat "fdiv" 'zpzz '(z p z z) #f))

     ;; Memory
     (ld1w . ,(InsnFormat "ld1w" 'zpaddr '(z p addr) #f))
     (st1w . ,(InsnFormat "st1w" 'zpaddr '(z p addr) #f))
     (ld1d . ,(InsnFormat "ld1d" 'zpaddr '(z p addr) #f))
     (st1d . ,(InsnFormat "st1d" 'zpaddr '(z p addr) #f))

     ;; Predicate
     (ptrue . ,(InsnFormat "ptrue" 'p '(p) #f))
     (whilelt . ,(InsnFormat "whilelt" 'pxx '(p x x) #f))

     ;; Reduction
     (faddv . ,(InsnFormat "faddv" 'vpz '(v p z) #f))

     ;; SVE2
     (cdot . ,(InsnFormat "cdot" 'zpzz '(z p z z) #t))
     (histcnt . ,(InsnFormat "histcnt" 'zpzz '(z p z z) #t))
     (match . ,(InsnFormat "match" 'ppzz '(p p z z) #t))
     )))

(define (get-insn-format op)
  (hash-ref sve-insn-formats op #f))
