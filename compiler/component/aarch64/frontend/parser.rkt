#lang racket/base

;; AArch64 Assembly S-Expression Parser
;;
;; Parses S-expressions into AArch64 IR structures.
;; This is a function-based parser, NOT macro-based.
;; Users write pure data (quoted S-expressions) that this module transforms.

(require racket/match
         racket/string
         "../../../../cutie-ftree/pvector.rkt"
         "../ir/types.rkt"
         "../ir/cfg.rkt"
         "../ir/config.rkt"
         "../ir/sve-insns.rkt")

(provide
 ;; Main entry point
 parse-asm-module
 parse-asm-fn

 ;; Component parsers (for testing)
 parse-params
 parse-type
 parse-reg
 parse-operand
 parse-insn
 parse-block
 parse-body

 ;; Error handling
 parse-error
 ParseError ParseError? ParseError-message ParseError-context)

;; ============================================================================
;; Error Handling
;; ============================================================================

(struct ParseError (message context) #:prefab)

(define (parse-error msg ctx)
  (raise (ParseError msg ctx)))

;; ============================================================================
;; Main Entry Points
;; ============================================================================

;; Parse a complete module (multiple functions)
;; Input: '(asm-module (asm-fn name1 ...) (asm-fn name2 ...))
(define (parse-asm-module sexp)
  (match sexp
    [`(asm-module . ,fns)
     (map parse-asm-fn fns)]
    [_ (parse-error "expected (asm-module ...)" sexp)]))

;; Parse a single function
;; Input: '(asm-fn name (params...) -> ret-type body...)
(define (parse-asm-fn sexp)
  (match sexp
    [`(asm-fn ,name ,params -> ,ret-type . ,body)
     (AsmFunction name
                  (parse-params params)
                  (parse-type ret-type)
                  (parse-body body))]
    [_ (parse-error "expected (asm-fn name (params...) -> ret-type body...)" sexp)]))

;; ============================================================================
;; Parameter Parsing
;; ============================================================================

;; Parse parameter list
;; Input: '([x0 : i64] [z0 : (sve f32)] [p0 : pred])
(define (parse-params params)
  (for/list ([p (in-list params)])
    (parse-param p)))

(define (parse-param p)
  (match p
    [`[,reg : ,type]
     (AsmParam (parse-reg reg) (parse-type type))]
    [_ (parse-error "expected [reg : type]" p)]))

;; ============================================================================
;; Type Parsing
;; ============================================================================

;; Parse type annotation
(define (parse-type t)
  (match t
    ;; Scalar integers (signed)
    ['i8  (Type:scalar 8 #t)]
    ['i16 (Type:scalar 16 #t)]
    ['i32 (Type:scalar 32 #t)]
    ['i64 (Type:scalar 64 #t)]

    ;; Scalar integers (unsigned)
    ['u8  (Type:scalar 8 #f)]
    ['u16 (Type:scalar 16 #f)]
    ['u32 (Type:scalar 32 #f)]
    ['u64 (Type:scalar 64 #f)]

    ;; Floating point
    ['f16 (Type:float 16)]
    ['f32 (Type:float 32)]
    ['f64 (Type:float 64)]

    ;; Predicate
    ['pred (Type:pred)]

    ;; Void
    ['void (Type:void)]

    ;; NEON vector: (vec elem lanes)
    [`(vec ,elem ,lanes)
     (Type:vec (parse-type elem) lanes)]

    ;; SVE vector: (sve elem)
    [`(sve ,elem)
     (Type:sve (parse-type elem))]

    ;; SVE2 vector: (sve2 elem)
    [`(sve2 ,elem)
     (Type:sve2 (parse-type elem))]

    ;; Pointer: (ptr elem)
    [`(ptr ,elem)
     (Type:ptr (parse-type elem))]

    [_ (parse-error "unknown type" t)]))

;; ============================================================================
;; Register Parsing
;; ============================================================================

;; Parse register name
(define (parse-reg sym)
  (define s (symbol->string sym))
  (cond
    ;; x-registers (64-bit)
    [(regexp-match #rx"^x([0-9]+)$" s)
     => (λ (m) (Reg:x (string->number (cadr m))))]

    ;; w-registers (32-bit)
    [(regexp-match #rx"^w([0-9]+)$" s)
     => (λ (m) (Reg:w (string->number (cadr m))))]

    ;; z-registers (SVE vector)
    [(regexp-match #rx"^z([0-9]+)$" s)
     => (λ (m) (Reg:z (string->number (cadr m))))]

    ;; p-registers (predicate)
    [(regexp-match #rx"^p([0-9]+)$" s)
     => (λ (m) (Reg:p (string->number (cadr m))))]

    ;; v-registers (NEON)
    [(regexp-match #rx"^v([0-9]+)$" s)
     => (λ (m) (Reg:v (string->number (cadr m)) 'q))]

    ;; v-registers with width: v0.4s, v1.2d, etc.
    [(regexp-match #rx"^v([0-9]+)\\.([0-9]+)([bhsdq])$" s)
     => (λ (m) (Reg:v (string->number (cadr m))
                       (string->symbol (cadddr m))))]

    ;; Special registers
    [(string=? s "sp") (Reg:sp)]
    [(string=? s "xzr") (Reg:xzr)]
    [(string=? s "wzr") (Reg:wzr)]

    [else (parse-error "unknown register" sym)]))

;; ============================================================================
;; Operand Parsing
;; ============================================================================

;; Parse any operand (register, immediate, memory, label)
(define (parse-operand op)
  (cond
    ;; Symbol -> register or label
    [(symbol? op)
     (cond
       [(register-symbol? op) (parse-reg op)]
       [else (Label:named op)])]

    ;; Number -> immediate
    [(number? op)
     (Imm op)]

    ;; List -> memory or shifted immediate
    [(list? op)
     (parse-complex-operand op)]

    [else (parse-error "unknown operand" op)]))

(define (register-symbol? sym)
  (define s (symbol->string sym))
  (or (regexp-match? #rx"^[xwzpv][0-9]+" s)
      (memq sym '(sp xzr wzr))))

;; Parse complex operand (memory addressing, shifted immediate)
(define (parse-complex-operand op)
  (match op
    ;; Memory base only: [xn]
    [`[,base]
     (Mem:base (parse-reg base))]

    ;; Memory offset: [xn #imm] or [xn imm]
    [`[,base ,offset]
     #:when (or (number? offset)
                (and (symbol? offset)
                     (string-prefix? (symbol->string offset) "#")))
     (Mem:offset (parse-reg base) (parse-offset offset))]

    ;; Pre-indexed: [xn #imm]!
    [`[,base ,offset !]
     (Mem:pre (parse-reg base) (parse-offset offset))]

    ;; Register offset: [xn xm]
    [`[,base ,index]
     #:when (register-symbol? index)
     (Mem:reg (parse-reg base) (parse-reg index))]

    ;; Scaled: [xn xm lsl #scale]
    [`[,base ,index lsl ,scale]
     (Mem:scaled (parse-reg base) (parse-reg index) (parse-offset scale))]

    ;; Shifted immediate: (lsl imm shift)
    [`(lsl ,value ,shift)
     (Imm:shifted value shift)]

    [_ (parse-error "unknown complex operand" op)]))

;; Parse offset (number or #number)
(define (parse-offset off)
  (cond
    [(number? off) off]
    [(symbol? off)
     (define s (symbol->string off))
     (cond
       [(string-prefix? s "#")
        (string->number (substring s 1))]
       [else (parse-error "invalid offset" off)])]
    [else (parse-error "invalid offset" off)]))

;; ============================================================================
;; Instruction Parsing
;; ============================================================================

;; Parse a single instruction
(define (parse-insn sexp)
  (match sexp
    ;; Return
    [`(ret)
     (Insn:ret)]

    ;; Arithmetic: (add dst src1 src2)
    [`(,op ,dst ,src1 ,src2)
     #:when (memq op '(add sub mul sdiv udiv
                       adc sbc madd msub
                       and orr eor bic orn))
     (Insn:arith op
                 (parse-operand dst)
                 (parse-operand src1)
                 (parse-operand src2))]

    ;; Floating-point arithmetic: (fadd dst src1 src2)
    [`(,op ,dst ,src1 ,src2)
     #:when (memq op '(fadd fsub fmul fdiv fmax fmin fnmul))
     (Insn:arith op
                 (parse-operand dst)
                 (parse-operand src1)
                 (parse-operand src2))]

    ;; Unary: (neg dst src)
    [`(,op ,dst ,src)
     #:when (memq op '(neg abs mvn fabs fneg fsqrt))
     (Insn:arith2 op
                  (parse-operand dst)
                  (parse-operand src))]

    ;; Move: (mov dst src)
    [`(mov ,dst ,src)
     (Insn:mov 'mov (parse-operand dst) (parse-operand src))]

    ;; Compare: (cmp src1 src2)
    [`(cmp ,src1 ,src2)
     (Insn:cmp 'cmp (parse-operand src1) (parse-operand src2))]

    ;; Load: (ldr dst addr)
    [`(,op ,dst ,addr)
     #:when (memq op '(ldr ldrb ldrh ldrsb ldrsh ldrsw))
     (Insn:load op (parse-operand dst) (parse-operand addr))]

    ;; Store: (str src addr)
    [`(,op ,src ,addr)
     #:when (memq op '(str strb strh))
     (Insn:store op (parse-operand src) (parse-operand addr))]

    ;; Load pair: (ldp dst1 dst2 addr)
    [`(ldp ,dst1 ,dst2 ,addr)
     (Insn:ldp 'ldp
               (parse-operand dst1)
               (parse-operand dst2)
               (parse-operand addr))]

    ;; Store pair: (stp src1 src2 addr)
    [`(stp ,src1 ,src2 ,addr)
     (Insn:stp 'stp
               (parse-operand src1)
               (parse-operand src2)
               (parse-operand addr))]

    ;; Branch: (b target) or (bl target)
    [`(,op ,target)
     #:when (memq op '(b bl))
     (Insn:branch op (parse-operand target))]

    ;; Conditional branch: (b.eq target)
    [`(,(? conditional-branch-op? op) ,target)
     (define cond (extract-condition op))
     (Insn:cond-branch op cond (parse-operand target))]

    ;; Compare and branch: (cbz reg target) (cbnz reg target)
    [`(,op ,reg ,target)
     #:when (memq op '(cbz cbnz tbz tbnz))
     (Insn:cbz op (parse-operand reg) (parse-operand target))]

    ;; Conditional select: (csel dst src1 src2 cond)
    [`(csel ,dst ,src1 ,src2 ,cond)
     (Insn:csel 'csel
                (parse-operand dst)
                (parse-operand src1)
                (parse-operand src2)
                cond)]

    ;; SVE predicated arithmetic: (fadd z0 p0 z1 z2)
    [`(,op ,dst ,pred ,src1 ,src2)
     #:when (or (sve-arith-op? op) (memq op '(sel)))
     (Insn:sve op
               (parse-operand pred)
               (parse-operand dst)
               (list (parse-operand src1) (parse-operand src2)))]

    ;; SVE load: (ld1w z0 p0 addr)
    [`(,op ,dst ,pred ,addr)
     #:when (sve-mem-op? op)
     (cond
       [(memq op '(ld1b ld1h ld1w ld1d ld1sb ld1sh ld1sw
                   ldff1b ldff1h ldff1w ldff1d ldnt1b ldnt1h ldnt1w ldnt1d))
        (Insn:sve-load op (parse-operand pred) (parse-operand dst) (parse-operand addr))]
       [(memq op '(st1b st1h st1w st1d stnt1b stnt1h stnt1w stnt1d))
        (Insn:sve-store op (parse-operand pred) (parse-operand dst) (parse-operand addr))]
       [else (parse-error "unknown SVE memory op" op)])]

    ;; SVE reduction: (faddv s0 p0 z0)
    [`(,op ,dst ,pred ,src)
     #:when (memq op sve-reduce-ops)
     (Insn:sve-reduce op
                      (parse-operand pred)
                      (parse-operand dst)
                      (parse-operand src))]

    ;; SVE whilelt: (whilelt p0 x0 x1)
    [`(whilelt ,pd ,rn ,rm)
     (Insn:whilelt (parse-operand pd)
                   (parse-operand rn)
                   (parse-operand rm))]

    ;; SVE ptrue: (ptrue p0)
    [`(ptrue ,pd)
     (AsmInsn 'ptrue (list (parse-operand pd)))]

    ;; Generic SVE instruction
    [`(,op . ,args)
     #:when (or (sve-op? op) (sve2-op? op))
     (AsmInsn op (map parse-operand args))]

    ;; Generic instruction fallback
    [`(,op . ,args)
     (AsmInsn op (map parse-operand args))]

    [_ (parse-error "unknown instruction" sexp)]))

;; Check if symbol is a conditional branch (b.eq, b.ne, etc.)
(define (conditional-branch-op? sym)
  (define s (symbol->string sym))
  (and (string-prefix? s "b.")
       (condition-code? (string->symbol (substring s 2)))))

;; Extract condition from b.eq -> 'eq
(define (extract-condition sym)
  (define s (symbol->string sym))
  (string->symbol (substring s 2)))

;; ============================================================================
;; Block Parsing
;; ============================================================================

;; Parse a labeled block
;; Input: (block label insn1 insn2 ... terminator)
(define (parse-block sexp cfg)
  (match sexp
    [`(block ,label . ,insns)
     (define-values (block-id cfg1) (cfg-fresh-block-id cfg))
     (define parsed-label (if (symbol? label) (Label:named label) label))
     (define block (make-empty-block block-id parsed-label))

     ;; Parse instructions and terminator
     (define-values (body-insns terminator)
       (split-terminator insns))

     (define block-with-insns
       (for/fold ([b block])
                 ([insn (in-list body-insns)])
         (block-append-insn b (parse-insn insn))))

     (define final-block
       (if terminator
           (block-set-terminator block-with-insns (parse-terminator terminator cfg1))
           block-with-insns))

     (values final-block cfg1)]

    [_ (parse-error "expected (block label ...)" sexp)]))

;; Split instructions from terminator
(define (split-terminator insns)
  (if (null? insns)
      (values '() #f)
      (let ([last-insn (last insns)])
        (if (terminator-syntax? last-insn)
            (values (drop-right insns 1) last-insn)
            (values insns #f)))))

(define (last lst)
  (if (null? (cdr lst)) (car lst) (last (cdr lst))))

(define (drop-right lst n)
  (take lst (- (length lst) n)))

(define (take lst n)
  (if (or (zero? n) (null? lst))
      '()
      (cons (car lst) (take (cdr lst) (sub1 n)))))

;; Check if sexp looks like a terminator
(define (terminator-syntax? sexp)
  (and (list? sexp)
       (not (null? sexp))
       (memq (car sexp) '(ret b bl b.eq b.ne b.lt b.le b.gt b.ge
                          b.hi b.hs b.lo b.ls b.mi b.pl b.vs b.vc b.al))))

;; Parse terminator instruction into Term:* struct
(define (parse-terminator sexp cfg)
  (match sexp
    [`(ret) (Term:ret)]
    [`(b ,target) (Term:jump (find-or-create-block-id target cfg))]
    [`(,(? conditional-branch-op? op) ,target)
     ;; For conditional branches, we need both targets
     ;; This is simplified - real impl would need else target
     (Term:cond (extract-condition op)
                (find-or-create-block-id target cfg)
                #f)]  ; else-target needs to come from next block
    [_ (parse-error "unknown terminator" sexp)]))

;; Find block ID by label or create placeholder
(define (find-or-create-block-id label cfg)
  (Label:named label))  ; Simplified: use named label, resolve later

;; ============================================================================
;; Body Parsing
;; ============================================================================

;; Parse function body (list of blocks or flat instructions)
(define (parse-body body)
  ;; Check if body is block-structured or flat
  (cond
    ;; Block-structured: ((block entry ...) (block loop ...) ...)
    [(and (not (null? body))
          (list? (car body))
          (eq? (caar body) 'block))
     (parse-block-structured-body body)]

    ;; Flat: just a list of instructions
    [else
     (parse-flat-body body)]))

;; Parse flat instruction list into a single block CFG
(define (parse-flat-body insns)
  (define cfg0 (make-empty-cfg))
  (define-values (block-id cfg1) (cfg-fresh-block-id cfg0))
  (define-values (label cfg2) (cfg-fresh-label cfg1))

  (define block (make-empty-block block-id label))

  (define-values (body-insns terminator)
    (split-terminator insns))

  (define block-with-insns
    (for/fold ([b block])
              ([insn (in-list body-insns)])
      (block-append-insn b (parse-insn insn))))

  (define final-block
    (if terminator
        (block-set-terminator block-with-insns (parse-terminator terminator cfg2))
        (block-set-terminator block-with-insns (Term:ret))))

  (cfg-add-block cfg2 final-block #:set-entry? #t))

;; Parse block-structured body into CFG
(define (parse-block-structured-body blocks)
  (define cfg0 (make-empty-cfg))

  (define-values (final-cfg _)
    (for/fold ([cfg cfg0]
               [first? #t])
              ([block-sexp (in-list blocks)])
      (define-values (block cfg1) (parse-block block-sexp cfg))
      (values (cfg-add-block cfg1 block #:set-entry? first?)
              #f)))
  final-cfg)
