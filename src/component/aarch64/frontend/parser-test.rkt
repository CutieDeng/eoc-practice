#lang racket/base

;; Unit Tests for AArch64 S-Expression Parser

(require rackunit
         rackunit/text-ui
         racket/match
         cutie-ftree/pvector
         "../ir/types.rkt"
         "../ir/cfg.rkt"
         "../frontend/parser.rkt")

;; ============================================================================
;; Register Parsing Tests
;; ============================================================================

(define register-tests
  (test-suite
   "Register Parsing"

   (test-case "parse x-registers"
     (check-equal? (parse-reg 'x0) (Reg:x 0))
     (check-equal? (parse-reg 'x15) (Reg:x 15))
     (check-equal? (parse-reg 'x30) (Reg:x 30)))

   (test-case "parse w-registers"
     (check-equal? (parse-reg 'w0) (Reg:w 0))
     (check-equal? (parse-reg 'w10) (Reg:w 10)))

   (test-case "parse z-registers (SVE)"
     (check-equal? (parse-reg 'z0) (Reg:z 0))
     (check-equal? (parse-reg 'z31) (Reg:z 31)))

   (test-case "parse p-registers (predicate)"
     (check-equal? (parse-reg 'p0) (Reg:p 0))
     (check-equal? (parse-reg 'p7) (Reg:p 7)))

   (test-case "parse v-registers"
     (check-equal? (parse-reg 'v0) (Reg:v 0 'q)))

   (test-case "parse special registers"
     (check-equal? (parse-reg 'sp) (Reg:sp))
     (check-equal? (parse-reg 'xzr) (Reg:xzr))
     (check-equal? (parse-reg 'wzr) (Reg:wzr)))))

;; ============================================================================
;; Type Parsing Tests
;; ============================================================================

(define type-tests
  (test-suite
   "Type Parsing"

   (test-case "parse scalar types"
     (check-equal? (parse-type 'i8) (Type:scalar 8 #t))
     (check-equal? (parse-type 'i16) (Type:scalar 16 #t))
     (check-equal? (parse-type 'i32) (Type:scalar 32 #t))
     (check-equal? (parse-type 'i64) (Type:scalar 64 #t))
     (check-equal? (parse-type 'u32) (Type:scalar 32 #f)))

   (test-case "parse float types"
     (check-equal? (parse-type 'f16) (Type:float 16))
     (check-equal? (parse-type 'f32) (Type:float 32))
     (check-equal? (parse-type 'f64) (Type:float 64)))

   (test-case "parse vector types"
     (check-equal? (parse-type '(vec i32 4))
                   (Type:vec (Type:scalar 32 #t) 4))
     (check-equal? (parse-type '(vec f32 4))
                   (Type:vec (Type:float 32) 4)))

   (test-case "parse SVE types"
     (check-equal? (parse-type '(sve f32))
                   (Type:sve (Type:float 32)))
     (check-equal? (parse-type '(sve2 i16))
                   (Type:sve2 (Type:scalar 16 #t))))

   (test-case "parse pointer types"
     (check-equal? (parse-type '(ptr i32))
                   (Type:ptr (Type:scalar 32 #t))))

   (test-case "parse special types"
     (check-equal? (parse-type 'pred) (Type:pred))
     (check-equal? (parse-type 'void) (Type:void)))))

;; ============================================================================
;; Operand Parsing Tests
;; ============================================================================

(define operand-tests
  (test-suite
   "Operand Parsing"

   (test-case "parse register operand"
     (define op (parse-operand 'x5))
     (check-equal? op (Reg:x 5)))

   (test-case "parse immediate operand"
     (check-equal? (parse-operand 42) (Imm 42))
     (check-equal? (parse-operand -10) (Imm -10)))

   (test-case "parse label operand"
     (define op (parse-operand 'loop))
     (check-true (Label:named? op))
     (check-equal? (Label:named-name op) 'loop))

   (test-case "parse memory base"
     (define op (parse-operand '[x0]))
     (check-true (Mem:base? op))
     (check-equal? (Mem:base-reg op) (Reg:x 0)))

   (test-case "parse memory offset"
     (define op (parse-operand '[x1 16]))
     (check-true (Mem:offset? op))
     (check-equal? (Mem:offset-reg op) (Reg:x 1))
     (check-equal? (Mem:offset-offset op) 16))

   (test-case "parse pre-indexed memory"
     (define op (parse-operand '[x2 -32 !]))
     (check-true (Mem:pre? op))
     (check-equal? (Mem:pre-offset op) -32))

   (test-case "parse register offset memory"
     (define op (parse-operand '[x3 x4]))
     (check-true (Mem:reg? op))
     (check-equal? (Mem:reg-base op) (Reg:x 3))
     (check-equal? (Mem:reg-index op) (Reg:x 4)))

   (test-case "parse shifted immediate"
     (define op (parse-operand '(lsl 1 12)))
     (check-true (Imm:shifted? op))
     (check-equal? (Imm:shifted-value op) 1)
     (check-equal? (Imm:shifted-shift op) 12))))

;; ============================================================================
;; Instruction Parsing Tests
;; ============================================================================

(define insn-tests
  (test-suite
   "Instruction Parsing"

   (test-case "parse add instruction"
     (define insn (parse-insn '(add x0 x1 x2)))
     (check-true (Insn:arith? insn))
     (check-equal? (Insn:arith-op insn) 'add)
     (check-equal? (Insn:arith-dst insn) (Reg:x 0)))

   (test-case "parse add with immediate"
     (define insn (parse-insn '(add x0 x1 42)))
     (check-true (Insn:arith? insn))
     (check-equal? (Insn:arith-src2 insn) (Imm 42)))

   (test-case "parse sub instruction"
     (define insn (parse-insn '(sub x3 x4 x5)))
     (check-true (Insn:arith? insn))
     (check-equal? (Insn:arith-op insn) 'sub))

   (test-case "parse mul instruction"
     (define insn (parse-insn '(mul x0 x1 x2)))
     (check-equal? (Insn:arith-op insn) 'mul))

   (test-case "parse logical instructions"
     (check-equal? (Insn:arith-op (parse-insn '(and x0 x1 x2))) 'and)
     (check-equal? (Insn:arith-op (parse-insn '(orr x0 x1 x2))) 'orr)
     (check-equal? (Insn:arith-op (parse-insn '(eor x0 x1 x2))) 'eor))

   (test-case "parse neg instruction"
     (define insn (parse-insn '(neg x0 x1)))
     (check-true (Insn:arith2? insn))
     (check-equal? (Insn:arith2-op insn) 'neg))

   (test-case "parse mov instruction"
     (define insn (parse-insn '(mov x0 x1)))
     (check-true (Insn:mov? insn)))

   (test-case "parse cmp instruction"
     (define insn (parse-insn '(cmp x0 x1)))
     (check-true (Insn:cmp? insn)))

   (test-case "parse ldr instruction"
     (define insn (parse-insn '(ldr x0 [x1])))
     (check-true (Insn:load? insn))
     (check-equal? (Insn:load-op insn) 'ldr))

   (test-case "parse str instruction"
     (define insn (parse-insn '(str x0 [x1 8])))
     (check-true (Insn:store? insn))
     (check-equal? (Insn:store-op insn) 'str))

   (test-case "parse ldp instruction"
     (define insn (parse-insn '(ldp x0 x1 [x2])))
     (check-true (Insn:ldp? insn)))

   (test-case "parse stp instruction"
     (define insn (parse-insn '(stp x0 x1 [x2 16])))
     (check-true (Insn:stp? insn)))

   (test-case "parse branch instruction"
     (define insn (parse-insn '(b target)))
     (check-true (Insn:branch? insn))
     (check-equal? (Insn:branch-op insn) 'b))

   (test-case "parse conditional branch"
     (define insn (parse-insn '(b.eq target)))
     (check-true (Insn:cond-branch? insn))
     (check-equal? (Insn:cond-branch-cond insn) 'eq))

   (test-case "parse csel instruction"
     (define insn (parse-insn '(csel x0 x1 x2 eq)))
     (check-true (Insn:csel? insn))
     (check-equal? (Insn:csel-cond insn) 'eq))

   (test-case "parse ret instruction"
     (define insn (parse-insn '(ret)))
     (check-true (Insn:ret? insn)))

   (test-case "parse SVE fadd instruction"
     (define insn (parse-insn '(fadd z0 p0 z1 z2)))
     (check-true (Insn:sve? insn))
     (check-equal? (Insn:sve-op insn) 'fadd))

   (test-case "parse SVE ld1w instruction"
     (define insn (parse-insn '(ld1w z0 p0 [x0])))
     (check-true (Insn:sve-load? insn))
     (check-equal? (Insn:sve-load-op insn) 'ld1w))

   (test-case "parse whilelt instruction"
     (define insn (parse-insn '(whilelt p0 x0 x1)))
     (check-true (Insn:whilelt? insn)))))

;; ============================================================================
;; Parameter Parsing Tests
;; ============================================================================

(define param-tests
  (test-suite
   "Parameter Parsing"

   (test-case "parse single parameter"
     (define params (parse-params '([x0 : i64])))
     (check-equal? (length params) 1)
     (define p (car params))
     (check-true (AsmParam? p))
     (check-equal? (AsmParam-reg p) (Reg:x 0))
     (check-equal? (AsmParam-type p) (Type:scalar 64 #t)))

   (test-case "parse multiple parameters"
     (define params (parse-params '([x0 : i64]
                                    [x1 : (ptr f32)]
                                    [z0 : (sve f32)]
                                    [p0 : pred])))
     (check-equal? (length params) 4)

     (check-equal? (AsmParam-reg (list-ref params 0)) (Reg:x 0))
     (check-equal? (AsmParam-reg (list-ref params 1)) (Reg:x 1))
     (check-equal? (AsmParam-reg (list-ref params 2)) (Reg:z 0))
     (check-equal? (AsmParam-reg (list-ref params 3)) (Reg:p 0)))))

;; ============================================================================
;; Function Parsing Tests
;; ============================================================================

(define function-tests
  (test-suite
   "Function Parsing"

   (test-case "parse simple function"
     (define fn (parse-asm-fn
                 '(asm-fn add-one
                    ([x0 : i64])
                    -> i64
                    (add x0 x0 1)
                    (ret))))

     (check-true (AsmFunction? fn))
     (check-equal? (AsmFunction-name fn) 'add-one)
     (check-equal? (length (AsmFunction-params fn)) 1)
     (check-true (Type:scalar? (AsmFunction-ret-type fn))))

   (test-case "parse function with multiple params"
     (define fn (parse-asm-fn
                 '(asm-fn sum-two
                    ([x0 : i64] [x1 : i64])
                    -> i64
                    (add x0 x0 x1)
                    (ret))))

     (check-equal? (length (AsmFunction-params fn)) 2))

   (test-case "parse function with SVE params"
     (define fn (parse-asm-fn
                 '(asm-fn vec-add
                    ([x0 : (ptr f32)]
                     [x1 : (ptr f32)]
                     [x2 : i64])
                    -> void
                    (ret))))

     (check-equal? (length (AsmFunction-params fn)) 3)
     (check-true (Type:void? (AsmFunction-ret-type fn))))))

;; ============================================================================
;; Body Parsing Tests
;; ============================================================================

(define body-tests
  (test-suite
   "Body Parsing"

   (test-case "parse flat body"
     (define body (parse-body
                   '((mov x0 42)
                     (add x0 x0 1)
                     (ret))))

     (check-true (AsmCfg? body))
     (check-equal? (cfg-block-count body) 1))

   (test-case "parse block-structured body"
     (define body (parse-body
                   '((block entry
                       (mov x0 0)
                       (b loop))
                     (block loop
                       (add x0 x0 1)
                       (ret)))))

     (check-true (AsmCfg? body))
     (check-equal? (cfg-block-count body) 2))))

;; ============================================================================
;; Error Handling Tests
;; ============================================================================

(define error-tests
  (test-suite
   "Error Handling"

   (test-case "invalid register raises error"
     (check-exn ParseError?
                (λ () (parse-reg 'invalid))))

   (test-case "invalid type raises error"
     (check-exn ParseError?
                (λ () (parse-type 'unknown-type))))

   (test-case "malformed parameter raises error"
     (check-exn ParseError?
                (λ () (parse-params '([x0 i64])))))  ; Missing :

   (test-case "invalid function syntax raises error"
     (check-exn ParseError?
                (λ () (parse-asm-fn '(asm-fn no-arrow)))))))

;; ============================================================================
;; Run All Tests
;; ============================================================================

(define all-tests
  (test-suite
   "AArch64 Parser"
   register-tests
   type-tests
   operand-tests
   insn-tests
   param-tests
   function-tests
   body-tests
   error-tests))

(module+ main
  (run-tests all-tests))

(module+ test
  (run-tests all-tests))
