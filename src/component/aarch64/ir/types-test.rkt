#lang racket/base

;; Unit Tests for AArch64 IR Types

(require rackunit
         rackunit/text-ui
         racket/match
         "../ir/types.rkt")

;; ============================================================================
;; Register Tests
;; ============================================================================

(define register-tests
  (test-suite
   "Register Types"

   (test-case "x-register creation and access"
     (define r (Reg:x 5))
     (check-true (Reg:x? r))
     (check-equal? (Reg:x-id r) 5)
     (check-false (Reg:w? r)))

   (test-case "w-register creation and access"
     (define r (Reg:w 10))
     (check-true (Reg:w? r))
     (check-equal? (Reg:w-id r) 10))

   (test-case "z-register (SVE) creation"
     (define r (Reg:z 15))
     (check-true (Reg:z? r))
     (check-true (sve-reg? r))
     (check-equal? (Reg:z-id r) 15))

   (test-case "p-register (predicate) creation"
     (define r (Reg:p 3))
     (check-true (Reg:p? r))
     (check-true (pred-reg? r))
     (check-equal? (Reg:p-id r) 3))

   (test-case "v-register with width"
     (define r (Reg:v 0 'q))
     (check-true (Reg:v? r))
     (check-true (simd-reg? r))
     (check-equal? (Reg:v-id r) 0)
     (check-equal? (Reg:v-width r) 'q))

   (test-case "special registers"
     (check-true (Reg:sp? (Reg:sp)))
     (check-true (Reg:xzr? (Reg:xzr)))
     (check-true (Reg:wzr? (Reg:wzr))))

   (test-case "register predicates"
     (check-true (gpr? (Reg:x 0)))
     (check-true (gpr? (Reg:w 0)))
     (check-true (gpr? (Reg:sp)))
     (check-false (gpr? (Reg:z 0)))

     (check-true (simd-reg? (Reg:v 0 'q)))
     (check-true (simd-reg? (Reg:d 0)))
     (check-false (simd-reg? (Reg:x 0)))

     (check-true (sve-reg? (Reg:z 0)))
     (check-false (sve-reg? (Reg:v 0 'q)))

     (check-true (any-reg? (Reg:x 0)))
     (check-true (any-reg? (Reg:z 0)))
     (check-true (any-reg? (Reg:p 0))))))

;; ============================================================================
;; Operand Tests
;; ============================================================================

(define operand-tests
  (test-suite
   "Operand Types"

   (test-case "immediate values"
     (define imm (Imm 42))
     (check-true (Imm? imm))
     (check-equal? (Imm-value imm) 42)

     (define shifted (Imm:shifted 1 12))
     (check-true (Imm:shifted? shifted))
     (check-equal? (Imm:shifted-value shifted) 1)
     (check-equal? (Imm:shifted-shift shifted) 12))

   (test-case "memory addressing modes"
     (define base (Mem:base (Reg:x 0)))
     (check-true (Mem:base? base))
     (check-true (mem-addr? base))

     (define offset (Mem:offset (Reg:x 1) 16))
     (check-true (Mem:offset? offset))
     (check-equal? (Mem:offset-offset offset) 16)

     (define pre (Mem:pre (Reg:x 2) -32))
     (check-true (Mem:pre? pre))

     (define post (Mem:post (Reg:x 3) 8))
     (check-true (Mem:post? post)))

   (test-case "labels"
     (define id-label (Label:id 5))
     (check-true (Label:id? id-label))
     (check-true (label? id-label))

     (define named (Label:named 'loop))
     (check-true (Label:named? named))
     (check-equal? (Label:named-name named) 'loop))

   (test-case "operand predicate"
     (check-true (operand? (Reg:x 0)))
     (check-true (operand? (Imm 100)))
     (check-true (operand? (Mem:base (Reg:x 0))))
     (check-true (operand? (Label:named 'foo))))))

;; ============================================================================
;; Instruction Tests
;; ============================================================================

(define instruction-tests
  (test-suite
   "Instruction Types"

   (test-case "arithmetic instruction"
     (define add (Insn:arith 'add (Reg:x 0) (Reg:x 1) (Reg:x 2)))
     (check-true (Insn:arith? add))
     (check-true (insn? add))
     (check-equal? (Insn:arith-op add) 'add)
     (check-true (Reg:x? (Insn:arith-dst add))))

   (test-case "unary instruction"
     (define neg (Insn:arith2 'neg (Reg:x 0) (Reg:x 1)))
     (check-true (Insn:arith2? neg))
     (check-equal? (Insn:arith2-op neg) 'neg))

   (test-case "load instruction"
     (define ldr (Insn:load 'ldr (Reg:x 0) (Mem:offset (Reg:x 1) 8)))
     (check-true (Insn:load? ldr))
     (check-equal? (Insn:load-op ldr) 'ldr))

   (test-case "store instruction"
     (define str (Insn:store 'str (Reg:x 0) (Mem:base (Reg:x 1))))
     (check-true (Insn:store? str))
     (check-equal? (Insn:store-op str) 'str))

   (test-case "branch instructions"
     (define b (Insn:branch 'b (Label:named 'target)))
     (check-true (Insn:branch? b))

     (define beq (Insn:cond-branch 'b.eq 'eq (Label:named 'target)))
     (check-true (Insn:cond-branch? beq))
     (check-equal? (Insn:cond-branch-cond beq) 'eq))

   (test-case "SVE instruction"
     (define fadd (Insn:sve 'fadd (Reg:p 0) (Reg:z 0)
                           (list (Reg:z 1) (Reg:z 2))))
     (check-true (Insn:sve? fadd))
     (check-equal? (Insn:sve-op fadd) 'fadd)
     (check-true (Reg:p? (Insn:sve-pred fadd))))

   (test-case "return instruction"
     (define ret (Insn:ret))
     (check-true (Insn:ret? ret))
     (check-true (insn? ret)))))

;; ============================================================================
;; Type Annotation Tests
;; ============================================================================

(define type-tests
  (test-suite
   "Type Annotations"

   (test-case "scalar types"
     (define i32 (Type:scalar 32 #t))
     (check-true (Type:scalar? i32))
     (check-equal? (Type:scalar-width i32) 32)
     (check-true (Type:scalar-signed? i32))

     (define u64 (Type:scalar 64 #f))
     (check-false (Type:scalar-signed? u64)))

   (test-case "float types"
     (define f32 (Type:float 32))
     (check-true (Type:float? f32))
     (check-equal? (Type:float-width f32) 32))

   (test-case "vector types"
     (define vec4 (Type:vec (Type:float 32) 4))
     (check-true (Type:vec? vec4))
     (check-equal? (Type:vec-lanes vec4) 4))

   (test-case "SVE types"
     (define sve-f32 (Type:sve (Type:float 32)))
     (check-true (Type:sve? sve-f32))

     (define sve2-i16 (Type:sve2 (Type:scalar 16 #t)))
     (check-true (Type:sve2? sve2-i16)))

   (test-case "pointer types"
     (define ptr (Type:ptr (Type:scalar 32 #t)))
     (check-true (Type:ptr? ptr)))

   (test-case "type predicate"
     (check-true (asm-type? (Type:scalar 64 #t)))
     (check-true (asm-type? (Type:sve (Type:float 32))))
     (check-true (asm-type? (Type:pred)))
     (check-true (asm-type? (Type:void))))))

;; ============================================================================
;; Function Definition Tests
;; ============================================================================

(define function-tests
  (test-suite
   "Function Definitions"

   (test-case "function parameter"
     (define param (AsmParam (Reg:x 0) (Type:scalar 64 #t)))
     (check-true (AsmParam? param))
     (check-true (Reg:x? (AsmParam-reg param))))

   (test-case "function definition"
     (define fn (AsmFunction
                 'test-fn
                 (list (AsmParam (Reg:x 0) (Type:scalar 64 #t)))
                 (Type:scalar 64 #t)
                 '()))  ; Empty body for test
     (check-true (AsmFunction? fn))
     (check-equal? (AsmFunction-name fn) 'test-fn)
     (check-equal? (length (AsmFunction-params fn)) 1))))

;; ============================================================================
;; Condition Code Tests
;; ============================================================================

(define condition-tests
  (test-suite
   "Condition Codes"

   (test-case "valid condition codes"
     (check-true (condition-code? 'eq))
     (check-true (condition-code? 'ne))
     (check-true (condition-code? 'lt))
     (check-true (condition-code? 'ge))
     (check-true (condition-code? 'hi))
     (check-true (condition-code? 'al)))

   (test-case "invalid condition codes"
     (check-false (condition-code? 'foo))
     (check-false (condition-code? 'bar)))))

;; ============================================================================
;; Run All Tests
;; ============================================================================

(define all-tests
  (test-suite
   "AArch64 IR Types"
   register-tests
   operand-tests
   instruction-tests
   type-tests
   function-tests
   condition-tests))

(module+ main
  (run-tests all-tests))

(module+ test
  (run-tests all-tests))
