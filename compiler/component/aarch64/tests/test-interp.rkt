#lang racket/base

;; Unit Tests for AArch64 Interpreter

(require rackunit
         rackunit/text-ui
         racket/class
         racket/match
         "../../../../cutie-ftree/pvector.rkt"
         "../ir/types.rkt"
         "../ir/cfg.rkt"
         "../ir/config.rkt"
         "../interp/state.rkt"
         "../interp/base.rkt")

;; ============================================================================
;; State Tests
;; ============================================================================

(define state-tests
  (test-suite
   "Machine State"

   (test-case "initial state creation"
     (define s (make-empty-state))
     (check-true (MachineState? s))
     (check-equal? (state-read-x s 0) 0)
     (check-equal? (state-read-sp s) #xFFFF0000))

   (test-case "x-register read/write"
     (define s0 (make-empty-state))
     (define s1 (state-write-x s0 5 42))

     (check-equal? (state-read-x s1 5) 42)
     ;; Original unchanged
     (check-equal? (state-read-x s0 5) 0))

   (test-case "w-register read/write"
     (define s0 (make-empty-state))
     ;; Write 64-bit value to x-register
     (define s1 (state-write-x s0 3 #xFFFFFFFF12345678))
     ;; Read as w-register (lower 32 bits)
     (check-equal? (state-read-w s1 3) #x12345678)

     ;; Write to w-register zero-extends
     (define s2 (state-write-w s1 3 #xABCDEF00))
     (check-equal? (state-read-x s2 3) #xABCDEF00))

   (test-case "z-register read/write"
     (define config config/sve-256)
     (define s0 (make-initial-state config))
     (define vec (make-sve-zero 32))  ; 256 bits = 32 bytes

     (define s1 (state-write-z s0 0 vec))
     (check-equal? (state-read-z s1 0) vec))

   (test-case "p-register read/write"
     (define s0 (make-empty-state))
     (define s1 (state-write-p s0 0 #b11110000))

     (check-equal? (state-read-p s1 0) #b11110000)
     (check-true (pred-active? #b11110000 4))
     (check-false (pred-active? #b11110000 0)))

   (test-case "generic register access"
     (define s0 (make-empty-state))
     (define s1 (state-write-reg s0 (Reg:x 7) 100))
     (check-equal? (state-read-reg s1 (Reg:x 7)) 100)

     ;; Zero registers always return 0
     (check-equal? (state-read-reg s1 (Reg:xzr)) 0)
     (check-equal? (state-read-reg s1 (Reg:wzr)) 0)

     ;; Writes to zero registers are discarded
     (define s2 (state-write-reg s1 (Reg:xzr) 999))
     (check-equal? (state-read-reg s2 (Reg:xzr)) 0))

   (test-case "memory read/write byte"
     (define s0 (make-empty-state))
     (define s1 (state-write-byte s0 #x1000 #xAB))
     (check-equal? (state-read-byte s1 #x1000) #xAB)
     (check-equal? (state-read-byte s1 #x1001) 0))  ; Uninitialized

   (test-case "memory read/write word"
     (define s0 (make-empty-state))
     (define s1 (state-write-word s0 #x2000 #x12345678))
     (check-equal? (state-read-word s1 #x2000) #x12345678)
     ;; Check byte order (little endian)
     (check-equal? (state-read-byte s1 #x2000) #x78)
     (check-equal? (state-read-byte s1 #x2001) #x56)
     (check-equal? (state-read-byte s1 #x2002) #x34)
     (check-equal? (state-read-byte s1 #x2003) #x12))

   (test-case "memory read/write doubleword"
     (define s0 (make-empty-state))
     (define s1 (state-write-doubleword s0 #x3000 #x123456789ABCDEF0))
     (check-equal? (state-read-doubleword s1 #x3000) #x123456789ABCDEF0))

   (test-case "flags operations"
     (define s0 (make-empty-state))

     ;; Set Z flag
     (define s1 (state-set-flag s0 flag-z))
     (check-true (state-test-flag s1 flag-z))
     (check-false (state-test-flag s1 flag-n))

     ;; Clear Z flag
     (define s2 (state-clear-flag s1 flag-z))
     (check-false (state-test-flag s2 flag-z)))

   (test-case "predicate operations"
     (check-true (pred-active? #b1010 1))
     (check-false (pred-active? #b1010 0))
     (check-true (pred-active? #b1010 3))
     (check-false (pred-active? #b1010 2))

     (check-equal? (pred-set-lane 0 3) #b1000)
     (check-equal? (pred-clear-lane #b1111 2) #b1011)
     (check-equal? (pred-all-true 4) #b1111)
     (check-equal? (pred-all-false) 0))))

;; ============================================================================
;; Interpreter Arithmetic Tests
;; ============================================================================

(define arith-tests
  (test-suite
   "Arithmetic Instructions"

   (test-case "add instruction"
     (define interp (new interp-aarch64-base%))
     (send interp write-x 1 100)
     (send interp write-x 2 50)

     (send interp exec-insn
           (Insn:arith 'add (Reg:x 0) (Reg:x 1) (Reg:x 2)))

     (check-equal? (send interp read-x 0) 150))

   (test-case "add with immediate"
     (define interp (new interp-aarch64-base%))
     (send interp write-x 1 100)

     (send interp exec-insn
           (Insn:arith 'add (Reg:x 0) (Reg:x 1) (Imm 25)))

     (check-equal? (send interp read-x 0) 125))

   (test-case "sub instruction"
     (define interp (new interp-aarch64-base%))
     (send interp write-x 1 100)
     (send interp write-x 2 30)

     (send interp exec-insn
           (Insn:arith 'sub (Reg:x 0) (Reg:x 1) (Reg:x 2)))

     (check-equal? (send interp read-x 0) 70))

   (test-case "mul instruction"
     (define interp (new interp-aarch64-base%))
     (send interp write-x 1 7)
     (send interp write-x 2 6)

     (send interp exec-insn
           (Insn:arith 'mul (Reg:x 0) (Reg:x 1) (Reg:x 2)))

     (check-equal? (send interp read-x 0) 42))

   (test-case "and instruction"
     (define interp (new interp-aarch64-base%))
     (send interp write-x 1 #xFF00)
     (send interp write-x 2 #x0FF0)

     (send interp exec-insn
           (Insn:arith 'and (Reg:x 0) (Reg:x 1) (Reg:x 2)))

     (check-equal? (send interp read-x 0) #x0F00))

   (test-case "orr instruction"
     (define interp (new interp-aarch64-base%))
     (send interp write-x 1 #xFF00)
     (send interp write-x 2 #x00FF)

     (send interp exec-insn
           (Insn:arith 'orr (Reg:x 0) (Reg:x 1) (Reg:x 2)))

     (check-equal? (send interp read-x 0) #xFFFF))

   (test-case "eor instruction"
     (define interp (new interp-aarch64-base%))
     (send interp write-x 1 #xFFFF)
     (send interp write-x 2 #x0F0F)

     (send interp exec-insn
           (Insn:arith 'eor (Reg:x 0) (Reg:x 1) (Reg:x 2)))

     (check-equal? (send interp read-x 0) #xF0F0))

   (test-case "w-register arithmetic (32-bit)"
     (define interp (new interp-aarch64-base%))
     (send interp write-x 1 #xFFFFFFFF00000064)  ; 100 in low 32 bits
     (send interp write-x 2 50)

     ;; Using w-registers should use 32-bit values
     (send interp exec-insn
           (Insn:arith 'add (Reg:w 0) (Reg:w 1) (Reg:w 2)))

     ;; Result is zero-extended to 64 bits
     (check-equal? (send interp read-x 0) 150))))

;; ============================================================================
;; Interpreter Move/Compare Tests
;; ============================================================================

(define move-tests
  (test-suite
   "Move and Compare Instructions"

   (test-case "mov register to register"
     (define interp (new interp-aarch64-base%))
     (send interp write-x 1 42)

     (send interp exec-insn
           (Insn:mov 'mov (Reg:x 0) (Reg:x 1)))

     (check-equal? (send interp read-x 0) 42))

   (test-case "mov immediate"
     (define interp (new interp-aarch64-base%))

     (send interp exec-insn
           (Insn:mov 'mov (Reg:x 0) (Imm 999)))

     (check-equal? (send interp read-x 0) 999))

   (test-case "cmp and condition codes"
     (define interp (new interp-aarch64-base%))
     (send interp write-x 1 100)
     (send interp write-x 2 100)

     (send interp exec-insn
           (Insn:cmp 'cmp (Reg:x 1) (Reg:x 2)))

     ;; Equal: Z=1
     (check-true (send interp test-condition 'eq))
     (check-false (send interp test-condition 'ne)))

   (test-case "cmp less than"
     (define interp (new interp-aarch64-base%))
     (send interp write-x 1 50)
     (send interp write-x 2 100)

     (send interp exec-insn
           (Insn:cmp 'cmp (Reg:x 1) (Reg:x 2)))

     (check-true (send interp test-condition 'lt))
     (check-false (send interp test-condition 'ge)))

   (test-case "cmp greater than"
     (define interp (new interp-aarch64-base%))
     (send interp write-x 1 100)
     (send interp write-x 2 50)

     (send interp exec-insn
           (Insn:cmp 'cmp (Reg:x 1) (Reg:x 2)))

     (check-true (send interp test-condition 'gt))
     (check-true (send interp test-condition 'ge))
     (check-false (send interp test-condition 'le)))

   (test-case "csel instruction"
     (define interp (new interp-aarch64-base%))
     (send interp write-x 1 10)
     (send interp write-x 2 20)

     ;; Set up Z=1 (equal condition true)
     (send interp exec-insn
           (Insn:cmp 'cmp (Reg:x 3) (Reg:x 3)))  ; x3 == x3

     (send interp exec-insn
           (Insn:csel 'csel (Reg:x 0) (Reg:x 1) (Reg:x 2) 'eq))

     (check-equal? (send interp read-x 0) 10)  ; src1 selected

     ;; Now with ne condition (false since Z=1)
     (send interp exec-insn
           (Insn:csel 'csel (Reg:x 0) (Reg:x 1) (Reg:x 2) 'ne))

     (check-equal? (send interp read-x 0) 20))))  ; src2 selected

;; ============================================================================
;; Interpreter Load/Store Tests
;; ============================================================================

(define memory-tests
  (test-suite
   "Load/Store Instructions"

   (test-case "str and ldr"
     (define interp (new interp-aarch64-base%))

     ;; Set up base address in x1
     (send interp write-x 1 #x10000)
     ;; Value to store in x0
     (send interp write-x 0 #x123456789ABCDEF0)

     ;; Store
     (send interp exec-insn
           (Insn:store 'str (Reg:x 0) (Mem:base (Reg:x 1))))

     ;; Load into x2
     (send interp exec-insn
           (Insn:load 'ldr (Reg:x 2) (Mem:base (Reg:x 1))))

     (check-equal? (send interp read-x 2) #x123456789ABCDEF0))

   (test-case "ldr with offset"
     (define interp (new interp-aarch64-base%))
     (send interp write-x 1 #x10000)
     (send interp write-x 0 #xDEADBEEF)

     ;; Store at offset 16
     (send interp exec-insn
           (Insn:store 'str (Reg:x 0) (Mem:offset (Reg:x 1) 16)))

     ;; Load from offset 16
     (send interp exec-insn
           (Insn:load 'ldr (Reg:x 2) (Mem:offset (Reg:x 1) 16)))

     (check-equal? (send interp read-x 2) #xDEADBEEF))

   (test-case "ldrw (32-bit load)"
     (define interp (new interp-aarch64-base%))
     (define s (send interp get-state))
     (define s1 (state-write-word s #x10000 #x12345678))
     (send interp set-state! s1)

     (send interp write-x 1 #x10000)
     (send interp exec-insn
           (Insn:load 'ldrw (Reg:x 0) (Mem:base (Reg:x 1))))

     (check-equal? (send interp read-x 0) #x12345678))

   (test-case "post-indexed load"
     (define interp (new interp-aarch64-base%))
     (define s (send interp get-state))
     (define s1 (state-write-doubleword s #x10000 #xCAFEBABE))
     (send interp set-state! s1)

     (send interp write-x 1 #x10000)
     (send interp exec-insn
           (Insn:load 'ldr (Reg:x 0) (Mem:post (Reg:x 1) 8)))

     ;; Value loaded
     (check-equal? (send interp read-x 0) #xCAFEBABE)
     ;; Base updated
     (check-equal? (send interp read-x 1) #x10008))

   (test-case "pre-indexed store"
     (define interp (new interp-aarch64-base%))
     (send interp write-x 0 #x12345678)
     (send interp write-x 1 #x10010)  ; Base

     (send interp exec-insn
           (Insn:store 'str (Reg:x 0) (Mem:pre (Reg:x 1) -16)))

     ;; Base should be updated to 0x10000
     (check-equal? (send interp read-x 1) #x10000)

     ;; Value stored at new address
     (define s (send interp get-state))
     (check-equal? (state-read-doubleword s #x10000) #x12345678))))

;; ============================================================================
;; Interpreter CFG Execution Tests
;; ============================================================================

(define cfg-exec-tests
  (test-suite
   "CFG Execution"

   (test-case "simple linear block"
     (define cfg0 (make-empty-cfg))
     (define-values (bid cfg1) (cfg-fresh-block-id cfg0))

     (define block
       (block-set-terminator
        (block-append-insns
         (make-empty-block bid (Label:named 'entry))
         (list (Insn:mov 'mov (Reg:x 0) (Imm 42))))
        (Term:ret)))

     (define cfg2 (cfg-add-block cfg1 block #:set-entry? #t))

     (define result (run-cfg cfg2))
     (check-true (InterpResult-halted? result))
     (check-equal? (InterpResult-value result) 42))

   (test-case "two blocks with jump"
     (define cfg0 (make-empty-cfg))
     (define-values (bid1 cfg1) (cfg-fresh-block-id cfg0))
     (define-values (bid2 cfg2) (cfg-fresh-block-id cfg1))

     (define block1
       (block-set-terminator
        (block-append-insns
         (make-empty-block bid1 (Label:named 'entry))
         (list (Insn:mov 'mov (Reg:x 0) (Imm 10))))
        (Term:jump bid2)))

     (define block2
       (block-set-terminator
        (block-append-insns
         (make-empty-block bid2 (Label:named 'add-more))
         (list (Insn:arith 'add (Reg:x 0) (Reg:x 0) (Imm 32))))
        (Term:ret)))

     (define cfg3 (cfg-add-block cfg2 block1 #:set-entry? #t))
     (define cfg4 (cfg-add-block cfg3 block2))

     (define result (run-cfg cfg4))
     (check-equal? (InterpResult-value result) 42))  ; 10 + 32

   (test-case "conditional branch"
     (define cfg0 (make-empty-cfg))
     (define-values (bid1 cfg1) (cfg-fresh-block-id cfg0))
     (define-values (bid2 cfg2) (cfg-fresh-block-id cfg1))
     (define-values (bid3 cfg3) (cfg-fresh-block-id cfg2))

     ;; Entry: compare x1 with 0, branch to bid2 if equal, else bid3
     (define block1
       (block-set-terminator
        (block-append-insns
         (make-empty-block bid1 (Label:named 'entry))
         (list (Insn:cmp 'cmp (Reg:x 1) (Imm 0))))
        (Term:cond 'eq bid2 bid3)))

     ;; If equal: x0 = 100
     (define block2
       (block-set-terminator
        (block-append-insns
         (make-empty-block bid2 (Label:named 'then))
         (list (Insn:mov 'mov (Reg:x 0) (Imm 100))))
        (Term:ret)))

     ;; If not equal: x0 = 200
     (define block3
       (block-set-terminator
        (block-append-insns
         (make-empty-block bid3 (Label:named 'else))
         (list (Insn:mov 'mov (Reg:x 0) (Imm 200))))
        (Term:ret)))

     (define cfg4 (cfg-add-block cfg3 block1 #:set-entry? #t))
     (define cfg5 (cfg-add-block cfg4 block2))
     (define cfg6 (cfg-add-block cfg5 block3))

     ;; Test with x1 = 0 (should take then branch)
     (define interp1 (new interp-aarch64-base%))
     (send interp1 write-x 1 0)
     (define result1 (send interp1 run-cfg cfg6))
     (check-equal? (InterpResult-value result1) 100)

     ;; Test with x1 = 5 (should take else branch)
     (define interp2 (new interp-aarch64-base%))
     (send interp2 write-x 1 5)
     (define result2 (send interp2 run-cfg cfg6))
     (check-equal? (InterpResult-value result2) 200))

   (test-case "simple loop"
     ;; Sum 1 to 5
     (define cfg0 (make-empty-cfg))
     (define-values (entry-id cfg1) (cfg-fresh-block-id cfg0))
     (define-values (loop-id cfg2) (cfg-fresh-block-id cfg1))
     (define-values (exit-id cfg3) (cfg-fresh-block-id cfg2))

     ;; Entry: x0 = 0 (sum), x1 = 5 (counter)
     (define entry-block
       (block-set-terminator
        (block-append-insns
         (make-empty-block entry-id (Label:named 'entry))
         (list (Insn:mov 'mov (Reg:x 0) (Imm 0))    ; sum = 0
               (Insn:mov 'mov (Reg:x 1) (Imm 5))))  ; n = 5
        (Term:jump loop-id)))

     ;; Loop: sum += n; n--; if n > 0 goto loop else exit
     (define loop-block
       (block-set-terminator
        (block-append-insns
         (make-empty-block loop-id (Label:named 'loop))
         (list (Insn:arith 'add (Reg:x 0) (Reg:x 0) (Reg:x 1))  ; sum += n
               (Insn:arith 'sub (Reg:x 1) (Reg:x 1) (Imm 1))    ; n--
               (Insn:cmp 'cmp (Reg:x 1) (Imm 0))))               ; compare n, 0
        (Term:cond 'gt loop-id exit-id)))

     ;; Exit: return
     (define exit-block
       (block-set-terminator
        (make-empty-block exit-id (Label:named 'exit))
        (Term:ret)))

     (define cfg4 (cfg-add-block cfg3 entry-block #:set-entry? #t))
     (define cfg5 (cfg-add-block cfg4 loop-block))
     (define cfg6 (cfg-add-block cfg5 exit-block))

     (define result (run-cfg cfg6))
     (check-equal? (InterpResult-value result) 15))))  ; 5+4+3+2+1 = 15

;; ============================================================================
;; Run All Tests
;; ============================================================================

(define all-tests
  (test-suite
   "AArch64 Interpreter"
   state-tests
   arith-tests
   move-tests
   memory-tests
   cfg-exec-tests))

(module+ main
  (run-tests all-tests))

(module+ test
  (run-tests all-tests))
