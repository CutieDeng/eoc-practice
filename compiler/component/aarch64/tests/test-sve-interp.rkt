#lang racket/base

;; Unit Tests for AArch64 SVE Interpreter

(require rackunit
         rackunit/text-ui
         racket/class
         racket/match
         "../../../../cutie-ftree/pvector.rkt"
         "../ir/types.rkt"
         "../ir/cfg.rkt"
         "../ir/config.rkt"
         "../interp/state.rkt"
         "../interp/base.rkt"
         "../interp/sve.rkt")

;; ============================================================================
;; SVE Predicate Tests
;; ============================================================================

(define sve-pred-tests
  (test-suite
   "SVE Predicate Operations"

   (test-case "whilelt generates correct predicate"
     (define config config/sve)
     (define interp (make-sve-interp #:config config))

     ;; Set up: n=0, m=3 (should activate first 3 lanes)
     (send interp write-x 0 0)  ; n
     (send interp write-x 1 3)  ; m

     (send interp exec-insn
           (Insn:whilelt (Reg:p 0) (Reg:x 0) (Reg:x 1)))

     (define s (send interp get-state))
     (define p-mask (state-read-p s 0))
     ;; With VL=128 bits, 4 elements of 32-bit each
     ;; First 3 lanes active: 0b0111 = 7
     (check-equal? p-mask #b0111))

   (test-case "whilelt with larger offset"
     (define config config/sve)
     (define interp (make-sve-interp #:config config))

     ;; n=2, m=5 → lanes 0,1,2 active (2+0<5, 2+1<5, 2+2<5, 2+3>=5)
     (send interp write-x 0 2)
     (send interp write-x 1 5)

     (send interp exec-insn
           (Insn:whilelt (Reg:p 0) (Reg:x 0) (Reg:x 1)))

     (define s (send interp get-state))
     (check-equal? (state-read-p s 0) #b0111))

   (test-case "whilelt all lanes inactive"
     (define config config/sve)
     (define interp (make-sve-interp #:config config))

     ;; n=10, m=5 → no lanes active
     (send interp write-x 0 10)
     (send interp write-x 1 5)

     (send interp exec-insn
           (Insn:whilelt (Reg:p 0) (Reg:x 0) (Reg:x 1)))

     (define s (send interp get-state))
     (check-equal? (state-read-p s 0) 0))

   (test-case "ptrue sets all lanes active"
     (define config config/sve)
     (define interp (make-sve-interp #:config config))

     (send interp exec-insn
           (Insn:sve-pred-op 'ptrue (Reg:p 0) (Reg:p 0) (Reg:p 0)))

     (define s (send interp get-state))
     ;; 4 elements for 128-bit VL with 32-bit elements
     (check-equal? (state-read-p s 0) #b1111))

   (test-case "pfalse clears all lanes"
     (define config config/sve)
     (define interp (make-sve-interp #:config config))

     ;; First set some lanes
     (define s0 (send interp get-state))
     (send interp set-state! (state-write-p s0 0 #b1111))

     (send interp exec-insn
           (Insn:sve-pred-op 'pfalse (Reg:p 0) (Reg:p 0) (Reg:p 0)))

     (define s1 (send interp get-state))
     (check-equal? (state-read-p s1 0) 0))))

;; ============================================================================
;; SVE Arithmetic Tests
;; ============================================================================

(define sve-arith-tests
  (test-suite
   "SVE Arithmetic Operations"

   (test-case "sve add with all lanes active"
     (define config config/sve)
     (define interp (make-sve-interp #:config config))

     ;; Set up Z registers with values
     (define s0 (send interp get-state))
     (define z1 (list->pvector '(1 2 3 4)))
     (define z2 (list->pvector '(10 20 30 40)))
     (define s1 (state-write-z s0 1 z1))
     (define s2 (state-write-z s1 2 z2))
     ;; Set all lanes active
     (define s3 (state-write-p s2 0 #b1111))
     (send interp set-state! s3)

     (send interp exec-insn
           (Insn:sve 'add (Reg:p 0) (Reg:z 0) (list (Reg:z 1) (Reg:z 2))))

     (define s4 (send interp get-state))
     (define result (state-read-z s4 0))
     (check-equal? (pvector->list result) '(11 22 33 44)))

   (test-case "sve add with partial predication"
     (define config config/sve)
     (define interp (make-sve-interp #:config config))

     ;; Set up Z registers
     (define s0 (send interp get-state))
     (define z0-init (list->pvector '(100 100 100 100)))  ; Initial values
     (define z1 (list->pvector '(1 2 3 4)))
     (define z2 (list->pvector '(10 20 30 40)))
     (define s1 (state-write-z s0 0 z0-init))
     (define s2 (state-write-z s1 1 z1))
     (define s3 (state-write-z s2 2 z2))
     ;; Only lanes 0 and 2 active
     (define s4 (state-write-p s3 0 #b0101))
     (send interp set-state! s4)

     (send interp exec-insn
           (Insn:sve 'add (Reg:p 0) (Reg:z 0) (list (Reg:z 1) (Reg:z 2))))

     (define s5 (send interp get-state))
     (define result (state-read-z s5 0))
     ;; Lanes 0,2 computed, lanes 1,3 preserved
     (check-equal? (pvector->list result) '(11 100 33 100)))

   (test-case "sve sub"
     (define config config/sve)
     (define interp (make-sve-interp #:config config))

     (define s0 (send interp get-state))
     (define z1 (list->pvector '(50 60 70 80)))
     (define z2 (list->pvector '(10 20 30 40)))
     (define s1 (state-write-z s0 1 z1))
     (define s2 (state-write-z s1 2 z2))
     (define s3 (state-write-p s2 0 #b1111))
     (send interp set-state! s3)

     (send interp exec-insn
           (Insn:sve 'sub (Reg:p 0) (Reg:z 0) (list (Reg:z 1) (Reg:z 2))))

     (define s4 (send interp get-state))
     (define result (state-read-z s4 0))
     (check-equal? (pvector->list result) '(40 40 40 40)))

   (test-case "sve mul"
     (define config config/sve)
     (define interp (make-sve-interp #:config config))

     (define s0 (send interp get-state))
     (define z1 (list->pvector '(2 3 4 5)))
     (define z2 (list->pvector '(10 10 10 10)))
     (define s1 (state-write-z s0 1 z1))
     (define s2 (state-write-z s1 2 z2))
     (define s3 (state-write-p s2 0 #b1111))
     (send interp set-state! s3)

     (send interp exec-insn
           (Insn:sve 'mul (Reg:p 0) (Reg:z 0) (list (Reg:z 1) (Reg:z 2))))

     (define s4 (send interp get-state))
     (define result (state-read-z s4 0))
     (check-equal? (pvector->list result) '(20 30 40 50)))))

;; ============================================================================
;; SVE Load/Store Tests
;; ============================================================================

(define sve-mem-tests
  (test-suite
   "SVE Load/Store Operations"

   (test-case "sve ld1w contiguous load"
     (define config config/sve)
     (define interp (make-sve-interp #:config config))

     ;; Store values in memory
     (define s0 (send interp get-state))
     (define s1 (state-write-word s0 #x1000 100))
     (define s2 (state-write-word s1 #x1004 200))
     (define s3 (state-write-word s2 #x1008 300))
     (define s4 (state-write-word s3 #x100C 400))
     ;; Set all lanes active
     (define s5 (state-write-p s4 0 #b1111))
     (send interp set-state! s5)

     ;; Set base address
     (send interp write-x 0 #x1000)

     (send interp exec-insn
           (Insn:sve-load 'ld1w (Reg:p 0) (Reg:z 0) (Mem:base (Reg:x 0))))

     (define s6 (send interp get-state))
     (define result (state-read-z s6 0))
     (check-equal? (pvector->list result) '(100 200 300 400)))

   (test-case "sve ld1w with partial predicate"
     (define config config/sve)
     (define interp (make-sve-interp #:config config))

     ;; Store values in memory
     (define s0 (send interp get-state))
     (define z0-init (list->pvector '(999 999 999 999)))
     (define s1 (state-write-z s0 0 z0-init))
     (define s2 (state-write-word s1 #x1000 100))
     (define s3 (state-write-word s2 #x1004 200))
     (define s4 (state-write-word s3 #x1008 300))
     (define s5 (state-write-word s4 #x100C 400))
     ;; Only lanes 1 and 3 active
     (define s6 (state-write-p s5 0 #b1010))
     (send interp set-state! s6)

     (send interp write-x 0 #x1000)

     (send interp exec-insn
           (Insn:sve-load 'ld1w (Reg:p 0) (Reg:z 0) (Mem:base (Reg:x 0))))

     (define s7 (send interp get-state))
     (define result (state-read-z s7 0))
     ;; Lanes 0,2 unchanged, lanes 1,3 loaded
     (check-equal? (pvector->list result) '(999 200 999 400)))

   (test-case "sve st1w contiguous store"
     (define config config/sve)
     (define interp (make-sve-interp #:config config))

     ;; Set up Z register with values
     (define s0 (send interp get-state))
     (define z0 (list->pvector '(111 222 333 444)))
     (define s1 (state-write-z s0 0 z0))
     (define s2 (state-write-p s1 0 #b1111))
     (send interp set-state! s2)

     (send interp write-x 1 #x2000)

     (send interp exec-insn
           (Insn:sve-store 'st1w (Reg:p 0) (Reg:z 0) (Mem:base (Reg:x 1))))

     (define s3 (send interp get-state))
     (check-equal? (state-read-word s3 #x2000) 111)
     (check-equal? (state-read-word s3 #x2004) 222)
     (check-equal? (state-read-word s3 #x2008) 333)
     (check-equal? (state-read-word s3 #x200C) 444))))

;; ============================================================================
;; SVE Comparison Tests
;; ============================================================================

(define sve-cmp-tests
  (test-suite
   "SVE Comparison Operations"

   (test-case "sve cmpeq"
     (define config config/sve)
     (define interp (make-sve-interp #:config config))

     (define s0 (send interp get-state))
     (define z1 (list->pvector '(1 2 3 4)))
     (define z2 (list->pvector '(1 5 3 8)))
     (define s1 (state-write-z s0 1 z1))
     (define s2 (state-write-z s1 2 z2))
     ;; Governing predicate: all active
     (define s3 (state-write-p s2 1 #b1111))
     (send interp set-state! s3)

     (send interp exec-insn
           (Insn:sve-cmp 'cmpeq (Reg:p 0) (Reg:p 1) (Reg:z 1) (Reg:z 2)))

     (define s4 (send interp get-state))
     ;; Lanes 0 and 2 are equal
     (check-equal? (state-read-p s4 0) #b0101))

   (test-case "sve cmplt"
     (define config config/sve)
     (define interp (make-sve-interp #:config config))

     (define s0 (send interp get-state))
     (define z1 (list->pvector '(1 5 3 8)))
     (define z2 (list->pvector '(2 4 3 10)))
     (define s1 (state-write-z s0 1 z1))
     (define s2 (state-write-z s1 2 z2))
     (define s3 (state-write-p s2 1 #b1111))
     (send interp set-state! s3)

     (send interp exec-insn
           (Insn:sve-cmp 'cmplt (Reg:p 0) (Reg:p 1) (Reg:z 1) (Reg:z 2)))

     (define s4 (send interp get-state))
     ;; 1<2 ✓, 5<4 ✗, 3<3 ✗, 8<10 ✓
     (check-equal? (state-read-p s4 0) #b1001))))

;; ============================================================================
;; Run All Tests
;; ============================================================================

(define all-tests
  (test-suite
   "AArch64 SVE Interpreter"
   sve-pred-tests
   sve-arith-tests
   sve-mem-tests
   sve-cmp-tests))

(module+ main
  (run-tests all-tests))

(module+ test
  (run-tests all-tests))
