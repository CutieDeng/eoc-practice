#lang racket/base

;; AArch64 Interpreter Machine State
;;
;; Uses persistent data structures from cutie-ftree:
;; - pvector for register files
;; - ordered-map for memory
;; - bitset for flags

(require racket/match
         "../../../../../cutie-ftree/pvector.rkt"
         "../../../../../cutie-ftree/ordered-map.rkt"
         "../../../../../cutie-ftree/bitset.rkt"
         "../../../../../cutie-ftree/comparator.rkt"
         "../ir/types.rkt"
         "../ir/config.rkt")

(provide
 ;; Machine state
 MachineState MachineState?
 MachineState-x-regs
 MachineState-v-regs
 MachineState-z-regs
 MachineState-p-regs
 MachineState-sp
 MachineState-pc
 MachineState-flags
 MachineState-memory
 MachineState-vl
 MachineState-config

 ;; State construction
 make-initial-state
 make-empty-state

 ;; Register access
 state-read-x
 state-write-x
 state-read-w
 state-write-w
 state-read-z
 state-write-z
 state-read-p
 state-write-p
 state-read-v
 state-write-v
 state-read-reg
 state-write-reg

 ;; Special registers
 state-read-sp
 state-write-sp
 state-read-pc
 state-write-pc

 ;; Flags
 state-read-flags
 state-write-flags
 state-set-flag
 state-clear-flag
 state-test-flag
 flag-n flag-z flag-c flag-v

 ;; Memory access
 state-read-byte
 state-write-byte
 state-read-halfword
 state-write-halfword
 state-read-word
 state-write-word
 state-read-doubleword
 state-write-doubleword

 ;; SVE vector operations
 make-sve-zero
 sve-vector-ref
 sve-vector-set
 sve-vector-length

 ;; Predicate operations
 pred-active?
 pred-set-lane
 pred-clear-lane
 pred-all-true
 pred-all-false)

;; ============================================================================
;; Machine State Structure
;; ============================================================================

;; MachineState:
;; - x-regs: pvector of 31 64-bit values (x0-x30)
;; - v-regs: pvector of 32 128-bit values (v0-v31, stored as pairs)
;; - z-regs: pvector of 32 SVE vectors (z0-z31, each is a pvector)
;; - p-regs: pvector of 16 predicates (p0-p15, each is an integer bitset)
;; - sp: stack pointer (64-bit)
;; - pc: program counter
;; - flags: NZCV flags (bitset)
;; - memory: ordered-map of address -> byte
;; - vl: vector length in bytes for SVE
;; - config: AsmConfig
(struct MachineState (x-regs v-regs z-regs p-regs sp pc flags memory vl config)
  #:prefab)

;; ============================================================================
;; Flag Constants
;; ============================================================================

(define flag-n 3)  ; Negative
(define flag-z 2)  ; Zero
(define flag-c 1)  ; Carry
(define flag-v 0)  ; Overflow

;; ============================================================================
;; State Construction
;; ============================================================================

(define (make-empty-state)
  (MachineState
   (make-pvector-n 31 0)           ; x0-x30 = 0
   (make-pvector-n 32 (cons 0 0))  ; v0-v31 = 0 (128-bit as pair)
   (make-pvector-n 32 (pvector-empty)) ; z0-z31 = empty
   (make-pvector-n 16 0)           ; p0-p15 = 0
   #xFFFF0000                      ; Initial SP
   0                               ; PC = 0
   (bitset)                        ; Flags clear
   (ordered-map-empty integer-compare) ; Empty memory
   16                              ; Default VL = 128 bits = 16 bytes
   #f))                            ; No config

(define (make-initial-state config)
  (define vl-bytes (if config (config-vl-bytes config) 16))
  (MachineState
   (make-pvector-n 31 0)
   (make-pvector-n 32 (cons 0 0))
   (make-pvector-n 32 (make-sve-zero vl-bytes))
   (make-pvector-n 16 0)
   #xFFFF0000
   0
   (bitset)
   (ordered-map-empty integer-compare)
   vl-bytes
   config))

;; Helper to create pvector of n elements
(define (make-pvector-n n val)
  (for/fold ([pv (pvector-empty)])
            ([_ (in-range n)])
    (pvector-cons-right pv val)))

;; ============================================================================
;; X Register Access (64-bit)
;; ============================================================================

(define (state-read-x state id)
  (unless (<= 0 id 30)
    (error 'state-read-x "invalid x-register id: ~a" id))
  (pvector-ref (MachineState-x-regs state) id))

(define (state-write-x state id val)
  (unless (<= 0 id 30)
    (error 'state-write-x "invalid x-register id: ~a" id))
  (struct-copy MachineState state
               [x-regs (pvector-set (MachineState-x-regs state) id val)]))

;; ============================================================================
;; W Register Access (32-bit, lower half of X)
;; ============================================================================

(define (state-read-w state id)
  (bitwise-and (state-read-x state id) #xFFFFFFFF))

(define (state-write-w state id val)
  ;; Writing to w register zero-extends to x register
  (state-write-x state id (bitwise-and val #xFFFFFFFF)))

;; ============================================================================
;; V Register Access (128-bit NEON)
;; ============================================================================

(define (state-read-v state id)
  (unless (<= 0 id 31)
    (error 'state-read-v "invalid v-register id: ~a" id))
  (pvector-ref (MachineState-v-regs state) id))

(define (state-write-v state id val)
  (unless (<= 0 id 31)
    (error 'state-write-v "invalid v-register id: ~a" id))
  (struct-copy MachineState state
               [v-regs (pvector-set (MachineState-v-regs state) id val)]))

;; ============================================================================
;; Z Register Access (SVE vectors)
;; ============================================================================

(define (state-read-z state id)
  (unless (<= 0 id 31)
    (error 'state-read-z "invalid z-register id: ~a" id))
  (pvector-ref (MachineState-z-regs state) id))

(define (state-write-z state id val)
  (unless (<= 0 id 31)
    (error 'state-write-z "invalid z-register id: ~a" id))
  (struct-copy MachineState state
               [z-regs (pvector-set (MachineState-z-regs state) id val)]))

;; ============================================================================
;; P Register Access (SVE predicates)
;; ============================================================================

(define (state-read-p state id)
  (unless (<= 0 id 15)
    (error 'state-read-p "invalid p-register id: ~a" id))
  (pvector-ref (MachineState-p-regs state) id))

(define (state-write-p state id val)
  (unless (<= 0 id 15)
    (error 'state-write-p "invalid p-register id: ~a" id))
  (struct-copy MachineState state
               [p-regs (pvector-set (MachineState-p-regs state) id val)]))

;; ============================================================================
;; Generic Register Access
;; ============================================================================

(define (state-read-reg state reg)
  (match reg
    [(Reg:x id) (state-read-x state id)]
    [(Reg:w id) (state-read-w state id)]
    [(Reg:z id) (state-read-z state id)]
    [(Reg:p id) (state-read-p state id)]
    [(Reg:v id _) (state-read-v state id)]
    [(Reg:sp) (MachineState-sp state)]
    [(Reg:xzr) 0]
    [(Reg:wzr) 0]
    [_ (error 'state-read-reg "unknown register: ~a" reg)]))

(define (state-write-reg state reg val)
  (match reg
    [(Reg:x id) (state-write-x state id val)]
    [(Reg:w id) (state-write-w state id val)]
    [(Reg:z id) (state-write-z state id val)]
    [(Reg:p id) (state-write-p state id val)]
    [(Reg:v id _) (state-write-v state id val)]
    [(Reg:sp) (struct-copy MachineState state [sp val])]
    [(Reg:xzr) state]  ; Writes to xzr are discarded
    [(Reg:wzr) state]  ; Writes to wzr are discarded
    [_ (error 'state-write-reg "unknown register: ~a" reg)]))

;; ============================================================================
;; Special Registers
;; ============================================================================

(define (state-read-sp state)
  (MachineState-sp state))

(define (state-write-sp state val)
  (struct-copy MachineState state [sp val]))

(define (state-read-pc state)
  (MachineState-pc state))

(define (state-write-pc state val)
  (struct-copy MachineState state [pc val]))

;; ============================================================================
;; Flags
;; ============================================================================

(define (state-read-flags state)
  (MachineState-flags state))

(define (state-write-flags state flags)
  (struct-copy MachineState state [flags flags]))

(define (state-set-flag state flag)
  (struct-copy MachineState state
               [flags (bitset-add (MachineState-flags state) flag)]))

(define (state-clear-flag state flag)
  (struct-copy MachineState state
               [flags (bitset-remove (MachineState-flags state) flag)]))

(define (state-test-flag state flag)
  (bitset-member? (MachineState-flags state) flag))

;; ============================================================================
;; Memory Access
;; ============================================================================

(define (state-read-byte state addr)
  (ordered-map-ref (MachineState-memory state) addr 0))

(define (state-write-byte state addr val)
  (struct-copy MachineState state
               [memory (ordered-map-set (MachineState-memory state)
                                        addr
                                        (bitwise-and val #xFF))]))

(define (state-read-halfword state addr)
  (+ (state-read-byte state addr)
     (arithmetic-shift (state-read-byte state (+ addr 1)) 8)))

(define (state-write-halfword state addr val)
  (define s1 (state-write-byte state addr (bitwise-and val #xFF)))
  (state-write-byte s1 (+ addr 1) (bitwise-and (arithmetic-shift val -8) #xFF)))

(define (state-read-word state addr)
  (+ (state-read-halfword state addr)
     (arithmetic-shift (state-read-halfword state (+ addr 2)) 16)))

(define (state-write-word state addr val)
  (define s1 (state-write-halfword state addr (bitwise-and val #xFFFF)))
  (state-write-halfword s1 (+ addr 2) (bitwise-and (arithmetic-shift val -16) #xFFFF)))

(define (state-read-doubleword state addr)
  (+ (state-read-word state addr)
     (arithmetic-shift (state-read-word state (+ addr 4)) 32)))

(define (state-write-doubleword state addr val)
  (define s1 (state-write-word state addr (bitwise-and val #xFFFFFFFF)))
  (state-write-word s1 (+ addr 4) (bitwise-and (arithmetic-shift val -32) #xFFFFFFFF)))

;; ============================================================================
;; SVE Vector Operations
;; ============================================================================

;; Create a zero SVE vector of given byte length
(define (make-sve-zero vl-bytes)
  (make-pvector-n (quotient vl-bytes 4) 0))  ; 32-bit elements

;; Get element from SVE vector
(define (sve-vector-ref vec idx)
  (pvector-ref vec idx))

;; Set element in SVE vector
(define (sve-vector-set vec idx val)
  (pvector-set vec idx val))

;; Get vector length in elements (for 32-bit elements)
(define (sve-vector-length vec)
  (pvector-length vec))

;; ============================================================================
;; Predicate Operations
;; ============================================================================

;; Check if lane is active in predicate
(define (pred-active? pred lane)
  (bitwise-bit-set? pred lane))

;; Set a lane in predicate
(define (pred-set-lane pred lane)
  (bitwise-ior pred (arithmetic-shift 1 lane)))

;; Clear a lane in predicate
(define (pred-clear-lane pred lane)
  (bitwise-and pred (bitwise-not (arithmetic-shift 1 lane))))

;; Create predicate with all lanes true (for vl elements)
(define (pred-all-true vl-elements)
  (sub1 (arithmetic-shift 1 vl-elements)))

;; Create predicate with all lanes false
(define (pred-all-false)
  0)
