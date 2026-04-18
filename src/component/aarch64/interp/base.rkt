#lang racket/base

;; AArch64 Base Interpreter
;;
;; Interprets AArch64 instructions for testing and simulation.
;; Uses class-based design for extensibility (SVE/SVE2 can override methods).

(require racket/class
         racket/match
         "../../../../../cutie-ftree/pvector.rkt"
         "../../../../../cutie-ftree/ordered-map.rkt"
         "../ir/types.rkt"
         "../ir/cfg.rkt"
         "../ir/config.rkt"
         "state.rkt")

(provide
 ;; Interpreter class
 interp-aarch64-base%

 ;; Convenience functions
 run-function
 run-cfg
 run-block
 step-insn

 ;; Result type
 InterpResult InterpResult?
 InterpResult-state InterpResult-value InterpResult-halted?
 interp-ok
 interp-halt
 interp-error)

;; ============================================================================
;; Interpreter Result
;; ============================================================================

(struct InterpResult (state value halted? error) #:prefab)

(define (interp-ok state #:value [value #f])
  (InterpResult state value #f #f))

(define (interp-halt state value)
  (InterpResult state value #t #f))

(define (interp-error msg state)
  (InterpResult state #f #t msg))

;; ============================================================================
;; Base Interpreter Class
;; ============================================================================

(define interp-aarch64-base%
  (class object%
    (init-field [config #f])

    ;; Current machine state (mutable for convenience in interpreter)
    (field [state (make-initial-state config)])

    ;; ========================================================================
    ;; State Management
    ;; ========================================================================

    (define/public (get-state) state)

    (define/public (set-state! new-state)
      (set! state new-state))

    (define/public (reset!)
      (set! state (make-initial-state config)))

    ;; ========================================================================
    ;; Register Access (convenience wrappers)
    ;; ========================================================================

    (define/public (read-reg reg)
      (state-read-reg state reg))

    (define/public (write-reg reg val)
      (set! state (state-write-reg state reg val)))

    (define/public (read-x id)
      (state-read-x state id))

    (define/public (write-x id val)
      (set! state (state-write-x state id val)))

    (define/public (read-w id)
      (state-read-w state id))

    (define/public (write-w id val)
      (set! state (state-write-w state id val)))

    ;; ========================================================================
    ;; Flag Operations
    ;; ========================================================================

    (define/public (update-flags-nz val width)
      ;; Update N and Z flags based on result value
      (define mask (sub1 (arithmetic-shift 1 width)))
      (define masked-val (bitwise-and val mask))
      (define sign-bit (arithmetic-shift 1 (sub1 width)))

      ;; Z flag: set if result is zero
      (set! state
            (if (zero? masked-val)
                (state-set-flag state flag-z)
                (state-clear-flag state flag-z)))

      ;; N flag: set if result is negative (sign bit set)
      (set! state
            (if (bitwise-bit-set? masked-val (sub1 width))
                (state-set-flag state flag-n)
                (state-clear-flag state flag-n))))

    (define/public (update-flags-add a b result width)
      (update-flags-nz result width)
      ;; C flag: carry out
      (define max-val (arithmetic-shift 1 width))
      (set! state
            (if (>= (+ a b) max-val)
                (state-set-flag state flag-c)
                (state-clear-flag state flag-c)))
      ;; V flag: signed overflow
      (define sign-bit (sub1 width))
      (define a-sign (bitwise-bit-set? a sign-bit))
      (define b-sign (bitwise-bit-set? b sign-bit))
      (define r-sign (bitwise-bit-set? result sign-bit))
      (set! state
            (if (and (eq? a-sign b-sign) (not (eq? a-sign r-sign)))
                (state-set-flag state flag-v)
                (state-clear-flag state flag-v))))

    (define/public (update-flags-sub a b result width)
      (update-flags-nz result width)
      ;; C flag: no borrow (a >= b for unsigned)
      (set! state
            (if (>= a b)
                (state-set-flag state flag-c)
                (state-clear-flag state flag-c)))
      ;; V flag: signed overflow
      (define sign-bit (sub1 width))
      (define a-sign (bitwise-bit-set? a sign-bit))
      (define b-sign (bitwise-bit-set? b sign-bit))
      (define r-sign (bitwise-bit-set? result sign-bit))
      (set! state
            (if (and (not (eq? a-sign b-sign)) (not (eq? a-sign r-sign)))
                (state-set-flag state flag-v)
                (state-clear-flag state flag-v))))

    (define/public (test-condition cond)
      (define n (state-test-flag state flag-n))
      (define z (state-test-flag state flag-z))
      (define c (state-test-flag state flag-c))
      (define v (state-test-flag state flag-v))
      (case cond
        [(eq) z]                    ; Equal (Z=1)
        [(ne) (not z)]              ; Not equal (Z=0)
        [(cs hs) c]                 ; Carry set / unsigned higher or same
        [(cc lo) (not c)]           ; Carry clear / unsigned lower
        [(mi) n]                    ; Minus (negative)
        [(pl) (not n)]              ; Plus (positive or zero)
        [(vs) v]                    ; Overflow set
        [(vc) (not v)]              ; Overflow clear
        [(hi) (and c (not z))]      ; Unsigned higher
        [(ls) (or (not c) z)]       ; Unsigned lower or same
        [(ge) (eq? n v)]            ; Signed greater or equal
        [(lt) (not (eq? n v))]      ; Signed less than
        [(gt) (and (not z) (eq? n v))] ; Signed greater than
        [(le) (or z (not (eq? n v)))]  ; Signed less or equal
        [(al) #t]                   ; Always
        [else #t]))

    ;; ========================================================================
    ;; Operand Evaluation
    ;; ========================================================================

    (define/public (eval-operand op)
      (match op
        ;; Registers
        [(? any-reg?) (read-reg op)]

        ;; Immediate
        [(Imm val) val]
        [(Imm:shifted val shift) (arithmetic-shift val shift)]

        ;; Memory - return address for separate load
        [(Mem:base reg) (read-reg reg)]
        [(Mem:offset reg off) (+ (read-reg reg) off)]
        [(Mem:pre reg off) (+ (read-reg reg) off)]
        [(Mem:post reg _) (read-reg reg)]
        [(Mem:reg base idx) (+ (read-reg base) (read-reg idx))]
        [(Mem:scaled base idx scale)
         (+ (read-reg base) (arithmetic-shift (read-reg idx) scale))]

        ;; Label - should be resolved before interpretation
        [(Label:id id) id]
        [(Label:named name) (error 'eval-operand "unresolved label: ~a" name)]

        [_ (error 'eval-operand "unknown operand: ~a" op)]))

    ;; Get operand width in bits
    (define/public (operand-width op)
      (match op
        [(Reg:x _) 64]
        [(Reg:w _) 32]
        [(Reg:xzr) 64]
        [(Reg:wzr) 32]
        [(Reg:sp) 64]
        [(Reg:d _) 64]
        [(Reg:s _) 32]
        [(Reg:h _) 16]
        [(Reg:v _ width)
         (case width
           [(q) 128] [(d) 64] [(s) 32] [(h) 16] [(b) 8])]
        [_ 64]))  ; Default to 64

    ;; ========================================================================
    ;; Instruction Execution
    ;; ========================================================================

    (define/public (exec-insn insn)
      (match insn
        ;; Arithmetic
        [(Insn:arith op dst src1 src2)
         (exec-arith op dst src1 src2)]

        ;; Unary
        [(Insn:arith2 op dst src)
         (exec-unary op dst src)]

        ;; Move
        [(Insn:mov _ dst src)
         (exec-mov dst src)]

        ;; Compare
        [(Insn:cmp _ src1 src2)
         (exec-cmp src1 src2)]

        ;; Load
        [(Insn:load op dst addr)
         (exec-load op dst addr)]

        ;; Store
        [(Insn:store op src addr)
         (exec-store op src addr)]

        ;; Load pair
        [(Insn:ldp _ dst1 dst2 addr)
         (exec-ldp dst1 dst2 addr)]

        ;; Store pair
        [(Insn:stp _ src1 src2 addr)
         (exec-stp src1 src2 addr)]

        ;; Conditional select
        [(Insn:csel _ dst src1 src2 cond)
         (exec-csel dst src1 src2 cond)]

        ;; Generic instruction
        [(AsmInsn op args)
         (exec-generic op args)]

        ;; Return (handled by run-block)
        [(Insn:ret) 'ret]

        ;; Branches (handled by run-block)
        [(Insn:branch _ _) insn]
        [(Insn:cond-branch _ _ _) insn]
        [(Insn:cbz _ _ _) insn]

        [_ (error 'exec-insn "unhandled instruction: ~a" insn)]))

    ;; ========================================================================
    ;; Arithmetic Operations
    ;; ========================================================================

    (define/public (exec-arith op dst src1 src2)
      (define v1 (eval-operand src1))
      (define v2 (eval-operand src2))
      (define width (operand-width dst))
      (define mask (sub1 (arithmetic-shift 1 width)))

      (define result
        (bitwise-and
         (case op
           [(add) (+ v1 v2)]
           [(sub) (- v1 v2)]
           [(mul) (* v1 v2)]
           [(sdiv) (if (zero? v2) 0 (quotient v1 v2))]
           [(udiv) (if (zero? v2) 0 (quotient v1 v2))]
           [(and) (bitwise-and v1 v2)]
           [(orr) (bitwise-ior v1 v2)]
           [(eor) (bitwise-xor v1 v2)]
           [(bic) (bitwise-and v1 (bitwise-not v2))]
           [(orn) (bitwise-ior v1 (bitwise-not v2))]
           [(adds) (begin (update-flags-add v1 v2 (+ v1 v2) width) (+ v1 v2))]
           [(subs) (begin (update-flags-sub v1 v2 (- v1 v2) width) (- v1 v2))]
           [(asr) (arithmetic-shift v1 (- v2))]
           [(lsl) (arithmetic-shift v1 v2)]
           [(lsr) (arithmetic-shift v1 (- v2))]  ; Logical right shift
           [(fadd) (+ v1 v2)]  ; Simplified FP
           [(fsub) (- v1 v2)]
           [(fmul) (* v1 v2)]
           [(fdiv) (if (zero? v2) +inf.0 (/ v1 v2))]
           [else (error 'exec-arith "unknown op: ~a" op)])
         mask))

      (write-reg dst result))

    ;; ========================================================================
    ;; Unary Operations
    ;; ========================================================================

    (define/public (exec-unary op dst src)
      (define v (eval-operand src))
      (define width (operand-width dst))
      (define mask (sub1 (arithmetic-shift 1 width)))

      (define result
        (bitwise-and
         (case op
           [(neg) (- v)]
           [(abs) (abs v)]
           [(mvn not) (bitwise-not v)]
           [(fabs) (abs v)]
           [(fneg) (- v)]
           [(fsqrt) (sqrt v)]
           [else (error 'exec-unary "unknown op: ~a" op)])
         mask))

      (write-reg dst result))

    ;; ========================================================================
    ;; Move
    ;; ========================================================================

    (define/public (exec-mov dst src)
      (define v (eval-operand src))
      (write-reg dst v))

    ;; ========================================================================
    ;; Compare
    ;; ========================================================================

    (define/public (exec-cmp src1 src2)
      (define v1 (eval-operand src1))
      (define v2 (eval-operand src2))
      (define width (operand-width src1))
      (define result (- v1 v2))
      (update-flags-sub v1 v2 result width))

    ;; ========================================================================
    ;; Load/Store
    ;; ========================================================================

    (define/public (exec-load op dst addr)
      (define address (eval-operand addr))
      (define value
        (case op
          [(ldr) (state-read-doubleword state address)]
          [(ldrw ldrsw) (state-read-word state address)]
          [(ldrh ldrsh) (state-read-halfword state address)]
          [(ldrb ldrsb) (state-read-byte state address)]
          [else (state-read-doubleword state address)]))

      ;; Handle sign extension for signed loads
      (define extended-value
        (case op
          [(ldrsb)
           (if (bitwise-bit-set? value 7)
               (bitwise-ior value #xFFFFFFFFFFFFFF00)
               value)]
          [(ldrsh)
           (if (bitwise-bit-set? value 15)
               (bitwise-ior value #xFFFFFFFFFFFF0000)
               value)]
          [(ldrsw)
           (if (bitwise-bit-set? value 31)
               (bitwise-ior value #xFFFFFFFF00000000)
               value)]
          [else value]))

      (write-reg dst extended-value)

      ;; Handle post-indexed addressing
      (when (Mem:post? addr)
        (define base-reg (Mem:post-reg addr))
        (define offset (Mem:post-offset addr))
        (write-reg base-reg (+ (read-reg base-reg) offset)))

      ;; Handle pre-indexed addressing (update base)
      (when (Mem:pre? addr)
        (define base-reg (Mem:pre-reg addr))
        (write-reg base-reg address)))

    (define/public (exec-store op src addr)
      (define address (eval-operand addr))
      (define value (eval-operand src))

      (case op
        [(str) (set! state (state-write-doubleword state address value))]
        [(strw) (set! state (state-write-word state address value))]
        [(strh) (set! state (state-write-halfword state address value))]
        [(strb) (set! state (state-write-byte state address value))]
        [else (set! state (state-write-doubleword state address value))])

      ;; Handle post-indexed
      (when (Mem:post? addr)
        (define base-reg (Mem:post-reg addr))
        (define offset (Mem:post-offset addr))
        (write-reg base-reg (+ (read-reg base-reg) offset)))

      ;; Handle pre-indexed
      (when (Mem:pre? addr)
        (define base-reg (Mem:pre-reg addr))
        (write-reg base-reg address)))

    ;; ========================================================================
    ;; Load/Store Pair
    ;; ========================================================================

    (define/public (exec-ldp dst1 dst2 addr)
      (define address (eval-operand addr))
      (define v1 (state-read-doubleword state address))
      (define v2 (state-read-doubleword state (+ address 8)))
      (write-reg dst1 v1)
      (write-reg dst2 v2)

      ;; Handle indexed addressing
      (when (Mem:pre? addr)
        (write-reg (Mem:pre-reg addr) address))
      (when (Mem:post? addr)
        (write-reg (Mem:post-reg addr)
                   (+ (read-reg (Mem:post-reg addr)) (Mem:post-offset addr)))))

    (define/public (exec-stp src1 src2 addr)
      (define address (eval-operand addr))
      (define v1 (eval-operand src1))
      (define v2 (eval-operand src2))
      (set! state (state-write-doubleword state address v1))
      (set! state (state-write-doubleword state (+ address 8) v2))

      ;; Handle indexed addressing
      (when (Mem:pre? addr)
        (write-reg (Mem:pre-reg addr) address))
      (when (Mem:post? addr)
        (write-reg (Mem:post-reg addr)
                   (+ (read-reg (Mem:post-reg addr)) (Mem:post-offset addr)))))

    ;; ========================================================================
    ;; Conditional Select
    ;; ========================================================================

    (define/public (exec-csel dst src1 src2 cond)
      (define result
        (if (test-condition cond)
            (eval-operand src1)
            (eval-operand src2)))
      (write-reg dst result))

    ;; ========================================================================
    ;; Generic Instruction
    ;; ========================================================================

    (define/public (exec-generic op args)
      ;; Fallback for unhandled instructions
      (void))

    ;; ========================================================================
    ;; Block Execution
    ;; ========================================================================

    (define/public (run-block block)
      ;; Execute all instructions in the block
      (for ([insn (in-pvector (AsmBlock-insns block))])
        (exec-insn insn))

      ;; Return terminator for control flow
      (AsmBlock-terminator block))

    ;; ========================================================================
    ;; CFG Execution
    ;; ========================================================================

    (define/public (run-cfg cfg #:max-steps [max-steps 10000])
      (define entry-id (AsmCfg-entry cfg))
      (unless entry-id
        (error 'run-cfg "CFG has no entry block"))

      (let loop ([current-id entry-id]
                 [steps 0])
        (when (>= steps max-steps)
          (error 'run-cfg "max steps exceeded"))

        (define block (cfg-get-block cfg current-id))
        (unless block
          (error 'run-cfg "block not found: ~a" current-id))

        (define term (run-block block))

        (match term
          [(Term:ret)
           ;; Return x0 as result
           (interp-halt state (read-x 0))]

          [(Term:jump target)
           (loop (resolve-target target cfg) (add1 steps))]

          [(Term:cond cond then-target else-target)
           (if (test-condition cond)
               (loop (resolve-target then-target cfg) (add1 steps))
               (if else-target
                   (loop (resolve-target else-target cfg) (add1 steps))
                   (interp-error "missing else target" state)))]

          [(Term:unreachable)
           (interp-error "reached unreachable" state)]

          [#f
           ;; No terminator - fall through to return
           (interp-halt state (read-x 0))]

          [_
           (interp-error (format "unknown terminator: ~a" term) state)])))

    ;; Resolve target to BlockId (vertex-id)
    (define/private (resolve-target target cfg)
      (match target
        [(? BlockId?) target]
        [(Label:named name)
         ;; Search for block with matching label
         (for/or ([block (in-cfg-blocks cfg)])
           (define label (AsmBlock-label block))
           (and (Label:named? label)
                (eq? (Label:named-name label) name)
                (AsmBlock-id block)))]
        [(Label:id id)
         ;; Search for block with matching Label:id
         (for/or ([block (in-cfg-blocks cfg)])
           (define label (AsmBlock-label block))
           (and (Label:id? label)
                (= (Label:id-id label) id)
                (AsmBlock-id block)))]
        [_ target]))

    ;; ========================================================================
    ;; Function Execution
    ;; ========================================================================

    (define/public (run-function fn args)
      ;; Load arguments into parameter registers
      (for ([param (in-list (AsmFunction-params fn))]
            [arg (in-list args)])
        (write-reg (AsmParam-reg param) arg))

      ;; Execute body
      (define body (AsmFunction-body fn))
      (cond
        [(AsmCfg? body)
         (run-cfg body)]
        [else
         ;; Flat instruction list - wrap in simple CFG
         (interp-halt state (read-x 0))]))

    (super-new)))

;; ============================================================================
;; Convenience Functions
;; ============================================================================

(define (run-function fn args #:config [config #f])
  (define interp (new interp-aarch64-base% [config config]))
  (send interp run-function fn args))

(define (run-cfg cfg #:config [config #f] #:max-steps [max-steps 10000])
  (define interp (new interp-aarch64-base% [config config]))
  (send interp run-cfg cfg #:max-steps max-steps))

(define (run-block block #:state [initial-state #f] #:config [config #f])
  (define interp (new interp-aarch64-base% [config config]))
  (when initial-state
    (send interp set-state! initial-state))
  (send interp run-block block)
  (send interp get-state))

(define (step-insn insn #:state [initial-state #f] #:config [config #f])
  (define interp (new interp-aarch64-base% [config config]))
  (when initial-state
    (send interp set-state! initial-state))
  (send interp exec-insn insn)
  (send interp get-state))
