#lang racket/base

;; ============================================================================
;; SVE memcpy Use Case (Simplified - uses currently supported instructions)
;; ============================================================================
;;
;; This example uses only the instructions that the current framework supports.
;; It demonstrates the core SVE vectorized copy pattern.
;;
;; ============================================================================

(require racket/format
         racket/list
         racket/pretty
         racket/class
         "../pipeline/main.rkt"
         "../ir/config.rkt"
         "../ir/types.rkt"
         "../interp/state.rkt"
         "../interp/sve.rkt")

;; ============================================================================
;; Simplified memcpy - single iteration (no loop, fixed length)
;; ============================================================================
;;
;; This version copies exactly VL bytes (vector length bytes)
;; Demonstrates: whilelt, ld1w, st1w (word-granularity for simplicity)

(define memcpy-sve-single-fn
  '(asm-fn memcpy_sve_single
     ;; Copy one vector's worth of 32-bit words
     ;; x0 = dst, x1 = src, x2 = word count (should be <= VL/4)
     ([x0 : (ptr u32)]
      [x1 : (ptr u32)]
      [x2 : u64])
     -> void

     ;; Generate predicate for active lanes
     (whilelt p0 xzr x2)

     ;; Load words from source
     (ld1w z0 p0 [x1])

     ;; Store words to destination
     (st1w z0 p0 [x0])

     ;; Return
     (ret)))

;; ============================================================================
;; Test: Compile and run with interpreter
;; ============================================================================

(module+ main
  (displayln "============================================")
  (displayln "SVE memcpy Use Case (Working Example)")
  (displayln "============================================")
  (newline)

  ;; Show source
  (displayln "Source code:")
  (displayln "-------------")
  (pretty-print memcpy-sve-single-fn)
  (newline)

  ;; Compile
  (displayln "Compiling with SVE-128 configuration...")
  (define compile-result
    (compile-function memcpy-sve-single-fn #:config config/sve))

  (printf "Compilation: ~a\n"
          (if (CompileResult-success? compile-result) "SUCCESS" "FAILED"))

  (when (not (CompileResult-success? compile-result))
    (printf "Errors: ~a\n" (CompileResult-errors compile-result)))
  (newline)

  ;; Create interpreter and run manually
  (when (CompileResult-success? compile-result)
    (displayln "Setting up SVE interpreter...")
    (displayln "------------------------------")

    ;; Create SVE interpreter with config
    (define interp (make-sve-interp #:config config/sve))

    ;; VL = 128 bits = 16 bytes = 4 words (32-bit each)
    (define vl-words 4)
    (printf "Vector length: 128 bits = ~a words\n" vl-words)
    (newline)

    ;; Simulate: copy 3 words (x2 = 3)
    ;; This means first 3 lanes active, 4th lane inactive
    (displayln "Test case: copy 3 words")
    (displayln "------------------------")

    ;; Set up registers
    ;; x0 = dst address (simulated as 0x1000)
    ;; x1 = src address (simulated as 0x2000)
    ;; x2 = word count = 3
    (send interp write-x 0 #x1000)  ; dst
    (send interp write-x 1 #x2000)  ; src
    (send interp write-x 2 3)       ; count = 3 words

    (printf "Initial state:\n")
    (printf "  x0 (dst) = 0x~x\n" (send interp read-x 0))
    (printf "  x1 (src) = 0x~x\n" (send interp read-x 1))
    (printf "  x2 (count) = ~a\n" (send interp read-x 2))
    (newline)

    ;; Execute whilelt instruction manually
    (displayln "Executing: (whilelt p0 xzr x2)")
    (send interp exec-insn
          (Insn:whilelt (Reg:p 0) (Reg:xzr) (Reg:x 2)))

    (define state-after-whilelt (send interp get-state))
    (define p-mask (state-read-p state-after-whilelt 0))
    (printf "  p0 predicate mask = 0b~a (binary)\n"
            (number->string p-mask 2))
    (printf "  Active lanes: ~a (first 3 of 4 lanes)\n"
            (for/sum ([i (in-range 4)])
              (if (bitwise-bit-set? p-mask i) 1 0)))
    (newline)

    ;; Note about memory operations
    (displayln "Memory operations (conceptual):")
    (displayln "  ld1w z0, p0/z, [x1]  ; Load from 0x2000 with predicate")
    (displayln "  st1w z0, p0, [x0]    ; Store to 0x1000 with predicate")
    (displayln "  Only lanes 0,1,2 would be active; lane 3 inactive")
    (newline)

    ;; Show what happens with different counts
    (displayln "Predicate generation for different counts:")
    (displayln "-------------------------------------------")
    (for ([count '(1 2 3 4 5 8)])
      (send interp write-x 2 count)
      (send interp exec-insn
            (Insn:whilelt (Reg:p 0) (Reg:xzr) (Reg:x 2)))
      (define s (send interp get-state))
      (define mask (state-read-p s 0))
      (printf "  count=~a: p0=0b~a (~a active lanes of 4)\n"
              count
              (~a (number->string mask 2) #:min-width 4 #:pad-string "0")
              (min count 4))))

  (newline)
  (displayln "============================================")
  (displayln "SVE memcpy demonstration complete!")
  (displayln "============================================"))

;; ============================================================================
;; Full memcpy pseudocode (for reference)
;; ============================================================================
;;
;; The complete SVE memcpy would look like:
;;
;;   memcpy_sve:
;;       cbz     x2, .exit           // if len == 0, return
;;   .loop:
;;       whilelt p0.b, xzr, x2       // p0 = (lane < remaining)
;;       ld1b    z0.b, p0/z, [x1]    // load bytes
;;       st1b    z0.b, p0, [x0]      // store bytes
;;       incb    x0                  // dst += VL
;;       incb    x1                  // src += VL
;;       decb    x2                  // len -= VL (saturating)
;;       b.gt    .loop               // continue if len > 0
;;   .exit:
;;       ret
;;
;; Instructions not yet in parser (TODO):
;;   - cbz (compare and branch if zero)
;;   - incb/inch/incw/incd (increment by VL elements)
;;   - decb/dech/decw/decd (decrement by VL elements)
;;   - ld1b/st1b (byte-granularity SVE load/store)
;;
;; ============================================================================

(provide memcpy-sve-single-fn)
