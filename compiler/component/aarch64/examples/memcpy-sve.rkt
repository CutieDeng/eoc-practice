#lang racket/base

;; ============================================================================
;; SVE memcpy Use Case
;; ============================================================================
;;
;; This example demonstrates using the aarch64-sve-asm framework to implement
;; a vectorized memory copy operation using SVE (Scalable Vector Extension).
;;
;; Function signature:
;;   void memcpy_sve(void* dst, void* src, size_t len)
;;
;; SVE advantages:
;;   - Vector-length agnostic: same code works on 128-bit to 2048-bit SVE
;;   - Predicated operations: handles non-VL-aligned lengths naturally
;;   - No explicit remainder loop needed
;;
;; ============================================================================

(require racket/format
         racket/list
         racket/pretty
         "../pipeline/main.rkt"
         "../ir/config.rkt")

;; ============================================================================
;; SVE memcpy Implementation
;; ============================================================================

;; The memcpy function using SVE byte operations
;;
;; Register allocation:
;;   x0 = dst pointer
;;   x1 = src pointer
;;   x2 = remaining length (decremented in loop)
;;   p0 = predicate (active lanes for current iteration)
;;   z0 = data vector
;;
;; Algorithm:
;;   while (len > 0) {
;;     p0 = whilelt(0, len)   // Generate predicate for active lanes
;;     z0 = ld1b(p0, [src])   // Load bytes (inactive lanes unchanged)
;;     st1b(z0, p0, [dst])    // Store bytes (inactive lanes not written)
;;     src += VL              // Advance source pointer by vector length
;;     dst += VL              // Advance dest pointer by vector length
;;     len -= VL              // Decrement remaining count
;;   }

(define memcpy-sve-fn
  '(asm-fn memcpy_sve
     ;; Parameters: dst pointer, src pointer, length
     ([x0 : (ptr u8)]      ; dst
      [x1 : (ptr u8)]      ; src
      [x2 : u64])          ; len
     -> void

     ;; Entry: check if length is zero
     (block entry
       (cbz x2 exit))       ; if len == 0, skip to exit

     ;; Main copy loop
     (block loop
       ;; Generate predicate: p0 = (lane_index < remaining_len)
       ;; For VL=128 bits (16 bytes), if len=10:
       ;;   p0 = 0b0000001111111111 (first 10 lanes active)
       (whilelt p0 xzr x2)

       ;; Load bytes from source with predicate
       ;; Only active lanes are loaded
       (ld1b z0 p0 [x1])

       ;; Store bytes to destination with predicate
       ;; Only active lanes are written
       (st1b z0 p0 [x0])

       ;; Increment pointers by vector length (in bytes)
       ;; incb increments by VL/8 (number of bytes in vector)
       (incb x0)
       (incb x1)

       ;; Decrement remaining length by VL bytes
       ;; decb decrements by VL/8
       (decb x2)

       ;; Continue if more bytes remain (x2 > 0)
       ;; Note: decb sets flags, so we can branch directly
       (b.gt loop))

     ;; Exit point
     (block exit
       (ret))))

;; ============================================================================
;; Test Harness
;; ============================================================================

;; Helper: create a memory buffer initialized with values
(define (make-test-buffer size fill-fn)
  (for/list ([i (in-range size)])
    (fill-fn i)))

;; Helper: display buffer contents
(define (display-buffer name buf)
  (printf "~a: [" name)
  (for ([b (in-list buf)]
        [i (in-naturals)])
    (when (> i 0) (printf " "))
    (printf "~a" b))
  (printf "]\n"))

;; ============================================================================
;; Run Tests
;; ============================================================================

(module+ main
  (displayln "=" )
  (displayln "SVE memcpy Use Case")
  (displayln "=")
  (newline)

  ;; Step 1: Parse and validate the function
  (displayln "Step 1: Parsing memcpy_sve function...")
  (displayln "----------------------------------------")
  (displayln "Source code:")
  (pretty-print memcpy-sve-fn)
  (newline)

  ;; Step 2: Compile the function
  (displayln "Step 2: Compiling with SVE-256 configuration...")
  (displayln "------------------------------------------------")

  (define compile-result
    (compile-function memcpy-sve-fn #:config config/sve-256))

  (if (CompileResult-success? compile-result)
      (begin
        (displayln "Compilation successful!")
        (printf "  - SVE enabled: ~a\n" (AsmConfig-sve? config/sve-256))
        (printf "  - Vector length: ~a bits (~a bytes)\n"
                (AsmConfig-vl config/sve-256)
                (quotient (AsmConfig-vl config/sve-256) 8)))
      (begin
        (displayln "Compilation failed!")
        (printf "  Errors: ~a\n" (CompileResult-errors compile-result))))
  (newline)

  ;; Step 3: Show the IR
  (when (CompileResult-success? compile-result)
    (displayln "Step 3: Generated IR structure")
    (displayln "-------------------------------")
    (define fn-ir (CompileResult-ir compile-result))
    (printf "  Function name: ~a\n" (AsmFunction-name fn-ir))
    (printf "  Parameters: ~a\n"
            (for/list ([p (AsmFunction-params fn-ir)])
              (format "[~a : ~a]"
                      (AsmParam-reg p)
                      (AsmParam-type p))))
    (printf "  Return type: ~a\n" (AsmFunction-ret-type fn-ir))
    (newline))

  ;; Step 4: Demonstrate interpretation (simulated memory)
  (displayln "Step 4: Simulated execution")
  (displayln "----------------------------")
  (displayln "Note: Full memory simulation requires memory model extension.")
  (displayln "Showing instruction trace concept:")
  (newline)

  ;; Show what the loop would do for different lengths
  (for ([len '(4 16 20 32)])
    (define vl-bytes 32)  ; SVE-256 = 32 bytes
    (define iterations (ceiling (/ len vl-bytes)))
    (printf "  len=~a bytes:\n" len)
    (printf "    - VL = ~a bytes (SVE-256)\n" vl-bytes)
    (printf "    - Loop iterations: ~a\n" iterations)
    (printf "    - Last iteration active lanes: ~a\n"
            (let ([remainder (modulo len vl-bytes)])
              (if (= remainder 0) vl-bytes remainder)))
    (newline))

  ;; Step 5: Show assembly-like output
  (displayln "Step 5: Pseudo-assembly output")
  (displayln "-------------------------------")
  (displayln #<<ASM
memcpy_sve:
    // x0 = dst, x1 = src, x2 = len
    cbz     x2, .exit           // if len == 0, return

.loop:
    whilelt p0.b, xzr, x2       // p0 = (lane < remaining)
    ld1b    z0.b, p0/z, [x1]    // load bytes with predicate
    st1b    z0.b, p0, [x0]      // store bytes with predicate
    incb    x0                  // dst += VL
    incb    x1                  // src += VL
    decb    x2                  // len -= VL
    b.gt    .loop               // continue if len > 0

.exit:
    ret
ASM
             )
  (newline)

  ;; Step 6: Comparison with scalar version
  (displayln "Step 6: Comparison with scalar implementation")
  (displayln "----------------------------------------------")
  (displayln "Scalar memcpy (for reference):")
  (displayln #<<SCALAR
memcpy_scalar:
    cbz     x2, .exit
.loop:
    ldrb    w3, [x1], #1        // load byte, post-increment
    strb    w3, [x0], #1        // store byte, post-increment
    subs    x2, x2, #1          // len--
    b.ne    .loop
.exit:
    ret
SCALAR
             )
  (newline)

  (displayln "Performance comparison (theoretical):")
  (displayln "  SVE-256 (32 bytes/iter): ~N/32 iterations")
  (displayln "  SVE-512 (64 bytes/iter): ~N/64 iterations")
  (displayln "  Scalar (1 byte/iter):    ~N iterations")
  (displayln "  Speedup: up to 32x-64x for large buffers")
  (newline)

  (displayln "=")
  (displayln "Use case demonstration complete!")
  (displayln "="))

;; ============================================================================
;; Additional SVE memcpy variants
;; ============================================================================

;; memcpy with 32-bit word granularity (for aligned data)
(define memcpy-sve-words-fn
  '(asm-fn memcpy_sve_words
     ([x0 : (ptr u32)]     ; dst (word-aligned)
      [x1 : (ptr u32)]     ; src (word-aligned)
      [x2 : u64])          ; word count
     -> void

     (block entry
       (cbz x2 exit))

     (block loop
       (whilelt p0 xzr x2)
       (ld1w z0 p0 [x1])   ; Load words
       (st1w z0 p0 [x0])   ; Store words
       (incw x0)           ; Increment by VL/4 words
       (incw x1)
       (decw x2)           ; Decrement word count
       (b.gt loop))

     (block exit
       (ret))))

;; memcpy with 64-bit doubleword granularity
(define memcpy-sve-dwords-fn
  '(asm-fn memcpy_sve_dwords
     ([x0 : (ptr u64)]     ; dst (dword-aligned)
      [x1 : (ptr u64)]     ; src (dword-aligned)
      [x2 : u64])          ; dword count
     -> void

     (block entry
       (cbz x2 exit))

     (block loop
       (whilelt p0 xzr x2)
       (ld1d z0 p0 [x1])   ; Load doublewords
       (st1d z0 p0 [x0])   ; Store doublewords
       (incd x0)           ; Increment by VL/8 dwords
       (incd x1)
       (decd x2)           ; Decrement dword count
       (b.gt loop))

     (block exit
       (ret))))

;; Export for use in other modules
(provide memcpy-sve-fn
         memcpy-sve-words-fn
         memcpy-sve-dwords-fn)
