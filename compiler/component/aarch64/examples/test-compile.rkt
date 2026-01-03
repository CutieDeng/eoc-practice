#lang racket/base

;; ============================================================================
;; Test: Full Compilation Pipeline
;; ============================================================================

(require racket/pretty
         "../main.rkt")

;; ============================================================================
;; Example 1: Simple add function
;; ============================================================================

(define add-fn
  '(asm-fn add_two
     ([x0 : i64] [x1 : i64])
     -> i64
     (add x0 x0 x1)
     (ret)))

;; ============================================================================
;; Example 2: SVE vector add (using virtual registers)
;; ============================================================================

(define vec-add-fn
  '(asm-fn vec_add
     ([x0 : (ptr f32)]   ; dst
      [x1 : (ptr f32)]   ; src1
      [x2 : (ptr f32)]   ; src2
      [x3 : i64])        ; len
     -> void

     ;; Use virtual registers
     (mov %dst x0)
     (mov %src1 x1)
     (mov %src2 x2)
     (mov %len x3)

     (block loop
       (whilelt %p.mask xzr %len)
       (ld1w %z.a %p.mask [%src1])
       (ld1w %z.b %p.mask [%src2])
       (fadd %z.c %p.mask %z.a %z.b)
       (st1w %z.c %p.mask [%dst])
       (incw %dst)
       (incw %src1)
       (incw %src2)
       (decw %len)
       (b.gt loop))

     (ret)))

;; ============================================================================
;; Main
;; ============================================================================

(module+ main
  (displayln "============================================")
  (displayln "AArch64-SVE-ASM Compiler Test")
  (displayln "============================================")
  (newline)

  ;; Test 1: Simple function
  (displayln "Test 1: Compiling add_two function")
  (displayln "-----------------------------------")
  (displayln "Source:")
  (pretty-print add-fn)
  (newline)

  (displayln "Generated Assembly:")
  (displayln "-------------------")
  (define asm1 (compile-to-string add-fn))
  (displayln asm1)
  (newline)

  ;; Write to file
  (compile add-fn "add_two.s")
  (displayln "Written to: add_two.s")
  (newline)

  ;; Test 2: Show pipeline stages
  (displayln "============================================")
  (displayln "Pipeline Stages Demo")
  (displayln "============================================")
  (newline)

  (displayln "Stage 1: Parse")
  (define-values (parsed _) (parse add-fn))
  (printf "  Function name: ~a\n" (AsmFunction-name parsed))
  (printf "  Parameters: ~a\n" (length (AsmFunction-params parsed)))
  (newline)

  (displayln "Stage 2: Validate")
  (define-values (validated verr) (validate parsed))
  (printf "  Valid: ~a\n" (if validated "yes" "no"))
  (newline)

  (displayln "Stage 3: Type Check")
  (define-values (checked terr) (type-check validated #f))
  (printf "  Type-correct: ~a\n" (if checked "yes" "no"))
  (newline)

  (displayln "Stage 4: Register Allocation")
  (define-values (allocated aerr) (allocate checked))
  (printf "  Allocated: ~a\n" (if allocated "yes" "no"))
  (newline)

  (displayln "Stage 5: Emit Assembly")
  (define-values (asm eerr) (emit allocated))
  (printf "  Output length: ~a bytes\n" (string-length asm))
  (newline)

  (displayln "============================================")
  (displayln "All tests complete!")
  (displayln "============================================"))
