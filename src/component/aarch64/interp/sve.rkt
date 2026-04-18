#lang racket/base

;; AArch64 SVE Interpreter Extension
;;
;; Extends the base interpreter to handle SVE (Scalable Vector Extension)
;; instructions. Uses class inheritance to override instruction execution.

(require racket/class
         racket/match
         cutie-ftree/pvector
         cutie-ftree/ordered-map
         "../ir/types.rkt"
         "../ir/cfg.rkt"
         "../ir/config.rkt"
         "state.rkt"
         "base.rkt")

(provide
 ;; SVE interpreter class
 interp-aarch64-sve%

 ;; SVE helper functions
 make-sve-interp
 run-sve-cfg
 run-sve-insn)

;; ============================================================================
;; SVE Interpreter Class
;; ============================================================================

(define interp-aarch64-sve%
  (class interp-aarch64-base%
    (super-new)

    (inherit-field state)
    (inherit read-reg write-reg
             read-x write-x
             eval-operand
             update-flags-nz test-condition)

    ;; Override instruction execution to handle SVE
    (define/override (exec-insn insn)
      (match insn
        ;; SVE arithmetic with predication
        [(Insn:sve op pred dst srcs)
         (exec-sve-arith op pred dst srcs)]

        ;; SVE contiguous load
        [(Insn:sve-load op pred dst addr)
         (exec-sve-load op pred dst addr)]

        ;; SVE contiguous store
        [(Insn:sve-store op pred src addr)
         (exec-sve-store op pred src addr)]

        ;; SVE reduction
        [(Insn:sve-reduce op pred dst src)
         (exec-sve-reduce op pred dst src)]

        ;; SVE comparison
        [(Insn:sve-cmp op pd pg src1 src2)
         (exec-sve-cmp op pd pg src1 src2)]

        ;; SVE predicate operations
        [(Insn:sve-pred-op op pd pg pn)
         (exec-sve-pred-op op pd pg pn)]

        ;; whilelt - generate predicate from loop counter
        [(Insn:whilelt pd rn rm)
         (exec-whilelt pd rn rm)]

        ;; Fall back to base class for non-SVE instructions
        [_ (super exec-insn insn)]))

    ;; ========================================================================
    ;; SVE Arithmetic Operations
    ;; ========================================================================

    (define/public (exec-sve-arith op pred dst srcs)
      (define vl (MachineState-vl state))
      (define elem-size (sve-op-elem-size op))
      (define num-elems (quotient vl elem-size))

      (define p-mask (read-p-reg pred))
      (define src1 (car srcs))
      (define src2 (cadr srcs))

      (define src1-vec (read-z-reg src1))
      (define src2-vec (read-z-reg src2))
      (define dst-vec (read-z-reg dst))

      (define result-vec
        (for/fold ([result dst-vec])
                  ([i (in-range num-elems)])
          (if (pred-active? p-mask i)
              (let* ([v1 (pvector-ref src1-vec i)]
                     [v2 (pvector-ref src2-vec i)]
                     [res (case op
                            [(add) (+ v1 v2)]
                            [(sub) (- v1 v2)]
                            [(mul) (* v1 v2)]
                            [(fadd) (+ v1 v2)]  ; Simplified float as real
                            [(fsub) (- v1 v2)]
                            [(fmul) (* v1 v2)]
                            [(and) (bitwise-and v1 v2)]
                            [(orr) (bitwise-ior v1 v2)]
                            [(eor) (bitwise-xor v1 v2)]
                            [else (error 'exec-sve-arith "unknown op: ~a" op)])])
                (pvector-set result i res))
              result)))

      (write-z-reg dst result-vec))

    ;; ========================================================================
    ;; SVE Load Operations
    ;; ========================================================================

    (define/public (exec-sve-load op pred dst addr)
      (define vl (MachineState-vl state))
      (define elem-size (sve-load-elem-size op))
      (define num-elems (quotient vl elem-size))

      (define p-mask (read-p-reg pred))
      (define base-addr (read-memory-addr addr))
      (define dst-vec (read-z-reg dst))

      (define result-vec
        (for/fold ([result dst-vec])
                  ([i (in-range num-elems)])
          (if (pred-active? p-mask i)
              (let* ([elem-addr (+ base-addr (* i elem-size))]
                     [val (read-memory-by-size elem-addr elem-size)])
                (pvector-set result i val))
              result)))

      (write-z-reg dst result-vec))

    ;; ========================================================================
    ;; SVE Store Operations
    ;; ========================================================================

    (define/public (exec-sve-store op pred src addr)
      (define vl (MachineState-vl state))
      (define elem-size (sve-store-elem-size op))
      (define num-elems (quotient vl elem-size))

      (define p-mask (read-p-reg pred))
      (define base-addr (read-memory-addr addr))
      (define src-vec (read-z-reg src))

      (for ([i (in-range num-elems)])
        (when (pred-active? p-mask i)
          (define elem-addr (+ base-addr (* i elem-size)))
          (define val (pvector-ref src-vec i))
          (write-memory-by-size elem-addr elem-size val))))

    ;; ========================================================================
    ;; SVE Reduction Operations
    ;; ========================================================================

    (define/public (exec-sve-reduce op pred dst src)
      (define vl (MachineState-vl state))
      (define elem-size (sve-reduce-elem-size op))
      (define num-elems (quotient vl elem-size))

      (define p-mask (read-p-reg pred))
      (define src-vec (read-z-reg src))

      (define result
        (for/fold ([acc (reduce-init op)])
                  ([i (in-range num-elems)])
          (if (pred-active? p-mask i)
              (let ([val (pvector-ref src-vec i)])
                (case op
                  [(addv uaddv saddv faddv) (+ acc val)]
                  [(fmaxv) (max acc val)]
                  [(fminv) (min acc val)]
                  [(andv) (bitwise-and acc val)]
                  [(orv) (bitwise-ior acc val)]
                  [(eorv) (bitwise-xor acc val)]
                  [else (error 'exec-sve-reduce "unknown op: ~a" op)]))
              acc)))

      ;; Write result to scalar register (s0, d0, etc.)
      (write-scalar-result dst result))

    ;; ========================================================================
    ;; SVE Comparison Operations
    ;; ========================================================================

    (define/public (exec-sve-cmp op pd pg src1 src2)
      (define vl (MachineState-vl state))
      (define elem-size (sve-cmp-elem-size op))
      (define num-elems (quotient vl elem-size))

      (define gov-mask (read-p-reg pg))
      (define src1-vec (read-z-reg src1))
      (define src2-vec (read-z-reg src2))

      (define result-mask
        (for/fold ([mask 0])
                  ([i (in-range num-elems)])
          (if (pred-active? gov-mask i)
              (let* ([v1 (pvector-ref src1-vec i)]
                     [v2 (pvector-ref src2-vec i)]
                     [cmp-result
                      (case op
                        [(cmpeq fcmeq) (= v1 v2)]
                        [(cmpne fcmne) (not (= v1 v2))]
                        [(cmplt fcmlt) (< v1 v2)]
                        [(cmple fcmle) (<= v1 v2)]
                        [(cmpgt fcmgt) (> v1 v2)]
                        [(cmpge fcmge) (>= v1 v2)]
                        [else (error 'exec-sve-cmp "unknown op: ~a" op)])])
                (if cmp-result
                    (bitwise-ior mask (arithmetic-shift 1 i))
                    mask))
              mask)))

      (write-p-reg pd result-mask))

    ;; ========================================================================
    ;; SVE Predicate Operations
    ;; ========================================================================

    (define/public (exec-sve-pred-op op pd pg pn)
      (case op
        [(ptrue) (exec-ptrue pd)]
        [(pfalse) (write-p-reg pd 0)]
        [(pnext) (exec-pnext pd pg pn)]
        [(pfirst) (exec-pfirst pd pg pn)]
        [(brka) (exec-brka pd pg pn)]
        [(brkb) (exec-brkb pd pg pn)]
        [else (error 'exec-sve-pred-op "unknown op: ~a" op)]))

    (define/public (exec-ptrue pd)
      (define vl (MachineState-vl state))
      (define num-elems (quotient vl 4)) ; Assume 32-bit elements
      ;; All elements active
      (define mask (sub1 (arithmetic-shift 1 num-elems)))
      (write-p-reg pd mask))

    (define/public (exec-pnext pd pg pn)
      ;; Find next active element after current first active
      (define gov-mask (read-p-reg pg))
      (define cur-mask (read-p-reg pn))
      ;; Clear first active bit, mask with governing
      (define first-bit (bitwise-and cur-mask (- cur-mask)))
      (define cleared (bitwise-xor cur-mask first-bit))
      (define result (bitwise-and cleared gov-mask))
      (write-p-reg pd result))

    (define/public (exec-pfirst pd pg pn)
      ;; Keep only first active element
      (define gov-mask (read-p-reg pg))
      (define cur-mask (read-p-reg pn))
      (define combined (bitwise-and gov-mask cur-mask))
      (define first-bit (bitwise-and combined (- combined)))
      (write-p-reg pd first-bit))

    (define/public (exec-brka pd pg pn)
      ;; Break after - set predicate up to and including first active
      (define gov-mask (read-p-reg pg))
      (define cur-mask (read-p-reg pn))
      (define combined (bitwise-and gov-mask cur-mask))
      (if (zero? combined)
          (write-p-reg pd gov-mask)
          (let* ([first-pos (integer-length (bitwise-and combined (- combined)))]
                 [break-mask (sub1 (arithmetic-shift 1 first-pos))])
            (write-p-reg pd (bitwise-and gov-mask break-mask)))))

    (define/public (exec-brkb pd pg pn)
      ;; Break before - set predicate up to but not including first active
      (define gov-mask (read-p-reg pg))
      (define cur-mask (read-p-reg pn))
      (define combined (bitwise-and gov-mask cur-mask))
      (if (zero? combined)
          (write-p-reg pd gov-mask)
          (let* ([first-pos (sub1 (integer-length (bitwise-and combined (- combined))))]
                 [break-mask (if (< first-pos 0) 0 (sub1 (arithmetic-shift 1 first-pos)))])
            (write-p-reg pd (bitwise-and gov-mask break-mask)))))

    ;; ========================================================================
    ;; whilelt - Generate predicate from loop counter
    ;; ========================================================================

    (define/public (exec-whilelt pd rn rm)
      (define vl (MachineState-vl state))
      (define elem-size 4) ; Assume 32-bit elements
      (define num-elems (quotient vl elem-size))

      (define n-val (match rn [(Reg:x id) (read-x id)] [_ (eval-operand rn)]))
      (define m-val (match rm [(Reg:x id) (read-x id)] [_ (eval-operand rm)]))

      ;; Generate predicate where each element i is active if (n + i) < m
      (define mask
        (for/fold ([m 0])
                  ([i (in-range num-elems)])
          (if (< (+ n-val i) m-val)
              (bitwise-ior m (arithmetic-shift 1 i))
              m)))

      (write-p-reg pd mask)
      ;; Update flags based on result (Z flag set if all lanes inactive)
      (update-flags-nz (if (zero? mask) 0 1) 64))

    ;; ========================================================================
    ;; Helper Functions
    ;; ========================================================================

    ;; Read Z register as pvector
    (define/public (read-z-reg reg)
      (match reg
        [(Reg:z id) (state-read-z state id)]
        [_ (error 'read-z-reg "not a z register: ~a" reg)]))

    ;; Write Z register as pvector
    (define/public (write-z-reg reg vec)
      (match reg
        [(Reg:z id)
         (set! state (state-write-z state id vec))]
        [_ (error 'write-z-reg "not a z register: ~a" reg)]))

    ;; Read P register as bitmask
    (define/public (read-p-reg reg)
      (match reg
        [(Reg:p id) (state-read-p state id)]
        [_ (error 'read-p-reg "not a p register: ~a" reg)]))

    ;; Write P register as bitmask
    (define/public (write-p-reg reg mask)
      (match reg
        [(Reg:p id)
         (set! state (state-write-p state id mask))]
        [_ (error 'write-p-reg "not a p register: ~a" reg)]))

    ;; Check if predicate lane is active
    (define (pred-active? mask i)
      (not (zero? (bitwise-and mask (arithmetic-shift 1 i)))))

    ;; Read memory address from operand
    (define/public (read-memory-addr addr)
      (match addr
        [(Mem:base (Reg:x id)) (read-x id)]
        [(Mem:offset (Reg:x id) off) (+ (read-x id) off)]
        [_ (error 'read-memory-addr "unsupported addr: ~a" addr)]))

    ;; Read memory by element size
    (define/public (read-memory-by-size addr size)
      (case size
        [(1) (state-read-byte state addr)]
        [(2) (state-read-halfword state addr)]
        [(4) (state-read-word state addr)]
        [(8) (state-read-doubleword state addr)]
        [else (error 'read-memory-by-size "unsupported size: ~a" size)]))

    ;; Write memory by element size
    (define/public (write-memory-by-size addr size val)
      (case size
        [(1) (set! state (state-write-byte state addr val))]
        [(2) (set! state (state-write-halfword state addr val))]
        [(4) (set! state (state-write-word state addr val))]
        [(8) (set! state (state-write-doubleword state addr val))]
        [else (error 'write-memory-by-size "unsupported size: ~a" size)]))

    ;; Write scalar result for reductions
    (define/public (write-scalar-result dst val)
      ;; Simplified: write to x register based on dst register type
      (match dst
        [(Reg:s id) (write-x id (exact-truncate val))]
        [(Reg:d id) (write-x id (exact-truncate val))]
        [(Reg:x id) (write-x id (exact-truncate val))]
        [_ (error 'write-scalar-result "unsupported dst: ~a" dst)]))

    ;; Get initial value for reduction
    (define (reduce-init op)
      (case op
        [(addv uaddv saddv faddv) 0]
        [(fmaxv) -inf.0]
        [(fminv) +inf.0]
        [(andv) -1]  ; All bits set
        [(orv eorv) 0]
        [else 0]))

    ;; Get element size for SVE operations
    (define (sve-op-elem-size op)
      (case op
        [(add sub mul and orr eor) 4]  ; 32-bit by default
        [(fadd fsub fmul) 4]           ; f32 by default
        [else 4]))

    (define (sve-load-elem-size op)
      (case op
        [(ld1b) 1]
        [(ld1h) 2]
        [(ld1w) 4]
        [(ld1d) 8]
        [else 4]))

    (define (sve-store-elem-size op)
      (case op
        [(st1b) 1]
        [(st1h) 2]
        [(st1w) 4]
        [(st1d) 8]
        [else 4]))

    (define (sve-reduce-elem-size op)
      (case op
        [(faddv fmaxv fminv) 4]  ; f32
        [else 4]))

    (define (sve-cmp-elem-size op)
      (case op
        [(fcmeq fcmne fcmlt fcmle fcmgt fcmge) 4]  ; f32
        [else 4]))

    (define (exact-truncate val)
      (if (exact-integer? val) val (inexact->exact (truncate val))))

    )) ;; End of interp-aarch64-sve%

;; ============================================================================
;; Helper Functions
;; ============================================================================

;; Create SVE interpreter with optional config
(define (make-sve-interp #:config [config #f])
  (new interp-aarch64-sve% [config config]))

;; Run SVE instruction
(define (run-sve-insn insn #:interp [interp (make-sve-interp)])
  (send interp exec-insn insn)
  (send interp get-state))

;; Run SVE CFG
(define (run-sve-cfg cfg #:interp [interp (make-sve-interp)])
  (send interp run-cfg cfg)
  (send interp get-state))
