#lang racket/base

;; AArch64 Assembly Configuration
;;
;; Configuration for SVE/SVE2 instruction generation,
;; vector length settings, and target-specific options.

(require racket/match)

(provide
 ;; Config structure (直接使用 struct 构造)
 (struct-out AsmConfig)

 ;; 便捷常量
 config/base        ; 基础配置 (无 SVE)
 config/sve         ; SVE 配置 (128-bit)
 config/sve-256     ; SVE 配置 (256-bit)
 config/sve-512     ; SVE 配置 (512-bit)
 config/sve2        ; SVE2 配置

 ;; Vector length utilities
 config-vl-bytes
 config-vl-elements

 ;; Target feature checking
 config-has-feature?

 ;; Known target features
 known-features)

;; ============================================================================
;; Configuration Structure
;; ============================================================================

;; AsmConfig - 直接用 struct 构造即可:
;;   (AsmConfig sve? sve2? vl features)
;;
;; 字段:
;; - sve?     : 是否启用 SVE
;; - sve2?    : 是否启用 SVE2 (隐含 SVE)
;; - vl       : 向量长度 (bits): 128, 256, 512, 1024, 2048
;; - features : 额外特性列表 (symbols)
(struct AsmConfig (sve? sve2? vl features) #:prefab)

;; ============================================================================
;; 便捷常量
;; ============================================================================

(define config/base     (AsmConfig #f #f 128 '()))
(define config/sve      (AsmConfig #t #f 128 '()))
(define config/sve-256  (AsmConfig #t #f 256 '()))
(define config/sve-512  (AsmConfig #t #f 512 '()))
(define config/sve2     (AsmConfig #t #t 256 '()))

;; ============================================================================
;; Vector Length Utilities
;; ============================================================================

;; Get vector length in bytes
(define (config-vl-bytes cfg)
  (quotient (AsmConfig-vl cfg) 8))

;; Get number of elements for a given element size (in bits)
(define (config-vl-elements cfg elem-bits)
  (quotient (AsmConfig-vl cfg) elem-bits))

;; ============================================================================
;; Target Features
;; ============================================================================

;; Check if a feature is enabled
(define (config-has-feature? cfg feature)
  (or (memq feature (AsmConfig-features cfg))
      ;; Implied features
      (and (eq? feature 'sve) (AsmConfig-sve? cfg))
      (and (eq? feature 'sve2) (AsmConfig-sve2? cfg))))

;; Known target features
(define known-features
  '(;; SIMD extensions
    sve          ; Scalable Vector Extension
    sve2         ; SVE2
    sve2-aes     ; SVE2 AES
    sve2-bitperm ; SVE2 bit permutation
    sve2-sha3    ; SVE2 SHA3
    sve2-sm4     ; SVE2 SM4

    ;; Other extensions
    fp16         ; Half-precision float
    bf16         ; BFloat16
    dotprod      ; Dot product
    i8mm         ; Int8 matrix multiply
    f32mm        ; FP32 matrix multiply
    f64mm        ; FP64 matrix multiply

    ;; Memory
    lse          ; Large System Extensions (atomics)
    lse2         ; LSE2

    ;; Crypto
    aes          ; AES
    sha2         ; SHA2
    sha3         ; SHA3
    sm3          ; SM3
    sm4          ; SM4

    ;; Branch
    bti          ; Branch Target Identification
    pauth        ; Pointer Authentication
    mte          ; Memory Tagging Extension
    ))
