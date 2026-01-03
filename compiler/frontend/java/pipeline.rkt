#lang racket/base

;; ============================================================
;; Java Frontend: 完整管线 (STUB)
;; ============================================================
;;
;; .class 文件 → JVM IR → CFG → RVSDG
;;
;; Note: This is currently a stub. The transform modules
;; need to be reimplemented in the new architecture.
;;
;; TODO: Reimplement when the JVM→CFG→RVSDG transforms are ready.
;; ============================================================

(require racket/match racket/list)
(require "reader.rkt")
(require "../../kernel/ir/jvm/types.rkt")
(require "../../kernel/ir/cfg/types.rkt")

;; === Stub Implementations ===

(define (java-file->rvsdg dat-file)
  (error 'java-file->rvsdg "Not yet implemented - transform modules need migration"))

(provide java-file->rvsdg)

(define (java-class->rvsdg jvm-class)
  (error 'java-class->rvsdg "Not yet implemented - transform modules need migration"))

(provide java-class->rvsdg)

(define (java-method->rvsdg method)
  (error 'java-method->rvsdg "Not yet implemented - transform modules need migration"))

(provide java-method->rvsdg)

(define (java-method->cfg method)
  (error 'java-method->cfg "Not yet implemented - transform modules need migration"))

(provide java-method->cfg)

;; === Working Functions ===

(define (java-class-methods jvm-class)
  (for/list ([m (JvmClass-methods jvm-class)])
    (list (JvmMethod-name m)
          (JvmMethod-descriptor m))))

(provide java-class-methods)

(define (java-method-info method)
  (list
    (cons 'name (JvmMethod-name method))
    (cons 'descriptor (JvmMethod-descriptor method))
    (cons 'access (JvmMethod-access-flags method))
    (cons 'max-stack (JvmMethod-max-stack method))
    (cons 'max-locals (JvmMethod-max-locals method))
    (cons 'insn-count (length (JvmMethod-instructions method)))))

(provide java-method-info)
