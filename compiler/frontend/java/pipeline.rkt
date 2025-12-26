#lang racket/base

;; ============================================================
;; Java Frontend: 完整管线
;; ============================================================
;;
;; .class 文件 → JVM IR → CFG → RVSDG
;; ============================================================

(require racket/match racket/list)
(require "reader.rkt")
(require "../../core/jvm.rkt")
(require "../../core/cfg.rkt")
(require "../../transform/jvm-to-cfg.rkt")
(require "../../transform/cfg-to-rvsdg.rkt")
(require "../../rvsdg/core-def.rkt")

;; === 完整管线 ===

;; 从序列化数据文件到 RVSDG
(define (java-file->rvsdg dat-file)
  (define jvm-class (read-jvm-class-file dat-file))
  (java-class->rvsdg jvm-class))

(provide java-file->rvsdg)

;; 从 JvmClass 到 RVSDG（每个方法一个 Region）
(define (java-class->rvsdg jvm-class)
  (for/list ([method (JvmClass-methods jvm-class)])
    (cons (JvmMethod-name method)
          (java-method->rvsdg method))))

(provide java-class->rvsdg)

;; 从 JvmMethod 到 RVSDG Region
(define (java-method->rvsdg method)
  (define cfg (jvm-method->cfg method))
  (cfg->rvsdg cfg))

(provide java-method->rvsdg)

;; === 分步管线（用于调试）===

;; JVM → CFG
(define (java-method->cfg method)
  (jvm-method->cfg method))

(provide java-method->cfg)

;; === 管线信息查询 ===

;; 获取类的方法列表
(define (java-class-methods jvm-class)
  (for/list ([m (JvmClass-methods jvm-class)])
    (list (JvmMethod-name m)
          (JvmMethod-descriptor m))))

(provide java-class-methods)

;; 获取方法的基本信息
(define (java-method-info method)
  (list
    (cons 'name (JvmMethod-name method))
    (cons 'descriptor (JvmMethod-descriptor method))
    (cons 'access (JvmMethod-access method))
    (cons 'max-stack (JvmMethod-max-stack method))
    (cons 'max-locals (JvmMethod-max-locals method))
    (cons 'insn-count (length (JvmMethod-insns method)))))

(provide java-method-info)

;; === 测试入口 ===

(module+ main
  (require racket/cmdline)

  (define input-file #f)
  (define method-name #f)
  (define show-cfg #f)
  (define show-rvsdg #f)

  (command-line
    #:program "java-pipeline"
    #:once-each
    [("-i" "--input") file "Input .dat file" (set! input-file file)]
    [("-m" "--method") name "Method to analyze" (set! method-name name)]
    [("--cfg") "Show CFG" (set! show-cfg #t)]
    [("--rvsdg") "Show RVSDG" (set! show-rvsdg #t)]
    #:args ()
    (when input-file
      (define jvm-class (read-jvm-class-file input-file))
      (displayln (format "Class: ~a" (JvmClass-name jvm-class)))
      (displayln "Methods:")
      (for ([m (JvmClass-methods jvm-class)])
        (displayln (format "  ~a ~a" (JvmMethod-name m) (JvmMethod-descriptor m)))

        (when (or (not method-name)
                  (equal? method-name (JvmMethod-name m)))
          (when show-cfg
            (displayln "  CFG:")
            (define cfg (java-method->cfg m))
            (displayln (format "    Blocks: ~a" (cfg-block-count cfg))))

          (when show-rvsdg
            (displayln "  RVSDG:")
            (define region (java-method->rvsdg m))
            (displayln (format "    Nodes: ~a" (rvsdg-raw/node-count region)))))))))

;; 需要的额外导入
(require "../../cfg/raw.rkt")
(require "../../rvsdg/raw/query.rkt")
