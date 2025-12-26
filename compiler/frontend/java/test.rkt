#lang racket/base

;; ============================================================
;; Java Frontend: 测试模块
;; ============================================================

(require rackunit)
(require racket/path)
(require "reader.rkt")
(require "pipeline.rkt")
(require "../../core/jvm.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "../../rvsdg/core-def.rkt")
(require "../../rvsdg/raw/query.rkt")

;; === 测试数据路径 ===

;; 使用绝对路径或相对于项目根目录的路径
(define test-dat-file
  (build-path (current-directory) "compiler/test/ClassTransform.dat"))

;; === Reader 测试 ===

(define reader-tests
  (test-suite "Java Reader Tests"

    (test-case "读取类文件"
      (define class-data (read-jvm-class-file test-dat-file))
      (check-pred JvmClass? class-data)
      (check-equal? (JvmClass-name class-data) "com/cutiedeng/ClassTransform"))

    (test-case "解析方法"
      (define class-data (read-jvm-class-file test-dat-file))
      (define methods (JvmClass-methods class-data))
      (check-true (> (length methods) 0))
      (check-true (andmap JvmMethod? methods)))

    (test-case "解析指令"
      (define class-data (read-jvm-class-file test-dat-file))
      (define method (car (JvmClass-methods class-data)))
      (define insns (JvmMethod-insns method))
      (check-true (list? insns))
      (check-true (andmap JvmInsn? insns)))))

;; === JVM → CFG 转换测试 ===

(define jvm-to-cfg-tests
  (test-suite "JVM to CFG Tests"

    (test-case "基本转换"
      (define class-data (read-jvm-class-file test-dat-file))
      (define method (car (JvmClass-methods class-data)))
      (define cfg (java-method->cfg method))
      (check-pred Cfg? cfg)
      (check-true (> (cfg-block-count cfg) 0)))

    (test-case "入口块设置"
      (define class-data (read-jvm-class-file test-dat-file))
      (define method (car (JvmClass-methods class-data)))
      (define cfg (java-method->cfg method))
      (check-pred BlockId? (cfg-get-entry cfg)))

    (test-case "块包含指令"
      (define class-data (read-jvm-class-file test-dat-file))
      (define method (car (JvmClass-methods class-data)))
      (define cfg (java-method->cfg method))
      (define entry (cfg-get-entry cfg))
      (define block (cfg-get-block cfg entry))
      (check-pred CfgBlock? block))))

;; === CFG → RVSDG 转换测试 ===

(define cfg-to-rvsdg-tests
  (test-suite "CFG to RVSDG Tests"

    (test-case "基本转换"
      (define class-data (read-jvm-class-file test-dat-file))
      (define method (car (JvmClass-methods class-data)))
      (define region (java-method->rvsdg method))
      (check-pred Region? region))

    (test-case "生成节点"
      (define class-data (read-jvm-class-file test-dat-file))
      (define method (car (JvmClass-methods class-data)))
      (define region (java-method->rvsdg method))
      (check-true (> (rvsdg-raw/node-count region) 0)))

    (test-case "所有方法可转换"
      (define class-data (read-jvm-class-file test-dat-file))
      (for ([method (JvmClass-methods class-data)])
        (define region (java-method->rvsdg method))
        (check-pred Region? region)))))

;; === 完整管线测试 ===

(define pipeline-tests
  (test-suite "Pipeline Tests"

    (test-case "java-class->rvsdg"
      (define class-data (read-jvm-class-file test-dat-file))
      (define results (java-class->rvsdg class-data))
      (check-true (list? results))
      (check-true (> (length results) 0))
      (for ([result results])
        (check-true (pair? result))
        (check-pred string? (car result))
        (check-pred Region? (cdr result))))))

;; === 运行测试 ===

(module+ test
  (require rackunit/text-ui)
  (run-tests reader-tests)
  (run-tests jvm-to-cfg-tests)
  (run-tests cfg-to-rvsdg-tests)
  (run-tests pipeline-tests))

(module+ main
  (require rackunit/text-ui)
  (displayln "=== Java Frontend Tests ===\n")
  (run-tests reader-tests)
  (run-tests jvm-to-cfg-tests)
  (run-tests cfg-to-rvsdg-tests)
  (run-tests pipeline-tests)
  (displayln "\nAll tests passed!"))
