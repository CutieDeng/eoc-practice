#lang racket/base

;; ============================================================
;; Java Frontend: JVM 数据读取器
;; ============================================================
;;
;; 读取 Java 工具生成的序列化数据，转换为 core/jvm.rkt 的结构
;; ============================================================

(require racket/match)
(require racket/list)
(require racket/file)
(require "../../core/jvm.rkt")

;; === 读取序列化文件 ===

(define (read-jvm-class-file path)
  (define data (file->value path))
  (parse-jvm-class data))

(provide read-jvm-class-file)

;; === 从 datum 解析 JvmClass ===

;; 原始格式 (来自 JvmBytecodeWrapper):
;; (Class name access fields methods annotations inner-classes)

(define (parse-jvm-class datum)
  (match datum
    [`(Class ,name ,access ,fields ,methods ,annotations ,inner-classes)
      (JvmClass
        (JvmVersion 52 0)  ; 默认 Java 8，实际版本需从属性获取
        name
        access
        #f                 ; super-class (需从属性获取)
        '()                ; interfaces
        (map parse-jvm-field fields)
        (map parse-jvm-method methods)
        '()                ; attributes
        (map parse-jvm-annotation annotations)
        (map parse-jvm-inner-class inner-classes))]
    [_ (error 'parse-jvm-class "Invalid class datum: ~a" datum)]))

(provide parse-jvm-class)

;; === 解析 JvmField ===

(define (parse-jvm-field datum)
  (match datum
    [`(Field ,name ,descriptor ,access ,value)
      (JvmField name descriptor access value '() '())]
    [_ (error 'parse-jvm-field "Invalid field datum: ~a" datum)]))

;; === 解析 JvmMethod ===

(define (parse-jvm-method datum)
  (match datum
    [`(Method ,name ,descriptor ,access ,exceptions ,local-vars ,insns ,line-infos)
      (JvmMethod
        name
        descriptor
        access
        0                  ; max-stack (需计算或从属性获取)
        0                  ; max-locals (需计算或从属性获取)
        (map parse-jvm-insn insns)
        '()                ; exception-table (需从 TRY-CATCH-BLOCK 指令提取)
        (map parse-jvm-local-var local-vars)
        (map parse-jvm-line-number line-infos)
        '()                ; attributes
        '()                ; annotations
        '())]              ; param-annots
    [_ (error 'parse-jvm-method "Invalid method datum: ~a" datum)]))

;; === 解析 JvmInsn ===

(define (parse-jvm-insn datum)
  (match datum
    [`(Insn ,opcode ,operands)
      (JvmInsn opcode operands)]
    [_ (error 'parse-jvm-insn "Invalid insn datum: ~a" datum)]))

;; === 解析 JvmLocalVar ===

(define (parse-jvm-local-var datum)
  (match datum
    [`#(,name ,descriptor ,start ,end ,index)
      (JvmLocalVar name descriptor #f start end index)]
    [(list name descriptor start end index)
      (JvmLocalVar name descriptor #f start end index)]
    [_ (error 'parse-jvm-local-var "Invalid local-var datum: ~a" datum)]))

;; === 解析 JvmLineNumber ===

(define (parse-jvm-line-number datum)
  (match datum
    [`(Line-Info ,line ,label)
      (JvmLineNumber line label)]
    [_ (error 'parse-jvm-line-number "Invalid line-number datum: ~a" datum)]))

;; === 解析 JvmAnnotation ===

(define (parse-jvm-annotation datum)
  (match datum
    [`(Annotation ,name ,values)
      (JvmAnnotation name values #t)]
    [_ (error 'parse-jvm-annotation "Invalid annotation datum: ~a" datum)]))

;; === 解析 JvmInnerClass ===

(define (parse-jvm-inner-class datum)
  (match datum
    [`(InnerClass ,name ,access ,outer-name ,inner-name)
      (JvmInnerClass name outer-name inner-name access)]
    [_ (error 'parse-jvm-inner-class "Invalid inner-class datum: ~a" datum)]))

;; === 提取异常表 ===

;; 从指令序列中提取 TRY-CATCH-BLOCK 伪指令
(define (extract-exception-table insns)
  (for/list ([insn insns]
             #:when (and (JvmInsn? insn)
                        (eq? (JvmInsn-opcode insn) 'TRY-CATCH-BLOCK)))
    (match (JvmInsn-operands insn)
      [`(,start ,end ,handler ,type)
        (JvmExceptionEntry start end handler type)]
      [_ (error 'extract-exception-table "Invalid try-catch datum")])))

(provide extract-exception-table)

;; === 过滤掉伪指令 ===

(define (filter-real-insns insns)
  (filter (lambda (insn)
            (and (JvmInsn? insn)
                 (not (member (JvmInsn-opcode insn)
                             '(TRY-CATCH-BLOCK CUTIEDENG-LABEL)))))
          insns))

(provide filter-real-insns)

;; === 提取标签 ===

(define (extract-labels insns)
  (for/list ([insn insns]
             #:when (and (JvmInsn? insn)
                        (eq? (JvmInsn-opcode insn) 'CUTIEDENG-LABEL)))
    (match (JvmInsn-operands insn)
      [`(,label) label]
      [_ (error 'extract-labels "Invalid label datum")])))

(provide extract-labels)
