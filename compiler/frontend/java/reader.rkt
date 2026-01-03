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
(require "../../kernel/ir/jvm/types.rkt")

;; === Java 工具输出的 prefab 结构 ===
;; 这些结构必须与 Java 端输出格式完全匹配

(struct Class (name access fields methods annotations inner-classes) #:prefab)
(struct Field (name desc access value) #:prefab)
(struct Method (name desc access exceptions local-vars insns line-infos) #:prefab)
(struct Insn (opcode args) #:prefab)
(struct Line-Info (line label) #:prefab)
(struct InnerClass (name outer inner access) #:prefab)
(struct Annotation (name values) #:prefab)

(provide (struct-out Class))
(provide (struct-out Field))
(provide (struct-out Method))
(provide (struct-out Insn))
(provide (struct-out Line-Info))
(provide (struct-out InnerClass))

;; === 读取序列化文件 ===

(define (read-jvm-class-file path)
  (define data (file->value path))
  (parse-jvm-class data))

(provide read-jvm-class-file)

;; === 从 prefab 解析 JvmClass ===

(define (parse-jvm-class datum)
  (JvmClass
    (JvmVersion 52 0)  ; 默认 Java 8
    (Class-name datum)
    (parse-access-flags (Class-access datum))
    #f                 ; super-class (需从属性获取)
    '()                ; interfaces
    (map parse-jvm-field (Class-fields datum))
    (map parse-jvm-method (Class-methods datum))
    '()                ; attributes
    '()                ; annotations
    (map parse-jvm-inner-class (Class-inner-classes datum))))

(provide parse-jvm-class)

;; === 解析访问标志 ===

(define (parse-access-flags flags)
  (define flag-map
    '((PUBLIC . #x0001)
      (PRIVATE . #x0002)
      (PROTECTED . #x0004)
      (STATIC . #x0008)
      (FINAL . #x0010)
      (SUPER . #x0020)
      (SYNCHRONIZED . #x0020)
      (VOLATILE . #x0040)
      (BRIDGE . #x0040)
      (TRANSIENT . #x0080)
      (VARARGS . #x0080)
      (NATIVE . #x0100)
      (INTERFACE . #x0200)
      (ABSTRACT . #x0400)
      (STRICT . #x0800)
      (SYNTHETIC . #x1000)
      (ANNOTATION . #x2000)
      (ENUM . #x4000)
      (MODULE . #x8000)
      (OPEN . #x0020)
      (TRANSITIVE . #x0020)))
  (for/fold ([result 0])
            ([flag flags])
    (define pair (assq flag flag-map))
    (if pair
        (bitwise-ior result (cdr pair))
        result)))

;; === 解析 JvmField ===

(define (parse-jvm-field datum)
  (JvmField
    (Field-name datum)
    (Field-desc datum)
    (parse-access-flags (Field-access datum))
    (Field-value datum)
    '()    ; attributes
    '()))  ; annotations

;; === 解析 JvmMethod ===

(define (parse-jvm-method datum)
  (JvmMethod
    (Method-name datum)
    (Method-desc datum)
    (parse-access-flags (Method-access datum))
    0                  ; max-stack
    0                  ; max-locals
    (map parse-jvm-insn (Method-insns datum))
    (extract-exception-entries (Method-insns datum))
    '()                ; local-vars
    (map parse-jvm-line-number (Method-line-infos datum))
    '()                ; attributes
    '()                ; annotations
    '()))              ; param-annots

;; === 解析 JvmInsn ===

(define (parse-jvm-insn datum)
  (JvmInsn (Insn-opcode datum) (Insn-args datum)))

;; === 解析 JvmLineNumber ===

(define (parse-jvm-line-number datum)
  (JvmLineNumber (Line-Info-line datum) (Line-Info-label datum)))

;; === 解析 JvmInnerClass ===

(define (parse-jvm-inner-class datum)
  (JvmInnerClass
    (InnerClass-name datum)
    (InnerClass-outer datum)
    (InnerClass-inner datum)
    (parse-access-flags (InnerClass-access datum))))

;; === 提取异常表 ===

(define (extract-exception-entries insns)
  (for/list ([insn insns]
             #:when (and (Insn? insn)
                        (eq? (Insn-opcode insn) 'TRY-CATCH-BLOCK)))
    (match (Insn-args insn)
      [`(,start ,end ,handler ,type)
        (JvmExceptionEntry start end handler type)]
      [_ #f])))

(provide extract-exception-entries)

;; === 过滤掉伪指令 ===

(define (filter-real-insns insns)
  (filter (lambda (insn)
            (and (JvmInsn? insn)
                 (not (member (JvmInsn-opcode insn)
                             '(TRY-CATCH-BLOCK CUTIEDENG-LABEL)))))
          insns))

(provide filter-real-insns)
