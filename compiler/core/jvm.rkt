#lang racket/base

;; ============================================================
;; Core: JVM IR 数据结构定义
;; ============================================================
;;
;; JVM 字节码的 Racket 表示
;; 支持多版本 Java 字节码（Java 8 ~ Java 21+）
;; ============================================================

;; === 类文件版本 ===

;; 主要 Java 版本对应的 class 文件版本号
;; Java 8  = 52, Java 11 = 55, Java 17 = 61, Java 21 = 65
(struct JvmVersion (major minor) #:prefab)

(provide (struct-out JvmVersion))

;; === 访问标志 ===

;; 访问标志常量（位掩码）
(define ACC_PUBLIC       #x0001)
(define ACC_PRIVATE      #x0002)
(define ACC_PROTECTED    #x0004)
(define ACC_STATIC       #x0008)
(define ACC_FINAL        #x0010)
(define ACC_SUPER        #x0020)  ; 类
(define ACC_SYNCHRONIZED #x0020)  ; 方法
(define ACC_VOLATILE     #x0040)  ; 字段
(define ACC_BRIDGE       #x0040)  ; 方法
(define ACC_TRANSIENT    #x0080)  ; 字段
(define ACC_VARARGS      #x0080)  ; 方法
(define ACC_NATIVE       #x0100)
(define ACC_INTERFACE    #x0200)
(define ACC_ABSTRACT     #x0400)
(define ACC_STRICT       #x0800)
(define ACC_SYNTHETIC    #x1000)
(define ACC_ANNOTATION   #x2000)
(define ACC_ENUM         #x4000)
(define ACC_MODULE       #x8000)

(provide ACC_PUBLIC ACC_PRIVATE ACC_PROTECTED ACC_STATIC
         ACC_FINAL ACC_SUPER ACC_SYNCHRONIZED ACC_VOLATILE
         ACC_BRIDGE ACC_TRANSIENT ACC_VARARGS ACC_NATIVE
         ACC_INTERFACE ACC_ABSTRACT ACC_STRICT ACC_SYNTHETIC
         ACC_ANNOTATION ACC_ENUM ACC_MODULE)

;; === 类表示 ===

(struct JvmClass (
  version         ; JvmVersion - 类文件版本
  name            ; String - 内部名称 (e.g., "java/lang/String")
  access          ; Integer - 访问标志
  super-class     ; String or #f - 父类
  interfaces      ; (Listof String) - 实现的接口
  fields          ; (Listof JvmField)
  methods         ; (Listof JvmMethod)
  attributes      ; (Listof JvmAttribute) - 其他属性
  annotations     ; (Listof JvmAnnotation)
  inner-classes   ; (Listof JvmInnerClass)
) #:prefab)

(provide (struct-out JvmClass))

;; === 字段表示 ===

(struct JvmField (
  name            ; String
  descriptor      ; String - 类型描述符
  access          ; Integer - 访问标志
  value           ; Any or #f - 常量值（静态常量）
  attributes      ; (Listof JvmAttribute)
  annotations     ; (Listof JvmAnnotation)
) #:prefab)

(provide (struct-out JvmField))

;; === 方法表示 ===

(struct JvmMethod (
  name            ; String
  descriptor      ; String - 方法描述符
  access          ; Integer - 访问标志
  max-stack       ; Integer - 最大栈深度
  max-locals      ; Integer - 最大局部变量数
  insns           ; (Listof JvmInsn) - 指令序列
  exception-table ; (Listof JvmExceptionEntry)
  local-vars      ; (Listof JvmLocalVar) - 局部变量表
  line-numbers    ; (Listof JvmLineNumber) - 行号表
  attributes      ; (Listof JvmAttribute)
  annotations     ; (Listof JvmAnnotation)
  param-annots    ; (Listof (Listof JvmAnnotation)) - 参数注解
) #:prefab)

(provide (struct-out JvmMethod))

;; === 指令表示 ===

;; JVM 指令（线性形式，栈式）
(struct JvmInsn (
  opcode          ; Symbol - 操作码名称
  operands        ; (Listof Any) - 操作数
) #:prefab)

(provide (struct-out JvmInsn))

;; 标签（伪指令，用于跳转目标）
(struct JvmLabel (
  name            ; String - 标签名
) #:prefab)

(provide (struct-out JvmLabel))

;; === 异常表 ===

(struct JvmExceptionEntry (
  start-label     ; String - 起始标签
  end-label       ; String - 结束标签
  handler-label   ; String - 处理器标签
  catch-type      ; String or #f - 捕获类型（#f = finally）
) #:prefab)

(provide (struct-out JvmExceptionEntry))

;; === 局部变量表 ===

(struct JvmLocalVar (
  name            ; String
  descriptor      ; String
  signature       ; String or #f - 泛型签名
  start-label     ; String
  end-label       ; String
  index           ; Integer - 槽位索引
) #:prefab)

(provide (struct-out JvmLocalVar))

;; === 行号表 ===

(struct JvmLineNumber (
  line            ; Integer
  label           ; String
) #:prefab)

(provide (struct-out JvmLineNumber))

;; === 注解 ===

(struct JvmAnnotation (
  type            ; String - 注解类型描述符
  values          ; (Listof (Pairof String Any)) - 键值对
  visible         ; Boolean - 运行时可见
) #:prefab)

(provide (struct-out JvmAnnotation))

;; === 内部类 ===

(struct JvmInnerClass (
  name            ; String - 内部类名
  outer-name      ; String or #f - 外部类名
  inner-name      ; String or #f - 简短名
  access          ; Integer - 访问标志
) #:prefab)

(provide (struct-out JvmInnerClass))

;; === 通用属性 ===

(struct JvmAttribute (
  name            ; String - 属性名
  data            ; Bytes or Any - 属性数据
) #:prefab)

(provide (struct-out JvmAttribute))

;; === 类型描述符解析 ===

;; JVM 类型
(struct JvmType () #:prefab)
(struct JvmPrimitive JvmType (kind) #:prefab)  ; kind: 'int 'long 'float 'double 'byte 'char 'short 'boolean 'void
(struct JvmReference JvmType (class-name) #:prefab)
(struct JvmArray JvmType (element-type) #:prefab)

(provide (struct-out JvmType))
(provide (struct-out JvmPrimitive))
(provide (struct-out JvmReference))
(provide (struct-out JvmArray))

;; === 方法句柄（invoke-dynamic 支持）===

(struct JvmMethodHandle (
  kind            ; Integer - 句柄类型 (1-9)
  owner           ; String - 所有者类
  name            ; String - 方法名
  descriptor      ; String - 描述符
  is-interface    ; Boolean
) #:prefab)

(provide (struct-out JvmMethodHandle))

;; === 常量动态（Java 11+）===

(struct JvmConstantDynamic (
  name            ; String
  descriptor      ; String
  bootstrap       ; JvmMethodHandle
  args            ; (Listof Any)
) #:prefab)

(provide (struct-out JvmConstantDynamic))
