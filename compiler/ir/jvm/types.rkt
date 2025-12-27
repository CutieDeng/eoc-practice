#lang racket/base

;; ============================================================
;; IR Layer: JVM Type Definitions
;; ============================================================
;;
;; JVM bytecode representation in Racket.
;; Supports Java 8 ~ Java 21+ bytecode versions.
;; ============================================================

;; === Class File Version ===

;; Java version to class file version mapping:
;; Java 8 = 52, Java 11 = 55, Java 17 = 61, Java 21 = 65
(struct JvmVersion (major minor) #:prefab)

(provide (struct-out JvmVersion))

;; === Access Flags ===

;; Access flag constants (bitmask)
(define ACC_PUBLIC       #x0001)
(define ACC_PRIVATE      #x0002)
(define ACC_PROTECTED    #x0004)
(define ACC_STATIC       #x0008)
(define ACC_FINAL        #x0010)
(define ACC_SUPER        #x0020)  ; class
(define ACC_SYNCHRONIZED #x0020)  ; method
(define ACC_VOLATILE     #x0040)  ; field
(define ACC_BRIDGE       #x0040)  ; method
(define ACC_TRANSIENT    #x0080)  ; field
(define ACC_VARARGS      #x0080)  ; method
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

;; === Class Representation ===

(struct JvmClass (
  version         ; JvmVersion - class file version
  name            ; String - internal name (e.g., "java/lang/String")
  access          ; Integer - access flags
  super-class     ; String or #f - superclass
  interfaces      ; (Listof String) - implemented interfaces
  fields          ; (Listof JvmField)
  methods         ; (Listof JvmMethod)
  attributes      ; (Listof JvmAttribute) - other attributes
  annotations     ; (Listof JvmAnnotation)
  inner-classes   ; (Listof JvmInnerClass)
) #:prefab)

(provide (struct-out JvmClass))

;; === Field Representation ===

(struct JvmField (
  name            ; String
  descriptor      ; String - type descriptor
  access          ; Integer - access flags
  value           ; Any or #f - constant value (static final)
  attributes      ; (Listof JvmAttribute)
  annotations     ; (Listof JvmAnnotation)
) #:prefab)

(provide (struct-out JvmField))

;; === Method Representation ===

(struct JvmMethod (
  name            ; String
  descriptor      ; String - method descriptor
  access          ; Integer - access flags
  max-stack       ; Integer - max stack depth
  max-locals      ; Integer - max local variables
  insns           ; (Listof JvmInsn) - instruction sequence
  exception-table ; (Listof JvmExceptionEntry)
  local-vars      ; (Listof JvmLocalVar) - local variable table
  line-numbers    ; (Listof JvmLineNumber) - line number table
  attributes      ; (Listof JvmAttribute)
  annotations     ; (Listof JvmAnnotation)
  param-annots    ; (Listof (Listof JvmAnnotation)) - parameter annotations
) #:prefab)

(provide (struct-out JvmMethod))

;; === Instruction Representation ===

;; JVM instruction (linear form, stack-based)
(struct JvmInsn (
  opcode          ; Symbol - opcode name
  operands        ; (Listof Any) - operands
) #:prefab)

(provide (struct-out JvmInsn))

;; Label (pseudo-instruction for jump targets)
(struct JvmLabel (
  name            ; String - label name
) #:prefab)

(provide (struct-out JvmLabel))

;; === Exception Table ===

(struct JvmExceptionEntry (
  start-label     ; String - start label
  end-label       ; String - end label
  handler-label   ; String - handler label
  catch-type      ; String or #f - catch type (#f = finally)
) #:prefab)

(provide (struct-out JvmExceptionEntry))

;; === Local Variable Table ===

(struct JvmLocalVar (
  name            ; String
  descriptor      ; String
  signature       ; String or #f - generic signature
  start-label     ; String
  end-label       ; String
  index           ; Integer - slot index
) #:prefab)

(provide (struct-out JvmLocalVar))

;; === Line Number Table ===

(struct JvmLineNumber (
  line            ; Integer
  label           ; String
) #:prefab)

(provide (struct-out JvmLineNumber))

;; === Annotations ===

(struct JvmAnnotation (
  type            ; String - annotation type descriptor
  values          ; (Listof (Pairof String Any)) - key-value pairs
  visible         ; Boolean - runtime visible
) #:prefab)

(provide (struct-out JvmAnnotation))

;; === Inner Classes ===

(struct JvmInnerClass (
  name            ; String - inner class name
  outer-name      ; String or #f - outer class name
  inner-name      ; String or #f - short name
  access          ; Integer - access flags
) #:prefab)

(provide (struct-out JvmInnerClass))

;; === Generic Attributes ===

(struct JvmAttribute (
  name            ; String - attribute name
  data            ; Bytes or Any - attribute data
) #:prefab)

(provide (struct-out JvmAttribute))

;; === Type Descriptors ===

;; JVM types
(struct JvmType () #:prefab)
(struct JvmPrimitive JvmType (kind) #:prefab)  ; kind: 'int 'long 'float 'double 'byte 'char 'short 'boolean 'void
(struct JvmReference JvmType (class-name) #:prefab)
(struct JvmArray JvmType (element-type) #:prefab)

(provide (struct-out JvmType))
(provide (struct-out JvmPrimitive))
(provide (struct-out JvmReference))
(provide (struct-out JvmArray))

;; === Method Handles (invoke-dynamic support) ===

(struct JvmMethodHandle (
  kind            ; Integer - handle type (1-9)
  owner           ; String - owner class
  name            ; String - method name
  descriptor      ; String - descriptor
  is-interface    ; Boolean
) #:prefab)

(provide (struct-out JvmMethodHandle))

;; === Constant Dynamic (Java 11+) ===

(struct JvmConstantDynamic (
  name            ; String
  descriptor      ; String
  bootstrap       ; JvmMethodHandle
  args            ; (Listof Any)
) #:prefab)

(provide (struct-out JvmConstantDynamic))
