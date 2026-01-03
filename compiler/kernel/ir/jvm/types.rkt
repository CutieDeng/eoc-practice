#lang racket/base

;; ============================================================
;; Kernel IR: JVM Type Definitions
;; ============================================================
;;
;; JVM bytecode IR data structures.
;; Used by the Java frontend and pipeline.
;;
;; ============================================================

(provide
  ;; Class structure
  (struct-out JvmClass)
  (struct-out JvmVersion)
  (struct-out JvmField)
  (struct-out JvmMethod)
  (struct-out JvmInnerClass)

  ;; Instructions
  (struct-out JvmInsn)

  ;; Method elements
  (struct-out JvmLineNumber)
  (struct-out JvmExceptionEntry)
  (struct-out JvmLocalVar))

;; ============================================================
;; Class Structure
;; ============================================================

;; JVM class version
(struct JvmVersion (major minor) #:prefab)

;; JVM class file
(struct JvmClass (
  version         ; JvmVersion
  name            ; String - fully qualified name
  access-flags    ; Integer - access flags bitmask
  super-class     ; String or #f - super class name
  interfaces      ; (Listof String) - interface names
  fields          ; (Listof JvmField)
  methods         ; (Listof JvmMethod)
  attributes      ; (Listof Any) - class attributes
  annotations     ; (Listof Any) - class annotations
  inner-classes   ; (Listof JvmInnerClass)
) #:prefab)

;; JVM field
(struct JvmField (
  name            ; String
  descriptor      ; String - type descriptor
  access-flags    ; Integer
  value           ; Any or #f - constant value
  attributes      ; (Listof Any)
  annotations     ; (Listof Any)
) #:prefab)

;; JVM method
(struct JvmMethod (
  name            ; String
  descriptor      ; String - type descriptor
  access-flags    ; Integer
  max-stack       ; Integer
  max-locals      ; Integer
  instructions    ; (Listof JvmInsn)
  exceptions      ; (Listof JvmExceptionEntry)
  local-vars      ; (Listof JvmLocalVar)
  line-numbers    ; (Listof JvmLineNumber)
  attributes      ; (Listof Any)
  annotations     ; (Listof Any)
  param-annotations ; (Listof Any)
) #:prefab)

;; JVM inner class info
(struct JvmInnerClass (
  name            ; String
  outer-class     ; String or #f
  inner-name      ; String or #f
  access-flags    ; Integer
) #:prefab)

;; ============================================================
;; Instructions
;; ============================================================

;; JVM bytecode instruction
(struct JvmInsn (
  opcode          ; Symbol - instruction name
  operands        ; (Listof Any) - instruction operands
) #:prefab)

;; ============================================================
;; Method Elements
;; ============================================================

;; Line number table entry
(struct JvmLineNumber (
  line            ; Integer - source line number
  label           ; Any - instruction label/offset
) #:prefab)

;; Exception table entry
(struct JvmExceptionEntry (
  start           ; Any - start label/offset
  end             ; Any - end label/offset
  handler         ; Any - handler label/offset
  catch-type      ; String or #f - exception class name
) #:prefab)

;; Local variable table entry
(struct JvmLocalVar (
  name            ; String
  descriptor      ; String
  signature       ; String or #f
  start           ; Any - start label/offset
  length          ; Integer - scope length
  index           ; Integer - local variable index
) #:prefab)
