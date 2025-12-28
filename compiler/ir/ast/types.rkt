#lang racket/base

;; ============================================================
;; Unified AST Type Definitions
;; ============================================================
;;
;; This module provides a unified set of AST types for:
;;   - L-language (high-level functional expressions)
;;   - C-language (low-level control flow)
;;   - Type annotations
;;
;; Designed to be used by both interpreters and optimizations.
;; ============================================================

(require racket/match)

;; ============================================================
;; Type Annotations
;; ============================================================

;; Primitive types
(struct TyInt () #:transparent)
(struct TyBool () #:transparent)
(struct TyVoid () #:transparent)
(struct TyAny () #:transparent)

;; Compound types
(struct TyVector (elem-types) #:transparent)  ; Fixed-size heterogeneous
(struct TyVectorOf (elem-type) #:transparent) ; Variable-size homogeneous
(struct TyFun (param-types return-type) #:transparent)
(struct TyTuple (elem-types) #:transparent)

(provide (struct-out TyInt) (struct-out TyBool) (struct-out TyVoid)
         (struct-out TyAny) (struct-out TyVector) (struct-out TyVectorOf)
         (struct-out TyFun) (struct-out TyTuple))

;; Type predicates
(define (type? x)
  (or (TyInt? x) (TyBool? x) (TyVoid? x) (TyAny? x)
      (TyVector? x) (TyVectorOf? x) (TyFun? x) (TyTuple? x)))

(provide type?)

;; ============================================================
;; L-Language: High-Level Expressions
;; ============================================================

;; Literals
(struct LInt (value) #:transparent)           ; Integer literal
(struct LBool (value) #:transparent)          ; Boolean literal
(struct LVoid () #:transparent)               ; Void value

;; Variables
(struct LVar (name) #:transparent)            ; Variable reference (symbol)
(struct LVarIdx (index) #:transparent)        ; Variable reference (de Bruijn index)

;; Binding forms
(struct LLet (name init body) #:transparent)  ; let name = init in body
(struct LLetRec (bindings body) #:transparent) ; letrec for mutual recursion

;; Control flow
(struct LIf (cond then else) #:transparent)   ; if-then-else
(struct LWhile (cond body) #:transparent)     ; while loop
(struct LBegin (exprs result) #:transparent)  ; sequencing

;; Mutation
(struct LSet (name value) #:transparent)      ; assignment
(struct LGet (name) #:transparent)            ; mutable read

;; Functions
(struct LLambda (params body) #:transparent)  ; lambda expression
(struct LApply (func args) #:transparent)     ; function application

;; Primitives
(struct LPrim (op args) #:transparent)        ; primitive operation

;; Type annotations
(struct LTyped (expr type) #:transparent)     ; type-annotated expression
(struct LCast (expr from-type to-type) #:transparent) ; type cast

;; Vectors
(struct LVectorRef (vec index) #:transparent)
(struct LVectorSet (vec index value) #:transparent)
(struct LVectorLen (vec) #:transparent)
(struct LMakeVector (size init) #:transparent)

;; Program structure
(struct LProgram (info body) #:transparent)
(struct LProgramDefs (info defs body) #:transparent)
(struct LDef (name params return-type body) #:transparent)

(provide (struct-out LInt) (struct-out LBool) (struct-out LVoid)
         (struct-out LVar) (struct-out LVarIdx)
         (struct-out LLet) (struct-out LLetRec)
         (struct-out LIf) (struct-out LWhile) (struct-out LBegin)
         (struct-out LSet) (struct-out LGet)
         (struct-out LLambda) (struct-out LApply)
         (struct-out LPrim) (struct-out LTyped) (struct-out LCast)
         (struct-out LVectorRef) (struct-out LVectorSet)
         (struct-out LVectorLen) (struct-out LMakeVector)
         (struct-out LProgram) (struct-out LProgramDefs) (struct-out LDef))

;; Expression predicate
(define (l-expr? x)
  (or (LInt? x) (LBool? x) (LVoid? x)
      (LVar? x) (LVarIdx? x)
      (LLet? x) (LLetRec? x)
      (LIf? x) (LWhile? x) (LBegin? x)
      (LSet? x) (LGet? x)
      (LLambda? x) (LApply? x)
      (LPrim? x) (LTyped? x) (LCast? x)
      (LVectorRef? x) (LVectorSet? x)
      (LVectorLen? x) (LMakeVector? x)))

(provide l-expr?)

;; ============================================================
;; C-Language: Low-Level Control Flow
;; ============================================================

;; Statements
(struct CAssign (lhs rhs) #:transparent)      ; lhs = rhs
(struct CReturn (value) #:transparent)        ; return value
(struct CGoto (label) #:transparent)          ; unconditional jump
(struct CIfGoto (cond then-label else-label) #:transparent) ; conditional jump

;; Blocks and programs
(struct CBlock (label stmts) #:transparent)   ; labeled block
(struct CProgram (info blocks) #:transparent) ; C program with blocks

;; Function calls
(struct CCall (func args) #:transparent)      ; function call
(struct CTailCall (func args) #:transparent)  ; tail call

;; Memory operations
(struct CAlloc (size type) #:transparent)     ; allocation
(struct CLoad (addr) #:transparent)           ; memory load
(struct CStore (addr value) #:transparent)    ; memory store

(provide (struct-out CAssign) (struct-out CReturn)
         (struct-out CGoto) (struct-out CIfGoto)
         (struct-out CBlock) (struct-out CProgram)
         (struct-out CCall) (struct-out CTailCall)
         (struct-out CAlloc) (struct-out CLoad) (struct-out CStore))

;; Statement predicate
(define (c-stmt? x)
  (or (CAssign? x) (CReturn? x)
      (CGoto? x) (CIfGoto? x)
      (CCall? x) (CTailCall? x)
      (CAlloc? x) (CLoad? x) (CStore? x)))

(provide c-stmt?)

;; ============================================================
;; Compatibility Layer
;; ============================================================
;;
;; Maps old AST types to new unified types for gradual migration.

;; Convert old L-types to new
(define (convert-old-l-expr old-expr)
  (match old-expr
    ;; Add conversion cases as needed
    [_ old-expr]))  ; Pass through by default

;; Convert old C-types to new
(define (convert-old-c-stmt old-stmt)
  (match old-stmt
    ;; Add conversion cases as needed
    [_ old-stmt]))  ; Pass through by default

(provide convert-old-l-expr convert-old-c-stmt)
