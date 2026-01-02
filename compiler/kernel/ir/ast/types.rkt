#lang racket/base

;; ============================================================
;; Kernel IR: AST Type Definitions
;; ============================================================
;;
;; Generic Abstract Syntax Tree types for high-level IR.
;; These are language-independent core expression forms.
;;
;; Note: Language-specific AST extensions (e.g., Java, Python)
;; should be defined in their respective pipeline modules.
;;
;; ============================================================

(provide
  ;; Literals
  (struct-out Int)
  (struct-out Bool)
  (struct-out Void)
  (struct-out Float)
  (struct-out String)

  ;; Variables
  (struct-out Var)
  (struct-out Var:named)

  ;; Binding & Control
  (struct-out Let)
  (struct-out If)
  (struct-out Begin)
  (struct-out WhileLoop)

  ;; Mutation (for imperative languages)
  (struct-out SetBang)
  (struct-out GetBang)

  ;; Functions
  (struct-out Lambda)
  (struct-out Apply)
  (struct-out Call)
  (struct-out FunRef)

  ;; Primitives
  (struct-out Prim)

  ;; Type annotations
  (struct-out HasType)

  ;; Program structure
  (struct-out Program)
  (struct-out Def)

  ;; Predicates
  literal?
  expr?
  atomic?)

;; ============================================================
;; Literals
;; ============================================================

;; Integer literal
(struct Int (value) #:prefab)

;; Boolean literal
(struct Bool (value) #:prefab)

;; Void/unit literal
(struct Void () #:prefab)

;; Floating point literal
(struct Float (value) #:prefab)

;; String literal
(struct String (value) #:prefab)

;; ============================================================
;; Variables
;; ============================================================

;; Variable reference by ID (after uniquification)
(struct Var (id) #:prefab)

;; Variable reference by name (before uniquification)
(struct Var:named (name) #:prefab)

;; ============================================================
;; Binding & Control Flow
;; ============================================================

;; Let binding: (let ([x e]) body)
(struct Let (var rhs body) #:prefab)

;; Conditional: (if cond then else)
(struct If (cond then else) #:prefab)

;; Sequence: (begin e1 e2 ... en body)
;; es: pvector of expressions
(struct Begin (exprs body) #:prefab)

;; While loop: (while cond body)
(struct WhileLoop (cond body) #:prefab)

;; ============================================================
;; Mutation
;; ============================================================

;; Variable assignment: (set! var rhs)
(struct SetBang (var rhs) #:prefab)

;; Variable read (for mutable variables): (get! var)
(struct GetBang (var) #:prefab)

;; ============================================================
;; Functions
;; ============================================================

;; Lambda expression: (lambda (params...) body)
(struct Lambda (params return-type body) #:prefab)

;; Function application: (f arg1 arg2 ...)
;; args: pvector of arguments
(struct Apply (func args) #:prefab)

;; Direct call (after closure conversion)
(struct Call (func args) #:prefab)

;; Function reference
(struct FunRef (name arity) #:prefab)

;; ============================================================
;; Primitives
;; ============================================================

;; Primitive operation: (op arg1 arg2 ...)
;; args: list of arguments
(struct Prim (op args) #:prefab)

;; ============================================================
;; Type Annotations
;; ============================================================

;; Type annotation wrapper
(struct HasType (expr type) #:prefab)

;; ============================================================
;; Program Structure
;; ============================================================

;; Top-level program
;; info: ordered-map of metadata
(struct Program (info body) #:prefab)

;; Function definition
(struct Def (name params return-type info body) #:prefab)

;; ============================================================
;; Predicates
;; ============================================================

(define (literal? x)
  (or (Int? x)
      (Bool? x)
      (Void? x)
      (Float? x)
      (String? x)))

(define (atomic? x)
  (or (literal? x)
      (Var? x)
      (Var:named? x)))

(define (expr? x)
  (or (literal? x)
      (Var? x)
      (Var:named? x)
      (Let? x)
      (If? x)
      (Begin? x)
      (WhileLoop? x)
      (SetBang? x)
      (GetBang? x)
      (Lambda? x)
      (Apply? x)
      (Call? x)
      (FunRef? x)
      (Prim? x)
      (HasType? x)))
