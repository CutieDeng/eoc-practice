#lang racket/base

;; ============================================================
;; C-var IR Type Definitions
;; ============================================================
;;
;; Intermediate representation after explicate-control.
;; Explicit control flow with basic blocks and jumps.
;;
;; ============================================================

(provide
  ;; Atomic expressions
  (struct-out CInt)
  (struct-out CBool)
  (struct-out CVar)

  ;; Expressions
  (struct-out CPrim)

  ;; Statements
  (struct-out CAssign)

  ;; Tail expressions (control flow)
  (struct-out CReturn)
  (struct-out CSeq)
  (struct-out CGoto)
  (struct-out CIf)

  ;; Program structure
  (struct-out CBlock)
  (struct-out CProgram)

  ;; Predicates
  catom?
  cexp?
  ctail?
  cstmt?)

;; ============================================================
;; Atomic Expressions
;; ============================================================

;; Integer constant
(struct CInt (value) #:prefab)

;; Boolean constant
(struct CBool (value) #:prefab)

;; Variable reference
(struct CVar (name) #:prefab)

;; ============================================================
;; Expressions
;; ============================================================

;; Primitive operation
;; op: symbol like 'read, '+, '-, 'not, '<, 'eq?, etc.
;; args: list of atomic expressions
(struct CPrim (op args) #:prefab)

;; ============================================================
;; Statements
;; ============================================================

;; Variable assignment
(struct CAssign (var exp) #:prefab)

;; ============================================================
;; Tail Expressions (Control Flow)
;; ============================================================

;; Return a value
(struct CReturn (exp) #:prefab)

;; Sequence: stmt followed by tail
(struct CSeq (stmt tail) #:prefab)

;; Unconditional jump
(struct CGoto (label) #:prefab)

;; Conditional branch
(struct CIf (cond then-label else-label) #:prefab)

;; ============================================================
;; Program Structure
;; ============================================================

;; Basic block with a tail expression
(struct CBlock (info tail) #:prefab)

;; C-var program
;; info: metadata (locals, etc.)
;; blocks: hash from label to CBlock
(struct CProgram (info blocks) #:prefab)

;; ============================================================
;; Predicates
;; ============================================================

(define (catom? x)
  (or (CInt? x)
      (CBool? x)
      (CVar? x)))

(define (cexp? x)
  (or (catom? x)
      (CPrim? x)))

(define (cstmt? x)
  (CAssign? x))

(define (ctail? x)
  (or (CReturn? x)
      (CSeq? x)
      (CGoto? x)
      (CIf? x)))
