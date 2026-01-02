#lang racket/base

;; ============================================================
;; Kernel IR: CFG Type Definitions
;; ============================================================
;;
;; Control Flow Graph data structures for SSA-based IR.
;; Pure data definitions only - no algorithms or operations.
;;
;; ============================================================

(provide
  ;; ID types
  (struct-out BlockId)
  (struct-out VarId)
  (struct-out InsnId)
  (struct-out InsnIdx)

  ;; CFG structure
  (struct-out Cfg)
  (struct-out CfgBlock)

  ;; Instructions
  (struct-out VfInsn)
  (struct-out PhiInsn)

  ;; Terminators
  (struct-out TermJump)
  (struct-out TermBranch)
  (struct-out TermSwitch)
  (struct-out TermReturn)
  (struct-out TermThrow)
  (struct-out TermUnreachable)

  ;; Predicates
  terminator?
  cfg-id?
  instruction?)

;; ============================================================
;; ID Types
;; ============================================================

;; Basic block identifier
(struct BlockId (id) #:prefab)

;; Variable identifier (SSA value)
(struct VarId (id) #:prefab)

;; Instruction identifier (globally unique, stable)
(struct InsnId (id) #:prefab)

;; Instruction index (within block, for traversal)
(struct InsnIdx (id) #:prefab)

;; ============================================================
;; CFG Structure
;; ============================================================

;; Control Flow Graph
;;
;; Fields:
;;   block-cnt : Integer - next BlockId to allocate
;;   var-cnt   : Integer - next VarId to allocate
;;   insn-cnt  : Integer - next InsnId to allocate
;;   entry     : BlockId - entry block
;;   exit      : BlockId or #f - unified exit (optional)
;;   blocks    : ordered-map: BlockId -> CfgBlock
;;   info      : ordered-map: Symbol -> Any (metadata)
;;
(struct Cfg (
  block-cnt
  var-cnt
  insn-cnt
  entry
  exit
  blocks
  info
) #:prefab)

;; Basic Block
;;
;; Fields:
;;   id         : BlockId
;;   phis       : (Listof PhiInsn) - SSA phi nodes
;;   insns      : (Listof VfInsn) - value-flow instructions
;;   terminator : Terminator - block terminator
;;
(struct CfgBlock (
  id
  phis
  insns
  terminator
) #:prefab)

;; ============================================================
;; Instructions
;; ============================================================

;; Value-flow instruction (stack-eliminated SSA form)
;;
;; Fields:
;;   op      : Symbol - operator name
;;   inputs  : (Listof Any) - operands (VarId, constants, etc.)
;;   outputs : (Listof VarId) - output variables
;;   info    : Any or #f - additional metadata
;;   id      : InsnId or #f - stable identifier
;;
(struct VfInsn (
  op
  inputs
  outputs
  info
  id
) #:prefab)

;; Phi node (SSA form)
;;
;; Fields:
;;   output  : VarId - output variable
;;   sources : (Listof (Pairof BlockId VarId)) - predecessors mapping
;;
(struct PhiInsn (
  output
  sources
) #:prefab)

;; ============================================================
;; Terminators
;; ============================================================

;; Unconditional jump
(struct TermJump (target) #:prefab)  ; target: BlockId

;; Conditional branch
(struct TermBranch (
  cond            ; VarId - condition
  then-target     ; BlockId
  else-target     ; BlockId
) #:prefab)

;; Switch/table branch
(struct TermSwitch (
  value           ; VarId - switch value
  cases           ; (Listof (Pairof Integer BlockId))
  default         ; BlockId
) #:prefab)

;; Return from function
(struct TermReturn (
  values          ; (Listof VarId)
) #:prefab)

;; Throw exception
(struct TermThrow (
  exception       ; VarId
) #:prefab)

;; Unreachable code marker
(struct TermUnreachable () #:prefab)

;; ============================================================
;; Predicates
;; ============================================================

(define (terminator? x)
  (or (TermJump? x)
      (TermBranch? x)
      (TermSwitch? x)
      (TermReturn? x)
      (TermThrow? x)
      (TermUnreachable? x)))

(define (cfg-id? x)
  (or (BlockId? x)
      (VarId? x)
      (InsnId? x)
      (InsnIdx? x)))

(define (instruction? x)
  (or (VfInsn? x)
      (PhiInsn? x)))
