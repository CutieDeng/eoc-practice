#lang racket/base

;; ============================================================
;; IR Layer: CFG Type Definitions
;; ============================================================
;;
;; Control Flow Graph data structures for SSA-based IR.
;; All IDs use zero-based integers.
;; ============================================================

;; === ID Types ===

;; Basic block ID
(struct BlockId (id) #:prefab)

;; Variable ID (SSA value)
(struct VarId (id) #:prefab)

;; Instruction ID (globally unique, stable)
(struct InsnId (id) #:prefab)

;; Instruction index (within block, for traversal)
(struct InsnIdx (id) #:prefab)

(provide (struct-out BlockId))
(provide (struct-out VarId))
(provide (struct-out InsnId))
(provide (struct-out InsnIdx))

;; === ID Operations ===

(define (block-id-offset base n)
  (BlockId (+ (BlockId-id base) n)))

(define (var-id-offset base n)
  (VarId (+ (VarId-id base) n)))

(define (insn-id-offset base n)
  (InsnId (+ (InsnId-id base) n)))

(provide block-id-offset var-id-offset insn-id-offset)

;; === CFG Structure ===

;; Control Flow Graph
(struct Cfg (
  block-cnt       ; Integer - next BlockId to allocate
  var-cnt         ; Integer - next VarId to allocate
  insn-cnt        ; Integer - next InsnId to allocate
  entry           ; BlockId - entry block
  exit            ; BlockId or #f - unified exit (optional)
  blocks          ; ordl: BlockId -> CfgBlock
  info            ; ordl: Symbol -> Any
) #:prefab)

(provide (struct-out Cfg))

;; Basic Block
(struct CfgBlock (
  id              ; BlockId
  phis            ; (Listof PhiInsn) - SSA phi nodes
  insns           ; (Listof VfInsn) - value-flow instructions
  terminator      ; Terminator - terminating instruction
) #:prefab)

(provide (struct-out CfgBlock))

;; === Value-Flow Instructions ===

;; Value-flow instruction (stack-eliminated form)
(struct VfInsn (
  op              ; Symbol - operator
  inputs          ; (Listof Any) - inputs (may contain VarId and other data)
  outputs         ; (Listof VarId) - output variables
  info            ; Any or #f - additional info
  id              ; InsnId or #f - stable identifier (optional)
) #:prefab)

(provide (struct-out VfInsn))

;; Phi node (SSA form)
(struct PhiInsn (
  output          ; VarId - output variable
  sources         ; (Listof (Pairof BlockId VarId)) - source mapping
) #:prefab)

(provide (struct-out PhiInsn))

;; === Terminators ===

;; Unconditional jump
(struct TermJump (target) #:prefab)  ; target: BlockId

;; Conditional branch
(struct TermBranch (
  cond            ; VarId - condition variable
  then-target     ; BlockId
  else-target     ; BlockId
) #:prefab)

;; Switch branch
(struct TermSwitch (
  value           ; VarId - switch value
  cases           ; (Listof (Pairof Integer BlockId)) - case mapping
  default         ; BlockId - default branch
) #:prefab)

;; Return
(struct TermReturn (
  values          ; (Listof VarId) - return values
) #:prefab)

;; Throw exception
(struct TermThrow (
  exception       ; VarId - exception object
) #:prefab)

;; Unreachable (for unreachable code)
(struct TermUnreachable () #:prefab)

(provide (struct-out TermJump))
(provide (struct-out TermBranch))
(provide (struct-out TermSwitch))
(provide (struct-out TermReturn))
(provide (struct-out TermThrow))
(provide (struct-out TermUnreachable))

;; === Predicates ===

(define (terminator? x)
  (or (TermJump? x)
      (TermBranch? x)
      (TermSwitch? x)
      (TermReturn? x)
      (TermThrow? x)
      (TermUnreachable? x)))

(provide terminator?)

;; === Info Key Conventions ===
;;
;; --- Basic Mappings ---
;; 'insn->block   : ordl: InsnId -> BlockId
;; 'block->insns  : ordl: BlockId -> (Listof InsnId)
;;
;; --- Use-Def Chains ---
;; 'var->def      : ordl: VarId -> InsnId
;; 'var->uses     : ordl: VarId -> (Listof (cons InsnId ArgIdx))
;;
;; --- Type Info ---
;; 'var->type     : ordl: VarId -> Type
;;
;; --- Control Flow Analysis ---
;; 'block->preds  : ordl: BlockId -> (Listof BlockId)
;; 'block->succs  : ordl: BlockId -> (Listof BlockId)
;; 'block->dom    : ordl: BlockId -> BlockId
;; 'block->idom   : ordl: BlockId -> (Listof BlockId)
;;
;; --- Debug Info ---
;; 'source-map    : ordl: InsnId -> SourceLoc
