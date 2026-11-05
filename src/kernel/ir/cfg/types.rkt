#lang racket/base

;; ============================================================
;; Kernel IR: Standard CFG
;; ============================================================
;;
;; The canonical Control Flow Graph representation used across all
;; IRs in this compiler. Shape:
;;
;;   - Block identity is the prefab `BlockId`, a wrapper over a
;;     natural allocated from `Cfg-block-cnt`.
;;
;;   - Control-flow structure is implicit: the successors of a block
;;     are exactly its terminator's targets (see
;;     component/cfg/utils/graph-ops.rkt).  No separate edge store.
;;
;;   - Block content (`CfgBlock`) carries phis + instructions +
;;     terminator + info. Phi and VfInsn are SSA-oriented but are
;;     the default content type; other IRs (e.g. aarch64 AsmBlock)
;;     define their own block struct while keeping the same
;;     BlockId / Term:* conventions.
;;
;;   - Terminators use the `Term:*` prefix style consistently.
;;
;; This module contains data definitions only -- no algorithms.
;; ============================================================

(require (only-in racket/compare integer-compare))

(provide
  ;; Block identity
  (struct-out BlockId)
  block-id-compare

  ;; Other ID types
  (struct-out VarId)
  (struct-out InsnId)
  (struct-out InsnIdx)

  ;; CFG structure
  (struct-out Cfg)
  (struct-out CfgBlock)

  ;; SSA instruction forms
  (struct-out VfInsn)
  (struct-out PhiInsn)

  ;; Terminators
  (struct-out Term:jump)
  (struct-out Term:cond)
  (struct-out Term:switch)
  (struct-out Term:ret)
  (struct-out Term:throw)
  (struct-out Term:unreachable)

  ;; Predicates
  terminator?
  cfg-id?
  instruction?

  ;; Comparison helpers
  integer-compare
  var-id-compare
  insn-id-compare)

;; ============================================================
;; Block identity
;; ============================================================

;; A block id is a prefab wrapper over a natural number allocated
;; per Cfg (see Cfg-block-cnt).  Prefab so a Cfg can be written and
;; read back verbatim.
(struct BlockId (id) #:prefab)
(define (block-id-compare a b)
  (integer-compare (BlockId-id a) (BlockId-id b)))

;; ============================================================
;; Other ID types
;; ============================================================

;; SSA variable identifier
(struct VarId (id) #:prefab)

;; Stable instruction identifier (optional, globally unique)
(struct InsnId (id) #:prefab)

;; Positional instruction index within a block (traversal only)
(struct InsnIdx (id) #:prefab)

;; ============================================================
;; CFG structure
;; ============================================================

;; Fields:
;;   blocks    : idmap[BlockId -> CfgBlock]
;;   entry     : BlockId  (or #f before set)
;;   exit      : BlockId or #f (optional unified exit)
;;   block-cnt : Integer - next BlockId id to allocate
;;   var-cnt   : Integer - next VarId id to allocate
;;   insn-cnt  : Integer - next InsnId id to allocate
;;   info      : hash[Symbol -> Any] - metadata
;;
;; Control-flow edges are not stored: successors are derived from
;; each block's terminator (component/cfg/utils/graph-ops.rkt).
;;
(struct Cfg (blocks entry exit block-cnt var-cnt insn-cnt info) #:prefab)

;; Fields:
;;   id         : BlockId
;;   phis       : pvector[PhiInsn]
;;   insns      : pvector[VfInsn]   (other IRs that reuse CfgBlock may
;;                                   carry their own instruction type
;;                                   here; the pvector requirement is
;;                                   uniform)
;;   terminator : Terminator or #f
;;   info       : hash[Symbol -> Any]
;;
(struct CfgBlock (id phis insns terminator info) #:prefab)

;; ============================================================
;; SSA instructions
;; ============================================================

;; Generic value-flow instruction
;;   op      : Symbol
;;   inputs  : pvector[Any] - VarIds and literals
;;   outputs : pvector[VarId]
;;   info    : Any or #f
;;   id      : InsnId or #f
(struct VfInsn (op inputs outputs info id) #:prefab)

;; SSA phi node
;;   output  : VarId
;;   sources : pvector[(Pairof BlockId VarId)]
(struct PhiInsn (output sources) #:prefab)

;; ============================================================
;; Terminators
;; ============================================================

;; Unconditional jump.   target : BlockId
(struct Term:jump (target) #:prefab)

;; Conditional branch.
;;   cond        : VarId
;;   then-target : BlockId
;;   else-target : BlockId
(struct Term:cond (cond then-target else-target) #:prefab)

;; Switch/table jump.
;;   value   : VarId
;;   cases   : pvector[(Pairof Integer BlockId)]
;;   default : BlockId
(struct Term:switch (value cases default) #:prefab)

;; Return.   values : pvector[VarId]
(struct Term:ret (values) #:prefab)

;; Throw.    exception : VarId
(struct Term:throw (exception) #:prefab)

;; Unreachable marker
(struct Term:unreachable () #:prefab)

;; ============================================================
;; Predicates
;; ============================================================

(define (terminator? x)
  (or (Term:jump? x)
      (Term:cond? x)
      (Term:switch? x)
      (Term:ret? x)
      (Term:throw? x)
      (Term:unreachable? x)))

(define (cfg-id? x)
  (or (BlockId? x)
      (VarId? x)
      (InsnId? x)
      (InsnIdx? x)))

(define (instruction? x)
  (or (VfInsn? x)
      (PhiInsn? x)))

;; ============================================================
;; Comparison helpers
;; ============================================================

(define (var-id-compare a b)
  (integer-compare (VarId-id a) (VarId-id b)))

(define (insn-id-compare a b)
  (integer-compare (InsnId-id a) (InsnId-id b)))
