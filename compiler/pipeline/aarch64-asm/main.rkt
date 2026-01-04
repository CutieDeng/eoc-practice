#lang racket/base

;; ============================================================================
;; AArch64 SVE Assembly Pipeline - Main Entry Point
;; ============================================================================
;;
;; This module provides the complete API for the AArch64 SVE assembly pipeline.
;; The pipeline is self-contained and based on cutie-ftree persistent data
;; structures for all internal representations.
;;
;; Architecture:
;; - IR layer: types, cfg, sve-insns (based on cutie-ftree/graph)
;; - Frontend: parser, type-check
;; - Analysis: liveness (via driver framework)
;; - Backend: regalloc, emit
;; - Interpreter: base, sve (for testing)
;;
;; ============================================================================

(require
 ;; Utilities
 "../../../cutie-ftree/bitset.rkt"

 ;; IR - Core types and structures
 "component/ir/types.rkt"
 "component/ir/cfg.rkt"
 "component/ir/config.rkt"
 "component/ir/sve-insns.rkt"

 ;; Frontend - Parsing
 "component/frontend/parser.rkt"

 ;; Analysis - Liveness
 "component/analysis/liveness.rkt"

 ;; Backend - Register allocation and emission
 "component/backend/regalloc.rkt"
 "component/backend/emit.rkt"

 ;; Interpreter - For testing
 "component/interp/state.rkt"
 "component/interp/base.rkt"
 "component/interp/sve.rkt")

;; ============================================================================
;; Re-export all public APIs
;; ============================================================================

(provide
 ;; === IR Types ===
 ;; Physical registers
 Reg:x Reg:x? Reg:x-id
 Reg:w Reg:w? Reg:w-id
 Reg:z Reg:z? Reg:z-id
 Reg:p Reg:p? Reg:p-id
 Reg:v Reg:v? Reg:v-id Reg:v-width
 Reg:b Reg:b? Reg:b-id
 Reg:h Reg:h? Reg:h-id
 Reg:s Reg:s? Reg:s-id
 Reg:d Reg:d? Reg:d-id
 Reg:q Reg:q? Reg:q-id
 Reg:sp Reg:sp?
 Reg:xzr Reg:xzr?
 Reg:wzr Reg:wzr?
 Reg:ffr Reg:ffr?
 any-reg? gpr? simd-reg? sve-reg? pred-reg? physical-reg?

 ;; Virtual registers
 VReg:gpr VReg:gpr? VReg:gpr-id VReg:gpr-width
 VReg:sve VReg:sve? VReg:sve-id
 VReg:pred VReg:pred? VReg:pred-id
 VReg:vec VReg:vec? VReg:vec-id
 vreg? vreg-id vreg-class

 ;; Immediates
 Imm Imm? Imm-value
 Imm:shifted Imm:shifted? Imm:shifted-value Imm:shifted-shift

 ;; Memory addressing
 Mem:base Mem:base? Mem:base-reg
 Mem:offset Mem:offset? Mem:offset-reg Mem:offset-offset
 Mem:pre Mem:pre? Mem:pre-reg Mem:pre-offset
 Mem:post Mem:post? Mem:post-reg Mem:post-offset
 Mem:reg Mem:reg? Mem:reg-base Mem:reg-index
 Mem:scaled Mem:scaled? Mem:scaled-base Mem:scaled-index Mem:scaled-scale
 mem-addr?

 ;; Labels
 Label:named Label:named? Label:named-name
 Label:id Label:id? Label:id-id
 Label:local Label:local? Label:local-parent Label:local-id
 label?

 ;; Instructions
 Insn:arith Insn:arith? Insn:arith-op Insn:arith-dst Insn:arith-src1 Insn:arith-src2
 Insn:arith2 Insn:arith2? Insn:arith2-op Insn:arith2-dst Insn:arith2-src
 Insn:load Insn:load? Insn:load-op Insn:load-dst Insn:load-addr
 Insn:store Insn:store? Insn:store-op Insn:store-src Insn:store-addr
 Insn:ldp Insn:ldp? Insn:ldp-op Insn:ldp-dst1 Insn:ldp-dst2 Insn:ldp-addr
 Insn:stp Insn:stp? Insn:stp-op Insn:stp-src1 Insn:stp-src2 Insn:stp-addr
 Insn:mov Insn:mov? Insn:mov-op Insn:mov-dst Insn:mov-src
 Insn:cmp Insn:cmp? Insn:cmp-op Insn:cmp-src1 Insn:cmp-src2
 Insn:csel Insn:csel? Insn:csel-op Insn:csel-dst Insn:csel-src1 Insn:csel-src2 Insn:csel-cond
 Insn:branch Insn:branch? Insn:branch-op Insn:branch-target
 Insn:cond-branch Insn:cond-branch? Insn:cond-branch-op Insn:cond-branch-cond Insn:cond-branch-target
 Insn:cbz Insn:cbz? Insn:cbz-op Insn:cbz-reg Insn:cbz-target
 Insn:ret Insn:ret?
 Insn:sve Insn:sve? Insn:sve-op Insn:sve-pred Insn:sve-dst Insn:sve-srcs
 Insn:sve-load Insn:sve-load? Insn:sve-load-op Insn:sve-load-pred Insn:sve-load-dst Insn:sve-load-addr
 Insn:sve-store Insn:sve-store? Insn:sve-store-op Insn:sve-store-pred Insn:sve-store-src Insn:sve-store-addr
 Insn:sve-reduce Insn:sve-reduce? Insn:sve-reduce-op Insn:sve-reduce-pred Insn:sve-reduce-dst Insn:sve-reduce-src
 Insn:sve-cmp Insn:sve-cmp? Insn:sve-cmp-op Insn:sve-cmp-pd Insn:sve-cmp-pg Insn:sve-cmp-src1 Insn:sve-cmp-src2
 Insn:sve-pred-op Insn:sve-pred-op? Insn:sve-pred-op-op Insn:sve-pred-op-pd Insn:sve-pred-op-pg Insn:sve-pred-op-pn
 Insn:whilelt Insn:whilelt? Insn:whilelt-pd Insn:whilelt-rn Insn:whilelt-rm
 insn?

 ;; AsmInsn generic
 AsmInsn AsmInsn? AsmInsn-op AsmInsn-args

 ;; Function & Param types
 AsmParam AsmParam? AsmParam-reg AsmParam-type
 Type:scalar Type:scalar? Type:scalar-width Type:scalar-signed?
 Type:float Type:float? Type:float-width
 Type:vec Type:vec? Type:vec-elem Type:vec-lanes
 Type:sve Type:sve? Type:sve-elem
 Type:sve2 Type:sve2? Type:sve2-elem
 Type:pred Type:pred?
 Type:ptr Type:ptr? Type:ptr-elem
 Type:void Type:void?
 asm-type?
 condition-code?
 operand?

 ;; === CFG ===
 ;; Block ID (vertex-id from graph)
 vertex-id vertex-id? vertex-id-val
 BlockId BlockId? BlockId-id
 block-id-compare

 ;; Blocks
 AsmBlock AsmBlock? AsmBlock-id AsmBlock-label AsmBlock-insns AsmBlock-terminator AsmBlock-info
 make-empty-block
 block-append-insn
 block-append-insns
 block-set-terminator
 block-insn-count

 ;; Terminators
 Term:ret Term:ret?
 Term:jump Term:jump? Term:jump-target
 Term:cond Term:cond? Term:cond-cond Term:cond-then-target Term:cond-else-target
 Term:switch Term:switch?
 Term:unreachable Term:unreachable?
 terminator?
 terminator-successors

 ;; CFG structure
 AsmCfg AsmCfg?
 AsmCfg-entry AsmCfg-graph AsmCfg-block-data AsmCfg-config
 make-empty-cfg
 cfg-add-block
 cfg-get-block
 cfg-update-block
 cfg-remove-block
 cfg-block-count
 cfg-entry-block
 in-cfg-blocks
 in-cfg-block-ids
 cfg-fresh-block-id
 cfg-fresh-label
 cfg-successors
 cfg-predecessors
 cfg-reachable-blocks
 cfg-add-edge
 cfg-remove-edge
 pvector-insns->list
 list->pvector-insns

 ;; === Config ===
 AsmConfig AsmConfig?
 AsmConfig-sve? AsmConfig-sve2? AsmConfig-vl AsmConfig-features
 config/base config/sve config/sve-256 config/sve-512 config/sve2
 config-vl-bytes config-vl-elements
 config-has-feature?
 known-features

 ;; === Frontend ===
 ;; Parser
 parse-asm-module
 parse-asm-fn
 parse-params
 parse-type
 parse-reg
 parse-operand
 parse-insn
 parse-block
 parse-body
 parse-error
 ParseError ParseError?

 ;; === Analysis ===
 ;; Liveness
 compute-liveness
 LivenessResult LivenessResult?
 LivenessResult-block-info LivenessResult-live-in LivenessResult-live-out
 BlockLiveness BlockLiveness?
 BlockLiveness-insn-liveness BlockLiveness-gen BlockLiveness-kill
 InsnLiveness InsnLiveness?
 InsnLiveness-live-before InsnLiveness-live-after
 insn-defs
 insn-uses
 get-live-after

 ;; === Backend ===
 ;; Register Allocation
 allocate-registers
 AllocationResult AllocationResult?
 AllocationResult-assignment AllocationResult-spilled
 AllocationResult-callee-saved-used AllocationResult-success?
 VRegIndex VRegIndex?
 build-vreg-index
 vreg-id->idx
 idx->vreg-id
 build-interference-graph
 color-graph
 gpr-caller-saved
 gpr-callee-saved
 gpr-allocatable
 sve-allocatable
 pred-allocatable
 apply-allocation
 rewrite-insn

 ;; Emission
 emit-function
 emit-cfg
 emit-to-string
 emit-to-file
 emit-insn
 emit-operand
 emit-register
 emit-directive

 ;; === Function Structure ===
 AsmFunction AsmFunction?
 AsmFunction-name AsmFunction-body AsmFunction-params AsmFunction-ret-type

 ;; === Interpreter ===
 ;; State
 MachineState MachineState?
 MachineState-x-regs MachineState-v-regs MachineState-z-regs MachineState-p-regs
 MachineState-sp MachineState-pc MachineState-flags MachineState-memory
 MachineState-vl MachineState-config
 make-initial-state
 make-empty-state
 state-read-x state-write-x

 ;; Base interpreter
 interp-aarch64-base%
 run-function run-cfg run-block step-insn
 InterpResult InterpResult?
 InterpResult-state InterpResult-value InterpResult-halted?
 interp-ok interp-halt interp-error

 ;; SVE interpreter
 interp-aarch64-sve%
 make-sve-interp run-sve-cfg run-sve-insn
 )

;; ============================================================================
;; Pipeline Composition
;; ============================================================================

;; Compile a parsed function to assembly string
(define (compile-function fn)
  (define cfg (AsmFunction-body fn))

  ;; Step 1: Register allocation
  (define allocation (allocate-registers cfg))

  (if (AllocationResult-success? allocation)
      ;; Step 2: Apply allocation
      (let* ([allocated-cfg (apply-allocation cfg allocation)]
             [allocated-fn (struct-copy AsmFunction fn [body allocated-cfg])])
        ;; Step 3: Emit assembly
        (emit-to-string allocated-fn))
      ;; Allocation failed - spills needed
      (error 'compile-function
             "register allocation failed: ~a spills needed"
             (bitset-count (AllocationResult-spilled allocation)))))

(provide compile-function)

;; Full pipeline: parse -> allocate -> emit
(define (compile-asm-sexp sexp)
  (define fn (parse-asm-fn sexp))
  (compile-function fn))

(provide compile-asm-sexp)
