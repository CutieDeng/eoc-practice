# RVSDG Compiler

A sophisticated SSA-based compiler infrastructure implemented in Racket, featuring 25 optimization passes ported from GCC.

## Overview

This compiler provides a complete pipeline from JVM bytecode to optimized RVSDG (Regionalized Value State Dependency Graph) representation:

```
Java Bytecode (.class)
        ↓
    JVM IR (stack-based)
        ↓
    CFG (SSA form)
        ↓
    25 Optimization Passes
        ↓
    RVSDG (value-flow graph)
```

## Project Structure

```
compiler/
├── lib/                    # Layer 0: Foundation (data structures)
│   ├── ftree.rkt          # Finger tree (RAL, ordered dict)
│   ├── graph.rkt          # Immutable graph
│   ├── bset.rkt           # Bit set operations
│   └── main.rkt           # Unified entry
│
├── ir/                     # Layer 1: Intermediate Representations
│   ├── cfg/               # Control Flow Graph (SSA)
│   │   ├── types.rkt      # BlockId, VarId, Cfg, VfInsn, etc.
│   │   └── raw.rkt        # CFG manipulation operations
│   └── jvm/               # JVM bytecode representation
│       └── types.rkt      # JvmClass, JvmMethod, JvmInsn
│
├── core/                   # Legacy type definitions (compatibility shims)
├── cfg/                    # CFG utilities (compatibility shims)
│
├── frontend/java/          # Java bytecode parser
│   ├── reader.rkt         # .class file reader
│   └── pipeline.rkt       # Java → CFG pipeline
│
├── transform/              # IR transformations
│   ├── jvm-to-cfg.rkt     # JVM → CFG conversion
│   └── cfg-to-rvsdg.rkt   # CFG → RVSDG lowering
│
├── optim/cfg/              # 25 Optimization passes
│   ├── pipeline.rkt       # Main optimization pipeline
│   ├── const-fold.rkt     # Constant folding
│   ├── dce.rkt            # Dead code elimination
│   ├── gvn.rkt            # Global value numbering
│   ├── sccp.rkt           # Sparse conditional constant propagation
│   ├── licm.rkt           # Loop invariant code motion
│   ├── pre.rkt            # Partial redundancy elimination
│   ├── inline.rkt         # Function inlining
│   └── ...                # (20+ more passes)
│
├── rvsdg/                  # RVSDG implementation
│   ├── core-def.rkt       # Node definitions
│   ├── interp.rkt         # Interpreter
│   └── analysis/          # RVSDG analysis
│
└── backend/x86/            # Code generation (WIP)
```

## Requirements

- **Racket** v8.0+ (tested on v9.0)
- No external dependencies (uses Racket standard library only)

## Installation

```bash
# Clone the repository
git clone <repo-url>
cd rvsdg-compiler

# Verify Racket is installed
racket --version

# Compile the project (optional, for faster loading)
raco make compiler/optim/cfg/pipeline.rkt
```

## Quick Start

### 1. Interactive REPL

```bash
racket -i
```

```racket
;; Load modules
(require "compiler/optim/cfg/pipeline.rkt")
(require "compiler/ir/cfg/main.rkt")

;; Create a simple CFG
(define cfg0 (cfg-empty))
(define-values (bid cfg1) (cfg-create-block cfg0))
(define cfg2 (cfg-set-entry cfg1 bid))

;; Add instructions: v0 = 10; v1 = 20; v2 = v0 + v1
(define v0 (VarId 0))
(define v1 (VarId 1))
(define v2 (VarId 2))

(define cfg3
  (cfg-block-append-insn cfg2 bid
    (VfInsn 'const '(10) (list v0) #f #f)))
(define cfg4
  (cfg-block-append-insn cfg3 bid
    (VfInsn 'const '(20) (list v1) #f #f)))
(define cfg5
  (cfg-block-append-insn cfg4 bid
    (VfInsn 'add (list v0 v1) (list v2) #f #f)))
(define cfg6
  (cfg-block-set-terminator cfg5 bid
    (TermReturn (list v2))))

;; Optimize
(define cfg-opt (cfg-optimize cfg6))
```

### 2. Optimize Java Bytecode

```bash
# Run the Java optimization test
racket compiler/optim/cfg/test-pipeline-java.rkt
```

### 3. Interactive Optimization Demo

```bash
racket -i compiler/optim/cfg/demo.rkt
```

```racket
;; In REPL:
(demo-all)              ; Run all optimization demos
(demo-const-fold)       ; Demo constant folding
(demo-dce)              ; Demo dead code elimination
(demo-gvn)              ; Demo global value numbering
```

## Running Tests

### Run All Optimization Tests

```bash
# Run all 249 tests
for f in compiler/optim/cfg/test-*.rkt; do
  echo "=== $f ===" && racket "$f"
done
```

### Run Individual Test Suites

```bash
racket compiler/optim/cfg/test-const-fold.rkt      # 10 tests
racket compiler/optim/cfg/test-dce.rkt             # 9 tests
racket compiler/optim/cfg/test-gvn.rkt             # 10 tests
racket compiler/optim/cfg/test-sccp.rkt            # 13 tests
racket compiler/optim/cfg/test-licm.rkt            # 5 tests
racket compiler/optim/cfg/test-pre.rkt             # 12 tests
racket compiler/optim/cfg/test-inline.rkt          # 10 tests
# ... and 19 more test files
```

### Compile Before Running (Faster)

```bash
raco make compiler/optim/cfg/*.rkt
```

## API Reference

### Optimization Pipeline

```racket
(require "compiler/optim/cfg/pipeline.rkt")

;; Apply full optimization pipeline
(cfg-optimize cfg) → optimized-cfg

;; Optimize until fixpoint
(cfg-optimize-fixpoint cfg [max-iters]) → optimized-cfg

;; With statistics
(cfg-optimize-with-stats cfg) → (values optimized-cfg stats-alist)
```

### Individual Passes

```racket
(cfg-const-fold cfg)        ; Constant folding
(cfg-dce cfg)               ; Dead code elimination
(cfg-copy-prop cfg)         ; Copy propagation
(cfg-sccp cfg)              ; SCCP
(cfg-gvn cfg)               ; Global value numbering
(cfg-licm cfg)              ; Loop invariant code motion
(cfg-pre cfg)               ; Partial redundancy elimination
(cfg-inline cfg)            ; Function inlining
(cfg-strength-reduce cfg)   ; Strength reduction
(cfg-algebraic-simplify cfg); Algebraic simplification
(cfg-jump-thread cfg)       ; Jump threading
(cfg-if-combine cfg)        ; If combining
(cfg-tail-merge cfg)        ; Tail merging
(cfg-dse cfg)               ; Dead store elimination
(cfg-loop-unroll cfg)       ; Loop unrolling
;; ... and more
```

### CFG Construction

```racket
(require "compiler/ir/cfg/main.rkt")

(cfg-empty)                              ; Create empty CFG
(cfg-create-block cfg)                   ; Create new block
(cfg-set-entry cfg block-id)             ; Set entry block
(cfg-block-append-insn cfg bid insn)     ; Add instruction
(cfg-block-set-terminator cfg bid term)  ; Set terminator
(cfg-get-block cfg block-id)             ; Get block
(cfg-all-block-ids cfg)                  ; List all blocks
```

## Project Statistics

| Metric | Value |
|--------|-------|
| Total Files | 148 Racket files |
| Lines of Code | ~26,500 |
| Optimization Passes | 25 |
| Unit Tests | 249 |
| Test Coverage | ~8,800 lines |

## Documentation

- [Optimization Guide](optim/OPTIMIZATION.md) - Detailed optimization documentation
- [CFG Usage Guide](optim/cfg/README.md) - CFG module usage
- [Optimization Roadmap](optim/optm-todo.md) - Implementation status

## License

MIT License

## References

- GCC Internals: https://gcc.gnu.org/onlinedocs/gccint/
- SSA Form: Cytron et al., "Efficiently Computing Static Single Assignment Form"
- PRE Algorithm: Morel & Renvoise, "Global Optimization by Suppression of Partial Redundancies"
- RVSDG: Bahmann et al., "Perfect Reconstructability of Control Flow from Demand Dependence Graphs"
