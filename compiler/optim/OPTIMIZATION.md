# GCC Optimization Porting Documentation

This document details the 25 optimization passes ported from GCC to this RVSDG compiler.

## Overview

### Porting Statistics

| Category | GCC Total | Ported | Percentage |
|----------|-----------|--------|------------|
| SSA Core | 28 | 10 | 36% |
| Loop | 15 | 7 | 47% |
| Control Flow | 8 | 5 | 63% |
| Memory | 6 | 3 | 50% |
| Advanced | 8 | 0 | 0% |
| **Total** | **65+** | **25** | **38%** |

### Implementation Phases

All 5 phases completed:

- **Phase 1**: Core SSA (8 passes) ✅
- **Phase 2**: Loop Optimization (7 passes) ✅
- **Phase 3**: Control Flow (5 passes) ✅
- **Phase 4**: Memory (3 passes) ✅
- **Phase 5**: Advanced (2 passes) ✅

---

## Implemented Optimizations

### Phase 1: Core SSA Optimizations

#### 1. Constant Folding (`const-fold.rkt`)

**GCC Reference**: `fold-const.cc`

**Description**: Evaluates constant expressions at compile time.

**Transformation**:
```
Before:                    After:
v0 = 10                   v0 = 10
v1 = 20                   v1 = 20
v2 = add v0, v1           v2 = 30        ; folded at compile time
return v2                 return v2
```

**Test Coverage**: 10 tests in `test-const-fold.rkt`

| Test | Description |
|------|-------------|
| 1 | Simple add of two constants |
| 2 | Subtraction folding |
| 3 | Multiplication folding |
| 4 | Division folding |
| 5 | Chained operations |
| 6 | Boolean operations |
| 7 | Comparison operations |
| 8 | Bitwise operations |
| 9 | No folding for variables |
| 10 | Multi-block folding |

---

#### 2. Dead Code Elimination (`dce.rkt`)

**GCC Reference**: `tree-ssa-dce.cc`

**Description**: Removes instructions whose results are never used.

**Transformation**:
```
Before:                    After:
v0 = const 10             v0 = const 10
v1 = const 20             ; deleted (unused)
v2 = add v0, 5            v2 = add v0, 5
return v2                 return v2
```

**Test Coverage**: 9 tests in `test-dce.rkt`

---

#### 3. Copy Propagation (`copy-prop.rkt`)

**GCC Reference**: `tree-ssa-copy.cc`

**Description**: Propagates copy assignments to their uses.

**Transformation**:
```
Before:                    After:
v0 = const 10             v0 = const 10
v1 = v0                   ; copy eliminated
v2 = add v1, 5            v2 = add v0, 5   ; v1 → v0
return v2                 return v2
```

**Test Coverage**: 7 tests in `test-copy-prop.rkt`

---

#### 4. SCCP - Sparse Conditional Constant Propagation (`sccp.rkt`)

**GCC Reference**: `tree-ssa-ccp.cc`

**Description**: Combines constant propagation with dead branch elimination using lattice-based analysis.

**Transformation**:
```
Before:                    After:
v0 = const 1              v0 = const 1
if v0 then B1 else B2     jump B1         ; branch eliminated
B1: v1 = 10               B1: v1 = 10
B2: v1 = 20               ; B2 eliminated (unreachable)
```

**Test Coverage**: 13 tests (4 lattice + 9 SCCP) in `test-sccp.rkt`

---

#### 5. GVN - Global Value Numbering (`gvn.rkt`)

**GCC Reference**: `tree-ssa-sccvn.cc`

**Description**: Assigns identical value numbers to equivalent expressions, enabling redundancy elimination.

**Transformation**:
```
Before:                    After:
v0 = add a, b             v0 = add a, b
v1 = add a, b             ; eliminated, v1 = v0
v2 = mul v0, v1           v2 = mul v0, v0
```

**Test Coverage**: 10 tests in `test-gvn.rkt`

---

#### 6. PHI Optimization (`phi-opt.rkt`)

**GCC Reference**: `tree-ssa-phiopt.cc`

**Description**: Simplifies or eliminates SSA phi nodes.

**Transformation**:
```
Before:                    After:
x = phi(a, a)             x = a           ; trivial phi
y = phi(x, z)             y = phi(a, z)   ; propagated
```

**Patterns Recognized**:
- Trivial phi: `phi(a, a)` → `a`
- Self-referential: `x = phi(x, a)` → `x = a`
- Phi chain propagation

**Test Coverage**: 9 tests in `test-phi-opt.rkt`

---

#### 7. Forward Propagation (`forward-prop.rkt`)

**GCC Reference**: `tree-ssa-forwprop.cc`

**Description**: Propagates single-use expressions forward, simplifying patterns.

**Transformation**:
```
Before:                    After:
v0 = not a                ; eliminated
v1 = not v0               v1 = a          ; double negation
```

**Patterns**:
- `not(not(a))` → `a`
- `neg(neg(a))` → `a`
- `(a + b) - b` → `a`
- `(a << 2) << 3` → `a << 5`

**Test Coverage**: 14 tests in `test-forward-prop.rkt`

---

#### 8. Expression Reassociation (`reassoc.rkt`)

**GCC Reference**: `tree-ssa-reassoc.cc`

**Description**: Reorders associative/commutative expressions for optimization.

**Transformation**:
```
Before:                    After:
v0 = add a, 1             v0 = add a, 6   ; constants combined
v1 = add v0, 2
v2 = add v1, 3
```

**Test Coverage**: 12 tests in `test-reassoc.rkt`

---

### Phase 2: Loop Optimizations

#### 9. Loop Analysis (`loop-analysis.rkt`)

**GCC Reference**: `cfgloop.cc`

**Description**: Detects natural loops and builds loop tree.

**Provides**:
- Loop header detection
- Back edge identification
- Loop body computation
- Nesting depth analysis

**Test Coverage**: 4 tests in `test-licm.rkt`

---

#### 10. LICM - Loop Invariant Code Motion (`licm.rkt`)

**GCC Reference**: `tree-ssa-loop-im.cc`

**Description**: Moves loop-invariant computations out of loops.

**Transformation**:
```
Before:                    After:
                          v_inv = mul x, y  ; hoisted
loop:                     loop:
  v0 = mul x, y             v0 = v_inv
  v1 = add v0, i            v1 = add v0, i
  ...                       ...
```

**Test Coverage**: 5 tests in `test-licm.rkt`

---

#### 11. Strength Reduction (`strength-reduce.rkt`)

**GCC Reference**: `tree-ssa-loop-ivopts.cc`

**Description**: Replaces expensive operations with cheaper equivalents.

**Transformations**:
| Before | After |
|--------|-------|
| `x * 2` | `x << 1` |
| `x * 4` | `x << 2` |
| `x / 2` | `x >> 1` (unsigned) |
| `x % 2` | `x & 1` |
| `x * 0` | `0` |
| `x * 1` | `x` |
| `x + 0` | `x` |

**Test Coverage**: 12 tests in `test-strength-reduce.rkt`

---

#### 12. Loop Normalization (`loop-normalize.rkt`)

**GCC Reference**: `tree-ssa-loop-ivcanon.cc`

**Description**: Converts loops to canonical form (start=0, step=1).

**Transformation**:
```
Before:                    After:
for (i = 5; i < 15; i++)  for (i' = 0; i' < 10; i'++)
  use(i)                    i = i' + 5
                            use(i)
```

**Test Coverage**: 10 tests in `test-loop-normalize.rkt`

---

#### 13. Induction Variable Optimization (`ivopts.rkt`)

**GCC Reference**: `tree-ssa-loop-ivopts.cc`

**Description**: Optimizes loop induction variables through strength reduction.

**Transformation**:
```
Before:                    After:
for (i = 0; i < n; i++)   t = a               ; base pointer
  a[i * 4]                for (i = 0; i < n; i++)
                            *t                ; use direct
                            t += 4            ; strength reduced
```

**Detects**:
- Basic IVs (BIV): `i = i + step`
- Derived IVs (DIV): `j = i * scale + offset`

**Test Coverage**: 10 tests in `test-ivopts.rkt`

---

#### 14. Loop Unrolling (`loop-unroll.rkt`)

**GCC Reference**: `loop-unroll.cc`

**Description**: Duplicates loop body to reduce overhead.

**Transformation** (unroll factor = 2):
```
Before:                    After:
for (i = 0; i < 4; i++)   i = 0
  body(i)                 body(0)
                          body(1)
                          body(2)
                          body(3)
                          ; loop eliminated
```

**Strategies**:
- Full unroll: When trip count is small and known
- Partial unroll: For larger loops

**Test Coverage**: 10 tests in `test-loop-unroll.rkt`

---

#### 15. Loop Distribution (`loop-distrib.rkt`)

**GCC Reference**: `tree-loop-distribution.cc`

**Description**: Splits loop with independent statements into multiple loops.

**Transformation**:
```
Before:                    After:
for (i)                   for (i)
  a[i] = 1                  a[i] = 1
  b[i] = 2
                          for (i)
                            b[i] = 2
```

**Benefits**: Enables vectorization, improves cache locality

**Test Coverage**: 10 tests in `test-loop-distrib.rkt`

---

#### 16. Loop Interchange (`loop-interchange.rkt`)

**GCC Reference**: `gimple-loop-interchange.cc`

**Description**: Reorders nested loop dimensions for better cache locality.

**Transformation**:
```
Before:                    After:
for (i)                   for (j)
  for (j)                   for (i)
    a[j][i] = ...            a[j][i] = ...
                          ; column-major → row-major access
```

**Test Coverage**: 10 tests in `test-loop-interchange.rkt`

---

### Phase 3: Control Flow Optimizations

#### 17. Jump Threading (`jump-thread.rkt`)

**GCC Reference**: `tree-ssa-threadedge.cc`

**Description**: Eliminates redundant conditional branches by threading paths.

**Transformation**:
```
Before:                    After:
B0: if (x) goto B1        B0: if (x) goto B1
B1: if (x) goto B2        B1: goto B2       ; condition known true
```

**Test Coverage**: 10 tests in `test-jump-thread.rkt`

---

#### 18. If Combining (`if-combine.rkt`)

**GCC Reference**: `tree-ssa-ifcombine.cc`

**Description**: Merges adjacent if-statements into logical operations.

**Transformation**:
```
Before:                    After:
if (a)                    if (a && b)
  if (b)                    then-block
    then-block
```

**Patterns**:
- AND pattern: nested ifs → `a && b`
- OR pattern: if-else chain → `a || b`

**Test Coverage**: 10 tests in `test-if-combine.rkt`

---

#### 19. Tail Merging (`tail-merge.rkt`)

**GCC Reference**: `tree-ssa-tail-merge.cc`

**Description**: Merges blocks with identical instruction sequences.

**Transformation**:
```
Before:                    After:
B1: x = 1                 B1: goto B3
    goto B3               B2: goto B3
B2: x = 1                 B3: x = 1        ; merged
    goto B3                   ...
B3: ...
```

**Test Coverage**: 10 tests in `test-tail-merge.rkt`

---

#### 20. Dominance-based Optimization (`dom-opt.rkt`)

**GCC Reference**: `tree-ssa-dom.cc`

**Description**: Uses dominator tree for value range analysis and simplification.

**Transformation**:
```
Before:                    After:
if (x > 0)                if (x > 0)
  if (x > 0)                ; eliminated (dominated, always true)
    body                      body
```

**Test Coverage**: 10 tests in `test-dom-opt.rkt`

---

#### 21. Alias Analysis (`alias-analysis.rkt`)

**GCC Reference**: `tree-ssa-alias.cc`

**Description**: Determines which memory references may alias.

**Provides**:
- Must-alias: definitely same location
- May-alias: possibly same location
- No-alias: definitely different

**Used by**: DSE, LICM, PRE

**Test Coverage**: 12 tests in `test-alias-analysis.rkt`

---

### Phase 4: Memory Optimizations

#### 22. Dead Store Elimination (`dse.rkt`)

**GCC Reference**: `tree-ssa-dse.cc`

**Description**: Removes stores that are overwritten before being read.

**Transformation**:
```
Before:                    After:
store x, loc              ; eliminated
store y, loc              store y, loc
load loc → v              load loc → v
```

**Test Coverage**: 10 tests in `test-dse.rkt`

---

#### 23. PHI Propagation (`phi-prop.rkt`)

**GCC Reference**: `tree-ssa-phiprop.cc`

**Description**: Propagates loads through PHI nodes.

**Transformation**:
```
Before:                    After:
B1: p1 = &a               B1: v1 = load a
B2: p2 = &b               B2: v2 = load b
B3: p = phi(p1, p2)       B3: v = phi(v1, v2)  ; load hoisted
    v = load *p
```

**Test Coverage**: 10 tests in `test-phi-prop.rkt`

---

### Phase 5: Advanced Optimizations

#### 24. Partial Redundancy Elimination (`pre.rkt`)

**GCC Reference**: `tree-ssa-pre.cc`

**Description**: Eliminates partially redundant computations through code motion.

**Types**:
- **Full redundancy**: All paths compute same expression
- **Partial redundancy**: Some paths compute expression

**Transformation**:
```
Before:                    After:
B1: ...                   B1: t = a + b      ; inserted
B2: x = a + b             B2: x = t          ; reused
B3: y = a + b             B3: y = t          ; reused
```

**Test Coverage**: 12 tests in `test-pre.rkt`

---

#### 25. Function Inlining (`inline.rkt`)

**GCC Reference**: `tree-inline.cc`

**Description**: Substitutes function calls with function body.

**Transformation**:
```
Before:                    After:
def add(x, y):            ; add inlined
  return x + y

main:                     main:
  r = call add(3, 4)        t0 = 3           ; param copy
                            t1 = 4
                            t2 = t0 + t1     ; inlined body
                            r = t2
```

**Heuristics**:
- Inline small functions (< 10 insns)
- Inline single-call functions
- Cost-benefit analysis

**Test Coverage**: 10 tests in `test-inline.rkt`

---

## Optimization Pipeline

The passes execute in this order:

```
Input CFG
    │
    ├─1. Inline          (enable opportunities)
    ├─2. GVN             (value numbering)
    ├─3. PRE             (partial redundancy)
    ├─4. Copy Propagation
    ├─5. PHI Optimization (pass 1)
    ├─6. PHI Propagation
    ├─7. SCCP            (constant propagation)
    ├─8. Constant Folding
    ├─9. Forward Propagation
    ├─10. Reassociation
    ├─11. Strength Reduction
    ├─12. Algebraic Simplification
    ├─13. Loop Normalization
    ├─14. Loop Distribution
    ├─15. Loop Interchange
    ├─16. IVOPTS
    ├─17. Loop Unrolling
    ├─18. LICM
    ├─19. Jump Threading
    ├─20. If Combining
    ├─21. Tail Merging
    ├─22. Dominance Optimization
    ├─23. PHI Optimization (pass 2)
    ├─24. DSE
    └─25. DCE            (cleanup)
    │
Output Optimized CFG
```

---

## Future Work

### Planned Optimizations

| Optimization | GCC File | Priority | Complexity |
|-------------|----------|----------|------------|
| Code Sinking | `tree-ssa-sink.cc` | High | Medium |
| Loop Header Copy | `tree-ssa-loop-ch.cc` | Medium | Medium |
| Loop Unswitching | `tree-ssa-loop-unswitch.cc` | Medium | Medium |
| Loop Fusion | `gimple-loop-jam.cc` | Low | Complex |
| Math Optimization | `tree-ssa-math-opts.cc` | Medium | Medium |
| Branch Prediction | `predict.cc` | Medium | Medium |

### Implementation Priority

1. **Code Sinking** - Complement to LICM, moves code closer to use
2. **Loop Unswitching** - Hoists loop-invariant conditionals
3. **Loop Header Copy** - Better loop structure for optimization

---

## Non-Portable Optimizations

These GCC optimizations cannot be directly ported:

### Architecture-Specific

| Optimization | Reason |
|-------------|--------|
| Vectorization (SLP/Loop) | Requires SIMD instruction set |
| Auto-increment | Architecture-specific addressing modes |
| Hardware loops | DSP-specific loop instructions |
| Prefetching | Cache architecture dependent |

### RTL-Level

| Optimization | Reason |
|-------------|--------|
| Register Allocation | After instruction selection |
| Instruction Scheduling | Machine-dependent |
| Peephole | Pattern matching on machine code |

### Language-Specific

| Optimization | Reason |
|-------------|--------|
| String operations | C/C++ specific (`strlen`, `strcpy`) |
| Exception handling | Language runtime dependent |
| Virtual dispatch | OOP language specific |

### Analysis-Heavy

| Optimization | Reason |
|-------------|--------|
| Interprocedural | Requires whole-program analysis |
| Profile-guided (PGO) | Requires runtime profiling |
| Link-time (LTO) | Requires linker integration |

---

## Testing

### Test Statistics

| Pass | Test File | Tests | Lines |
|------|-----------|-------|-------|
| Constant Fold | `test-const-fold.rkt` | 10 | 180 |
| DCE | `test-dce.rkt` | 9 | 160 |
| Copy Prop | `test-copy-prop.rkt` | 7 | 140 |
| SCCP | `test-sccp.rkt` | 13 | 220 |
| GVN | `test-gvn.rkt` | 10 | 180 |
| PHI Opt | `test-phi-opt.rkt` | 9 | 200 |
| Forward Prop | `test-forward-prop.rkt` | 14 | 280 |
| Reassoc | `test-reassoc.rkt` | 12 | 240 |
| LICM | `test-licm.rkt` | 5 | 200 |
| Strength Reduce | `test-strength-reduce.rkt` | 12 | 220 |
| Loop Normalize | `test-loop-normalize.rkt` | 10 | 300 |
| IVOPTS | `test-ivopts.rkt` | 10 | 280 |
| Loop Unroll | `test-loop-unroll.rkt` | 10 | 300 |
| Loop Distrib | `test-loop-distrib.rkt` | 10 | 300 |
| Loop Interchange | `test-loop-interchange.rkt` | 10 | 300 |
| Jump Thread | `test-jump-thread.rkt` | 10 | 250 |
| If Combine | `test-if-combine.rkt` | 10 | 250 |
| Tail Merge | `test-tail-merge.rkt` | 10 | 250 |
| Dom Opt | `test-dom-opt.rkt` | 10 | 250 |
| Alias Analysis | `test-alias-analysis.rkt` | 12 | 280 |
| DSE | `test-dse.rkt` | 10 | 230 |
| PHI Prop | `test-phi-prop.rkt` | 10 | 250 |
| PRE | `test-pre.rkt` | 12 | 350 |
| Inline | `test-inline.rkt` | 10 | 210 |
| **Total** | **24 files** | **249** | **~5,800** |

### Running Tests

```bash
# All tests
for f in compiler/optim/cfg/test-*.rkt; do
  echo "=== $f ===" && racket "$f"
done

# Single test
racket compiler/optim/cfg/test-gvn.rkt

# Real Java code test
racket compiler/optim/cfg/test-pipeline-java.rkt
```

---

## Comparison with GCC

### Similarities

1. **SSA-based IR**: Both use SSA form for analysis
2. **Pass ordering**: Similar optimization sequence
3. **Algorithms**: Same core algorithms (GVN, SCCP, PRE)

### Differences

1. **IR Level**: Our CFG is higher-level than GIMPLE
2. **Target**: RVSDG vs machine code
3. **Scope**: Single-function vs whole-program
4. **Backend**: No register allocation or scheduling

### Code Size Comparison

| Component | GCC (C++) | This Project (Racket) |
|-----------|-----------|----------------------|
| Constant Fold | ~4,000 lines | ~150 lines |
| DCE | ~1,500 lines | ~120 lines |
| GVN | ~6,000 lines | ~200 lines |
| LICM | ~2,000 lines | ~180 lines |
| PRE | ~3,500 lines | ~400 lines |

The Racket implementation is significantly smaller due to:
- Functional programming style
- Pattern matching
- Immutable data structures
- No low-level memory management

---

## References

- GCC Source: https://gcc.gnu.org/git/gcc.git
- GCC Internals: https://gcc.gnu.org/onlinedocs/gccint/
- SSA Book: "SSA-based Compiler Design" (Springer)
- Dragon Book: "Compilers: Principles, Techniques, and Tools"
