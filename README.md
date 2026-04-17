# Digamma

Digamma is an experimental Scheme implementation featuring a self-hosted compiler, an LLVM-based JIT backend, a concurrent garbage collector, and a tagged-pointer object system with ARM64 TBI support. Its virtual machine, **nanos**, compiles Scheme to native code on-demand through a multi-stage pipeline.

## Benchmarks

Digamma is under development. Full benchmarks are not available at this time.

- [Partial gambit benchmark results on Raspberry Pi 5](bench/benchmark_baseline.html)

## Key Features

- **LLVM JIT compilation**: Compiles Scheme to native code on-demand via LLVM ORC (JITLink + CompileOnDemand).
- **Concurrent GC**: Mostly-concurrent mark-sweep collector with slab allocator and write barriers for low-latency execution.
- **Self-hosted compiler**: Multi-stage pipeline (macro expand → optimize → lambda lift → compile) written in Scheme and cross-compiled into a boot image.
- **Hygienic macros**: `syntax-case` and `syntax-rules` with module-level macro export/import.
- **First-class continuations**: `call/cc`, escape continuations, and `dynamic-wind` powered by Boost.Context.
- **Tagged pointers**: 3-bit primary tags with 6-bit type codes; ARM64 TBI for zero-cost tag masking in release builds.

## Architecture

### Virtual Machine (nanos)

Nanos is a register-based virtual machine that JIT-compiles Scheme to native code through LLVM ORC. Each Scheme expression is compiled on-demand: source code is read, macro-expanded, optimized, and emitted as LLVM IR, which is then compiled to native machine code and executed immediately.

### Object Representation

Digamma uses a tagged-pointer representation on 64-bit architectures:

| Type | Tag |
|---|---|
| Fixnum (63-bit) | `.....1` |
| Cons (pair) | `...000` |
| Heap object | `...010` with 6-bit type code |
| Short flonum (61-bit) | `...100` |

Heap objects carry a 6-bit type code (`tc6`) supporting symbols, strings, vectors, closures, continuations, ports, hashtables, cells, and more.

On ARM64, the top byte of pointers is used for type tags via hardware TBI (Top Byte Ignore), allowing tagged pointers to be dereferenced without masking in release builds.

### Memory Management

The garbage collector is a concurrent, mostly-non-moving mark-sweep collector running on a dedicated thread:

- **Slab allocator**: Fixed-size object pools with per-slab bitmaps for fast allocation and sweep.
- **Tri-color marking**: Concurrent marking with write barriers and a shade queue for mutator cooperation.
- **Multi-phase collection**: Three-phase stop-the-world pauses (root snapshot → concurrent mark → final mark) to minimize mutator pause times.
- **Safepoints**: The mutator cooperates with the collector at safepoints inserted by the compiler.

### Compiler Pipeline

Scheme source is processed through a multi-stage pipeline, implemented in Scheme and cross-compiled to a boot image (`boot/core.ir`):

```
Source → Macro Expand → Optimize → Lambda Lift → Compile → LLVM IR → Native Code
```

1. **Macro expansion** (`core/macroexpand.scm`): Full R6RS `syntax-case` and R7RS `syntax-rules` with hygienic renaming, `let-syntax`, `letrec-syntax`, `define-syntax`, and module-level macro export/import.

2. **Optimization** (`core/optimizer.scm`): Constant folding, dead code elimination, beta reduction, let-floating, lambda dropping, inlining, unused parameter removal, and pure primitive substitution.

3. **Lambda lifting** (`core/lambda-lift.scm`): Promotes closed-over lambdas from `let` and `letrec*` bindings to top-level definitions, eliminating closure allocation for functions whose free variables are limited to sibling bindings.

4. **Compilation** (`core/compiler.scm`): Emits a register-based IR with closure conversion, free variable analysis, and cell-based boxing for mutated and forward-referenced variables. The compiler emits `letrec*` as a first-class core form with selective cell boxing — only variables that are mutated or forward-referenced by lambda initializers require cells; all others use direct register access.

5. **Code generation** (`src/codegen.cpp`, `src/codegen_emit.cpp`): Translates the IR to LLVM IR, handling closure creation, tail calls, global references, cell operations, and safepoint insertion. Uses LLVM ORC (JITLink + CompileOnDemand) for lazy compilation.

### Continuations

First-class continuations are implemented using Boost.Context for stack capture and restoration. The implementation supports:

- `call/cc` (full continuations with stack copy)
- `call-with-escape-continuation` (one-shot escape continuations)
- `dynamic-wind` for winding/unwinding protection

Continuation objects are GC-managed with memoized live-pointer arrays for efficient tracing.

### Module System

The macro expander includes a module system (`define-module`, `import-module`) with:

- Named exports and renamed exports
- Import modifiers: `prefix`, `only`, `except`, `rename`
- Macro export and import across module boundaries
- Aggregated library definitions

## Requirements

- **LLVM 22** or later
- **CMake 3.13.4** or later
- **vcpkg** (recommended for managing dependencies)
- **Boost 1.88** or later (specifically `boost_context`)
- **replxx** (for interactive REPL)
- **CLI11** (for command-line argument parsing)

## Building

```bash
mkdir build && cd build
cmake ..
make -j$(nproc)
```

### Boot Image

The compiler pipeline is self-hosted: the Scheme files in `core/` are cross-compiled by an external RnRS Scheme (such as Ypsilon or Gauche) into `boot/core.ir`, which nanos loads at startup. To regenerate the boot image after modifying core files:

```bash
rm boot/core.ir
ypsilon boot/build-core-ir.scm or gosh boot/build-core-ir.scm
```

## Usage

Run the `nanos` executable:

```bash
./build/nanos --boot boot/core.ir              # Interactive REPL
./build/nanos --boot boot/core.ir --script foo.scm  # Run a script
```

Use `./nanos --help` for a full list of command-line options.

## Testing

```bash
cd build && ctest                    # C++ unit tests (20 tests)
cd .. && tests/run_tests.sh          # Scheme integration tests
cd core/tests && ./run_tests.sh      # Core compiler tests
```

## Acknowledgments

Digamma is the ancestor and the successor of [Ypsilon](https://github.com/fujita-y/ypsilon).

## License

BSD 2-Clause. See [LICENSE](LICENSE) for details.
