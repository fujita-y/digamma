# Digamma

Digamma is an experimental Scheme implementation featuring a self-hosted compiler, an LLVM-based JIT backend, a concurrent garbage collector, and a tagged-pointer object system with ARM64 TBI support. Its virtual machine, **nanos**, compiles Scheme to native code on-demand through a multi-stage pipeline.

## Features

### 1. Fiber-Based Concurrency, Networking, and Asynchronous I/O

Digamma implements lightweight, cooperative multitasking using **fibers** backed by `boost::fibers`. The fiber scheduler is wired directly into Boost.Asio's `io_context`, so I/O completion handlers run inline without spawning OS threads.

```scheme
(let* ((f1 (fiber (lambda ()
                    (display "Fiber 1 starting\n")
                    (fiber-sleep-for 100)
                    (display "Fiber 1 done\n")
                    42)))
       (f2 (fiber (lambda ()
                    (display "Fiber 2 starting\n")
                    (fiber-yield)
                    (display "Fiber 2 done\n")
                    'done))))
  (display (list (future-get f1) (future-get f2)))
  (newline))
```

**Fiber primitives:**
- `(fiber <thunk>)` → future — spawns a fiber.
- `(fiber-yield)` — cooperatively yields.
- `(fiber-sleep-for <msec>)` — suspends without blocking the scheduler.
- `(future-get <future>)`, `(future-wait <future>)`, `(future-wait-for <future> <msec>)` — synchronization.
- `(future? <obj>)` — predicate.

**Async networking primitives** (fiber-aware, no thread blocking):
- `(get-bytevector-n-async <port> <n>)` → future — non-blocking port read.
- `(https-get <url> <port>)` — blocking HTTPS GET (suspends current fiber).
- `(https-get-async <url> <port>)` → future — detached fiber HTTPS GET.

---

### 2. Fiber-Native Google Cloud AI Integration

Digamma provides **built-in, non-blocking access to Vertex AI (Gemini) and Dialogflow CX** — directly from Scheme, with zero mutator blocking. Unlike typical FFI wrappers that stall the interpreter, every AI call is dispatched through the fiber scheduler using `asio-grpc`, so other fibers keep running while cloud inference is in flight.

#### Vertex AI / Gemini

```scheme
;; Synchronous — blocks the current fiber, not the scheduler
(generate-content "Explain monads in one sentence.")

;; Asynchronous — returns a future immediately; other fibers run concurrently
(let ((f (generate-content-async "Write a haiku about Scheme.")))
  (display "Doing other work while waiting...\n")
  (display (future-get f))
  (newline))
```

**Primitives:**
- `(vertex-generate-content prompt [model] [location] [project-id])` — synchronous call.
- `(vertex-generate-content-async prompt [model] [location] [project-id])` → future — non-blocking, fiber-aware.

Default model: `gemini-2.5-flash`. Reads `GOOGLE_CLOUD_PROJECT` and `GOOGLE_CLOUD_LOCATION` from the environment.

#### Dialogflow CX

```scheme
;; Dispatch two intents concurrently; no thread management needed
(let* ((f1 (dialogflow-cx-detect-intent-async "Book a flight" "my-agent-id"))
       (f2 (dialogflow-cx-detect-intent-async "Cancel order" "my-agent-id")))
  (display (future-get f1)) (newline)
  (display (future-get f2)) (newline))
```

**Primitives:**
- `(dialogflow-cx-detect-intent text agent-id [session-id] [location] [project-id] [language-code])` — synchronous.
- `(dialogflow-cx-detect-intent-async text agent-id [session-id] [location] [project-id] [language-code])` → future — non-blocking.

Session IDs are auto-generated (UUID) if not provided.

> [!NOTE]
> Both APIs require setting the `GOOGLE_CLOUD_PROJECT` environment variable (or passing it as an argument). `GOOGLE_CLOUD_LOCATION` defaults to `us-central1`.

---

### 3. LLVM JIT Compilation (On-Demand, per Scheme Expression)

Nanos uses **LLVM ORC** (JITLink + CompileOnDemand) to compile each Scheme expression to native machine code on the fly. The pipeline runs entirely in-process with no ahead-of-time batch step:

```
Source → Macro Expand → Optimize → Lambda Lift → Compile → LLVM IR → Native Code
```

Notable code-generation optimizations:
- **Escape and stack-allocation analysis** (`phase2b`–`phase2d`): elides heap allocation and write barriers for short-lived closures.
- **Inlined primitives**: common arithmetic, predicate, and list operations emitted as inline LLVM IR.
- **Tail-call optimization**: proper tail calls via LLVM musttail.

---

### 4. ARM64 Top Byte Ignore (TBI) Tagged Pointers

On ARM64, Digamma exploits the hardware **Top Byte Ignore** feature to store type tags in the high byte of every pointer. In release builds, tagged pointers can be dereferenced directly — no masking needed, zero runtime cost for tag removal.

| Type | Tag |
|---|---|
| Fixnum (63-bit) | `.....1` |
| Cons (pair) | `...000` |
| Heap object | `...010` + 6-bit type code |
| Short flonum (61-bit) | `...100` |

Heap objects use a 6-bit type code (`tc6`) supporting symbols, strings, vectors, closures, fibers, ports, hashtables, cells, and more.

---

### 5. Concurrent Garbage Collector

The GC is a **mostly-concurrent, mark-sweep collector** running on a dedicated thread:

- **Slab allocator**: fixed-size object pools with per-slab bitmaps for fast allocation and sweep.
- **Tri-color marking**: concurrent marking with write barriers and a shade queue for mutator cooperation.
- **Multi-phase STW**: three short stop-the-world pauses (root snapshot → concurrent mark → final mark) minimize mutator pause times.
- **Safepoints**: the compiler inserts cooperative safepoints; fiber stacks are included in the root set.

---

### 6. C Foreign Function Interface (CFFI)

The `(core cffi)` module provides a dynamic C FFI backed by LLVM ORC:

- `load-shared-object` / `lookup-shared-object` — load dynamic libraries and resolve C symbols.
- `c-function` — bind Scheme procedures to C functions with typed signatures (`int`, `double`, `void*`, `size_t`, …).
- `c-callback` — create native C function pointers that invoke Scheme closures (enables C callback integration).

---

### 7. Self-Hosted Compiler and Hygienic Macros

The entire compiler pipeline is written in Scheme and cross-compiled into a boot image (`boot/core.ir`):

1. **Macro expansion**: full R6RS `syntax-case` and R7RS `syntax-rules` with hygienic renaming, `let-syntax`, `letrec-syntax`, module-level macro export/import.
2. **Optimization**: constant folding, dead code elimination, beta reduction, let-floating, lambda dropping, inlining, unused parameter removal, pure primitive substitution.
3. **Lambda lifting**: promotes closed-over lambdas to top-level definitions where possible.
4. **Compilation**: register-based IR with closure conversion, free variable analysis, selective cell boxing for mutated/forward-referenced variables.

---

## Benchmarks

- Preliminary results from a partial [Gambit benchmark suite on Raspberry Pi 5](https://fujita-y.github.io/benchmarks/benchmark_baseline.html) compare **Nanos** against **Ypsilon 2.0.9** and **Guile 3.0.10**.
- Each result is a trimmed average of 7 runs (min/max excluded) reporting wall-clock (real) and CPU (user) time.
- Nanos records the fastest real time in **23 of 30 benchmarks**.

---

## Requirements

- **LLVM 22** or later
- **CMake 3.13.4** or later
- **vcpkg** (recommended for managing dependencies)
- **Boost 1.88** or later (`boost_context`, `boost_fiber`, `boost_asio`, `boost_beast`)
- **OpenSSL** (for TLS support in HTTPS networking)
- **replxx** (for interactive REPL)
- **CLI11** (for command-line argument parsing)
- **google-cloud-cpp** (specifically `aiplatform` and `dialogflow_cx` features)
- **asio-grpc** (for asynchronous gRPC on Asio event loops)

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
ypsilon boot/build-core-ir.scm   # or: gosh boot/build-core-ir.scm
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
cd build && ctest                    # C++ unit tests (5 test executables)
cd .. && tests/run_tests.sh          # Scheme integration tests (3 suites)
cd core/tests && ./run_tests.sh      # Core compiler tests (4 suites)
```

The C++ test suite (`tests/`) is organised into five executables: `test_core` (object model and GC), `test_io` (port and I/O primitives), `test_compiler` (code-generation pipeline), `test_runtime` (evaluator and runtime behaviour), and `test_eval` (end-to-end evaluation).

The Scheme integration suite (`tests/`) covers the module system (`test_modules.scm`), macro and syntax forms (`test_syntax.scm`), and system/environment primitives (`test_system.scm`).

The core compiler suite (`core/tests/`) exercises macro expansion (`test_macroexpand.scm`), full syntax-rules/syntax-case semantics (`test_syntax.scm`), the compiler output (`test_compiler.scm`), and core language forms (`test_core.scm`).

## Acknowledgments

Digamma is the ancestor and the successor of [Ypsilon](https://github.com/fujita-y/ypsilon).

## License

BSD 2-Clause. See [LICENSE](LICENSE) for details.
