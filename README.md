# Digamma

[![arm64 llvm22](https://github.com/fujita-y/digamma/actions/workflows/ci-llvm22-arm64.yml/badge.svg)](https://github.com/fujita-y/digamma/actions/workflows/ci-llvm22-arm64.yml)
[![arm64 llvm21](https://github.com/fujita-y/digamma/actions/workflows/ci-llvm21-arm64.yml/badge.svg)](https://github.com/fujita-y/digamma/actions/workflows/ci-llvm21-arm64.yml)
[![amd64 llvm21](https://github.com/fujita-y/digamma/actions/workflows/ci-llvm21-amd64.yml/badge.svg)](https://github.com/fujita-y/digamma/actions/workflows/ci-llvm21-amd64.yml)
[![docker](https://github.com/fujita-y/digamma/actions/workflows/docker.yml/badge.svg)](https://github.com/fujita-y/digamma/actions/workflows/docker.yml)

Digamma is a lightweight Scheme dialect designed for AI workflow automation.
It focuses on execution efficiency and Lisp-based extensibility rather than strict RnRS compliance.
Users seeking a complete R6RS/R7RS implementation should see [Ypsilon](https://github.com/fujita-y/ypsilon), developed by the same author.

Digamma features a self-hosted compiler, an LLVM-based JIT backend, a concurrent garbage collector, and a tagged-pointer object system with ARM64 TBI support.
Its virtual machine, **nanos**, compiles Scheme to native code on-demand through a multi-stage pipeline.

While Digamma does not target full RnRS conformance, it provides standard hygienic macros — `syntax-case` and `syntax-rules` — for expressive, composable abstractions.

### Highlights

| Category | Description |
|---|---|
| **Runtime** | Fiber-based concurrency with integrated async I/O and networking |
| **AI** | Native Vertex AI (Gemini) and Dialogflow CX integration via asio-grpc |
| **Compiler** | Self-hosted Scheme compiler with LLVM ORC JIT backend |
| **GC** | Mostly-concurrent mark-sweep collector on a dedicated thread |
| **FFI** | Dynamic C foreign function interface backed by LLVM ORC |
| **Pointers** | ARM64 TBI tagged-pointer object system with zero-cost tag access |

---

## Features

### Fiber-Based Concurrency, Networking, and Asynchronous I/O

Digamma implements lightweight, cooperative multitasking using **fibers** backed by `boost::fibers`.
The fiber scheduler is wired directly into Boost.Asio's `io_context`, so I/O completion handlers run inline without spawning OS threads.

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

<details>
<summary><b>Fiber primitives</b></summary>

| Primitive | Description |
|---|---|
| `(fiber <thunk>)` | Spawn a fiber; returns a future |
| `(fiber-yield)` | Cooperatively yield to the scheduler |
| `(fiber-sleep-for <msec>)` | Suspend without blocking the scheduler |
| `(future-get <future>)` | Block until the future resolves and return its value |
| `(future-wait <future>)` | Block until the future resolves |
| `(future-wait-for <future> <msec>)` | Block with a timeout |
| `(future? <obj>)` | Future type predicate |

</details>

<details>
<summary><b>Async networking primitives</b> (fiber-aware, non-blocking)</summary>

| Primitive | Description |
|---|---|
| `(get-bytevector-n-async <port> <n>)` | Non-blocking port read; returns a future |
| `(https-get <url> <port>)` | HTTPS GET; suspends the current fiber |
| `(https-get-async <url> <port>)` | Non-blocking HTTPS GET; returns a future |

</details>

---

### Google Cloud AI Integration

Digamma provides **built-in, non-blocking access to Vertex AI (Gemini) and Dialogflow CX** directly from Scheme.
Unlike typical FFI wrappers that stall the interpreter, every AI call is dispatched through the fiber scheduler using `asio-grpc`, so other fibers keep running while cloud inference is in flight.

#### Vertex AI / Gemini

```scheme
;; Synchronous — blocks the current fiber, not the scheduler
(generate-content "Explain monads in one sentence.")

;; Asynchronous — returns a future immediately
(let ((f (generate-content-async "Write a haiku about Scheme.")))
  (display "Doing other work while waiting...\n")
  (display (future-get f))
  (newline))
```

<details>
<summary><b>Vertex AI primitives</b></summary>

| Primitive | Description |
|---|---|
| `(generate-content prompt [model] [location] [project-id])` | Synchronous inference |
| `(generate-content-async prompt [model] [location] [project-id])` | Non-blocking; returns a future |

</details>

Default model: `gemini-2.5-flash`.

#### Dialogflow CX

```scheme
;; Dispatch two intents concurrently
(let* ((f1 (dialogflow-cx-detect-intent-async "Book a flight" "my-agent-id"))
       (f2 (dialogflow-cx-detect-intent-async "Cancel order" "my-agent-id")))
  (display (future-get f1)) (newline)
  (display (future-get f2)) (newline))
```

<details>
<summary><b>Dialogflow CX primitives</b></summary>

| Primitive | Description |
|---|---|
| `(dialogflow-cx-detect-intent text agent-id [session-id] [location] [project-id] [language-code])` | Synchronous intent detection |
| `(dialogflow-cx-detect-intent-async text agent-id [session-id] [location] [project-id] [language-code])` | Non-blocking; returns a future |

</details>

Session IDs are auto-generated (UUID) if omitted.

> [!NOTE]
> Both APIs require the `GOOGLE_CLOUD_PROJECT` environment variable (or an explicit argument).
> `GOOGLE_CLOUD_LOCATION` defaults to `us-central1`.

---

### LLVM JIT Compilation

Nanos uses **LLVM ORC** (JITLink + CompileOnDemand) to compile each Scheme expression to native machine code on the fly.
The pipeline runs entirely in-process with no ahead-of-time batch step:

```
Source → Macro Expand → Optimize → Lambda Lift → Compile → LLVM IR → Native Code
```

Key code-generation optimizations:

- **Escape and stack-allocation analysis** — elides heap allocation and write barriers for short-lived closures.
- **Inlined primitives** — common arithmetic, predicate, and list operations emitted as inline LLVM IR.
- **Tail-call optimization** — proper tail calls via LLVM `musttail`.

---

### ARM64 Tagged Pointers (TBI)

On ARM64, Digamma exploits the hardware **Top Byte Ignore** feature to store type tags in the high byte of every pointer.
In release builds, tagged pointers can be dereferenced directly — no masking needed, zero runtime cost for tag removal.

| Type | Tag Encoding |
|---|---|
| Fixnum (63-bit) | `.....1` |
| Cons (pair) | `...000` |
| Heap object | `...010` + type code in high byte|
| Short flonum (61-bit) | `...100` |

Heap objects use a 6-bit type code (`tc6`) supporting symbols, strings, vectors, closures, fibers, ports, hashtables, cells, and more.

---

### Concurrent Garbage Collector

The GC is a **mostly-concurrent, mark-sweep collector** running on a dedicated thread:

- **Slab allocator** — fixed-size object pools with per-slab bitmaps for fast allocation and sweep.
- **Tri-color marking** — concurrent marking with write barriers and a shade queue for mutator cooperation.
- **Multi-phase STW** — three short stop-the-world pauses (root snapshot → concurrent mark → final mark) minimize mutator pause times.
- **Safepoints** — the compiler inserts cooperative safepoints; fiber stacks are included in the root set.

---

### C Foreign Function Interface

The `(core cffi)` module provides a dynamic C FFI backed by LLVM ORC:

<details>
<summary><b>FFI primitives</b></summary>

| Primitive | Description |
|---|---|
| `load-shared-object` / `lookup-shared-object` | Load dynamic libraries and resolve C symbols |
| `c-function` | Bind Scheme procedures to C functions with typed signatures (`int`, `double`, `void*`, `size_t`, …) |
| `c-callback` | Create native C function pointers that invoke Scheme closures |

</details>

---

### Self-Hosted Compiler and Hygienic Macros

The entire compiler pipeline is written in Scheme and cross-compiled into a boot image (`boot/core.ir`):

1. **Macro expansion** — full R6RS `syntax-case` and R7RS `syntax-rules` with hygienic renaming, `let-syntax`, `letrec-syntax`, and module-level macro export/import.
2. **Optimization** — constant folding, dead code elimination, beta reduction, let-floating, lambda dropping, inlining, unused parameter removal, pure primitive substitution.
3. **Lambda lifting** — promotes closed-over lambdas to top-level definitions where possible.
4. **Compilation** — register-based IR with closure conversion, free variable analysis, selective cell boxing for mutated/forward-referenced variables.

---

## Benchmarks

Preliminary results from a partial [Gambit benchmark suite on Raspberry Pi 5](https://fujita-y.github.io/benchmarks/benchmark_baseline.html) compare **Nanos** against **Ypsilon 2.0.9** and **Guile 3.0.10**.
Each result is a trimmed average of 7 runs (min/max excluded) reporting wall-clock and CPU time.
Nanos records the fastest wall-clock time in **23 of 30 benchmarks**.

---

## Getting Started

### Requirements

| Dependency | Notes |
|---|---|
| **LLVM 21+** | JIT backend |
| **CMake 3.13.4+** | Build system |
| **Boost 1.88+** | `fiber`, `asio`, `beast` |
| **OpenSSL** | TLS support for HTTPS networking |
| **vcpkg** | Recommended for dependency management |
| **replxx** | Interactive REPL |
| **CLI11** | Command-line argument parsing |
| **google-cloud-cpp** | `aiplatform` and `dialogflow_cx` features |
| **asio-grpc** | Asynchronous gRPC on Asio event loops |

### Building

```bash
mkdir build && cd build
cmake ..
make -j$(nproc)
```

### Docker

A pre-built Docker image is available on [Docker Hub](https://hub.docker.com/r/fujitay/digamma):

```bash
# x86_64
docker run --rm -it fujitay/digamma:latest-amd64-llvm21

# ARM64
docker run --rm -it fujitay/digamma:latest-arm64-llvm22
```

### Rebuilding the Boot Image

The compiler pipeline is self-hosted: the Scheme files in `core/` are cross-compiled by an external RnRS Scheme (such as Ypsilon or Gauche) into `boot/core.ir`, which nanos loads at startup.
To regenerate the boot image after modifying core files:

```bash
rm boot/core.ir
ypsilon boot/build-core-ir.scm   # or: gosh boot/build-core-ir.scm
```

### Usage

```bash
./build/nanos --boot boot/core.ir                   # Interactive REPL
./build/nanos --boot boot/core.ir --script foo.scm   # Run a script
```

Run `./nanos --help` for a full list of command-line options.

---

## Testing

```bash
cd build && ctest                    # C++ unit tests
cd .. && tests/run_tests.sh          # Scheme integration tests
cd core/tests && ./run_tests.sh      # Core compiler tests
```

<details>
<summary><b>Test suite details</b></summary>

**C++ unit tests** (`tests/`) — five executables:

| Executable | Coverage |
|---|---|
| `test_core` | Object model and GC |
| `test_io` | Port and I/O primitives |
| `test_compiler` | Code-generation pipeline |
| `test_runtime` | Evaluator and runtime behaviour |
| `test_eval` | End-to-end evaluation |

**Scheme integration tests** (`tests/`):

| Suite | Coverage |
|---|---|
| `test_modules.scm` | Module system |
| `test_syntax.scm` | Macro and syntax forms |
| `test_system.scm` | System/environment primitives |

**Core compiler tests** (`core/tests/`):

| Suite | Coverage |
|---|---|
| `test_macroexpand.scm` | Macro expansion |
| `test_syntax.scm` | syntax-rules / syntax-case semantics |
| `test_compiler.scm` | Compiler output |
| `test_core.scm` | Core language forms |

</details>

---

## Acknowledgments

Digamma is the ancestor and the successor of [Ypsilon](https://github.com/fujita-y/ypsilon).

## License

BSD 2-Clause. See [LICENSE](LICENSE) for details.
