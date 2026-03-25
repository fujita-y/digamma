# Digamma

Digamma is a high-performance, experimental Scheme implementation. It features a modern virtual machine (**nanos**) with an LLVM-based JIT compiler and a concurrent garbage collector.

## Key Features

- **LLVM on-demand compile**: Dynamically compiles Scheme code to efficient machine code on demand.
- **Concurrent garbage collection**: A multi-threaded, concurrent heap designed for low-latency execution.
- **Original syntax-case and syntax-rules implementation**: A robust and efficient implementation of Scheme's macro system.
- **First-class continuation support**: Native and efficient support for `call/cc` and `dynamic-wind` powered by Boost.Context.
- **ARM64 TBI Support**: Leverages ARM64 Top Byte Ignoring for efficient pointer tagging.
- **Modern C++**: Built with C++20 for performance and safety.

## Requirements

- **LLVM 22.1.1 or later**
- **CMake 3.13.4 or later**
- **vcpkg** (recommended for managing dependencies)
- **Boost 1.88 or later** (specifically `boost_context`)
- **replxx 1.1.1 or later** (for interactive REPL features)
- **CLI11 2.5.0 or later** (for command-line argument parsing)

## Building

To build Digamma, follow these steps:

```bash
mkdir build && cd build
cmake ..
make -j$(nproc)
```

## Usage

Run the `nanos` executable:

```bash
./nanos [options] [script]
```

Use `./nanos --help` for a full list of command-line options.

## Acknowledgments

Digamma is an experimental implementation. It is the ancestor and the successor of [Ypsilon](https://github.com/fujita-y/ypsilon).
