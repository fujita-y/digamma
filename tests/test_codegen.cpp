// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include <llvm/Support/TargetSelect.h>
#include <sstream>
#include "codegen.h"
#include "object_heap.h"
#include "reader.h"

void fatal(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");
  exit(1);
}

void warning(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");
}

void trace(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
}

static bool some_test_failed = false;

class CodegenTest {
 public:
  llvm::LLVMContext context;
  llvm::Module* module;
  llvm::ExecutionEngine* engine;
  codegen_t* codegen;

  CodegenTest() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();

    module = new llvm::Module("test_module", context);
    std::string err;
    engine = llvm::EngineBuilder(std::unique_ptr<llvm::Module>(module)).setErrorStr(&err).setEngineKind(llvm::EngineKind::JIT).create();

    if (!engine) {
      fprintf(stderr, "Could not create ExecutionEngine: %s\n", err.c_str());
      exit(1);
    }

    codegen = new codegen_t(context, module, engine);
  }

  ~CodegenTest() {
    delete codegen;
    delete engine;  // engine takes ownership of module
  }

  scm_obj_t read_code(const std::string& input) {
    std::istringstream is(input);
    reader_t reader(is);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) {
      throw std::runtime_error("Read error: " + reader.get_error_message());
    }
    return obj;
  }
};

void run_test(const char* name, std::function<bool(CodegenTest&)> test) {
  CodegenTest env;
  try {
    if (test(env)) {
      printf("\033[32m%s passed\033[0m\n", name);
    } else {
      printf("\033[31m###### %s failed\033[0m\n", name);
      some_test_failed = true;
    }
  } catch (const std::exception& e) {
    printf("\033[31m###### %s failed with exception: %s\033[0m\n", name, e.what());
    some_test_failed = true;
  }
}

int main(int argc, char** argv) {
  object_heap_t heap;
  heap.init(1024 * 1024 * 2, 1024 * 1024);

  run_test("ConstantReturn", [](CodegenTest& env) -> bool {
    // ((const r0 3) (ret)) ;=> 3
    scm_obj_t code = env.read_code("((const r0 3) (ret))");
    intptr_t result = env.codegen->compile(code);
    return result == 3;
  });

  run_test("MovInstruction", [](CodegenTest& env) -> bool {
    // ((const r0 10) (mov r1 r0) (mov r0 r1) (ret)) ;=> 10
    scm_obj_t code = env.read_code("((const r0 10) (mov r1 r0) (mov r0 r1) (ret))");
    intptr_t result = env.codegen->compile(code);
    return result == 10;
  });

  run_test("ControlFlow", [](CodegenTest& env) -> bool {
    // ((const r0 2) (if L1 L2) (label L1) (const r0 1) (jump L3) (label L2) (const r0 3) (label L3) (ret)) ;=> 1
    scm_obj_t code = env.read_code("((const r0 2) (if L1 L2) (label L1) (const r0 1) (jump L3) (label L2) (const r0 3) (label L3) (ret))");
    intptr_t result = env.codegen->compile(code);
    return result == 1;
  });

  run_test("ControlFlowFalse", [](CodegenTest& env) -> bool {
    // ((const r0 0) (if L1 L2) (label L1) (const r0 1) (jump L3) (label L2) (const r0 3) (label L3) (ret)) ;=> 3
    scm_obj_t code = env.read_code("((const r0 0) (if L1 L2) (label L1) (const r0 1) (jump L3) (label L2) (const r0 3) (label L3) (ret))");
    intptr_t result = env.codegen->compile(code);
    return result == 3;
  });

  run_test("ControlFlowBoolean", [](CodegenTest& env) -> bool {
    // ((const r0 #t) (if L1 L2) (label L1) (const r0 #f) (if L4 L5) (label L4) (const r0 1) (jump L6) (label L5) (const r0 2) (label L6) (jump
    // L3) (label L2) (const r0 3) (label L3) (ret))
    scm_obj_t code = env.read_code(
        "((const r0 #t) (if L1 L2) (label L1) (const r0 #f) (if L4 L5) (label L4) (const r0 1) (jump L6) (label L5) (const r0 2) (label L6) "
        "(jump L3) (label L2) (const r0 3) (label L3) (ret))");
    intptr_t result = env.codegen->compile(code);
    return result == 2;
  });

  run_test("HighRegisterCount", [](CodegenTest& env) -> bool {
    // ((const r100 123) (mov r0 r100) (ret)) ;=> 123
    scm_obj_t code = env.read_code("((const r100 123) (mov r0 r100) (ret))");
    intptr_t result = env.codegen->compile(code);
    return result == 123;
  });

  heap.destroy();
  return some_test_failed ? 1 : 0;
}
