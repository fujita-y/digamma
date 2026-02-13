// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include <llvm/Support/TargetSelect.h>
#include <sstream>
#include "codegen.h"
#include "hash.h"
#include "object_heap.h"
#include "reader.h"

// Helper macros
#define CAR(x) (((scm_cons_rec_t*)(x))->car)
#define CDR(x) (((scm_cons_rec_t*)(x))->cdr)

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

class ClosureAnalysisTest {
 public:
  llvm::LLVMContext context;
  llvm::Module* module;
  llvm::ExecutionEngine* engine;
  codegen_t* codegen;

  ClosureAnalysisTest() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();

    module = new llvm::Module("test_module", context);
    std::string err;
    engine = llvm::EngineBuilder(std::unique_ptr<llvm::Module>(module)).setErrorStr(&err).setEngineKind(llvm::EngineKind::JIT).create();

    if (!engine) {
      fprintf(stderr, "Could not create ExecutionEngine: %s\n", err.c_str());
      exit(1);
    }

    codegen = new codegen_t(context, engine);
  }

  ~ClosureAnalysisTest() {
    delete codegen;
    // engine takes ownership of module
    delete engine;
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

  void analyze(const std::string& code_str) {
    scm_obj_t code = read_code(code_str);
    codegen->phase0_create_module();
    codegen->phase1_parse_instructions(code);
    codegen->analyze_closure_labels();
  }

  // Helper to find instruction by opcode
  const Instruction* find_first_instruction(Opcode op) {
    for (const auto& func : codegen->functions) {
      for (const auto& inst : func.instructions) {
        if (inst.op == op) {
          return &inst;
        }
      }
    }
    return nullptr;
  }

  // Helper to find instruction by opcode in a specific function (by label name)
  const Instruction* find_instruction_in_function(const char* label_name, Opcode op) {
    // Find function with this label
    for (const auto& func : codegen->functions) {
      if (func.label != scm_nil) {
        const char* name = (const char*)((scm_symbol_rec_t*)to_address(func.label))->name;
        if (strcmp(name, label_name) == 0) {
          for (const auto& inst : func.instructions) {
            if (inst.op == op) return &inst;
          }
        }
      } else {
        // Main function
        if (label_name == nullptr) {
          for (const auto& inst : func.instructions) {
            if (inst.op == op) return &inst;
          }
        }
      }
    }
    return nullptr;
  }
};

static bool some_test_failed = false;

void run_test(const char* name, std::function<bool(ClosureAnalysisTest&)> test) {
  printf("Running test: %s\n", name);
  fflush(stdout);
  ClosureAnalysisTest env;
  try {
    if (test(env)) {
      printf("\033[32m%s passed\033[0m\n", name);
      fflush(stdout);
    } else {
      printf("\033[31m###### %s failed\033[0m\n", name);
      some_test_failed = true;
      fflush(stdout);
    }
  } catch (const std::exception& e) {
    printf("\033[31m###### %s failed with exception: %s\033[0m\n", name, e.what());
    some_test_failed = true;
    fflush(stdout);
  }
}

int main() {
  printf("Starting test_closure_analysis\n");
  object_heap_t heap;
  heap.init(1024 * 1024 * 2, 1024 * 1024);

  run_test("SimpleLocalClosure", [](ClosureAnalysisTest& env) -> bool {
    // ((make-closure r0 C1 () #f 0 #f) (call r0 0) (ret) (label C1) (ret))
    env.analyze("((make-closure r0 C1 () #f 0 #f) (call r0 0) (ret) (label C1) (ret))");

    const Instruction* call_inst = env.find_first_instruction(Opcode::CALL);
    if (!call_inst) {
      printf("  CALL instruction not found\n");
      return false;
    }

    if (call_inst->closure_label == scm_nil) {
      printf("  closure_label is nil\n");
      return false;
    }

    // Check if label name is C1
    const char* label_name = (const char*)((scm_symbol_rec_t*)to_address(call_inst->closure_label))->name;
    if (strcmp(label_name, "C1") != 0) {
      printf("  Expected C1, got %s\n", label_name);
      return false;
    }

    return true;
  });

  run_test("GlobalClosure", [](ClosureAnalysisTest& env) -> bool {
    // ((make-closure r0 C1 () #f 0 #f) (global-set! g r0) (global-ref r1 g) (call r1 0) (ret) (label C1) (ret))
    env.analyze("((make-closure r0 C1 () #f 0 #f) (global-set! g r0) (global-ref r1 g) (call r1 0) (ret) (label C1) (ret))");

    const Instruction* call_inst = env.find_first_instruction(Opcode::CALL);
    if (!call_inst) {
      printf("  CALL instruction not found\n");
      return false;
    }

    if (call_inst->closure_label == scm_nil) {
      printf("  closure_label is nil\n");
      return false;
    }

    const char* label_name = (const char*)((scm_symbol_rec_t*)to_address(call_inst->closure_label))->name;
    if (strcmp(label_name, "C1") != 0) {
      printf("  Expected C1, got %s\n", label_name);
      return false;
    }

    return true;
  });

  run_test("UnknownClosure", [](ClosureAnalysisTest& env) -> bool {
    // ((call r0 0) (ret)) -- r0 is unknown
    env.analyze("((call r0 0) (ret))");

    const Instruction* call_inst = env.find_first_instruction(Opcode::CALL);
    if (!call_inst) return false;

    if (call_inst->closure_label != scm_nil) {
      printf("  Expected nil closure_label, got something\n");
      return false;
    }

    return true;
  });

  run_test("ControlFlowMerge_Same", [](ClosureAnalysisTest& env) -> bool {
    // If both branches set r0 to C1, then after join r0 should be C1
    // ((make-closure r1 C1 () #f 0 #f)
    //  (if L1 L2)
    //  (label L1) (mov r0 r1) (jump L3)
    //  (label L2) (mov r0 r1) (label L3)
    //  (call r0 0) (ret)
    //  (label C1) (ret))
    env.analyze(
        "((make-closure r1 C1 () #f 0 #f) "
        "(if L1 L2) "
        "(label L1) (mov r0 r1) (jump L3) "
        "(label L2) (mov r0 r1) (label L3) "
        "(call r0 0) (ret) "
        "(label C1) (ret))");

    const Instruction* call_inst = env.find_first_instruction(Opcode::CALL);
    if (!call_inst) return false;

    if (call_inst->closure_label == scm_nil) {
      printf("  closure_label is nil, expected C1 (merged)\n");
      return false;
    }
    return true;
  });

  run_test("ControlFlowMerge_Diff", [](ClosureAnalysisTest& env) -> bool {
    // If branches set r0 to different closures, result should be nil (conservative)
    // ((make-closure r1 C1 () #f 0 #f)
    //  (make-closure r2 C2 () #f 0 #f)
    //  (if L1 L2)
    //  (label L1) (mov r0 r1) (jump L3)
    //  (label L2) (mov r0 r2) (label L3)
    //  (call r0 0) (ret)
    //  (label C1) (ret)
    //  (label C2) (ret))
    env.analyze(
        "((make-closure r1 C1 () #f 0 #f) "
        "(make-closure r2 C2 () #f 0 #f) "
        "(if L1 L2) "
        "(label L1) (mov r0 r1) (jump L3) "
        "(label L2) (mov r0 r2) (label L3) "
        "(call r0 0) (ret) "
        "(label C1) (ret) "
        "(label C2) (ret))");

    const Instruction* call_inst = env.find_first_instruction(Opcode::CALL);
    if (!call_inst) return false;

    if (call_inst->closure_label != scm_nil) {
      printf("  closure_label should be nil due to conflict\n");
      return false;
    }
    return true;
  });

  run_test("TailCallAnalysis", [](ClosureAnalysisTest& env) -> bool {
    // ((make-closure r0 C1 () #f 0 #f) (tail-call r0 0) (label C1) (ret))
    env.analyze("((make-closure r0 C1 () #f 0 #f) (tail-call r0 0) (label C1) (ret))");

    const Instruction* inst = env.find_first_instruction(Opcode::TAIL_CALL);
    if (!inst) return false;
    if (inst->closure_label == scm_nil) return false;
    return true;
  });

  // Test recursive function analysis (needs global-set! / global-ref propagation)
  run_test("RecursiveGlobal", [](ClosureAnalysisTest& env) -> bool {
    // (make-closure r0 C1 ... ) (global-set! fib r0) (call r0 ...)
    // (label C1) ... (global-ref r1 fib) (call r1 ...)

    // Note: Since we analyze functions in order, and 'fib' is set in main,
    // when we analyze C1, we might not know 'fib' yet if we don't propagate globals across functions
    // or if we don't do fixpoint iteration across functions.
    // The current implementation in codegen.cpp `analyze_closure_labels` iterates `functions` once?
    // Let's check the code for `analyze_closure_labels`...
    // It iterates `functions` once.
    // But it accumulates `global_closure_defs` across the loop over functions.
    // So if main is processed first, it populates global_closure_defs.
    // Then C1 is processed, it sees global_closure_defs.
    // So simple recursion should work if main comes first.

    env.analyze(
        "((make-closure r0 C1 () #f 0 #f) "
        "(global-set! fib r0) "
        "(call r0 0) (ret) "
        "(label C1) "
        "(global-ref r1 fib) "
        "(call r1 0) (ret))");

    // Check the call in C1
    const Instruction* recursive_call = env.find_instruction_in_function("C1", Opcode::CALL);
    if (!recursive_call) {
      printf("  Recursive CALL not found\n");
      return false;
    }

    if (recursive_call->closure_label == scm_nil) {
      printf("  Recursive call label is nil (global-ref analysis failed)\n");
      return false;
    }

    return true;
  });

  heap.destroy();
  return some_test_failed ? 1 : 0;
}
