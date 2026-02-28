// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include <llvm/Support/TargetSelect.h>
#include <sstream>
#include "codegen.h"
#include "hash.h"
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

class ClosureAnalysisTest {
 public:
  std::unique_ptr<nanos_jit_t> jit;
  codegen_t* codegen;

  ClosureAnalysisTest() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();

    auto jit_expected = nanos_jit_t::Create();
    if (!jit_expected) {
      fprintf(stderr, "Could not create LLJIT: %s\n", llvm::toString(jit_expected.takeError()).c_str());
      exit(1);
    }
    jit = std::move(*jit_expected);

    auto ts_ctx = std::make_unique<llvm::LLVMContext>();
    codegen = new codegen_t(llvm::orc::ThreadSafeContext(std::move(ts_ctx)), jit.get());
  }

  ~ClosureAnalysisTest() { delete codegen; }

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

  run_test("GlobalHeapClosure", [](ClosureAnalysisTest& env) -> bool {
    // Manually bind a closure to a global symbol in the heap
    scm_obj_t sym = make_symbol("heap-func");
    scm_obj_t closure = make_closure(nullptr, 2, 0, 0, nullptr, scm_nil, 0);
    object_heap_t::current()->environment_variable_set(sym, closure);

    // Analyze code that references this heap closure
    // ((global-ref r0 heap-func) (call r0 2) (ret))
    env.analyze("((global-ref r0 heap-func) (call r0 2) (ret))");

    const Instruction* call_inst = env.find_first_instruction(Opcode::CALL);
    if (!call_inst) return false;

    if (call_inst->closure_label == scm_nil) {
      printf("  closure_label is nil (expected G_heap-func)\n");
      return false;
    }

    const char* label_name = (const char*)((scm_symbol_rec_t*)to_address(call_inst->closure_label))->name;
    if (strcmp(label_name, "heap-func") != 0) {
      printf("  Expected heap-func, got %s\n", label_name);
      return false;
    }

    return true;
  });

  run_test("GlobalHeapClosureRest", [](ClosureAnalysisTest& env) -> bool {
    // Manually bind a closure with rest arguments to a global symbol in the heap
    scm_obj_t sym = make_symbol("heap-func-rest");
    // make_closure(code, argc, rest, nsize, env, literals)
    // argc=1, rest=1
    scm_obj_t closure = make_closure(nullptr, 1, 1, 0, nullptr, scm_nil, 0);
    object_heap_t::current()->environment_variable_set(sym, closure);

    // Analyze code that references this heap closure
    // ((global-ref r0 heap-func-rest) (call r0 3) (ret))
    // Note: call argument count (3) > fixed argc (1), which is valid for rest
    env.analyze("((global-ref r0 heap-func-rest) (call r0 3) (ret))");

    const Instruction* call_inst = env.find_first_instruction(Opcode::CALL);
    if (!call_inst) return false;

    if (call_inst->closure_label == scm_nil) {
      printf("  closure_label is nil (expected G_heap-func-rest)\n");
      return false;
    }

    const char* label_name = (const char*)((scm_symbol_rec_t*)to_address(call_inst->closure_label))->name;
    if (strcmp(label_name, "heap-func-rest") != 0) {
      printf("  Expected heap-func-rest, got %s\n", label_name);
      return false;
    }

    // Check that we correctly identified it has rest arguments
    if (env.codegen->closure_params.find(call_inst->closure_label) == env.codegen->closure_params.end()) {
      printf("  closure_params not populated for heap-func-rest\n");
      return false;
    }
    auto params = env.codegen->closure_params[call_inst->closure_label];
    if (params.first != 1 || params.second != true) {
      printf("  Expected argc=1, rest=true. Got argc=%d, rest=%d\n", params.first, params.second);
      return false;
    }

    return true;
  });

  run_test("GlobalHeapClosure_WriteRead", [](ClosureAnalysisTest& env) -> bool {
    // ((make-closure r5 C9 () #f 0 #f)
    //  (global-set! foo r5)
    //  (global-ref r3 foo)
    //  (call r3 0)
    //  (ret)
    //  (label C9) (ret))
    env.analyze(
        "((make-closure r5 C9 () #f 0 #f) "
        "(global-set! foo r5) "
        "(global-ref r3 foo) "
        "(call r3 0) (ret) "
        "(label C9) (ret))");

    const Instruction* call_inst = env.find_first_instruction(Opcode::CALL);
    if (!call_inst) return false;

    if (call_inst->closure_label == scm_nil) {
      printf("  closure_label is nil (expected C9)\n");
      return false;
    }

    // In this case, it should be C9 (the original label), not 'foo' (the global name),
    // because logic detected it locally.
    const char* label_name = (const char*)symbol_name(call_inst->closure_label);
    if (strcmp(label_name, "C9") != 0) {
      printf("  Expected C9, got %s\n", label_name);
      return false;
    }

    return true;
  });

  heap.destroy();
  return some_test_failed ? 1 : 0;
}
