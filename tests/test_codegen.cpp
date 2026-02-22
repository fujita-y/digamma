// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include <llvm/Support/TargetSelect.h>
#include <sstream>
#include "codegen.h"
#include "hash.h"
#include "nanos_subr.h"
#include "object_heap.h"
#include "reader.h"

// Helper macros for cons access (copied from codegen.cpp)
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

static bool some_test_failed = false;

static void c_global_set(scm_obj_t sym, scm_obj_t val) {
  object_heap_t* heap = object_heap_t::current();
  scm_obj_t env = heap->m_environment;
  scm_environment_rec_t* env_rec = (scm_environment_rec_t*)to_address(env);
  hashtable_set(env_rec->variables, sym, make_cell(val));
}

class CodegenTest {
 public:
  std::unique_ptr<llvm::orc::LLJIT> jit;
  codegen_t* codegen;

  CodegenTest() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();

    auto jit_expected = llvm::orc::LLJITBuilder().create();
    if (!jit_expected) {
      fprintf(stderr, "Could not create LLJIT: %s\n", llvm::toString(jit_expected.takeError()).c_str());
      exit(1);
    }
    jit = std::move(*jit_expected);

    auto gen = llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(jit->getDataLayout().getGlobalPrefix());
    if (!gen) {
      fprintf(stderr, "Failed to create symbol generator: %s\n", llvm::toString(gen.takeError()).c_str());
      exit(1);
    }
    jit->getMainJITDylib().addGenerator(std::move(*gen));

    auto ts_ctx = std::make_unique<llvm::LLVMContext>();
    codegen = new codegen_t(llvm::orc::ThreadSafeContext(std::move(ts_ctx)), jit.get());
  }

  ~CodegenTest() { delete codegen; }

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
  printf("Running test: %s\n", name);
  fflush(stdout);
  CodegenTest env;
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

int main(int argc, char** argv) {
  printf("Starting test_codegen\n");
  fflush(stdout);
  object_heap_t heap;
  heap.init(1024 * 1024 * 2, 1024 * 1024);

  run_test("ConstantReturn", [](CodegenTest& env) -> bool {
    // ((const r0 3) (ret)) ;=> 3
    scm_obj_t code = env.read_code("((const r0 3) (ret))");
    intptr_t result = env.codegen->compile(code)();
    return result == make_fixnum(3);
  });

  run_test("MovInstruction", [](CodegenTest& env) -> bool {
    // ((const r0 10) (mov r1 r0) (mov r0 r1) (ret)) ;=> 10
    scm_obj_t code = env.read_code("((const r0 10) (mov r1 r0) (mov r0 r1) (ret))");
    intptr_t result = env.codegen->compile(code)();
    return result == make_fixnum(10);
  });

  run_test("ControlFlow", [](CodegenTest& env) -> bool {
    // ((const r0 2) (if L1 L2) (label L1) (const r0 1) (jump L3) (label L2) (const r0 3) (label L3) (ret)) ;=> 1
    scm_obj_t code = env.read_code("((const r0 2) (if L1 L2) (label L1) (const r0 1) (jump L3) (label L2) (const r0 3) (label L3) (ret))");
    intptr_t result = env.codegen->compile(code)();
    /* 2 is true */
    return result == make_fixnum(1);
  });

  run_test("ControlFlowFalse", [](CodegenTest& env) -> bool {
    // ((const r0 #f) (if L1 L2) (label L1) (const r0 1) (jump L3) (label L2) (const r0 3) (label L3) (ret)) ;=> 3
    scm_obj_t code = env.read_code("((const r0 #f) (if L1 L2) (label L1) (const r0 1) (jump L3) (label L2) (const r0 3) (label L3) (ret))");
    intptr_t result = env.codegen->compile(code)();
    return result == make_fixnum(3);
  });

  run_test("ControlFlowBoolean", [](CodegenTest& env) -> bool {
    // ((const r0 #t) (if L1 L2) (label L1) (const r0 #f) (if L4 L5) (label L4) (const r0 1) (jump L6) (label L5) (const r0 2) (label L6) (jump
    // L3) (label L2) (const r0 3) (label L3) (ret))
    scm_obj_t code = env.read_code(
        "((const r0 #t) (if L1 L2) (label L1) (const r0 #f) (if L4 L5) (label L4) (const r0 1) (jump L6) (label L5) (const r0 2) (label L6) "
        "(jump L3) (label L2) (const r0 3) (label L3) (ret))");
    intptr_t result = env.codegen->compile(code)();
    // L1(#t)->L4(#f)->L5 -> 2, L2 -> 3
    return result == make_fixnum(2);
  });

  run_test("HighRegisterCount", [](CodegenTest& env) -> bool {
    // ((const r100 123) (mov r0 r100) (ret)) ;=> 123
    scm_obj_t code = env.read_code("((const r100 123) (mov r0 r100) (ret))");
    intptr_t result = env.codegen->compile(code)();
    return result == make_fixnum(123);
  });

  run_test("MakeClosure", [](CodegenTest& env) -> bool {
    // ((make-closure r0 C1 () #f 0 #f) (ret) (label C1) (const r0 42) (ret)) ;=> closure address
    // We check if the result is a closure heap object.
    scm_obj_t code = env.read_code("((make-closure r0 C1 () #f 0 #f) (ret) (label C1) (const r0 42) (ret))");
    scm_obj_t result = (scm_obj_t)env.codegen->compile(code)();
    return is_closure(result);
  });

  run_test("MakeClosureCapture", [](CodegenTest& env) -> bool {
    // ((const r1 123) (make-closure r0 C1 (r1) #f 0 #f) (ret) (label C1) (const r0 42) (ret))
    // Check if closure captures r1 (val 123) at env[0]
    scm_obj_t code = env.read_code("((const r1 123) (make-closure r0 C1 (r1) #f 0 #f) (ret) (label C1) (const r0 42) (ret))");
    scm_obj_t result = (scm_obj_t)env.codegen->compile(code)();
    if (!is_closure(result)) return false;
    scm_closure_rec_t* rec = (scm_closure_rec_t*)to_address(result);
    // Check nsize and env[0]
    // 123 is fixnum -> (123 << 1) | 1 = 247
    if (rec->nsize != 1) {
      printf("Closure nsize mismatch: expected 1, got %d\n", rec->nsize);
      return false;
    }
    if (rec->env[0] != make_fixnum(123)) {
      printf("Closure env[0] mismatch: expected %ld, got %ld\n", make_fixnum(123), rec->env[0]);
      return false;
    }
    return true;
  });

  run_test("MakeClosureLiterals", [](CodegenTest& env) -> bool {
    // ((make-closure r0 C1 () #f 0 #f) (ret) (label C1) (const r0 (1 . 2)) (ret))
    // Check if closure has literals vector with (1 . 2)
    scm_obj_t code = env.read_code("((make-closure r0 C1 () #f 0 #f) (ret) (label C1) (const r0 (1 . 2)) (ret))");
    scm_obj_t result = (scm_obj_t)env.codegen->compile(code)();
    if (!is_closure(result)) return false;
    scm_closure_rec_t* rec = (scm_closure_rec_t*)to_address(result);
    // Check literals
    scm_obj_t literals = rec->literals;
    if (!is_vector(literals)) {
      printf("Closure literals mismatch: expected vector, got %lx\n", literals);
      return false;
    }
    if (vector_nsize(literals) != 1) {
      printf("Closure literals size mismatch: expected 1, got %d\n", vector_nsize(literals));
      return false;
    }
    scm_obj_t lit = vector_elts(literals)[0];
    if (!is_cons(lit)) {
      printf("Closure literal mismatch: expected cons, got %lx\n", lit);
      return false;
    }
    if (((scm_cons_rec_t*)lit)->car != make_fixnum(1) || ((scm_cons_rec_t*)lit)->cdr != make_fixnum(2)) {
      printf("Closure literal value mismatch: expected (1 . 2)\n");
      return false;
    }
    return true;
  });

  run_test("MakeClosureArgs", [](CodegenTest& env) -> bool {
    // ((make-closure r0 C1 () #f 5 #t) (ret) (label C1) (const r0 42) (ret))
    // Check argc=5, rest=1
    scm_obj_t code = env.read_code("((make-closure r0 C1 () #f 5 #t) (ret) (label C1) (const r0 42) (ret))");
    scm_obj_t result = (scm_obj_t)env.codegen->compile(code)();
    if (!is_closure(result)) return false;
    scm_closure_rec_t* rec = (scm_closure_rec_t*)to_address(result);
    if (rec->argc != 5) {
      printf("Closure argc mismatch: expected 5, got %d\n", rec->argc);
      return false;
    }
    if (rec->rest != 1) {
      printf("Closure rest mismatch: expected 1, got %d\n", rec->rest);
      return false;
    }
    return true;
  });

  run_test("GlobalSet", [](CodegenTest& env) -> bool {
    // ((const r0 123) (global-set! foo r0) (ret))
    // Check if interaction-environment has foo = 123
    scm_obj_t code = env.read_code("((const r2 123) (global-set! foo r2) (ret))");
    env.codegen->compile(code)();

    object_heap_t* heap = object_heap_t::current();
    scm_obj_t env_obj = heap->m_environment;
    scm_environment_rec_t* env_rec = (scm_environment_rec_t*)to_address(env_obj);
    scm_obj_t cell = hashtable_ref(env_rec->variables, make_symbol("foo"), scm_undef);
    scm_obj_t val = scm_undef;
    if (is_cell(cell)) {
      val = cell_value(cell);
    } else {
      printf("GlobalSet mismatch: expected cell, got %lx\n", cell);
      return false;
    }
    if (val != make_fixnum(123)) {
      printf("GlobalSet mismatch: expected 123, got %ld\n", val);
      return false;
    }
    return true;
  });

  run_test("GlobalRef", [](CodegenTest& env) -> bool {
    // ((const r0 456) (global-set! bar r0) (global-ref r1 bar) (ret))
    // Check if r1 has 456
    scm_obj_t code = env.read_code("((const r0 100) (const r1 456) (global-set! bar r1) (global-ref r0 bar) (ret))");
    intptr_t result = env.codegen->compile(code)();
    if (result != make_fixnum(456)) {
      printf("GlobalRef mismatch: expected 456, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("CallInstruction", [](CodegenTest& env) -> bool {
    // ((make-closure r0 C1 () #f 0 #f) (call r0 0) (ret) (label C1) (const r0 42) (ret))
    // Call C1, result should be in r0 (42)
    scm_obj_t code = env.read_code("((make-closure r0 C1 () #f 0 #f) (call r0 0) (ret) (label C1) (const r0 42) (ret))");
    intptr_t result = env.codegen->compile(code)();
    if (result != make_fixnum(42)) {
      printf("CallInstruction mismatch: expected 42, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("GlobalClosureCall", [](CodegenTest& env) -> bool {
    // Sequence 1: Create closure and set to global 'a'
    // ((make-closure r0 C1 () #f 1 #f) (global-set! a r0) (ret) (label C1) (mov r1 r0) (ret))
    scm_obj_t code1 = env.read_code("((make-closure r0 C1 () #f 1 #f) (global-set! a r0) (ret) (label C1) (mov r1 r0) (ret))");
    env.codegen->compile(code1)();

    // Sequence 2: Get global 'a' and call it
    // ((const r1 10) (global-ref r2 a) (mov r0 r1) (call r2 1) (ret))
    scm_obj_t code2 = env.read_code("((const r1 10) (global-ref r2 a) (mov r0 r1) (call r2 1) (ret))");
    intptr_t result = env.codegen->compile(code2)();

    // Result should be 10 (passed as argument r0 -> r1 -> ret)
    if (result != make_fixnum(10)) {
      printf("GlobalClosureCall mismatch: expected 10, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("NestedClosure", [](CodegenTest& env) -> bool {
    // (make-closure r0 C1 () #f 0 #f) (call r0 0) (global-set! inner r0) (ret)
    // (label C1) (const r1 999) (make-closure r0 C2 (r1) #f 0 #f) (ret)
    // (label C2) (ret)
    scm_obj_t code = env.read_code(
        "((make-closure r0 C1 () #f 0 #f) (call r0 0) (global-set! inner r0) (ret) "
        "(label C1) (const r1 999) (make-closure r0 C2 (r1) #f 0 #f) (ret) "
        "(label C2) (ret))");

    env.codegen->compile(code)();

    object_heap_t* heap = object_heap_t::current();
    scm_obj_t env_obj = heap->m_environment;
    scm_environment_rec_t* env_rec = (scm_environment_rec_t*)to_address(env_obj);
    scm_obj_t cell = hashtable_ref(env_rec->variables, make_symbol("inner"), scm_undef);
    scm_obj_t val = scm_undef;
    if (is_cell(cell)) {
      val = cell_value(cell);
    } else {
      printf("NestedClosure: expected cell, got %lx\n", cell);
      return false;
    }
    if (!is_closure(val)) {
      printf("NestedClosure: expected closure, got %lx\n", val);
      return false;
    }

    scm_closure_rec_t* rec = (scm_closure_rec_t*)to_address(val);
    if (rec->nsize != 1) {
      printf("NestedClosure: nsize mismatch, expected 1, got %d\n", rec->nsize);
      return false;
    }
    if (rec->env[0] != make_fixnum(999)) {
      printf("NestedClosure: captured val mismatch, expected 999, got %ld\n", rec->env[0]);
      return false;
    }

    return true;
  });

  run_test("ClosureArgsSum", [](CodegenTest& env) -> bool {
    // ((const r0 10) (const r1 20) (const r2 30) (make-closure r3 C1 () #f 3 #f) (call r3 3) (ret)
    //  (label C1) (mov r0 r2) (ret))
    scm_obj_t code = env.read_code(
        "((const r0 10) (const r1 20) (const r2 30) "
        "(make-closure r3 C1 () #f 3 #f) "
        "(call r3 3) (ret) "
        "(label C1) (mov r0 r2) (ret))");

    intptr_t result = env.codegen->compile(code)();
    if (result != make_fixnum(30)) {
      printf("ClosureArgsSum: expected 30, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("GlobalInc", [](CodegenTest& env) -> bool {
    // ((const r0 100) (global-set! cnt r0) (global-ref r1 cnt) (mov r0 r1) (ret))
    scm_obj_t code = env.read_code(
        "((const r0 100) (global-set! cnt r0) "
        "(global-ref r1 cnt) (mov r0 r1) (ret))");

    intptr_t result = env.codegen->compile(code)();
    if (result != make_fixnum(100)) {
      printf("GlobalInc: expected 100, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("ClosureControlFlow", [](CodegenTest& env) -> bool {
    // ((make-closure r3 C1 () #f 1 #f) (const r0 #f) (call r3 1) (ret)
    //  (label C1) (if L1 L2) (label L1) (const r0 1) (ret) (label L2) (const r0 2) (ret))
    scm_obj_t code = env.read_code(
        "((make-closure r3 C1 () #f 1 #f) "
        "(const r0 #f) "
        "(call r3 1) (ret) "
        "(label C1) (if L1 L2) "
        "(label L1) (const r0 1) (ret) "
        "(label L2) (const r0 2) (ret))");

    intptr_t result = env.codegen->compile(code)();
    if (result != make_fixnum(2)) {
      printf("ClosureControlFlow: expected 2, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("RestArgumentsExact", [](CodegenTest& env) -> bool {
    // ((make-closure r5 C1 () #f 2 #t)
    //  (const r0 10) (const r1 20)
    //  (call r5 2) (ret)
    //  (label C1) (mov r0 r2) (ret))
    // Call with 2 args. Fixed=2. Rest should be nil.
    // Rest is in r2.
    scm_obj_t code = env.read_code(
        "((make-closure r5 C1 () #f 2 #t) "
        "(const r0 10) (const r1 20) "
        "(call r5 2) (ret) "
        "(label C1) (mov r0 r2) (ret))");

    intptr_t result = env.codegen->compile(code)();
    if (result != scm_nil) {
      printf("RestArgumentsExact: expected nil, got %lx\n", result);
      return false;
    }
    return true;
  });

  run_test("RestArgumentsExtra", [](CodegenTest& env) -> bool {
    // ((make-closure r5 C1 () #f 2 #t)
    //  (const r0 10) (const r1 20) (const r2 30) (const r3 40)
    //  (call r5 4) (ret)
    //  (label C1) (mov r0 r2) (ret))
    // Call with 4 args. Fixed=2. Rest should be (30 40).
    // Note: r2 is the rest argument (index 2 in callee).
    scm_obj_t code = env.read_code(
        "((make-closure r5 C1 () #f 2 #t) "
        "(const r0 10) (const r1 20) (const r2 30) (const r3 40) "
        "(call r5 4) (ret) "
        "(label C1) (mov r0 r2) (ret))");

    intptr_t result = env.codegen->compile(code)();
    // Check if result is list (30 40)
    if (!is_cons(result)) {
      printf("RestArgumentsExtra: expected cons, got %lx\n", result);
      return false;
    }
    if (CAR(result) != make_fixnum(30)) {
      printf("RestArgumentsExtra: car mismatch, expected 30, got %lx\n", CAR(result));
      return false;
    }
    scm_obj_t cdr = CDR(result);
    if (!is_cons(cdr)) {
      printf("RestArgumentsExtra: cdr expected cons, got %lx\n", cdr);
      return false;
    }
    if (CAR(cdr) != make_fixnum(40)) {
      printf("RestArgumentsExtra: cadr mismatch, expected 40, got %lx\n", CAR(cdr));
      return false;
    }
    if (CDR(cdr) != scm_nil) {
      printf("RestArgumentsExtra: cddr mismatch, expected nil, got %lx\n", CDR(cdr));
      return false;
    }
    return true;
  });

  run_test("GlobalClosureRest", [](CodegenTest& env) -> bool {
    // ((make-closure r0 C1 () #f 2 #t) (global-set! a r0) (ret) (label C1) (mov r3 r2) (mov r2 r1) (mov r1 r0) (mov r0 r3) (ret))
    // Setup closure in global 'a'
    scm_obj_t SETUP_CODE = env.read_code(
        "((make-closure r0 C1 () #f 2 #t) (global-set! a r0) (ret) "
        "(label C1) (mov r3 r2) (mov r2 r1) (mov r1 r0) (mov r0 r3) (ret))");
    env.codegen->compile(SETUP_CODE)();

    // ((const r5 1) (const r6 2) (const r7 3) (const r8 4) (const r9 5)
    //  (global-ref r10 a)
    //  (mov r0 r5) (mov r1 r6) (mov r2 r7) (mov r3 r8) (mov r4 r9)
    //  (call r10 5) (ret))
    // Call 'a' with 5 args: 1 2 3 4 5
    // Fixed=2.
    // args: r0=1, r1=2.
    // rest in r2: (3 4 5)
    // Body:
    // r3 <- r2 (rest)
    // r2 <- r1 (2)
    // r1 <- r0 (1)
    // r0 <- r3 (rest)
    // ret r0 -> returns rest list (3 4 5)
    scm_obj_t CALL_CODE = env.read_code(
        "((const r5 1) (const r6 2) (const r7 3) (const r8 4) (const r9 5) "
        "(global-ref r10 a) "
        "(mov r0 r5) (mov r1 r6) (mov r2 r7) (mov r3 r8) (mov r4 r9) "
        "(call r10 5) (ret))");

    intptr_t result = env.codegen->compile(CALL_CODE)();

    // Check if result is (3 4 5)
    if (!is_cons(result)) {
      printf("GlobalClosureRest: expected cons, got %lx\n", result);
      return false;
    }
    if (CAR(result) != make_fixnum(3)) return false;
    scm_obj_t cdr1 = CDR(result);
    if (!is_cons(cdr1)) return false;
    if (CAR(cdr1) != make_fixnum(4)) return false;
    scm_obj_t cdr2 = CDR(cdr1);
    if (!is_cons(cdr2)) return false;
    if (CAR(cdr2) != make_fixnum(5)) return false;
    if (CDR(cdr2) != scm_nil) return false;

    return true;
  });

  run_test("ClosureRef", [](CodegenTest& env) -> bool {
    // ((const r1 123) (make-closure r0 C1 (r1) #f 0 #f) (call r0 0) (ret)
    //  (label C1) (closure-ref r0 0) (ret))
    // Closure captures r1 (123). C1 reads index 0 of free vars -> 123.
    scm_obj_t code = env.read_code(
        "((const r1 123) "
        "(make-closure r0 C1 (r1) #f 0 #f) "
        "(call r0 0) (ret) "
        "(label C1) (closure-ref r0 0) (ret))");

    intptr_t result = env.codegen->compile(code)();
    if (result != make_fixnum(123)) {
      printf("ClosureRef: expected 123, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("ClosureSet", [](CodegenTest& env) -> bool {
    // ((const r1 10) (make-closure r0 C1 (r1) #f 0 #f) (call r0 0) (ret)
    //  (label C1)
    //  (closure-ref r0 0)      ; r0 = 10
    //  (const r1 20)
    //  (closure-set! 0 r1)     ; env[0] = 20
    //  (closure-ref r2 0)      ; r2 = 20
    //  (mov r0 r2)
    //  (ret))
    scm_obj_t code = env.read_code(
        "((const r1 10) "
        "(make-closure r0 C1 (r1) #f 0 #f) "
        "(call r0 0) (ret) "
        "(label C1) "
        "(closure-ref r0 0) "
        "(const r1 20) "
        "(closure-set! 0 r1) "
        "(closure-ref r2 0) "
        "(mov r0 r2) "
        "(ret))");

    intptr_t result = env.codegen->compile(code)();
    if (result != make_fixnum(20)) {
      printf("ClosureSet: expected 20, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("ClosureSelf", [](CodegenTest& env) -> bool {
    // ((make-closure r0 C1 () #f 0 #f) (call r0 0) (ret)
    //  (label C1)
    //  (closure-self r0)
    //  (ret))
    scm_obj_t code = env.read_code(
        "((make-closure r0 C1 () #f 0 #f) "
        "(call r0 0) (ret) "
        "(label C1) "
        "(closure-self r0) "
        "(ret))");

    intptr_t result = env.codegen->compile(code)();
    if (!is_closure((scm_obj_t)result)) {
      printf("ClosureSelf: expected closure, got %lx\n", result);
      return false;
    }
    return true;
  });

  run_test("MakeCell", [](CodegenTest& env) -> bool {
    // ((const r0 123) (make-cell r0) (ret))
    // r0 should contain a cell pointing to 123
    scm_obj_t code = env.read_code("((const r0 123) (make-cell r0) (ret))");
    scm_obj_t result = (scm_obj_t)env.codegen->compile(code)();

    if (!is_cell(result)) {
      printf("MakeCell: expected cell, got %lx\n", result);
      return false;
    }
    scm_obj_t val = cell_value(result);
    if (val != make_fixnum(123)) {
      printf("MakeCell: expected 123 inside cell, got %ld\n", val);
      return false;
    }
    return true;
  });

  run_test("ClosureCellRef", [](CodegenTest& env) -> bool {
    // ((const r1 123)
    //  (make-cell r1)           ; r1 is now a cell containing 123
    //  (make-closure r0 C1 (r1) #f 0 #f)
    //  (call r0 0) (ret)
    //  (label C1)
    //  (closure-cell-ref r0 0)  ; r0 = cell-value(env[0])
    //  (ret))
    scm_obj_t code = env.read_code(
        "((const r1 123) "
        "(make-cell r1) "
        "(make-closure r0 C1 (r1) #f 0 #f) "
        "(call r0 0) (ret) "
        "(label C1) "
        "(closure-cell-ref r0 0) "
        "(ret))");

    intptr_t result = env.codegen->compile(code)();
    if (result != make_fixnum(123)) {
      printf("ClosureCellRef: expected 123, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("ClosureCellSet", [](CodegenTest& env) -> bool {
    // ((const r1 123)
    //  (make-cell r1)           ; r1 is now a cell containing 123
    //  (make-closure r0 C1 (r1) #f 0 #f)
    //  (call r0 0) (ret)
    //  (label C1)
    //  (const r2 456)
    //  (closure-cell-set! 0 r2) ; cell-value(env[0]) = 456
    //  (closure-cell-ref r0 0)  ; r0 = cell-value(env[0])
    //  (ret))
    scm_obj_t code = env.read_code(
        "((const r1 123) "
        "(make-cell r1) "
        "(make-closure r0 C1 (r1) #f 0 #f) "
        "(call r0 0) (ret) "
        "(label C1) "
        "(const r2 456) "
        "(closure-cell-set! 0 r2) "
        "(closure-cell-ref r0 0) "
        "(ret))");

    intptr_t result = env.codegen->compile(code)();
    if (result != make_fixnum(456)) {
      printf("ClosureCellSet: expected 456, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("RegCellRef", [](CodegenTest& env) -> bool {
    // ((const r1 123)
    //  (make-cell r1)      ; r1 is now a cell containing 123
    //  (reg-cell-ref r0 r1) ; r0 = cell-value(r1)
    //  (ret))
    scm_obj_t code = env.read_code(
        "((const r1 123) "
        "(make-cell r1) "
        "(reg-cell-ref r0 r1) "
        "(ret))");

    intptr_t result = env.codegen->compile(code)();
    if (result != make_fixnum(123)) {
      printf("RegCellRef: expected 123, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("RegCellSet", [](CodegenTest& env) -> bool {
    // ((const r1 123)
    //  (const r2 456)
    //  (make-cell r1)      ; r1 is now a cell containing 123
    //  (reg-cell-set! r1 r2) ; cell-value(r1) = 456
    //  (reg-cell-ref r0 r1) ; r0 = cell-value(r1)
    //  (ret))
    scm_obj_t code = env.read_code(
        "((const r1 123) "
        "(const r2 456) "
        "(make-cell r1) "
        "(reg-cell-set! r1 r2) "
        "(reg-cell-ref r0 r1) "
        "(ret))");

    intptr_t result = env.codegen->compile(code)();
    if (result != make_fixnum(456)) {
      printf("RegCellSet: expected 456, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("TailCall", [](CodegenTest& env) -> bool {
    // ((make-closure r0 C1 () #f 0 #f)
    //  (make-closure r1 C2 (r0) #f 0 #f)
    //  (call r1 0)
    //  (ret)
    //  (label C1)
    //  (const r0 42)
    //  (ret)
    //  (label C2)
    //  (closure-ref r0 0)
    //  (tail-call r0 0))
    scm_obj_t code = env.read_code(
        "((make-closure r0 C1 () #f 0 #f) "
        "(make-closure r1 C2 (r0) #f 0 #f) "
        "(call r1 0) "
        "(ret) "
        "(label C1) "
        "(const r0 42) "
        "(ret) "
        "(label C2) "
        "(closure-ref r0 0) "
        "(tail-call r0 0))");

    intptr_t result = env.codegen->compile(code)();
    if (result != make_fixnum(42)) {
      printf("TailCall: expected 42, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("ApplyTest", [](CodegenTest& env) -> bool {
    // Register primitives

    scm_obj_t scm_subr_num_add = make_closure((void*)subr_num_add, 0, 1, 0, nullptr, scm_nil, 1);
    c_global_set(make_symbol("+"), scm_subr_num_add);

    scm_obj_t scm_subr_list = make_closure((void*)subr_list, 0, 1, 0, nullptr, scm_nil, 1);
    c_global_set(make_symbol("list"), scm_subr_list);

    scm_obj_t scm_subr_apply = make_closure((void*)subr_apply, 0, 1, 0, nullptr, scm_nil, 1);
    c_global_set(make_symbol("apply"), scm_subr_apply);

    // Test: (apply + '(1 2))
    scm_obj_t code1 = env.read_code(
        "((const r10 (1 2)) "
        "(global-ref r11 +) "
        "(global-ref r12 apply) "
        "(mov r0 r11) "
        "(mov r1 r10) "
        "(call r12 2) "
        "(ret))");

    intptr_t result1 = env.codegen->compile(code1)();
    if (result1 != make_fixnum(3)) {
      printf("ApplyTest 1 (apply + '(1 2)): expected 3, got %ld\n", result1);
      return false;
    }

    // Test: (apply + 1 2 '(3)) => 6
    scm_obj_t code2 = env.read_code(
        "((const r10 (3)) "
        "(const r11 1) "
        "(const r12 2) "
        "(global-ref r13 +) "
        "(global-ref r14 apply) "
        "(mov r0 r13) "
        "(mov r1 r11) "
        "(mov r2 r12) "
        "(mov r3 r10) "
        "(call r14 4) "
        "(ret))");

    intptr_t result2 = env.codegen->compile(code2)();
    if (result2 != make_fixnum(6)) {
      printf("ApplyTest 2 (apply + 1 2 '(3)): expected 6, got %ld\n", result2);
      return false;
    }

    return true;
  });

  heap.destroy();
  return some_test_failed ? 1 : 0;
}
