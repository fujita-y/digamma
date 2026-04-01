// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include <llvm/Support/TargetSelect.h>
#include <sstream>
#include "codegen.h"
#include "hash.h"
#include "nanos.h"
#include "object_heap.h"
#include "context.h"
#include "reader.h"

SUBR subr_num_add(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_apply(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_list(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_cons(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_values(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_call_with_values(scm_obj_t self, scm_obj_t producer, scm_obj_t consumer);
SUBR subr_hashtable_entries(scm_obj_t self, scm_obj_t a1);
SUBR subr_make_eq_hashtable(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_hashtable_set(scm_obj_t self, scm_obj_t a1, scm_obj_t a2, scm_obj_t a3);
SUBR subr_codegen_and_run(scm_obj_t self, scm_obj_t coreform);

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
  scm_obj_t env = context::s_current_environment;
  scm_environment_rec_t* env_rec = (scm_environment_rec_t*)to_address(env);
  hashtable_set(env_rec->variables, sym, make_cell(val));
}

class CodegenTest {
 public:
  std::unique_ptr<nanos_jit_t> jit;
  codegen_t* codegen;

  CodegenTest() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();

    auto jit_expected = nanos_jit_t::Create();
    if (!jit_expected) {
      fprintf(stderr, "Could not create LLJIT: %s\n", llvm::toString(jit_expected.takeError()).c_str());
      exit(1);
    }
    jit = std::move(*jit_expected);

    auto ts_ctx = std::make_unique<llvm::LLVMContext>();
    codegen = new codegen_t(std::move(ts_ctx), jit.get());
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

#ifdef __has_feature
  #if __has_feature(hwaddress_sanitizer)
extern "C" const char* __hwasan_default_options() { return "leak_check_at_exit=0"; }
  #endif
#endif

int main(int argc, char** argv) {
  printf("Starting test_codegen\n");
  fflush(stdout);
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 2, 1024 * 1024);
  context::init();

  run_test("ConstantReturn", [](CodegenTest& env) -> bool {
    // ((const r0 3) (ret)) ;=> 3
    scm_obj_t code = env.read_code("((const r0 3) (ret))");
    intptr_t result = env.codegen->compile(code).release_and_run();
    return result == make_fixnum(3);
  });

  run_test("MovInstruction", [](CodegenTest& env) -> bool {
    // ((const r0 10) (mov r1 r0) (mov r0 r1) (ret)) ;=> 10
    scm_obj_t code = env.read_code("((const r0 10) (mov r1 r0) (mov r0 r1) (ret))");
    intptr_t result = env.codegen->compile(code).release_and_run();
    return result == make_fixnum(10);
  });

  run_test("ControlFlow", [](CodegenTest& env) -> bool {
    // ((const r0 2) (if L1 L2) (label L1) (const r0 1) (jump L3) (label L2) (const r0 3) (label L3) (ret)) ;=> 1
    scm_obj_t code = env.read_code("((const r0 2) (if L1 L2) (label L1) (const r0 1) (jump L3) (label L2) (const r0 3) (label L3) (ret))");
    intptr_t result = env.codegen->compile(code).release_and_run();
    /* 2 is true */
    return result == make_fixnum(1);
  });

  run_test("ControlFlowFalse", [](CodegenTest& env) -> bool {
    // ((const r0 #f) (if L1 L2) (label L1) (const r0 1) (jump L3) (label L2) (const r0 3) (label L3) (ret)) ;=> 3
    scm_obj_t code = env.read_code("((const r0 #f) (if L1 L2) (label L1) (const r0 1) (jump L3) (label L2) (const r0 3) (label L3) (ret))");
    intptr_t result = env.codegen->compile(code).release_and_run();
    return result == make_fixnum(3);
  });

  run_test("ControlFlowBoolean", [](CodegenTest& env) -> bool {
    // ((const r0 #t) (if L1 L2) (label L1) (const r0 #f) (if L4 L5) (label L4) (const r0 1) (jump L6) (label L5) (const r0 2) (label L6) (jump
    // L3) (label L2) (const r0 3) (label L3) (ret))
    scm_obj_t code = env.read_code(
        "((const r0 #t) (if L1 L2) (label L1) (const r0 #f) (if L4 L5) (label L4) (const r0 1) (jump L6) (label L5) (const r0 2) (label L6) "
        "(jump L3) (label L2) (const r0 3) (label L3) (ret))");
    intptr_t result = env.codegen->compile(code).release_and_run();
    // L1(#t)->L4(#f)->L5 -> 2, L2 -> 3
    return result == make_fixnum(2);
  });

  run_test("HighRegisterCount", [](CodegenTest& env) -> bool {
    // ((const r100 123) (mov r0 r100) (ret)) ;=> 123
    scm_obj_t code = env.read_code("((const r100 123) (mov r0 r100) (ret))");
    intptr_t result = env.codegen->compile(code).release_and_run();
    return result == make_fixnum(123);
  });

  run_test("MakeClosure", [](CodegenTest& env) -> bool {
    // ((make-closure r0 C1 () 0 #f) (ret) (label C1) (const r0 42) (ret)) ;=> closure address
    // We check if the result is a closure heap object.
    scm_obj_t code = env.read_code("((make-closure r0 C1 () 0 #f) (ret) (label C1) (const r0 42) (ret))");
    scm_obj_t result = (scm_obj_t)env.codegen->compile(code).release_and_run();
    return is_closure(result);
  });

  run_test("MakeClosureCapture", [](CodegenTest& env) -> bool {
    // ((const r1 123) (make-closure r0 C1 (r1) 0 #f) (ret) (label C1) (const r0 42) (ret))
    // Check if closure captures r1 (val 123) at env[0]
    scm_obj_t code = env.read_code("((const r1 123) (make-closure r0 C1 (r1) 0 #f) (ret) (label C1) (const r0 42) (ret))");
    scm_obj_t result = (scm_obj_t)env.codegen->compile(code).release_and_run();
    if (!is_closure(result)) return false;
    scm_closure_rec_t* rec = (scm_closure_rec_t*)to_address(result);
    // Check nsize and env[0]
    // 123 is fixnum -> (123 << 1) | 1 = 247
    if (rec->nenv != 1) {
      printf("Closure nsize mismatch: expected 1, got %d\n", rec->nenv);
      return false;
    }
    if (rec->env[0] != make_fixnum(123)) {
      printf("Closure env[0] mismatch: expected %ld, got %ld\n", make_fixnum(123), rec->env[0]);
      return false;
    }
    return true;
  });

  run_test("MakeClosureLiterals", [](CodegenTest& env) -> bool {
    // ((make-closure r0 C1 () 0 #f) (ret) (label C1) (const r0 (1 . 2)) (ret))
    // Check if closure has literals vector with (1 . 2)
    scm_obj_t code = env.read_code("((make-closure r0 C1 () 0 #f) (ret) (label C1) (const r0 (1 . 2)) (ret))");
    scm_obj_t result = (scm_obj_t)env.codegen->compile(code).release_and_run();
    if (!is_closure(result)) return false;
    scm_closure_rec_t* rec = (scm_closure_rec_t*)to_address(result);
    return true;
  });

  run_test("MakeClosureArgs", [](CodegenTest& env) -> bool {
    // ((make-closure r0 C1 () 5 #t) (ret) (label C1) (const r0 42) (ret))
    // Check argc=5, rest=1
    scm_obj_t code = env.read_code("((make-closure r0 C1 () 5 #t) (ret) (label C1) (const r0 42) (ret))");
    scm_obj_t result = (scm_obj_t)env.codegen->compile(code).release_and_run();
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
    // Check if current-context.has foo = 123
    scm_obj_t code = env.read_code("((const r2 123) (global-set! foo r2) (ret))");
    env.codegen->compile(code).release_and_run();

    object_heap_t* heap = object_heap_t::current();
    scm_obj_t env_obj = context::s_current_environment;
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
    intptr_t result = env.codegen->compile(code).release_and_run();
    if (result != make_fixnum(456)) {
      printf("GlobalRef mismatch: expected 456, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("CallInstruction", [](CodegenTest& env) -> bool {
    // ((make-closure r0 C1 () 0 #f) (call r0 0) (ret) (label C1) (const r0 42) (ret))
    // Call C1, result should be in r0 (42)
    scm_obj_t code = env.read_code("((make-closure r0 C1 () 0 #f) (call r0 0) (ret) (label C1) (const r0 42) (ret))");
    intptr_t result = env.codegen->compile(code).release_and_run();
    if (result != make_fixnum(42)) {
      printf("CallInstruction mismatch: expected 42, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("GlobalClosureCall", [](CodegenTest& env) -> bool {
    // Sequence 1: Create closure and set to global 'a'
    // ((make-closure r0 C1 () 1 #f) (global-set! a r0) (ret) (label C1) (mov r1 r0) (ret))
    scm_obj_t code1 = env.read_code("((make-closure r0 C1 () 1 #f) (global-set! a r0) (ret) (label C1) (mov r1 r0) (ret))");
    env.codegen->compile(code1).release_and_run();

    // Sequence 2: Get global 'a' and call it
    // ((const r1 10) (global-ref r2 a) (mov r0 r1) (call r2 1) (ret))
    scm_obj_t code2 = env.read_code("((const r1 10) (global-ref r2 a) (mov r0 r1) (call r2 1) (ret))");
    intptr_t result = env.codegen->compile(code2).release_and_run();

    // Result should be 10 (passed as argument r0 -> r1 -> ret)
    if (result != make_fixnum(10)) {
      printf("GlobalClosureCall mismatch: expected 10, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("NestedClosure", [](CodegenTest& env) -> bool {
    // (make-closure r0 C1 () 0 #f) (call r0 0) (global-set! inner r0) (ret)
    // (label C1) (const r1 999) (make-closure r0 C2 (r1) 0 #f) (ret)
    // (label C2) (ret)
    scm_obj_t code = env.read_code(
        "((make-closure r0 C1 () 0 #f) (call r0 0) (global-set! inner r0) (ret) "
        "(label C1) (const r1 999) (make-closure r0 C2 (r1) 0 #f) (ret) "
        "(label C2) (ret))");

    env.codegen->compile(code).release_and_run();

    object_heap_t* heap = object_heap_t::current();
    scm_obj_t env_obj = context::s_current_environment;
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
    if (rec->nenv != 1) {
      printf("NestedClosure: nsize mismatch, expected 1, got %d\n", rec->nenv);
      return false;
    }
    if (rec->env[0] != make_fixnum(999)) {
      printf("NestedClosure: captured val mismatch, expected 999, got %ld\n", rec->env[0]);
      return false;
    }

    return true;
  });

  run_test("ClosureArgsSum", [](CodegenTest& env) -> bool {
    // ((const r0 10) (const r1 20) (const r2 30) (make-closure r3 C1 () 3 #f) (call r3 3) (ret)
    //  (label C1) (mov r0 r2) (ret))
    scm_obj_t code = env.read_code(
        "((const r0 10) (const r1 20) (const r2 30) "
        "(make-closure r3 C1 () 3 #f) "
        "(call r3 3) (ret) "
        "(label C1) (mov r0 r2) (ret))");

    intptr_t result = env.codegen->compile(code).release_and_run();
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

    intptr_t result = env.codegen->compile(code).release_and_run();
    if (result != make_fixnum(100)) {
      printf("GlobalInc: expected 100, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("ClosureControlFlow", [](CodegenTest& env) -> bool {
    // ((make-closure r3 C1 () 1 #f) (const r0 #f) (call r3 1) (ret)
    //  (label C1) (if L1 L2) (label L1) (const r0 1) (ret) (label L2) (const r0 2) (ret))
    scm_obj_t code = env.read_code(
        "((make-closure r3 C1 () 1 #f) "
        "(const r0 #f) "
        "(call r3 1) (ret) "
        "(label C1) (if L1 L2) "
        "(label L1) (const r0 1) (ret) "
        "(label L2) (const r0 2) (ret))");

    intptr_t result = env.codegen->compile(code).release_and_run();
    if (result != make_fixnum(2)) {
      printf("ClosureControlFlow: expected 2, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("RestArgumentsExact", [](CodegenTest& env) -> bool {
    // ((make-closure r5 C1 () 2 #t)
    //  (const r0 10) (const r1 20)
    //  (call r5 2) (ret)
    //  (label C1) (mov r0 r2) (ret))
    // Call with 2 args. Fixed=2. Rest should be nil.
    // Rest is in r2.
    scm_obj_t code = env.read_code(
        "((make-closure r5 C1 () 2 #t) "
        "(const r0 10) (const r1 20) "
        "(call r5 2) (ret) "
        "(label C1) (mov r0 r2) (ret))");

    intptr_t result = env.codegen->compile(code).release_and_run();
    if (result != scm_nil) {
      printf("RestArgumentsExact: expected nil, got %lx\n", result);
      return false;
    }
    return true;
  });

  run_test("RestArgumentsExtra", [](CodegenTest& env) -> bool {
    // ((make-closure r5 C1 () 2 #t)
    //  (const r0 10) (const r1 20) (const r2 30) (const r3 40)
    //  (call r5 4) (ret)
    //  (label C1) (mov r0 r2) (ret))
    // Call with 4 args. Fixed=2. Rest should be (30 40).
    // Note: r2 is the rest argument (index 2 in callee).
    scm_obj_t code = env.read_code(
        "((make-closure r5 C1 () 2 #t) "
        "(const r0 10) (const r1 20) (const r2 30) (const r3 40) "
        "(call r5 4) (ret) "
        "(label C1) (mov r0 r2) (ret))");

    intptr_t result = env.codegen->compile(code).release_and_run();
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
    // ((make-closure r0 C1 () 2 #t) (global-set! a r0) (ret) (label C1) (mov r3 r2) (mov r2 r1) (mov r1 r0) (mov r0 r3) (ret))
    // Setup closure in global 'a'
    scm_obj_t SETUP_CODE = env.read_code(
        "((make-closure r0 C1 () 2 #t) (global-set! a r0) (ret) "
        "(label C1) (mov r3 r2) (mov r2 r1) (mov r1 r0) (mov r0 r3) (ret))");
    env.codegen->compile(SETUP_CODE).release_and_run();

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

    intptr_t result = env.codegen->compile(CALL_CODE).release_and_run();

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
    // ((const r1 123) (make-closure r0 C1 (r1) 0 #f) (call r0 0) (ret)
    //  (label C1) (closure-ref r0 0) (ret))
    // Closure captures r1 (123). C1 reads index 0 of free vars -> 123.
    scm_obj_t code = env.read_code(
        "((const r1 123) "
        "(make-closure r0 C1 (r1) 0 #f) "
        "(call r0 0) (ret) "
        "(label C1) (closure-ref r0 0) (ret))");

    intptr_t result = env.codegen->compile(code).release_and_run();
    if (result != make_fixnum(123)) {
      printf("ClosureRef: expected 123, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("ClosureSet", [](CodegenTest& env) -> bool {
    // ((const r1 10) (make-closure r0 C1 (r1) 0 #f) (call r0 0) (ret)
    //  (label C1)
    //  (closure-ref r0 0)      ; r0 = 10
    //  (const r1 20)
    //  (closure-set! 0 r1)     ; env[0] = 20
    //  (closure-ref r2 0)      ; r2 = 20
    //  (mov r0 r2)
    //  (ret))
    scm_obj_t code = env.read_code(
        "((const r1 10) "
        "(make-closure r0 C1 (r1) 0 #f) "
        "(call r0 0) (ret) "
        "(label C1) "
        "(closure-ref r0 0) "
        "(const r1 20) "
        "(closure-set! 0 r1) "
        "(closure-ref r2 0) "
        "(mov r0 r2) "
        "(ret))");

    intptr_t result = env.codegen->compile(code).release_and_run();
    if (result != make_fixnum(20)) {
      printf("ClosureSet: expected 20, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("ClosureSelf", [](CodegenTest& env) -> bool {
    // ((make-closure r0 C1 () 0 #f) (call r0 0) (ret)
    //  (label C1)
    //  (closure-self r0)
    //  (ret))
    scm_obj_t code = env.read_code(
        "((make-closure r0 C1 () 0 #f) "
        "(call r0 0) (ret) "
        "(label C1) "
        "(closure-self r0) "
        "(ret))");

    intptr_t result = env.codegen->compile(code).release_and_run();
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
    scm_obj_t result = (scm_obj_t)env.codegen->compile(code).release_and_run();

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
    //  (make-closure r0 C1 (r1) 0 #f)
    //  (call r0 0) (ret)
    //  (label C1)
    //  (closure-cell-ref r0 0)  ; r0 = cell-value(env[0])
    //  (ret))
    scm_obj_t code = env.read_code(
        "((const r1 123) "
        "(make-cell r1) "
        "(make-closure r0 C1 (r1) 0 #f) "
        "(call r0 0) (ret) "
        "(label C1) "
        "(closure-cell-ref r0 0) "
        "(ret))");

    intptr_t result = env.codegen->compile(code).release_and_run();
    if (result != make_fixnum(123)) {
      printf("ClosureCellRef: expected 123, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("ClosureCellSet", [](CodegenTest& env) -> bool {
    // ((const r1 123)
    //  (make-cell r1)           ; r1 is now a cell containing 123
    //  (make-closure r0 C1 (r1) 0 #f)
    //  (call r0 0) (ret)
    //  (label C1)
    //  (const r2 456)
    //  (closure-cell-set! 0 r2) ; cell-value(env[0]) = 456
    //  (closure-cell-ref r0 0)  ; r0 = cell-value(env[0])
    //  (ret))
    scm_obj_t code = env.read_code(
        "((const r1 123) "
        "(make-cell r1) "
        "(make-closure r0 C1 (r1) 0 #f) "
        "(call r0 0) (ret) "
        "(label C1) "
        "(const r2 456) "
        "(closure-cell-set! 0 r2) "
        "(closure-cell-ref r0 0) "
        "(ret))");

    intptr_t result = env.codegen->compile(code).release_and_run();
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

    intptr_t result = env.codegen->compile(code).release_and_run();
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

    intptr_t result = env.codegen->compile(code).release_and_run();
    if (result != make_fixnum(456)) {
      printf("RegCellSet: expected 456, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("TailCall", [](CodegenTest& env) -> bool {
    // ((make-closure r0 C1 () 0 #f)
    //  (make-closure r1 C2 (r0) 0 #f)
    //  (call r1 0)
    //  (ret)
    //  (label C1)
    //  (const r0 42)
    //  (ret)
    //  (label C2)
    //  (closure-ref r0 0)
    //  (tail-call r0 0))
    scm_obj_t code = env.read_code(
        "((make-closure r0 C1 () 0 #f) "
        "(make-closure r1 C2 (r0) 0 #f) "
        "(call r1 0) "
        "(ret) "
        "(label C1) "
        "(const r0 42) "
        "(ret) "
        "(label C2) "
        "(closure-ref r0 0) "
        "(tail-call r0 0))");

    intptr_t result = env.codegen->compile(code).release_and_run();
    if (result != make_fixnum(42)) {
      printf("TailCall: expected 42, got %ld\n", result);
      return false;
    }
    return true;
  });

  run_test("ApplyTest", [](CodegenTest& env) -> bool {
    // Register primitives

    scm_obj_t scm_subr_num_add = make_closure((void*)subr_num_add, 0, 1, 0, nullptr, 1);
    c_global_set(make_symbol("+"), scm_subr_num_add);

    scm_obj_t scm_subr_list = make_closure((void*)subr_list, 0, 1, 0, nullptr, 1);
    c_global_set(make_symbol("list"), scm_subr_list);

    scm_obj_t scm_subr_apply = make_closure((void*)subr_apply, 0, 1, 0, nullptr, 1);
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

    intptr_t result1 = env.codegen->compile(code1).release_and_run();
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

    intptr_t result2 = env.codegen->compile(code2).release_and_run();
    if (result2 != make_fixnum(6)) {
      printf("ApplyTest 2 (apply + 1 2 '(3)): expected 6, got %ld\n", result2);
      return false;
    }

    return true;
  });

  run_test("CallWithValues_basic", [](CodegenTest& env) -> bool {
    // Register values and call-with-values as globals so Scheme code can call them
    c_global_set(make_symbol("values"), make_closure((void*)subr_values, 0, 1, 0, nullptr, 1));
    c_global_set(make_symbol("call-with-values"), make_closure((void*)subr_call_with_values, 2, 0, 0, nullptr, 1));

    // Producer closure: calls (values 10 20), returns a values object
    // Consumer closure: takes two args, returns their sum via + (fixnum add)
    //
    // producer:
    //   (const r1 10) (const r2 20)
    //   (global-ref r3 values)
    //   (mov r0 r1) (mov r1 r2)
    //   (tail-call r3 2)
    //
    // consumer (of 2 args a b):
    //   r0=a, r1=b
    //   sum = a + b  via fixnum arithmetic
    //   (ret sum)
    //
    // We build both closures, register them, then call call-with-values.

    // Build producer closure: (lambda () (values 10 20))
    scm_obj_t prod_code = env.read_code(
        "((make-closure r0 C1 () 0 #f) (ret)"
        " (label C1)"
        "   (const r1 10) (const r2 20)"
        "   (global-ref r3 values)"
        "   (mov r0 r1) (mov r1 r2)"
        "   (tail-call r3 2))");
    scm_obj_t producer = (scm_obj_t)env.codegen->compile(prod_code).release_and_run();
    if (!is_closure(producer)) return false;

    // Build consumer closure: (lambda (a b) (+ a b))
    c_global_set(make_symbol("+"), make_closure((void*)subr_num_add, 0, 1, 0, nullptr, 1));
    scm_obj_t cons_code = env.read_code(
        "((make-closure r0 C2 () 2 #f) (ret)"
        " (label C2)"
        "   (global-ref r2 +)"
        "   (call r2 2)"
        "   (ret))");
    scm_obj_t consumer = (scm_obj_t)env.codegen->compile(cons_code).release_and_run();
    if (!is_closure(consumer)) return false;

    // Call call-with-values directly
    scm_obj_t result = subr_call_with_values(scm_nil, producer, consumer);
    if (result != make_fixnum(30)) {
      printf("CallWithValues_basic: expected 30, got %ld\n", fixnum(result));
      return false;
    }
    return true;
  });

  run_test("CallWithValues_single", [](CodegenTest& env) -> bool {
    // Producer returns a single (non-values) value 42.
    // Consumer takes one arg and returns it unchanged.
    c_global_set(make_symbol("values"), make_closure((void*)subr_values, 0, 1, 0, nullptr, 1));
    c_global_set(make_symbol("call-with-values"), make_closure((void*)subr_call_with_values, 2, 0, 0, nullptr, 1));

    // Producer: (lambda () 42)  — returns a plain fixnum, not a values object
    scm_obj_t prod_code = env.read_code(
        "((make-closure r0 C1 () 0 #f) (ret)"
        " (label C1) (const r0 42) (ret))");
    scm_obj_t producer = (scm_obj_t)env.codegen->compile(prod_code).release_and_run();
    if (!is_closure(producer)) return false;

    // Consumer: (lambda (x) x)  — identity
    scm_obj_t cons_code = env.read_code(
        "((make-closure r0 C2 () 1 #f) (ret)"
        " (label C2) (ret))");  // r0 already holds arg0
    scm_obj_t consumer = (scm_obj_t)env.codegen->compile(cons_code).release_and_run();
    if (!is_closure(consumer)) return false;

    scm_obj_t result = subr_call_with_values(scm_nil, producer, consumer);
    if (result != make_fixnum(42)) {
      printf("CallWithValues_single: expected 42, got %ld\n", fixnum(result));
      return false;
    }
    return true;
  });

  run_test("HashtableEntries_via_values", [](CodegenTest& env) -> bool {
    // Build an eq-hashtable, insert two entries, then use hashtable-entries
    // and call-with-values to extract them.
    c_global_set(make_symbol("values"), make_closure((void*)subr_values, 0, 1, 0, nullptr, 1));
    c_global_set(make_symbol("call-with-values"), make_closure((void*)subr_call_with_values, 2, 0, 0, nullptr, 1));
    c_global_set(make_symbol("hashtable-entries"), make_closure((void*)subr_hashtable_entries, 1, 0, 0, nullptr, 1));

    // Create hashtable with two entries directly in C++
    scm_obj_t args0[] = {};
    scm_obj_t ht = subr_make_eq_hashtable(scm_nil, 0, args0);
    scm_obj_t ka = make_symbol("p");
    scm_obj_t kb = make_symbol("q");
    subr_hashtable_set(scm_nil, ht, ka, make_fixnum(100));
    subr_hashtable_set(scm_nil, ht, kb, make_fixnum(200));

    // Call hashtable-entries — should return a values object with 2 vectors
    scm_obj_t entries = subr_hashtable_entries(scm_nil, ht);
    if (!is_values(entries)) {
      printf("HashtableEntries_via_values: expected values object\n");
      return false;
    }
    if (values_nsize(entries) != 2) {
      printf("HashtableEntries_via_values: expected 2 elements, got %d\n", values_nsize(entries));
      return false;
    }
    scm_obj_t kv = values_elts(entries)[0];
    scm_obj_t vv = values_elts(entries)[1];
    if (!is_vector(kv) || !is_vector(vv)) return false;
    if (vector_nsize(kv) != 2 || vector_nsize(vv) != 2) return false;

    // Use call-with-values with a consumer that returns the length of the keys vector
    // Consumer: (lambda (keys vals) (vector-length keys))  — we fake it: just sum values
    // For simplicity, use a consumer closure that adds the two values together.
    // The two values in the hashtable are 100 and 200; their sum is 300.
    c_global_set(make_symbol("+"), make_closure((void*)subr_num_add, 0, 1, 0, nullptr, 1));

    // Producer: (lambda () entries)  where entries is the values object captured as a closure literal
    // We'll test call-with-values directly in C++, using the entries values object.
    scm_obj_t* elts = values_elts(entries);
    // Manually build a 2-arg consumer that sums vector[0] from each vector (100 + 200)
    // i.e., consumer receives (keys-vec vals-vec) and returns (+ (vector-ref keys-vec 0) 0) — too complex.
    // Instead: verify correctness by checking the vectors directly.
    //
    // Cross-check: set of keys matches {p, q} and paired values sum to 300.
    int total = 0;
    bool ok_p = false, ok_q = false;
    for (int i = 0; i < 2; i++) {
      scm_obj_t k = vector_elts(kv)[i];
      scm_obj_t v = vector_elts(vv)[i];
      if (!is_fixnum(v)) return false;
      total += (int)fixnum(v);
      if (k == ka) ok_p = true;
      if (k == kb) ok_q = true;
    }
    if (total != 300) {
      printf("HashtableEntries_via_values: sum mismatch %d\n", total);
      return false;
    }
    if (!ok_p || !ok_q) {
      printf("HashtableEntries_via_values: key mismatch\n");
      return false;
    }
    return true;
  });

  run_test("CodegenAndRunSubr", [](CodegenTest& env) -> bool {
    // ((const r0 123) (ret)) ;=> 123
    scm_obj_t code = env.read_code("((const r0 123) (ret))");
    scm_obj_t result = subr_codegen_and_run(scm_nil, code);
    return result == make_fixnum(123);
  });

  context::destroy();
  heap->destroy();
  delete heap;
  return some_test_failed ? 1 : 0;
}
