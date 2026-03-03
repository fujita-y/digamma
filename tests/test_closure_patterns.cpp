// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include <llvm/Support/TargetSelect.h>
#include <sstream>
#include "codegen.h"
#include "hash.h"
#include "nanos.h"
#include "object_heap.h"
#include "reader.h"

SUBR subr_num_add(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_apply(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_list(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_cons(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);

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

int main(int argc, char** argv) {
  printf("Starting test_closure_patterns\n");
  fflush(stdout);
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 2, 1024 * 1024);

  // 1. Direct Call (Optimization)
  run_test("DirectCallLocal", [](CodegenTest& env) -> bool {
    // ((make-closure r0 C1 () #f 0 #f) (call r0 0) (ret) (label C1) (const r0 100) (ret))
    scm_obj_t code = env.read_code(
        "((make-closure r0 C1 () #f 0 #f) "
        "(call r0 0) (ret) "
        "(label C1) (const r0 100) (ret))");
    intptr_t result = env.codegen->compile(code)();
    return result == make_fixnum(100);
  });

  // 2. Global Direct Call (Optimization)
  run_test("DirectCallGlobal", [](CodegenTest& env) -> bool {
    scm_obj_t setup = env.read_code(
        "((make-closure r0 C1 () #f 0 #f) (global-set! f r0) (ret) "
        "(label C1) (const r0 200) (ret))");
    env.codegen->compile(setup)();

    scm_obj_t code = env.read_code("((global-ref r0 f) (call r0 0) (ret))");
    intptr_t result = env.codegen->compile(code)();
    return result == make_fixnum(200);
  });

  // 3. Generic Call - Unknown Closure (Bridge)
  run_test("GenericCallBridge", [](CodegenTest& env) -> bool {
    // Define Global +
    scm_obj_t scm_subr_num_add = make_closure((void*)subr_num_add, 0, 1, 0, nullptr, scm_nil, 1);
    c_global_set(make_symbol("+"), scm_subr_num_add);

    // Adder: (lambda (n) (+ n 10))
    // C1 args: r0 (n).
    // Body: (const r1 10) (global-ref r2 +) (call r2 2)
    // call r2 2: args r0, r1. r0=n, r1=10.
    scm_obj_t setup = env.read_code(
        "((make-closure r0 C1 () #f 1 #f) (global-set! adder r0) (ret) "
        "(label C1) "
        "(const r1 10) "
        "(global-ref r2 +) "
        "(call r2 2) (ret))");  // returns output of +
    env.codegen->compile(setup)();

    // apply-it: (lambda (f arg) (f arg))
    // C2 args: r0 (f), r1 (arg).
    // Body: (call r0 1).
    // call r0 1: args r0. r0=f.
    // Wait, (f arg) -> call f with 1 arg. Arg is 'arg'.
    // call rX N takes args r0...r(N-1).
    // So for (call f 1), we need r0 to be 'arg'.
    // But currently r0 is 'f'.
    // So we need to swap or move.
    // (mov r2 r0) ; r2 = f
    // (mov r0 r1) ; r0 = arg
    // (call r2 1) ; call f using r0 as arg
    scm_obj_t apply_it = env.read_code(
        "((make-closure r0 C2 () #f 2 #f) (global-set! apply-it r0) (ret) "
        "(label C2) "
        "(mov r2 r0) "
        "(mov r0 r1) "
        "(call r2 1) (ret))");
    env.codegen->compile(apply_it)();

    // Call: (apply-it adder 5)
    // r0 = apply-it
    // r1 = adder
    // r2 = 5
    // But call apply-it (2 args) needs args in r0, r1.
    // r0 = adder, r1 = 5.
    // closure in r3.
    scm_obj_t call_it = env.read_code(
        "((global-ref r0 adder) "
        "(const r1 5) "
        "(global-ref r3 apply-it) "
        "(call r3 2) (ret))");

    intptr_t result = env.codegen->compile(call_it)();
    return result == make_fixnum(15);
  });

  // 4. Rest Arguments - Generic Call via Bridge
  run_test("GenericCallRest", [](CodegenTest& env) -> bool {
    // list-it: (lambda (. args) args)
    // Fixed=0, Rest=True.
    // C1 args: r0 is list of args.
    // Body: return r0.
    scm_obj_t setup = env.read_code(
        "((make-closure r0 C1 () #f 0 #t) (global-set! list-it r0) (ret) "
        "(label C1) (ret))");
    env.codegen->compile(setup)();

    // apply-it-2: (lambda (f arg1 arg2) (f arg1 arg2))
    // C2 args: r0=f, r1=arg1, r2=arg2
    // Call f with 2 args.
    // Need r0=arg1, r1=arg2.
    // mov r3 r0 ; f
    // mov r0 r1 ; arg1
    // mov r1 r2 ; arg2
    // call r3 2
    scm_obj_t apply_it = env.read_code(
        "((make-closure r0 C2 () #f 3 #f) (global-set! apply-it-2 r0) (ret) "
        "(label C2) "
        "(mov r3 r0) "
        "(mov r0 r1) "
        "(mov r1 r2) "
        "(call r3 2) (ret))");
    env.codegen->compile(apply_it)();

    // Call: (apply-it-2 list-it 42 99)
    // Args: r0=list-it, r1=42, r2=99.
    // closure r10=apply-it-2
    scm_obj_t call_it = env.read_code(
        "((global-ref r0 list-it) "
        "(const r1 42) "
        "(const r2 99) "
        "(global-ref r10 apply-it-2) "
        "(call r10 3) (ret))");

    intptr_t result = env.codegen->compile(call_it)();
    // Expect (42 99)
    if (!is_cons(result)) return false;
    if (CAR(result) != make_fixnum(42)) return false;
    if (CAR(CDR(result)) != make_fixnum(99)) return false;
    return true;
  });

  // 5. Large Argument Count (Bridge Fallback > 10)
  run_test("LargeArgsFallback", [](CodegenTest& env) -> bool {
    // Function taking 12 arguments
    // CBig args: r0...r11.
    // Return r11.
    scm_obj_t setup = env.read_code(
        "((make-closure r0 CBig () #f 12 #f) (global-set! big-f r0) (ret) "
        "(label CBig) "
        "(mov r0 r11) (ret))");
    env.codegen->compile(setup)();

    // Caller
    // Prepare args r0...r11.
    // Closure in r20.
    // (global-ref r20 big-f)
    // (call r20 12)
    scm_obj_t code = env.read_code(
        "((const r0 0) (const r1 1) (const r2 2) (const r3 3) (const r4 4) "
        "(const r5 5) (const r6 6) (const r7 7) (const r8 8) (const r9 9) "
        "(const r10 10) (const r11 11) "
        "(global-ref r20 big-f) "
        "(call r20 12) (ret))");

    intptr_t result = env.codegen->compile(code)();
    return result == make_fixnum(11);
  });

  // 6. Apply with Closure
  run_test("ApplyClosure", [](CodegenTest& env) -> bool {
    scm_obj_t scm_subr_apply = make_closure((void*)subr_apply, 0, 1, 0, nullptr, scm_nil, 1);
    c_global_set(make_symbol("apply"), scm_subr_apply);

    scm_obj_t scm_subr_cons = make_closure((void*)subr_cons, 2, 0, 0, nullptr, scm_nil, 1);
    c_global_set(make_symbol("cons"), scm_subr_cons);

    // my-cons: (lambda (a b) (cons a b))
    // C1: r0=a, r1=b.
    // Call cons(a, b).
    // r2=cons.
    // Need r0=a, r1=b. Already there.
    // call r2 2.
    scm_obj_t setup = env.read_code(
        "((make-closure r0 C1 () #f 2 #f) (global-set! my-cons r0) (ret) "
        "(label C1) "
        "(global-ref r2 cons) "
        "(call r2 2) (ret))");
    env.codegen->compile(setup)();

    // (apply my-cons '(1 2))
    // args: r0=my-cons, r1='(1 2).
    // closure r10=apply.
    scm_obj_t code = env.read_code(
        "((global-ref r0 my-cons) (const r1 (1 2)) "
        "(global-ref r10 apply) "
        "(call r10 2) (ret))");

    intptr_t result = env.codegen->compile(code)();
    if (!is_cons(result)) return false;
    if (CAR(result) != make_fixnum(1)) return false;
    if (CDR(result) != make_fixnum(2)) return false;
    return true;
  });

  heap->destroy();
  delete heap;
  return some_test_failed ? 1 : 0;
}
