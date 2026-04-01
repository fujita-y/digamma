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

SUBR subr_apply(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_num_add(scm_obj_t self, int argc, scm_obj_t argv[]);
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

int main(int argc, char** argv) {
  printf("Starting test_apply_patterns\n");
  fflush(stdout);
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 2, 1024 * 1024);
  context::init();

  // Define primitives
  scm_obj_t scm_subr_apply = make_closure((void*)subr_apply, 0, 1, 0, nullptr, 1);
  c_global_set(make_symbol("apply"), scm_subr_apply);

  scm_obj_t scm_subr_num_add = make_closure((void*)subr_num_add, 0, 1, 0, nullptr, 1);
  c_global_set(make_symbol("+"), scm_subr_num_add);

  scm_obj_t scm_subr_list = make_closure((void*)subr_list, 0, 1, 0, nullptr, 1);
  c_global_set(make_symbol("list"), scm_subr_list);

  scm_obj_t scm_subr_cons = make_closure((void*)subr_cons, 2, 0, 0, nullptr, 1);
  c_global_set(make_symbol("cons"), scm_subr_cons);

  // 1. Tail Apply
  run_test("TailApply", [](CodegenTest& env) -> bool {
    // (lambda (f args) (apply f args)) -> tail call
    // (apply + '(1 2))
    scm_obj_t code = env.read_code(
        "((const r0 (1 2)) "
        "(global-ref r1 +) "
        "(global-ref r2 apply) "
        "(tail-call r2 2) (ret))");  // apply takes (f . args) ? No apply(proc, args). 2 args here.
    // Wait, apply in nanos is (apply proc arg1 ... args-list).
    // So (apply + '(1 2)) -> 2 arguments: + and '(1 2).
    // (call r2 2) means call closure in r2 with 2 arguments: r0 and r1.
    // Need r0=+, r1='(1 2).
    // Registers: r0='(1 2), r1=+, r2=apply.
    // Move r2->r3 (apply).
    // r0=+, r1='(1 2).
    scm_obj_t correct_code = env.read_code(
        "((const r0 (1 2)) "
        "(global-ref r1 +) "
        "(global-ref r2 apply) "
        // Swap to get correct args
        "(mov r3 r2) "  // r3 = apply
        "(mov r2 r0) "  // r2 = '(1 2)
        "(mov r0 r1) "  // r0 = +
        "(mov r1 r2) "  // r1 = '(1 2)
        // call apply with 2 args: +, '(1 2)
        "(tail-call r3 2))");

    intptr_t result = env.codegen->compile(correct_code).release_and_run();
    return result == make_fixnum(3);
  });

  // 2. Non-Tail Apply
  run_test("NonTailApply", [](CodegenTest& env) -> bool {
    // (apply + '(1 2)) then add 1 -> (+ (apply ...) 1)
    // Non-tail call to apply.
    scm_obj_t code = env.read_code(
        "((const r0 (1 2)) "
        "(global-ref r1 +) "
        "(global-ref r2 apply) "
        // Setup args
        "(mov r3 r2) "  // r3 = apply
        "(mov r2 r0) "  // r2 = '(1 2)
        "(mov r0 r1) "  // r0 = +
        "(mov r1 r2) "  // r1 = '(1 2)
        "(call r3 2) "  // Result in r0
        // Add 1
        "(const r1 1) "
        "(global-ref r2 +) "
        "(call r2 2) (ret))");

    intptr_t result = env.codegen->compile(code).release_and_run();
    return result == make_fixnum(4);
  });

  // 3. Runtime List
  run_test("RuntimeListApply", [](CodegenTest& env) -> bool {
    // (apply + (cons 1 (cons 2 '())))
    // r0 = 1
    // r1 = 2
    // r2 = '()
    // r3 = cons
    // ... construct list in r0
    // apply + r0
    scm_obj_t code = env.read_code(
        "((const r0 1) (const r1 2) (const r2 ()) "
        "(global-ref r3 cons) "
        // (cons 2 '()) -> r4
        "(mov r10 r3) "  // cons
        // r0=2, r1='()
        "(mov r5 r0) "   // save 1
        "(mov r0 r1) "   // 2
        "(mov r1 r2) "   // '()
        "(call r10 2) "  // r0 = (2)
        "(mov r4 r0) "
        // (cons 1 r4) -> r0
        "(mov r1 r4) "
        "(mov r0 r5) "   // 1
        "(call r10 2) "  // r0 = (1 2)
        // apply + r0
        "(mov r1 r0) "        // list
        "(global-ref r0 +) "  // proc
        "(global-ref r10 apply) "
        "(call r10 2) (ret))");

    intptr_t result = env.codegen->compile(code).release_and_run();
    return result == make_fixnum(3);
  });

  // 4. Complex Args (Fixed + List)
  run_test("ComplexArgsApply", [](CodegenTest& env) -> bool {
    // (apply + 1 2 '(3 4)) => 10
    // apply takes: +, 1, 2, '(3 4). Total 4 args to apply.
    scm_obj_t code = env.read_code(
        "((const r3 (3 4)) "
        "(const r1 1) "
        "(const r2 2) "
        "(global-ref r0 +) "
        "(global-ref r10 apply) "
        "(call r10 4) (ret))");

    intptr_t result = env.codegen->compile(code).release_and_run();
    return result == make_fixnum(10);
  });

  // 5. Apply Apply
  run_test("ApplyApply", [](CodegenTest& env) -> bool {
    // (apply apply (list + '(1 2)))
    // Equivalent to (apply + '(1 2)) -> 3
    // Inner list: (+ '(1 2))
    scm_obj_t code = env.read_code(
        "((global-ref r0 +) "
        "(const r1 (1 2)) "
        "(global-ref r2 list) "
        // make list (+ '(1 2))
        "(call r2 2) "             // r0 = (+ (1 2))
        "(mov r1 r0) "             // arg list
        "(global-ref r0 apply) "   // proc to apply
        "(global-ref r10 apply) "  // apply itself
        "(call r10 2) (ret))");

    intptr_t result = env.codegen->compile(code).release_and_run();
    return result == make_fixnum(3);
  });

  // 6. Apply to Closure
  run_test("ApplyToClosure", [](CodegenTest& env) -> bool {
    // (make-closure ...) -> f
    // (apply f '(10))
    // f = (lambda (x) (+ x 1))
    scm_obj_t setup = env.read_code(
        "((make-closure r0 C1 () 1 #f) (global-set! f r0) (ret) "
        "(label C1) (const r1 1) (global-ref r2 +) (call r2 2) (ret))");
    env.codegen->compile(setup).release_and_run();

    scm_obj_t code = env.read_code(
        "((global-ref r0 f) "
        "(const r1 (10)) "
        "(global-ref r10 apply) "
        "(call r10 2) (ret))");

    intptr_t result = env.codegen->compile(code).release_and_run();
    return result == make_fixnum(11);
  });

  context::destroy();
  heap->destroy();
  delete heap;
  return some_test_failed ? 1 : 0;
}
