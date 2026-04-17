// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include <llvm/Support/TargetSelect.h>
#include <sstream>
#include "codegen.h"
#include "context.h"
#include "equiv.h"
#include "hash.h"
#include "nanos.h"
#include "object_heap.h"
#include "reader.h"

SUBR subr_num_add(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_num_sub(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_num_mul(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_num_div(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_list(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_car(scm_obj_t self, scm_obj_t a1);
SUBR subr_cdr(scm_obj_t self, scm_obj_t a1);
SUBR subr_cadr(scm_obj_t self, scm_obj_t a1);
SUBR subr_caddr(scm_obj_t self, scm_obj_t a1);
SUBR subr_cons(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_null_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_pair_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_not(scm_obj_t self, scm_obj_t a1);
SUBR subr_eq_p(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_num_eq(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_append(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_write(scm_obj_t self, scm_obj_t a1);
SUBR subr_newline(scm_obj_t self);

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

static scm_obj_t codegen_and_run(scm_obj_t inst_list) {
  scoped_gc_protect protect(inst_list);
  compiled_code_t func = codegen_t::current()->compile(inst_list);
  scm_obj_t result = (scm_obj_t)func.release_and_run();
  return result;
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

void register_core_primitives() {
  c_global_set(make_symbol("+"), make_closure((void*)subr_num_add, 0, 1, 0, nullptr, 1));
  c_global_set(make_symbol("-"), make_closure((void*)subr_num_sub, 0, 1, 0, nullptr, 1));
  c_global_set(make_symbol("*"), make_closure((void*)subr_num_mul, 0, 1, 0, nullptr, 1));
  c_global_set(make_symbol("/"), make_closure((void*)subr_num_div, 0, 1, 0, nullptr, 1));
  c_global_set(make_symbol("list"), make_closure((void*)subr_list, 0, 1, 0, nullptr, 1));
  c_global_set(make_symbol("car"), make_closure((void*)subr_car, 1, 0, 0, nullptr, 1));
  c_global_set(make_symbol("cdr"), make_closure((void*)subr_cdr, 1, 0, 0, nullptr, 1));
  c_global_set(make_symbol("cadr"), make_closure((void*)subr_cadr, 1, 0, 0, nullptr, 1));
  c_global_set(make_symbol("caddr"), make_closure((void*)subr_caddr, 1, 0, 0, nullptr, 1));
  c_global_set(make_symbol("cons"), make_closure((void*)subr_cons, 2, 0, 0, nullptr, 1));
  c_global_set(make_symbol("null?"), make_closure((void*)subr_null_p, 1, 0, 0, nullptr, 1));
  c_global_set(make_symbol("pair?"), make_closure((void*)subr_pair_p, 1, 0, 0, nullptr, 1));
  c_global_set(make_symbol("not"), make_closure((void*)subr_not, 1, 0, 0, nullptr, 1));
  c_global_set(make_symbol("eq?"), make_closure((void*)subr_eq_p, 2, 0, 0, nullptr, 1));
  c_global_set(make_symbol("="), make_closure((void*)subr_num_eq, 0, 1, 0, nullptr, 1));
  c_global_set(make_symbol("append"), make_closure((void*)subr_append, 0, 1, 0, nullptr, 1));
  c_global_set(make_symbol("write"), make_closure((void*)subr_write, 1, 0, 0, nullptr, 1));
  c_global_set(make_symbol("newline"), make_closure((void*)subr_newline, 0, 0, 0, nullptr, 1));
}

int main(int argc, char** argv) {
  printf("Starting test_deriv\n");
  fflush(stdout);
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 2, 1024 * 1024);
  context::init();

  run_test("DerivTest", [](CodegenTest& env) -> bool {
    // Register Primitives
    register_core_primitives();

    // Define map
    scm_obj_t map_code = env.read_code(
        "((make-closure r0 C1 () 2 #f) (global-set! map r0) (ret) "
        "(label C1) (mov r3 r1) (mov r2 r0) (mov r4 r3) (global-ref r5 null?) (mov r0 r4) "
        "(call r5 1) (if L1 L2) (label L1) (const r0 ()) (ret) (label L2) (mov r6 r3) "
        "(global-ref r7 car) (mov r0 r6) (call r7 1) (mov r5 r0) (mov r6 r2) (call r6 1) "
        "(mov r4 r0) (mov r6 r2) (mov r8 r3) (global-ref r9 cdr) (mov r0 r8) (call r9 1) "
        "(mov r7 r0) (global-ref r8 map) (mov r0 r6) (mov r1 r7) (call r8 2) (mov r5 r0) "
        "(global-ref r6 cons) (mov r0 r4) (mov r1 r5) (tail-call r6 2))");
    codegen_and_run(map_code);

    // Define deriv
    scm_obj_t deriv_code = env.read_code(
        "((make-closure r0 C1 () 1 #f) (global-set! deriv r0) (ret) "
        "(label C1) (mov r4 r0) (mov r6 r0) (global-ref r7 pair?) (call r7 1) (mov r5 r0) "
        "(global-ref r6 not) (call r6 1) (if L1 L2) (label L1) (mov r5 r4) (const r6 x) "
        "(global-ref r7 eq?) (mov r0 r5) (mov r1 r6) (call r7 2) (if L4 L5) (label L4) "
        "(const r0 1) (ret) (label L5) (const r0 0) (ret) (label L2) (mov r6 r4) "
        "(global-ref r7 car) (mov r0 r6) (call r7 1) (mov r5 r0) (const r6 +) "
        "(global-ref r7 eq?) (mov r1 r6) (call r7 2) (if L7 L8) (label L7) (const r5 +) "
        "(global-ref r7 deriv) (mov r9 r4) (global-ref r10 cdr) (mov r0 r9) (call r10 1) "
        "(mov r8 r0) (global-ref r9 map) (mov r0 r7) (mov r1 r8) (call r9 2) (mov r6 r0) "
        "(global-ref r7 cons) (mov r0 r5) (mov r1 r6) (tail-call r7 2) (label L8) "
        "(mov r6 r4) (global-ref r7 car) (mov r0 r6) (call r7 1) (mov r5 r0) (const r6 -) "
        "(global-ref r7 eq?) (mov r1 r6) (call r7 2) (if L10 L11) (label L10) (const r5 -) "
        "(global-ref r7 deriv) (mov r9 r4) (global-ref r10 cdr) (mov r0 r9) (call r10 1) "
        "(mov r8 r0) (global-ref r9 map) (mov r0 r7) (mov r1 r8) (call r9 2) (mov r6 r0) "
        "(global-ref r7 cons) (mov r0 r5) (mov r1 r6) (tail-call r7 2) (label L11) "
        "(mov r6 r4) (global-ref r7 car) (mov r0 r6) (call r7 1) (mov r5 r0) (const r6 *) "
        "(global-ref r7 eq?) (mov r1 r6) (call r7 2) (if L13 L14) (label L13) (const r5 *) "
        "(mov r6 r4) (const r0 +) (mov r8 r0) (make-closure r0 C2 () 1 #f) (mov r10 r0) "
        "(mov r12 r4) (global-ref r13 cdr) (mov r0 r12) (call r13 1) (mov r11 r0) "
        "(global-ref r12 map) (mov r0 r10) (mov r1 r11) (call r12 2) (mov r9 r0) "
        "(global-ref r10 cons) (mov r0 r8) (mov r1 r9) (call r10 2) (mov r7 r0) "
        "(global-ref r8 list) (mov r0 r5) (mov r1 r6) (mov r2 r7) (tail-call r8 3) "
        "(label L14) (mov r6 r4) (global-ref r7 car) (mov r0 r6) (call r7 1) (mov r5 r0) "
        "(const r6 /) (global-ref r7 eq?) (mov r1 r6) (call r7 2) (if L16 L17) (label L16) "
        "(const r5 -) (const r7 /) (mov r10 r4) (global-ref r11 cadr) (mov r0 r10) "
        "(call r11 1) (mov r9 r0) (global-ref r10 deriv) (call r10 1) (mov r8 r0) "
        "(mov r10 r4) (global-ref r11 caddr) (mov r0 r10) (call r11 1) (mov r9 r0) "
        "(global-ref r10 list) (mov r0 r7) (mov r1 r8) (mov r2 r9) (call r10 3) (mov r6 r0) "
        "(const r8 /) (mov r10 r4) (global-ref r11 cadr) (mov r0 r10) (call r11 1) "
        "(mov r9 r0) (const r11 *) (mov r13 r4) (global-ref r14 caddr) (mov r0 r13) "
        "(call r14 1) (mov r12 r0) (mov r14 r4) (global-ref r15 caddr) (mov r0 r14) "
        "(call r15 1) (mov r13 r0) (mov r16 r4) (global-ref r17 caddr) (mov r0 r16) "
        "(call r17 1) (mov r15 r0) (global-ref r16 deriv) (call r16 1) (mov r14 r0) "
        "(global-ref r15 list) (mov r0 r11) (mov r1 r12) (mov r2 r13) (mov r3 r14) "
        "(call r15 4) (mov r10 r0) (global-ref r11 list) (mov r0 r8) (mov r1 r9) "
        "(mov r2 r10) (call r11 3) (mov r7 r0) (global-ref r8 list) (mov r0 r5) "
        "(mov r1 r6) (mov r2 r7) (tail-call r8 3) (label L17) "
        "(const r0 \"No derivation method available\") (mov r5 r0) (tail-call r5 0) "
        "(label C2) (mov r3 r0) (const r4 /) (mov r6 r3) (global-ref r7 deriv) (mov r0 r6) "
        "(call r7 1) (mov r5 r0) (mov r6 r3) (global-ref r7 list) (mov r0 r4) (mov r1 r5) "
        "(mov r2 r6) (tail-call r7 3))");
    codegen_and_run(deriv_code);

    // Run test case
    scm_obj_t test_case = env.read_code(
        "((const r1 (+ (* 3 x x) (* a x x) (* b x) 5)) "
        "(global-ref r2 deriv) (mov r0 r1) (call r2 1) (ret))");

    intptr_t result = (intptr_t)codegen_and_run(test_case);

    // Check result
    const char* expected_sexp =
        "(+ (* (* 3 x x) (+ (/ 0 3) (/ 1 x) (/ 1 x))) (* (* a x x) (+ (/ 0 a) (/ 1 x) (/ 1 x))) (* (* b x) (+ (/ 0 b) (/ 1 x))) 0)";
    scm_obj_t expected = env.read_code(expected_sexp);

    object_heap_t* heap = object_heap_t::current();
    if (!equal_p(result, expected)) {
      printf("DerivTest failed: result does not match expected.\n");
      printf("Expected: %s\n", expected_sexp);
      // Note: We don't have a printer here to show result easily :()
      return false;
    }
    return true;
  });

  run_test("MapClosureTest1", [](CodegenTest& env) -> bool {
    register_core_primitives();

    // Define map
    scm_obj_t map_code = env.read_code(R"(
      ((make-closure r0 C1 () 2 #f)
       (global-set! map r0)
       (ret)
       (label C1)
       (mov r3 r1)
       (mov r2 r0)
       (mov r4 r3)
       (global-ref r5 null?)
       (mov r0 r4)
       (call r5 1)
       (if L1 L2)
       (label L1)
       (const r0 ())
       (ret)
       (label L2)
       (mov r6 r3)
       (global-ref r7 car)
       (mov r0 r6)
       (call r7 1)
       (mov r5 r0)
       (mov r6 r2)
       (call r6 1)
       (mov r4 r0)
       (mov r6 r2)
       (mov r8 r3)
       (global-ref r9 cdr)
       (mov r0 r8)
       (call r9 1)
       (mov r7 r0)
       (global-ref r8 map)
       (mov r0 r6)
       (mov r1 r7)
       (call r8 2)
       (mov r5 r0)
       (global-ref r6 cons)
       (mov r0 r4)
       (mov r1 r5)
       (tail-call r6 2))
    )");
    codegen_and_run(map_code);

    // Run test case
    scm_obj_t test_case = env.read_code(R"(
      ((make-closure r0 C1 () 1 #f) 
       (mov r2 r0) 
       (const r3 (1 2 3)) 
       (global-ref r4 map) 
       (mov r1 r3) 
       (call r4 2) 
       (ret) 
       (label C1) 
       (mov r2 r0) 
       (const r3 5) 
       (mov r4 r2) 
       (global-ref r5 -) 
       (mov r0 r3) 
       (mov r1 r4) 
       (tail-call r5 2))
    )");
    intptr_t result = (intptr_t)codegen_and_run(test_case);

    // Expected: (4 3 2)
    const char* expected_sexp = "(4 3 2)";
    scm_obj_t expected = env.read_code(expected_sexp);

    object_heap_t* heap = object_heap_t::current();
    if (!equal_p(result, expected)) {
      printf("MapClosureTest1 failed: result != (4 3 2)\n");
      return false;
    }
    return true;
  });

  run_test("MapClosureTest2", [](CodegenTest& env) -> bool {
    register_core_primitives();

    // Define map
    scm_obj_t map_code = env.read_code(R"(
      ((make-closure r0 C1 () 2 #f) (global-set! map r0) (ret) (label C1) (mov r2 r1) (mov r1 r0) (const r0 #f) (mov r3 r0) (closure-self r3) (make-closure r0 C2 (r3) 2 #f) (mov r3 r0) (mov r4 r1) (mov r0 r2) (mov r5 r0) (mov r6 r3) (mov r0 r4) (mov r1 r5) (tail-call r6 2) (label C2) (mov r3 r1) (mov r2 r0) (mov r4 r3) (global-ref r5 null?) (mov r0 r4) (call r5 1) (if L1 L2) (label L1) (const r0 ()) (ret) (label L2) (mov r6 r3) (global-ref r7 car) (mov r0 r6) (call r7 1) (mov r5 r0) (mov r6 r2) (call r6 1) (mov r4 r0) (mov r6 r2) (mov r8 r3) (global-ref r9 cdr) (mov r0 r8) (call r9 1) (mov r7 r0) (closure-self r0) (mov r8 r0) (mov r0 r6) (mov r1 r7) (call r8 2) (mov r5 r0) (global-ref r6 cons) (mov r0 r4) (mov r1 r5) (tail-call r6 2))
    )");
    codegen_and_run(map_code);

    // Run test case
    scm_obj_t test_case = env.read_code(R"(
      ((make-closure r0 C1 () 1 #f) (mov r2 r0) (const r3 (1 2 3)) (global-ref r4 map) (mov r1 r3) (call r4 2) (ret) (label C1) (mov r2 r0) (const r3 5) (mov r4 r2) (global-ref r5 list) (mov r0 r3) (mov r1 r4) (tail-call r5 2))
    )");
    intptr_t result = (intptr_t)codegen_and_run(test_case);

    // Expected: ((5 1) (5 2) (5 3))
    const char* expected_sexp = "((5 1) (5 2) (5 3))";
    scm_obj_t expected = env.read_code(expected_sexp);

    object_heap_t* heap = object_heap_t::current();
    if (!equal_p(result, expected)) {
      printf("MapClosureTest2 failed: result != ((5 1) (5 2) (5 3))\n");
      return false;
    }

    return true;
  });

  run_test("NQueensTest", [](CodegenTest& env) -> bool {
    register_core_primitives();

    // Set trace? to #f
    scm_obj_t set_trace = env.read_code("((const r0 #f) (global-set! trace? r0) (ret))");
    codegen_and_run(set_trace);

    // Define nqueens and its helper functions
    scm_obj_t nqueens_code = env.read_code(R"(
      ((make-closure r0 C1 () 1 #f) (global-set! nqueens r0) (ret)
       (label C1) (mov r3 r0) (const r0 *undefined*) (mov r4 r0) (make-cell r4) (const r0 *undefined*) (mov r5 r0) (make-cell r5) (const r0 *undefined*) (mov r6 r0) (make-cell r6) (make-closure r0 C2 () 1 #f) (reg-cell-set! r4 r0) (make-closure r0 C4 (r6 r5) 3 #f) (reg-cell-set! r5 r0) (make-closure r0 C5 (r6) 3 #f) (reg-cell-set! r6 r0) (mov r8 r3) (reg-cell-ref r9 r4) (mov r0 r8) (call r9 1) (mov r7 r0) (const r8 ()) (const r9 ()) (reg-cell-ref r10 r5) (mov r1 r8) (mov r2 r9) (tail-call r10 3)
       (label C5) (mov r5 r2) (mov r4 r1) (mov r3 r0) (mov r6 r5) (global-ref r7 null?) (mov r0 r6) (call r7 1) (if L16 L17) (label L16) (const r0 #t) (ret) (label L17) (mov r8 r5) (global-ref r9 car) (mov r0 r8) (call r9 1) (mov r7 r0) (mov r9 r3) (mov r10 r4) (global-ref r11 +) (mov r0 r9) (mov r1 r10) (call r11 2) (mov r8 r0) (global-ref r9 =) (mov r0 r7) (mov r1 r8) (call r9 2) (mov r6 r0) (global-ref r7 not) (call r7 1) (if L19 L20) (label L19) (mov r8 r5) (global-ref r9 car) (mov r0 r8) (call r9 1) (mov r7 r0) (mov r9 r3) (mov r10 r4) (global-ref r11 -) (mov r0 r9) (mov r1 r10) (call r11 2) (mov r8 r0) (global-ref r9 =) (mov r0 r7) (mov r1 r8) (call r9 2) (mov r6 r0) (global-ref r7 not) (call r7 1) (if L22 L23) (label L22) (mov r6 r3) (mov r8 r4) (const r9 1) (global-ref r10 +) (mov r0 r8) (mov r1 r9) (call r10 2) (mov r7 r0) (mov r9 r5) (global-ref r10 cdr) (mov r0 r9) (call r10 1) (mov r8 r0) (closure-cell-ref r9 0) (mov r0 r6) (mov r1 r7) (mov r2 r8) (tail-call r9 3) (label L23) (const r0 #f) (ret) (label L20) (const r0 #f) (ret)
       (label C4) (mov r5 r2) (mov r4 r1) (mov r3 r0) (mov r6 r0) (global-ref r7 null?) (call r7 1) (if L4 L5) (label L4) (mov r6 r4) (global-ref r7 null?) (mov r0 r6) (call r7 1) (if L7 L8) (label L7) (global-ref r0 trace?) (if L10 L11) (label L10) (mov r6 r5) (global-ref r7 write) (mov r0 r6) (call r7 1) (global-ref r0 newline) (mov r6 r0) (call r6 0) (jump L12) (label L11) (const r0 #f) (label L12) (const r0 1) (ret) (label L8) (const r0 0) (ret) (label L5) (mov r8 r3) (global-ref r9 car) (mov r0 r8) (call r9 1) (mov r7 r0) (const r8 1) (mov r9 r5) (closure-cell-ref r10 0) (mov r1 r8) (mov r2 r9) (call r10 3) (if L13 L14) (label L13) (mov r9 r3) (global-ref r10 cdr) (mov r0 r9) (call r10 1) (mov r8 r0) (mov r9 r4) (global-ref r10 append) (mov r1 r9) (call r10 2) (mov r7 r0) (const r8 ()) (mov r11 r3) (global-ref r12 car) (mov r0 r11) (call r12 1) (mov r10 r0) (mov r11 r5) (global-ref r12 cons) (mov r1 r11) (call r12 2) (mov r9 r0) (closure-cell-ref r10 1) (mov r0 r7) (mov r1 r8) (mov r2 r9) (call r10 3) (jump L15) (label L14) (const r0 0) (label L15) (mov r6 r0) (mov r9 r3) (global-ref r10 cdr) (mov r0 r9) (call r10 1) (mov r8 r0) (mov r11 r3) (global-ref r12 car) (mov r0 r11) (call r12 1) (mov r10 r0) (mov r11 r4) (global-ref r12 cons) (mov r1 r11) (call r12 2) (mov r9 r0) (mov r10 r5) (closure-cell-ref r11 1) (mov r0 r8) (mov r1 r9) (mov r2 r10) (call r11 3) (mov r7 r0) (global-ref r8 +) (mov r0 r6) (mov r1 r7) (tail-call r8 2)
       (label C2) (mov r1 r0) (const r0 #f) (mov r2 r0) (closure-self r2) (make-closure r0 C3 (r2) 2 #f) (mov r2 r0) (mov r3 r1) (const r0 ()) (mov r4 r0) (mov r5 r2) (mov r0 r3) (mov r1 r4) (tail-call r5 2)
       (label C3) (mov r3 r1) (mov r2 r0) (mov r4 r0) (const r5 0) (global-ref r6 =) (mov r1 r5) (call r6 2) (if L1 L2) (label L1) (mov r0 r3) (ret) (label L2) (mov r5 r2) (const r6 1) (global-ref r7 -) (mov r0 r5) (mov r1 r6) (call r7 2) (mov r4 r0) (mov r6 r2) (mov r7 r3) (global-ref r8 cons) (mov r0 r6) (mov r1 r7) (call r8 2) (mov r5 r0) (closure-self r0) (mov r6 r0) (mov r0 r4) (mov r1 r5) (tail-call r6 2)))
    )");
    codegen_and_run(nqueens_code);

    // Run test case: (nqueens 8)
    scm_obj_t test_case = env.read_code(R"(
      ((const r1 8) (global-ref r2 nqueens) (mov r0 r1) (call r2 1) (ret))
    )");
    intptr_t result = (intptr_t)codegen_and_run(test_case);

    // Expected: 92
    if (result != make_fixnum(92)) {
      printf("NQueensTest failed: expected 92, got %ld\n", fixnum((scm_obj_t)result));
      return false;
    }

    return true;
  });

  context::destroy();
  heap->destroy();
  delete heap;
  return some_test_failed ? 1 : 0;
}
