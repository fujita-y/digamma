#include "core.h"
#include "object.h"
#include <iostream>
#include <llvm/Support/TargetSelect.h>
#include <sstream>
#include "codegen.h"
#include "hash.h"
#include "nanos_subr.h"
#include "object_heap.h"
#include "reader.h"

#define PASSED(m) std::cout << "PASSED: " << (m) << std::endl
#define FAILED(m) std::cout << "FAILED: " << (m) << std::endl

// Mock primitives
extern "C" scm_obj_t subr_num_add(scm_obj_t self, int argc, scm_obj_t argv[]);
extern "C" scm_obj_t subr_apply(scm_obj_t, int, scm_obj_t*);
extern "C" scm_obj_t subr_list(scm_obj_t self, int argc, scm_obj_t argv[]);

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

static void c_global_set(scm_obj_t sym, scm_obj_t val) {
  object_heap_t* heap = object_heap_t::current();
  scm_obj_t env = heap->m_environment;
  scm_environment_rec_t* env_rec = (scm_environment_rec_t*)to_address(env);
  hashtable_set(env_rec->variables, sym, make_cell(val));
}

inline scm_obj_t my_tc6_pointer(void* x, uintptr_t tc6_num) { return (uintptr_t)x | 0x02; }

inline scm_tc6_t my_tc6_tag(uintptr_t tc6_num) { return (tc6_num << 8) | 0x06; }

scm_obj_t my_make_closure(void* code, int argc, int rest, int nsize, scm_obj_t env[], scm_obj_t literals, int cdecl) {
  // object_heap_t& heap = *object_heap_t::current();
  // scm_closure_rec_t* rec = (scm_closure_rec_t*)heap.alloc_collectible(sizeof(scm_closure_rec_t) + (nsize - 1) * sizeof(scm_obj_t));
  scm_closure_rec_t* rec = (scm_closure_rec_t*)malloc(sizeof(scm_closure_rec_t) + (nsize > 0 ? (nsize - 1) * sizeof(scm_obj_t) : 0));
  rec->tag = my_tc6_tag(tc6_closure);
  rec->literals = literals;
  rec->code = code;
  rec->argc = argc;
  rec->rest = rest;
  rec->nsize = nsize;
  rec->cdecl = cdecl;
  for (int i = 0; i < nsize; i++) {
    rec->env[i] = env[i];
  }
  return my_tc6_pointer(rec, tc6_closure);
}

scm_obj_t read_code(const std::string& input) {
  // ...
  std::istringstream is(input);
  reader_t reader(is);
  bool err = false;
  scm_obj_t obj = reader.read(err);
  if (err) {
    std::cerr << "Read error: " << reader.get_error_message() << std::endl;
    exit(1);
  }
  return obj;
}

void test_simple_apply(codegen_t& codegen) {
  std::cout << "Testing simple apply..." << std::endl;
  // (apply + '(1 2)) -> 3
  scm_obj_t code = read_code(
      "((const r10 (1 2)) "
      "(global-ref r11 +) "
      "(global-ref r12 apply) "
      "(mov r0 r11) "
      "(mov r1 r10) "
      "(call r12 2) "
      "(ret))");

  intptr_t result = codegen.compile(code);
  if (result == make_fixnum(3))
    PASSED("simple apply");
  else
    FAILED("simple apply");
}

void test_mixed_args_apply(codegen_t& codegen) {
  std::cout << "Testing mixed args apply..." << std::endl;
  // (apply + 1 2 '(3)) -> 6
  scm_obj_t code = read_code(
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

  intptr_t result = codegen.compile(code);
  if (result == make_fixnum(6))
    PASSED("mixed args apply");
  else
    FAILED("mixed args apply");
}

void test_apply_empty_list(codegen_t& codegen) {
  std::cout << "Testing apply with empty list..." << std::endl;
  // (apply + 1 2 '()) -> 3
  scm_obj_t code = read_code(
      "((const r10 ()) "
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

  intptr_t result = codegen.compile(code);
  if (result == make_fixnum(3))
    PASSED("apply with empty list");
  else
    FAILED("apply with empty list");
}

void test_musttail_apply(codegen_t& codegen) {
  std::cout << "Testing musttail apply reproduction..." << std::endl;
  // ((global-ref r4 +) (const r6 1) (const r7 2) (const r8 3) (const r9 4) (global-ref r10 list)
  //  (mov r0 r6) (mov r1 r7) (mov r2 r8) (mov r3 r9) (call r10 4) (mov r5 r0)
  //  (global-ref r6 apply) (mov r0 r4) (mov r1 r5) (call r6 2) (ret))
  scm_obj_t code = read_code(
      "( (global-ref r4 +) (const r6 1) (const r7 2) (const r8 3) (const r9 4) (global-ref r10 list) "
      "  (mov r0 r6) (mov r1 r7) (mov r2 r8) (mov r3 r9) (call r10 4) (mov r5 r0) "
      "  (global-ref r6 apply) (mov r0 r4) (mov r1 r5) (call r6 2) (ret) )");

  intptr_t result = codegen.compile(code);
  if (result == make_fixnum(10))
    PASSED("musttail apply repro");
  else
    FAILED("musttail apply repro");
}

int main() {
  object_heap_t heap;
  heap.init(4 * 1024 * 1024, 1024 * 1024);
  std::cerr << "Heap initialized" << std::endl;
  std::cerr << "Current heap: " << object_heap_t::current() << std::endl;

  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();

  auto ts_ctx = std::make_unique<llvm::orc::ThreadSafeContext>(std::make_unique<llvm::LLVMContext>());
  auto jit = llvm::orc::LLJITBuilder().create();
  if (!jit) {
    std::cerr << "Failed to create LLJIT" << std::endl;
    return 1;
  }
  codegen_t codegen(*ts_ctx, jit->get());
  nanos_set_codegen(&codegen);

  // Register primitives
  scm_obj_t scm_subr_num_add = my_make_closure((void*)subr_num_add, 0, 1, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("+"), scm_subr_num_add);

  scm_obj_t scm_subr_list = my_make_closure((void*)subr_list, 0, 1, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("list"), scm_subr_list);

  scm_obj_t scm_subr_apply = my_make_closure((void*)subr_apply, 0, 1, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("apply"), scm_subr_apply);

  test_simple_apply(codegen);
  test_mixed_args_apply(codegen);
  test_apply_empty_list(codegen);
  test_musttail_apply(codegen);

  std::cout << "All tests passed." << std::endl;
  return 0;
}
