#include "core.h"
#include <cassert>
#include <cstdarg>
#include <dlfcn.h>
#include <llvm/Support/TargetSelect.h>
#include <sstream>
#include "codegen.h"
#include "context.h"
#include "equiv.h"
#include "hash.h"
#include "nanos.h"
#include "nanos_jit.h"
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

SUBR subr_codegen_cdecl_callout(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_codegen_cdecl_callback(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_string_utf8_nul(scm_obj_t self, scm_obj_t a1);

static bool some_test_failed = false;

#define ASSERT_TRUE(expr)                         \
  do {                                            \
    if (!(expr)) {                                \
      printf("\033[31mFAIL: %s\033[0m\n", #expr); \
      some_test_failed = true;                    \
    } else {                                      \
      printf("\033[32mPASS: %s\033[0m\n", #expr); \
    }                                             \
  } while (0)

class CodegenTest {
 public:
  nanos_t* nanos;
  CodegenTest() {
    nanos = new nanos_t();
    nanos->init();
  }
  ~CodegenTest() {
    nanos->destroy();
    delete nanos;
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

__attribute__((no_sanitize("hwaddress"))) SUBR test_qsort_compare(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  uint32_t* p1 = (uint32_t*)fixnum(a1);
  uint32_t* p2 = (uint32_t*)fixnum(a2);
  if (*p1 == *p2) return make_fixnum(0);
  if (*p1 < *p2) return make_fixnum(1);
  return make_fixnum(-1);
}

int main(int argc, char** argv) {
  printf("Starting test_cffi\n");
  fflush(stdout);

  run_test("TestCFFI_snprintf", [](CodegenTest& env) -> bool {
    void* sym_snprintf = dlsym(RTLD_DEFAULT, "snprintf");
    if (!sym_snprintf) {
      printf("snprintf not found!\n");
      return false;
    }

    scm_obj_t adrs = make_fixnum((intptr_t)sym_snprintf);
    scm_obj_t sig1 = make_string("qoooox");  // caller: int(void* size_t void*) unsigned-long double -> q o o o o x
    scm_obj_t sig2 = make_string("qooo");    // callee: int(void* size_t void*) -> q o o o
    scm_obj_t args[] = {adrs, sig1, sig2};
    scm_obj_t snprintf_closure = subr_codegen_cdecl_callout(scm_nil, 3, args);

    scm_obj_t buf = make_u8vector(32);
    memset(u8vector_elts(buf), 0, 32);

    scm_obj_t fmt = subr_string_utf8_nul(scm_nil, make_string("%06lu %.3lf"));

    scm_obj_t call_args[] = {buf, make_fixnum(32), fmt, make_fixnum(246), make_flonum(123.4)};

    codegen_t::bridge_func_t bridge = codegen_t::current()->call_closure_bridge();
    scm_obj_t n = (scm_obj_t)bridge(snprintf_closure, 5, call_args);

    if (!is_fixnum(n) || fixnum(n) <= 0) {
      printf("snprintf failed or returned non-fixnum\n");
      return false;
    }

    const char* result_str = (const char*)u8vector_elts(buf);
    printf("pos: %ld\n", fixnum(n));
    printf("result: %s\n", result_str);

    if (strcmp(result_str, "000246 123.400") != 0) {
      printf("snprintf output mismatch! expected: '000246 123.400'\n");
      return false;
    }

    return true;
  });

  run_test("TestCFFI_qsort", [](CodegenTest& env) -> bool {
    scm_obj_t comp_closure = make_closure((void*)test_qsort_compare, 2, 0, 0, nullptr, 0);
    scm_obj_t sig_cmp = make_string("qoo");
    scm_obj_t cmp_callback_ptr_obj = subr_codegen_cdecl_callback(scm_nil, comp_closure, sig_cmp);

    void* sym_qsort = dlsym(RTLD_DEFAULT, "qsort");
    if (!sym_qsort) {
      printf("qsort not found!\n");
      return false;
    }

    scm_obj_t adrs_qsort = make_fixnum((intptr_t)sym_qsort);
    scm_obj_t sig_qsort = make_string("ioqqo");
    scm_obj_t args_qsort[] = {adrs_qsort, sig_qsort};
    scm_obj_t qsort_closure = subr_codegen_cdecl_callout(scm_nil, 2, args_qsort);

    scm_obj_t nums = make_u8vector(20);
    uint32_t* p = (uint32_t*)u8vector_elts(nums);
    p[0] = 10000;
    p[1] = 1000;
    p[2] = 10;
    p[3] = 100000;
    p[4] = 100;

    printf("org: ");
    for (int i = 0; i < 5; i++) printf("%d ", p[i]);
    printf("\n");

    scm_obj_t call_args[] = {nums, make_fixnum(5), make_fixnum(4), cmp_callback_ptr_obj};

    codegen_t::bridge_func_t bridge = codegen_t::current()->call_closure_bridge();
    bridge(qsort_closure, 4, call_args);

    printf("sorted: ");
    for (int i = 0; i < 5; i++) printf("%d ", p[i]);
    printf("\n");

    if (p[0] != 100000 || p[1] != 10000 || p[2] != 1000 || p[3] != 100 || p[4] != 10) {
      printf("qsort array is not sorted as expected!\n");
      return false;
    }

    return true;
  });

  return some_test_failed ? 1 : 0;
}
