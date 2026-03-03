#include <cstring>
#include <llvm/Support/TargetSelect.h>
#include "../src/codegen.h"
#include "../src/codegen_aux.h"
#include "../src/nanos_context.h"
#include "../src/object.h"
#include "../src/object_heap.h"

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

static bool test_return_normally() {
  auto dummy_proc = [](scm_obj_t self, int argc, scm_obj_t argv[]) -> scm_obj_t { return make_fixnum(42); };

  scm_obj_t proc = make_closure((void*)*+(dummy_proc), 0, 1, 0, nullptr, scm_nil, 1);
  scm_obj_t result = subr_call_ec(scm_undef, proc);

  if (result != make_fixnum(42)) {
    printf("\033[31m###### call/ec normal return failed\033[0m\n");
    some_test_failed = true;
    return false;
  }
  printf("\033[32mcall/ec normal return passed\033[0m\n");
  return true;
}

static bool test_invoke_continuation() {
  auto invoke_proc = [](scm_obj_t self, int argc, scm_obj_t argv[]) -> scm_obj_t {
    scm_obj_t cont = argv[0];
    scm_obj_t args[] = {make_cons(make_fixnum(100), scm_nil)};
    return c_apply_helper(cont, 1, args);
  };

  scm_obj_t proc = make_closure((void*)*+(invoke_proc), 0, 1, 0, nullptr, scm_nil, 1);
  scm_obj_t result = subr_call_ec(scm_undef, proc);

  if (result != make_fixnum(100)) {
    printf("\033[31m###### call/ec invoke passed\033[0m\n");
    some_test_failed = true;
    return false;
  }
  printf("\033[32mcall/ec invoke continuation passed\033[0m\n");
  return true;
}

static bool test_double_invoke_fails() {
  auto invoke_twice_proc = [](scm_obj_t self, int argc, scm_obj_t argv[]) -> scm_obj_t {
    scm_obj_t cont = argv[0];
    scm_obj_t args[] = {make_cons(make_fixnum(100), scm_nil)};
    c_apply_helper(cont, 1, args);
    return make_fixnum(999);
  };

  scm_obj_t proc = make_closure((void*)*+(invoke_twice_proc), 0, 1, 0, nullptr, scm_nil, 1);
  scm_obj_t result = subr_call_ec(scm_undef, proc);

  if (result != make_fixnum(100)) {
    printf("\033[31m###### call/ec double invoke failed\033[0m\n");
    some_test_failed = true;
    return false;
  }
  printf("\033[32mcall/ec double invoke passed\033[0m\n");
  return true;
}

#ifdef __has_feature
  #if __has_feature(hwaddress_sanitizer)
extern "C" const char* __hwasan_default_options() { return "leak_check_at_exit=0"; }
  #endif
#endif

int main(int argc, char** argv) {
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 2, 1024 * 1024);
  heap->m_collect_trip_bytes = 1024 * 512;

  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  {
    auto jit_expected = nanos_jit_t::Create();
    if (!jit_expected) {
      fprintf(stderr, "Could not create LLJIT: %s\n", llvm::toString(jit_expected.takeError()).c_str());
      exit(1);
    }
    auto jit = std::move(*jit_expected);

    auto ts_ctx = std::make_unique<llvm::LLVMContext>();

    codegen_t cg(std::move(ts_ctx), jit.get());

    scm_obj_t scm_subr_call_ec = make_closure((void*)subr_call_ec, 1, 0, 0, nullptr, scm_nil, 1);
    c_global_set(make_symbol("call/ec"), scm_subr_call_ec);

    test_return_normally();
    test_invoke_continuation();
    test_double_invoke_fails();
  }

  heap->destroy();
  delete heap;

  return some_test_failed ? 1 : 0;
}
