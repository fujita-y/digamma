#include <cstring>
#include <llvm/Support/TargetSelect.h>
#include <sanitizer/hwasan_interface.h>
#include "../src/codegen.h"
#include "../src/codegen_aux.h"
#include "../src/continuation.h"
#include "../src/context.h"
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
  scm_obj_t proc = make_closure((void*)*+(dummy_proc), 0, 1, 0, nullptr, 1);
  scm_obj_t result = subr_call_cc(scm_undef, proc);
  if (result != make_fixnum(42)) {
    printf("\033[31m###### call/cc normal return failed\033[0m\n");
    some_test_failed = true;
    return false;
  }
  printf("\033[32mcall/cc normal return passed\033[0m\n");
  return true;
}

static bool test_invoke_continuation() {
  auto invoke_proc = [](scm_obj_t self, int argc, scm_obj_t argv[]) -> scm_obj_t {
    scm_obj_t cont = argv[0];
    scm_obj_t args[] = {make_cons(make_fixnum(100), scm_nil)};
    return c_apply_helper(cont, 1, args);
  };
  scm_obj_t proc = make_closure((void*)*+(invoke_proc), 0, 1, 0, nullptr, 1);
  scm_obj_t result = subr_call_cc(scm_undef, proc);
  if (result != make_fixnum(100)) {
    printf("\033[31m###### call/cc invoke failed\033[0m\n");
    some_test_failed = true;
    return false;
  }
  printf("\033[32mcall/cc invoke continuation passed\033[0m\n");
  return true;
}

static bool test_multishot() {
  static int count = 0;
  static scm_obj_t global_cont = scm_undef;

  auto multishot_proc = [](scm_obj_t self, int argc, scm_obj_t argv[]) -> scm_obj_t {
    global_cont = argv[0];
    context::gc_protect(global_cont);
    return make_fixnum(count++);
  };

  scm_obj_t proc = make_closure((void*)*+(multishot_proc), 0, 1, 0, nullptr, 1);
  printf("test_multishot: calling call/cc (initial)\n");
  fflush(stdout);
  scm_obj_t result = subr_call_cc(scm_undef, proc);
  printf("test_multishot: call/cc returned result=%ld, count=%d\n", is_fixnum(result) ? fixnum(result) : -1, count);
  fflush(stdout);

  if (count < 3) {
    printf("test_multishot: incrementing count and invoking global_cont (count=%d -> %d)\n", count, count + 1);
    fflush(stdout);
    int next_val = count++;  // Increment AFTER return
    scm_obj_t args[] = {make_cons(make_fixnum(next_val), scm_nil)};
    c_apply_helper(global_cont, 1, args);
  }

  if (count == 3 && result == make_fixnum(2)) {
    printf("\033[32mcall/cc multishot passed\033[0m\n");
    return true;
  }

  printf("\033[31m###### call/cc multishot failed (count=%d, result=%ld)\033[0m\n", count, is_fixnum(result) ? fixnum(result) : -1);
  some_test_failed = true;
  return false;
}

#ifdef __has_feature
  #if __has_feature(hwaddress_sanitizer)
extern "C" const char* __hwasan_default_options() { return "leak_check_at_exit=0"; }
  #endif
#endif

int main(int argc, char** argv) {
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 64, 1024 * 1024 * 16);
  context::init();

  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  codegen_t* codegen = nullptr;
  {
    auto jit_expected = nanos_jit_t::Create();
    if (!jit_expected) exit(1);
    auto jit = std::move(*jit_expected);
    auto ts_ctx = std::make_unique<llvm::LLVMContext>();
    codegen = new codegen_t(std::move(ts_ctx), jit.get());

    test_return_normally();
    test_invoke_continuation();
    test_multishot();
  }
  codegen->destroy();
  delete codegen;

  context::destroy();
  heap->destroy();
  delete heap;
  puts("all test done");
  return some_test_failed ? 1 : 0;
}
