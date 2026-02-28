#include <cstring>
#include <llvm/Support/TargetSelect.h>
#include <sanitizer/hwasan_interface.h>
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

static int pre_count = 0;
static int value_count = 0;
static int post_count = 0;

static scm_obj_t subr_pre(scm_obj_t self, int argc, scm_obj_t argv[]) {
  pre_count++;
  return scm_unspecified;
}

static scm_obj_t subr_post(scm_obj_t self, int argc, scm_obj_t argv[]) {
  post_count++;
  return scm_unspecified;
}

static bool test_basic_dynamic_wind() {
  pre_count = 0;
  value_count = 0;
  post_count = 0;

  auto value_proc = [](scm_obj_t self, int argc, scm_obj_t argv[]) -> scm_obj_t {
    value_count++;
    return make_fixnum(42);
  };

  scm_obj_t pre = make_closure((void*)subr_pre, 0, 0, 0, nullptr, scm_nil, 1);
  scm_obj_t value = make_closure((void*)*+(value_proc), 0, 0, 0, nullptr, scm_nil, 1);
  scm_obj_t post = make_closure((void*)subr_post, 0, 0, 0, nullptr, scm_nil, 1);

  scm_obj_t result = subr_dynamic_wind(scm_undef, pre, value, post);

  if (result == make_fixnum(42) && pre_count == 1 && value_count == 1 && post_count == 1) {
    printf("\033[32mdynamic-wind basic passed\033[0m\n");
    return true;
  }
  printf("\033[31m###### dynamic-wind basic failed (pre=%d, value=%d, post=%d)\033[0m\n", pre_count, value_count, post_count);
  some_test_failed = true;
  return false;
}

static bool test_dynamic_wind_with_call_cc() {
  pre_count = 0;
  value_count = 0;
  post_count = 0;
  static scm_obj_t captured_cont = scm_undef;

  auto value_proc = [](scm_obj_t self, int argc, scm_obj_t argv[]) -> scm_obj_t {
    value_count++;
    auto callcc_proc = [](scm_obj_t self, int argc, scm_obj_t argv[]) -> scm_obj_t {
      captured_cont = argv[0];
      object_heap_t::current()->add_root(captured_cont);
      return make_fixnum(100);
    };
    scm_obj_t p = make_closure((void*)*+(callcc_proc), 0, 1, 0, nullptr, scm_nil, 1);
    return subr_call_cc(scm_undef, p);
  };

  scm_obj_t pre = make_closure((void*)subr_pre, 0, 0, 0, nullptr, scm_nil, 1);
  scm_obj_t value = make_closure((void*)*+(value_proc), 0, 0, 0, nullptr, scm_nil, 1);
  scm_obj_t post = make_closure((void*)subr_post, 0, 0, 0, nullptr, scm_nil, 1);

  scm_obj_t result = subr_dynamic_wind(scm_undef, pre, value, post);

  if (result == make_fixnum(100)) {
    if (pre_count == 1 && post_count == 1) {
      printf("Initial dynamic-wind finished. Invoking continuation.\n");
      scm_obj_t args[] = {make_cons(make_fixnum(200), scm_nil)};
      c_apply_helper(captured_cont, 1, args);
      // Unreachable!
      fatal("unreachable");
    }
  } else if (result == make_fixnum(200)) {
    if (pre_count == 2 && post_count == 2) {
      printf("\033[32mdynamic-wind with call/cc passed\033[0m\n");
      return true;
    }
  }

  printf("\033[31m###### dynamic-wind with call/cc failed (pre=%d, post=%d, result=0x%lx)\033[0m\n", pre_count, post_count,
         (unsigned long)result);
  some_test_failed = true;
  return false;
}

#ifdef __has_feature
  #if __has_feature(hwaddress_sanitizer)
extern "C" const char* __hwasan_default_options() { return "leak_check_at_exit=0"; }
  #endif
#endif

int main() {
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 64, 1024 * 1024 * 16);

  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  auto jit_expected = nanos_jit_t::Create();
  if (!jit_expected) exit(1);
  auto jit = std::move(*jit_expected);
  auto ts_ctx = std::make_unique<llvm::LLVMContext>();
  codegen_t* cg = new codegen_t(llvm::orc::ThreadSafeContext(std::move(ts_ctx)), jit.get());

  test_basic_dynamic_wind();
  test_dynamic_wind_with_call_cc();

  delete cg;
  heap->destroy();
  delete heap;

  puts("all test done");
  return some_test_failed ? 1 : 0;
}
