#include "core.h"

#include "object.h"
#include "object_heap.h"

static bool some_test_failed = false;

static bool test_fixnum_in_range(int64_t i64) {
  int64_t x1 = i64;
  scm_obj_t x2 = make_fixnum(x1);
  int64_t x3 = fixnum(x2);
  if (x1 != x3) {
    printf("\033[31m###### fixnum failed: %ld != %ld\033[0m\n", x1, x3);
    some_test_failed = true;
    return false;
  }
  printf("\033[32mfixnum passed: %ld\033[0m\n", x1);
  return true;
}

static bool test_flonum(double d) {
  double x4 = d;
  scm_obj_t x5 = make_flonum(x4);
  double x6 = flonum(x5);
  if (x4 != x6) {
    printf("\033[31m###### flonum failed: %.17e != %.17e\033[0m\n", x4, x6);
    some_test_failed = true;
    return false;
  }
  printf("\033[32mflonum passed: %.17e (%s)\033[0m\n", x4, is_short_flonum(x5) ? "short" : "long");
  return true;
}

int main(int argc, char** argv) {
  object_heap_t heap;
  heap.init(1024 * 1024, 1024 * 256);

  test_fixnum_in_range(-1);
  test_fixnum_in_range(0);
  test_fixnum_in_range(1);

  test_flonum(0.0);
  test_flonum(-0.0);
  test_flonum(1.3e20);
  test_flonum(-1.3e20);
  test_flonum(5.87747175411143884e-39);  // 0x3800000000000001 short
  test_flonum(6.8056473384187685e+38);   // 0x47ffffffffffffff short

  test_flonum(5.87747175411143754e-39);  // 0x3800000000000000 long
  test_flonum(5.87747175411143689e-39);  // 0x37ffffffffffffff long
  test_flonum(6.80564733841876927e+38);  // 0x4800000000000000 long
  test_flonum(6.80564733841877985e+38);  // 0x4800000000000007 long
  test_flonum(1.3e-100);
  test_flonum(1.3e+100);

  heap.destroy();

  return some_test_failed ? 1 : 0;
}

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
