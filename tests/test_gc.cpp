#include "../src/core.h"
#include "../src/object.h"
#include "../src/object_heap.h"

static bool some_test_failed = false;

static bool test_gc_allocation(int num_loops) {
  object_heap_t heap;

  heap.init(1024 * 256, 1024 * 64);

  // Allocate enough objects to potentially trigger GC or just verify allocation works
  for (int i = 0; i < num_loops; i++) {
    scm_obj_t obj = make_cons(make_symbol("foo"), make_list(3, make_cons(make_fixnum(1), make_fixnum(2)),
                                                            make_cons(make_symbol("bar"), make_string("baz")), make_u8vector(122)));
    usleep(1);
    heap.safepoint();
    if (!is_cons(obj)) {
      printf("\033[31m###### allocation failed at %d\033[0m\n", i);
      some_test_failed = true;
      return false;
    }
  }

  heap.destroy();

  printf("\033[32mtest_gc_allocation passed\033[0m\n");
  return true;
}

int main(int argc, char** argv) {
  int num_loops = 10000;
  if (argc > 1) {
    num_loops = atoi(argv[1]);
  }
  test_gc_allocation(num_loops);
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
