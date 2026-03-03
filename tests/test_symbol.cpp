#include "../src/core.h"
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

static bool test_symbol_intern(const char* name) {
  scm_obj_t x1 = make_symbol(name);
  scm_obj_t x2 = make_symbol(name);
  if (x1 != x2) {
    printf("\033[31m###### symbol intern failed: %s (0x%lx != 0x%lx)\033[0m\n", name, x1, x2);
    some_test_failed = true;
    return false;
  }
  printf("\033[32msymbol intern passed: %s\033[0m\n", name);
  return true;
}

int main(int argc, char** argv) {
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 2, 1024 * 1024);
  heap->m_collect_trip_bytes = 1024 * 512;

  test_symbol_intern("foobar");
  test_symbol_intern("hogehoge");
  test_symbol_intern("abc");
  test_symbol_intern("");
  test_symbol_intern("white space");
  test_symbol_intern("special-chars!@#$%^&*()");
  test_symbol_intern("12345");

  heap->destroy();
  delete heap;

  return some_test_failed ? 1 : 0;
}
