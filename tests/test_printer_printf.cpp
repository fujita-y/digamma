#include <cassert>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <sstream>
#include "../src/object.h"
#include "../src/object_heap.h"
#include "../src/context.h"
#include "../src/printer.h"

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

void test_printf_basic() {
  std::stringstream ss;
  printer_t printer(ss);

  // Test basic string
  ss.str("");
  printer.format("hello");
  assert(ss.str() == "hello");
  std::cout << "Basic string test passed" << std::endl;

  // Test %% escape
  ss.str("");
  printer.format("100%% done");
  assert(ss.str() == "100% done");
  std::cout << "%% escape test passed" << std::endl;

  // Test %d
  ss.str("");
  printer.format("number: %d", 42);
  assert(ss.str() == "number: 42");
  std::cout << "%d test passed" << std::endl;

  // Test %s
  ss.str("");
  printer.format("hello %s", "world");
  assert(ss.str() == "hello world");
  std::cout << "%s test passed" << std::endl;

  // Test %f
  ss.str("");
  printer.format("pi: %.2f", 3.14159);
  assert(ss.str() == "pi: 3.14");
  std::cout << "%f test passed" << std::endl;
}

void test_printf_with_scheme_objects() {
  std::stringstream ss;
  printer_t printer(ss);

  // Initializing heap for object creation
  object_heap_t* heap = new object_heap_t();
  heap->init(4 * 1024 * 1024, 128 * 1024);
  context::init();

  // Test %w with fixnum
  ss.str("");
  printer.format("value: %w", make_fixnum(123));
  assert(ss.str() == "value: 123");
  std::cout << "%w with fixnum test passed" << std::endl;

  // Test %w with boolean
  ss.str("");
  printer.format("bool: %w", scm_true);
  assert(ss.str() == "bool: #t");
  std::cout << "%w with boolean test passed" << std::endl;

  // Test %w with string
  ss.str("");
  printer.format("str: %w", make_string("hello"));
  assert(ss.str() == "str: \"hello\"");
  std::cout << "%w with string test passed" << std::endl;

  // Test %w with symbol
  ss.str("");
  printer.format("sym: %w", make_symbol("foo"));
  assert(ss.str() == "sym: foo");
  std::cout << "%w with symbol test passed" << std::endl;

  // Test %w with list
  ss.str("");
  scm_obj_t list = make_list(3, make_fixnum(1), make_fixnum(2), make_fixnum(3));
  printer.format("list: %w", list);
  assert(ss.str() == "list: (1 2 3)");
  std::cout << "%w with list test passed" << std::endl;

  // Test multiple %w
  ss.str("");
  printer.format("%w + %w = %w", make_fixnum(1), make_fixnum(2), make_fixnum(3));
  assert(ss.str() == "1 + 2 = 3");
  std::cout << "Multiple %w test passed" << std::endl;

  // Test mixing %w with standard directives
  ss.str("");
  printer.format("The answer is %w which is %d in decimal", make_fixnum(42), 42);
  assert(ss.str() == "The answer is 42 which is 42 in decimal");
  std::cout << "Mixed directives test passed" << std::endl;

  context::destroy();
  heap->destroy();
  delete heap;
}

int main() {
  test_printf_basic();
  test_printf_with_scheme_objects();
  std::cout << "\nAll tests passed!" << std::endl;
  return 0;
}
