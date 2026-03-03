#include <cassert>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <sstream>
#include "../src/object.h"
#include "../src/object_heap.h"
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

void test_printer() {
  std::stringstream ss;
  printer_t printer(ss);

  // Initializing heap for object creation
  object_heap_t* heap = new object_heap_t();
  heap->init(4 * 1024 * 1024, 128 * 1024);

  // Test fixnum
  ss.str("");
  printer.write(make_fixnum(123));
  assert(ss.str() == "123");
  std::cout << "Fixnum test passed" << std::endl;

  // Test simple boolean
  ss.str("");
  printer.write(scm_true);
  assert(ss.str() == "#t");
  std::cout << "Boolean #t test passed" << std::endl;

  ss.str("");
  printer.write(scm_false);
  assert(ss.str() == "#f");
  std::cout << "Boolean #f test passed" << std::endl;

  // Test nil
  ss.str("");
  printer.write(scm_nil);
  assert(ss.str() == "()");
  std::cout << "Nil test passed" << std::endl;

  // Test string
  ss.str("");
  printer.write(make_string("hello"));
  assert(ss.str() == "\"hello\"");
  std::cout << "String test passed" << std::endl;

  // Test symbol
  ss.str("");
  printer.write(make_symbol("foo"));
  assert(ss.str() == "foo");
  std::cout << "Symbol test passed" << std::endl;

  // Test list (1 2 3)
  ss.str("");
  scm_obj_t list = make_list(3, make_fixnum(1), make_fixnum(2), make_fixnum(3));
  printer.write(list);
  assert(ss.str() == "(1 2 3)");
  std::cout << "List test passed" << std::endl;

  // Test improper list (1 . 2)
  ss.str("");
  scm_obj_t pair = make_cons(make_fixnum(1), make_fixnum(2));
  printer.write(pair);
  assert(ss.str() == "(1 . 2)");
  std::cout << "Improper list test passed" << std::endl;

  // Test vector #(1 2)
  ss.str("");
  scm_obj_t vec = make_vector(2, scm_nil);
  ((scm_vector_rec_t*)to_address(vec))->elts[0] = make_fixnum(1);
  ((scm_vector_rec_t*)to_address(vec))->elts[1] = make_fixnum(2);
  printer.write(vec);
  assert(ss.str() == "#(1 2)");
  std::cout << "Vector test passed" << std::endl;

  heap->destroy();
  delete heap;
}

int main() {
  test_printer();
  return 0;
}
