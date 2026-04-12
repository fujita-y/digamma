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

void test_printer() {
  std::stringstream ss;
  printer_t printer(ss);

  // Initializing heap for object creation
  object_heap_t* heap = new object_heap_t();
  heap->init(4 * 1024 * 1024, 128 * 1024);
  context::init();

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

  // Test flonum
  ss.str("");
  printer.write(make_flonum(1.25));
  assert(ss.str() == "1.25");
  std::cout << "Flonum test passed" << std::endl;

  // Test char
  ss.str("");
  printer.write(make_char('a'));
  assert(ss.str() == "#\\a");
  ss.str("");
  printer.write(make_char(' '));
  assert(ss.str() == "#\\space");
  ss.str("");
  printer.write(make_char('\n'));
  assert(ss.str() == "#\\newline");
  std::cout << "Char test passed" << std::endl;

  // Test special constants
  ss.str("");
  printer.write(scm_undef);
  assert(ss.str() == "#<undef>");
  ss.str("");
  printer.write(scm_unspecified);
  assert(ss.str() == "#<unspecified>");
  ss.str("");
  printer.write(scm_eof);
  assert(ss.str() == "#<eof>");
  std::cout << "Special constants test passed" << std::endl;

  // Test opaque types (just check they print something sensible)
  ss.str("");
  printer.write(make_cell(make_fixnum(42)));
  assert(ss.str().find("#<cell") == 0);
  
  ss.str("");
  printer.write(make_u8vector(0));
  assert(ss.str() == "#u8()");
  
  ss.str("");
  printer.write(make_values(0));
  assert(ss.str() == "#<values>");

  std::cout << "Opaque types test passed" << std::endl;

  context::destroy();
  heap->destroy();
  delete heap;
}

void test_format() {
  std::cout << "Starting format tests..." << std::endl;
  std::stringstream ss;
  printer_t printer(ss);

  // Initializing heap for object creation
  object_heap_t* heap = new object_heap_t();
  heap->init(4 * 1024 * 1024, 128 * 1024);
  context::init();

  auto run_test = [&](const char* fmt_str, int argc, scm_obj_t* argv, const char* expected) {
    ss.str("");
    printer.format(argc, argv);
    if (ss.str() != expected) {
      std::cerr << "Format test failed for \"" << fmt_str << "\"" << std::endl;
      std::cerr << "Expected: \"" << expected << "\"" << std::endl;
      std::cerr << "Actual:   \"" << ss.str() << "\"" << std::endl;
      assert(false);
    }
  };

  // (format "~8,2f ~a" 6.0 'hello) ;=> "    6.00 hello"
  {
    scm_obj_t args[3];
    args[0] = make_string("~8,2f ~a");
    args[1] = make_flonum(6.0);
    args[2] = make_symbol("hello");
    run_test("~8,2f ~a", 3, args, "    6.00 hello");
    std::cout << "Test 1 passed" << std::endl;
  }

  // (format "~8,4f ~a" 6.0 'hello) ;=> "  6.0000 hello"
  {
    scm_obj_t args[3];
    args[0] = make_string("~8,4f ~a");
    args[1] = make_flonum(6.0);
    args[2] = make_symbol("hello");
    run_test("~8,4f ~a", 3, args, "  6.0000 hello");
    std::cout << "Test 2 passed" << std::endl;
  }

  // (format "~8,f ~a" 6.0 'hello)  ;=> "      6. hello"
  {
    scm_obj_t args[3];
    args[0] = make_string("~8,f ~a");
    args[1] = make_flonum(6.0);
    args[2] = make_symbol("hello");
    run_test("~8,f ~a", 3, args, "      6. hello");
    std::cout << "Test 3 passed" << std::endl;
  }

  // (format "~0,0f ~a" 6.0 'hello) ;=> "6. hello"
  {
    scm_obj_t args[3];
    args[0] = make_string("~0,0f ~a");
    args[1] = make_flonum(6.0);
    args[2] = make_symbol("hello");
    run_test("~0,0f ~a", 3, args, "6. hello");
    std::cout << "Test 4 passed" << std::endl;
  }

  // (format "~s ~a ~~ ~%" 'foo "bar") ;=> "foo bar ~ \n"
  {
    scm_obj_t args[3];
    args[0] = make_string("~s ~a ~~ ~%");
    args[1] = make_symbol("foo");
    args[2] = make_string("bar");
    run_test("~s ~a ~~ ~%", 3, args, "foo bar ~ \n");
    std::cout << "Test 5 passed" << std::endl;
  }

  std::cout << "Format tests passed" << std::endl;

  context::destroy();
  heap->destroy();
  delete heap;
}

void test_display() {
  std::cout << "Starting display tests..." << std::endl;
  std::stringstream ss;
  printer_t printer(ss);

  object_heap_t* heap = new object_heap_t();
  heap->init(4 * 1024 * 1024, 128 * 1024);
  context::init();

  // String
  ss.str("");
  printer.display(make_string("hello"));
  assert(ss.str() == "hello");

  // Char
  ss.str("");
  printer.display(make_char('a'));
  assert(ss.str() == "a");

  // Symbol
  ss.str("");
  printer.display(make_symbol("foo"));
  assert(ss.str() == "foo");

  std::cout << "Display tests passed" << std::endl;

  context::destroy();
  heap->destroy();
  delete heap;
}

void test_write_ss() {
  std::cout << "Starting write_ss tests..." << std::endl;
  std::stringstream ss;
  printer_t printer(ss);

  object_heap_t* heap = new object_heap_t();
  heap->init(4 * 1024 * 1024, 128 * 1024);
  context::init();

  // Shared
  {
    ss.str("");
    scm_obj_t p = make_cons(make_fixnum(1), scm_nil);
    scm_obj_t list = make_cons(p, p);
    printer.write_ss(list);
    assert(ss.str() == "(#0=(1) . #0#)");
  }

  // Circular
  {
    ss.str("");
    scm_obj_t p = make_cons(make_fixnum(1), scm_nil);
    ((scm_cons_rec_t*)p)->cdr = p;
    printer.write_ss(p);
    assert(ss.str() == "#0=(1 . #0#)");
  }

  std::cout << "write_ss tests passed" << std::endl;

  context::destroy();
  heap->destroy();
  delete heap;
}

int main() {
  test_printer();
  test_format();
  test_display();
  test_write_ss();
  return 0;
}
