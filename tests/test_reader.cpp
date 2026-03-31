#include <cassert>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <sstream>
#include "../src/object.h"
#include "../src/object_heap.h"
#include "../src/environment.h"
#include "../src/printer.h"
#include "../src/reader.h"

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

void test_reader() {
  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    environment::init();
    std::stringstream ss("123");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for 123");
    assert(is_fixnum(obj));
    assert(fixnum(obj) == 123);
    std::cout << "Fixnum 123 passed" << std::endl;
    environment::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    environment::init();
    std::stringstream ss("-123");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for -123");
    assert(is_fixnum(obj));
    assert(fixnum(obj) == -123);
    std::cout << "Fixnum -123 passed" << std::endl;
    environment::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    environment::init();
    std::stringstream ss("foo");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for foo");
    assert(is_symbol(obj));
    assert(std::string((char*)symbol_name(obj)) == "foo");
    std::cout << "Symbol foo passed" << std::endl;
    environment::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    environment::init();
    std::stringstream ss("|foo bar|");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for |foo bar|");
    assert(is_symbol(obj));
    assert(std::string((char*)symbol_name(obj)) == "foo bar");
    std::cout << "Symbol |foo bar| passed" << std::endl;
    environment::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    environment::init();
    std::stringstream ss("\"hello\"");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for \"hello\"");
    assert(is_string(obj));
    assert(std::string((char*)string_name(obj)) == "hello");
    std::cout << "String \"hello\" passed" << std::endl;
    environment::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    environment::init();
    std::stringstream ss("(1 2 3)");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for (1 2 3)");
    assert(is_cons(obj));
    assert(fixnum(((scm_cons_rec_t*)obj)->car) == 1);

    obj = ((scm_cons_rec_t*)obj)->cdr;
    assert(is_cons(obj));
    assert(fixnum(((scm_cons_rec_t*)obj)->car) == 2);

    obj = ((scm_cons_rec_t*)obj)->cdr;
    assert(is_cons(obj));
    assert(fixnum(((scm_cons_rec_t*)obj)->car) == 3);

    obj = ((scm_cons_rec_t*)obj)->cdr;
    assert(obj == scm_nil);
    std::cout << "List (1 2 3) passed" << std::endl;
    environment::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    environment::init();
    std::stringstream ss("(1 . 2)");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for (1 . 2)");
    assert(is_cons(obj));
    assert(fixnum(((scm_cons_rec_t*)obj)->car) == 1);
    assert(fixnum(((scm_cons_rec_t*)obj)->cdr) == 2);
    std::cout << "List (1 . 2) passed" << std::endl;
    environment::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    environment::init();
    std::stringstream ss("#(1 2)");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for #(1 2)");
    assert(is_vector(obj));
    assert(vector_nsize(obj) == 2);
    assert(fixnum(vector_elts(obj)[0]) == 1);
    assert(fixnum(vector_elts(obj)[1]) == 2);
    std::cout << "Vector #(1 2) passed" << std::endl;
    environment::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    environment::init();
    std::stringstream ss("#u8(1 255)");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for #u8(1 255)");
    assert(is_u8vector(obj));
    assert(u8vector_nsize(obj) == 2);
    assert(u8vector_elts(obj)[0] == 1);
    assert(u8vector_elts(obj)[1] == 255);
    std::cout << "Bytevector #u8(1 255) passed" << std::endl;
    environment::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    environment::init();
    std::stringstream ss("'foo");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for 'foo");
    assert(is_cons(obj));
    // (quote foo)
    scm_obj_t car = ((scm_cons_rec_t*)obj)->car;
    assert(is_symbol(car));
    assert(std::string((char*)symbol_name(car)) == "quote");
    std::cout << "Quote 'foo passed" << std::endl;
    environment::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    environment::init();
    std::stringstream ss("\"\\x3b1;\"");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for \"\\x3b1;\"");
    assert(is_string(obj));
    // alpha is 0xCE 0xB1
    const uint8_t* str = string_name(obj);
    assert(str[0] == 0xCE);
    assert(str[1] == 0xB1);
    assert(str[2] == 0);
    std::cout << "String \"\\x3b1;\" (alpha) passed" << std::endl;
    environment::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    environment::init();
    std::stringstream ss("\"\\x1f600;\"");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for \"\\x1f600;\"");
    assert(is_string(obj));
    // grinning face is 0xF0 0x9F 0x98 0x80
    const uint8_t* str = string_name(obj);
    assert(str[0] == 0xF0);
    assert(str[1] == 0x9F);
    assert(str[2] == 0x98);
    assert(str[3] == 0x80);
    assert(str[4] == 0);
    std::cout << "String \"\\x1f600;\" (grinning face) passed" << std::endl;
    environment::destroy();
    heap->destroy();
    delete heap;
  }

  // Test #i prefix (inexact)
  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    environment::init();
    std::stringstream ss("#i42");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for #i42: %s", reader.get_error_message().c_str());
    assert(is_short_flonum(obj) || is_long_flonum(obj));
    assert(flonum(obj) == 42.0);
    std::cout << "Inexact #i42 passed" << std::endl;
    environment::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    environment::init();
    std::stringstream ss("#i#x1A");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for #i#x1A: %s", reader.get_error_message().c_str());
    assert(is_short_flonum(obj) || is_long_flonum(obj));
    assert(flonum(obj) == 26.0);
    std::cout << "Inexact #i#x1A passed" << std::endl;
    environment::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    environment::init();
    std::stringstream ss("#x#i10");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for #x#i10: %s", reader.get_error_message().c_str());
    assert(is_short_flonum(obj) || is_long_flonum(obj));
    assert(flonum(obj) == 16.0);
    std::cout << "Inexact #x#i10 passed" << std::endl;
    environment::destroy();
    heap->destroy();
    delete heap;
  }

  // Test square brackets
  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    environment::init();
    std::stringstream ss("[1 2 3]");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for [1 2 3]");
    assert(is_cons(obj));
    assert(fixnum(((scm_cons_rec_t*)obj)->car) == 1);
    std::cout << "List [1 2 3] passed" << std::endl;
    environment::destroy();
    heap->destroy();
    delete heap;
  }

  // Mixed pairing test
  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    environment::init();
    std::stringstream ss("(1 [2 3] 4)");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for (1 [2 3] 4)");
    assert(is_cons(obj));
    std::cout << "Mixed paired list (1 [2 3] 4) passed" << std::endl;
    environment::destroy();
    heap->destroy();
    delete heap;
  }

  // Vector with square brackets
  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    environment::init();
    std::stringstream ss("#[1 2]");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for #[1 2]");
    assert(is_vector(obj));
    assert(vector_nsize(obj) == 2);
    std::cout << "Vector #[1 2] passed" << std::endl;
    environment::destroy();
    heap->destroy();
    delete heap;
  }
}

void test_reader_errors() {
  auto check_error = [](const char* input, const char* expected_msg) {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    environment::init();
    std::stringstream ss(input);
    reader_t reader(ss);
    bool err = false;
    reader.read(err);
    if (!err) {
      fatal("expected error for input: %s", input);
    }
    std::string msg = reader.get_error_message();
    if (msg != expected_msg) {
      fatal("expected error message '%s', got '%s' for input: %s", expected_msg, msg.c_str(), input);
    }
    std::cout << "Error case passed: " << input << " -> " << msg << std::endl;
    environment::destroy();
    heap->destroy();
    delete heap;
  };

  check_error("(1 2", "unexpected end-of-file while reading list");
  check_error("\"foo", "unexpected end-of-file while reading string");
  check_error("(1 . 2 3)", "more than one item following dot('.') while reading list or mismatched parentheses");
  check_error("#u8(256)", "u8vector element out of range");
  check_error("#u8(a)", "expected fixnum in u8vector");
  check_error("\"\\xZZ;\"", "inline hex escape missing terminating semi-colon");
  check_error("#\\unknown", "invalid lexical syntax");
  check_error("[1 2 3)", "mismatched parentheses");
  check_error("(1 2 3]", "mismatched parentheses");
  check_error("]", "unexpected closing bracket");
  check_error(")", "unexpected closing bracket");
  check_error("(1 . 2]", "more than one item following dot('.') while reading list or mismatched parentheses");
}

void test_r6rs_abbreviations() {
  auto check_abbrev = [](const char* input, const char* expected_symbol) {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    environment::init();
    std::stringstream ss(input);
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for %s: %s", input, reader.get_error_message().c_str());
    assert(is_cons(obj));
    scm_obj_t car = ((scm_cons_rec_t*)obj)->car;
    assert(is_symbol(car));
    assert(std::string((char*)symbol_name(car)) == expected_symbol);
    std::cout << "Abbreviated " << input << " -> (" << expected_symbol << " ...) passed" << std::endl;
    environment::destroy();
    heap->destroy();
    delete heap;
  };

  check_abbrev("#'x", "syntax");
  check_abbrev("#`x", "quasisyntax");
  check_abbrev("#,x", "unsyntax");
  check_abbrev("#,@x", "unsyntax-splicing");
  check_abbrev("#`(a #,b #,@c)", "quasisyntax");
}

int main() {
  test_reader();
  test_reader_errors();
  test_r6rs_abbreviations();
  return 0;
}
