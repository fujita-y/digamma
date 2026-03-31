#include <cassert>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
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

#define CHECK(cond)                                                     \
  do {                                                                  \
    if (!(cond)) fatal("Check failed: %s at line %d", #cond, __LINE__); \
  } while (0)

void test_r7rs_comments() {
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024, 64 * 1024);
  environment::init();

  {
    // Block comment
    std::stringstream ss("#| this is a comment |# 123");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for block comment");
    CHECK(is_fixnum(obj));
    CHECK(fixnum(obj) == 123);
    std::cout << "Block comment passed" << std::endl;
  }

  {
    // Nested block comment
    std::stringstream ss("#| this #| is |# nested |# 456");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for nested block comment");
    CHECK(is_fixnum(obj));
    CHECK(fixnum(obj) == 456);
    std::cout << "Nested block comment passed" << std::endl;
  }

  {
    // Datum comment
    std::stringstream ss("#; (ignore this) 789");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for datum comment");
    if (!is_fixnum(obj)) {
      std::cerr << "Simple datum comment failed. Got type=";
      if (is_symbol(obj))
        std::cerr << "symbol " << (char*)symbol_name(obj);
      else if (is_string(obj))
        std::cerr << "string " << (char*)string_name(obj);
      else if (is_cons(obj))
        std::cerr << "cons";
      else if (obj == scm_eof)
        std::cerr << "eof";
      else
        std::cerr << "other " << std::hex << obj;
      std::cerr << std::endl;
    }
    CHECK(is_fixnum(obj));
    CHECK(fixnum(obj) == 789);
    std::cout << "Datum comment passed" << std::endl;
  }

  {
    // Datum comment nested
    std::stringstream ss("#; #; ignore ignore 101");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for nested datum comment");
    if (!is_fixnum(obj)) {
      std::cerr << "Expected fixnum 101, got type=";
      if (is_symbol(obj))
        std::cerr << "symbol " << (char*)symbol_name(obj);
      else if (is_string(obj))
        std::cerr << "string " << (char*)string_name(obj);
      else if (obj == scm_eof)
        std::cerr << "eof";
      else
        std::cerr << "other " << std::hex << obj;
      std::cerr << std::endl;
    }
    CHECK(is_fixnum(obj));
    CHECK(fixnum(obj) == 101);
    std::cout << "Nested datum comment passed" << std::endl;
  }

  environment::destroy();
  heap->destroy();
  delete heap;
}

void test_r7rs_strings() {
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024, 64 * 1024);
  environment::init();

  {
    // String escape input
    std::stringstream ss("\"foo \\\"bar\\\" baz\"");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for string escape");
    CHECK(is_string(obj));
    CHECK(strcmp((char*)string_name(obj), "foo \"bar\" baz") == 0);
    std::cout << "String escape input passed" << std::endl;
  }

  {
    // String escape output
    std::stringstream ss;
    printer_t printer(ss);
    scm_obj_t str = make_string("foo \"bar\" baz");
    printer.write(str);
    CHECK(ss.str() == "\"foo \\\"bar\\\" baz\"");
    std::cout << "String escape output passed" << std::endl;
  }

  environment::destroy();
  heap->destroy();
  delete heap;
}

void test_r7rs_symbols() {
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024, 64 * 1024);
  environment::init();

  {
    // Symbol escape output
    std::stringstream ss;
    printer_t printer(ss);
    scm_obj_t sym = make_symbol("foo bar");
    printer.write(sym);
    CHECK(ss.str() == "|foo bar|");
    std::cout << "Symbol escape output passed" << std::endl;
  }

  {
    // Symbol pipe escape output
    std::stringstream ss;
    printer_t printer(ss);
    scm_obj_t sym = make_symbol("foo|bar");
    printer.write(sym);
    CHECK(ss.str() == "|foo\\|bar|");  // expecting |foo\|bar|
    std::cout << "Symbol pipe escape output passed" << std::endl;
  }

  environment::destroy();
  heap->destroy();
  delete heap;
}

int main() {
  test_r7rs_comments();
  test_r7rs_strings();
  test_r7rs_symbols();
  return 0;
}
