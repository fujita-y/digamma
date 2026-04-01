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

void test_radix() {
  auto check_num = [](const char* input, intptr_t expected_val) {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss(input);
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for %s", input);
    if (!is_fixnum(obj)) fatal("expected fixnum for %s", input);
    if (fixnum(obj) != expected_val) fatal("expected %ld, got %ld for %s", expected_val, fixnum(obj), input);
    std::cout << "Passed: " << input << " -> " << fixnum(obj) << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  };

  // Hex
  check_num("#x10", 16);
  check_num("#xff", 255);
  check_num("#xFF", 255);
  check_num("#x-10", -16);

  // Binary
  check_num("#b101", 5);
  check_num("#b-10", -2);

  // Octal
  check_num("#o77", 63);
  check_num("#o-10", -8);

  // Decimal
  check_num("#d10", 10);
  check_num("#d-10", -10);
}

void test_exactness_error() {
  auto check_error = [](const char* input, const char* expected_msg) {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss(input);
    reader_t reader(ss);
    bool err = false;
    reader.read(err);
    if (!err) {
      fatal("expected error for input: %s", input);
    }
    std::string msg = reader.get_error_message();
    if (msg.find(expected_msg) == std::string::npos) {
      fatal("expected error message containing '%s', got '%s' for input: %s", expected_msg, msg.c_str(), input);
    }
    std::cout << "Error case passed: " << input << " -> " << msg << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  };

  check_error("#e10", "exactness prefix #e not supported");
}

int main() {
  test_radix();
  test_exactness_error();
  return 0;
}
