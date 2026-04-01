#include "../src/core.h"
#include "../src/object.h"
#include "../src/object_heap.h"
#include "../src/context.h"
#include "../src/port.h"
#include <iostream>
#include <sstream>
#include <cstring>
#include <cstdarg>

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

static bool test_standard_ports() {
  scm_obj_t sin = context::s_standard_input_port;
  scm_obj_t sout = context::s_standard_output_port;
  scm_obj_t serr = context::s_standard_error_port;

  if (!is_port(sin) || !is_port(sout) || !is_port(serr)) {
    printf("\033[31m###### standard ports failed: not a port\033[0m\n");
    some_test_failed = true;
    return false;
  }

  if (port_get_istream(sin) != &std::cin) {
    printf("\033[31m###### standard input port failed: stream mismatch\033[0m\n");
    some_test_failed = true;
    return false;
  }

  if (port_get_ostream(sout) != &std::cout) {
    printf("\033[31m###### standard output port failed: stream mismatch\033[0m\n");
    some_test_failed = true;
    return false;
  }

  if (port_get_ostream(serr) != &std::cerr) {
    printf("\033[31m###### standard error port failed: stream mismatch\033[0m\n");
    some_test_failed = true;
    return false;
  }

  printf("\033[32mstandard ports passed\033[0m\n");
  return true;
}

static bool test_string_port() {
  scm_obj_t port = make_port(make_symbol("string-port"));
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  
  std::stringstream* ss = new std::stringstream();
  rec->aux->stream = ss;
  rec->aux->owned = true;

  if (port_get_ostream(port) != ss) {
    printf("\033[31m###### string port ostream failed\033[0m\n");
    some_test_failed = true;
    return false;
  }

  if (port_get_istream(port) != ss) {
    printf("\033[31m###### string port istream failed\033[0m\n");
    some_test_failed = true;
    return false;
  }

  *ss << "hello";
  char buf[6];
  port_get_istream(port)->read(buf, 5);
  buf[5] = '\0';
  if (strcmp(buf, "hello") != 0) {
    printf("\033[31m###### string port read failed: %s\033[0m\n", buf);
    some_test_failed = true;
    return false;
  }

  // port_finalize is called by heap destroy or manually later
  port_finalize((scm_port_rec_t*)to_address(port));

  printf("\033[32mstring port passed\033[0m\n");
  return true;
}

int main(int argc, char** argv) {
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 2, 1024 * 1024);
  context::init();

  test_standard_ports();
  test_string_port();

  context::destroy();
  heap->destroy();
  delete heap;

  return some_test_failed ? 1 : 0;
}
