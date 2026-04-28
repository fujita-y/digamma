// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "context.h"
#include "nanos.h"
#include "object_heap.h"
#include "port.h"
#include "printer.h"
#include "reader.h"
#include "subr.h"
#include "utf8.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <sstream>
#include <stdexcept>
#include <unistd.h>

// ============================================================================
// Ports
// ============================================================================

SUBR subr_put_char(scm_obj_t self, scm_obj_t port, scm_obj_t ch) {
  if (!is_port(port)) throw std::runtime_error("put-char: first argument must be a port");
  if (!is_output_port(port)) throw std::runtime_error("put-char: first argument must be an output port");
  if (!is_char(ch)) throw std::runtime_error("put-char: second argument must be a character");
  return port_put_char(port, ch);
}

SUBR subr_put_string(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 2 || argc > 4) throw std::runtime_error("put-string: wrong number of arguments");
  scm_obj_t port = argv[0];
  scm_obj_t str = argv[1];
  if (!is_port(port)) throw std::runtime_error("put-string: first argument must be a port");
  if (!is_output_port(port)) throw std::runtime_error("put-string: first argument must be an output port");
  if (!is_string(str)) throw std::runtime_error("put-string: second argument must be a string");

  const uint8_t* s = (const uint8_t*)string_name(str);

  if (argc == 2) {
    return port_put_bytes(port, s, strlen((const char*)s));
  }

  int limit = strlen((const char*)s) + 1;
  int len = utf8_string_length(s);
  int start = 0;
  int count = len;

  if (argc >= 3) {
    if (!is_fixnum(argv[2])) throw std::runtime_error("put-string: start index must be an exact integer");
    start = fixnum(argv[2]);
    if (start > len) throw std::runtime_error("put-string: start index out of bounds");
    count = len - start;
  }
  if (argc == 4) {
    if (!is_fixnum(argv[3])) throw std::runtime_error("put-string: count must be an exact integer");
    count = fixnum(argv[3]);
    if (start + count > len) throw std::runtime_error("put-string: count out of bounds");
  }
  int byte_offset = utf8_char_index_to_byte_offset(s, start, limit);
  int byte_count = utf8_char_index_to_byte_offset(s + byte_offset, count, limit - byte_offset);

  assert(byte_offset >= 0 && byte_offset < limit);
  assert(byte_count >= 0 && byte_offset + byte_count < limit);
  return port_put_bytes(port, s + byte_offset, byte_count);
}

SUBR subr_current_input_port(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc == 0) return context::s_current_input_port;
  if (is_port(argv[0])) {
    if (!is_input_port(argv[0])) throw std::runtime_error("current-input-port: argument must be an input port");
    object_heap_t::current()->write_barrier(argv[0]);
    context::s_current_input_port = argv[0];
    return scm_unspecified;
  }
  throw std::runtime_error("current-input-port: argument must be a port");
}

SUBR subr_current_output_port(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc == 0) return context::s_current_output_port;
  if (is_port(argv[0])) {
    if (!is_output_port(argv[0])) throw std::runtime_error("current-output-port: argument must be an output port");
    object_heap_t::current()->write_barrier(argv[0]);
    context::s_current_output_port = argv[0];
    return scm_unspecified;
  }
  throw std::runtime_error("current-output-port: argument must be a port");
}

SUBR subr_current_error_port(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc == 0) return context::s_current_error_port;
  if (is_port(argv[0])) {
    if (!is_output_port(argv[0])) throw std::runtime_error("current-error-port: argument must be an output port");
    object_heap_t::current()->write_barrier(argv[0]);
    context::s_current_error_port = argv[0];
    return scm_unspecified;
  }
  throw std::runtime_error("current-error-port: argument must be a port");
}

SUBR subr_standard_input_port(scm_obj_t self) { return context::s_standard_input_port; }

SUBR subr_standard_output_port(scm_obj_t self) { return context::s_standard_output_port; }

SUBR subr_standard_error_port(scm_obj_t self) { return context::s_standard_error_port; }

// ============================================================================
// I/O  - R6RS 8
// ============================================================================

// write  - R6RS 8.3
SUBR subr_write(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1 || argc > 2) throw std::runtime_error("write: wrong number of arguments");
  scm_obj_t port = (argc == 2) ? argv[1] : context::s_current_output_port;
  if (!is_port(port)) throw std::runtime_error("write: argument must be a port");
  if (!is_output_port(port)) throw std::runtime_error("write: argument must be an output port");
  std::ostream* os = port_get_ostream(port);
  printer_t(*os).write(argv[0]);
  return scm_unspecified;
}

// write/ss - SRFI-38
SUBR subr_write_ss(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1 || argc > 2) throw std::runtime_error("write/ss: wrong number of arguments");
  scm_obj_t port = (argc == 2) ? argv[1] : context::s_current_output_port;
  if (!is_port(port)) throw std::runtime_error("write/ss: argument must be a port");
  if (!is_output_port(port)) throw std::runtime_error("write/ss: argument must be an output port");
  std::ostream* os = port_get_ostream(port);
  printer_t(*os).write_ss(argv[0]);
  return scm_unspecified;
}

// display  - R6RS 8.3
SUBR subr_display(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1 || argc > 2) throw std::runtime_error("display: wrong number of arguments");
  scm_obj_t port = (argc == 2) ? argv[1] : context::s_current_output_port;
  if (!is_port(port)) throw std::runtime_error("display: argument must be a port");
  if (!is_output_port(port)) throw std::runtime_error("display: argument must be an output port");
  std::ostream* os = port_get_ostream(port);
  printer_t(*os).display(argv[0]);
  return scm_unspecified;
}

// format - SRFI-28
SUBR subr_format(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1) throw std::runtime_error("format: too few arguments");
  if (!is_string(argv[0])) throw std::runtime_error("format: first argument must be a string");
  std::ostringstream oss;
  printer_t printer(oss);
  printer.format(argc, argv);
  return make_string(oss.str().c_str());
}

// newline  - R6RS 8.3
SUBR subr_newline(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc > 1) throw std::runtime_error("newline: wrong number of arguments");
  scm_obj_t port = (argc == 1) ? argv[0] : context::s_current_output_port;
  if (!is_port(port)) throw std::runtime_error("newline: argument must be a port");
  if (!is_output_port(port)) throw std::runtime_error("newline: argument must be an output port");
  std::ostream* os = port_get_ostream(port);
  *os << std::endl;
  return scm_unspecified;
}

// flush-output-port  - R6RS 8.2.11
SUBR subr_flush_output_port(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc == 0) return port_flush_output(context::s_current_output_port);
  if (argc == 1) {
    if (!is_port(argv[0])) throw std::runtime_error("flush-output-port: argument must be a port");
    if (!is_output_port(argv[0])) throw std::runtime_error("flush-output-port: argument must be an output port");
    return port_flush_output(argv[0]);
  }
  throw std::runtime_error("flush-output-port: wrong number of arguments");
}

// open-file-input-port  - R6RS 8.2.7
SUBR subr_open_file_input_port(scm_obj_t self, scm_obj_t a1) {
  if (!is_string(a1)) throw std::runtime_error("open-file-input-port: argument must be a string");
  return port_open_input_file((const char*)string_name(a1));
}

// open-file-output-port  - R6RS 8.2.10
SUBR subr_open_file_output_port(scm_obj_t self, scm_obj_t a1) {
  if (!is_string(a1)) throw std::runtime_error("open-file-output-port: argument must be a string");
  return port_open_output_file((const char*)string_name(a1));
}

// file-exists?  - R6RS 10
SUBR subr_file_exists_p(scm_obj_t self, scm_obj_t a1) {
  if (!is_string(a1)) throw std::runtime_error("file-exists?: argument must be a string");
  return (access((const char*)string_name(a1), F_OK) == 0) ? scm_true : scm_false;
}

SUBR thunk_string_output_port_extract(scm_obj_t self, int argc, scm_obj_t argv[]) {
  scm_closure_rec_t* closure = (scm_closure_rec_t*)to_address(self);
  scm_obj_t port = closure->env[0];
  return port_get_output_string(port);
}

SUBR subr_open_string_output_port(scm_obj_t self) {
  scm_obj_t port = port_open_string_output_port();
  scm_obj_t env[1] = {port};
  scm_obj_t proc = make_closure((void*)thunk_string_output_port_extract, 0, 0, 1, env, 1);
  scm_obj_t result = make_values(2);
  values_elts(result)[0] = port;
  values_elts(result)[1] = proc;
  return result;
}

// close-port  - R6RS 8.2.6
SUBR subr_close_port(scm_obj_t self, scm_obj_t a1) {
  if (!is_port(a1)) throw std::runtime_error("close-port: argument must be a port");
  port_close(a1);
  return scm_unspecified;
}

// eof-object?  - R6RS 8.2.1
SUBR subr_eof_object_p(scm_obj_t self, scm_obj_t a1) { return (a1 == scm_eof) ? scm_true : scm_false; }

// read  - R6RS 8.2.9
SUBR subr_read(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc > 1) throw std::runtime_error("read: wrong number of arguments");
  scm_obj_t port = (argc == 1) ? argv[0] : context::s_current_input_port;
  if (!is_port(port)) throw std::runtime_error("read: argument must be a port");
  if (!is_input_port(port)) throw std::runtime_error("read: argument must be an input port");
  std::istream* is = port_get_istream(port);

  reader_t reader(*is);
  bool err = false;
  scm_obj_t obj = reader.read(err);
  if (err) {
    throw std::runtime_error("read: " + reader.get_error_message());
  }
  return obj;
}

// get-bytevector-n - R6RS 8.2.9
SUBR subr_get_bytevector_n(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  scm_obj_t port = a1;
  scm_obj_t n = a2;
  if (!is_port(port)) throw std::runtime_error("get-bytevector-n: first argument must be a port");
  if (!is_input_port(port)) throw std::runtime_error("get-bytevector-n: first argument must be an input port");
  if (!is_fixnum(n)) throw std::runtime_error("get-bytevector-n: second argument must be a fixnum");
  int count = fixnum(n);
  if (count < 0) throw std::runtime_error("get-bytevector-n: count must be non-negative");
  return port_get_bytes_n(port, count);
}

void init_subr_io() {
  auto reg = [](const char* name, void* func, int req, bool opt) {
    context::environment_variable_set(make_symbol(name), make_closure(func, req, opt ? 1 : 0, 0, nullptr, 1));
  };

  reg("write", (void*)subr_write, 1, true);
  reg("write/ss", (void*)subr_write_ss, 1, true);
  reg("write-with-shared-structure", (void*)subr_write_ss, 1, true);
  reg("display", (void*)subr_display, 1, true);
  reg("newline", (void*)subr_newline, 0, true);
  reg("put-char", (void*)subr_put_char, 2, false);
  reg("put-string", (void*)subr_put_string, 2, true);
  reg("format", (void*)subr_format, 1, true);
  reg("flush-output-port", (void*)subr_flush_output_port, 0, true);
  reg("open-file-input-port", (void*)subr_open_file_input_port, 1, false);
  reg("open-file-output-port", (void*)subr_open_file_output_port, 1, false);
  reg("file-exists?", (void*)subr_file_exists_p, 1, false);
  reg("open-string-output-port", (void*)subr_open_string_output_port, 0, false);
  reg("close-port", (void*)subr_close_port, 1, false);
  reg("eof-object?", (void*)subr_eof_object_p, 1, false);
  reg("read", (void*)subr_read, 0, true);
  reg("get-bytevector-n", (void*)subr_get_bytevector_n, 2, false);
  reg("current-input-port", (void*)subr_current_input_port, 0, true);
  reg("current-output-port", (void*)subr_current_output_port, 0, true);
  reg("current-error-port", (void*)subr_current_error_port, 0, true);
  reg("standard-input-port", (void*)subr_standard_input_port, 0, false);
  reg("standard-output-port", (void*)subr_standard_output_port, 0, false);
  reg("standard-error-port", (void*)subr_standard_error_port, 0, false);
}
