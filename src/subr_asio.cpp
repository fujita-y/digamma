// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "context.h"
#include "port.h"
#include "subr.h"

SUBR subr_put_string_async(scm_obj_t self, scm_obj_t port, scm_obj_t str) {
  if (!is_port(port)) throw std::runtime_error("put-string-async: first argument must be a port");
  if (!is_output_port(port)) throw std::runtime_error("put-string-async: first argument must be an output port");
  if (!is_string(str)) throw std::runtime_error("put-string-async: second argument must be a string");
  return port_put_string_async(port, str);
}

SUBR subr_get_bytevector_n_async(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  scm_obj_t port = a1;
  scm_obj_t n = a2;
  if (!is_port(port)) throw std::runtime_error("get-bytevector-n-async: first argument must be a port");
  if (!is_input_port(port)) throw std::runtime_error("get-bytevector-n-async: first argument must be an input port");
  if (!is_fixnum(n)) throw std::runtime_error("get-bytevector-n-async: second argument must be a fixnum");
  int count = fixnum(n);
  if (count < 0) throw std::runtime_error("get-bytevector-n-async: count must be non-negative");
  return port_get_bytes_n_async(port, count);
}

void init_subr_asio() {
  auto reg = [](const char* name, void* func, int req, bool opt) {
    context::environment_variable_set(make_symbol(name), make_closure(func, req, opt ? 1 : 0, 0, nullptr, 1));
  };

  reg("put-string-async", (void*)subr_put_string_async, 2, false);
  reg("get-bytevector-n-async", (void*)subr_get_bytevector_n_async, 2, false);
}
