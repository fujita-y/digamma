// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "context.h"
#include "subr.h"

#include <cstring>
#include <stdexcept>

// ============================================================================
// Bytevectors (u8vectors) - SRFI-4 / SRFI-66 / R6RS
// ============================================================================

// u8vector?  - SRFI-4
SUBR subr_u8vector_p(scm_obj_t self, scm_obj_t a1) { return is_u8vector(a1) ? scm_true : scm_false; }

// make-u8vector  - SRFI-4
SUBR subr_make_u8vector(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1 || argc > 2) throw std::runtime_error("make-u8vector: wrong number of arguments");
  if (!is_fixnum(argv[0])) throw std::runtime_error("make-u8vector: first argument must be an exact integer");
  intptr_t n = fixnum(argv[0]);
  if (n < 0) throw std::runtime_error("make-u8vector: length must be non-negative");
  scm_obj_t v = make_u8vector((int)n);
  if (argc == 2) {
    if (!is_fixnum(argv[1])) throw std::runtime_error("make-u8vector: fill value must be an exact integer");
    uint8_t fill = (uint8_t)fixnum(argv[1]);
    memset(u8vector_elts(v), fill, n);
  }
  return v;
}

// make-u8vector-mapping
SUBR subr_make_u8vector_mapping(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_fixnum(a1)) throw std::runtime_error("make-u8vector-mapping: first argument must be an exact integer");
  if (!is_fixnum(a2)) throw std::runtime_error("make-u8vector-mapping: second argument must be an exact integer");
  return make_u8vector_mapping((uint8_t*)fixnum(a1), (int)fixnum(a2));
}

// u8vector-length  - SRFI-4
SUBR subr_u8vector_length(scm_obj_t self, scm_obj_t a1) {
  if (is_u8vector(a1)) return make_fixnum(u8vector_nsize(a1));
  throw std::runtime_error("u8vector-length: argument must be a u8vector");
}

// u8vector-ref  - SRFI-4
SUBR subr_u8vector_ref(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_u8vector(a1)) throw std::runtime_error("u8vector-ref: first argument must be a u8vector");
  if (!is_fixnum(a2)) throw std::runtime_error("u8vector-ref: second argument must be an exact integer");
  intptr_t n = fixnum(a2);
  int sz = u8vector_nsize(a1);
  if (n < 0 || n >= sz) throw std::runtime_error("u8vector-ref: index out of bounds");
  return make_fixnum(u8vector_elts(a1)[n]);
}

// u8vector-uint32-ref
SUBR subr_u8vector_uint32_ref(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_u8vector(a1)) throw std::runtime_error("u8vector-uint32-ref: first argument must be a u8vector");
  if (!is_fixnum(a2)) throw std::runtime_error("u8vector-uint32-ref: second argument must be an exact integer");
  intptr_t n = fixnum(a2);
  int sz = u8vector_nsize(a1);
  if (n < 0 || n + 4 > sz) throw std::runtime_error("u8vector-uint32-ref: index out of bounds");
  uint32_t val;
  memcpy(&val, u8vector_elts(a1) + n, 4);
  return make_fixnum(val);
}

// u8vector-int32-ref
SUBR subr_u8vector_int32_ref(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_u8vector(a1)) throw std::runtime_error("u8vector-int32-ref: first argument must be a u8vector");
  if (!is_fixnum(a2)) throw std::runtime_error("u8vector-int32-ref: second argument must be an exact integer");
  intptr_t n = fixnum(a2);
  int sz = u8vector_nsize(a1);
  if (n < 0 || n + 4 > sz) throw std::runtime_error("u8vector-int32-ref: index out of bounds");
  int32_t val;
  memcpy(&val, u8vector_elts(a1) + n, 4);
  return make_fixnum(val);
}

// u8vector-uint32-set!
SUBR subr_u8vector_uint32_set(scm_obj_t self, scm_obj_t a1, scm_obj_t a2, scm_obj_t a3) {
  if (!is_u8vector(a1)) throw std::runtime_error("u8vector-uint32-set!: first argument must be a u8vector");
  if (!is_fixnum(a2)) throw std::runtime_error("u8vector-uint32-set!: second argument must be an exact integer");
  if (!is_fixnum(a3)) throw std::runtime_error("u8vector-uint32-set!: third argument must be an exact integer");
  intptr_t n = fixnum(a2);
  int sz = u8vector_nsize(a1);
  if (n < 0 || n + 4 > sz) throw std::runtime_error("u8vector-uint32-set!: index out of bounds");
  uint32_t val = (uint32_t)fixnum(a3);
  memcpy(u8vector_elts(a1) + n, &val, 4);
  return scm_unspecified;
}

// u8vector-int32-set!
SUBR subr_u8vector_int32_set(scm_obj_t self, scm_obj_t a1, scm_obj_t a2, scm_obj_t a3) {
  if (!is_u8vector(a1)) throw std::runtime_error("u8vector-int32-set!: first argument must be a u8vector");
  if (!is_fixnum(a2)) throw std::runtime_error("u8vector-int32-set!: second argument must be an exact integer");
  if (!is_fixnum(a3)) throw std::runtime_error("u8vector-int32-set!: third argument must be an exact integer");
  intptr_t n = fixnum(a2);
  int sz = u8vector_nsize(a1);
  if (n < 0 || n + 4 > sz) throw std::runtime_error("u8vector-int32-set!: index out of bounds");
  int32_t val = (int32_t)fixnum(a3);
  memcpy(u8vector_elts(a1) + n, &val, 4);
  return scm_unspecified;
}

// u8vector-set!  - SRFI-4
SUBR subr_u8vector_set(scm_obj_t self, scm_obj_t a1, scm_obj_t a2, scm_obj_t a3) {
  if (!is_u8vector(a1)) throw std::runtime_error("u8vector-set!: first argument must be a u8vector");
  if (!is_fixnum(a2)) throw std::runtime_error("u8vector-set!: second argument must be an exact integer");
  if (!is_fixnum(a3)) throw std::runtime_error("u8vector-set!: third argument must be an exact integer");
  intptr_t n = fixnum(a2);
  int sz = u8vector_nsize(a1);
  if (n < 0 || n >= sz) throw std::runtime_error("u8vector-set!: index out of bounds");
  u8vector_elts(a1)[n] = (uint8_t)fixnum(a3);
  return scm_unspecified;
}

void init_subr_uvector() {
  auto reg = [](const char* name, void* func, int req, bool opt) {
    context::environment_variable_set(make_symbol(name), make_closure(func, req, opt ? 1 : 0, 0, nullptr, 1));
  };

  reg("u8vector?", (void*)subr_u8vector_p, 1, false);
  reg("make-u8vector", (void*)subr_make_u8vector, 1, true);
  reg("make-u8vector-mapping", (void*)subr_make_u8vector_mapping, 2, false);
  reg("u8vector-length", (void*)subr_u8vector_length, 1, false);
  reg("u8vector-ref", (void*)subr_u8vector_ref, 2, false);
  reg("u8vector-uint32-ref", (void*)subr_u8vector_uint32_ref, 2, false);
  reg("u8vector-int32-ref", (void*)subr_u8vector_int32_ref, 2, false);
  reg("u8vector-uint32-set!", (void*)subr_u8vector_uint32_set, 3, false);
  reg("u8vector-int32-set!", (void*)subr_u8vector_int32_set, 3, false);
  reg("u8vector-set!", (void*)subr_u8vector_set, 3, false);
}
