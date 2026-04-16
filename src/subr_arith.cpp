// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "context.h"
#include "subr.h"

#include <cerrno>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>

// ============================================================================
// Arithmetic  - R6RS 11.7
// ============================================================================

static inline double val_double(scm_obj_t x) {
  if (is_fixnum(x)) return (double)fixnum(x);
  if (is_flonum(x)) return flonum(x);
  throw std::runtime_error("number expected");
}

SUBR subr_num_add(scm_obj_t self, int argc, scm_obj_t argv[]) {
  bool inexact = false;
  intptr_t sum_i = 0;
  double sum_f = 0.0;
  for (int i = 0; i < argc; i++) {
    scm_obj_t arg = argv[i];
    if (is_fixnum(arg)) [[likely]] {
      if (inexact)
        sum_f += (double)fixnum(arg);
      else
        sum_i += fixnum(arg);
    } else if (is_flonum(arg)) {
      if (!inexact) {
        inexact = true;
        sum_f = (double)sum_i;
      }
      sum_f += flonum(arg);
    } else {
      throw std::runtime_error("+: arguments must be numbers");
    }
  }
  if (inexact) return make_flonum(sum_f);
  return make_fixnum(sum_i);
}

SUBR subr_num_sub(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc == 0) [[unlikely]]
    throw std::runtime_error("-: too few arguments");

  bool inexact = false;
  intptr_t res_i = 0;
  double res_f = 0.0;

  if (is_fixnum(argv[0])) [[likely]] {
    res_i = fixnum(argv[0]);
    res_f = (double)res_i;
  } else if (is_flonum(argv[0])) {
    inexact = true;
    res_f = flonum(argv[0]);
  } else {
    throw std::runtime_error("-: arguments must be numbers");
  }

  if (argc == 1) [[unlikely]] {
    if (inexact) return make_flonum(-res_f);
    return make_fixnum(-res_i);
  }

  for (int i = 1; i < argc; i++) {
    scm_obj_t arg = argv[i];
    if (is_fixnum(arg)) [[likely]] {
      if (inexact)
        res_f -= (double)fixnum(arg);
      else
        res_i -= fixnum(arg);
    } else if (is_flonum(arg)) {
      if (!inexact) {
        inexact = true;
        res_f = (double)res_i;
      }
      res_f -= flonum(arg);
    } else {
      throw std::runtime_error("-: arguments must be numbers");
    }
  }
  if (inexact) return make_flonum(res_f);
  return make_fixnum(res_i);
}

SUBR subr_num_mul(scm_obj_t self, int argc, scm_obj_t argv[]) {
  bool inexact = false;
  intptr_t p_i = 1;
  double p_f = 1.0;
  for (int i = 0; i < argc; i++) {
    scm_obj_t arg = argv[i];
    if (is_fixnum(arg)) [[likely]] {
      if (inexact)
        p_f *= (double)fixnum(arg);
      else
        p_i *= fixnum(arg);
    } else if (is_flonum(arg)) {
      if (!inexact) {
        inexact = true;
        p_f = (double)p_i;
      }
      p_f *= flonum(arg);
    } else {
      throw std::runtime_error("*: arguments must be numbers");
    }
  }
  if (inexact) return make_flonum(p_f);
  return make_fixnum(p_i);
}

SUBR subr_num_div(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc == 0) [[unlikely]]
    throw std::runtime_error("/: too few arguments");

  bool inexact = false;
  intptr_t res_i = 0;
  double res_f = 0.0;

  if (is_fixnum(argv[0])) [[likely]] {
    res_i = fixnum(argv[0]);
    res_f = (double)res_i;
  } else if (is_flonum(argv[0])) {
    inexact = true;
    res_f = flonum(argv[0]);
  } else {
    throw std::runtime_error("/: arguments must be numbers");
  }

  if (argc == 1) [[unlikely]] {
    if (inexact) return make_flonum(1.0 / res_f);
    if (res_i == 0) throw std::runtime_error("/: division by zero");
    return make_fixnum(1 / res_i);
  }

  for (int i = 1; i < argc; i++) {
    scm_obj_t arg = argv[i];
    if (is_fixnum(arg)) [[likely]] {
      intptr_t d = fixnum(arg);
      if (d == 0) [[unlikely]]
        throw std::runtime_error("/: division by zero");
      if (inexact)
        res_f /= (double)d;
      else
        res_i /= d;
    } else if (is_flonum(arg)) {
      double d = flonum(arg);
      if (d == 0.0) [[unlikely]]
        throw std::runtime_error("/: division by zero");
      if (!inexact) {
        inexact = true;
        res_f = (double)res_i;
      }
      res_f /= d;
    } else {
      throw std::runtime_error("/: arguments must be numbers");
    }
  }
  if (inexact) return make_flonum(res_f);
  return make_fixnum(res_i);
}

// ============================================================================
// Numeric Comparisons  - R6RS 11.7.3
// ============================================================================

SUBR subr_num_eq(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1) [[unlikely]]
    throw std::runtime_error("=: too few arguments");
  if (argc == 1) return scm_true;

  double first;
  try {
    first = val_double(argv[0]);
  } catch (const std::runtime_error& e) {
    throw std::runtime_error("=: " + std::string(e.what()));
  }

  for (int i = 1; i < argc; i++) {
    try {
      if (val_double(argv[i]) != first) return scm_false;
    } catch (const std::runtime_error& e) {
      throw std::runtime_error("=: " + std::string(e.what()));
    }
  }
  return scm_true;
}

SUBR subr_num_lt(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1) [[unlikely]]
    throw std::runtime_error("<: too few arguments");
  if (argc == 1) return scm_true;

  double prev;
  try {
    prev = val_double(argv[0]);
  } catch (const std::runtime_error& e) {
    throw std::runtime_error("<: " + std::string(e.what()));
  }

  for (int i = 1; i < argc; i++) {
    try {
      double cur = val_double(argv[i]);
      if (prev < cur) {
        prev = cur;
        continue;
      }
    } catch (const std::runtime_error& e) {
      throw std::runtime_error("<: " + std::string(e.what()));
    }
    return scm_false;
  }
  return scm_true;
}

SUBR subr_num_gt(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1) [[unlikely]]
    throw std::runtime_error(">: too few arguments");
  if (argc == 1) return scm_true;

  double prev;
  try {
    prev = val_double(argv[0]);
  } catch (const std::runtime_error& e) {
    throw std::runtime_error(">: " + std::string(e.what()));
  }

  for (int i = 1; i < argc; i++) {
    try {
      double cur = val_double(argv[i]);
      if (prev > cur) {
        prev = cur;
        continue;
      }
    } catch (const std::runtime_error& e) {
      throw std::runtime_error(">: " + std::string(e.what()));
    }
    return scm_false;
  }
  return scm_true;
}

SUBR subr_num_le(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1) [[unlikely]]
    throw std::runtime_error("<=: too few arguments");
  if (argc == 1) return scm_true;

  double prev;
  try {
    prev = val_double(argv[0]);
  } catch (const std::runtime_error& e) {
    throw std::runtime_error("<=: " + std::string(e.what()));
  }

  for (int i = 1; i < argc; i++) {
    try {
      double cur = val_double(argv[i]);
      if (prev <= cur) {
        prev = cur;
        continue;
      }
    } catch (const std::runtime_error& e) {
      throw std::runtime_error("<=: " + std::string(e.what()));
    }
    return scm_false;
  }
  return scm_true;
}

SUBR subr_num_ge(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1) [[unlikely]]
    throw std::runtime_error(">=: too few arguments");
  if (argc == 1) return scm_true;

  double prev;
  try {
    prev = val_double(argv[0]);
  } catch (const std::runtime_error& e) {
    throw std::runtime_error(">=: " + std::string(e.what()));
  }

  for (int i = 1; i < argc; i++) {
    try {
      double cur = val_double(argv[i]);
      if (prev >= cur) {
        prev = cur;
        continue;
      }
    } catch (const std::runtime_error& e) {
      throw std::runtime_error(">=: " + std::string(e.what()));
    }
    return scm_false;
  }
  return scm_true;
}

SUBR subr_zero_p(scm_obj_t self, scm_obj_t a1) {
  if (is_fixnum(a1)) {
    return fixnum(a1) == 0 ? scm_true : scm_false;
  } else if (is_flonum(a1)) {
    return flonum(a1) == 0.0 ? scm_true : scm_false;
  } else {
    throw std::runtime_error("zero?: argument must be a number");
  }
}

SUBR subr_remainder(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_fixnum(a1) || !is_fixnum(a2)) {
    throw std::runtime_error("remainder: integers expected");
  }
  intptr_t v1 = fixnum(a1);
  intptr_t v2 = fixnum(a2);
  if (v2 == 0) throw std::runtime_error("remainder: division by zero");
  return make_fixnum(v1 % v2);
}

SUBR subr_quotient(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_fixnum(a1) || !is_fixnum(a2)) {
    throw std::runtime_error("quotient: integers expected");
  }
  intptr_t v1 = fixnum(a1);
  intptr_t v2 = fixnum(a2);
  if (v2 == 0) throw std::runtime_error("quotient: division by zero");
  return make_fixnum(v1 / v2);
}

SUBR subr_exact_to_inexact(scm_obj_t self, scm_obj_t a1) {
  if (is_fixnum(a1)) {
    return make_flonum((double)fixnum(a1));
  } else if (is_flonum(a1)) {
    return a1;
  } else {
    throw std::runtime_error("exact->inexact: number expected");
  }
}

SUBR subr_inexact_to_exact(scm_obj_t self, scm_obj_t a1) {
  if (is_fixnum(a1)) return a1;
  if (is_flonum(a1)) {
    return make_fixnum((intptr_t)std::round(flonum(a1)));
  }
  throw std::runtime_error("inexact->exact: number expected");
}

SUBR subr_positive_p(scm_obj_t self, scm_obj_t a1) {
  if (is_fixnum(a1)) {
    return fixnum(a1) > 0 ? scm_true : scm_false;
  } else if (is_flonum(a1)) {
    return flonum(a1) > 0.0 ? scm_true : scm_false;
  } else {
    throw std::runtime_error("positive?: number expected");
  }
}

SUBR subr_negative_p(scm_obj_t self, scm_obj_t a1) {
  if (is_fixnum(a1)) {
    return fixnum(a1) < 0 ? scm_true : scm_false;
  } else if (is_flonum(a1)) {
    return flonum(a1) < 0.0 ? scm_true : scm_false;
  } else {
    throw std::runtime_error("negative?: number expected");
  }
}

SUBR subr_round(scm_obj_t self, scm_obj_t a1) {
  if (is_fixnum(a1)) return a1;
  if (is_flonum(a1)) {
    return make_flonum(std::round(flonum(a1)));
  }
  throw std::runtime_error("round: number expected");
}

SUBR subr_odd_p(scm_obj_t self, scm_obj_t a1) {
  if (is_fixnum(a1)) return (fixnum(a1) & 1) ? scm_true : scm_false;
  throw std::runtime_error("odd?: integer expected");
}

SUBR subr_even_p(scm_obj_t self, scm_obj_t a1) {
  if (is_fixnum(a1)) return (fixnum(a1) & 1) == 0 ? scm_true : scm_false;
  throw std::runtime_error("even?: integer expected");
}

SUBR subr_sqrt(scm_obj_t self, scm_obj_t a1) { return make_flonum(std::sqrt(val_double(a1))); }

SUBR subr_sin(scm_obj_t self, scm_obj_t a1) { return make_flonum(std::sin(val_double(a1))); }

SUBR subr_cos(scm_obj_t self, scm_obj_t a1) { return make_flonum(std::cos(val_double(a1))); }

SUBR subr_tan(scm_obj_t self, scm_obj_t a1) { return make_flonum(std::tan(val_double(a1))); }

SUBR subr_atan(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc == 1) return make_flonum(std::atan(val_double(argv[0])));
  if (argc == 2) return make_flonum(std::atan2(val_double(argv[0]), val_double(argv[1])));
  throw std::runtime_error("atan: 1 or 2 arguments expected");
}

SUBR subr_modulo(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_fixnum(a1) || !is_fixnum(a2)) {
    throw std::runtime_error("modulo: integers expected");
  }
  intptr_t v1 = fixnum(a1);
  intptr_t v2 = fixnum(a2);
  if (v2 == 0) throw std::runtime_error("modulo: division by zero");
  intptr_t r = v1 % v2;
  if (r != 0 && (r ^ v2) < 0) r += v2;
  return make_fixnum(r);
}

// ============================================================================
// Initialization
// ============================================================================

void init_subr_arith() {
  auto reg = [](const char* name, void* func, int req, int opt) {
    context::environment_variable_set(make_symbol(name), make_closure(func, req, opt, 0, nullptr, 1));
  };

  // arithmetic
  reg("+", (void*)subr_num_add, 0, 1);
  reg("-", (void*)subr_num_sub, 1, 1);
  reg("*", (void*)subr_num_mul, 0, 1);
  reg("/", (void*)subr_num_div, 1, 1);

  // numeric comparisons
  reg("=", (void*)subr_num_eq, 2, 1);
  reg("<", (void*)subr_num_lt, 2, 1);
  reg(">", (void*)subr_num_gt, 2, 1);
  reg("<=", (void*)subr_num_le, 2, 1);
  reg(">=", (void*)subr_num_ge, 2, 1);
  reg("zero?", (void*)subr_zero_p, 1, 0);
  reg("remainder", (void*)subr_remainder, 2, 0);
  reg("quotient", (void*)subr_quotient, 2, 0);
  reg("exact->inexact", (void*)subr_exact_to_inexact, 1, 0);
  reg("inexact->exact", (void*)subr_inexact_to_exact, 1, 0);
  reg("positive?", (void*)subr_positive_p, 1, 0);
  reg("negative?", (void*)subr_negative_p, 1, 0);
  reg("round", (void*)subr_round, 1, 0);
  reg("odd?", (void*)subr_odd_p, 1, 0);
  reg("even?", (void*)subr_even_p, 1, 0);
  reg("sqrt", (void*)subr_sqrt, 1, 0);
  reg("sin", (void*)subr_sin, 1, 0);
  reg("cos", (void*)subr_cos, 1, 0);
  reg("tan", (void*)subr_tan, 1, 0);
  reg("atan", (void*)subr_atan, 1, 1);
  reg("modulo", (void*)subr_modulo, 2, 0);
}
