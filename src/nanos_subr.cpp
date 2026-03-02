// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "codegen_aux.h"
#include "equiv.h"
#include "hash.h"
#include "nanos.h"
#include "nanos_context.h"
#include "object_heap.h"
#include "printer.h"

#include <cerrno>
#include <cmath>
#include <cstdio>
#include <cstring>

// ============================================================================
// Arithmetic
// ============================================================================

SUBR subr_num_add(scm_obj_t self, int argc, scm_obj_t argv[]) {
  intptr_t sum = 0;
  for (int i = 0; i < argc; i++) {
    if (is_fixnum(argv[i])) [[likely]] {
      sum += fixnum(argv[i]);
    } else {
      throw std::runtime_error("+: arguments must be fixnums");
    }
  }
  return make_fixnum(sum);
}

SUBR subr_num_sub(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc == 0) [[unlikely]]
    throw std::runtime_error("-: too few arguments");
  if (!is_fixnum(argv[0])) [[unlikely]]
    throw std::runtime_error("-: arguments must be fixnums");

  intptr_t result = fixnum(argv[0]);

  if (argc == 1) [[unlikely]] {
    return make_fixnum(-result);
  }

  for (int i = 1; i < argc; i++) {
    if (is_fixnum(argv[i])) [[likely]] {
      result -= fixnum(argv[i]);
    } else {
      throw std::runtime_error("-: arguments must be fixnums");
    }
  }
  return make_fixnum(result);
}

SUBR subr_num_mul(scm_obj_t self, int argc, scm_obj_t argv[]) {
  intptr_t p = 1;
  for (int i = 0; i < argc; i++) {
    if (is_fixnum(argv[i])) [[likely]] {
      p *= fixnum(argv[i]);
    } else {
      throw std::runtime_error("*: arguments must be fixnums");
    }
  }
  return make_fixnum(p);
}

SUBR subr_num_div(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc == 0) [[unlikely]]
    throw std::runtime_error("/: too few arguments");
  if (!is_fixnum(argv[0])) [[unlikely]]
    throw std::runtime_error("/: arguments must be fixnums");

  intptr_t result = fixnum(argv[0]);

  if (argc == 1) [[unlikely]] {
    return make_fixnum(1 / result);
  }

  for (int i = 1; i < argc; i++) {
    if (is_fixnum(argv[i])) [[likely]] {
      intptr_t d = fixnum(argv[i]);
      if (d == 0) [[unlikely]]
        throw std::runtime_error("/: division by zero");
      result /= d;
    } else {
      throw std::runtime_error("/: arguments must be fixnums");
    }
  }
  return make_fixnum(result);
}

// ============================================================================
// Numeric Comparisons
// ============================================================================

SUBR subr_num_eq(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1) [[unlikely]]
    throw std::runtime_error("=: too few arguments");

  if (!is_fixnum(argv[0])) [[unlikely]]
    throw std::runtime_error("=: arguments must be fixnums");
  scm_obj_t first = argv[0];

  for (int i = 1; i < argc; i++) {
    if (!is_fixnum(argv[i])) [[unlikely]]
      throw std::runtime_error("=: arguments must be fixnums");
    if (argv[i] != first) return scm_false;
  }
  return scm_true;
}

SUBR subr_num_lt(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1) [[unlikely]]
    throw std::runtime_error("<: too few arguments");

  if (!is_fixnum(argv[0])) [[unlikely]]
    throw std::runtime_error("<: arguments must be fixnums");
  scm_obj_t first = argv[0];

  for (int i = 1; i < argc; i++) {
    if (!is_fixnum(argv[i])) [[unlikely]]
      throw std::runtime_error("<: arguments must be fixnums");
    if (first < argv[i]) {
      first = argv[i];
      continue;
    }
    return scm_false;
  }
  return scm_true;
}

SUBR subr_num_gt(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1) [[unlikely]]
    throw std::runtime_error(">: too few arguments");

  if (!is_fixnum(argv[0])) [[unlikely]]
    throw std::runtime_error(">: arguments must be fixnums");
  scm_obj_t first = argv[0];

  for (int i = 1; i < argc; i++) {
    if (!is_fixnum(argv[i])) [[unlikely]]
      throw std::runtime_error(">: arguments must be fixnums");
    if (first > argv[i]) {
      first = argv[i];
      continue;
    }
    return scm_false;
  }
  return scm_true;
}

SUBR subr_num_le(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1) [[unlikely]]
    throw std::runtime_error("<=: too few arguments");

  if (!is_fixnum(argv[0])) [[unlikely]]
    throw std::runtime_error("<=: arguments must be fixnums");
  scm_obj_t first = argv[0];

  for (int i = 1; i < argc; i++) {
    if (!is_fixnum(argv[i])) [[unlikely]]
      throw std::runtime_error("<=: arguments must be fixnums");
    if (first <= argv[i]) {
      first = argv[i];
      continue;
    }
    return scm_false;
  }
  return scm_true;
}

SUBR subr_num_ge(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1) [[unlikely]]
    throw std::runtime_error(">=: too few arguments");

  if (!is_fixnum(argv[0])) [[unlikely]]
    throw std::runtime_error(">=: arguments must be fixnums");
  scm_obj_t first = argv[0];

  for (int i = 1; i < argc; i++) {
    if (!is_fixnum(argv[i])) [[unlikely]]
      throw std::runtime_error(">=: arguments must be fixnums");
    if (first >= argv[i]) {
      first = argv[i];
      continue;
    }
    return scm_false;
  }
  return scm_true;
}

// ============================================================================
// Pairs & Lists
// ============================================================================

SUBR subr_cons(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) { return make_cons(a1, a2); }

SUBR subr_car(scm_obj_t self, scm_obj_t a1) {
  if (is_cons(a1)) {
    scm_cons_rec_t* cons = (scm_cons_rec_t*)a1;
    return cons->car;
  }
  throw std::runtime_error("car: argument must be a cons cell");
}

SUBR subr_cdr(scm_obj_t self, scm_obj_t a1) {
  if (is_cons(a1)) {
    scm_cons_rec_t* cons = (scm_cons_rec_t*)a1;
    return cons->cdr;
  }
  throw std::runtime_error("cdr: argument must be a cons cell");
}

SUBR subr_cadr(scm_obj_t self, scm_obj_t a1) { return subr_car(self, subr_cdr(self, a1)); }

SUBR subr_caddr(scm_obj_t self, scm_obj_t a1) { return subr_car(self, subr_cdr(self, subr_cdr(self, a1))); }

SUBR subr_list(scm_obj_t self, int argc, scm_obj_t argv[]) {
  scm_obj_t list = scm_nil;
  for (int i = argc - 1; i >= 0; i--) {
    list = make_cons(argv[i], list);
  }
  return list;
}

static scm_obj_t append2(scm_obj_t lst1, scm_obj_t lst2) {
  if (lst1 == scm_nil) return lst2;
  scm_obj_t head = make_cons(CAR(lst1), scm_nil);
  scm_obj_t tail = head;
  lst1 = CDR(lst1);
  while (lst1 != scm_nil) {
    CDR(tail) = make_cons(CAR(lst1), scm_nil);
    tail = CDR(tail);
    lst1 = CDR(lst1);
  }
  CDR(tail) = lst2;
  return head;
}

SUBR subr_append(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc == 0) return scm_nil;
  if (argc == 1) return argv[0];
  scm_obj_t obj = scm_undef;
  for (int i = argc - 1; i >= 0; i--) {
    if (obj == scm_undef)
      obj = argv[i];
    else
      obj = append2(argv[i], obj);
  }
  return obj;
}

SUBR subr_caar(scm_obj_t self, scm_obj_t a1) { return subr_car(self, subr_car(self, a1)); }
SUBR subr_cadar(scm_obj_t self, scm_obj_t a1) { return subr_car(self, subr_cdr(self, subr_car(self, a1))); }
SUBR subr_cadddr(scm_obj_t self, scm_obj_t a1) { return subr_car(self, subr_cdr(self, subr_cdr(self, subr_cdr(self, a1)))); }
SUBR subr_cddr(scm_obj_t self, scm_obj_t a1) { return subr_cdr(self, subr_cdr(self, a1)); }
SUBR subr_cdddr(scm_obj_t self, scm_obj_t a1) { return subr_cdr(self, subr_cdr(self, subr_cdr(self, a1))); }

SUBR subr_set_car_bang(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (is_cons(a1)) {
    object_heap_t::current()->write_barrier(a2);
    CAR(a1) = a2;
    return scm_unspecified;
  }
  throw std::runtime_error("set-car!: argument must be a cons cell");
}

SUBR subr_length(scm_obj_t self, scm_obj_t a1) {
  int len = 0;
  scm_obj_t slow = a1;
  scm_obj_t fast = a1;
  while (true) {
    if (fast == scm_nil) return make_fixnum(len);
    if (!is_cons(fast)) throw std::runtime_error("length: argument must be a proper list");
    fast = CDR(fast);
    len++;
    if (fast == scm_nil) return make_fixnum(len);
    if (!is_cons(fast)) throw std::runtime_error("length: argument must be a proper list");
    fast = CDR(fast);
    len++;
    slow = CDR(slow);
    if (slow == fast) throw std::runtime_error("length: cycle detected");
  }
}

SUBR subr_list_ref(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_fixnum(a2)) throw std::runtime_error("list-ref: second argument must be an exact integer");
  intptr_t k = fixnum(a2);
  if (k < 0) throw std::runtime_error("list-ref: index must be non-negative");
  scm_obj_t cur = a1;
  while (k > 0) {
    if (!is_cons(cur)) throw std::runtime_error("list-ref: index out of bounds or not a proper list");
    cur = CDR(cur);
    k--;
  }
  if (!is_cons(cur)) throw std::runtime_error("list-ref: index out of bounds or not a proper list");
  return CAR(cur);
}

SUBR subr_list_to_vector(scm_obj_t self, scm_obj_t a1) {
  int len = 0;
  scm_obj_t cur = a1;
  while (is_cons(cur)) {
    len++;
    cur = CDR(cur);
  }
  if (cur != scm_nil) throw std::runtime_error("list->vector: argument must be a proper list");
  scm_obj_t v = make_vector(len, scm_undef);
  scm_obj_t* elts = vector_elts(v);
  cur = a1;
  for (int i = 0; i < len; i++) {
    elts[i] = CAR(cur);
    cur = CDR(cur);
  }
  return v;
}

SUBR subr_memq(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  scm_obj_t cur = a2;
  while (is_cons(cur)) {
    if (CAR(cur) == a1) return cur;
    cur = CDR(cur);
  }
  if (cur != scm_nil) throw std::runtime_error("memq: second argument must be a proper list");
  return scm_false;
}

SUBR subr_memv(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  scm_obj_t cur = a2;
  while (is_cons(cur)) {
    if (eqv_p(CAR(cur), a1)) return cur;
    cur = CDR(cur);
  }
  if (cur != scm_nil) throw std::runtime_error("memv: second argument must be a proper list");
  return scm_false;
}

SUBR subr_member(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  scm_obj_t cur = a2;
  while (is_cons(cur)) {
    scm_obj_t visited = make_hashtable(address_hash, address_equiv, 4);
    if (equal_p(visited, a1, CAR(cur))) return cur;
    cur = CDR(cur);
  }
  if (cur != scm_nil) throw std::runtime_error("member: second argument must be a proper list");
  return scm_false;
}

SUBR subr_assq(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  scm_obj_t cur = a2;
  while (is_cons(cur)) {
    scm_obj_t pair = CAR(cur);
    if (!is_cons(pair)) throw std::runtime_error("assq: alist must contain pairs");
    if (CAR(pair) == a1) return pair;
    cur = CDR(cur);
  }
  if (cur != scm_nil) throw std::runtime_error("assq: second argument must be a proper list");
  return scm_false;
}

SUBR subr_assoc(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  scm_obj_t cur = a2;
  while (is_cons(cur)) {
    scm_obj_t pair = CAR(cur);
    if (!is_cons(pair)) throw std::runtime_error("assoc: alist must contain pairs");
    scm_obj_t visited = make_hashtable(address_hash, address_equiv, 4);
    if (equal_p(visited, a1, CAR(pair))) return pair;
    cur = CDR(cur);
  }
  if (cur != scm_nil) throw std::runtime_error("assoc: second argument must be a proper list");
  return scm_false;
}

SUBR subr_reverse(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t cur = a1;
  scm_obj_t result = scm_nil;
  while (is_cons(cur)) {
    result = make_cons(CAR(cur), result);
    cur = CDR(cur);
  }
  if (cur != scm_nil) throw std::runtime_error("reverse: argument must be a proper list");
  return result;
}

// ============================================================================
// Predicates & Logic
// ============================================================================

SUBR subr_not(scm_obj_t self, scm_obj_t a1) { return (a1 == scm_false) ? scm_true : scm_false; }

// boolean?
SUBR subr_boolean_p(scm_obj_t self, scm_obj_t a1) { return (a1 == scm_true || a1 == scm_false) ? scm_true : scm_false; }

// char?
SUBR subr_char_p(scm_obj_t self, scm_obj_t a1) { return is_char(a1) ? scm_true : scm_false; }

// eq?
SUBR subr_eq_p(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) { return (a1 == a2) ? scm_true : scm_false; }

// eqv?
SUBR subr_eqv_p(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) { return eqv_p(a1, a2) ? scm_true : scm_false; }

// equal?
SUBR subr_equal_p(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  scm_obj_t visited = make_hashtable(address_hash, address_equiv, 4);
  return equal_p(visited, a1, a2) ? scm_true : scm_false;
}

// exact?
SUBR subr_exact_p(scm_obj_t self, scm_obj_t a1) {
  if (is_fixnum(a1)) return scm_true;
  if (is_short_flonum(a1) || is_long_flonum(a1)) return scm_false;
  throw std::runtime_error("exact?: argument must be a number");
}

// inexact?
SUBR subr_inexact_p(scm_obj_t self, scm_obj_t a1) {
  if (is_short_flonum(a1) || is_long_flonum(a1)) return scm_true;
  if (is_fixnum(a1)) return scm_false;
  throw std::runtime_error("inexact?: argument must be a number");
}

// infinite?
SUBR subr_infinite_p(scm_obj_t self, scm_obj_t a1) {
  if (is_fixnum(a1)) return scm_false;
  if (is_short_flonum(a1)) return std::isinf(flonum(a1)) ? scm_true : scm_false;
  if (is_long_flonum(a1)) return std::isinf(flonum(a1)) ? scm_true : scm_false;
  throw std::runtime_error("infinite?: argument must be a real number");
}

// integer?
SUBR subr_integer_p(scm_obj_t self, scm_obj_t a1) {
  if (is_fixnum(a1)) return scm_true;
  if (is_short_flonum(a1) || is_long_flonum(a1)) {
    double d = flonum(a1);
    return (std::isfinite(d) && d == std::trunc(d)) ? scm_true : scm_false;
  }
  return scm_false;
}

// list? (tortoise-and-hare cycle detection)
SUBR subr_list_p(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t slow = a1;
  scm_obj_t fast = a1;
  while (true) {
    if (fast == scm_nil) return scm_true;
    if (!is_cons(fast)) return scm_false;
    fast = CDR(fast);
    if (fast == scm_nil) return scm_true;
    if (!is_cons(fast)) return scm_false;
    fast = CDR(fast);
    slow = CDR(slow);
    if (slow == fast) return scm_false;  // cycle
  }
}

// null?
SUBR subr_null_p(scm_obj_t self, scm_obj_t a1) { return (a1 == scm_nil) ? scm_true : scm_false; }

// number?
SUBR subr_number_p(scm_obj_t self, scm_obj_t a1) { return (is_fixnum(a1) || is_short_flonum(a1) || is_long_flonum(a1)) ? scm_true : scm_false; }

// nan?
SUBR subr_nan_p(scm_obj_t self, scm_obj_t a1) {
  if (is_fixnum(a1)) return scm_false;
  if (is_short_flonum(a1) || is_long_flonum(a1)) return std::isnan(flonum(a1)) ? scm_true : scm_false;
  throw std::runtime_error("nan?: argument must be a real number");
}

// pair?
SUBR subr_pair_p(scm_obj_t self, scm_obj_t a1) { return is_cons(a1) ? scm_true : scm_false; }

// procedure?
SUBR subr_procedure_p(scm_obj_t self, scm_obj_t a1) { return (is_closure(a1) || is_continuation(a1) || is_escape(a1)) ? scm_true : scm_false; }

// real?
SUBR subr_real_p(scm_obj_t self, scm_obj_t a1) { return (is_fixnum(a1) || is_short_flonum(a1) || is_long_flonum(a1)) ? scm_true : scm_false; }

// string?
SUBR subr_string_p(scm_obj_t self, scm_obj_t a1) { return is_string(a1) ? scm_true : scm_false; }

// symbol?
SUBR subr_symbol_p(scm_obj_t self, scm_obj_t a1) { return is_symbol(a1) ? scm_true : scm_false; }

// vector?
SUBR subr_vector_p(scm_obj_t self, scm_obj_t a1) { return is_vector(a1) ? scm_true : scm_false; }

// ============================================================================
// Vectors
// ============================================================================

// vector
SUBR subr_vector(scm_obj_t self, int argc, scm_obj_t argv[]) {
  scm_obj_t v = make_vector(argc, scm_undef);
  scm_obj_t* elts = vector_elts(v);
  for (int i = 0; i < argc; i++) elts[i] = argv[i];
  return v;
}

// vector-length
SUBR subr_vector_length(scm_obj_t self, scm_obj_t a1) {
  if (is_vector(a1)) return make_fixnum(vector_nsize(a1));
  throw std::runtime_error("vector-length: argument must be a vector");
}

// vector-ref
SUBR subr_vector_ref(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_vector(a1)) throw std::runtime_error("vector-ref: first argument must be a vector");
  if (!is_fixnum(a2)) throw std::runtime_error("vector-ref: second argument must be an exact integer");
  intptr_t n = fixnum(a2);
  int sz = vector_nsize(a1);
  if (n < 0 || n >= sz) throw std::runtime_error("vector-ref: index out of bounds");
  return vector_elts(a1)[n];
}

// vector-set!
SUBR subr_vector_set(scm_obj_t self, scm_obj_t a1, scm_obj_t a2, scm_obj_t a3) {
  if (!is_vector(a1)) throw std::runtime_error("vector-set!: first argument must be a vector");
  if (!is_fixnum(a2)) throw std::runtime_error("vector-set!: second argument must be an exact integer");
  intptr_t n = fixnum(a2);
  int sz = vector_nsize(a1);
  if (n < 0 || n >= sz) throw std::runtime_error("vector-set!: index out of bounds");
  object_heap_t::current()->write_barrier(a3);
  vector_elts(a1)[n] = a3;
  return scm_undef;
}

// vector->list
SUBR subr_vector_to_list(scm_obj_t self, scm_obj_t a1) {
  if (!is_vector(a1)) throw std::runtime_error("vector->list: argument must be a vector");
  int n = vector_nsize(a1);
  scm_obj_t* elts = vector_elts(a1);
  scm_obj_t lst = scm_nil;
  for (int i = n - 1; i >= 0; i--) lst = make_cons(elts[i], lst);
  return lst;
}

// ============================================================================
// Strings
// ============================================================================

// string-length  (byte length — ASCII/UTF-8 byte count)
SUBR subr_string_length(scm_obj_t self, scm_obj_t a1) {
  if (!is_string(a1)) throw std::runtime_error("string-length: argument must be a string");
  return make_fixnum((intptr_t)strlen((const char*)string_name(a1)));
}

// string-ref  (byte index → char)
SUBR subr_string_ref(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_string(a1)) throw std::runtime_error("string-ref: first argument must be a string");
  if (!is_fixnum(a2)) throw std::runtime_error("string-ref: second argument must be an exact integer");
  const char* s = (const char*)string_name(a1);
  intptr_t idx = fixnum(a2);
  intptr_t len = (intptr_t)strlen(s);
  if (idx < 0 || idx >= len) throw std::runtime_error("string-ref: index out of bounds");
  // Decode UTF-8 character at byte position idx
  const uint8_t* p = (const uint8_t*)s + idx;
  uint32_t ucs4;
  if (*p < 0x80) {
    ucs4 = *p;
  } else if ((*p & 0xE0) == 0xC0) {
    ucs4 = ((uint32_t)(*p & 0x1F) << 6) | (p[1] & 0x3F);
  } else if ((*p & 0xF0) == 0xE0) {
    ucs4 = ((uint32_t)(*p & 0x0F) << 12) | ((uint32_t)(p[1] & 0x3F) << 6) | (p[2] & 0x3F);
  } else {
    ucs4 = ((uint32_t)(*p & 0x07) << 18) | ((uint32_t)(p[1] & 0x3F) << 12) | ((uint32_t)(p[2] & 0x3F) << 6) | (p[3] & 0x3F);
  }
  return make_char(ucs4);
}

// string=?
SUBR subr_string_eq(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_string(a1)) throw std::runtime_error("string=?: arguments must be strings");
  if (!is_string(a2)) throw std::runtime_error("string=?: arguments must be strings");
  return strcmp((const char*)string_name(a1), (const char*)string_name(a2)) == 0 ? scm_true : scm_false;
}

// string-append
SUBR subr_string_append(scm_obj_t self, int argc, scm_obj_t argv[]) {
  size_t total = 0;
  for (int i = 0; i < argc; i++) {
    if (!is_string(argv[i])) throw std::runtime_error("string-append: arguments must be strings");
    total += strlen((const char*)string_name(argv[i]));
  }
  std::string buf;
  buf.reserve(total);
  for (int i = 0; i < argc; i++) buf += (const char*)string_name(argv[i]);
  return make_string(buf.c_str());
}

// substring  (byte offsets)
SUBR subr_substring(scm_obj_t self, scm_obj_t a1, scm_obj_t a2, scm_obj_t a3) {
  if (!is_string(a1)) throw std::runtime_error("substring: first argument must be a string");
  if (!is_fixnum(a2)) throw std::runtime_error("substring: second argument must be an exact integer");
  if (!is_fixnum(a3)) throw std::runtime_error("substring: third argument must be an exact integer");
  const char* s = (const char*)string_name(a1);
  intptr_t len = (intptr_t)strlen(s);
  intptr_t from = fixnum(a2);
  intptr_t to = fixnum(a3);
  if (from < 0 || from > len) throw std::runtime_error("substring: start index out of bounds");
  if (to < 0 || to > len) throw std::runtime_error("substring: end index out of bounds");
  if (to < from) throw std::runtime_error("substring: end index before start index");
  std::string buf(s + from, s + to);
  return make_string(buf.c_str());
}

// symbol->string
SUBR subr_symbol_to_string(scm_obj_t self, scm_obj_t a1) {
  if (!is_symbol(a1)) throw std::runtime_error("symbol->string: argument must be a symbol");
  return make_string((const char*)symbol_name(a1));
}

// string->symbol
SUBR subr_string_to_symbol(scm_obj_t self, scm_obj_t a1) {
  if (!is_string(a1)) throw std::runtime_error("string->symbol: argument must be a string");
  return make_symbol((const char*)string_name(a1));
}

// number->string  (radix 2/8/10/16; fixnum or flonum)
SUBR subr_number_to_string(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1 || argc > 2) throw std::runtime_error("number->string: wrong number of arguments");
  int radix = 10;
  if (argc == 2) {
    if (!is_fixnum(argv[1])) throw std::runtime_error("number->string: radix must be an exact integer");
    radix = (int)fixnum(argv[1]);
    if (radix != 2 && radix != 8 && radix != 10 && radix != 16) throw std::runtime_error("number->string: radix must be 2, 8, 10, or 16");
  }
  char buf[128];
  if (is_fixnum(argv[0])) {
    intptr_t n = fixnum(argv[0]);
    switch (radix) {
      case 2: {
        if (n == 0) {
          buf[0] = '0';
          buf[1] = '\0';
          break;
        }
        char tmp[66];
        int pos = 65;
        tmp[pos] = '\0';
        bool neg = n < 0;
        uintptr_t u = neg ? (uintptr_t)(-(n + 1)) + 1 : (uintptr_t)n;
        while (u) {
          tmp[--pos] = '0' + (u & 1);
          u >>= 1;
        }
        if (neg) tmp[--pos] = '-';
        return make_string(tmp + pos);
      }
      case 8:
        snprintf(buf, sizeof(buf), "%jo", (intmax_t)n);
        break;
      case 10:
        snprintf(buf, sizeof(buf), "%jd", (intmax_t)n);
        break;
      case 16:
        snprintf(buf, sizeof(buf), "%jx", (intmax_t)n);
        break;
    }
    return make_string(buf);
  }
  if (is_short_flonum(argv[0]) || is_long_flonum(argv[0])) {
    if (radix != 10) throw std::runtime_error("number->string: radix must be 10 for inexact numbers");
    double d = flonum(argv[0]);
    if (std::isinf(d)) {
      snprintf(buf, sizeof(buf), "%sinf.0", d < 0 ? "-" : "+");
    } else if (std::isnan(d)) {
      snprintf(buf, sizeof(buf), "+nan.0");
    } else {
      snprintf(buf, sizeof(buf), "%.17g", d);
      // Ensure there is a decimal point so it reads back as inexact
      if (!strchr(buf, '.') && !strchr(buf, 'e') && !strchr(buf, 'n') && !strchr(buf, 'i')) strncat(buf, ".0", sizeof(buf) - strlen(buf) - 1);
    }
    return make_string(buf);
  }
  throw std::runtime_error("number->string: argument must be a number");
}

// string->number  (radix 2/8/10/16; returns #f on failure)
SUBR subr_string_to_number(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1 || argc > 2) throw std::runtime_error("string->number: wrong number of arguments");
  if (!is_string(argv[0])) throw std::runtime_error("string->number: first argument must be a string");
  int radix = 10;
  if (argc == 2) {
    if (!is_fixnum(argv[1])) throw std::runtime_error("string->number: radix must be an exact integer");
    radix = (int)fixnum(argv[1]);
    if (radix != 2 && radix != 8 && radix != 10 && radix != 16) throw std::runtime_error("string->number: radix must be 2, 8, 10, or 16");
  }
  const char* s = (const char*)string_name(argv[0]);
  if (*s == '\0') return scm_false;
  // Try integer parse first
  char* end;
  errno = 0;
  long long ival = strtoll(s, &end, radix);
  if (errno == 0 && end != s && *end == '\0') return make_fixnum((int64_t)ival);
  // Try flonum (base 10 only)
  if (radix == 10) {
    errno = 0;
    double dval = strtod(s, &end);
    if (errno == 0 && end != s && *end == '\0') return make_flonum(dval);
  }
  return scm_false;
}

// ============================================================================
// Characters
// ============================================================================

static inline uint32_t char_ucs4(scm_obj_t x) { return (uint32_t)((uintptr_t)x >> 32); }

// char=?
SUBR subr_char_eq(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_char(a1) || !is_char(a2)) throw std::runtime_error("char=?: arguments must be characters");
  return char_ucs4(a1) == char_ucs4(a2) ? scm_true : scm_false;
}

// char-numeric?
SUBR subr_char_numeric_p(scm_obj_t self, scm_obj_t a1) {
  if (!is_char(a1)) throw std::runtime_error("char-numeric?: argument must be a character");
  uint32_t c = char_ucs4(a1);
  return (c >= '0' && c <= '9') ? scm_true : scm_false;
}

// ============================================================================
// Arithmetic extras
// ============================================================================

// max
SUBR subr_max(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1) throw std::runtime_error("max: too few arguments");
  scm_obj_t result = argv[0];
  bool inexact = is_short_flonum(result) || is_long_flonum(result);
  double rval = is_fixnum(result) ? (double)fixnum(result) : flonum(result);
  for (int i = 1; i < argc; i++) {
    scm_obj_t cur = argv[i];
    if (!is_fixnum(cur) && !is_short_flonum(cur) && !is_long_flonum(cur)) throw std::runtime_error("max: arguments must be real numbers");
    if (is_short_flonum(cur) || is_long_flonum(cur)) inexact = true;
    double cval = is_fixnum(cur) ? (double)fixnum(cur) : flonum(cur);
    if (cval > rval) {
      rval = cval;
      result = cur;
    }
  }
  if (inexact && is_fixnum(result)) return make_flonum(rval);
  return result;
}

// ============================================================================
// I/O
// ============================================================================

SUBR subr_write(scm_obj_t self, scm_obj_t a1) {
  printer_t(std::cout).write(a1);
  return scm_undef;
}

SUBR subr_display(scm_obj_t self, scm_obj_t a1) {
  printer_t(std::cout).display(a1);
  return scm_undef;
}

SUBR subr_newline(scm_obj_t self) {
  std::cout << std::endl;
  return scm_undef;
}

// ============================================================================
// GC & System
// ============================================================================

SUBR subr_collect(scm_obj_t self) {
  object_heap_t::current()->collect();
  return scm_undef;
}

SUBR subr_safepoint(scm_obj_t self) {
  object_heap_t::current()->safepoint();
  return scm_undef;
}

SUBR subr_gensym(scm_obj_t self, int argc, scm_obj_t argv[]) {
  static int gensym_counter = 1;
  if (argc > 1) throw std::runtime_error("gensym: too many arguments");
  if (argc == 1 && !is_string(argv[0])) throw std::runtime_error("gensym: argument must be a string");
  const char* prefix = (argc == 0) ? "tmp" : (const char*)string_name(argv[0]);
  struct timeval tv;
  gettimeofday(&tv, NULL);
  char buf[128];
  snprintf(buf, sizeof(buf), "%s_%x%x%x", prefix, (unsigned int)tv.tv_sec, (unsigned int)tv.tv_usec, (unsigned int)gensym_counter++);
  return make_uninterned_symbol(buf);
}

// ============================================================================
// Application & Control
// ============================================================================

SUBR subr_apply(scm_obj_t self, int argc, scm_obj_t argv[]) { return c_apply_helper(argv[0], argc - 1, &argv[1]); }

// ============================================================================
// Initialization & Registration
// ============================================================================

void nanos_t::init_subr() {
  auto reg = [](const char* name, void* func, int req, int opt) {
    c_global_set(make_symbol(name), make_closure(func, req, opt, 0, nullptr, scm_nil, 1));
  };
  auto make_subr = [](void* func, int req, int opt) { return make_closure(func, req, opt, 0, nullptr, scm_nil, 1); };

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

  // pairs & lists
  reg("cons", (void*)subr_cons, 2, 0);
  reg("car", (void*)subr_car, 1, 0);
  reg("cdr", (void*)subr_cdr, 1, 0);
  reg("caar", (void*)subr_caar, 1, 0);
  reg("cadr", (void*)subr_cadr, 1, 0);
  reg("cddr", (void*)subr_cddr, 1, 0);
  reg("cadar", (void*)subr_cadar, 1, 0);
  reg("caddr", (void*)subr_caddr, 1, 0);
  reg("cdddr", (void*)subr_cdddr, 1, 0);
  reg("cadddr", (void*)subr_cadddr, 1, 0);
  reg("set-car!", (void*)subr_set_car_bang, 2, 0);
  reg("length", (void*)subr_length, 1, 0);
  reg("list-ref", (void*)subr_list_ref, 2, 0);
  reg("list->vector", (void*)subr_list_to_vector, 1, 0);
  reg("memq", (void*)subr_memq, 2, 0);
  reg("memv", (void*)subr_memv, 2, 0);
  reg("member", (void*)subr_member, 2, 0);
  reg("assq", (void*)subr_assq, 2, 0);
  reg("assoc", (void*)subr_assoc, 2, 0);
  reg("reverse", (void*)subr_reverse, 1, 0);
  reg("list", (void*)subr_list, 0, 1);
  reg("append", (void*)subr_append, 0, 1);

  // predicates & logic
  reg("not", (void*)subr_not, 1, 0);
  reg("boolean?", (void*)subr_boolean_p, 1, 0);
  reg("char?", (void*)subr_char_p, 1, 0);
  reg("eq?", (void*)subr_eq_p, 2, 0);
  reg("equal?", (void*)subr_equal_p, 2, 0);
  reg("eqv?", (void*)subr_eqv_p, 2, 0);
  reg("exact?", (void*)subr_exact_p, 1, 0);
  reg("inexact?", (void*)subr_inexact_p, 1, 0);
  reg("infinite?", (void*)subr_infinite_p, 1, 0);
  reg("integer?", (void*)subr_integer_p, 1, 0);
  reg("list?", (void*)subr_list_p, 1, 0);
  reg("null?", (void*)subr_null_p, 1, 0);
  reg("number?", (void*)subr_number_p, 1, 0);
  reg("nan?", (void*)subr_nan_p, 1, 0);
  reg("pair?", (void*)subr_pair_p, 1, 0);
  reg("procedure?", (void*)subr_procedure_p, 1, 0);
  reg("real?", (void*)subr_real_p, 1, 0);
  reg("string?", (void*)subr_string_p, 1, 0);
  reg("symbol?", (void*)subr_symbol_p, 1, 0);
  reg("vector?", (void*)subr_vector_p, 1, 0);

  // vectors
  reg("vector", (void*)subr_vector, 0, 1);
  reg("vector-length", (void*)subr_vector_length, 1, 0);
  reg("vector-ref", (void*)subr_vector_ref, 2, 0);
  reg("vector-set!", (void*)subr_vector_set, 3, 0);
  reg("vector->list", (void*)subr_vector_to_list, 1, 0);

  // strings
  reg("string-length", (void*)subr_string_length, 1, 0);
  reg("string-ref", (void*)subr_string_ref, 2, 0);
  reg("string=?", (void*)subr_string_eq, 2, 0);
  reg("string-append", (void*)subr_string_append, 0, 1);
  reg("substring", (void*)subr_substring, 3, 0);
  reg("symbol->string", (void*)subr_symbol_to_string, 1, 0);
  reg("string->symbol", (void*)subr_string_to_symbol, 1, 0);
  reg("number->string", (void*)subr_number_to_string, 1, 1);
  reg("string->number", (void*)subr_string_to_number, 1, 1);

  // characters
  reg("char=?", (void*)subr_char_eq, 2, 0);
  reg("char-numeric?", (void*)subr_char_numeric_p, 1, 0);

  // arithmetic extras
  reg("max", (void*)subr_max, 1, 1);

  // I/O
  reg("write", (void*)subr_write, 1, 0);
  reg("display", (void*)subr_display, 1, 0);
  reg("newline", (void*)subr_newline, 0, 0);

  // GC & system
  reg("collect", (void*)subr_collect, 0, 0);
  reg("safepoint", (void*)subr_safepoint, 0, 0);
  reg("gensym", (void*)subr_gensym, 0, 1);

  // application & control
  reg("apply", (void*)subr_apply, 0, 1);
  reg("call/ec", (void*)subr_call_ec, 1, 0);
  reg("dynamic-wind", (void*)subr_dynamic_wind, 3, 0);
  reg("continuation?", (void*)subr_continuation_p, 1, 0);
  scm_obj_t scm_subr_call_cc = make_subr((void*)subr_call_cc, 1, 0);
  c_global_set(make_symbol("call/cc"), scm_subr_call_cc);
  c_global_set(make_symbol("call-with-current-continuation"), scm_subr_call_cc);
}
