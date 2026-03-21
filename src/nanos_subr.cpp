// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "codegen.h"
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
#include <random>
#include <sstream>

static int safe_list_length(scm_obj_t lst) {
  int len = 0;
  scm_obj_t slow = lst;
  scm_obj_t fast = lst;
  while (true) {
    if (fast == scm_nil) return len;
    if (!is_cons(fast)) return -1;
    fast = CDR(fast);
    len++;
    if (fast == scm_nil) return len;
    if (!is_cons(fast)) return -1;
    fast = CDR(fast);
    len++;
    slow = CDR(slow);
    if (slow == fast) return -2;  // cycle
  }
}

// ============================================================================
// Arithmetic  - R6RS 11.7
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
// Numeric Comparisons  - R6RS 11.7.3
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
  intptr_t first = fixnum(argv[0]);

  for (int i = 1; i < argc; i++) {
    if (!is_fixnum(argv[i])) [[unlikely]]
      throw std::runtime_error("<: arguments must be fixnums");
    if (first < fixnum(argv[i])) {
      first = fixnum(argv[i]);
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
  intptr_t first = fixnum(argv[0]);

  for (int i = 1; i < argc; i++) {
    if (!is_fixnum(argv[i])) [[unlikely]]
      throw std::runtime_error(">: arguments must be fixnums");
    if (first > fixnum(argv[i])) {
      first = fixnum(argv[i]);
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
  intptr_t first = fixnum(argv[0]);

  for (int i = 1; i < argc; i++) {
    if (!is_fixnum(argv[i])) [[unlikely]]
      throw std::runtime_error("<=: arguments must be fixnums");
    if (first <= fixnum(argv[i])) {
      first = fixnum(argv[i]);
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
  intptr_t first = fixnum(argv[0]);

  for (int i = 1; i < argc; i++) {
    if (!is_fixnum(argv[i])) [[unlikely]]
      throw std::runtime_error(">=: arguments must be fixnums");
    if (first >= fixnum(argv[i])) {
      first = fixnum(argv[i]);
      continue;
    }
    return scm_false;
  }
  return scm_true;
}

// ============================================================================
// Pairs & Lists  - R6RS 11.9
// ============================================================================

// cons  - R6RS 11.9
SUBR subr_cons(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) { return make_cons(a1, a2); }

// car  - R6RS 11.9
SUBR subr_car(scm_obj_t self, scm_obj_t a1) {
  if (is_cons(a1)) {
    scm_cons_rec_t* cons = (scm_cons_rec_t*)a1;
    return cons->car;
  }
  throw std::runtime_error("car: argument must be a cons cell");
}

// cdr  - R6RS 11.9
SUBR subr_cdr(scm_obj_t self, scm_obj_t a1) {
  if (is_cons(a1)) {
    scm_cons_rec_t* cons = (scm_cons_rec_t*)a1;
    return cons->cdr;
  }
  throw std::runtime_error("cdr: argument must be a cons cell, but got " + scm_obj_to_string(a1));
}

// cadr, caddr, caar, cadar, caddar, cadddr, cddr, cdddr, cdar, cddar, cdddar  - R6RS 11.9
SUBR subr_cadr(scm_obj_t self, scm_obj_t a1) { return subr_car(self, subr_cdr(self, a1)); }
SUBR subr_caddr(scm_obj_t self, scm_obj_t a1) { return subr_car(self, subr_cdr(self, subr_cdr(self, a1))); }
SUBR subr_caar(scm_obj_t self, scm_obj_t a1) { return subr_car(self, subr_car(self, a1)); }
SUBR subr_cadar(scm_obj_t self, scm_obj_t a1) { return subr_car(self, subr_cdr(self, subr_car(self, a1))); }
SUBR subr_caddar(scm_obj_t self, scm_obj_t a1) { return subr_car(self, subr_cdr(self, subr_cdr(self, subr_car(self, a1)))); }
SUBR subr_cadddr(scm_obj_t self, scm_obj_t a1) { return subr_car(self, subr_cdr(self, subr_cdr(self, subr_cdr(self, a1)))); }
SUBR subr_cddr(scm_obj_t self, scm_obj_t a1) { return subr_cdr(self, subr_cdr(self, a1)); }
SUBR subr_cdddr(scm_obj_t self, scm_obj_t a1) { return subr_cdr(self, subr_cdr(self, subr_cdr(self, a1))); }
SUBR subr_cdar(scm_obj_t self, scm_obj_t a1) { return subr_cdr(self, subr_car(self, a1)); }
SUBR subr_cddar(scm_obj_t self, scm_obj_t a1) { return subr_cdr(self, subr_cdr(self, subr_car(self, a1))); }
SUBR subr_cdddar(scm_obj_t self, scm_obj_t a1) { return subr_cdr(self, subr_cdr(self, subr_cdr(self, subr_car(self, a1)))); }

// list  - R6RS 11.9
SUBR subr_list(scm_obj_t self, int argc, scm_obj_t argv[]) {
  scm_obj_t list = scm_nil;
  for (int i = argc - 1; i >= 0; i--) {
    list = make_cons(argv[i], list);
  }
  return list;
}

static scm_obj_t do_transpose(int each_len, int argc, scm_obj_t argv[]) {
  scm_obj_t ans = scm_nil;
  scm_obj_t ans_tail = scm_nil;
  for (int i = 0; i < each_len; i++) {
    scm_obj_t elt = make_cons(CAR(argv[0]), scm_nil);
    scm_obj_t elt_tail = elt;
    argv[0] = CDR(argv[0]);
    for (int n = 1; n < argc; n++) {
      CDR(elt_tail) = make_cons(CAR(argv[n]), scm_nil);
      elt_tail = CDR(elt_tail);
      argv[n] = CDR(argv[n]);
    }
    if (ans == scm_nil) {
      ans = make_cons(elt, scm_nil);
      ans_tail = ans;
    } else {
      CDR(ans_tail) = make_cons(elt, scm_nil);
      ans_tail = CDR(ans_tail);
    }
  }
  return ans;
}

SUBR subr_list_transpose(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc >= 1) {
    int each_len = safe_list_length(argv[0]);
    if (each_len >= 0) {
      for (int i = 1; i < argc; i++) {
        int len = safe_list_length(argv[i]);
        if (len >= 0) {
          if (len == each_len) continue;
          throw std::runtime_error("list-transpose: all lists must have same length");
        }
        throw std::runtime_error("list-transpose: arguments must be proper lists");
      }
      return do_transpose(each_len, argc, argv);
    }
    throw std::runtime_error("list-transpose: arguments must be proper lists");
  }
  throw std::runtime_error("list-transpose: wrong number of arguments");
}

SUBR subr_list_transpose_plus(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc >= 1) {
    int each_len = safe_list_length(argv[0]);
    if (each_len >= 0) {
      for (int i = 1; i < argc; i++) {
        int len = safe_list_length(argv[i]);
        if (len >= 0) {
          if (len == each_len) continue;
          return scm_false;
        }
        return scm_false;
      }
      return do_transpose(each_len, argc, argv);
    }
    return scm_false;
  }
  throw std::runtime_error("list-transpose+: wrong number of arguments");
}

SUBR subr_cons_ast(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc > 0) {
    if (argc == 1) return argv[0];
    scm_obj_t obj = make_cons(argv[0], scm_nil);
    scm_obj_t tail = obj;
    for (int i = 1; i < argc - 1; i++) {
      scm_obj_t e = make_cons(argv[i], scm_nil);
      CDR(tail) = e;
      tail = e;
    }
    CDR(tail) = argv[argc - 1];
    return obj;
  }
  throw std::runtime_error("cons*: wrong number of arguments");
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

// append  - R6RS 11.9
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

// set-car!  - R6RS 11.9
SUBR subr_set_car(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (is_cons(a1)) {
    object_heap_t::current()->write_barrier(a2);
    CAR(a1) = a2;
    return scm_unspecified;
  }
  throw std::runtime_error("set-car!: argument must be a cons cell");
}

// set-cdr!  - R6RS 11.9
SUBR subr_set_cdr(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (is_cons(a1)) {
    object_heap_t::current()->write_barrier(a2);
    CDR(a1) = a2;
    return scm_unspecified;
  }
  throw std::runtime_error("set-cdr!: argument must be a cons cell");
}

// length  - R6RS 11.9
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

// list-ref  - R6RS 11.9
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

// list->vector  - R6RS 11.13
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

// memq  - R6RS 11.9
SUBR subr_memq(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  scm_obj_t cur = a2;
  while (is_cons(cur)) {
    if (CAR(cur) == a1) return cur;
    cur = CDR(cur);
  }
  if (cur != scm_nil)
    throw std::runtime_error("memq: second argument must be a proper list: (" + scm_obj_to_string(a1) + " " + scm_obj_to_string(a2) + ")");
  return scm_false;
}

// memv  - R6RS 11.9
SUBR subr_memv(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  scm_obj_t cur = a2;
  while (is_cons(cur)) {
    if (eqv_p(CAR(cur), a1)) return cur;
    cur = CDR(cur);
  }
  if (cur != scm_nil) throw std::runtime_error("memv: second argument must be a proper list");
  return scm_false;
}

// member  - R6RS 11.9
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

// assq  - R6RS 11.9
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

// assv  - R6RS 11.9
SUBR subr_assv(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  scm_obj_t cur = a2;
  while (is_cons(cur)) {
    scm_obj_t pair = CAR(cur);
    if (!is_cons(pair)) throw std::runtime_error("assv: alist must contain pairs");
    if (eqv_p(CAR(pair), a1)) return pair;
    cur = CDR(cur);
  }
  if (cur != scm_nil) throw std::runtime_error("assv: second argument must be a proper list");
  return scm_false;
}

// assoc  - R6RS 11.9
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

// reverse  - R6RS 11.9
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

// not  - R6RS 11.8
SUBR subr_not(scm_obj_t self, scm_obj_t a1) { return (a1 == scm_false) ? scm_true : scm_false; }

// boolean?  - R6RS 11.8
SUBR subr_boolean_p(scm_obj_t self, scm_obj_t a1) { return (a1 == scm_true || a1 == scm_false) ? scm_true : scm_false; }

// char?  - R6RS 11.11
SUBR subr_char_p(scm_obj_t self, scm_obj_t a1) { return is_char(a1) ? scm_true : scm_false; }

// eq?  - R6RS 11.5
SUBR subr_eq_p(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) { return (a1 == a2) ? scm_true : scm_false; }

// eqv?  - R6RS 11.5
SUBR subr_eqv_p(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) { return eqv_p(a1, a2) ? scm_true : scm_false; }

// equal?  - R6RS 11.5
SUBR subr_equal_p(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  scm_obj_t visited = make_hashtable(address_hash, address_equiv, 4);
  return equal_p(visited, a1, a2) ? scm_true : scm_false;
}

// exact?  - R6RS 11.7.2
SUBR subr_exact_p(scm_obj_t self, scm_obj_t a1) {
  if (is_fixnum(a1)) return scm_true;
  if (is_short_flonum(a1) || is_long_flonum(a1)) return scm_false;
  throw std::runtime_error("exact?: argument must be a number");
}

// inexact?  - R6RS 11.7.2
SUBR subr_inexact_p(scm_obj_t self, scm_obj_t a1) {
  if (is_short_flonum(a1) || is_long_flonum(a1)) return scm_true;
  if (is_fixnum(a1)) return scm_false;
  throw std::runtime_error("inexact?: argument must be a number");
}

// infinite?  - R6RS 11.7.3
SUBR subr_infinite_p(scm_obj_t self, scm_obj_t a1) {
  if (is_fixnum(a1)) return scm_false;
  if (is_short_flonum(a1)) return std::isinf(flonum(a1)) ? scm_true : scm_false;
  if (is_long_flonum(a1)) return std::isinf(flonum(a1)) ? scm_true : scm_false;
  throw std::runtime_error("infinite?: argument must be a real number");
}

// fixnum?  - Nanos extension
SUBR subr_fixnum_p(scm_obj_t self, scm_obj_t a1) { return is_fixnum(a1) ? scm_true : scm_false; }

// integer?  - R6RS 11.7.2
SUBR subr_integer_p(scm_obj_t self, scm_obj_t a1) {
  if (is_fixnum(a1)) return scm_true;
  if (is_short_flonum(a1) || is_long_flonum(a1)) {
    double d = flonum(a1);
    return (std::isfinite(d) && d == std::trunc(d)) ? scm_true : scm_false;
  }
  return scm_false;
}

// list?  - R6RS 11.9 (tortoise-and-hare cycle detection)
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

// null?  - R6RS 11.9
SUBR subr_null_p(scm_obj_t self, scm_obj_t a1) { return (a1 == scm_nil) ? scm_true : scm_false; }

// number?  - R6RS 11.7.2
SUBR subr_number_p(scm_obj_t self, scm_obj_t a1) { return (is_fixnum(a1) || is_short_flonum(a1) || is_long_flonum(a1)) ? scm_true : scm_false; }

// nan?  - R6RS 11.7.3
SUBR subr_nan_p(scm_obj_t self, scm_obj_t a1) {
  if (is_fixnum(a1)) return scm_false;
  if (is_short_flonum(a1) || is_long_flonum(a1)) return std::isnan(flonum(a1)) ? scm_true : scm_false;
  throw std::runtime_error("nan?: argument must be a real number");
}

// pair?  - R6RS 11.9
SUBR subr_pair_p(scm_obj_t self, scm_obj_t a1) { return is_cons(a1) ? scm_true : scm_false; }

// procedure?  - R6RS 11.6
SUBR subr_procedure_p(scm_obj_t self, scm_obj_t a1) { return (is_closure(a1) || is_continuation(a1) || is_escape(a1)) ? scm_true : scm_false; }

// real?  - R6RS 11.7.2
SUBR subr_real_p(scm_obj_t self, scm_obj_t a1) { return (is_fixnum(a1) || is_short_flonum(a1) || is_long_flonum(a1)) ? scm_true : scm_false; }

// string?  - R6RS 11.12
SUBR subr_string_p(scm_obj_t self, scm_obj_t a1) { return is_string(a1) ? scm_true : scm_false; }

// symbol?  - R6RS 11.10
SUBR subr_symbol_p(scm_obj_t self, scm_obj_t a1) { return is_symbol(a1) ? scm_true : scm_false; }

// vector?  - R6RS 11.13
SUBR subr_vector_p(scm_obj_t self, scm_obj_t a1) { return is_vector(a1) ? scm_true : scm_false; }

// undefined - Nanos extension
SUBR subr_undefined(scm_obj_t self) { return scm_undef; }

// unspecified - Nanos extension
SUBR subr_unspecified(scm_obj_t self) { return scm_unspecified; }

// undefined? - Nanos extension
SUBR subr_undefined_p(scm_obj_t self, scm_obj_t a1) { return (a1 == scm_undef) ? scm_true : scm_false; }

// unspecified? - Nanos extension
SUBR subr_unspecified_p(scm_obj_t self, scm_obj_t a1) { return (a1 == scm_unspecified) ? scm_true : scm_false; }

// ============================================================================
// Vectors  - R6RS 11.13
// ============================================================================

// make-vector  - R6RS 11.13
SUBR subr_make_vector(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1 || argc > 2) throw std::runtime_error("make-vector: wrong number of arguments");
  if (!is_fixnum(argv[0])) throw std::runtime_error("make-vector: first argument must be an exact integer");
  intptr_t n = fixnum(argv[0]);
  if (n < 0) throw std::runtime_error("make-vector: length must be non-negative");
  scm_obj_t fill = (argc == 2) ? argv[1] : scm_unspecified;
  return make_vector((int)n, fill);
}

// vector  - R6RS 11.13
SUBR subr_vector(scm_obj_t self, int argc, scm_obj_t argv[]) {
  scm_obj_t v = make_vector(argc, scm_undef);
  scm_obj_t* elts = vector_elts(v);
  for (int i = 0; i < argc; i++) elts[i] = argv[i];
  return v;
}

// vector-length  - R6RS 11.13
SUBR subr_vector_length(scm_obj_t self, scm_obj_t a1) {
  if (is_vector(a1)) return make_fixnum(vector_nsize(a1));
  throw std::runtime_error("vector-length: argument must be a vector");
}

// vector-ref  - R6RS 11.13
SUBR subr_vector_ref(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_vector(a1)) throw std::runtime_error("vector-ref: first argument must be a vector");
  if (!is_fixnum(a2)) throw std::runtime_error("vector-ref: second argument must be an exact integer");
  intptr_t n = fixnum(a2);
  int sz = vector_nsize(a1);
  if (n < 0 || n >= sz) throw std::runtime_error("vector-ref: index out of bounds");
  return vector_elts(a1)[n];
}

// vector-set!  - R6RS 11.13
SUBR subr_vector_set(scm_obj_t self, scm_obj_t a1, scm_obj_t a2, scm_obj_t a3) {
  if (!is_vector(a1)) throw std::runtime_error("vector-set!: first argument must be a vector");
  if (!is_fixnum(a2)) throw std::runtime_error("vector-set!: second argument must be an exact integer");
  intptr_t n = fixnum(a2);
  int sz = vector_nsize(a1);
  if (n < 0 || n >= sz) throw std::runtime_error("vector-set!: index out of bounds");
  object_heap_t::current()->write_barrier(a3);
  vector_elts(a1)[n] = a3;
  return scm_unspecified;
}

// vector->list  - R6RS 11.13
SUBR subr_vector_to_list(scm_obj_t self, scm_obj_t a1) {
  if (!is_vector(a1)) throw std::runtime_error("vector->list: argument must be a vector");
  int n = vector_nsize(a1);
  scm_obj_t* elts = vector_elts(a1);
  scm_obj_t lst = scm_nil;
  for (int i = n - 1; i >= 0; i--) lst = make_cons(elts[i], lst);
  return lst;
}

// ============================================================================
// Strings  - R6RS 11.12
// ============================================================================

// string-length  - R6RS 11.12 (byte length - ASCII/UTF-8 byte count)
SUBR subr_string_length(scm_obj_t self, scm_obj_t a1) {
  if (!is_string(a1)) throw std::runtime_error("string-length: argument must be a string");
  return make_fixnum((intptr_t)strlen((const char*)string_name(a1)));
}

// string-ref  - R6RS 11.12 (byte index → char)
SUBR subr_string_ref(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_string(a1)) throw std::runtime_error("string-ref: first argument must be a string");
  if (!is_fixnum(a2)) throw std::runtime_error("string-ref: second argument must be an exact integer");
  const char* s = (const char*)string_name(a1);
  intptr_t idx = fixnum(a2);
  intptr_t len = (intptr_t)strlen(s);
  if (idx < 0 || idx >= len)
    throw std::runtime_error("string-ref: index out of bounds: " + scm_obj_to_string(a1) + ", " + scm_obj_to_string(a2));
  // Decode UTF-8 character at byte position idx
  const uint8_t* p = (const uint8_t*)s + idx;
  uint32_t ucs4 = *p;  // [TODO] unicode support
  return make_char(ucs4);
}

// string=?  - R6RS 11.12
SUBR subr_string_eq(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_string(a1)) throw std::runtime_error("string=?: arguments must be strings");
  if (!is_string(a2)) throw std::runtime_error("string=?: arguments must be strings");
  return strcmp((const char*)string_name(a1), (const char*)string_name(a2)) == 0 ? scm_true : scm_false;
}

// string-append  - R6RS 11.12
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

// substring  - R6RS 11.12 (byte offsets)
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

// symbol->string  - R6RS 11.10
SUBR subr_symbol_to_string(scm_obj_t self, scm_obj_t a1) {
  if (!is_symbol(a1)) throw std::runtime_error("symbol->string: argument must be a symbol");
  return make_string((const char*)symbol_name(a1));
}

// string->symbol  - R6RS 11.10
SUBR subr_string_to_symbol(scm_obj_t self, scm_obj_t a1) {
  if (!is_string(a1)) throw std::runtime_error("string->symbol: argument must be a string");
  return make_symbol((const char*)string_name(a1));
}

// number->string  - R6RS 11.7.3 (radix 2/8/10/16; fixnum or flonum)
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

// string->number  - R6RS 11.7.3 (radix 2/8/10/16; returns #f on failure)
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
// Characters  - R6RS 11.11
// ============================================================================

static inline uint32_t char_ucs4(scm_obj_t x) { return (uint32_t)((uintptr_t)x >> 32); }

// char=?  - R6RS 11.11
SUBR subr_char_eq(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_char(a1) || !is_char(a2)) throw std::runtime_error("char=?: arguments must be characters");
  return char_ucs4(a1) == char_ucs4(a2) ? scm_true : scm_false;
}

// char-numeric?  - R6RS 11.11
SUBR subr_char_numeric_p(scm_obj_t self, scm_obj_t a1) {
  if (!is_char(a1)) throw std::runtime_error("char-numeric?: argument must be a character");
  uint32_t c = char_ucs4(a1);
  return (c >= '0' && c <= '9') ? scm_true : scm_false;
}

// ============================================================================
// Arithmetic extras
// ============================================================================

// max  - R6RS 11.7.3
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

// min  - R6RS 11.7.3
SUBR subr_min(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1) throw std::runtime_error("min: too few arguments");
  scm_obj_t result = argv[0];
  bool inexact = is_short_flonum(result) || is_long_flonum(result);
  double rval = is_fixnum(result) ? (double)fixnum(result) : flonum(result);
  for (int i = 1; i < argc; i++) {
    scm_obj_t cur = argv[i];
    if (!is_fixnum(cur) && !is_short_flonum(cur) && !is_long_flonum(cur)) throw std::runtime_error("min: arguments must be real numbers");
    if (is_short_flonum(cur) || is_long_flonum(cur)) inexact = true;
    double cval = is_fixnum(cur) ? (double)fixnum(cur) : flonum(cur);
    if (cval < rval) {
      rval = cval;
      result = cur;
    }
  }
  if (inexact && is_fixnum(result)) return make_flonum(rval);
  return result;
}

// ============================================================================
// I/O  - R6RS 8
// ============================================================================

// write  - R6RS 8.3
SUBR subr_write(scm_obj_t self, scm_obj_t a1) {
  printer_t(std::cout).write(a1);
  return scm_unspecified;
}

// display  - R6RS 8.3
SUBR subr_display(scm_obj_t self, scm_obj_t a1) {
  printer_t(std::cout).display(a1);
  return scm_unspecified;
}

// newline  - R6RS 8.3
SUBR subr_newline(scm_obj_t self) {
  std::cout << std::endl;
  return scm_unspecified;
}

// ============================================================================
// GC & System
// ============================================================================

SUBR subr_collect(scm_obj_t self) {
  object_heap_t::current()->collect();
  return scm_unspecified;
}

SUBR subr_safepoint(scm_obj_t self) {
  object_heap_t::current()->safepoint();
  return scm_unspecified;
}

SUBR subr_gensym(scm_obj_t self, int argc, scm_obj_t argv[]) {
  static int gensym_counter = 1;
  if (argc > 1) throw std::runtime_error("gensym: too many arguments");
  if (argc == 1 && !is_string(argv[0])) throw std::runtime_error("gensym: argument must be a string");
  const char* prefix = (argc == 0) ? "gensym" : (const char*)string_name(argv[0]);
  struct timeval tv;
  gettimeofday(&tv, NULL);
  char buf[128];
  snprintf(buf, sizeof(buf), "%s_%x%x%x", prefix, (unsigned int)tv.tv_sec, (unsigned int)tv.tv_usec, (unsigned int)gensym_counter++);
  return make_uninterned_symbol(buf);
}

SUBR subr_uuid(scm_obj_t self) {
  static thread_local std::random_device rd;
  static thread_local std::mt19937 gen(rd());
  std::uniform_int_distribution<> dis(0, 15);
  std::uniform_int_distribution<> dis_variant(8, 11);

  char buf[37];
  const char* hex = "0123456789abcdef";
  for (int i = 0; i < 36; i++) {
    if (i == 8 || i == 13 || i == 18 || i == 23) {
      buf[i] = '-';
    } else if (i == 14) {
      buf[i] = '4';  // Version 4
    } else if (i == 19) {
      buf[i] = hex[dis_variant(gen)];  // Variant: 8, 9, a, or b
    } else {
      buf[i] = hex[dis(gen)];
    }
  }
  buf[36] = '\0';
  return make_string(buf);
}

SUBR subr_exit(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc == 0) exit(0);
  scm_obj_t a1 = argv[0];
  if (is_fixnum(a1)) exit((int)fixnum(a1));
  if (a1 == scm_false) exit(1);
  exit(0);
}

// ============================================================================
// Error  - R6RS 11.14 (simplified: throws runtime_error instead of raising a condition)
// ============================================================================

// error  - R6RS 11.14 signature, simplified: throws std::runtime_error
// (error message irritant ...)
SUBR subr_error(scm_obj_t self, int argc, scm_obj_t argv[]) {
  std::ostringstream oss;
  oss << "error: ";
  if (argc >= 1) {
    printer_t(oss).display(argv[0]);  // message: display mode (no quotes on strings)
  }
  for (int i = 1; i < argc; i++) {
    oss << " ";
    printer_t(oss).write(argv[i]);  // irritants: write mode (quoted)
  }
  throw std::runtime_error(oss.str());
}

// ============================================================================
// Application & Control
// ============================================================================

SUBR subr_apply(scm_obj_t self, int argc, scm_obj_t argv[]) { return c_apply_helper(argv[0], argc - 1, &argv[1]); }

// ============================================================================
// Hashtables
// ============================================================================

// hashtable?  - R6RS 12.1
SUBR subr_hashtable_p(scm_obj_t self, scm_obj_t a1) { return is_hashtable(a1) ? scm_true : scm_false; }

// equal-hash  - R6RS 12.4
SUBR subr_equal_hash(scm_obj_t self, scm_obj_t a1) { return make_fixnum((intptr_t)equal_hash(a1, INT32_MAX)); }

// make-eq-hashtable  - R6RS 12.2
SUBR subr_make_eq_hashtable(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc > 1) throw std::runtime_error("make-eq-hashtable: too many arguments");
  int capacity = 32;
  if (argc == 1) {
    if (!is_fixnum(argv[0])) throw std::runtime_error("make-eq-hashtable: capacity must be an exact integer");
    intptr_t cap = fixnum(argv[0]);
    if (cap < 0) throw std::runtime_error("make-eq-hashtable: capacity must be non-negative");
    capacity = (int)cap;
  }
  return make_hashtable(address_hash, address_equiv, capacity);
}

// make-eqv-hashtable  - R6RS 12.2
SUBR subr_make_eqv_hashtable(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc > 1) throw std::runtime_error("make-eqv-hashtable: too many arguments");
  int capacity = 32;
  if (argc == 1) {
    if (!is_fixnum(argv[0])) throw std::runtime_error("make-eqv-hashtable: capacity must be an exact integer");
    intptr_t cap = fixnum(argv[0]);
    if (cap < 0) throw std::runtime_error("make-eqv-hashtable: capacity must be non-negative");
    capacity = (int)cap;
  }
  return make_hashtable(eqv_hash, eqv_equiv, capacity);
}

// make-equal-hashtable  - digamma extension
SUBR subr_make_equal_hashtable(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc > 1) throw std::runtime_error("make-equal-hashtable: too many arguments");
  int capacity = 32;
  if (argc == 1) {
    if (!is_fixnum(argv[0])) throw std::runtime_error("make-equal-hashtable: capacity must be an exact integer");
    intptr_t cap = fixnum(argv[0]);
    if (cap < 0) throw std::runtime_error("make-equal-hashtable: capacity must be non-negative");
    capacity = (int)cap;
  }
  return make_hashtable(equal_hash, equal_equiv, capacity);
}

// hashtable-ref  - R6RS 12.3
// (hashtable-ref ht key default)
SUBR subr_hashtable_ref(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 2 || argc > 3) throw std::runtime_error("hashtable-ref: wrong number of arguments");
  if (!is_hashtable(argv[0])) throw std::runtime_error("hashtable-ref: first argument must be a hashtable");
  scm_obj_t default_val = (argc == 3) ? argv[2] : scm_undef;
  scm_obj_t result = hashtable_ref(argv[0], argv[1], default_val);
  if (argc == 2 && result == scm_undef) throw std::runtime_error("hashtable-ref: key not found");
  return result;
}

// hashtable-set!  - R6RS 12.3
// (hashtable-set! ht key value)
SUBR subr_hashtable_set(scm_obj_t self, scm_obj_t a1, scm_obj_t a2, scm_obj_t a3) {
  if (!is_hashtable(a1)) throw std::runtime_error("hashtable-set!: first argument must be a hashtable");
  hashtable_set(a1, a2, a3);
  return scm_unspecified;
}

// hashtable-delete!  - R6RS 12.3
// (hashtable-delete! ht key)
SUBR subr_hashtable_delete(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_hashtable(a1)) throw std::runtime_error("hashtable-delete!: first argument must be a hashtable");
  hashtable_delete(a1, a2);
  return scm_unspecified;
}

// hashtable-contains?  - R6RS 12.3
// (hashtable-contains? ht key)
SUBR subr_hashtable_contains(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_hashtable(a1)) throw std::runtime_error("hashtable-contains?: first argument must be a hashtable");
  scm_obj_t result = hashtable_ref(a1, a2, scm_undef);
  return (result != scm_undef) ? scm_true : scm_false;
}

// hashtable-clear!  - R6RS 12.3
// (hashtable-clear! ht)
SUBR subr_hashtable_clear(scm_obj_t self, scm_obj_t a1) {
  if (!is_hashtable(a1)) throw std::runtime_error("hashtable-clear!: first argument must be a hashtable");
  hashtable_clear(a1);
  return scm_unspecified;
}

// hashtable-entries  - R6RS 12.3
// (hashtable-entries ht) => (values keys-vector values-vector)
SUBR subr_hashtable_entries(scm_obj_t self, scm_obj_t a1) {
  if (!is_hashtable(a1)) throw std::runtime_error("hashtable-entries: argument must be a hashtable");
  scm_hashtable_rec_t* ht = (scm_hashtable_rec_t*)to_address(a1);
  hashtable_aux_t* aux = ht->aux;
  int nsize = aux->capacity;

  // Count live entries
  int live = aux->live;

  // Allocate result vectors
  scm_obj_t keys_vec = make_vector(live, scm_unspecified);
  scm_obj_t vals_vec = make_vector(live, scm_unspecified);
  scm_obj_t* keys_elts = vector_elts(keys_vec);
  scm_obj_t* vals_elts = vector_elts(vals_vec);

  int out = 0;
  for (int i = 0; i < nsize; i++) {
    scm_obj_t k = aux->elts[i];
    if (k == scm_hash_free || k == scm_hash_deleted) continue;
    keys_elts[out] = k;
    vals_elts[out] = aux->elts[i + nsize];
    out++;
  }

  // Pack into a 2-element values object
  scm_obj_t result = make_values(2);
  values_elts(result)[0] = keys_vec;
  values_elts(result)[1] = vals_vec;
  return result;
}

// hashtable->alist  - digamma extension
// (hashtable->alist ht) => ((key . value) ...)
// Like hashtable-entries but returns an association list instead of two vectors.
SUBR subr_hashtable_alist(scm_obj_t self, scm_obj_t a1) {
  if (!is_hashtable(a1)) throw std::runtime_error("hashtable->alist: argument must be a hashtable");
  scm_hashtable_rec_t* ht = (scm_hashtable_rec_t*)to_address(a1);
  hashtable_aux_t* aux = ht->aux;
  int nsize = aux->capacity;

  scm_obj_t head = scm_nil;
  scm_obj_t tail = scm_nil;
  for (int i = 0; i < nsize; i++) {
    scm_obj_t k = aux->elts[i];
    if (k == scm_hash_free || k == scm_hash_deleted) continue;
    scm_obj_t pair = make_cons(k, aux->elts[i + nsize]);
    scm_obj_t cell = make_cons(pair, scm_nil);
    if (head == scm_nil) {
      head = cell;
      tail = cell;
    } else {
      CDR(tail) = cell;
      tail = cell;
    }
  }
  return head;
}

// ============================================================================
// Environment Access
// ============================================================================

// make-environment
SUBR subr_make_environment(scm_obj_t self, scm_obj_t a1) {
  if (!is_symbol(a1)) throw std::runtime_error("make-environment: argument must be a symbol");
  return make_environment(a1);
}

// copy-environment-variables!
SUBR subr_copy_environment_variables(scm_obj_t self, scm_obj_t a1, scm_obj_t a2, scm_obj_t a3) {
  if (!is_environment(a1)) throw std::runtime_error("copy-environment-variables!: first argument must be an environment");
  if (!is_environment(a2)) throw std::runtime_error("copy-environment-variables!: second argument must be an environment");
  scm_environment_rec_t* dst = (scm_environment_rec_t*)to_address(a1);
  scm_environment_rec_t* src = (scm_environment_rec_t*)to_address(a2);
  scm_obj_t cur = a3;
  while (is_cons(cur)) {
    scm_obj_t key = CAR(cur);
    if (!is_symbol(key)) throw std::runtime_error("copy-environment-variables!: list elements must be symbols");
    scm_obj_t cell = hashtable_ref(src->variables, key, scm_undef);
    if (cell != scm_undef) {
      scm_obj_t val = cell_value(cell);
      if (is_closure(val)) {
        hashtable_set(dst->variables, key, make_cell(val));
      } else {
        hashtable_set(dst->variables, key, cell);
      }
    }
    cur = CDR(cur);
  }
  if (cur != scm_nil) throw std::runtime_error("copy-environment-variables!: third argument must be a proper list");
  return scm_unspecified;
}

// copy-environment-macros!
SUBR subr_copy_environment_macros(scm_obj_t self, scm_obj_t a1, scm_obj_t a2, scm_obj_t a3) {
  if (!is_environment(a1)) throw std::runtime_error("copy-environment-macros!: first argument must be an environment");
  if (!is_environment(a2)) throw std::runtime_error("copy-environment-macros!: second argument must be an environment");
  scm_environment_rec_t* dst = (scm_environment_rec_t*)to_address(a1);
  scm_environment_rec_t* src = (scm_environment_rec_t*)to_address(a2);
  scm_obj_t cur = a3;
  while (is_cons(cur)) {
    scm_obj_t key = CAR(cur);
    if (!is_symbol(key)) throw std::runtime_error("copy-environment-macros!: list elements must be symbols");
    scm_obj_t val = hashtable_ref(src->macros, key, scm_undef);
    if (val != scm_undef) {
      hashtable_set(dst->macros, key, val);
    }
    cur = CDR(cur);
  }
  if (cur != scm_nil) throw std::runtime_error("copy-environment-macros!: third argument must be a proper list");
  return scm_unspecified;
}

// environment-macros
SUBR subr_environment_macros(scm_obj_t self, scm_obj_t a1) {
  if (!is_environment(a1)) throw std::runtime_error("environment-macros: argument must be an environment");
  scm_environment_rec_t* env = (scm_environment_rec_t*)to_address(a1);
  return env->macros;
}

// environment-variables
SUBR subr_environment_variables(scm_obj_t self, scm_obj_t a1) {
  if (!is_environment(a1)) throw std::runtime_error("environment-variables: argument must be an environment");
  scm_environment_rec_t* env = (scm_environment_rec_t*)to_address(a1);
  return env->variables;
}

// current-environment
SUBR subr_current_environment(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc == 0) {
    return object_heap_t::current()->m_current_environment;
  } else if (argc == 1) {
    if (!is_environment(argv[0])) throw std::runtime_error("current-environment: argument must be an environment");
    object_heap_t::current()->write_barrier(argv[0]);
    object_heap_t::current()->m_current_environment = argv[0];
    return scm_unspecified;
  } else {
    throw std::runtime_error("current-environment: wrong number of arguments");
  }
}

// environment-macro-set!  - digamma core
// (environment-macro-set! name transformer)
SUBR subr_environment_macro_set(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_symbol(a1)) throw std::runtime_error("environment-macro-set!: first argument must be a symbol");
  object_heap_t::current()->environment_macro_set(a1, a2);
  return scm_unspecified;
}

// environment-macro-ref  - digamma core
// (environment-macro-ref name) => transformer or scm_undef
SUBR subr_environment_macro_ref(scm_obj_t self, scm_obj_t a1) {
  if (!is_symbol(a1)) throw std::runtime_error("environment-macro-ref: argument must be a symbol");
  return object_heap_t::current()->environment_macro_ref(a1);
}

// environment-variable-set!  - digamma core
// (environment-variable-set! name value)
SUBR subr_environment_variable_set(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_symbol(a1)) throw std::runtime_error("environment-variable-set!: first argument must be a symbol");
  object_heap_t::current()->environment_variable_set(a1, a2);
  return scm_unspecified;
}

// environment-variable-ref  - digamma core
// (environment-variable-ref name) => value or scm_undef
SUBR subr_environment_variable_ref(scm_obj_t self, scm_obj_t a1) {
  if (!is_symbol(a1)) throw std::runtime_error("environment-variable-ref: argument must be a symbol");
  return object_heap_t::current()->environment_variable_ref(a1);
}

// environment-macro-contains?  - digamma core
// (environment-macro-contains? name) => #t or #f
SUBR subr_environment_macro_contains(scm_obj_t self, scm_obj_t a1) {
  if (!is_symbol(a1)) throw std::runtime_error("environment-macro-contains?: argument must be a symbol");
  return object_heap_t::current()->environment_macro_contains(a1) ? scm_true : scm_false;
}

// environment-variable-contains?  - digamma core
// (environment-variable-contains? name) => #t or #f
SUBR subr_environment_variable_contains(scm_obj_t self, scm_obj_t a1) {
  if (!is_symbol(a1)) throw std::runtime_error("environment-variable-contains?: argument must be a symbol");
  return object_heap_t::current()->environment_variable_contains(a1) ? scm_true : scm_false;
}

// interaction-environment - R6RS 11.16
SUBR subr_interaction_environment(scm_obj_t self) { return object_heap_t::current()->m_interaction_environment; }

// system-environment
SUBR subr_system_environment(scm_obj_t self) { return object_heap_t::current()->m_system_environment; }

// ============================================================================
// Multiple Return Values
// ============================================================================

// values  - R6RS 11.14
// (values obj ...)  - pack zero or more objects into a values object
SUBR subr_values(scm_obj_t self, int argc, scm_obj_t argv[]) {
  // Single value: return it directly (avoids wrapping for common case)
  if (argc == 1) return argv[0];
  scm_obj_t v = make_values(argc);
  scm_obj_t* elts = values_elts(v);
  for (int i = 0; i < argc; i++) elts[i] = argv[i];
  return v;
}

// call-with-values  - R6RS 11.14
// (call-with-values producer consumer)
// Calls (producer), then applies consumer to the resulting values.
SUBR subr_call_with_values(scm_obj_t self, scm_obj_t producer, scm_obj_t consumer) {
  if (!is_closure(producer)) throw std::runtime_error("call-with-values: first argument must be a procedure");
  if (!is_closure(consumer)) throw std::runtime_error("call-with-values: second argument must be a procedure");

  codegen_t* cg = codegen_t::current();
  if (!cg) throw std::runtime_error("call-with-values: JIT not initialized");
  auto bridge = cg->call_closure_bridge();

  // Step 1: call producer thunk
  scm_obj_t result = (scm_obj_t)bridge(producer, 0, nullptr);

  // Step 2: spread values (or single value) to consumer
  if (is_values(result)) {
    int n = values_nsize(result);
    scm_obj_t* elts = values_elts(result);
    return (scm_obj_t)bridge(consumer, n, elts);
  } else {
    return (scm_obj_t)bridge(consumer, 1, &result);
  }
}

// codegen-and-run - Nanos extension
SUBR subr_codegen_and_run(scm_obj_t self, scm_obj_t coreform) {
  try {
    auto func = codegen_t::current()->compile(coreform);
    intptr_t result = func();
    return (scm_obj_t)result;
  } catch (std::exception& e) {
    throw std::runtime_error(e.what());
  }
}

// ============================================================================
// Initialization & Registration
// ============================================================================

void nanos_t::init_subr() {
  object_heap_t* heap = object_heap_t::current();
  auto reg = [heap](const char* name, void* func, int req, int opt) {
    heap->environment_variable_set(make_symbol(name), make_closure(func, req, opt, 0, nullptr, scm_nil, 1));
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
  reg("cdar", (void*)subr_cdar, 1, 0);
  reg("cadr", (void*)subr_cadr, 1, 0);
  reg("cddr", (void*)subr_cddr, 1, 0);
  reg("cadar", (void*)subr_cadar, 1, 0);
  reg("cddar", (void*)subr_cddar, 1, 0);
  reg("caddar", (void*)subr_caddar, 1, 0);
  reg("cdddar", (void*)subr_cdddar, 1, 0);
  reg("caddr", (void*)subr_caddr, 1, 0);
  reg("cdddr", (void*)subr_cdddr, 1, 0);
  reg("cadddr", (void*)subr_cadddr, 1, 0);
  reg("set-car!", (void*)subr_set_car, 2, 0);
  reg("set-cdr!", (void*)subr_set_cdr, 2, 0);
  reg("length", (void*)subr_length, 1, 0);
  reg("list", (void*)subr_list, 0, 1);
  reg("list-transpose", (void*)subr_list_transpose, 1, 1);
  reg("list-transpose+", (void*)subr_list_transpose_plus, 1, 1);
  reg("cons*", (void*)subr_cons_ast, 1, 1);
  reg("list-ref", (void*)subr_list_ref, 2, 0);
  reg("list->vector", (void*)subr_list_to_vector, 1, 0);
  reg("memq", (void*)subr_memq, 2, 0);
  reg("memv", (void*)subr_memv, 2, 0);
  reg("member", (void*)subr_member, 2, 0);
  reg("assq", (void*)subr_assq, 2, 0);
  reg("assv", (void*)subr_assv, 2, 0);
  reg("assoc", (void*)subr_assoc, 2, 0);
  reg("reverse", (void*)subr_reverse, 1, 0);
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
  reg("fixnum?", (void*)subr_fixnum_p, 1, 0);
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
  reg("undefined", (void*)subr_undefined, 0, 0);
  reg("unspecified", (void*)subr_unspecified, 0, 0);
  reg("undefined?", (void*)subr_undefined_p, 1, 0);
  reg("unspecified?", (void*)subr_unspecified_p, 1, 0);

  // vectors
  reg("vector", (void*)subr_vector, 0, 1);
  reg("make-vector", (void*)subr_make_vector, 1, 1);
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
  reg("min", (void*)subr_min, 1, 1);

  // I/O
  reg("write", (void*)subr_write, 1, 0);
  reg("display", (void*)subr_display, 1, 0);
  reg("newline", (void*)subr_newline, 0, 0);

  // hashtables
  reg("hashtable?", (void*)subr_hashtable_p, 1, 0);
  reg("equal-hash", (void*)subr_equal_hash, 1, 0);
  reg("make-eq-hashtable", (void*)subr_make_eq_hashtable, 0, 1);
  reg("make-eqv-hashtable", (void*)subr_make_eqv_hashtable, 0, 1);
  reg("make-equal-hashtable", (void*)subr_make_equal_hashtable, 0, 1);
  reg("hashtable-ref", (void*)subr_hashtable_ref, 2, 1);
  reg("hashtable-set!", (void*)subr_hashtable_set, 3, 0);
  reg("hashtable-delete!", (void*)subr_hashtable_delete, 2, 0);
  reg("hashtable-contains?", (void*)subr_hashtable_contains, 2, 0);
  reg("hashtable-clear!", (void*)subr_hashtable_clear, 1, 0);
  reg("hashtable-entries", (void*)subr_hashtable_entries, 1, 0);
  reg("hashtable->alist", (void*)subr_hashtable_alist, 1, 0);

  // environment access
  reg("make-environment", (void*)subr_make_environment, 1, 0);
  reg("copy-environment-variables!", (void*)subr_copy_environment_variables, 3, 0);
  reg("copy-environment-macros!", (void*)subr_copy_environment_macros, 3, 0);
  reg("environment-macros", (void*)subr_environment_macros, 1, 0);
  reg("environment-variables", (void*)subr_environment_variables, 1, 0);
  reg("current-environment", (void*)subr_current_environment, 0, 1);
  reg("environment-macro-set!", (void*)subr_environment_macro_set, 2, 0);
  reg("environment-macro-ref", (void*)subr_environment_macro_ref, 1, 0);
  reg("environment-macro-contains?", (void*)subr_environment_macro_contains, 1, 0);
  reg("environment-variable-set!", (void*)subr_environment_variable_set, 2, 0);
  reg("environment-variable-ref", (void*)subr_environment_variable_ref, 1, 0);
  reg("environment-variable-contains?", (void*)subr_environment_variable_contains, 1, 0);
  reg("interaction-environment", (void*)subr_interaction_environment, 0, 0);
  reg("system-environment", (void*)subr_system_environment, 0, 0);

  // multiple return values
  reg("values", (void*)subr_values, 0, 1);
  reg("call-with-values", (void*)subr_call_with_values, 2, 0);

  // GC & system
  reg("collect", (void*)subr_collect, 0, 0);
  reg("safepoint", (void*)subr_safepoint, 0, 0);
  reg("gensym", (void*)subr_gensym, 0, 1);
  reg("uuid", (void*)subr_uuid, 0, 0);
  reg("exit", (void*)subr_exit, 0, 1);

  // application & control
  reg("error", (void*)subr_error, 1, 1);
  reg("apply", (void*)subr_apply, 0, 1);
  reg("call/ec", (void*)subr_call_ec, 1, 0);
  reg("dynamic-wind", (void*)subr_dynamic_wind, 3, 0);
  reg("continuation?", (void*)subr_continuation_p, 1, 0);
  reg("codegen-and-run", (void*)subr_codegen_and_run, 1, 0);
  scm_obj_t scm_subr_call_cc = make_subr((void*)subr_call_cc, 1, 0);
  heap->environment_variable_set(make_symbol("call/cc"), scm_subr_call_cc);
  heap->environment_variable_set(make_symbol("call-with-current-continuation"), scm_subr_call_cc);
}
