// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "nanos_subr.h"
#include <boost/context/continuation.hpp>
#include <boost/context/detail/fcontext.hpp>
#include <cstdio>
#include <sanitizer/hwasan_interface.h>
#include <ucontext.h>
#include "codegen_aux.h"
#include "object_heap.h"
#include "printer.h"

#define CAR(x) (((scm_cons_rec_t*)(x))->car)
#define CDR(x) (((scm_cons_rec_t*)(x))->cdr)

SUBR scm_obj_t subr_num_add(scm_obj_t self, int argc, scm_obj_t argv[]) {
  intptr_t sum = 0;
  for (int i = 0; i < argc; i++) {
    if (is_fixnum(argv[i])) {
      sum += fixnum(argv[i]);
    } else {
      throw std::runtime_error("+: arguments must be fixnums");
    }
  }
  return make_fixnum(sum);
}

SUBR scm_obj_t subr_num_sub(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc == 0) throw std::runtime_error("-: too few arguments");
  if (!is_fixnum(argv[0])) throw std::runtime_error("-: arguments must be fixnums");

  intptr_t result = fixnum(argv[0]);

  if (argc == 1) {
    return make_fixnum(-result);
  }

  for (int i = 1; i < argc; i++) {
    if (is_fixnum(argv[i])) {
      result -= fixnum(argv[i]);
    } else {
      throw std::runtime_error("-: arguments must be fixnums");
    }
  }
  return make_fixnum(result);
}

SUBR scm_obj_t subr_num_eq(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1) throw std::runtime_error("=: too few arguments");

  if (!is_fixnum(argv[0])) throw std::runtime_error("=: arguments must be fixnums");
  scm_obj_t first = argv[0];

  for (int i = 1; i < argc; i++) {
    if (!is_fixnum(argv[i])) throw std::runtime_error("=: arguments must be fixnums");
    if (argv[i] != first) return scm_false;
  }
  return scm_true;
}

SUBR scm_obj_t subr_num_mul(scm_obj_t self, int argc, scm_obj_t argv[]) {
  intptr_t p = 1;
  for (int i = 0; i < argc; i++) {
    if (is_fixnum(argv[i])) {
      p *= fixnum(argv[i]);
    } else {
      throw std::runtime_error("*: arguments must be fixnums");
    }
  }
  return make_fixnum(p);
}

SUBR scm_obj_t subr_num_div(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc == 0) throw std::runtime_error("/: too few arguments");
  if (!is_fixnum(argv[0])) throw std::runtime_error("/: arguments must be fixnums");

  intptr_t result = fixnum(argv[0]);

  if (argc == 1) {
    return make_fixnum(1 / result);
  }

  for (int i = 1; i < argc; i++) {
    if (is_fixnum(argv[i])) {
      intptr_t d = fixnum(argv[i]);
      if (d == 0) throw std::runtime_error("/: division by zero");
      result /= d;
    } else {
      throw std::runtime_error("/: arguments must be fixnums");
    }
  }
  return make_fixnum(result);
}

SUBR scm_obj_t subr_list(scm_obj_t self, int argc, scm_obj_t argv[]) {
  scm_obj_t list = scm_nil;
  for (int i = argc - 1; i >= 0; i--) {
    list = make_cons(argv[i], list);
  }
  return list;
}

SUBR scm_obj_t subr_car(scm_obj_t self, scm_obj_t a1) {
  if (is_cons(a1)) {
    scm_cons_rec_t* cons = (scm_cons_rec_t*)a1;
    return cons->car;
  }
  throw std::runtime_error("car: argument must be a cons cell");
}

SUBR scm_obj_t subr_cdr(scm_obj_t self, scm_obj_t a1) {
  if (is_cons(a1)) {
    scm_cons_rec_t* cons = (scm_cons_rec_t*)a1;
    return cons->cdr;
  }
  throw std::runtime_error("cdr: argument must be a cons cell");
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

SUBR scm_obj_t subr_append(scm_obj_t self, int argc, scm_obj_t argv[]) {
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

SUBR scm_obj_t subr_cons(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) { return make_cons(a1, a2); }

SUBR scm_obj_t subr_not(scm_obj_t self, scm_obj_t a1) { return (a1 == scm_false) ? scm_true : scm_false; }

SUBR scm_obj_t subr_eq_p(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) { return (a1 == a2) ? scm_true : scm_false; }

SUBR scm_obj_t subr_pair_p(scm_obj_t self, scm_obj_t a1) { return is_cons(a1) ? scm_true : scm_false; }

SUBR scm_obj_t subr_null_p(scm_obj_t self, scm_obj_t a1) { return (a1 == scm_nil) ? scm_true : scm_false; }

SUBR scm_obj_t subr_cadr(scm_obj_t self, scm_obj_t a1) { return subr_car(self, subr_cdr(self, a1)); }

SUBR scm_obj_t subr_caddr(scm_obj_t self, scm_obj_t a1) { return subr_car(self, subr_cdr(self, subr_cdr(self, a1))); }

SUBR scm_obj_t subr_apply(scm_obj_t self, int argc, scm_obj_t argv[]) { return c_apply_helper(argv[0], argc - 1, &argv[1]); }

SUBR scm_obj_t subr_write(scm_obj_t self, scm_obj_t a1) {
  printer_t(std::cout).print(a1);
  return scm_undef;
}

SUBR scm_obj_t subr_newline(scm_obj_t self) {
  std::cout << std::endl;
  return scm_undef;
}

SUBR scm_obj_t subr_collect(scm_obj_t self) {
  object_heap_t::current()->collect();
  return scm_undef;
}

SUBR scm_obj_t subr_safepoint(scm_obj_t self) {
  object_heap_t::current()->safepoint();
  return scm_undef;
}
