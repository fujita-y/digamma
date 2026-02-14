// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "nanos_subr.h"
#include "printer.h"

SUBR scm_obj_t subr_num_add(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (is_fixnum(a1) && is_fixnum(a2)) {
    return make_fixnum(fixnum(a1) + fixnum(a2));
  }
  throw std::runtime_error("+: arguments must be fixnums");
}

SUBR scm_obj_t subr_num_sub(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (is_fixnum(a1) && is_fixnum(a2)) {
    return make_fixnum(fixnum(a1) - fixnum(a2));
  }
  throw std::runtime_error("-: arguments must be fixnums");
}

SUBR scm_obj_t subr_num_eq(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (is_fixnum(a1) && is_fixnum(a2)) {
    return (a1 == a2) ? scm_true : scm_false;
  }
  throw std::runtime_error("=: arguments must be fixnums");
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

SUBR scm_obj_t subr_cons(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) { return make_cons(a1, a2); }

SUBR scm_obj_t subr_not(scm_obj_t self, scm_obj_t a1) { return (a1 == scm_false) ? scm_true : scm_false; }

SUBR scm_obj_t subr_eq_p(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) { return (a1 == a2) ? scm_true : scm_false; }

SUBR scm_obj_t subr_pair_p(scm_obj_t self, scm_obj_t a1) { return is_cons(a1) ? scm_true : scm_false; }

SUBR scm_obj_t subr_null_p(scm_obj_t self, scm_obj_t a1) { return (a1 == scm_nil) ? scm_true : scm_false; }

SUBR scm_obj_t subr_cadr(scm_obj_t self, scm_obj_t a1) { return subr_car(self, subr_cdr(self, a1)); }

SUBR scm_obj_t subr_caddr(scm_obj_t self, scm_obj_t a1) { return subr_car(self, subr_cdr(self, subr_cdr(self, a1))); }
