// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"

#include "nanos_subr.h"

static inline scm_obj_t add_num(scm_obj_t a1, scm_obj_t a2) {
  if (is_fixnum(a1) && is_fixnum(a2)) {
    return make_fixnum(fixnum(a1) + fixnum(a2));
  }
  return scm_undef;
}

extern "C" scm_obj_t subr_num_add(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) { return add_num(a1, a2); }

static inline scm_obj_t sub_num(scm_obj_t a1, scm_obj_t a2) {
  if (is_fixnum(a1) && is_fixnum(a2)) {
    return make_fixnum(fixnum(a1) - fixnum(a2));
  }
  return scm_undef;
}

extern "C" scm_obj_t subr_num_sub(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) { return sub_num(a1, a2); }

static inline scm_obj_t eq_num(scm_obj_t a1, scm_obj_t a2) {
  if (is_fixnum(a1) && is_fixnum(a2)) {
    return (a1 == a2) ? scm_true : scm_false;
  }
  return scm_false;
}

extern "C" scm_obj_t subr_num_eq(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) { return eq_num(a1, a2); }

extern "C" scm_obj_t subr_list(scm_obj_t self, int argc, scm_obj_t argv[]) {
  scm_obj_t list = scm_nil;
  for (int i = argc - 1; i >= 0; i--) {
    list = make_cons(argv[i], list);
  }
  return list;
}
