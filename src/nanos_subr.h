// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef NANOS_SUBR_H_INCLUDED
#define NANOS_SUBR_H_INCLUDED

#include "core.h"
#include "object.h"

#define SUBR extern "C"

SUBR scm_obj_t subr_num_add(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR scm_obj_t subr_num_sub(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR scm_obj_t subr_num_eq(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR scm_obj_t subr_list(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR scm_obj_t subr_car(scm_obj_t self, scm_obj_t a1);
SUBR scm_obj_t subr_cdr(scm_obj_t self, scm_obj_t a1);
SUBR scm_obj_t subr_not(scm_obj_t self, scm_obj_t a1);
SUBR scm_obj_t subr_eq_p(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR scm_obj_t subr_pair_p(scm_obj_t self, scm_obj_t a1);
SUBR scm_obj_t subr_null_p(scm_obj_t self, scm_obj_t a1);
SUBR scm_obj_t subr_cadr(scm_obj_t self, scm_obj_t a1);
SUBR scm_obj_t subr_caddr(scm_obj_t self, scm_obj_t a1);
SUBR scm_obj_t subr_cons(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR scm_obj_t subr_num_mul(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR scm_obj_t subr_num_div(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR scm_obj_t subr_apply(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR scm_obj_t subr_append(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR scm_obj_t subr_write(scm_obj_t self, scm_obj_t a1);
SUBR scm_obj_t subr_newline(scm_obj_t self);
SUBR scm_obj_t subr_collect(scm_obj_t self);
SUBR scm_obj_t subr_safepoint(scm_obj_t self);
SUBR scm_obj_t subr_call_ec(scm_obj_t self, scm_obj_t a1);
#endif  // NANOS_SUBR_H_INCLUDED
