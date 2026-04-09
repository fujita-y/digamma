// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef CONTINUATION_H_INCLUDED
#define CONTINUATION_H_INCLUDED

#include "core.h"
#include "object.h"

#define SUBR extern "C" scm_obj_t

scm_obj_t get_current_winders();
void set_current_winders(scm_obj_t winders);

SUBR subr_call_ec(scm_obj_t self, scm_obj_t a1);
SUBR subr_call_cc(scm_obj_t self, scm_obj_t a1);
SUBR subr_dynamic_wind(scm_obj_t self, scm_obj_t pre, scm_obj_t value, scm_obj_t post);
SUBR subr_continuation_p(scm_obj_t self, scm_obj_t a1);

void restore_continuation(scm_obj_t cont_obj, scm_continuation_rec_t* rec, scm_obj_t val);

#endif
