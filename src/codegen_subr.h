// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef CODEGEN_SUBR_H_INCLUDED
#define CODEGEN_SUBR_H_INCLUDED

#include "core.h"
#include "object.h"

extern "C" scm_obj_t subr_num_add(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
extern "C" scm_obj_t subr_num_sub(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
extern "C" scm_obj_t subr_num_eq(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);

#endif  // CODEGEN_SUBR_H_INCLUDED
