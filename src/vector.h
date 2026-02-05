// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef VECTOR_H_INCLUDED
#define VECTOR_H_INCLUDED

#include "core.h"
#include "object.h"

scm_obj_t vector_ref(scm_obj_t vector, int k);
scm_obj_t vector_set(scm_obj_t vector, int k, scm_obj_t obj);

#endif
