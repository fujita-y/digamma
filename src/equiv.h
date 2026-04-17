// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef EQUIV_H_INCLUDED
#define EQUIV_H_INCLUDED

#include "core.h"
#include "object.h"

class object_heap_t;

bool eqv_p(scm_obj_t obj1, scm_obj_t obj2);
bool equal_p(scm_obj_t lst1, scm_obj_t lst2);

#endif
