// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef SUBR_H_INCLUDED
#define SUBR_H_INCLUDED

#include "core.h"

#define SUBR extern "C" scm_obj_t

void init_subr_base();
void init_subr_hash();
void init_subr_cxr();
void init_subr_env();
void init_subr_io();
void init_subr_misc();

#endif  // SUBR_H_INCLUDED
