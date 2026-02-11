// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef CODEGEN_AUX_H_INCLUDED
#define CODEGEN_AUX_H_INCLUDED

#include "core.h"
#include "object.h"

extern "C" scm_obj_t c_make_closure(void* code, int argc, int rest, int nsize, scm_obj_t env[], scm_obj_t literals);
extern "C" void c_global_set(scm_obj_t key, scm_obj_t value);
extern "C" scm_obj_t c_global_ref(scm_obj_t key);
extern "C" scm_obj_t c_make_cons(scm_obj_t car, scm_obj_t cdr);
extern "C" scm_obj_t c_make_cell(scm_obj_t value);
extern "C" void c_write_barrier(scm_obj_t obj);
extern "C" scm_obj_t c_construct_rest_list(int count, intptr_t argv[]);
extern "C" scm_obj_t c_make_closure_s1(void* code, int argc);
extern "C" scm_obj_t c_make_closure_s2(void* code, int argc, scm_obj_t literals);

#endif  // CODEGEN_AUX_H_INCLUDED
