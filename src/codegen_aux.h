// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef CODEGEN_AUX_H_INCLUDED
#define CODEGEN_AUX_H_INCLUDED

#include "core.h"
#include "object.h"

extern "C" scm_obj_t c_make_closure(void* code, int argc, int rest, int nenv, scm_obj_t env[]);
extern "C" scm_obj_t c_make_closure_s1(void* code, int argc);
extern "C" scm_obj_t c_make_cons(scm_obj_t car, scm_obj_t cdr);
extern "C" scm_obj_t c_make_cell(scm_obj_t value);
extern "C" scm_obj_t c_construct_rest_list(int count, intptr_t argv[]);
extern "C" void c_write_barrier(scm_obj_t obj);
extern "C" scm_obj_t c_apply_helper(scm_obj_t proc, int argc, scm_obj_t argv[]);
extern "C" void c_safepoint(void);
extern "C" scm_obj_t c_call_closure_thunk_0(scm_obj_t proc);
extern "C" void c_test_application(scm_obj_t proc, int argc, const char* name);
extern "C" void c_unbound_variable_error(const char* name);
extern "C" scm_obj_t c_num_add(scm_obj_t arg1, scm_obj_t arg2);
extern "C" scm_obj_t c_num_sub(scm_obj_t arg1, scm_obj_t arg2);
extern "C" scm_obj_t c_num_mul(scm_obj_t arg1, scm_obj_t arg2);
extern "C" scm_obj_t c_num_eq(scm_obj_t arg1, scm_obj_t arg2);
extern "C" scm_obj_t c_num_lt(scm_obj_t arg1, scm_obj_t arg2);
extern "C" scm_obj_t c_num_gt(scm_obj_t arg1, scm_obj_t arg2);
extern "C" scm_obj_t c_num_le(scm_obj_t arg1, scm_obj_t arg2);
extern "C" scm_obj_t c_num_ge(scm_obj_t arg1, scm_obj_t arg2);
extern "C" void c_error_car(scm_obj_t obj);
extern "C" void c_error_cdr(scm_obj_t obj);
extern "C" void c_error_vector_ref(scm_obj_t vec, scm_obj_t idx);
extern "C" void c_error_vector_set(scm_obj_t vec, scm_obj_t idx, scm_obj_t val);
extern "C" void c_error_tuple_ref(scm_obj_t tup, scm_obj_t idx);
extern "C" void c_error_tuple_set(scm_obj_t tup, scm_obj_t idx, scm_obj_t val);
extern "C" scm_obj_t c_append2(scm_obj_t arg1, scm_obj_t arg2);

bool is_side_effect_free_aux_helper(const char* name);
bool is_never_return_aux_helper(const char* name);
void reset_safepoint_cache();  // clears the thread_local s_stop_the_world cache

#endif  // CODEGEN_AUX_H_INCLUDED
