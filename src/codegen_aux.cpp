// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "codegen_aux.h"
#include "object_heap.h"

extern "C" scm_obj_t c_make_closure(void* code, int argc, int rest, int nsize, scm_obj_t env[], scm_obj_t literals) {
  return make_closure(code, argc, rest, nsize, env, literals);
}

extern "C" scm_obj_t c_make_closure_s1(void* code, int argc) { return make_closure(code, argc, 0, 0, nullptr, scm_nil); }

extern "C" scm_obj_t c_make_closure_s2(void* code, int argc, scm_obj_t literals) { return make_closure(code, argc, 0, 0, nullptr, literals); }

extern "C" void c_global_set(scm_obj_t key, scm_obj_t value) {
  assert(is_symbol(key));
  object_heap_t* heap = object_heap_t::current();
  heap->environment_variable_set(key, value);
}

extern "C" scm_obj_t c_make_cons(scm_obj_t car, scm_obj_t cdr) { return make_cons(car, cdr); }

extern "C" scm_obj_t c_make_cell(scm_obj_t value) { return make_cell(value); }

extern "C" void c_write_barrier(scm_obj_t obj) { object_heap_t::current()->write_barrier(obj); }

extern "C" scm_obj_t c_construct_rest_list(int count, intptr_t argv[]) {
  if (count <= 0) return scm_nil;
  scm_obj_t list = scm_nil;
  for (int i = count - 1; i >= 0; i--) {
    list = make_cons(argv[i], list);
  }
  return list;
}
