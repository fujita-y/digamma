// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "vector.h"
#include "object_heap.h"

scm_obj_t vector_ref(scm_obj_t vec, int k) {
  assert(is_vector(vec));
  scm_vector_rec_t* rec = (scm_vector_rec_t*)to_address(vec);
  scm_obj_t obj = rec->elts[k];
  return obj;
}

scm_obj_t vector_set(scm_obj_t vec, int k, scm_obj_t obj) {
  assert(is_vector(vec));
  scm_vector_rec_t* rec = (scm_vector_rec_t*)to_address(vec);
  object_heap_t::current()->write_barrier(obj);
  rec->elts[k] = obj;
  return scm_unspecified;
}