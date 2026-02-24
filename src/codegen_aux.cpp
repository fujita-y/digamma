// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "codegen_aux.h"
#include <boost/context/continuation.hpp>
#include "codegen.h"
#include "object_heap.h"

#define CAR(x) (((scm_cons_rec_t*)(x))->car)
#define CDR(x) (((scm_cons_rec_t*)(x))->cdr)

static thread_local bool* s_stop_the_world = nullptr;

extern "C" void c_safepoint(void) {
  if (s_stop_the_world != nullptr) [[likely]] {
    if (*s_stop_the_world) [[unlikely]] {
      object_heap_t::current()->safepoint();
    }
    return;
  }
  object_heap_t* heap = object_heap_t::current();
  s_stop_the_world = heap->stop_the_world_ptr();
  if (*s_stop_the_world) heap->safepoint();
}

extern "C" scm_obj_t c_make_closure(void* code, int argc, int rest, int nsize, scm_obj_t env[], scm_obj_t literals) {
  return make_closure(code, argc, rest, nsize, env, literals, 0);
}

extern "C" scm_obj_t c_make_closure_s1(void* code, int argc) { return make_closure(code, argc, 0, 0, nullptr, scm_nil, 0); }

extern "C" scm_obj_t c_make_closure_s2(void* code, int argc, scm_obj_t literals) { return make_closure(code, argc, 0, 0, nullptr, literals, 0); }

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

extern "C" scm_obj_t c_apply_helper(scm_obj_t proc, int argc, scm_obj_t argv[]) {
  if (argc < 1) {
    throw std::runtime_error("apply: too few arguments");
  }

  scm_obj_t list = argv[argc - 1];  // Last argument must be a list

  std::vector<scm_obj_t> args;
  for (int i = 0; i < argc - 1; i++) {
    args.push_back(argv[i]);
  }

  scm_obj_t curr = list;
  while (is_cons(curr)) {
    args.push_back(CAR(curr));
    curr = CDR(curr);
  }
  if (curr != scm_nil) throw std::runtime_error("apply: last argument must be a proper list");

  codegen_t* cg = codegen_t::current();
  if (!cg) throw std::runtime_error("apply: JIT not initialized");

  void* bridge_ptr = cg->get_call_closure_bridge_ptr();
  using bridge_func_t = intptr_t (*)(scm_obj_t, int, scm_obj_t*);
  auto bridge = (bridge_func_t)bridge_ptr;

  return (scm_obj_t)bridge(proc, args.size(), args.data());
}

extern "C" scm_obj_t c_call_closure_thunk_0(scm_obj_t proc) {
  codegen_t* cg = codegen_t::current();
  if (!cg) throw std::runtime_error("c_call_closure_thunk_0: JIT not initialized");
  void* bridge_ptr = cg->get_call_closure_bridge_ptr();
  using bridge_func_t = intptr_t (*)(scm_obj_t, int, scm_obj_t*);
  auto bridge = (bridge_func_t)bridge_ptr;
  return (scm_obj_t)bridge(proc, 0, nullptr);
}

extern "C" void c_test_application(scm_obj_t proc, int argc) {
  if (!is_closure(proc)) [[unlikely]] {
    throw std::runtime_error("error in codegen: attempt to call a non-procedure");
  }
  scm_closure_rec_t* closure = (scm_closure_rec_t*)to_address(proc);
  if (closure->rest) [[unlikely]] {
    if (argc < closure->argc) [[unlikely]] {
      throw std::runtime_error("error in codegen: wrong number of arguments");
    }
  } else {
    if (argc != closure->argc) [[unlikely]] {
      throw std::runtime_error("error in codegen: wrong number of arguments");
    }
  }
}