// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "codegen_aux.h"
#include <boost/context/continuation.hpp>
#include "codegen.h"
#include "object_heap.h"

static thread_local bool* s_stop_the_world = nullptr;

static inline bool starts_with(const char* str, const char* prefix) { return strncmp(str, prefix, strlen(prefix)) == 0; }

bool is_side_effect_free_aux_helper(const char* name) {
  // return true if function no need to call if returned value is not used
  if (starts_with(name, "c_make_closure")) return true;
  if (starts_with(name, "c_make_cons")) return true;
  if (starts_with(name, "c_construct_rest_list")) return true;
  return false;
}

bool is_never_return_aux_helper(const char* name) {
  // return true if function never return
  if (starts_with(name, "c_unbound_variable_error")) return true;
  if (starts_with(name, "c_error_car")) return true;
  if (starts_with(name, "c_error_cdr")) return true;
  return false;
}

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

extern "C" scm_obj_t c_make_closure(void* code, int argc, int rest, int nenv, scm_obj_t env[]) {
  return make_closure(code, argc, rest, nenv, env, 0);
}

extern "C" scm_obj_t c_make_closure_s1(void* code, int argc) { return make_closure(code, argc, 0, 0, nullptr, 0); }

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
    args.push_back(cons_car(curr));
    curr = cons_cdr(curr);
  }
  if (curr != scm_nil) throw std::runtime_error("apply: last argument must be a proper list");

  codegen_t* cg = codegen_t::current();
  if (!cg) throw std::runtime_error("apply: JIT not initialized");

  auto bridge = cg->call_closure_bridge();
  return (scm_obj_t)bridge(proc, args.size(), args.data());
}

extern "C" scm_obj_t c_call_closure_thunk_0(scm_obj_t proc) {
  codegen_t* cg = codegen_t::current();
  if (!cg) throw std::runtime_error("c_call_closure_thunk_0: JIT not initialized");
  auto bridge = cg->call_closure_bridge();
  return (scm_obj_t)bridge(proc, 0, nullptr);
}

extern "C" void c_test_application(scm_obj_t proc, int argc, const char* name) {
  if (!is_closure(proc)) [[unlikely]] {
    throw std::runtime_error("error: attempt to call a non-procedure " + to_string(proc) + " in variable " + name);
  }
  scm_closure_rec_t* closure = (scm_closure_rec_t*)to_address(proc);
  if (closure->rest) [[unlikely]] {
    if (argc < closure->argc) [[unlikely]] {
      throw std::runtime_error("error: too few arguments to apply " + to_string(proc) + " in variable " + name);
    }
  } else {
    if (argc != closure->argc) [[unlikely]] {
      throw std::runtime_error("error: too many arguments to apply " + to_string(proc) + " in variable " + name);
    }
  }
}

extern "C" void c_unbound_variable_error(const char* name) { throw std::runtime_error("error: unbound variable " + std::string(name)); }

extern "C" void c_error_car(scm_obj_t obj) { throw std::runtime_error("error: car: argument must be a pair: " + to_string(obj)); }

extern "C" void c_error_cdr(scm_obj_t obj) { throw std::runtime_error("error: cdr: argument must be a pair: " + to_string(obj)); }

extern "C" scm_obj_t c_append2(scm_obj_t arg1, scm_obj_t arg2) {
  if (arg1 == scm_nil) [[unlikely]] {
    return arg2;
  }
  if (!is_cons(arg1)) [[unlikely]] {
    throw std::runtime_error("error: append: wrong type of arguments: " + to_string(arg1) + " " + to_string(arg2));
  }
  scm_obj_t lst1 = arg1;
  scm_obj_t lst2 = arg2;
  scm_obj_t head = make_cons(cons_car(lst1), scm_nil);
  scm_obj_t tail = head;
  lst1 = cons_cdr(lst1);
  while (lst1 != scm_nil) [[likely]] {
    if (!is_cons(lst1)) [[unlikely]] {
      throw std::runtime_error("error: append: wrong type of arguments: " + to_string(arg1) + " " + to_string(arg2));
    }
    scm_cons_rec_t* tail_cons = (scm_cons_rec_t*)tail;
    tail_cons->cdr = make_cons(cons_car(lst1), scm_nil);
    tail = tail_cons->cdr;
    lst1 = cons_cdr(lst1);
  }
  scm_cons_rec_t* tail_cons = (scm_cons_rec_t*)tail;
  tail_cons->cdr = lst2;
  return head;
}

extern "C" scm_obj_t c_num_add(scm_obj_t arg1, scm_obj_t arg2) {
  if (is_flonum(arg1)) {
    if (is_flonum(arg2)) return make_flonum(flonum(arg1) + flonum(arg2));
    if (is_fixnum(arg2)) return make_flonum(flonum(arg1) + fixnum(arg2));
  } else if (is_flonum(arg2)) {
    if (is_fixnum(arg1)) return make_flonum(fixnum(arg1) + flonum(arg2));
  }
  throw std::runtime_error("+: arguments must be numbers: " + to_string(arg1) + " " + to_string(arg2));
}

extern "C" scm_obj_t c_num_sub(scm_obj_t arg1, scm_obj_t arg2) {
  if (is_flonum(arg1)) {
    if (is_flonum(arg2)) return make_flonum(flonum(arg1) - flonum(arg2));
    if (is_fixnum(arg2)) return make_flonum(flonum(arg1) - fixnum(arg2));
  } else if (is_flonum(arg2)) {
    if (is_fixnum(arg1)) return make_flonum(fixnum(arg1) - flonum(arg2));
  }
  throw std::runtime_error("- : arguments must be numbers: " + to_string(arg1) + " " + to_string(arg2));
}

extern "C" scm_obj_t c_num_mul(scm_obj_t arg1, scm_obj_t arg2) {
  if (is_flonum(arg1)) {
    if (is_flonum(arg2)) return make_flonum(flonum(arg1) * flonum(arg2));
    if (is_fixnum(arg2)) return make_flonum(flonum(arg1) * fixnum(arg2));
  } else if (is_flonum(arg2)) {
    if (is_fixnum(arg1)) return make_flonum(fixnum(arg1) * flonum(arg2));
  }
  throw std::runtime_error("*: arguments must be numbers: " + to_string(arg1) + " " + to_string(arg2));
}

extern "C" scm_obj_t c_num_eq(scm_obj_t arg1, scm_obj_t arg2) {
  double lhs;
  double rhs;
  if (is_fixnum(arg1)) {
    lhs = fixnum(arg1);
  } else if (is_flonum(arg1)) {
    lhs = flonum(arg1);
  } else {
    throw std::runtime_error("=: arguments must be numbers: " + to_string(arg1) + " " + to_string(arg2));
  }
  if (is_fixnum(arg2)) {
    rhs = fixnum(arg2);
  } else if (is_flonum(arg2)) {
    rhs = flonum(arg2);
  } else {
    throw std::runtime_error("=: arguments must be numbers: " + to_string(arg1) + " " + to_string(arg2));
  }
  return lhs == rhs ? scm_true : scm_false;
}

extern "C" scm_obj_t c_num_lt(scm_obj_t arg1, scm_obj_t arg2) {
  double lhs;
  double rhs;
  if (is_fixnum(arg1)) {
    lhs = fixnum(arg1);
  } else if (is_flonum(arg1)) {
    lhs = flonum(arg1);
  } else {
    throw std::runtime_error("<: arguments must be numbers: " + to_string(arg1) + " " + to_string(arg2));
  }
  if (is_fixnum(arg2)) {
    rhs = fixnum(arg2);
  } else if (is_flonum(arg2)) {
    rhs = flonum(arg2);
  } else {
    throw std::runtime_error("<: arguments must be numbers: " + to_string(arg1) + " " + to_string(arg2));
  }
  return lhs < rhs ? scm_true : scm_false;
}

extern "C" scm_obj_t c_num_gt(scm_obj_t arg1, scm_obj_t arg2) {
  double lhs;
  double rhs;
  if (is_fixnum(arg1)) {
    lhs = fixnum(arg1);
  } else if (is_flonum(arg1)) {
    lhs = flonum(arg1);
  } else {
    throw std::runtime_error(">: arguments must be numbers: " + to_string(arg1) + " " + to_string(arg2));
  }
  if (is_fixnum(arg2)) {
    rhs = fixnum(arg2);
  } else if (is_flonum(arg2)) {
    rhs = flonum(arg2);
  } else {
    throw std::runtime_error(">: arguments must be numbers: " + to_string(arg1) + " " + to_string(arg2));
  }
  return lhs > rhs ? scm_true : scm_false;
}

extern "C" scm_obj_t c_num_le(scm_obj_t arg1, scm_obj_t arg2) {
  double lhs;
  double rhs;
  if (is_fixnum(arg1)) {
    lhs = fixnum(arg1);
  } else if (is_flonum(arg1)) {
    lhs = flonum(arg1);
  } else {
    throw std::runtime_error("<=: arguments must be numbers: " + to_string(arg1) + " " + to_string(arg2));
  }
  if (is_fixnum(arg2)) {
    rhs = fixnum(arg2);
  } else if (is_flonum(arg2)) {
    rhs = flonum(arg2);
  } else {
    throw std::runtime_error("<=: arguments must be numbers: " + to_string(arg1) + " " + to_string(arg2));
  }
  return lhs <= rhs ? scm_true : scm_false;
}

extern "C" scm_obj_t c_num_ge(scm_obj_t arg1, scm_obj_t arg2) {
  double lhs;
  double rhs;
  if (is_fixnum(arg1)) {
    lhs = fixnum(arg1);
  } else if (is_flonum(arg1)) {
    lhs = flonum(arg1);
  } else {
    throw std::runtime_error(">=: arguments must be numbers: " + to_string(arg1) + " " + to_string(arg2));
  }
  if (is_fixnum(arg2)) {
    rhs = fixnum(arg2);
  } else if (is_flonum(arg2)) {
    rhs = flonum(arg2);
  } else {
    throw std::runtime_error(">=: arguments must be numbers: " + to_string(arg1) + " " + to_string(arg2));
  }
  return lhs >= rhs ? scm_true : scm_false;
}
