// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "nanos_subr.h"
#include "codegen.h"
#include "codegen_aux.h"

static thread_local codegen_t* s_codegen = nullptr;

void nanos_set_codegen(void* cg) { s_codegen = (codegen_t*)cg; }

SUBR scm_obj_t subr_num_add(scm_obj_t self, int argc, scm_obj_t argv[]) {
  intptr_t sum = 0;
  for (int i = 0; i < argc; i++) {
    if (is_fixnum(argv[i])) {
      sum += fixnum(argv[i]);
    } else {
      throw std::runtime_error("+: arguments must be fixnums");
    }
  }
  return make_fixnum(sum);
}

SUBR scm_obj_t subr_num_sub(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc == 0) throw std::runtime_error("-: too few arguments");
  if (!is_fixnum(argv[0])) throw std::runtime_error("-: arguments must be fixnums");

  intptr_t result = fixnum(argv[0]);

  if (argc == 1) {
    return make_fixnum(-result);
  }

  for (int i = 1; i < argc; i++) {
    if (is_fixnum(argv[i])) {
      result -= fixnum(argv[i]);
    } else {
      throw std::runtime_error("-: arguments must be fixnums");
    }
  }
  return make_fixnum(result);
}

SUBR scm_obj_t subr_num_eq(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1) throw std::runtime_error("=: too few arguments");

  if (!is_fixnum(argv[0])) throw std::runtime_error("=: arguments must be fixnums");
  scm_obj_t first = argv[0];

  for (int i = 1; i < argc; i++) {
    if (!is_fixnum(argv[i])) throw std::runtime_error("=: arguments must be fixnums");
    if (argv[i] != first) return scm_false;
  }
  return scm_true;
}

SUBR scm_obj_t subr_list(scm_obj_t self, int argc, scm_obj_t argv[]) {
  scm_obj_t list = scm_nil;
  for (int i = argc - 1; i >= 0; i--) {
    list = make_cons(argv[i], list);
  }
  return list;
}

SUBR scm_obj_t subr_car(scm_obj_t self, scm_obj_t a1) {
  if (is_cons(a1)) {
    scm_cons_rec_t* cons = (scm_cons_rec_t*)a1;
    return cons->car;
  }
  throw std::runtime_error("car: argument must be a cons cell");
}

SUBR scm_obj_t subr_cdr(scm_obj_t self, scm_obj_t a1) {
  if (is_cons(a1)) {
    scm_cons_rec_t* cons = (scm_cons_rec_t*)a1;
    return cons->cdr;
  }
  throw std::runtime_error("cdr: argument must be a cons cell");
}

SUBR scm_obj_t subr_cons(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) { return make_cons(a1, a2); }

SUBR scm_obj_t subr_not(scm_obj_t self, scm_obj_t a1) { return (a1 == scm_false) ? scm_true : scm_false; }

SUBR scm_obj_t subr_eq_p(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) { return (a1 == a2) ? scm_true : scm_false; }

SUBR scm_obj_t subr_pair_p(scm_obj_t self, scm_obj_t a1) { return is_cons(a1) ? scm_true : scm_false; }

SUBR scm_obj_t subr_null_p(scm_obj_t self, scm_obj_t a1) { return (a1 == scm_nil) ? scm_true : scm_false; }

SUBR scm_obj_t subr_cadr(scm_obj_t self, scm_obj_t a1) { return subr_car(self, subr_cdr(self, a1)); }

SUBR scm_obj_t subr_caddr(scm_obj_t self, scm_obj_t a1) { return subr_car(self, subr_cdr(self, subr_cdr(self, a1))); }

#define CAR(x) (((scm_cons_rec_t*)(x))->car)
#define CDR(x) (((scm_cons_rec_t*)(x))->cdr)

SUBR scm_obj_t subr_apply(scm_obj_t self, int argc, scm_obj_t argv[]) { return c_apply_helper(argv[0], argc - 1, &argv[1]); }

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

  if (!s_codegen) throw std::runtime_error("apply: JIT not initialized");

  void* bridge_ptr = s_codegen->get_call_closure_bridge_ptr();
  using bridge_func_t = intptr_t (*)(scm_obj_t, int, scm_obj_t*);
  auto bridge = (bridge_func_t)bridge_ptr;

  return (scm_obj_t)bridge(proc, args.size(), args.data());
}