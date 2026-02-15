// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "nanos_subr.h"
#include "codegen.h"

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

SUBR scm_obj_t subr_apply(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 2) {
    throw std::runtime_error("apply: too few arguments");
  }

  scm_obj_t proc = argv[0];
  scm_obj_t list = argv[argc - 1];  // Last argument must be a list

  std::vector<scm_obj_t> args;
  for (int i = 1; i < argc - 1; i++) {
    args.push_back(argv[i]);
  }

  scm_obj_t curr = list;
  while (is_cons(curr)) {
    args.push_back(CAR(curr));
    curr = CDR(curr);
  }
  if (curr != scm_nil) throw std::runtime_error("apply: last argument must be a proper list");

  auto sym_const = make_symbol("const");
  auto sym_call = make_symbol("call");
  auto sym_ret = make_symbol("ret");
  auto sym_r = [&](int i) { return make_symbol(("r" + std::to_string(i)).c_str()); };

  std::vector<scm_obj_t> insts;

  int arg_count = args.size();
  for (int i = 0; i < arg_count; i++) {
    // (const rI arg)
    scm_obj_t r = sym_r(i);
    insts.push_back(make_cons(sym_const, make_cons(r, make_cons(args[i], scm_nil))));
  }

  // Proc in r(arg_count)
  scm_obj_t r_proc = sym_r(arg_count);
  insts.push_back(make_cons(sym_const, make_cons(r_proc, make_cons(proc, scm_nil))));

  // (call r_proc arg_count)
  insts.push_back(make_cons(sym_call, make_cons(r_proc, make_cons(make_fixnum(arg_count), scm_nil))));

  // (ret)
  insts.push_back(make_cons(sym_ret, scm_nil));

  // Convert vector to list
  scm_obj_t scm_inst_list = scm_nil;
  for (auto it = insts.rbegin(); it != insts.rend(); ++it) {
    scm_inst_list = make_cons(*it, scm_inst_list);
  }

  if (!s_codegen) throw std::runtime_error("apply: JIT not initialized");

  intptr_t result = s_codegen->compile(scm_inst_list);

  return (scm_obj_t)result;
}