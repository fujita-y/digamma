// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "codegen.h"
#include "context.h"
#include "continuation.h"
#include "equiv.h"
#include "list.h"
#include "nanos.h"
#include "object_heap.h"
#include "subr.h"

#include <cerrno>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <random>
#include <sys/time.h>

// fixnum?  - Nanos extension
SUBR subr_fixnum_p(scm_obj_t self, scm_obj_t a1) { return is_fixnum(a1) ? scm_true : scm_false; }

// undefined - Nanos extension
SUBR subr_undefined(scm_obj_t self) { return scm_undef; }

// unspecified - Nanos extension
SUBR subr_unspecified(scm_obj_t self) { return scm_unspecified; }

// undefined? - Nanos extension
SUBR subr_undefined_p(scm_obj_t self, scm_obj_t a1) { return (a1 == scm_undef) ? scm_true : scm_false; }

// unspecified? - Nanos extension
SUBR subr_unspecified_p(scm_obj_t self, scm_obj_t a1) { return (a1 == scm_unspecified) ? scm_true : scm_false; }

// ============================================================================
// GC & System
// ============================================================================

SUBR subr_collect(scm_obj_t self) {
  object_heap_t::current()->collect();
  return scm_unspecified;
}

SUBR subr_safepoint(scm_obj_t self) {
  object_heap_t::current()->safepoint();
  return scm_unspecified;
}

SUBR subr_gensym(scm_obj_t self, int argc, scm_obj_t argv[]) {
  static int gensym_counter = 1;
  if (argc > 1) throw std::runtime_error("gensym: too many arguments");
  if (argc == 1 && !is_string(argv[0])) throw std::runtime_error("gensym: argument must be a string");
  const char* prefix = (argc == 0) ? "gensym" : (const char*)string_name(argv[0]);
  struct timeval tv;
  gettimeofday(&tv, NULL);
  char buf[128];
  snprintf(buf, sizeof(buf), "%s_%x%x%x", prefix, (unsigned int)tv.tv_sec, (unsigned int)tv.tv_usec, (unsigned int)gensym_counter++);
  return make_uninterned_symbol(buf);
}

SUBR subr_uuid(scm_obj_t self) {
  static thread_local std::random_device rd;
  static thread_local std::mt19937 gen(rd());
  std::uniform_int_distribution<> dis(0, 15);
  std::uniform_int_distribution<> dis_variant(8, 11);

  char buf[37];
  const char* hex = "0123456789abcdef";
  for (int i = 0; i < 36; i++) {
    if (i == 8 || i == 13 || i == 18 || i == 23) {
      buf[i] = '-';
    } else if (i == 14) {
      buf[i] = '4';  // Version 4
    } else if (i == 19) {
      buf[i] = hex[dis_variant(gen)];  // Variant: 8, 9, a, or b
    } else {
      buf[i] = hex[dis(gen)];
    }
  }
  buf[36] = '\0';
  return make_string(buf);
}

SUBR subr_exit(scm_obj_t self, int argc, scm_obj_t argv[]) {
  nanos_t::current()->destroy();
  if (argc == 0) exit(0);
  scm_obj_t a1 = argv[0];
  if (is_fixnum(a1)) exit((int)fixnum(a1));
  if (a1 == scm_false) exit(1);
  exit(0);
}

// codegen-and-run - Nanos extension
SUBR subr_codegen_and_run(scm_obj_t self, scm_obj_t inst_list) {
  try {
    scoped_gc_protect p(inst_list);
    compiled_code_t func = codegen_t::current()->compile(inst_list);
    scm_obj_t result = (scm_obj_t)func.release_and_run();
    return result;
  } catch (std::exception& e) {
    throw std::runtime_error(e.what());
  }
}

// ============================================================================
// Misc
// ============================================================================

// cyclic-object?
SUBR subr_cyclic_object_p(scm_obj_t self, scm_obj_t a1) { return cyclic_object_p(a1) ? scm_true : scm_false; }

// ============================================================================
// Initialization
// ============================================================================

void init_subr_misc() {
  auto reg = [](const char* name, void* func, int req, int opt) {
    context::environment_variable_set(make_symbol(name), make_closure(func, req, opt, 0, nullptr, 1));
  };

  reg("fixnum?", (void*)subr_fixnum_p, 1, 0);
  reg("undefined", (void*)subr_undefined, 0, 0);
  reg("unspecified", (void*)subr_unspecified, 0, 0);
  reg("undefined?", (void*)subr_undefined_p, 1, 0);
  reg("unspecified?", (void*)subr_unspecified_p, 1, 0);
  reg("collect", (void*)subr_collect, 0, 0);
  reg("safepoint", (void*)subr_safepoint, 0, 0);
  reg("gensym", (void*)subr_gensym, 0, 1);
  reg("uuid", (void*)subr_uuid, 0, 0);
  reg("exit", (void*)subr_exit, 0, 1);
  reg("continuation?", (void*)subr_continuation_p, 1, 0);
  reg("codegen-and-run", (void*)subr_codegen_and_run, 1, 0);
  reg("cyclic-object?", (void*)subr_cyclic_object_p, 1, 0);
}
