// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "codegen.h"
#include "context.h"
#include "equiv.h"
#include "exception.h"
#include "list.h"
#include "object_heap.h"
#include "subr.h"
#include "uniq_id.h"

#include <cerrno>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <format>
#include <string>
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
  if (argc > 1) throw std::runtime_error("gensym: too many arguments");
  if (argc == 1 && !is_string(argv[0])) throw std::runtime_error("gensym: argument must be a string");
  const char* prefix = (argc == 0) ? "g" : (const char*)string_name(argv[0]);
  std::string name = std::format("{}_{}", prefix, generate_process_unique_suffix());
  return make_uninterned_symbol(name.c_str());
}

SUBR subr_uuid(scm_obj_t self) { return make_string(generate_uuid().c_str()); }

SUBR subr_exit(scm_obj_t self, int argc, scm_obj_t argv[]) {
  int status = 0;
  if (argc > 1) throw std::runtime_error("exit: too many arguments");
  if (argc == 1) {
    scm_obj_t a1 = argv[0];
    if (is_fixnum(a1)) {
      status = (int)fixnum(a1);
    } else if (a1 == scm_false) {
      status = 1;
    } else {
      throw std::runtime_error("exit: argument must be a fixnum or #f");
    }
  }
  throw nanos_exit_t(status);
}

// codegen-and-run - Nanos extension
SUBR subr_codegen_and_run(scm_obj_t self, scm_obj_t inst_list) {
  scoped_gc_protect protect(inst_list);
  compiled_code_t func = codegen_t::current()->compile(inst_list);
  scm_obj_t result = (scm_obj_t)func.release_and_run();
  return result;
}

// ============================================================================
// Exception Interface
// ============================================================================

// with-cpp-exception-handler - Nanos extension
// (with-cpp-exception-handler handler thunk)
// Calls (thunk). If a C++ std::exception escapes, calls (handler message-string)
// instead of propagating it. nanos_exit_t is always re-thrown.
SUBR subr_with_cpp_exception_handler(scm_obj_t self, scm_obj_t handler, scm_obj_t thunk) {
  if (!is_closure(handler)) throw std::runtime_error("with-cpp-exception-handler: first argument must be a procedure");
  if (!is_closure(thunk)) throw std::runtime_error("with-cpp-exception-handler: second argument must be a procedure");
  codegen_t* cg = codegen_t::current();
  if (!cg) throw std::runtime_error("with-cpp-exception-handler: JIT not initialized");
  auto bridge = cg->call_closure_bridge();
  try {
    return (scm_obj_t)bridge(thunk, 0, nullptr);
  } catch (const nanos_exit_t&) {
    throw;
  } catch (const std::exception& e) {
    scm_obj_t msg = make_string(e.what());
    return (scm_obj_t)bridge(handler, 1, &msg);
  } catch (...) {
    scm_obj_t msg = make_string("unknown exception");
    return (scm_obj_t)bridge(handler, 1, &msg);
  }
}

// ============================================================================
// Tuple
// ============================================================================

// tuple?
SUBR subr_tuple_p(scm_obj_t self, scm_obj_t a1) { return is_tuple(a1) ? scm_true : scm_false; }

// tuple
SUBR subr_tuple(scm_obj_t self, int argc, scm_obj_t argv[]) {
  scm_obj_t t = make_tuple(argc);
  scm_obj_t* elts = tuple_elts(t);
  for (int i = 0; i < argc; i++) elts[i] = argv[i];
  return t;
}

// make-tuple  (same signature as R6RS make-vector)
SUBR subr_make_tuple(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1 || argc > 2) throw std::runtime_error("make-tuple: wrong number of arguments");
  if (!is_fixnum(argv[0])) throw std::runtime_error("make-tuple: first argument must be an exact integer");
  intptr_t n = fixnum(argv[0]);
  if (n < 0) throw std::runtime_error("make-tuple: length must be non-negative");
  scm_obj_t fill = (argc == 2) ? argv[1] : scm_unspecified;
  scm_obj_t t = make_tuple((int)n);
  scm_obj_t* elts = tuple_elts(t);
  for (int i = 0; i < (int)n; i++) elts[i] = fill;
  return t;
}

// tuple-ref
SUBR subr_tuple_ref(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  assert(is_tuple(a1));
  assert(is_fixnum(a2));
  int index = (int)fixnum(a2);
  assert(index >= 0 && index < tuple_nsize(a1));
  return tuple_elts(a1)[index];
}

// tuple-set!
SUBR subr_tuple_set(scm_obj_t self, scm_obj_t a1, scm_obj_t a2, scm_obj_t a3) {
  assert(is_tuple(a1));
  assert(is_fixnum(a2));
  int index = (int)fixnum(a2);
  assert(index >= 0 && index < tuple_nsize(a1));
  object_heap_t::current()->write_barrier(a3);
  tuple_elts(a1)[index] = a3;
  return scm_unspecified;
}

// ============================================================================
// Misc
// ============================================================================

// cyclic-object?
SUBR subr_cyclic_object_p(scm_obj_t self, scm_obj_t a1) { return cyclic_object_p(a1) ? scm_true : scm_false; }

// time-usage
SUBR subr_time_usage(scm_obj_t self) {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  struct rusage usage;
  getrusage(RUSAGE_SELF, &usage);
  return make_list(3, make_flonum((double)tv.tv_sec + tv.tv_usec / 1000000.0),
                   make_flonum((double)usage.ru_utime.tv_sec + usage.ru_utime.tv_usec / 1000000.0),
                   make_flonum((double)usage.ru_stime.tv_sec + usage.ru_stime.tv_usec / 1000000.0));
}

// current-collect-trip-bytes - Nanos extension
SUBR subr_current_collect_trip_bytes(scm_obj_t self, int argc, scm_obj_t argv[]) {
  object_heap_t* heap = object_heap_t::current();
  if (argc == 0) return make_fixnum(heap->m_collect_trip_bytes);
  if (is_fixnum(argv[0])) {
    intptr_t val = fixnum(argv[0]);
    if (val < 0) throw std::runtime_error("current-collect-trip-bytes: argument must be a non-negative integer");
    heap->m_collect_trip_bytes = (uint64_t)val;
    return scm_unspecified;
  }
  throw std::runtime_error("current-collect-trip-bytes: argument must be a fixnum");
}

// ============================================================================
// Initialization
// ============================================================================

void init_subr_misc() {
  auto reg = [](const char* name, void* func, int req, bool opt) {
    context::environment_variable_set(make_symbol(name), make_closure(func, req, opt ? 1 : 0, 0, nullptr, 1));
  };

  reg("fixnum?", (void*)subr_fixnum_p, 1, false);
  reg("undefined", (void*)subr_undefined, 0, false);
  reg("unspecified", (void*)subr_unspecified, 0, false);
  reg("undefined?", (void*)subr_undefined_p, 1, false);
  reg("unspecified?", (void*)subr_unspecified_p, 1, false);
  reg("collect", (void*)subr_collect, 0, false);
  reg("safepoint", (void*)subr_safepoint, 0, false);
  reg("gensym", (void*)subr_gensym, 0, true);
  reg("uuid", (void*)subr_uuid, 0, false);
  reg("exit", (void*)subr_exit, 0, true);
  reg("codegen-and-run", (void*)subr_codegen_and_run, 1, false);
  reg("cyclic-object?", (void*)subr_cyclic_object_p, 1, false);
  reg("time-usage", (void*)subr_time_usage, 0, false);
  reg("with-cpp-exception-handler", (void*)subr_with_cpp_exception_handler, 2, false);
  reg("make-tuple", (void*)subr_make_tuple, 1, true);
  reg("tuple", (void*)subr_tuple, 0, true);
  reg("tuple?", (void*)subr_tuple_p, 1, false);
  reg("tuple-ref", (void*)subr_tuple_ref, 2, false);
  reg("tuple-set!", (void*)subr_tuple_set, 3, false);
  reg("current-collect-trip-bytes", (void*)subr_current_collect_trip_bytes, 0, true);
}
