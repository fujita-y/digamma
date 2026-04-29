// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "continuation.h"
#include <cstdio>
#include <sanitizer/hwasan_interface.h>
#include <ucontext.h>
#include "codegen_aux.h"
#include "context.h"
#include "exception.h"
#include "object_heap.h"

thread_local static bool s_restored = false;

extern "C" {
  static scm_obj_t subr_invoke_continuation(scm_obj_t self, int argc, scm_obj_t argv[]);
  static scm_obj_t subr_invoke_escape_continuation(scm_obj_t self, int argc, scm_obj_t argv[]);
}

// ============================================================================
// State Handling & Winders
// ============================================================================

scm_obj_t get_current_winders() { return context::s_current_winders; }

void set_current_winders(scm_obj_t winders) {
  object_heap_t::current()->write_barrier(winders);
  context::s_current_winders = winders;
}

static void do_wind(scm_obj_t target_winders) {
  scm_obj_t current = context::s_current_winders;
  scm_obj_t target = target_winders;

  if (current == target) return;

  // Find common ancestor
  scm_obj_t c = current;
  int current_len = 0;
  while (is_cons(c)) {
    current_len++;
    c = cons_cdr(c);
  }

  scm_obj_t t = target;
  int target_len = 0;
  while (is_cons(t)) {
    target_len++;
    t = cons_cdr(t);
  }

  c = current;
  t = target;
  while (current_len > target_len) {
    c = cons_cdr(c);
    current_len--;
  }
  while (target_len > current_len) {
    t = cons_cdr(t);
    target_len--;
  }
  while (c != t) {
    c = cons_cdr(c);
    t = cons_cdr(t);
  }
  scm_obj_t common_ancestor = c;

  // Unwind current to common ancestor
  c = current;
  while (c != common_ancestor) {
    scm_obj_t winder = cons_car(c);
    scm_obj_t post = cons_cdr(winder);
    c_call_closure_thunk_0(post);
    c = cons_cdr(c);
    set_current_winders(c);
  }

  // Wind target from common ancestor
  auto wind_recursive = [&](auto self, scm_obj_t t) -> void {
    if (t == common_ancestor) return;
    self(self, cons_cdr(t));
    scm_obj_t winder = cons_car(t);
    scm_obj_t pre = cons_car(winder);
    c_call_closure_thunk_0(pre);
    set_current_winders(t);
  };
  wind_recursive(wind_recursive, target);
}

// ============================================================================
// Hardware Address Sanitizer & Utilities
// ============================================================================

// ============================================================================
// Continuations & call/cc
// ============================================================================

SUBR subr_invoke_escape_continuation(scm_obj_t self, int argc, scm_obj_t argv[]) {
  scm_closure_rec_t* clo = (scm_closure_rec_t*)to_address(self);
  scm_obj_t cont_obj = clo->env[0];

  scm_escape_rec_t* cont_rec = (scm_escape_rec_t*)to_address(cont_obj);
  if (argc > 1) {
    throw std::runtime_error("apply: continuation expects at most 1 argument");
  }
  if (cont_rec->invoked) {
    throw std::runtime_error("apply: one-shot continuation invoked out of scope or already invoked");
  }
  if (!cont_rec->uctx) {
    throw std::runtime_error("apply: invalid continuation");
  }

  scm_obj_t retval = (argc == 0) ? scm_undef : argv[0];
  object_heap_t::current()->write_barrier(retval);
  cont_rec->retval = retval;
  cont_rec->invoked = true;

  context::gc_protect(cont_obj);

  try {
    do_wind(cont_rec->winders);
  } catch (...) {
    context::gc_unprotect(cont_obj);
    throw;
  }

  context::s_continuation_captured_retval = retval;
  s_restored = true;

  context::gc_unprotect(cont_obj);

#if __has_feature(hwaddress_sanitizer) || defined(__SANITIZE_HWADDRESS__)
  void* sp;
  __asm__ volatile("mov %0, sp" : "=r"(sp));
  if ((uintptr_t)sp < cont_rec->sp) {
    __hwasan_tag_memory(sp, 0, cont_rec->sp - (uintptr_t)sp);
  }
#endif
  setcontext(cont_rec->uctx);
  return scm_undef;
}

__attribute__((no_sanitize("hwaddress"))) SUBR subr_call_ec(scm_obj_t self, scm_obj_t proc) {
  ucontext_t uctx;
  s_restored = false;
  if (getcontext(&uctx) == 0) {
    if (!s_restored) {
      void* sp;
      __asm__ volatile("mov %0, sp" : "=r"(sp));
      scm_obj_t cont_obj = make_escape(&uctx, (uintptr_t)sp, context::s_current_winders);
      scm_escape_rec_t* rec = (scm_escape_rec_t*)to_address(cont_obj);

      scm_obj_t env[1] = {cont_obj};
      scm_obj_t clo = make_closure((void*)subr_invoke_escape_continuation, 0, 1, 1, env, 1);
      scm_obj_t argv[1] = {make_cons(clo, scm_nil)};
      scm_obj_t ret = scm_undef;
      try {
        ret = c_apply_helper(proc, 1, argv);
      } catch (const nanos_exit_t& e) {
        throw;
      } catch (const std::exception& e) {
        std::cerr << "Exception while evaluating call/ec procedure: " << e.what() << std::endl;
        throw nanos_exit_t(1);
      } catch (...) {
        std::cerr << "Unknown exception while evaluating call/ec procedure" << std::endl;
        throw nanos_exit_t(1);
      }

      if (!rec->invoked) {
        // Procedure returned normally
        object_heap_t::current()->write_barrier(ret);
        rec->retval = ret;
        rec->invoked = true;
        return ret;
      } else {
        // Unreachable if invoked directly.
        fatal("%s:%u unreachable", __FILE__, __LINE__);
        return scm_undef;
      }
    } else {
      return context::s_continuation_captured_retval;
    }
  }
  fatal("getcontext failed");
}

// ============================================================================
// Dynamic Wind & Extensions
// ============================================================================

SUBR subr_dynamic_wind(scm_obj_t self, scm_obj_t pre, scm_obj_t value, scm_obj_t post) {
  c_call_closure_thunk_0(pre);
  scm_obj_t winder = make_cons(pre, post);
  scm_obj_t old_winders = get_current_winders();
  set_current_winders(make_cons(winder, old_winders));
  scm_obj_t res = scm_unspecified;
  try {
    res = c_call_closure_thunk_0(value);
  } catch (...) {
    set_current_winders(old_winders);
    c_call_closure_thunk_0(post);
    throw;
  }
  set_current_winders(old_winders);
  c_call_closure_thunk_0(post);
  return res;
}

SUBR subr_continuation_p(scm_obj_t self, scm_obj_t a1) {
  if (is_closure(a1)) {
    scm_closure_rec_t* clo = (scm_closure_rec_t*)to_address(a1);
    if (clo->code == (void*)subr_invoke_escape_continuation) {
      return scm_true;
    }
  }
  return scm_false;
}
