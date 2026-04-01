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
#include "object_heap.h"

thread_local static bool s_restored = false;
thread_local static scm_continuation_rec_t* s_restored_rec = nullptr;
__attribute__((aligned(16))) thread_local static uint8_t s_restore_stack[8192];

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
    c = CDR(c);
  }

  scm_obj_t t = target;
  int target_len = 0;
  while (is_cons(t)) {
    target_len++;
    t = CDR(t);
  }

  c = current;
  t = target;
  while (current_len > target_len) {
    c = CDR(c);
    current_len--;
  }
  while (target_len > current_len) {
    t = CDR(t);
    target_len--;
  }
  while (c != t) {
    c = CDR(c);
    t = CDR(t);
  }
  scm_obj_t common_ancestor = c;

  // Unwind current to common ancestor
  c = current;
  while (c != common_ancestor) {
    scm_obj_t winder = CAR(c);
    scm_obj_t post = CDR(winder);
    c_call_closure_thunk_0(post);
    c = CDR(c);
    set_current_winders(c);
  }

  // Wind target from common ancestor
  auto wind_recursive = [&](auto self, scm_obj_t t) -> void {
    if (t == common_ancestor) return;
    self(self, CDR(t));
    scm_obj_t winder = CAR(t);
    scm_obj_t pre = CAR(winder);
    c_call_closure_thunk_0(pre);
    set_current_winders(t);
  };
  wind_recursive(wind_recursive, target);
}

// ============================================================================
// Hardware Address Sanitizer & Utilities
// ============================================================================

__attribute__((no_sanitize("hwaddress"))) static void copy_memory_hwasan_safe(void* dst, const void* src, size_t size) {
  volatile uint64_t* d64 = (volatile uint64_t*)dst;
  const volatile uint64_t* s64 = (const volatile uint64_t*)src;
  size_t n64 = size / 8;
  for (size_t i = 0; i < n64; i++) d64[i] = s64[i];
  size_t rem = size % 8;
  if (rem) {
    volatile uint8_t* d8 = (volatile uint8_t*)dst + n64 * 8;
    const volatile uint8_t* s8 = (const volatile uint8_t*)src + n64 * 8;
    for (size_t i = 0; i < rem; i++) d8[i] = s8[i];
  }
}

static uint8_t get_mem_tag(void* p) {
#if __has_feature(hwaddress_sanitizer) || defined(__SANITIZE_HWADDRESS__)
  p = (void*)((uintptr_t)p & ~0xF);
  // Pointer tag 0 is a wildcard in HWASAn and will always match in __hwasan_test_shadow.
  // We must skip t=0 and check 1 to 255. If none match, the actual tag must be 00.
  for (int t = 1; t < 256; t++) {
    void* pt = __hwasan_tag_pointer(p, t);
    if (__hwasan_test_shadow(pt, 1) == -1) {
      return (uint8_t)t;
    }
  }
#endif
  return 0;
}

// ============================================================================
// Continuations & call/cc
// ============================================================================

__attribute__((used)) __attribute__((no_sanitize("hwaddress"))) extern "C" void restore_trampoline(scm_continuation_rec_t* rec) {
  uintptr_t stack_top = rec->stack_bottom - rec->stack_size;
  uint8_t* dst = (uint8_t*)prune_memory_address(stack_top);
  uint8_t* src = (uint8_t*)prune_memory_address((uintptr_t)rec->stack_copy);
#if __has_feature(hwaddress_sanitizer) || defined(__SANITIZE_HWADDRESS__)
  __hwasan_tag_memory(dst, 0, rec->stack_size);
#endif
  copy_memory_hwasan_safe(dst, src, rec->stack_size);
#if __has_feature(hwaddress_sanitizer) || defined(__SANITIZE_HWADDRESS__)
  __hwasan_handle_vfork((void*)stack_top);
  for (size_t i = 0; i < (rec->stack_size + 15) / 16; i++) {
    __hwasan_tag_memory((void*)(stack_top + i * 16), rec->shadow_copy[i], 16);
  }
#endif
  setcontext(rec->uctx);
}

__attribute__((no_sanitize("hwaddress"))) void restore_continuation(scm_continuation_rec_t* rec, scm_obj_t val) {
  do_wind(rec->winders);
  object_heap_t::current()->write_barrier(val);
  context::s_continuation_captured_retval = val;
  s_restored = true;
  s_restored_rec = rec;
  // Acquire collector lock before switching to the temp stack. Once we switch SP
  // to s_restore_stack the mutator cannot reach a safepoint, so we must prevent
  // the GC from entering a stop-the-world phase during this window. The lock is
  // implicitly released when setcontext() jumps back to the subr_call_cc capture
  // site — see restore_trampoline_unlock which runs after the context switch.
  object_heap_t::current()->collector_lock().lock();
  uintptr_t tmp_sp = (uintptr_t)s_restore_stack + sizeof(s_restore_stack) - 64;
  __asm__ volatile(
      "mov sp, %0\n"
      "mov x0, %1\n"
      "bl restore_trampoline\n"
      :
      : "r"(tmp_sp), "r"(rec)
      : "x0", "memory");
}

SUBR subr_invoke_continuation(scm_obj_t self, int argc, scm_obj_t argv[]) {
  scm_closure_rec_t* clo = (scm_closure_rec_t*)to_address(self);
  scm_obj_t cont_obj = clo->env[0];
  scoped_gc_protect gc_protect(cont_obj);
  scm_continuation_rec_t* cont_rec = (scm_continuation_rec_t*)to_address(cont_obj);
  if (argc > 1) throw std::runtime_error("apply: continuation expects at most 1 argument");
  scm_obj_t val = (argc == 0) ? scm_undef : argv[0];
  restore_continuation(cont_rec, val);
  return scm_undef;
}

SUBR subr_invoke_escape_continuation(scm_obj_t self, int argc, scm_obj_t argv[]) {
  scm_closure_rec_t* clo = (scm_closure_rec_t*)to_address(self);
  scm_obj_t cont_obj = clo->env[0];
  scoped_gc_protect gc_protect(cont_obj);
  scm_escape_rec_t* cont_rec = (scm_escape_rec_t*)to_address(cont_obj);
  if (argc > 1) throw std::runtime_error("apply: continuation expects at most 1 argument");
  if (cont_rec->invoked) throw std::runtime_error("apply: one-shot continuation already invoked");
  if (!cont_rec->uctx) throw std::runtime_error("apply: invalid continuation");

  scm_obj_t retval = (argc == 0) ? scm_undef : argv[0];
  object_heap_t::current()->write_barrier(retval);
  cont_rec->retval = retval;
  cont_rec->invoked = true;

  do_wind(cont_rec->winders);

  context::s_continuation_captured_retval = retval;
  s_restored = true;
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

__attribute__((no_sanitize("hwaddress"))) SUBR subr_call_cc(scm_obj_t self, scm_obj_t proc) {
  uint64_t stack_bottom = capture_thread_stack_bottom();
  ucontext_t uctx;
  s_restored = false;
  if (getcontext(&uctx) == 0) {
    if (!s_restored) {
      void* sp;
      __asm__ volatile("mov %0, sp" : "=r"(sp));
      size_t stack_size = stack_bottom - (uintptr_t)sp;
      uint8_t* stack_copy = (uint8_t*)malloc(stack_size);

      uint8_t* d_ptr = (uint8_t*)prune_memory_address((uintptr_t)stack_copy);
      uint8_t* s_ptr = (uint8_t*)prune_memory_address((uintptr_t)sp);
      copy_memory_hwasan_safe(d_ptr, s_ptr, stack_size);

#if __has_feature(hwaddress_sanitizer) || defined(__SANITIZE_HWADDRESS__)
      size_t shadow_size = (stack_size + 15) / 16;
      uint8_t* shadow_copy = (uint8_t*)malloc(shadow_size);
      for (size_t i = 0; i < shadow_size; i++) {
        shadow_copy[i] = get_mem_tag((void*)((uintptr_t)sp + i * 16));
      }
#else
      uint8_t* shadow_copy = nullptr;
#endif

      scm_obj_t cont_obj = make_continuation(&uctx, stack_size, stack_copy, shadow_copy, stack_bottom, context::s_current_winders);
      scm_obj_t clo = make_closure((void*)subr_invoke_continuation, 0, 1, 1, &cont_obj, 1);
      scm_obj_t proc_argv[1] = {make_cons(clo, scm_nil)};
      return c_apply_helper(proc, 1, proc_argv);
    } else {
      // Unlock the collector lock acquired in restore_continuation before the
      // stack switch. We are now back on the real stack and can reach safepoints.
      object_heap_t::current()->collector_lock().unlock();
#if __has_feature(hwaddress_sanitizer) || defined(__SANITIZE_HWADDRESS__)
      if (s_restored_rec) {
        uintptr_t stack_top = s_restored_rec->stack_bottom - s_restored_rec->stack_size;
        for (size_t i = 0; i < (s_restored_rec->stack_size + 15) / 16; i++) {
          __hwasan_tag_memory((void*)(stack_top + i * 16), s_restored_rec->shadow_copy[i], 16);
        }
        s_restored_rec = nullptr;
      }
#endif
      return context::s_continuation_captured_retval;
    }
  }
  fatal("getcontext failed");
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
      } catch (const std::exception& e) {
        fprintf(stderr, "Exception while evaluating call/ec procedure: %s\n", e.what());
        exit(EXIT_FAILURE);
      } catch (...) {
        fprintf(stderr, "Unknown exception while evaluating call/ec procedure\n");
        exit(EXIT_FAILURE);
      }

      if (!rec->invoked) {
        // Procedure returned normally
        object_heap_t::current()->write_barrier(ret);
        rec->retval = ret;
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
  scm_obj_t res = c_call_closure_thunk_0(value);
  set_current_winders(old_winders);
  c_call_closure_thunk_0(post);
  return res;
}

SUBR subr_continuation_p(scm_obj_t self, scm_obj_t a1) {
  if (is_closure(a1)) {
    scm_closure_rec_t* clo = (scm_closure_rec_t*)to_address(a1);
    if (clo->code == (void*)subr_invoke_continuation || clo->code == (void*)subr_invoke_escape_continuation) {
      return scm_true;
    }
  }
  return scm_false;
}
