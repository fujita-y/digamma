// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "nanos_subr.h"
#include <boost/context/continuation.hpp>
#include <boost/context/detail/fcontext.hpp>
#include <cstdio>
#include <sanitizer/hwasan_interface.h>
#include <ucontext.h>
#include "codegen_aux.h"
#include "object_heap.h"
#include "printer.h"

#define CAR(x) (((scm_cons_rec_t*)(x))->car)
#define CDR(x) (((scm_cons_rec_t*)(x))->cdr)

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

SUBR scm_obj_t subr_num_mul(scm_obj_t self, int argc, scm_obj_t argv[]) {
  intptr_t p = 1;
  for (int i = 0; i < argc; i++) {
    if (is_fixnum(argv[i])) {
      p *= fixnum(argv[i]);
    } else {
      throw std::runtime_error("*: arguments must be fixnums");
    }
  }
  return make_fixnum(p);
}

SUBR scm_obj_t subr_num_div(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc == 0) throw std::runtime_error("/: too few arguments");
  if (!is_fixnum(argv[0])) throw std::runtime_error("/: arguments must be fixnums");

  intptr_t result = fixnum(argv[0]);

  if (argc == 1) {
    return make_fixnum(1 / result);
  }

  for (int i = 1; i < argc; i++) {
    if (is_fixnum(argv[i])) {
      intptr_t d = fixnum(argv[i]);
      if (d == 0) throw std::runtime_error("/: division by zero");
      result /= d;
    } else {
      throw std::runtime_error("/: arguments must be fixnums");
    }
  }
  return make_fixnum(result);
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

static scm_obj_t append2(scm_obj_t lst1, scm_obj_t lst2) {
  if (lst1 == scm_nil) return lst2;
  scm_obj_t head = make_cons(CAR(lst1), scm_nil);
  scm_obj_t tail = head;
  lst1 = CDR(lst1);
  while (lst1 != scm_nil) {
    CDR(tail) = make_cons(CAR(lst1), scm_nil);
    tail = CDR(tail);
    lst1 = CDR(lst1);
  }
  CDR(tail) = lst2;
  return head;
}

static thread_local scm_obj_t s_captured_retval = scm_undef;
static thread_local bool s_restored = false;
static thread_local scm_continuation_rec_t* s_restored_rec = nullptr;

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

__attribute__((no_sanitize("hwaddress"))) SUBR scm_obj_t subr_call_cc(scm_obj_t self, scm_obj_t proc) {
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

      scm_obj_t cont_obj = make_continuation(&uctx, stack_size, stack_copy, shadow_copy, stack_bottom);
      scm_obj_t proc_argv[1] = {make_cons(cont_obj, scm_nil)};
      return c_apply_helper(proc, 1, proc_argv);
    } else {
#if __has_feature(hwaddress_sanitizer) || defined(__SANITIZE_HWADDRESS__)
      if (s_restored_rec) {
        uintptr_t stack_top = s_restored_rec->stack_bottom - s_restored_rec->stack_size;
        for (size_t i = 0; i < (s_restored_rec->stack_size + 15) / 16; i++) {
          __hwasan_tag_memory((void*)(stack_top + i * 16), s_restored_rec->shadow_copy[i], 16);
        }
        s_restored_rec = nullptr;
      }
#endif
      return s_captured_retval;
    }
  }
  fatal("getcontext failed");
}

__attribute__((aligned(16))) static uint8_t s_restore_stack[64 * 1024];

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

__attribute__((no_sanitize("hwaddress"))) extern "C" void restore_continuation(scm_continuation_rec_t* rec, scm_obj_t val) {
  s_captured_retval = val;
  s_restored = true;
  s_restored_rec = rec;
  uintptr_t tmp_sp = (uintptr_t)s_restore_stack + sizeof(s_restore_stack) - 64;
  __asm__ volatile(
      "mov sp, %0\n"
      "mov x0, %1\n"
      "bl restore_trampoline\n"
      :
      : "r"(tmp_sp), "r"(rec)
      : "x0", "memory");
}

SUBR scm_obj_t subr_append(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc == 0) return scm_nil;
  if (argc == 1) return argv[0];
  scm_obj_t obj = scm_undef;
  for (int i = argc - 1; i >= 0; i--) {
    if (obj == scm_undef)
      obj = argv[i];
    else
      obj = append2(argv[i], obj);
  }
  return obj;
}

SUBR scm_obj_t subr_cons(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) { return make_cons(a1, a2); }

SUBR scm_obj_t subr_not(scm_obj_t self, scm_obj_t a1) { return (a1 == scm_false) ? scm_true : scm_false; }

SUBR scm_obj_t subr_eq_p(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) { return (a1 == a2) ? scm_true : scm_false; }

SUBR scm_obj_t subr_pair_p(scm_obj_t self, scm_obj_t a1) { return is_cons(a1) ? scm_true : scm_false; }

SUBR scm_obj_t subr_null_p(scm_obj_t self, scm_obj_t a1) { return (a1 == scm_nil) ? scm_true : scm_false; }

SUBR scm_obj_t subr_cadr(scm_obj_t self, scm_obj_t a1) { return subr_car(self, subr_cdr(self, a1)); }

SUBR scm_obj_t subr_caddr(scm_obj_t self, scm_obj_t a1) { return subr_car(self, subr_cdr(self, subr_cdr(self, a1))); }

SUBR scm_obj_t subr_apply(scm_obj_t self, int argc, scm_obj_t argv[]) { return c_apply_helper(argv[0], argc - 1, &argv[1]); }

SUBR scm_obj_t subr_write(scm_obj_t self, scm_obj_t a1) {
  printer_t(std::cout).print(a1);
  return scm_undef;
}

SUBR scm_obj_t subr_newline(scm_obj_t self) {
  std::cout << std::endl;
  return scm_undef;
}

SUBR scm_obj_t subr_collect(scm_obj_t self) {
  object_heap_t::current()->collect();
  return scm_undef;
}

SUBR scm_obj_t subr_safepoint(scm_obj_t self) {
  object_heap_t::current()->safepoint();
  return scm_undef;
}

SUBR scm_obj_t subr_call_ec(scm_obj_t self, scm_obj_t proc) {
  scm_obj_t cont_obj = make_escape(boost::context::continuation{});
  scm_escape_rec_t* rec = (scm_escape_rec_t*)to_address(cont_obj);

  auto k = boost::context::callcc([proc, cont_obj, rec](boost::context::continuation&& sink) {
    // Sink is the main continuation. We save it inside the continuation object.
    delete rec->cont;  // Should be empty/default constructed initially
    rec->cont = new boost::context::continuation(std::move(sink));

    scm_obj_t argv[1] = {make_cons(cont_obj, scm_nil)};
    scm_obj_t ret = scm_undef;
    try {
      ret = c_apply_helper(proc, 1, argv);
    } catch (const boost::context::detail::forced_unwind& e) {
      throw;
    } catch (const std::exception& e) {
      fprintf(stderr, "Exception while evaluating call/ec procedure: %s\n", e.what());
      exit(EXIT_FAILURE);
    } catch (...) {
      fprintf(stderr, "Unknown exception while evaluating call/ec procedure\n");
      exit(EXIT_FAILURE);
    }

    if (!rec->invoked) {
      // Procedure returned normally
      rec->retval = ret;
      auto main_k = std::move(*(rec->cont));
      delete rec->cont;
      rec->cont = nullptr;
      return main_k.resume();
    } else {
      // Unreachable if invoked directly.
      fatal("%s:%u unreachable", __FILE__, __LINE__);
      return std::move(*(rec->cont));
    }
  });

  // We are back!
  // At this point, rec->retval holds the value (either returned normally or passed to the continuation).
  // The continuation is one-shot, so if it was used, it was destroyed.
  // Actually, wait! The lambda returns `boost::context::continuation`, and `callcc` returns that!
  // So `k` is the continuation that yielded to us. We don't really need it, or we can discard it.
  // Wait! boost::context::continuation variables must be destroyed ONLY if they are valid?
  // Yes, default destructor handles it.

  return rec->retval;
}
