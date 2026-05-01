// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "arch_amd64.h"

// 7 registers total: rbx, rbp, r12, r13, r14, r15 (6 callee-saved) + rsp (1)
void capture_amd64_core_state(uint64_t regs[7]) {
  __asm__ volatile(
      "movq %%rbx, 0(%0)\n"
      "movq %%rbp, 8(%0)\n"
      "movq %%r12, 16(%0)\n"
      "movq %%r13, 24(%0)\n"
      "movq %%r14, 32(%0)\n"
      "movq %%r15, 40(%0)\n"
      "movq %%rsp, 48(%0)\n"
      :
      : "r"(regs)
      : "memory");
}

uint64_t capture_thread_stack_bottom() {
  static thread_local uint64_t cached = 0;
  if (cached) [[likely]] {
    return cached;
  }
  pthread_attr_t attr;
  pthread_getattr_np(pthread_self(), &attr);
  void* stackaddr;
  size_t stacksize;
  pthread_attr_getstack(&attr, &stackaddr, &stacksize);
  pthread_attr_destroy(&attr);
  cached = (uint64_t)stackaddr + stacksize;
  return cached;
}
