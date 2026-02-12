
#include "core.h"
#include "arch_arm64.h"

// 11 registers total: x19-x28 (10) + SP (1)
void capture_arm64_core_state(uint64_t regs[11]) {
  __asm__ volatile(
      // Store General Purpose Pairs (x19-x28)
      "stp x19, x20, [%0, #0]\n"
      "stp x21, x22, [%0, #16]\n"
      "stp x23, x24, [%0, #32]\n"
      "stp x25, x26, [%0, #48]\n"
      "stp x27, x28, [%0, #64]\n"

      // Capture Stack Pointer (SP) via scratch register x9
      "mov x9, sp\n"
      "str x9, [%0, #80]\n"

      :
      : "r"(regs)
      : "x9", "memory");
}

void* capture_thread_stack_bottom() {
  pthread_attr_t attr;
  pthread_getattr_np(pthread_self(), &attr);
  void* stackaddr;
  size_t stacksize;
  pthread_attr_getstack(&attr, &stackaddr, &stacksize);
  pthread_attr_destroy(&attr);
  return (uint8_t*)stackaddr + stacksize;
}
