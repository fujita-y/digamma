// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef ARCH_ARM64_H_INCLUDED
#define ARCH_ARM64_H_INCLUDED

#include "core.h"

// #define USE_TBI 0

#ifndef USE_TBI
  #if defined(IS_DEBUG_BUILD)
    #define USE_TBI 0
  #else
    #define USE_TBI 1
  #endif
#endif

uint64_t capture_thread_stack_bottom();
void capture_arm64_core_state(uint64_t regs[11]);  // x19-x28, sp

#if USE_TBI
inline uint64_t prune_memory_address(uint64_t addr) { return addr & ~((uint64_t)0xff << 56); }
#else
inline uint64_t prune_memory_address(uint64_t addr) { return addr; }
#endif

#endif
