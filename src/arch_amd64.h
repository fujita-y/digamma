// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef ARCH_AMD64_H_INCLUDED
#define ARCH_AMD64_H_INCLUDED

#include "core.h"

// x86_64 has no Top Byte Ignore (TBI) / Memory Tagging Extension (MTE)
#define USE_TBI 0

uint64_t capture_thread_stack_bottom();

// 7 registers total: rbx, rbp, r12-r15 (6) + rsp (1)
void capture_amd64_core_state(uint64_t regs[7]);

// On amd64 there are no tag bits in pointers, so pruning is a no-op.
inline uint64_t prune_memory_address(uint64_t addr) { return addr; }

#endif
