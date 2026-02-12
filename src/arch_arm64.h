// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef ARCH_ARM64_H_INCLUDED
#define ARCH_ARM64_H_INCLUDED

#include "core.h"

uint64_t capture_thread_stack_bottom();
void capture_arm64_core_state(uint64_t regs[11]);

#endif
