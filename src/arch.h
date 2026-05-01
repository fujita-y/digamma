// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

// Unified architecture header — include this instead of arch_arm64.h / arch_amd64.h.

#ifndef ARCH_H_INCLUDED
#define ARCH_H_INCLUDED

#if defined(__aarch64__)
  #include "arch_arm64.h"
  // Map the portable name to the arm64-specific implementation.
  inline void capture_arch_core_state(uint64_t regs[11]) { capture_arm64_core_state(regs); }
  static constexpr int ARCH_CORE_STATE_REGS = 11;
#elif defined(__x86_64__)
  #include "arch_amd64.h"
  inline void capture_arch_core_state(uint64_t regs[7]) { capture_amd64_core_state(regs); }
  static constexpr int ARCH_CORE_STATE_REGS = 7;
#else
  #error "Unsupported architecture: only aarch64 and x86_64 are supported."
#endif

#endif  // ARCH_H_INCLUDED
