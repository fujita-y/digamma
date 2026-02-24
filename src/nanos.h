// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef NANOS_H_INCLUDED
#define NANOS_H_INCLUDED

#include "core.h"
#include <llvm-19/llvm/ExecutionEngine/Orc/LLJIT.h>
#include <memory>

class nanos_t {
  std::unique_ptr<llvm::orc::LLJIT> m_jit;

  void init_subr();
  void init_codegen();

 public:
  void init();
  void destroy();
  void run();
};

#endif  // NANOS_H_INCLUDED
