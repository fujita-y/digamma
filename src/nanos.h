// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef NANOS_H_INCLUDED
#define NANOS_H_INCLUDED

#include "core.h"
#include <memory>
#include "nanos_jit.h"

#define SUBR extern "C" scm_obj_t

class nanos_t {
  std::unique_ptr<nanos_jit_t> m_jit;

  void init_subr();
  void init_codegen();

 public:
  void init();
  void destroy();
  void run();
};

#endif  // NANOS_H_INCLUDED
