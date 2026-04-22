// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "context.h"
#include "nanos.h"
#include "subr.h"

#include <cerrno>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>

// ============================================================================
// Initialization & Registration
// ============================================================================

void nanos_t::init_subr() {
  auto reg = [](const char* name, void* func, int req, int opt) {
    context::environment_variable_set(make_symbol(name), make_closure(func, req, opt, 0, nullptr, 1));
  };

  init_subr_base();
  init_subr_arith();
  init_subr_cxr();
  init_subr_io();
  init_subr_hash();
  init_subr_env();
  init_subr_misc();
  init_subr_cffi();
  init_subr_uvector();
  init_subr_fiber();
}
