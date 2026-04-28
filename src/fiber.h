// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef FIBER_H_INCLUDED
#define FIBER_H_INCLUDED

#include "core.h"
#include "object.h"
#include "context.h"

void init_fiber_scheduler();

void fiber_set_focus_main(bool enable);
void fiber_scan_stacks();
int fiber_live_count();

class fiber_unwind_guard {
 public:
  explicit fiber_unwind_guard(scm_obj_t future) : m_future(future) { assert(is_future(future)); }
  ~fiber_unwind_guard() {
    context::gc_unprotect(m_future);
    context::s_live_fiber_count--;
  }

 private:
  fiber_unwind_guard(const fiber_unwind_guard&) = delete;
  fiber_unwind_guard& operator=(const fiber_unwind_guard&) = delete;
  scm_obj_t m_future;
};

#endif
