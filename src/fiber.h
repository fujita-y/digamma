// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef FIBER_H_INCLUDED
#define FIBER_H_INCLUDED

#include "core.h"

void init_fiber_scheduler();

void fiber_set_focus_main(bool enable);
void fiber_scan_stacks();
int fiber_live_count();

#endif
