// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "environment.h"

scm_obj_t environment::s_standard_input_port;
scm_obj_t environment::s_standard_output_port;
scm_obj_t environment::s_standard_error_port;

void environment::init() {}

void environment::destroy() {
  s_standard_input_port = scm_undef;
  s_standard_output_port = scm_undef;
  s_standard_error_port = scm_undef;
}
