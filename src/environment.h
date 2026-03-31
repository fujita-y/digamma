// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"

class environment {
 public:
  static scm_obj_t s_standard_input_port;
  static scm_obj_t s_standard_output_port;
  static scm_obj_t s_standard_error_port;

  static void init();
  static void destroy();
};