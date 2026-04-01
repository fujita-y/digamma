// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include <mutex>
#include <string>
#include <unordered_map>
#include <unordered_set>

class environment {
 public:
  static scm_obj_t s_standard_input_port;
  static scm_obj_t s_standard_output_port;
  static scm_obj_t s_standard_error_port;

  static scm_obj_t s_interaction_environment;
  static scm_obj_t s_system_environment;
  static scm_obj_t s_current_environment;

  thread_local static scm_obj_t s_continuation_captured_retval;
  thread_local static scm_obj_t s_current_winders;

  static std::unordered_set<scm_obj_t> s_literals;

  static std::mutex s_symbols_mutex;
  static std::unordered_map<std::string, scm_obj_t> s_symbols;

  static scm_obj_t environment_macro_ref(scm_obj_t key);
  static scm_obj_t environment_variable_ref(scm_obj_t key);
  static scm_obj_t environment_variable_cell_ref(scm_obj_t key);
  static void environment_macro_set(scm_obj_t key, scm_obj_t value);
  static void environment_variable_set(scm_obj_t key, scm_obj_t value);
  static bool environment_macro_contains(scm_obj_t key);
  static bool environment_variable_contains(scm_obj_t key);

  static void init();
  static void destroy();
  static void add_literal(scm_obj_t obj);
};