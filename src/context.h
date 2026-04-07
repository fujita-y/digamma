// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef CONTEXT_H_INCLUDED
#define CONTEXT_H_INCLUDED

#include "core.h"
#include "object.h"
#include <mutex>
#include <string>
#include <unordered_map>
#include <unordered_set>

class context {
 public:
  thread_local static scm_obj_t s_current_input_port;
  thread_local static scm_obj_t s_current_output_port;
  thread_local static scm_obj_t s_current_error_port;
  thread_local static scm_obj_t s_current_environment;
  thread_local static scm_obj_t s_continuation_captured_retval;
  thread_local static scm_obj_t s_current_winders;

  static scm_obj_t s_standard_input_port;
  static scm_obj_t s_standard_output_port;
  static scm_obj_t s_standard_error_port;
  static scm_obj_t s_interaction_environment;
  static scm_obj_t s_system_environment;
  static std::unordered_set<scm_obj_t> s_literals;
  static std::mutex s_symbols_mutex;
  static std::unordered_map<std::string, scm_obj_t> s_symbols;
  static std::unordered_set<scm_obj_t> s_gc_protected;

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
  static void gc_protect(scm_obj_t obj);
  static void gc_unprotect(scm_obj_t obj);
};

class scoped_gc_protect {
  scoped_gc_protect(const scoped_gc_protect&) = delete;
  scoped_gc_protect& operator=(const scoped_gc_protect&) = delete;
  scm_obj_t m_obj;

 public:
  scoped_gc_protect(scm_obj_t obj) : m_obj(obj) { context::gc_protect(obj); }
  ~scoped_gc_protect() {
    context::gc_unprotect(m_obj);
    m_obj = (scm_obj_t) nullptr;
  }
};

class scoped_gc_protect_vector {
  scoped_gc_protect_vector(const scoped_gc_protect_vector&) = delete;
  scoped_gc_protect_vector& operator=(const scoped_gc_protect_vector&) = delete;
  std::vector<scm_obj_t> m_objs;

 public:
  scoped_gc_protect_vector(const std::vector<scm_obj_t>& objs) : m_objs(objs) {
    for (scm_obj_t obj : m_objs) context::gc_protect(obj);
  }
  ~scoped_gc_protect_vector() {
    for (scm_obj_t obj : m_objs) context::gc_unprotect(obj);
  }
};

#endif
