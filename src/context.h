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

class object_heap_t;
class codegen_t;
class nanos_t;

class context {
 public:
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
  static bool is_gc_protected(scm_obj_t obj);

  thread_local static scm_obj_t s_current_input_port;
  thread_local static scm_obj_t s_current_output_port;
  thread_local static scm_obj_t s_current_error_port;
  thread_local static scm_obj_t s_current_environment;
  thread_local static scm_obj_t s_continuation_captured_retval;
  thread_local static scm_obj_t s_current_winders;
  thread_local static object_heap_t* s_current_object_heap;
  thread_local static codegen_t* s_current_codegen;
  thread_local static nanos_t* s_current_nanos;

  thread_local static scm_obj_t s_standard_input_port;
  thread_local static scm_obj_t s_standard_output_port;
  thread_local static scm_obj_t s_standard_error_port;
  thread_local static scm_obj_t s_interaction_environment;
  thread_local static scm_obj_t s_system_environment;
  thread_local static std::unordered_set<scm_obj_t> s_literals;
  thread_local static std::unordered_set<scm_obj_t> s_gc_protected;
  thread_local static std::vector<scm_obj_t> s_trampolines;
};

class scoped_gc_protect {
 public:
  scoped_gc_protect(scm_obj_t obj) : m_obj(obj) {
    if (context::is_gc_protected(obj)) {
      m_protected = false;
    } else {
      context::gc_protect(obj);
      m_protected = true;
    }
  }
  ~scoped_gc_protect() {
    if (m_protected) context::gc_unprotect(m_obj);
    m_obj = (scm_obj_t) nullptr;
  }

 private:
  scoped_gc_protect(const scoped_gc_protect&) = delete;
  scoped_gc_protect& operator=(const scoped_gc_protect&) = delete;

  scm_obj_t m_obj;
  bool m_protected;
};

class scoped_gc_protect_vector {
 public:
  scoped_gc_protect_vector(const std::vector<scm_obj_t>& objs) : m_obj_vec(objs) {
    for (scm_obj_t obj : m_obj_vec) {
      if (context::is_gc_protected(obj)) {
        m_protected_vec.push_back(false);
      } else {
        context::gc_protect(obj);
        m_protected_vec.push_back(true);
      }
    }
  }
  ~scoped_gc_protect_vector() {
    for (size_t i = 0; i < m_obj_vec.size(); ++i) {
      if (m_protected_vec[i]) {
        context::gc_unprotect(m_obj_vec[i]);
      }
    }
  }

 private:
  scoped_gc_protect_vector(const scoped_gc_protect_vector&) = delete;
  scoped_gc_protect_vector& operator=(const scoped_gc_protect_vector&) = delete;

  std::vector<scm_obj_t> m_obj_vec;
  std::vector<bool> m_protected_vec;
};

#endif
