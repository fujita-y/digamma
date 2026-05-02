// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef CONTEXT_H_INCLUDED
#define CONTEXT_H_INCLUDED

#include "core.h"
#include "object.h"

#include <boost/context/stack_context.hpp>
#include <boost/fiber/fixedsize_stack.hpp>
#ifndef NDEBUG
  #include <boost/fiber/protected_fixedsize_stack.hpp>
#endif
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

class object_heap_t;
class codegen_t;
class nanos_t;
class asio_context;

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

  thread_local static scm_obj_t s_current_input_port;
  thread_local static scm_obj_t s_current_output_port;
  thread_local static scm_obj_t s_current_error_port;
  thread_local static scm_obj_t s_current_environment;

  thread_local static object_heap_t* s_current_object_heap;
  thread_local static codegen_t* s_current_codegen;
  thread_local static nanos_t* s_current_nanos;

  thread_local static scm_obj_t s_standard_input_port;
  thread_local static scm_obj_t s_standard_output_port;
  thread_local static scm_obj_t s_standard_error_port;
  thread_local static scm_obj_t s_interaction_environment;
  thread_local static scm_obj_t s_system_environment;
  thread_local static std::unordered_set<scm_obj_t> s_literals;
  thread_local static std::unordered_multiset<scm_obj_t> s_gc_protected;
  thread_local static std::vector<scm_obj_t> s_trampolines;
  thread_local static int s_trampoline_uid;
  thread_local static int s_cffi_uid;
  thread_local static std::unordered_map<std::string, void*> s_callout_cache;

  struct fiber_stack_info {
    void* sp;
    void* usable_start;
    size_t usable_size;
  };

  class fiber_stack_allocator {
   public:
    boost::context::stack_context allocate();
    void deallocate(boost::context::stack_context sctx);

   private:
#ifdef NDEBUG
    boost::fibers::fixedsize_stack m_alloc;
#else
    boost::fibers::protected_fixedsize_stack m_alloc;
#endif
  };

  thread_local static int s_live_fiber_count;
  thread_local static std::vector<fiber_stack_info> s_fiber_stacks;
  thread_local static fiber_stack_allocator s_fiber_stack_allocator;
  thread_local static asio_context* s_asio_context;
};

class scoped_gc_protect {
 public:
  scoped_gc_protect(scm_obj_t obj) : m_obj(obj) { context::gc_protect(m_obj); }
  ~scoped_gc_protect() {
    context::gc_unprotect(m_obj);
    m_obj = (scm_obj_t) nullptr;
  }

 private:
  scoped_gc_protect(const scoped_gc_protect&) = delete;
  scoped_gc_protect& operator=(const scoped_gc_protect&) = delete;
  scm_obj_t m_obj;
};

class scoped_gc_protect_vector {
 public:
  scoped_gc_protect_vector(const std::vector<scm_obj_t>& objs) : m_objs(objs) {
    for (scm_obj_t obj : m_objs) {
      context::gc_protect(obj);
    }
  }
  ~scoped_gc_protect_vector() {
    for (size_t i = 0; i < m_objs.size(); ++i) {
      context::gc_unprotect(m_objs[i]);
    }
  }

 private:
  scoped_gc_protect_vector(const scoped_gc_protect_vector&) = delete;
  scoped_gc_protect_vector& operator=(const scoped_gc_protect_vector&) = delete;
  std::vector<scm_obj_t> m_objs;
};

#endif
