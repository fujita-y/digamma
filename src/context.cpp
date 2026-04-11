// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "context.h"
#include "hash.h"
#include "object_heap.h"
#include "port.h"

thread_local scm_obj_t context::s_current_input_port;
thread_local scm_obj_t context::s_current_output_port;
thread_local scm_obj_t context::s_current_error_port;
thread_local scm_obj_t context::s_current_environment;
thread_local scm_obj_t context::s_continuation_captured_retval = scm_undef;
thread_local scm_obj_t context::s_current_winders = scm_nil;

scm_obj_t context::s_standard_input_port;
scm_obj_t context::s_standard_output_port;
scm_obj_t context::s_standard_error_port;
scm_obj_t context::s_interaction_environment;
scm_obj_t context::s_system_environment;
std::unordered_set<scm_obj_t> context::s_literals;
std::mutex context::s_symbols_mutex;
std::unordered_map<std::string, scm_obj_t> context::s_symbols;
std::unordered_set<scm_obj_t> context::s_gc_protected;

void context::init() {
  if (object_heap_t::current() == nullptr) {
    fatal("%s:%u context::init() called before object_heap_t::init()", __FILE__, __LINE__);
  }
  s_standard_input_port = port_standard_input();
  s_standard_output_port = port_standard_output();
  s_standard_error_port = port_standard_error();
  s_current_input_port = s_standard_input_port;
  s_current_output_port = s_standard_output_port;
  s_current_error_port = s_standard_error_port;
  s_interaction_environment = make_environment(make_symbol("interaction-environment"));
  s_system_environment = make_environment(make_symbol("system-environment"));
  s_current_environment = s_system_environment;
}

void context::destroy() {
  port_finalize((scm_port_rec_t*)to_address(s_standard_input_port));
  port_finalize((scm_port_rec_t*)to_address(s_standard_output_port));
  port_finalize((scm_port_rec_t*)to_address(s_standard_error_port));
  s_symbols.clear();
  s_literals.clear();
  s_gc_protected.clear();
  s_interaction_environment = scm_undef;
  s_system_environment = scm_undef;
  s_current_environment = scm_undef;
  s_standard_input_port = scm_undef;
  s_standard_output_port = scm_undef;
  s_standard_error_port = scm_undef;
  s_current_input_port = scm_undef;
  s_current_output_port = scm_undef;
  s_current_error_port = scm_undef;
}

void context::environment_macro_set(scm_obj_t key, scm_obj_t value) {
  assert(is_symbol(key));
  scm_obj_t env = context::s_current_environment;
  scm_environment_rec_t* env_rec = (scm_environment_rec_t*)to_address(env);
  hashtable_delete(env_rec->variables, key);
  hashtable_set(env_rec->macros, key, value);
}

scm_obj_t context::environment_macro_ref(scm_obj_t key) {
  assert(is_symbol(key));
  scm_obj_t env = context::s_current_environment;
  scm_environment_rec_t* env_rec = (scm_environment_rec_t*)to_address(env);
  scm_obj_t value = hashtable_ref(env_rec->macros, key, scm_undef);
  return value;
}

void context::environment_variable_set(scm_obj_t key, scm_obj_t value) {
  assert(is_symbol(key));
  scm_obj_t env = context::s_current_environment;
  scm_environment_rec_t* env_rec = (scm_environment_rec_t*)to_address(env);
  hashtable_delete(env_rec->macros, key);
  scm_obj_t cell = hashtable_ref(env_rec->variables, key, scm_undef);
  if (cell == scm_undef) {
    scm_obj_t new_cell = make_cell(value);
    hashtable_set(env_rec->variables, key, new_cell);
    return;
  }
  scm_cell_rec_t* cell_rec = (scm_cell_rec_t*)to_address(cell);
  object_heap_t::current()->write_barrier(value);
  cell_rec->value = value;
}

scm_obj_t context::environment_variable_ref(scm_obj_t key) {
  assert(is_symbol(key));
  scm_obj_t env = context::s_current_environment;
  scm_environment_rec_t* env_rec = (scm_environment_rec_t*)to_address(env);
  scm_obj_t cell = hashtable_ref(env_rec->variables, key, scm_undef);
  if (cell == scm_undef) {
    scm_obj_t new_cell = make_cell(scm_undef);
    hashtable_set(env_rec->variables, key, new_cell);
    return scm_undef;
  }
  scm_cell_rec_t* cell_rec = (scm_cell_rec_t*)to_address(cell);
  return cell_rec->value;
}

scm_obj_t context::environment_variable_cell_ref(scm_obj_t key) {
  assert(is_symbol(key));
  scm_obj_t env = context::s_current_environment;
  scm_environment_rec_t* env_rec = (scm_environment_rec_t*)to_address(env);
  scm_obj_t cell = hashtable_ref(env_rec->variables, key, scm_undef);
  if (cell == scm_undef) {
    scm_obj_t new_cell = make_cell(scm_undef);
    hashtable_set(env_rec->variables, key, new_cell);
    return new_cell;
  }
  return cell;
}

bool context::environment_macro_contains(scm_obj_t key) {
  assert(is_symbol(key));
  return environment_macro_ref(key) != scm_undef;
}

bool context::environment_variable_contains(scm_obj_t key) {
  assert(is_symbol(key));
  return environment_variable_ref(key) != scm_undef;
}

void context::add_literal(scm_obj_t obj) {
  object_heap_t::current()->write_barrier(obj);
  s_literals.insert(obj);
}

void context::gc_protect(scm_obj_t obj) {
  if (is_cons(obj) || is_heap_object(obj)) {
    if (s_gc_protected.contains(obj)) {
      fatal("%s:%u gc_protect called on already protected object %s", __FILE__, __LINE__, to_string(obj).c_str());
    }
    object_heap_t::current()->write_barrier(obj);
    s_gc_protected.insert(obj);
  }
}

void context::gc_unprotect(scm_obj_t obj) {
  if (is_cons(obj) || is_heap_object(obj)) {
    if (!s_gc_protected.contains(obj)) {
      fatal("%s:%u gc_unprotect called on non-protected object %s", __FILE__, __LINE__, to_string(obj).c_str());
    }
    s_gc_protected.erase(obj);
  }
}

bool context::is_gc_protected(scm_obj_t obj) {
  if (is_cons(obj) || is_heap_object(obj)) {
    return s_gc_protected.contains(obj);
  }
  return true;
}
