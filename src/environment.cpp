// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "environment.h"
#include "hash.h"
#include "object_heap.h"

scm_obj_t environment::s_standard_input_port;
scm_obj_t environment::s_standard_output_port;
scm_obj_t environment::s_standard_error_port;

scm_obj_t environment::s_interaction_environment;
scm_obj_t environment::s_system_environment;
scm_obj_t environment::s_current_environment;
std::unordered_set<scm_obj_t> environment::s_literals;

void environment::init() {
  if (object_heap_t::current() == nullptr) {
    fatal("%s:%u environment::init() called before object_heap_t::init()", __FILE__, __LINE__);
  }
  s_interaction_environment = make_environment(make_symbol("interaction-environment"));
  s_system_environment = make_environment(make_symbol("system-environment"));
  s_current_environment = s_system_environment;
}

void environment::destroy() {
  s_standard_input_port = scm_undef;
  s_standard_output_port = scm_undef;
  s_standard_error_port = scm_undef;
  s_interaction_environment = scm_undef;
  s_system_environment = scm_undef;
  s_current_environment = scm_undef;
}

void environment::environment_macro_set(scm_obj_t key, scm_obj_t value) {
  assert(is_symbol(key));
  scm_obj_t env = environment::s_current_environment;
  scm_environment_rec_t* env_rec = (scm_environment_rec_t*)to_address(env);
  hashtable_delete(env_rec->variables, key);
  hashtable_set(env_rec->macros, key, value);
}

scm_obj_t environment::environment_macro_ref(scm_obj_t key) {
  assert(is_symbol(key));
  scm_obj_t env = environment::s_current_environment;
  scm_environment_rec_t* env_rec = (scm_environment_rec_t*)to_address(env);
  scm_obj_t value = hashtable_ref(env_rec->macros, key, scm_undef);
  return value;
}

void environment::environment_variable_set(scm_obj_t key, scm_obj_t value) {
  assert(is_symbol(key));
  scm_obj_t env = environment::s_current_environment;
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

scm_obj_t environment::environment_variable_ref(scm_obj_t key) {
  assert(is_symbol(key));
  scm_obj_t env = environment::s_current_environment;
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

scm_obj_t environment::environment_variable_cell_ref(scm_obj_t key) {
  assert(is_symbol(key));
  scm_obj_t env = environment::s_current_environment;
  scm_environment_rec_t* env_rec = (scm_environment_rec_t*)to_address(env);
  scm_obj_t cell = hashtable_ref(env_rec->variables, key, scm_undef);
  if (cell == scm_undef) {
    scm_obj_t new_cell = make_cell(scm_undef);
    hashtable_set(env_rec->variables, key, new_cell);
    return new_cell;
  }
  return cell;
}

bool environment::environment_macro_contains(scm_obj_t key) {
  assert(is_symbol(key));
  return environment_macro_ref(key) != scm_undef;
}

bool environment::environment_variable_contains(scm_obj_t key) {
  assert(is_symbol(key));
  return environment_variable_ref(key) != scm_undef;
}

void environment::add_literal(scm_obj_t obj) {
  object_heap_t::current()->write_barrier(obj);
  s_literals.insert(obj);
}
