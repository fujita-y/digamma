// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "context.h"
#include "hash.h"
#include "list.h"
#include "nanos.h"
#include "object_heap.h"
#include "subr.h"

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <stdexcept>
#include <string>

// ============================================================================
// Environment Access
// ============================================================================

// make-environment
SUBR subr_make_environment(scm_obj_t self, scm_obj_t a1) {
  if (!is_symbol(a1)) throw std::runtime_error("make-environment: argument must be a symbol");
  return make_environment(a1);
}

// copy-environment-variables!
SUBR subr_copy_environment_variables(scm_obj_t self, scm_obj_t a1, scm_obj_t a2, scm_obj_t a3) {
  if (!is_environment(a1)) throw std::runtime_error("copy-environment-variables!: first argument must be an environment");
  if (!is_environment(a2)) throw std::runtime_error("copy-environment-variables!: second argument must be an environment");
  scm_environment_rec_t* src = (scm_environment_rec_t*)to_address(a1);
  scm_environment_rec_t* dst = (scm_environment_rec_t*)to_address(a2);
  scm_obj_t cur = a3;
  while (is_cons(cur)) {
    scm_obj_t key = cons_car(cur);
    if (!is_symbol(key)) throw std::runtime_error("copy-environment-variables!: list elements must be symbols");
    if (!is_symbol_interned(key)) {
      std::string msg = "copy-environment-variables!: symbol not interned: " + std::string((char*)symbol_name(key));
      throw std::runtime_error(msg);
    }
    scm_obj_t cell = hashtable_ref(src->variables, key, scm_undef);
    if (cell != scm_undef) {
      assert(is_cell(cell));
      scm_obj_t val = cell_value(cell);
      if (is_closure(val)) {
        hashtable_set(dst->variables, key, make_cell(val));
      } else {
        throw std::runtime_error("copy-environment-variables!: variable contains other than closure: " + std::string((char*)symbol_name(key)));
      }
    } else {
      throw std::runtime_error("copy-environment-variables!: symbol not found in source environment: " + std::string((char*)symbol_name(key)));
    }
    cur = cons_cdr(cur);
  }
  if (cur != scm_nil) throw std::runtime_error("copy-environment-variables!: third argument must be a proper list");
  return scm_unspecified;
}

// copy-environment-macros!
SUBR subr_copy_environment_macros(scm_obj_t self, scm_obj_t a1, scm_obj_t a2, scm_obj_t a3) {
  if (!is_environment(a1)) throw std::runtime_error("copy-environment-macros!: first argument must be an environment");
  if (!is_environment(a2)) throw std::runtime_error("copy-environment-macros!: second argument must be an environment");
  scm_environment_rec_t* src = (scm_environment_rec_t*)to_address(a1);
  scm_environment_rec_t* dst = (scm_environment_rec_t*)to_address(a2);
  scm_obj_t cur = a3;
  while (is_cons(cur)) {
    scm_obj_t key = cons_car(cur);
    if (!is_symbol(key)) throw std::runtime_error("copy-environment-macros!: list elements must be symbols");
    if (!is_symbol_interned(key)) {
      std::string msg = "copy-environment-macros!: symbol not interned: " + std::string((char*)symbol_name(key));
      throw std::runtime_error(msg);
    }
    scm_obj_t val = hashtable_ref(src->macros, key, scm_undef);
    if (val != scm_undef) {
      hashtable_set(dst->macros, key, val);
    } else {
      std::string msg = "copy-environment-macros!: symbol not found in source environment: " + std::string((char*)symbol_name(key));
      throw std::runtime_error(msg);
    }
    cur = cons_cdr(cur);
  }
  if (cur != scm_nil) throw std::runtime_error("copy-environment-macros!: third argument must be a proper list");
  return scm_unspecified;
}

// environment-macros
SUBR subr_environment_macros(scm_obj_t self, scm_obj_t a1) {
  if (!is_environment(a1)) throw std::runtime_error("environment-macros: argument must be an environment");
  scm_environment_rec_t* env = (scm_environment_rec_t*)to_address(a1);
  return env->macros;
}

// environment-variables
SUBR subr_environment_variables(scm_obj_t self, scm_obj_t a1) {
  if (!is_environment(a1)) throw std::runtime_error("environment-variables: argument must be an environment");
  scm_environment_rec_t* env = (scm_environment_rec_t*)to_address(a1);
  return env->variables;
}

// current-environment
SUBR subr_current_environment(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc == 0) {
    return context::s_current_environment;
  } else if (argc == 1) {
    if (!is_environment(argv[0])) throw std::runtime_error("current-environment: argument must be an environment");
    object_heap_t::current()->write_barrier(argv[0]);
    context::s_current_environment = argv[0];
    return scm_unspecified;
  } else {
    throw std::runtime_error("current-environment: wrong number of arguments");
  }
}

// environment-macro-set!  - digamma core
// (environment-macro-set! name transformer)
SUBR subr_environment_macro_set(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_symbol(a1)) throw std::runtime_error("environment-macro-set!: first argument must be a symbol");
  context::environment_macro_set(a1, a2);
  return scm_unspecified;
}

// environment-macro-ref  - digamma core
// (environment-macro-ref name) => transformer or scm_undef
SUBR subr_environment_macro_ref(scm_obj_t self, scm_obj_t a1) {
  if (!is_symbol(a1)) throw std::runtime_error("environment-macro-ref: argument must be a symbol");
  return context::environment_macro_ref(a1);
}

// environment-variable-set!  - digamma core
// (environment-variable-set! name value)
SUBR subr_environment_variable_set(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_symbol(a1)) throw std::runtime_error("environment-variable-set!: first argument must be a symbol");
  context::environment_variable_set(a1, a2);
  return scm_unspecified;
}

// environment-variable-ref  - digamma core
// (environment-variable-ref name) => value or scm_undef
SUBR subr_environment_variable_ref(scm_obj_t self, scm_obj_t a1) {
  if (!is_symbol(a1)) throw std::runtime_error("environment-variable-ref: argument must be a symbol");
  return context::environment_variable_ref(a1);
}

// environment-macro-contains?  - digamma core
// (environment-macro-contains? name) => #t or #f
SUBR subr_environment_macro_contains(scm_obj_t self, scm_obj_t a1) {
  if (!is_symbol(a1)) throw std::runtime_error("environment-macro-contains?: argument must be a symbol");
  return context::environment_macro_contains(a1) ? scm_true : scm_false;
}

// environment-variable-contains?  - digamma core
// (environment-variable-contains? name) => #t or #f
SUBR subr_environment_variable_contains(scm_obj_t self, scm_obj_t a1) {
  if (!is_symbol(a1)) throw std::runtime_error("environment-variable-contains?: argument must be a symbol");
  return context::environment_variable_contains(a1) ? scm_true : scm_false;
}

// interaction-environment - R6RS 11.16
SUBR subr_interaction_environment(scm_obj_t self) { return context::s_interaction_environment; }

// system-environment
SUBR subr_system_environment(scm_obj_t self) { return context::s_system_environment; }

// undefine  - nanos specific
// (undefine name) - removes a macro or variable binding for 'name' from the
// current environment only.
// Useful to undo define-syntax or define without leaving stale bindings.
SUBR subr_undefine(scm_obj_t self, scm_obj_t a1) {
  if (!is_symbol(a1)) throw std::runtime_error("undefine: argument must be a symbol");
  scm_obj_t env = context::s_current_environment;
  if (!is_environment(env)) return scm_unspecified;
  scm_environment_rec_t* env_rec = (scm_environment_rec_t*)to_address(env);
  hashtable_delete(env_rec->macros, a1);
  hashtable_delete(env_rec->variables, a1);
  return scm_unspecified;
}

// lookup-process-environment
SUBR subr_lookup_process_environment(scm_obj_t self, scm_obj_t a1) {
  if (!is_string(a1)) throw std::runtime_error("lookup-process-environment: argument must be a string");
  const char* val = std::getenv((const char*)string_name(a1));
  if (val) return make_string(val);
  return scm_false;
}

void init_subr_env() {
  auto reg = [](const char* name, void* func, int req, int opt) {
    context::environment_variable_set(make_symbol(name), make_closure(func, req, opt, 0, nullptr, 1));
  };

  reg("make-environment", (void*)subr_make_environment, 1, 0);
  reg("copy-environment-variables!", (void*)subr_copy_environment_variables, 3, 0);
  reg("copy-environment-macros!", (void*)subr_copy_environment_macros, 3, 0);
  reg("environment-macros", (void*)subr_environment_macros, 1, 0);
  reg("environment-variables", (void*)subr_environment_variables, 1, 0);
  reg("current-environment", (void*)subr_current_environment, 0, 1);
  reg("environment-macro-set!", (void*)subr_environment_macro_set, 2, 0);
  reg("environment-macro-ref", (void*)subr_environment_macro_ref, 1, 0);
  reg("environment-macro-contains?", (void*)subr_environment_macro_contains, 1, 0);
  reg("environment-variable-set!", (void*)subr_environment_variable_set, 2, 0);
  reg("environment-variable-ref", (void*)subr_environment_variable_ref, 1, 0);
  reg("environment-variable-contains?", (void*)subr_environment_variable_contains, 1, 0);
  reg("interaction-environment", (void*)subr_interaction_environment, 0, 0);
  reg("system-environment", (void*)subr_system_environment, 0, 0);
  reg("lookup-process-environment", (void*)subr_lookup_process_environment, 1, 0);
  reg("undefine", (void*)subr_undefine, 1, 0);
}
