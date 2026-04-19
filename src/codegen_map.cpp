// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "codegen.h"
#include "context.h"
#include "object_heap.h"
#include "subr.h"

void codegen_t::init_opcode_map() {
  opcode_map[make_symbol("const")] = Opcode::CONST;
  opcode_map[make_symbol("unspecified")] = Opcode::UNSPECIFIED;
  opcode_map[make_symbol("mov")] = Opcode::MOV;
  opcode_map[make_symbol("if")] = Opcode::IF;
  opcode_map[make_symbol("jump")] = Opcode::JUMP;
  opcode_map[make_symbol("label")] = Opcode::LABEL;
  opcode_map[make_symbol("ret")] = Opcode::RET;
  opcode_map[make_symbol("make-closure")] = Opcode::MAKE_CLOSURE;
  opcode_map[make_symbol("global-set!")] = Opcode::GLOBAL_SET;
  opcode_map[make_symbol("global-ref")] = Opcode::GLOBAL_REF;
  opcode_map[make_symbol("call")] = Opcode::CALL;
  opcode_map[make_symbol("tail-call")] = Opcode::TAIL_CALL;
  opcode_map[make_symbol("closure-ref")] = Opcode::CLOSURE_REF;
  opcode_map[make_symbol("closure-set!")] = Opcode::CLOSURE_SET;
  opcode_map[make_symbol("closure-cell-set!")] = Opcode::CLOSURE_CELL_SET;
  opcode_map[make_symbol("closure-self")] = Opcode::CLOSURE_SELF;
  opcode_map[make_symbol("closure-cell-ref")] = Opcode::CLOSURE_CELL_REF;
  opcode_map[make_symbol("reg-cell-ref")] = Opcode::REG_CELL_REF;
  opcode_map[make_symbol("reg-cell-set!")] = Opcode::REG_CELL_SET;
  opcode_map[make_symbol("make-cell")] = Opcode::MAKE_CELL;
  opcode_map[make_symbol("safepoint")] = Opcode::SAFEPOINT;
  object_heap_t* heap = object_heap_t::current();
  for (const auto& pair : opcode_map) {
    if (!context::is_gc_protected(pair.first)) context::gc_protect(pair.first);
  }

  // initialize code_ptr dispatch maps
  nullary_code_map[(void*)subr_unspecified] = &codegen_t::emit_unspecified_subr;

  unary_code_map[(void*)subr_car] = &codegen_t::emit_car_subr;
  unary_code_map[(void*)subr_cdr] = &codegen_t::emit_cdr_subr;
  unary_code_map[(void*)subr_not] = &codegen_t::emit_not_subr;
  unary_code_map[(void*)subr_null_p] = &codegen_t::emit_null_p_subr;
  unary_code_map[(void*)subr_pair_p] = &codegen_t::emit_pair_p_subr;

  binary_code_map[(void*)subr_eq_p] = &codegen_t::emit_eq_p_subr;
  binary_code_map[(void*)subr_num_add] = &codegen_t::emit_num_add_subr;
  binary_code_map[(void*)subr_num_sub] = &codegen_t::emit_num_sub_subr;
  binary_code_map[(void*)subr_num_mul] = &codegen_t::emit_num_mul_subr;
  binary_code_map[(void*)subr_num_eq] = &codegen_t::emit_num_eq_subr;
  binary_code_map[(void*)subr_num_lt] = &codegen_t::emit_num_lt_subr;
  binary_code_map[(void*)subr_num_gt] = &codegen_t::emit_num_gt_subr;
  binary_code_map[(void*)subr_num_le] = &codegen_t::emit_num_le_subr;
  binary_code_map[(void*)subr_num_ge] = &codegen_t::emit_num_ge_subr;
  binary_code_map[(void*)subr_append] = &codegen_t::emit_append2_subr;
  binary_code_map[(void*)subr_vector_ref] = &codegen_t::emit_vector_ref_subr;
  binary_code_map[(void*)subr_tuple_ref] = &codegen_t::emit_tuple_ref_subr;

  ternary_code_map[(void*)subr_vector_set] = &codegen_t::emit_vector_set_subr;
  ternary_code_map[(void*)subr_tuple_set] = &codegen_t::emit_tuple_set_subr;

  tc6_code_map[(void*)subr_symbol_p] = tc6_symbol;
  tc6_code_map[(void*)subr_string_p] = tc6_string;
  tc6_code_map[(void*)subr_vector_p] = tc6_vector;
  tc6_code_map[(void*)subr_hashtable_p] = tc6_hashtable;
  tc6_code_map[(void*)subr_tuple_p] = tc6_tuple;
  tc6_code_map[(void*)subr_continuation_p] = tc6_continuation;

  // arithmetic
  no_gc_code_set.insert((void*)subr_num_eq);
  no_gc_code_set.insert((void*)subr_num_lt);
  no_gc_code_set.insert((void*)subr_num_gt);
  no_gc_code_set.insert((void*)subr_num_le);
  no_gc_code_set.insert((void*)subr_num_ge);

  // list/vector/string accessors and mutators
  no_gc_code_set.insert((void*)subr_set_car);
  no_gc_code_set.insert((void*)subr_set_cdr);
  no_gc_code_set.insert((void*)subr_length);
  no_gc_code_set.insert((void*)subr_list_ref);
  no_gc_code_set.insert((void*)subr_memq);
  no_gc_code_set.insert((void*)subr_memv);
  no_gc_code_set.insert((void*)subr_assq);
  no_gc_code_set.insert((void*)subr_assv);
  no_gc_code_set.insert((void*)subr_list_p);
  no_gc_code_set.insert((void*)subr_vector_length);
  no_gc_code_set.insert((void*)subr_vector_ref);
  no_gc_code_set.insert((void*)subr_vector_set);
  no_gc_code_set.insert((void*)subr_string_length);
  no_gc_code_set.insert((void*)subr_string_ref);
  no_gc_code_set.insert((void*)subr_string_eq);

  // basic predicates and logic
  no_gc_code_set.insert((void*)subr_boolean_p);
  no_gc_code_set.insert((void*)subr_continuation_p);
  no_gc_code_set.insert((void*)subr_char_eq);
  no_gc_code_set.insert((void*)subr_char_numeric_p);
  no_gc_code_set.insert((void*)subr_char_p);
  no_gc_code_set.insert((void*)subr_eq_p);
  no_gc_code_set.insert((void*)subr_eqv_p);
  no_gc_code_set.insert((void*)subr_exact_p);
  no_gc_code_set.insert((void*)subr_hashtable_p);
  no_gc_code_set.insert((void*)subr_inexact_p);
  no_gc_code_set.insert((void*)subr_infinite_p);
  no_gc_code_set.insert((void*)subr_integer_p);
  no_gc_code_set.insert((void*)subr_number_p);
  no_gc_code_set.insert((void*)subr_nan_p);
  no_gc_code_set.insert((void*)subr_not);
  no_gc_code_set.insert((void*)subr_null_p);
  no_gc_code_set.insert((void*)subr_pair_p);
  no_gc_code_set.insert((void*)subr_procedure_p);
  no_gc_code_set.insert((void*)subr_real_p);
  no_gc_code_set.insert((void*)subr_symbol_p);
  no_gc_code_set.insert((void*)subr_string_p);
  no_gc_code_set.insert((void*)subr_tuple_p);
  no_gc_code_set.insert((void*)subr_vector_p);

  // environment and hash
  no_gc_code_set.insert((void*)subr_environment_macros);
  no_gc_code_set.insert((void*)subr_environment_variables);
  no_gc_code_set.insert((void*)subr_environment_macro_ref);
  no_gc_code_set.insert((void*)subr_environment_variable_ref);
  no_gc_code_set.insert((void*)subr_environment_macro_contains);
  no_gc_code_set.insert((void*)subr_environment_variable_contains);
  no_gc_code_set.insert((void*)subr_interaction_environment);
  no_gc_code_set.insert((void*)subr_system_environment);
  no_gc_code_set.insert((void*)subr_equal_hash);
  no_gc_code_set.insert((void*)subr_hashtable_delete);
  no_gc_code_set.insert((void*)subr_hashtable_clear);

  // I/O and misc
  no_gc_code_set.insert((void*)subr_current_input_port);
  no_gc_code_set.insert((void*)subr_current_output_port);
  no_gc_code_set.insert((void*)subr_current_error_port);
  no_gc_code_set.insert((void*)subr_standard_input_port);
  no_gc_code_set.insert((void*)subr_standard_output_port);
  no_gc_code_set.insert((void*)subr_standard_error_port);
  no_gc_code_set.insert((void*)subr_file_exists_p);
  no_gc_code_set.insert((void*)subr_eof_object_p);
  no_gc_code_set.insert((void*)subr_fixnum_p);
  no_gc_code_set.insert((void*)subr_undefined);
  no_gc_code_set.insert((void*)subr_unspecified);
  no_gc_code_set.insert((void*)subr_undefined_p);
  no_gc_code_set.insert((void*)subr_unspecified_p);
  no_gc_code_set.insert((void*)subr_cyclic_object_p);
  no_gc_code_set.insert((void*)subr_tuple_set);
  no_gc_code_set.insert((void*)subr_tuple_ref);

  // cxr
  no_gc_code_set.insert((void*)subr_car);
  no_gc_code_set.insert((void*)subr_cdr);
  no_gc_code_set.insert((void*)subr_caar);
  no_gc_code_set.insert((void*)subr_cadr);
  no_gc_code_set.insert((void*)subr_cdar);
  no_gc_code_set.insert((void*)subr_cddr);
  no_gc_code_set.insert((void*)subr_caaar);
  no_gc_code_set.insert((void*)subr_caadr);
  no_gc_code_set.insert((void*)subr_cadar);
  no_gc_code_set.insert((void*)subr_caddr);
  no_gc_code_set.insert((void*)subr_cdaar);
  no_gc_code_set.insert((void*)subr_cdadr);
  no_gc_code_set.insert((void*)subr_cddar);
  no_gc_code_set.insert((void*)subr_cdddr);
  no_gc_code_set.insert((void*)subr_caaaar);
  no_gc_code_set.insert((void*)subr_caaadr);
  no_gc_code_set.insert((void*)subr_caadar);
  no_gc_code_set.insert((void*)subr_caaddr);
  no_gc_code_set.insert((void*)subr_cadaar);
  no_gc_code_set.insert((void*)subr_cadadr);
  no_gc_code_set.insert((void*)subr_caddar);
  no_gc_code_set.insert((void*)subr_cadddr);
  no_gc_code_set.insert((void*)subr_cdaaar);
  no_gc_code_set.insert((void*)subr_cdaadr);
  no_gc_code_set.insert((void*)subr_cdadar);
  no_gc_code_set.insert((void*)subr_cdaddr);
  no_gc_code_set.insert((void*)subr_cddaar);
  no_gc_code_set.insert((void*)subr_cddadr);
  no_gc_code_set.insert((void*)subr_cdddar);
  no_gc_code_set.insert((void*)subr_cddddr);
  no_gc_code_set.insert((void*)subr_caaaaar);
  no_gc_code_set.insert((void*)subr_caaaadr);
  no_gc_code_set.insert((void*)subr_caaadar);
  no_gc_code_set.insert((void*)subr_caaaddr);
  no_gc_code_set.insert((void*)subr_caadaar);
  no_gc_code_set.insert((void*)subr_caadadr);
  no_gc_code_set.insert((void*)subr_caaddar);
  no_gc_code_set.insert((void*)subr_caadddr);
  no_gc_code_set.insert((void*)subr_cadaaar);
  no_gc_code_set.insert((void*)subr_cadaadr);
  no_gc_code_set.insert((void*)subr_cadadar);
  no_gc_code_set.insert((void*)subr_cadaddr);
  no_gc_code_set.insert((void*)subr_caddaar);
  no_gc_code_set.insert((void*)subr_caddadr);
  no_gc_code_set.insert((void*)subr_cadddar);
  no_gc_code_set.insert((void*)subr_caddddr);
  no_gc_code_set.insert((void*)subr_cdaaaar);
  no_gc_code_set.insert((void*)subr_cdaaadr);
  no_gc_code_set.insert((void*)subr_cdaadar);
  no_gc_code_set.insert((void*)subr_cdaaddr);
  no_gc_code_set.insert((void*)subr_cdadaar);
  no_gc_code_set.insert((void*)subr_cdadadr);
  no_gc_code_set.insert((void*)subr_cdaddar);
  no_gc_code_set.insert((void*)subr_cdadddr);
  no_gc_code_set.insert((void*)subr_cddaaar);
  no_gc_code_set.insert((void*)subr_cddaadr);
  no_gc_code_set.insert((void*)subr_cddadar);
  no_gc_code_set.insert((void*)subr_cddaddr);
  no_gc_code_set.insert((void*)subr_cdddaar);
  no_gc_code_set.insert((void*)subr_cdddadr);
  no_gc_code_set.insert((void*)subr_cddddar);
  no_gc_code_set.insert((void*)subr_cdddddr);

  // Higher-order functions that call their closure argument(s) but never store
  // them on the heap.  A closure passed to any of these as an argument is
  // therefore non-escaping from the GC's perspective.
  //
  // Inclusion criteria:
  //   - The function applies its proc/predicate/comparator argument to list /
  //     vector / string elements without retaining it beyond the call.
  //   - Notably excluded: call/cc (continuation captures the stack),
  //     dynamic-wind (stores thunks in the dynamic-wind record).
  //
  // R7RS list / vector / string traversal
  auto intern_safe = [&](const char* name) {
    scm_obj_t sym = make_symbol(name);
    if (!context::is_gc_protected(sym)) context::gc_protect(sym);
    proc_arg_safe_callees.insert(sym);
  };

  intern_safe("map");
  intern_safe("for-each");
  intern_safe("filter");
  intern_safe("filter-map");  // SRFI-1
  intern_safe("partition");
  intern_safe("remove");
  intern_safe("find");
  intern_safe("for-all");       // SRFI-1 / R7RS-small
  intern_safe("every");         // SRFI-1 alias
  intern_safe("exists");        // SRFI-1 / R7RS-large
  intern_safe("any");           // SRFI-1 alias
  intern_safe("fold");          // SRFI-1
  intern_safe("fold-right");    // SRFI-1
  intern_safe("fold-left");     // R7RS-large / SRFI-1
  intern_safe("reduce");        // SRFI-1
  intern_safe("reduce-right");  // SRFI-1
  intern_safe("count");         // SRFI-1
  intern_safe("append-map");    // SRFI-1
  intern_safe("flat-map");      // common alias
  intern_safe("filter-fold");
  intern_safe("list-sort");            // R7RS / SRFI-132
  intern_safe("sort");                 // common
  intern_safe("sort!");                // in-place sort — predicate not stored
  intern_safe("vector-map");           // R7RS
  intern_safe("vector-for-each");      // R7RS
  intern_safe("string-for-each");      // R7RS
  intern_safe("string-map");           // R7RS
  intern_safe("vector-sort");          // SRFI-132
  intern_safe("vector-sort!");         // SRFI-132
  intern_safe("vector-stable-sort");   // SRFI-132
  intern_safe("vector-stable-sort!");  // SRFI-132
  intern_safe("iota");                 // SRFI-1 (no proc, but harmless)
}
