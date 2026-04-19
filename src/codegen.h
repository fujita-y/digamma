// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef CODEGEN_H_INCLUDED
#define CODEGEN_H_INCLUDED

#include "core.h"
#include "object.h"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include "nanos_jit.h"

#include <unordered_map>
#include <unordered_set>
#include <vector>

// ============================================================================
//  Opcode enum
// ============================================================================

enum class Opcode {
  CONST,
  MOV,
  IF,
  JUMP,
  LABEL,
  CALL,
  TAIL_CALL,
  RET,
  MAKE_CLOSURE,
  CLOSURE_REF,
  CLOSURE_SET,
  CLOSURE_SELF,
  CLOSURE_CELL_REF,
  CLOSURE_CELL_SET,
  REG_CELL_REF,
  REG_CELL_SET,
  MAKE_CELL,
  GLOBAL_SET,
  GLOBAL_REF,
  SAFEPOINT,
  UNKNOWN
};

// ============================================================================
//  Instruction
// ============================================================================

struct Instruction {
  Opcode op;
  scm_obj_t original;
  int rn1 = -1;              // reg num at operand position 1
  int rn2 = -1;              // reg num at operand position 2
  int rn3 = -1;              // reg num at operand position 3
  scm_obj_t opr1 = scm_nil;  // symbol at operand position 1
  scm_obj_t opr2 = scm_nil;  // symbol or literal at operand position 2
  int argc = 0;
  bool has_rest = false;
  scm_obj_t free_indices = scm_nil;
  scm_obj_t closure_label = scm_nil;
  // For MAKE_CLOSURE: true if the closure cannot escape to heap-reachable memory,
  // i.e. it is only used directly as a callee within the same compile unit and
  // is never stored to globals, cells, or passed as a non-callee argument.
  // Used to elide write barriers on stores of this closure's value.
  bool no_escape = false;
  // Stronger than no_escape: the closure value never flowed through a heap cell
  // (cell_aliases stayed empty during the escape scan).  Safe to stack-allocate
  // because the struct cannot be accessed after the creating frame returns.
  bool stack_alloc = false;
};

// ============================================================================
//  compiled_code_t — RAII wrapper for JIT-compiled code
// ============================================================================

struct compiled_code_t {
  intptr_t (*func_ptr)(void) = nullptr;
  llvm::orc::ResourceTrackerSP tracker;

  compiled_code_t() = default;
  compiled_code_t(intptr_t (*f)(void), llvm::orc::ResourceTrackerSP rt) : func_ptr(f), tracker(std::move(rt)) {}
  ~compiled_code_t();

  compiled_code_t(const compiled_code_t&) = delete;
  compiled_code_t& operator=(const compiled_code_t&) = delete;

  compiled_code_t(compiled_code_t&& other);
  compiled_code_t& operator=(compiled_code_t&& other);

  intptr_t release_and_run();

  operator bool() const { return func_ptr != nullptr; }
};

// ============================================================================
//  codegen_t — bytecode-to-LLVM-IR compiler
// ============================================================================

class codegen_t {
  friend class ClosureAnalysisTest;

 public:
  typedef scm_obj_t (*bridge_func_t)(scm_obj_t, int, scm_obj_t*);

 private:
  // --------------------------------------------------------------------------
  //  Per-function metadata
  // --------------------------------------------------------------------------

  struct FunctionInfo {
    llvm::Function* llvm_function = nullptr;
    std::vector<Instruction> instructions;
    int max_reg = -1;           // -1 means no registers used
    int argc = 0;               // For closures
    bool has_rest = false;      // For closures
    scm_obj_t label = scm_nil;  // Closure label or nil for main
  };

  // --------------------------------------------------------------------------
  //  Static / thread-local state
  // --------------------------------------------------------------------------

  thread_local static codegen_t* s_current;

  // --------------------------------------------------------------------------
  //  LLVM infrastructure
  // --------------------------------------------------------------------------

  std::unique_ptr<llvm::LLVMContext> context_uptr;  // owns the current compilation context
  std::unique_ptr<llvm::IRBuilder<>> builder;       // in-place builder, recreated per compile()
  nanos_jit_t* jit;

  llvm::Module* main_module = nullptr;  // current module being compiled
  std::unique_ptr<llvm::Module> main_module_uptr;
  llvm::Module* closure_module = nullptr;
  std::unique_ptr<llvm::Module> closure_module_uptr;

  // --------------------------------------------------------------------------
  //  Code generation state
  // --------------------------------------------------------------------------

  llvm::Function* main_function = nullptr;
  llvm::Function* current_function = nullptr;
  llvm::Value* current_closure_self = nullptr;  // 'self' argument of the current closure

  std::vector<FunctionInfo> functions;
  FunctionInfo* current_function_info = nullptr;

  std::vector<llvm::AllocaInst*> allocas;                   // register index -> alloca
  std::unordered_map<scm_obj_t, llvm::BasicBlock*> labels;  // label name -> basic block

  // --------------------------------------------------------------------------
  //  Closure metadata
  // --------------------------------------------------------------------------

  std::unordered_map<scm_obj_t, llvm::Function*> function_map;  // label symbol -> llvm function

  // --------------------------------------------------------------------------
  //  Cached values
  // --------------------------------------------------------------------------

  scm_obj_t cached_symbol_label;
  scm_obj_t cached_symbol_apply;
  scm_obj_t cached_symbol_safepoint;
  bridge_func_t cached_call_closure_bridge = nullptr;

  std::unordered_map<scm_obj_t, Opcode> opcode_map;
  std::unordered_map<void*, void (codegen_t::*)(bool)> nullary_code_map;
  std::unordered_map<void*, void (codegen_t::*)(bool)> unary_code_map;
  std::unordered_map<void*, void (codegen_t::*)(bool)> binary_code_map;
  std::unordered_map<void*, void (codegen_t::*)(bool)> ternary_code_map;
  std::unordered_map<void*, int> tc6_code_map;
  std::unordered_set<void*> no_gc_code_set;
  // Symbols of known higher-order functions that call their closure arguments
  // but never store them on the heap.  Used by phase2b_analyze_no_escape to
  // avoid falsely marking a closure as escaped when it is only passed to one
  // of these safe callees.
  std::unordered_set<scm_obj_t> proc_arg_safe_callees;

  std::vector<scm_obj_t> gc_protected_objects;

  // --------------------------------------------------------------------------
  //  Compilation pipeline (phases)
  // --------------------------------------------------------------------------

  // RAII helper: swaps in a fresh LLVMContext+IRBuilder for the duration of a
  // compile() call, then restores the previous state on destruction.
  struct CompileScope {
    codegen_t& self;
    std::unique_ptr<llvm::LLVMContext> saved_ctx;
    CompileScope(codegen_t& self);
    ~CompileScope();
  };

  void phase0_create_module();
  void phase1_parse_instructions(scm_obj_t inst_list);
  void phase2a_analyze_closure_labels();
  void phase2b_analyze_no_escape();
  void phase2c_analyze_safepoints();
  void phase2d_analyze_cell_stack_alloc();
  void phase3_create_functions();
  void phase4_generate_code();
  void phase5_optimize_and_verify();
  compiled_code_t phase6_finalize();
  void optimize_module(llvm::Module& mod);
  void reset_compile_state();

  // Configure a new Module with JIT data layout, target triple, PIC/PIE level
  void configure_module(llvm::Module& M);
  std::string generate_unique_suffix();

  // --------------------------------------------------------------------------
  //  Instruction parsing
  // --------------------------------------------------------------------------

  void init_opcode_map();
  void parse_instructions(scm_obj_t inst_list);
  void parse_single_instruction(scm_obj_t inst_obj, FunctionInfo& func_info, scm_obj_t& current_closure_label);
  void parse_const(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info, scm_obj_t& current_closure_label);
  void parse_mov(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info);
  void parse_if(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info);
  void parse_jump(const scm_obj_t& inst_obj, Instruction& inst);
  void parse_label(const scm_obj_t& inst_obj, Instruction& inst, scm_obj_t& current_closure_label);
  void parse_ret(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info);
  void parse_make_closure(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info);
  void parse_global_set(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info);
  void parse_global_ref(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info);
  void parse_call(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info);
  void parse_tail_call(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info);
  void parse_closure_ref(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info);
  void parse_closure_set(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info);
  void parse_closure_self(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info);
  void parse_closure_cell_ref(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info);
  void parse_closure_cell_set(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info);
  void parse_reg_cell_ref(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info);
  void parse_reg_cell_set(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info);
  void parse_make_cell(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info);

  // --------------------------------------------------------------------------
  //  IR emission — per-instruction emitters
  // --------------------------------------------------------------------------

  void emit_inst(const Instruction& inst);
  void emit_const(const Instruction& inst);
  void emit_mov(const Instruction& inst);
  void emit_if(const Instruction& inst);
  void emit_jump(const Instruction& inst);
  void emit_label(const Instruction& inst);
  void emit_ret(const Instruction& inst);
  void emit_make_closure(const Instruction& inst);
  void emit_global_set(const Instruction& inst);
  void emit_global_ref(const Instruction& inst);
  void emit_call(const Instruction& inst);
  void emit_tail_call(const Instruction& inst);
  void emit_closure_ref(const Instruction& inst);
  void emit_closure_set(const Instruction& inst);
  void emit_closure_self(const Instruction& inst);
  void emit_closure_cell_ref(const Instruction& inst);
  void emit_closure_cell_set(const Instruction& inst);
  void emit_reg_cell_ref(const Instruction& inst);
  void emit_reg_cell_set(const Instruction& inst);
  void emit_make_cell(const Instruction& inst);
  void emit_safepoint(const Instruction& inst);

  // --------------------------------------------------------------------------
  //  IR emission — call helpers
  // --------------------------------------------------------------------------

  void emit_call_common(const Instruction& inst, bool is_tail);
  void emit_apply_call(const Instruction& inst, bool is_tail);
  void emit_known_closure_call(const Instruction& inst, bool is_tail);
  void emit_generic_closure_call(const Instruction& inst, bool is_tail);
  void emit_generic_rest_call(llvm::Value* closure, llvm::Value* code_void_ptr, llvm::Value* is_cdecl, const Instruction& inst, bool is_tail,
                              llvm::BasicBlock* merge_block, llvm::BasicBlock*& rest_exit_block);
  void emit_generic_normal_call(llvm::Value* closure, llvm::Value* code_void_ptr, llvm::Value* is_cdecl, const Instruction& inst, bool is_tail,
                                llvm::BasicBlock* merge_block, llvm::BasicBlock*& normal_exit_block);

  // --------------------------------------------------------------------------
  //  Inline primitives
  // --------------------------------------------------------------------------
  void emit_unspecified_subr(bool is_tail);
  void emit_null_p_subr(bool is_tail);
  void emit_pair_p_subr(bool is_tail);
  void emit_tc6_predicate(int tc6_num, bool is_tail);
  void emit_eq_p_subr(bool is_tail);
  void emit_not_subr(bool is_tail);
  void emit_car_subr(bool is_tail);
  void emit_cdr_subr(bool is_tail);
  void emit_num_add_subr(bool is_tail);
  void emit_num_sub_subr(bool is_tail);
  void emit_num_mul_subr(bool is_tail);
  void emit_num_eq_subr(bool is_tail);
  void emit_num_lt_subr(bool is_tail);
  void emit_num_gt_subr(bool is_tail);
  void emit_num_le_subr(bool is_tail);
  void emit_num_ge_subr(bool is_tail);
  void emit_append2_subr(bool is_tail);
  void emit_vector_ref_subr(bool is_tail);
  void emit_vector_set_subr(bool is_tail);
  void emit_tuple_ref_subr(bool is_tail);
  void emit_tuple_set_subr(bool is_tail);

  void emit_write_barrier(llvm::Value* value);

  // --------------------------------------------------------------------------
  //  IR helpers — types, values, utilities
  // --------------------------------------------------------------------------

  llvm::Type* getInt64Type() { return llvm::Type::getInt64Ty(*context_uptr); }
  llvm::Type* getInt32Type() { return llvm::Type::getInt32Ty(*context_uptr); }
  llvm::Type* getVoidPtrType() { return builder->getPtrTy(); }
  llvm::Type* getInt64PtrType() { return llvm::PointerType::getUnqual(*context_uptr); }
  llvm::Value* getScmFalseValue() { return llvm::ConstantInt::get(*context_uptr, llvm::APInt(64, (uint64_t)scm_false)); }

  llvm::Value* get_reg(int idx);
  void set_reg(int idx, llvm::Value* val);
  void create_allocas(llvm::Function* f, int num_regs);
  void setup_closure_rest_arguments(int fixed_argc, llvm::Value* actual_argc, llvm::Value* argv_ptr);

  llvm::Value* getClosureCodePtr(llvm::Value* closure_tagged);

  llvm::Function* get_or_create_external_function(const char* name, llvm::FunctionType* type, void* symbol_ptr);
  void add_side_effect_free_attributes(llvm::Function* func);
  void add_never_return_attributes(llvm::Function* func);
  void add_common_closure_attributes(llvm::Function* func);

  // --------------------------------------------------------------------------
  //  Debugging
  // --------------------------------------------------------------------------

  void dump_instructions(const std::vector<Instruction>& instructions);

  // --------------------------------------------------------------------------
  //  Public interface
  // --------------------------------------------------------------------------

 public:
  codegen_t(std::unique_ptr<llvm::LLVMContext> ctx, nanos_jit_t* jit);

  compiled_code_t compile(scm_obj_t inst_list);

  llvm::Function* get_or_create_call_closure_bridge();
  bridge_func_t call_closure_bridge();

  nanos_jit_t* get_jit() const { return jit; }

  // Closure parameters: label symbol -> {fixed_argc, has_rest}
  std::unordered_map<scm_obj_t, std::pair<int, bool>> closure_params;
  std::unordered_map<scm_obj_t, scm_obj_t> global_closure_defs;

  // Cross-function closure-cell label tracking:
  //   closure_cell_labels[closure_label][cell_idx] = value_label
  //     Records the closure label stored in each cell slot of a closure,
  //     populated when reg-cell-set! writes a known closure into a captured
  //     cell register (make_closure_free_reg is a local in phase2, scoped
  //     per function to avoid false cross-function matches).
  std::unordered_map<scm_obj_t, std::unordered_map<int, scm_obj_t>> closure_cell_labels;

  void destroy() { s_current = nullptr; }

  static codegen_t* current() {
    assert(s_current);
    return s_current;
  }
};

#endif
