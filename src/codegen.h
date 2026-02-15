// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef CODEGEN_H_INCLUDED
#define CODEGEN_H_INCLUDED

#include "core.h"
#include "object.h"

#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

#include <map>
#include <vector>

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
  UNKNOWN
};

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
};

class codegen_t {
  llvm::orc::ThreadSafeContext ts_context;
  llvm::LLVMContext& context;
  llvm::orc::LLJIT* jit;
  llvm::IRBuilder<> builder;
  llvm::Module* module = nullptr;  // Current module being compiled
  std::unique_ptr<llvm::Module> current_module_uptr;
  llvm::Function* main_function = nullptr;

  // Register allocas: index -> alloca
  std::vector<llvm::AllocaInst*> allocas;

  // Labels: name -> basic block
  std::map<scm_obj_t, llvm::BasicBlock*> labels;

  // Cached symbols
  scm_obj_t sym_const;
  scm_obj_t sym_mov;
  scm_obj_t sym_if;
  scm_obj_t sym_jump;
  scm_obj_t sym_label;
  scm_obj_t sym_ret;
  scm_obj_t sym_make_closure;
  scm_obj_t sym_global_set;
  scm_obj_t sym_global_ref;
  scm_obj_t sym_call;
  scm_obj_t sym_tail_call;
  scm_obj_t sym_closure_ref;
  scm_obj_t sym_closure_set;
  scm_obj_t sym_closure_cell_set;
  scm_obj_t sym_apply;  // Added sym_apply

  scm_obj_t sym_closure_self;
  scm_obj_t sym_closure_cell_ref;
  scm_obj_t sym_reg_cell_ref;
  scm_obj_t sym_reg_cell_set;
  scm_obj_t sym_make_cell;

  // Closure literals: label symbol -> literals vector
  std::map<scm_obj_t, scm_obj_t> closure_literals;

  // Tracks closure entry points: label symbol -> llvm function
  std::map<scm_obj_t, llvm::Function*> function_map;

  // The function currently being generated
  llvm::Function* current_function = nullptr;

  // The 'self' argument of the current closure function
  llvm::Value* current_closure_self = nullptr;

  friend class ClosureAnalysisTest;

  struct FunctionInfo {
    llvm::Function* llvm_function = nullptr;
    std::vector<Instruction> instructions;
    int max_reg = -1;           // -1 means no registers used
    int argc = 0;               // For closures
    bool has_rest = false;      // For closures
    scm_obj_t label = scm_nil;  // Closure label or nil for main
  };

  std::vector<FunctionInfo> functions;
  FunctionInfo* current_function_info = nullptr;

  // Helper to get register value
  llvm::Value* get_reg(int idx);

  // Helper to set register value
  void set_reg(int idx, llvm::Value* val);

  // Parse list into instructions vector
  void parse_instructions(scm_obj_t inst_list);

  // Helper to parse a single instruction and update max_reg
  void parse_single_instruction(scm_obj_t inst_obj, FunctionInfo& func_info, scm_obj_t& current_closure_label,
                                std::vector<scm_obj_t>& current_literals);

  // Helper to finish collecting literals for a closure
  void finish_closure_literals(scm_obj_t& current_closure_label, std::vector<scm_obj_t>& current_literals);

  // Helper to get or create an external function declaration in the current module
  llvm::Function* get_or_create_external_function(const char* name, llvm::FunctionType* type, void* symbol_ptr);

  // Helper to get or create the fixed-arity apply stub
  llvm::Function* get_or_create_fixed_apply_stub();

  // Helper to create allocas at function entry
  void create_allocas(llvm::Function* f, int num_regs);

  // Helper to setup rest arguments for a closure function
  void setup_closure_rest_arguments(int fixed_argc, llvm::Value* actual_argc, llvm::Value* argv_ptr);

  // LLVM type helpers to reduce code duplication
  llvm::Type* getInt64Type() { return llvm::Type::getInt64Ty(context); }
  llvm::Type* getInt32Type() { return llvm::Type::getInt32Ty(context); }
  llvm::Type* getVoidPtrType() { return builder.getPtrTy(); }
  llvm::Value* getScmFalseValue() { return llvm::ConstantInt::get(context, llvm::APInt(64, (uint64_t)scm_false)); }

  // Helper to get code pointer from closure object
  llvm::Value* getClosureCodePtr(llvm::Value* closure_tagged);

  // Helper to emit write barrier call
  void emitWriteBarrier(llvm::Value* value);

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

  void emit_call_common(const Instruction& inst, bool is_tail);

  // Helper to initialize opcode map
  void init_opcode_map();

  // Opcode map for faster lookup
  std::map<scm_obj_t, Opcode> opcode_map;

  // Analysis pass
  void analyze_closure_labels();

  // Dump instructions for debugging
  void dump_instructions(const std::vector<Instruction>& instructions);

  // Compilation phases
  void phase0_create_module();
  void phase1_parse_instructions(scm_obj_t inst_list);
  void phase2_create_functions();
  void phase3_generate_code();
  void phase4_optimize_and_verify();
  intptr_t phase5_finalize();

 public:
  codegen_t(llvm::orc::ThreadSafeContext ts_ctx, llvm::orc::LLJIT* jit);

  // Compile a list of instructions
  intptr_t compile(scm_obj_t inst_list);

  // Helper to get or create the general call closure bridge
  llvm::Function* get_or_create_call_closure_bridge();

  // Helper to get the compiled address of the call closure bridge
  void* get_call_closure_bridge_ptr();

  // Closure parameters: label symbol -> {fixed_argc, has_rest}
  std::map<scm_obj_t, std::pair<int, bool>> closure_params;

  friend class ClosureAnalysisTest;
};

#endif
