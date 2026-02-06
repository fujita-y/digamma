// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef CODEGEN_H_INCLUDED
#define CODEGEN_H_INCLUDED

#include "core.h"
#include "object.h"

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

#include <map>
#include <string>
#include <vector>

class codegen_t {
  llvm::LLVMContext& context;
  llvm::Module* module;
  llvm::IRBuilder<> builder;
  llvm::ExecutionEngine* engine;

  // Register mapping: index -> current llvm value
  std::vector<llvm::Value*> registers;
  int num_registers = 0;

  // Labels: name -> basic block
  std::map<scm_obj_t, llvm::BasicBlock*> labels;

  // Cached symbols
  scm_obj_t sym_const;
  scm_obj_t sym_mov;
  scm_obj_t sym_if;
  scm_obj_t sym_jump;
  scm_obj_t sym_label;
  scm_obj_t sym_ret;

  // Phi nodes at the start of each block for every register
  // block -> register_index -> PhiNode
  std::map<llvm::BasicBlock*, std::vector<llvm::PHINode*>> block_phis;

  // Helper to get register value
  llvm::Value* get_reg(int idx);

  // Helper to set register value (update current mapping)
  void set_reg(int idx, llvm::Value* val);

  // Helper to ensure phi nodes for a target block
  void ensure_phis(llvm::BasicBlock* target);

  // Helper to add incoming values to phi nodes of a target block
  void add_phi_incoming(llvm::BasicBlock* target);

  void emit_inst(scm_obj_t inst);
  void emit_const(scm_obj_t args);
  void emit_mov(scm_obj_t args);
  void emit_if(scm_obj_t args);
  void emit_jump(scm_obj_t args);
  void emit_label(scm_obj_t args);
  void emit_ret(scm_obj_t args);

 public:
  codegen_t(llvm::LLVMContext& ctx, llvm::Module* mod, llvm::ExecutionEngine* ee);

  // Compile a list of instructions
  intptr_t compile(scm_obj_t inst_list);
};

#endif
