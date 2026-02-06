// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "codegen.h"

#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Utils.h>

codegen_t::codegen_t(llvm::LLVMContext& ctx, llvm::Module* mod, llvm::ExecutionEngine* ee)
    : context(ctx), module(mod), builder(ctx), engine(ee) {
  sym_const = make_symbol("const");
  sym_mov = make_symbol("mov");
  sym_if = make_symbol("if");
  sym_jump = make_symbol("jump");
  sym_label = make_symbol("label");
  sym_ret = make_symbol("ret");
}

static int parse_reg(scm_obj_t s) {
  if (!is_symbol(s)) return -1;
  const char* str = (const char*)symbol_name(s);
  if (str[0] == 'r') {
    // Fast path: assume valid format like "r0", "r12"
    int val = 0;
    const char* p = str + 1;
    while (*p) {
      if (*p < '0' || *p > '9') return -1;
      val = val * 10 + (*p - '0');
      p++;
    }
    return val;
  }
  return -1;
}

// Helper macros for cons access
#define CAR(x)   (((scm_cons_rec_t*)(x))->car)
#define CDR(x)   (((scm_cons_rec_t*)(x))->cdr)
#define CAAR(x)  CAR(CAR(x))
#define CADR(x)  CAR(CDR(x))
#define CADDR(x) CAR(CDR(CDR(x)))

static int scan_registers(scm_obj_t inst_list, scm_obj_t sym_const, scm_obj_t sym_mov) {
  int max_reg = 0;
  scm_obj_t curr = inst_list;
  while (curr != scm_nil) {
    if (!is_cons(curr)) break;
    scm_obj_t inst = CAR(curr);
    if (is_cons(inst)) {
      scm_obj_t op = CAR(inst);
      if (op == sym_const) {
        // (const reg val)
        int r = parse_reg(CADR(inst));
        if (r > max_reg) max_reg = r;
      } else if (op == sym_mov) {
        // (mov dst src)
        int r1 = parse_reg(CADR(inst));
        int r2 = parse_reg(CADDR(inst));  // Note: parse_reg returns -1 for non-reg, effectively ignored
        if (r1 > max_reg) max_reg = r1;
        if (r2 > max_reg) max_reg = r2;
      }
    }
    curr = CDR(curr);
  }
  return max_reg;
}

llvm::Value* codegen_t::get_reg(int idx) {
  if (idx < 0 || idx >= num_registers) {
    throw std::runtime_error("Register index out of bounds");
  }
  llvm::Value* val = registers[idx];
  if (!val) {
    return llvm::ConstantInt::get(context, llvm::APInt(64, 0));
  }
  return val;
}

void codegen_t::set_reg(int idx, llvm::Value* val) {
  if (idx < 0 || idx >= num_registers) {
    throw std::runtime_error("Register index out of bounds");
  }
  registers[idx] = val;
}

void codegen_t::ensure_phis(llvm::BasicBlock* target) {
  if (block_phis.find(target) != block_phis.end()) return;

  llvm::IRBuilder<>::InsertPointGuard guard(builder);
  builder.SetInsertPoint(target);

  std::vector<llvm::PHINode*> phis;
  phis.resize(num_registers);

  for (int i = 0; i < num_registers; ++i) {
    phis[i] = builder.CreatePHI(llvm::Type::getInt64Ty(context), 0, "r" + std::to_string(i));
  }
  block_phis[target] = phis;
}

void codegen_t::add_phi_incoming(llvm::BasicBlock* target) {
  ensure_phis(target);
  std::vector<llvm::PHINode*>& phis = block_phis[target];
  llvm::BasicBlock* current_block = builder.GetInsertBlock();

  for (int i = 0; i < num_registers; ++i) {
    phis[i]->addIncoming(get_reg(i), current_block);
  }
}

intptr_t codegen_t::compile(scm_obj_t inst_list) {
  // 0. Scan for max register usage
  int max_reg = scan_registers(inst_list, sym_const, sym_mov);
  num_registers = max_reg + 1;
  registers.clear();
  registers.resize(num_registers, nullptr);
  block_phis.clear();
  labels.clear();

  // 1. Create a function type: intptr_t()
  llvm::FunctionType* funcType = llvm::FunctionType::get(llvm::Type::getInt64Ty(context), false);
  llvm::Function* function = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "scheme_func", module);

  // 2. Create entry block
  llvm::BasicBlock* entryBlock = llvm::BasicBlock::Create(context, "entry", function);
  builder.SetInsertPoint(entryBlock);

  // Initialize registers with 0/undef
  llvm::Value* zero = llvm::ConstantInt::get(context, llvm::APInt(64, 0));
  std::fill(registers.begin(), registers.end(), zero);

  // 3. Pre-scan for labels to create BasicBlocks
  scm_obj_t curr = inst_list;
  while (curr != scm_nil) {
    if (!is_cons(curr)) break;
    scm_obj_t inst = CAR(curr);
    if (is_cons(inst)) {
      scm_obj_t op_sym = CAR(inst);
      if (op_sym == sym_label) {
        scm_obj_t label_sym = CADR(inst);
        const char* label_name_str = (const char*)symbol_name(label_sym);
        labels[label_sym] = llvm::BasicBlock::Create(context, label_name_str, function);
      }
    }
    curr = CDR(curr);
  }

  // 4. Trace instructions
  curr = inst_list;
  bool block_terminated = false;

  while (curr != scm_nil) {
    if (!is_cons(curr)) break;
    scm_obj_t inst = CAR(curr);
    curr = CDR(curr);

    // If previous block terminated, we might have unreachable code or need a new block.
    // We proceed assuming the instruction stream is well-formed (labels follow jumps).

    emit_inst(inst);

    if (is_cons(inst)) {
      scm_obj_t op_sym = CAR(inst);
      if (op_sym == sym_ret || op_sym == sym_jump || op_sym == sym_if) {
        block_terminated = true;
      } else if (op_sym == sym_label) {
        block_terminated = false;
      }
    }
  }

  if (llvm::verifyFunction(*function, &llvm::errs())) {
    // Verification failed, error printed to stderr
  }

  // 5. Optimize
  llvm::legacy::FunctionPassManager fpm(module);
  // Add some standard O2 passes
  fpm.add(llvm::createInstructionCombiningPass());
  fpm.add(llvm::createReassociatePass());
  fpm.add(llvm::createGVNPass());
  fpm.add(llvm::createCFGSimplificationPass());
  fpm.add(llvm::createPromoteMemoryToRegisterPass());

  fpm.doInitialization();
  fpm.run(*function);

  std::error_code EC;
  llvm::raw_fd_ostream dest("/tmp/nanos.ll", EC, llvm::sys::fs::OF_None);
  if (EC) {
    llvm::errs() << "Could not open file: " << EC.message() << "\n";
  } else {
    module->print(dest, nullptr);
  }

  engine->finalizeObject();
  void* funcPtr = engine->getPointerToFunction(function);
  auto func = (intptr_t (*)())funcPtr;
  return func();
}

void codegen_t::emit_inst(scm_obj_t inst) {
  if (!is_cons(inst)) return;

  scm_obj_t op_sym = CAR(inst);
  scm_obj_t args = CDR(inst);

  if (op_sym == sym_const)
    emit_const(args);
  else if (op_sym == sym_mov)
    emit_mov(args);
  else if (op_sym == sym_if)
    emit_if(args);
  else if (op_sym == sym_jump)
    emit_jump(args);
  else if (op_sym == sym_label)
    emit_label(args);
  else if (op_sym == sym_ret)
    emit_ret(args);
}

void codegen_t::emit_const(scm_obj_t args) {
  scm_obj_t reg_sym = CAR(args);
  scm_obj_t val_obj = CADR(args);

  int reg_idx = parse_reg(reg_sym);
  if (reg_idx < 0) return;

  int64_t val = 0;
  if (is_fixnum(val_obj)) {
    val = fixnum(val_obj);
  } else if (val_obj == scm_false) {
    val = 0;
  } else if (val_obj == scm_true) {
    val = 1;
  } else {
    val = (int64_t)val_obj;
  }

  llvm::Value* v = llvm::ConstantInt::get(context, llvm::APInt(64, val));
  set_reg(reg_idx, v);
}

void codegen_t::emit_mov(scm_obj_t args) {
  scm_obj_t dst_sym = CAR(args);
  scm_obj_t src_sym = CADR(args);

  int dst_idx = parse_reg(dst_sym);
  int src_idx = parse_reg(src_sym);
  if (dst_idx < 0 || src_idx < 0) return;

  set_reg(dst_idx, get_reg(src_idx));
}

void codegen_t::emit_if(scm_obj_t args) {
  scm_obj_t l1_sym = CAR(args);
  scm_obj_t l2_sym = CADR(args);

  llvm::BasicBlock* b1 = labels[l1_sym];
  llvm::BasicBlock* b2 = labels[l2_sym];

  if (!b1) printf("ERROR: Label %s not found!\n", (char*)symbol_name(l1_sym));
  if (!b2) printf("ERROR: Label %s not found!\n", (char*)symbol_name(l2_sym));

  add_phi_incoming(b1);
  add_phi_incoming(b2);

  llvm::Value* cond = get_reg(0);
  llvm::Value* zero = llvm::ConstantInt::get(context, llvm::APInt(64, 0));
  llvm::Value* cmp = builder.CreateICmpNE(cond, zero, "cond");

  builder.CreateCondBr(cmp, b1, b2);
}

void codegen_t::emit_jump(scm_obj_t args) {
  scm_obj_t l1_sym = CAR(args);
  llvm::BasicBlock* target = labels[l1_sym];

  if (!target) printf("ERROR: Label %s not found!\n", (char*)symbol_name(l1_sym));

  add_phi_incoming(target);
  builder.CreateBr(target);
}

void codegen_t::emit_label(scm_obj_t args) {
  scm_obj_t l1_sym = CAR(args);
  llvm::BasicBlock* block = labels[l1_sym];

  llvm::BasicBlock* current = builder.GetInsertBlock();
  if (current->getTerminator() == nullptr) {
    add_phi_incoming(block);
    builder.CreateBr(block);
  }

  builder.SetInsertPoint(block);
  ensure_phis(block);

  std::vector<llvm::PHINode*>& phis = block_phis[block];
  for (int i = 0; i < num_registers; ++i) {
    registers[i] = phis[i];
  }
}

void codegen_t::emit_ret(scm_obj_t args) { builder.CreateRet(get_reg(0)); }
