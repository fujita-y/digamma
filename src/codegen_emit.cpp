// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "codegen.h"
#include "codegen_aux.h"
#include "codegen_common.h"
#include "context.h"
#include "object_heap.h"

#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/IR/MDBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/OptimizationLevel.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar/ADCE.h>
#include <llvm/Transforms/Scalar/EarlyCSE.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/SCCP.h>
#include <llvm/Transforms/Scalar/SROA.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>
#include <llvm/Transforms/Utils/SimplifyCFGOptions.h>

#define ENABLE_INLINE 1
#define BL            (*builder)
#define CT            (*context_uptr)

// ============================================================================
// Instruction Emission Switch
// ============================================================================

void codegen_t::emit_inst(const Instruction& inst) {
  switch (inst.op) {
    case Opcode::CONST:
      emit_const(inst);
      break;
    case Opcode::MOV:
      emit_mov(inst);
      break;
    case Opcode::IF:
      emit_if(inst);
      break;
    case Opcode::JUMP:
      emit_jump(inst);
      break;
    case Opcode::LABEL:
      emit_label(inst);
      break;
    case Opcode::RET:
      emit_ret(inst);
      break;
    case Opcode::MAKE_CLOSURE:
      emit_make_closure(inst);
      break;
    case Opcode::GLOBAL_SET:
      emit_global_set(inst);
      break;
    case Opcode::GLOBAL_REF:
      emit_global_ref(inst);
      break;
    case Opcode::CALL:
      emit_call(inst);
      break;
    case Opcode::TAIL_CALL:
      emit_tail_call(inst);
      break;
    case Opcode::CLOSURE_REF:
      emit_closure_ref(inst);
      break;
    case Opcode::CLOSURE_SET:
      emit_closure_set(inst);
      break;
    case Opcode::CLOSURE_SELF:
      emit_closure_self(inst);
      break;
    case Opcode::CLOSURE_CELL_REF:
      emit_closure_cell_ref(inst);
      break;
    case Opcode::CLOSURE_CELL_SET:
      emit_closure_cell_set(inst);
      break;
    case Opcode::REG_CELL_REF:
      emit_reg_cell_ref(inst);
      break;
    case Opcode::REG_CELL_SET:
      emit_reg_cell_set(inst);
      break;
    case Opcode::MAKE_CELL:
      emit_make_cell(inst);
      break;
    case Opcode::SAFEPOINT:
      emit_safepoint(inst);
      break;
    default:
      fatal("%s:%u codegen: unknown opcode encountered during emission", __FILE__, __LINE__);
      break;
  }
}

// ============================================================================
// Basic Instruction Emitters
// ============================================================================

void codegen_t::emit_safepoint(const Instruction& inst) {
  // Get the address of the stop-the-world flag from the heap
  std::atomic<bool>* poll_ptr = object_heap_t::current()->stop_the_world_ptr();
  llvm::Value* poll_addr_const = createInt64Constant(CT, (uint64_t)poll_ptr);
  llvm::Value* poll_val_ptr = BL.CreateIntToPtr(poll_addr_const, BL.getPtrTy());

  // Load of the flag
  llvm::LoadInst* load = BL.CreateLoad(BL.getInt8Ty(), poll_val_ptr, "safepoint_poll");
  load->setAlignment(llvm::Align(1));

  // Check if a stop-the-world event is requested (flag != 0)
  llvm::Value* is_requested = BL.CreateICmpNE(load, BL.getInt8(0), "safepoint_requested");

  // Create blocks for the fast and slow paths
  llvm::BasicBlock* slow_path = llvm::BasicBlock::Create(CT, "safepoint_slow", current_function);
  llvm::BasicBlock* fast_path = llvm::BasicBlock::Create(CT, "safepoint_fast", current_function);

  // Use branch weights to tell LLVM that the slow path is extremely unlikely
  llvm::MDBuilder MDB(CT);
  BL.CreateCondBr(is_requested, slow_path, fast_path, MDB.createBranchWeights(1, 1000000));

  // --- Slow Path: Call the runtime handler ---
  BL.SetInsertPoint(slow_path);
  llvm::FunctionType* ft = llvm::FunctionType::get(BL.getVoidTy(), false);
  llvm::Function* safepoint_func = get_or_create_external_function("c_safepoint", ft, (void*)&c_safepoint);
  BL.CreateCall(ft, safepoint_func);
  BL.CreateBr(fast_path);

  // --- Fast Path: Continue execution ---
  BL.SetInsertPoint(fast_path);
}

// Emit a constant value to a register
void codegen_t::emit_const(const Instruction& inst) {
  if (inst.rn1 < 0) return;
  uint64_t val = (uint64_t)inst.opr1;
  llvm::Value* v = createInt64Constant(CT, val);
  set_reg(inst.rn1, v);
}

// Move value from one register to another
void codegen_t::emit_mov(const Instruction& inst) {
  if (inst.rn1 < 0 || inst.rn2 < 0) return;
  set_reg(inst.rn1, get_reg(inst.rn2));
}

// Conditional branch based on r0 value (#f vs everything else)
void codegen_t::emit_if(const Instruction& inst) {
  llvm::BasicBlock* b1 = labels[inst.opr1];
  llvm::BasicBlock* b2 = labels[inst.opr2];

  if (!b1) {
    fatal("%s:%u codegen: if instruction label not found for true branch", __FILE__, __LINE__);
  }
  if (!b2) {
    fatal("%s:%u codegen: if instruction label not found for false branch", __FILE__, __LINE__);
  }

  llvm::Value* cond = get_reg(0);
  llvm::Value* scm_false_v = this->getScmFalseValue();
  llvm::Value* cmp = BL.CreateICmpNE(cond, scm_false_v, "cond");
  BL.CreateCondBr(cmp, b1, b2);
}

// Unconditional jump to label
void codegen_t::emit_jump(const Instruction& inst) {
  llvm::BasicBlock* target = labels[inst.opr1];
  if (!target) {
    fatal("%s:%u codegen: jump target label not found", __FILE__, __LINE__);
  }
  BL.CreateBr(target);
}

// Set insertion point to label's basic block
void codegen_t::emit_label(const Instruction& inst) {
  llvm::BasicBlock* block = labels[inst.opr1];
  if (!block) {
    fatal("%s:%u codegen: label basic block not found", __FILE__, __LINE__);
  }
  llvm::BasicBlock* current = BL.GetInsertBlock();

  if (current->getTerminator() == nullptr) {
    BL.CreateBr(block);
  }

  BL.SetInsertPoint(block);
}

// Return r0 as function result
void codegen_t::emit_ret(const Instruction& inst) { BL.CreateRet(get_reg(0)); }

// Create a closure object with captured environment
void codegen_t::emit_make_closure(const Instruction& inst) {
  llvm::Function* target_func = function_map[inst.opr1];
  if (!target_func) {
    fatal("%s:%u codegen: closure function not found for label", __FILE__, __LINE__);
  }

  llvm::Function* closure_func = this->main_module->getFunction(target_func->getName());
  if (!closure_func) {
    closure_func =
        llvm::Function::Create(target_func->getFunctionType(), llvm::Function::ExternalLinkage, target_func->getName(), this->main_module);
  }

  // Count free variables
  int nenv = count_list_length(inst.free_indices);

  // Common types
  llvm::Type* intptrTy = this->getInt64Type();
  llvm::Type* voidPtrTy = this->getVoidPtrType();
  llvm::Type* int32Ty = this->getInt32Type();

  // Prepare common arguments
  llvm::Value* code_ptr = closure_func;
  llvm::Value* argc = createInt32Constant(CT, inst.argc);

  llvm::Value* closure;
  if (!inst.has_rest && nenv == 0) {
    // Simple closure optimization
    std::vector<llvm::Type*> simpleArgTypes = {voidPtrTy, int32Ty};
    llvm::FunctionType* simpleFT = llvm::FunctionType::get(intptrTy, simpleArgTypes, false);
    llvm::Function* make_simple_closure_func = get_or_create_external_function("c_make_closure_s1", simpleFT, (void*)&c_make_closure_s1);
    closure = BL.CreateCall(simpleFT, make_simple_closure_func, {code_ptr, argc}, "closure");
  } else {
    // General case: Prepare environment array
    llvm::Value* env_array = nullptr;
    if (nenv > 0) {
      llvm::IRBuilder<> TmpB(&current_function->getEntryBlock(), current_function->getEntryBlock().begin());
      env_array = TmpB.CreateAlloca(intptrTy, createInt32Constant(CT, nenv), "env");
      scm_obj_t curr = inst.free_indices;
      for (int i = 0; i < nenv; i++) {
        int reg_idx = parse_reg(cons_car(curr));
        llvm::Value* reg_val = get_reg(reg_idx);
        llvm::Value* ptr = BL.CreateGEP(BL.getInt64Ty(), env_array, createInt32Constant(CT, i));
        BL.CreateStore(reg_val, ptr);
        curr = cons_cdr(curr);
      }
    } else {
      env_array = llvm::ConstantPointerNull::get(BL.getPtrTy());
    }

    llvm::FunctionType* ft = llvm::FunctionType::get(intptrTy, {voidPtrTy, int32Ty, int32Ty, int32Ty, voidPtrTy}, false);
    llvm::Function* make_closure_func = get_or_create_external_function("c_make_closure", ft, (void*)&c_make_closure);

    llvm::Value* rest = createInt32Constant(CT, inst.has_rest ? 1 : 0);
    llvm::Value* nenv_val = createInt32Constant(CT, nenv);
    llvm::Value* env_ptr = env_array;

    std::vector<llvm::Value*> callArgs = {code_ptr, argc, rest, nenv_val, env_ptr};
    closure = BL.CreateCall(ft, make_closure_func, callArgs, "closure");
  }

  set_reg(inst.rn1, closure);
}

// Set global variable to value in register
void codegen_t::emit_global_set(const Instruction& inst) {
  scm_obj_t cell = context::environment_variable_cell_ref(inst.opr1);
  scm_cell_rec_t* rec = (scm_cell_rec_t*)to_address(cell);

  // Create a constant for the value address
  llvm::Value* value_address = createInt64Constant(CT, (uint64_t)&(rec->value));

  if (inst.rn1 < 0) {
    fatal("%s:%u codegen: global-set! missing register operand", __FILE__, __LINE__);
  }
  llvm::Value* val = get_reg(inst.rn1);

  // Get pointer to cell's value
  llvm::Value* val_ptr = BL.CreateIntToPtr(value_address, BL.getPtrTy());

  // Store the value directly
  BL.CreateStore(val, val_ptr);

  emitWriteBarrier(val);
}

// Load global variable value into register
void codegen_t::emit_global_ref(const Instruction& inst) {
  // Resolve the address of the global variable's value slot at compile time
  scm_obj_t cell = context::environment_variable_cell_ref(inst.opr2);
  scm_cell_rec_t* rec = (scm_cell_rec_t*)to_address(cell);

  // Create a constant for the value address
  llvm::Value* value_address = createInt64Constant(CT, (uint64_t)&(rec->value));

  if (inst.rn1 < 0) {
    fatal("%s:%u codegen: global-ref missing register operand", __FILE__, __LINE__);
  }

  // Get pointer to cell's value
  llvm::Value* value_ptr = BL.CreateIntToPtr(value_address, BL.getPtrTy());

  // Load the value directly
  llvm::Value* val = BL.CreateLoad(this->getInt64Type(), value_ptr, "gref_val");

  if (rec->value == scm_undef && global_closure_defs.count(inst.opr2) == 0) {
    // Variable is unbound at compile time — it may be defined at runtime (e.g.
    // letrec forward refs), so emit a runtime guard.
    //
    // Structure:
    //   if (val == scm_undef) goto error_bb;   // cold / unlikely
    //   goto ok_bb;
    // error_bb:
    //   c_unbound_variable_error(name);  // NoReturn — never falls through
    //   unreachable
    // ok_bb:
    //   ... (set_reg below)

    llvm::Value* scm_undef_v = createInt64Constant(CT, (uint64_t)scm_undef);
    llvm::Value* is_undef = BL.CreateICmpEQ(val, scm_undef_v, "is_undef");

    llvm::BasicBlock* error_bb = llvm::BasicBlock::Create(CT, "gref_undef_err", current_function);
    llvm::BasicBlock* ok_bb = llvm::BasicBlock::Create(CT, "gref_ok", current_function);

    // Apply branch weights: error path is very unlikely (weight 1 vs 1000)
    llvm::MDBuilder mdb(CT);
    llvm::MDNode* weights = mdb.createBranchWeights(1, 1000);
    BL.CreateCondBr(is_undef, error_bb, ok_bb, weights);

    // --- error path ---
    BL.SetInsertPoint(error_bb);
    {
      // Intern the variable name as a module-level constant C string
      std::string var_name = to_string(inst.opr2);
      llvm::Value* name_ptr = BL.CreateGlobalString(var_name, "gref_name");

      // c_unbound_variable_error(const char* name) — NoReturn + Cold
      llvm::FunctionType* err_ft = llvm::FunctionType::get(BL.getVoidTy(), {BL.getPtrTy()}, false);
      llvm::Function* err_func = get_or_create_external_function("c_unbound_variable_error", err_ft, (void*)&c_unbound_variable_error);
      llvm::CallInst* err_call = BL.CreateCall(err_ft, err_func, {name_ptr});
      err_call->setCallingConv(llvm::CallingConv::C);
      // NoReturn function: terminate this block with unreachable
      BL.CreateUnreachable();
    }

    // --- normal path ---
    BL.SetInsertPoint(ok_bb);

#ifndef NDEBUG
    printf("forward global-ref: %s %s\n", to_string(inst.opr2).c_str(), to_string(rec->value).c_str());
#endif
  }

  set_reg(inst.rn1, val);
}

// ============================================================================
// Bridge and Call Helpers
// ============================================================================

llvm::Function* codegen_t::get_or_create_call_closure_bridge() {
  const char* name = "__nanos_call_closure_bridge";
  llvm::Function* f = main_module->getFunction(name);
  if (f) return f;

  // Signature: i64 (i64 closure, i64 argc, i64* argv)
  llvm::Type* i64 = this->getInt64Type();
  llvm::Type* i64_ptr = this->getInt64PtrType();
  llvm::FunctionType* ft = llvm::FunctionType::get(i64, {i64, i64, i64_ptr}, false);

  // Check if the bridge is already defined in the JIT
  if (auto sym = jit->lookup(name)) {
    // Symbol exists in JIT, just provide external declaration in this module
    auto f2 = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, name, main_module);
    f2->setDSOLocal(true);
    f2->addFnAttr(llvm::Attribute::NoInline);
    return f2;
  } else {
    // Consume the error (symbol not found)
    llvm::consumeError(sym.takeError());
  }

  f = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, name, main_module);
  f->setDSOLocal(true);
  f->addFnAttr(llvm::Attribute::NoInline);

  llvm::BasicBlock* saved_block = BL.GetInsertBlock();
  llvm::BasicBlock* entry = llvm::BasicBlock::Create(CT, "entry", f);
  BL.SetInsertPoint(entry);

  auto args = f->arg_begin();
  llvm::Value* closure = args++;
  closure->setName("closure");
  llvm::Value* argc = args++;
  argc->setName("argc");
  llvm::Value* argv = args++;
  argv->setName("argv");

  llvm::Value* code_ptr = getClosureCodePtr(closure);
  llvm::Value* closure_ptr = untagPointer(BL, CT, closure);

  // Load rest and cdecl flags
  llvm::Value* rest_ptr = BL.CreateConstInBoundsGEP1_32(BL.getInt8Ty(), closure_ptr, CLOSURE_REST_FIELD_OFFSET);
  llvm::Value* rest_field = BL.CreateLoad(BL.getInt32Ty(), rest_ptr, "rest");
  llvm::Value* is_rest = BL.CreateICmpNE(rest_field, BL.getInt32(0), "is_rest");

  llvm::Value* cdecl_ptr = BL.CreateConstInBoundsGEP1_32(BL.getInt8Ty(), closure_ptr, CLOSURE_CDECL_FIELD_OFFSET);
  llvm::Value* cdecl_field = BL.CreateLoad(BL.getInt32Ty(), cdecl_ptr, "cdecl");
  llvm::Value* is_cdecl = BL.CreateICmpNE(cdecl_field, BL.getInt32(0), "is_cdecl");

  llvm::BasicBlock* rest_b = llvm::BasicBlock::Create(CT, "rest", f);
  llvm::BasicBlock* fixed_b = llvm::BasicBlock::Create(CT, "fixed", f);
  BL.CreateCondBr(is_rest, rest_b, fixed_b);

  // Rest path: (self, argc, argv[])
  BL.SetInsertPoint(rest_b);
  {
    llvm::FunctionType* rest_ft = llvm::FunctionType::get(i64, {i64, i64, i64_ptr}, false);
    llvm::Value* fp = code_ptr;

    llvm::BasicBlock* cdecl_path = llvm::BasicBlock::Create(CT, "rest_cdecl", f);
    llvm::BasicBlock* scheme_path = llvm::BasicBlock::Create(CT, "rest_scheme", f);
    BL.CreateCondBr(is_cdecl, cdecl_path, scheme_path);

    BL.SetInsertPoint(cdecl_path);
    auto call_c = BL.CreateCall(rest_ft, fp, {closure, argc, argv});
    call_c->setCallingConv(llvm::CallingConv::C);
    BL.CreateRet(call_c);

    BL.SetInsertPoint(scheme_path);
    auto call_s = BL.CreateCall(rest_ft, fp, {closure, argc, argv});
    call_s->setCallingConv(CLOSURE_CALLING_CONV);
    BL.CreateRet(call_s);
  }

  // Fixed path: (self, arg0, arg1, ...)
  BL.SetInsertPoint(fixed_b);
  {
    llvm::BasicBlock* def_b = llvm::BasicBlock::Create(CT, "fixed_def", f);
    llvm::SwitchInst* sw = BL.CreateSwitch(argc, def_b, BRIDGE_MAX_ARGS + 1);
    for (int n = 0; n <= BRIDGE_MAX_ARGS; ++n) {
      llvm::BasicBlock* bb = llvm::BasicBlock::Create(CT, "fixed_" + std::to_string(n), f);
      sw->addCase(llvm::cast<llvm::ConstantInt>(createInt64Constant(CT, (uint64_t)n)), bb);
      BL.SetInsertPoint(bb);

      std::vector<llvm::Type*> pts(n + 1, i64);
      llvm::FunctionType* fixed_ft = llvm::FunctionType::get(i64, pts, false);
      llvm::Value* fp = code_ptr;

      std::vector<llvm::Value*> call_args;
      call_args.push_back(closure);
      for (int i = 0; i < n; ++i) {
        llvm::Value* p = BL.CreateGEP(i64, argv, createInt32Constant(CT, i));
        call_args.push_back(BL.CreateLoad(i64, p));
      }

      llvm::BasicBlock* cdecl_p = llvm::BasicBlock::Create(CT, "fixed_cdecl_" + std::to_string(n), f);
      llvm::BasicBlock* scheme_p = llvm::BasicBlock::Create(CT, "fixed_scheme_" + std::to_string(n), f);
      BL.CreateCondBr(is_cdecl, cdecl_p, scheme_p);

      BL.SetInsertPoint(cdecl_p);
      auto c = BL.CreateCall(fixed_ft, fp, call_args);
      c->setCallingConv(llvm::CallingConv::C);
      BL.CreateRet(c);

      BL.SetInsertPoint(scheme_p);
      auto s = BL.CreateCall(fixed_ft, fp, call_args);
      s->setCallingConv(CLOSURE_CALLING_CONV);
      BL.CreateRet(s);
    }
    BL.SetInsertPoint(def_b);
    BL.CreateRet(getScmFalseValue());
  }

  if (saved_block) BL.SetInsertPoint(saved_block);
  return f;
}

codegen_t::bridge_func_t codegen_t::call_closure_bridge() {
  if (cached_call_closure_bridge) return cached_call_closure_bridge;

  const char* name = "__nanos_call_closure_bridge";
  auto sym = jit->lookup(name);
  if (sym) {
    cached_call_closure_bridge = sym->toPtr<codegen_t::bridge_func_t>();
    return cached_call_closure_bridge;
  }

  // Compile the bridge in its own fresh context, cleanly isolated from any
  // surrounding compilation. CompileScope saves & restores context_uptr.
  {
    CompileScope scope(*this);

    auto* saved_main_func = main_function;
    auto saved_builder_ip = BL.saveIP();

    main_module_uptr = std::make_unique<llvm::Module>("bridge_module", CT);
    main_module = main_module_uptr.get();
    configure_module(*main_module);

    (void)get_or_create_call_closure_bridge();

    optimize_module(*main_module);
    // Move context into ThreadSafeContext owned by the JIT.
    // CompileScope::~CompileScope will restore the previous context.
    llvm::orc::ThreadSafeContext tsc(std::move(context_uptr));
    auto tsm = llvm::orc::ThreadSafeModule(std::move(main_module_uptr), std::move(tsc));

    if (auto err = jit->addIRModule(std::move(tsm))) {
      fatal("%s:%u codegen: failed to add bridge module to JIT: %s", __FILE__, __LINE__, llvm::toString(std::move(err)).c_str());
    }

    main_module = nullptr;
    main_function = saved_main_func;
    BL.restoreIP(saved_builder_ip);
  }  // ~CompileScope restores context_uptr and builder

  sym = jit->lookup(name);
  if (!sym) {
    fatal("%s:%u codegen: failed to look up closure bridge after compilation", __FILE__, __LINE__);
  }
  cached_call_closure_bridge = sym->toPtr<codegen_t::bridge_func_t>();
  return cached_call_closure_bridge;
}

void codegen_t::emit_apply_call(const Instruction& inst, bool is_tail) {
  if (inst.argc < 2) {
    throw std::runtime_error("error in codegen: wrong number of arguments to apply");
  }
  // Optimized apply
  // Signature: i64 c_apply_helper(i64 proc, i32 argc, i64* argv)
  llvm::Type* i64 = this->getInt64Type();
  llvm::Type* i32 = this->getInt32Type();
  llvm::Type* i64_ptr = this->getInt64PtrType();
  llvm::FunctionType* ft = llvm::FunctionType::get(i64, {i64, i32, i64_ptr}, false);
  llvm::Function* apply_helper = get_or_create_external_function("c_apply_helper", ft, (void*)&c_apply_helper);

  // Prepare arguments for c_apply_helper
  // proc in r0, arguments in r1...r(argc-1)
  llvm::Value* proc = get_reg(0);
  llvm::Value* argc_val = createInt32Constant(CT, inst.argc - 1);
  llvm::Value* argv_array = nullptr;
  if (inst.argc > 1) {
    llvm::IRBuilder<> TmpB(&current_function->getEntryBlock(), current_function->getEntryBlock().begin());
    argv_array = TmpB.CreateAlloca(i64, createInt32Constant(CT, inst.argc - 1), "apply_argv");
    for (int i = 1; i < inst.argc; i++) {
      llvm::Value* p = BL.CreateGEP(i64, argv_array, createInt32Constant(CT, i - 1));
      BL.CreateStore(get_reg(i), p);
    }
  } else {
    argv_array = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(i64_ptr));
  }

  auto call = BL.CreateCall(ft, apply_helper, {proc, argc_val, argv_array}, "apply_opt");
  if (is_tail) {
    BL.CreateRet(call);
  } else {
    set_reg(0, call);
  }
}

void codegen_t::emit_known_closure_call(const Instruction& inst, bool is_tail) {
#if ENABLE_INLINE
  if (is_symbol(inst.closure_label)) {
    scm_obj_t val = context::environment_variable_ref(inst.closure_label);
    if (is_closure(val)) {
      scm_closure_rec_t* closure_rec = (scm_closure_rec_t*)to_address(val);
      void* code_ptr = closure_rec->code;

      std::string label_name = (const char*)symbol_name(inst.closure_label);

      // inline unary primitives
      if (inst.argc == 1) {
        auto it = unary_code_map.find(code_ptr);
        if (it != unary_code_map.end()) {
          // std::cout << "inline unary primitive: " << label_name << std::endl;
          (this->*(it->second))(is_tail);
          return;
        }

        auto it_tc6 = tc6_code_map.find(code_ptr);
        if (it_tc6 != tc6_code_map.end()) {
          // std::cout << "inline tc6 primitive: " << label_name << std::endl;
          emit_tc6_predicate(it_tc6->second, is_tail);
          return;
        }
      }

      // inline binary primitives
      if (inst.argc == 2) {
        auto it = binary_code_map.find(code_ptr);
        if (it != binary_code_map.end()) {
          // std::cout << "inline binary primitive: " << label_name << std::endl;
          (this->*(it->second))(is_tail);
          return;
        }
      }
    }
  }
#endif

  // Check if it is a global closure (known at compile time but not in this module's function_map)
  if (is_symbol(inst.closure_label) && function_map.find(inst.closure_label) == function_map.end()) {
    // Attempt to resolve it as a global closure
    if (closure_params.find(inst.closure_label) != closure_params.end()) {
      auto [fixed_argc, has_rest] = closure_params[inst.closure_label];

      // Retrieve the actual closure object to get code pointer and cdecl
      scm_obj_t val = context::environment_variable_ref(inst.closure_label);
      if (is_closure(val)) {
        scm_closure_rec_t* closure_rec = (scm_closure_rec_t*)to_address(val);
        void* code_ptr = closure_rec->code;
        int cdecl = closure_rec->cdecl;

        // Construct function type
        llvm::Type* retType = this->getInt64Type();
        std::vector<llvm::Type*> paramTypes;

        // Self argument
        paramTypes.push_back(this->getInt64Type());

        if (has_rest) {
          // (self, argc, argv[])
          paramTypes.push_back(this->getInt64Type());     // argc
          paramTypes.push_back(this->getInt64PtrType());  // argv[]
        } else {
          // (self, arg0, arg1, ...)
          for (int i = 0; i < fixed_argc; i++) {
            paramTypes.push_back(this->getInt64Type());
          }
        }

        llvm::FunctionType* funcType = llvm::FunctionType::get(retType, paramTypes, false);

        // Prepare arguments
        std::vector<llvm::Value*> args;
        args.push_back(get_reg(inst.rn1));  // self

        if (has_rest) {
          if (inst.argc < fixed_argc) {
            throw std::runtime_error("error in codegen: too few arguments to apply: " + to_string(inst.closure_label));
          }
          args.push_back(createInt64Constant(CT, inst.argc));
          llvm::Value* argv_array = nullptr;
          if (inst.argc > 0) {
            llvm::IRBuilder<> TmpB(&current_function->getEntryBlock(), current_function->getEntryBlock().begin());
            argv_array = TmpB.CreateAlloca(this->getInt64Type(), createInt32Constant(CT, inst.argc), "argv");
            for (int i = 0; i < inst.argc; i++) {
              llvm::Value* arg_ptr = BL.CreateGEP(this->getInt64Type(), argv_array, createInt32Constant(CT, i));
              BL.CreateStore(get_reg(i), arg_ptr);
            }
          } else {
            argv_array = llvm::ConstantPointerNull::get(BL.getPtrTy());
          }
          args.push_back(argv_array);
        } else {
          if (inst.argc != fixed_argc) {
            throw std::runtime_error("error in codegen: wrong number of arguments to apply: " + to_string(inst.closure_label));
          }
          for (int i = 0; i < inst.argc; i++) {
            args.push_back(get_reg(i));
          }
        }

        // Create function pointer
        llvm::Value* funcPtr = createInt64Constant(CT, (uint64_t)code_ptr);
        llvm::Value* typedFuncPtr = BL.CreateIntToPtr(funcPtr, BL.getPtrTy());

        // Emit call
        // If cdecl == 0 (Scheme), use tailcc. If cdecl == 1 (C), use generic ccc.
        llvm::CallInst* call = BL.CreateCall(funcType, typedFuncPtr, args, is_tail ? "tail_call_global" : "call_global");

        if (cdecl == 0) {
          call->setCallingConv(CLOSURE_CALLING_CONV);
          if (is_tail) {
            if (!has_rest) {
              call->setTailCallKind(llvm::CallInst::TCK_MustTail);
            }
            BL.CreateRet(call);
          } else {
            set_reg(0, call);
          }
        } else {
          // cdecl == 1: C calling convention (default)
          call->setCallingConv(llvm::CallingConv::C);
          if (is_tail) {
            BL.CreateRet(call);
          } else {
            set_reg(0, call);
          }
        }
        return;
      }
    } else {
      scm_obj_t val = context::environment_variable_ref(inst.closure_label);
      if (val != scm_undef && !is_closure(val)) {
        throw std::runtime_error("error in codegen: attempt to call a non-procedure");
      }
    }
  }

  if (is_symbol(inst.closure_label) && function_map.count(inst.closure_label)) {
    llvm::Function* target_func = function_map[inst.closure_label];

    // Get closure parameters from compile-time info
    if (closure_params.find(inst.closure_label) == closure_params.end()) {
      fatal("%s:%u codegen: closure params not found for label", __FILE__, __LINE__);
    }
    auto [fixed_argc, has_rest] = closure_params[inst.closure_label];

    // Prepare arguments
    std::vector<llvm::Value*> args;

    // 1. Closure object (self)
    args.push_back(get_reg(inst.rn1));

    if (has_rest) {
      if (inst.argc < fixed_argc) {
        throw std::runtime_error("error in codegen: wrong number of arguments");
      }
      // Target expects: (self, argc, argv[])
      // We need to pack our register arguments into an array

      args.push_back(createInt64Constant(CT, inst.argc));

      llvm::Value* argv_array = nullptr;
      if (inst.argc > 0) {
        llvm::IRBuilder<> TmpB(&current_function->getEntryBlock(), current_function->getEntryBlock().begin());
        argv_array = TmpB.CreateAlloca(this->getInt64Type(), createInt32Constant(CT, inst.argc), "argv");
        for (int i = 0; i < inst.argc; i++) {
          llvm::Value* arg_ptr = BL.CreateGEP(this->getInt64Type(), argv_array, createInt32Constant(CT, i));
          BL.CreateStore(get_reg(i), arg_ptr);
        }
      } else {
        argv_array = llvm::ConstantPointerNull::get(BL.getPtrTy());
      }
      args.push_back(argv_array);

    } else {
      // Target expects: (self, arg0, arg1, ...)
      // Pass arguments directly
      // Note: target function has fixed_argc + 1 parameters (including self).
      // The call instruction has inst.argc arguments.
      if (inst.argc != fixed_argc) {
        throw std::runtime_error("error in codegen: wrong number of arguments");
      }

      for (int i = 0; i < inst.argc; i++) {
        args.push_back(get_reg(i));
      }
    }

    llvm::Function* local_target_func = this->main_module->getFunction(target_func->getName());
    if (!local_target_func) {
      local_target_func =
          llvm::Function::Create(target_func->getFunctionType(), llvm::Function::ExternalLinkage, target_func->getName(), this->main_module);
    }

    llvm::CallInst* call = BL.CreateCall(local_target_func->getFunctionType(), local_target_func, args, is_tail ? "tail_call_opt" : "call_opt");
    call->setCallingConv(CLOSURE_CALLING_CONV);

    if (is_tail) {
      if (!has_rest) {
        call->setTailCallKind(llvm::CallInst::TCK_MustTail);
      }
      BL.CreateRet(call);
    } else {
      set_reg(0, call);
    }
    return;
  }

  // Fallback if we couldn't optimize despite known label (unlikely given logic above)
  emit_generic_closure_call(inst, is_tail);
}

void codegen_t::emit_generic_rest_call(llvm::Value* closure, llvm::Value* code_void_ptr, llvm::Value* is_cdecl, const Instruction& inst,
                                       bool is_tail, llvm::BasicBlock* merge_block, llvm::BasicBlock*& rest_exit_block) {
  // --- Rest Block: (self, argc, argv[]) ---
  std::vector<llvm::Type*> paramTypes;
  paramTypes.push_back(this->getInt64Type());  // self
  paramTypes.push_back(this->getInt64Type());  // argc
  paramTypes.push_back(BL.getPtrTy());         // argv[]
  llvm::FunctionType* funcType = llvm::FunctionType::get(this->getInt64Type(), paramTypes, false);

  // Allocate argv array and populate it
  llvm::Value* argv_array = nullptr;
  if (inst.argc > 0) {
    llvm::IRBuilder<> TmpB(&current_function->getEntryBlock(), current_function->getEntryBlock().begin());
    argv_array = TmpB.CreateAlloca(this->getInt64Type(), createInt32Constant(CT, inst.argc), "argv");
    for (int i = 0; i < inst.argc; i++) {
      llvm::Value* arg_ptr = BL.CreateGEP(this->getInt64Type(), argv_array, createInt32Constant(CT, i));
      BL.CreateStore(get_reg(i), arg_ptr);
    }
  } else {
    argv_array = llvm::ConstantPointerNull::get(BL.getPtrTy());
  }

  std::vector<llvm::Value*> args;
  args.push_back(closure);
  args.push_back(createInt64Constant(CT, inst.argc));
  args.push_back(argv_array);

  llvm::Value* func_ptr = code_void_ptr;

  // Handle CDECL vs SCHEME (Tail) calling convention
  llvm::BasicBlock* cdecl_block = llvm::BasicBlock::Create(CT, "rest_cdecl", current_function);
  llvm::BasicBlock* scheme_block = llvm::BasicBlock::Create(CT, "rest_scheme", current_function);
  llvm::BasicBlock* local_merge = nullptr;

  if (!is_tail) {
    local_merge = llvm::BasicBlock::Create(CT, "rest_local_merge", current_function);
  }

  // Check cdecl flag
  BL.CreateCondBr(is_cdecl, cdecl_block, scheme_block);

  // CDECL path - no musttail
  BL.SetInsertPoint(cdecl_block);
  llvm::CallInst* call_c = BL.CreateCall(funcType, func_ptr, args, "rest_call_c");
  call_c->setCallingConv(llvm::CallingConv::C);
  if (is_tail) {
    BL.CreateRet(call_c);
  } else {
    BL.CreateBr(local_merge);
  }

  // Scheme path - musttail if tail
  BL.SetInsertPoint(scheme_block);
  llvm::CallInst* call_s = BL.CreateCall(funcType, func_ptr, args, "rest_call_s");
  call_s->setCallingConv(CLOSURE_CALLING_CONV);
  if (is_tail) {
    BL.CreateRet(call_s);
  } else {
    BL.CreateBr(local_merge);
  }

  if (!is_tail) {
    BL.SetInsertPoint(local_merge);
    llvm::PHINode* phi = BL.CreatePHI(this->getInt64Type(), 2, "rest_res");
    phi->addIncoming(call_c, cdecl_block);
    phi->addIncoming(call_s, scheme_block);

    // Explicitly transition to merge_block
    BL.CreateBr(merge_block);
  }
  rest_exit_block = BL.GetInsertBlock();
}

void codegen_t::emit_generic_normal_call(llvm::Value* closure, llvm::Value* code_void_ptr, llvm::Value* is_cdecl, const Instruction& inst,
                                         bool is_tail, llvm::BasicBlock* merge_block, llvm::BasicBlock*& normal_exit_block) {
  std::vector<llvm::Type*> normalParamTypes;
  normalParamTypes.push_back(this->getInt64Type());  // self
  for (int i = 0; i < inst.argc; i++) {
    normalParamTypes.push_back(this->getInt64Type());
  }
  llvm::FunctionType* normalFuncType = llvm::FunctionType::get(this->getInt64Type(), normalParamTypes, false);

  std::vector<llvm::Value*> normalArgs;
  normalArgs.push_back(closure);
  for (int i = 0; i < inst.argc; i++) {
    normalArgs.push_back(get_reg(i));
  }

  llvm::Value* normal_func_ptr = code_void_ptr;

  // Handle CDECL vs SCHEME (Tail) calling convention
  llvm::BasicBlock* cdecl_block = llvm::BasicBlock::Create(CT, "norm_cdecl", current_function);
  llvm::BasicBlock* scheme_block = llvm::BasicBlock::Create(CT, "norm_scheme", current_function);
  llvm::BasicBlock* local_merge = nullptr;

  if (!is_tail) {
    local_merge = llvm::BasicBlock::Create(CT, "norm_local_merge", current_function);
  }

  // Check cdecl flag
  BL.CreateCondBr(is_cdecl, cdecl_block, scheme_block);

  // CDECL path - no musttail
  BL.SetInsertPoint(cdecl_block);
  llvm::CallInst* call_c = BL.CreateCall(normalFuncType, normal_func_ptr, normalArgs, "norm_call_c");
  call_c->setCallingConv(llvm::CallingConv::C);
  if (is_tail) {
    BL.CreateRet(call_c);
  } else {
    BL.CreateBr(local_merge);
  }

  // Scheme path - musttail if tail
  BL.SetInsertPoint(scheme_block);
  llvm::CallInst* call_s = BL.CreateCall(normalFuncType, normal_func_ptr, normalArgs, "norm_call_s");
  call_s->setCallingConv(CLOSURE_CALLING_CONV);
  if (is_tail) {
    call_s->setTailCallKind(llvm::CallInst::TCK_MustTail);
    BL.CreateRet(call_s);
  } else {
    BL.CreateBr(local_merge);
  }

  if (!is_tail) {
    BL.SetInsertPoint(local_merge);
    llvm::PHINode* phi = BL.CreatePHI(this->getInt64Type(), 2, "norm_res");
    phi->addIncoming(call_c, cdecl_block);
    phi->addIncoming(call_s, scheme_block);
    BL.CreateBr(merge_block);
  }
  normal_exit_block = BL.GetInsertBlock();
}

void codegen_t::emit_generic_closure_call(const Instruction& inst, bool is_tail) {
  llvm::Type* i64 = this->getInt64Type();
  llvm::Type* i32 = this->getInt32Type();
  llvm::Type* i8_ptr = BL.getPtrTy();
  llvm::FunctionType* test_ft = llvm::FunctionType::get(BL.getVoidTy(), {i64, i32, i8_ptr}, false);
  llvm::Function* test_func = get_or_create_external_function("c_test_application", test_ft, (void*)&c_test_application);
  llvm::Value* proc_name_val = BL.CreateGlobalString(to_string(inst.closure_label), "proc_name");
  BL.CreateCall(test_ft, test_func, {get_reg(inst.rn1), createInt32Constant(CT, inst.argc), proc_name_val});

  if (inst.argc <= BRIDGE_MAX_ARGS) {
    // Optimized generic call via bridge
    llvm::Function* bridge = get_or_create_call_closure_bridge();
    llvm::Type* i64 = this->getInt64Type();
    llvm::Type* i64_ptr = this->getInt64PtrType();

    // Prepare arguments for bridge: i64 (i64 closure, i64 argc, i64* argv)
    llvm::Value* closure = get_reg(inst.rn1);
    llvm::Value* argc_val = createInt64Constant(CT, inst.argc);
    llvm::Value* argv_array = nullptr;
    if (inst.argc > 0) {
      llvm::IRBuilder<> TmpB(&current_function->getEntryBlock(), current_function->getEntryBlock().begin());
      argv_array = TmpB.CreateAlloca(i64, createInt32Constant(CT, inst.argc), "bridge_argv");
      for (int i = 0; i < inst.argc; i++) {
        llvm::Value* p = BL.CreateGEP(i64, argv_array, createInt32Constant(CT, i));
        BL.CreateStore(get_reg(i), p);
      }
    } else {
      argv_array = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(i64_ptr));
    }

    auto call = BL.CreateCall(bridge->getFunctionType(), bridge, {closure, argc_val, argv_array}, "bridge_call");
    // The bridge itself handles the calling convention (Scheme/CDECL) and musttail optimization internally.
    // However, the call TO the bridge is a standard C call.
    if (is_tail) {
      BL.CreateRet(call);
    } else {
      set_reg(0, call);
    }
    return;
  }

  // Fallback for large argument counts: Get closure object from register
  llvm::Value* closure = get_reg(inst.rn1);

  // Get code pointer from closure struct
  llvm::Value* code_void_ptr = getClosureCodePtr(closure);

  // Read closure's rest field to determine calling convention
  llvm::Value* closure_ptr = untagPointer(BL, CT, closure);
  llvm::Value* rest_field_ptr = BL.CreateConstInBoundsGEP1_32(BL.getInt8Ty(), closure_ptr, CLOSURE_REST_FIELD_OFFSET);
  llvm::Value* rest_field_ptr_i32 = rest_field_ptr;
  llvm::Value* rest_flag = BL.CreateLoad(BL.getInt32Ty(), rest_field_ptr_i32, "rest");

  // Load cdecl field (already defined at top of file)
  llvm::Value* cdecl_field_ptr = BL.CreateConstInBoundsGEP1_32(BL.getInt8Ty(), closure_ptr, CLOSURE_CDECL_FIELD_OFFSET);
  llvm::Value* cdecl_field_ptr_i32 = cdecl_field_ptr;
  llvm::Value* cdecl_flag = BL.CreateLoad(BL.getInt32Ty(), cdecl_field_ptr_i32, "cdecl");

  // Branch based on rest flag
  llvm::Value* is_rest = BL.CreateICmpNE(rest_flag, BL.getInt32(0), "is_rest");
  llvm::Value* is_cdecl = BL.CreateICmpNE(cdecl_flag, BL.getInt32(0), "is_cdecl");

  llvm::BasicBlock* rest_block = llvm::BasicBlock::Create(CT, is_tail ? "rest_tail_call" : "rest_call", current_function);
  llvm::BasicBlock* normal_block = llvm::BasicBlock::Create(CT, is_tail ? "normal_tail_call" : "normal_call", current_function);
  llvm::BasicBlock* merge_block = nullptr;
  if (!is_tail) {
    merge_block = llvm::BasicBlock::Create(CT, "call_merge", current_function);
  }

  llvm::MDBuilder MDB((CT));
  auto branch_weights = MDB.createBranchWeights(1, 2000);  // 1 = unlikely (rest), 2000 = likely (normal)
  BL.CreateCondBr(is_rest, rest_block, normal_block, branch_weights);

  llvm::BasicBlock* rest_exit_block = nullptr;
  llvm::BasicBlock* normal_exit_block = nullptr;

  // --- Rest Block ---
  BL.SetInsertPoint(rest_block);
  emit_generic_rest_call(closure, code_void_ptr, is_cdecl, inst, is_tail, merge_block, rest_exit_block);

  // --- Normal Block ---
  BL.SetInsertPoint(normal_block);
  emit_generic_normal_call(closure, code_void_ptr, is_cdecl, inst, is_tail, merge_block, normal_exit_block);

  // --- Merge Block ---
  if (!is_tail) {
    BL.SetInsertPoint(merge_block);
    llvm::PHINode* phi = BL.CreatePHI(this->getInt64Type(), 2, "call_result");

    // Retrieve the result from the exit blocks by extracting the PHI node
    // from the first instruction of each local merge block.

    if (rest_exit_block->empty()) {
      fatal("rest_exit_block empty");
    }
    llvm::PHINode* rest_res = llvm::dyn_cast<llvm::PHINode>(&rest_exit_block->front());
    if (!rest_res) fatal("Expected PHI in rest_exit_block");

    if (normal_exit_block->empty()) {
      fatal("normal_exit_block empty");
    }
    llvm::PHINode* normal_res = llvm::dyn_cast<llvm::PHINode>(&normal_exit_block->front());
    if (!normal_res) fatal("Expected PHI in normal_exit_block");

    phi->addIncoming(rest_res, rest_exit_block);
    phi->addIncoming(normal_res, normal_exit_block);

    // Store result in r0
    set_reg(0, phi);
  }
}

// ============================================================================
// Instruction Emission: Calls & Ref/Set
// ============================================================================

void codegen_t::emit_call_common(const Instruction& inst, bool is_tail) {
  if (inst.closure_label == cached_symbol_apply) {
    emit_apply_call(inst, is_tail);
    return;
  }

  // Check if it's a known closure (global or local) call optimization
  if ((is_symbol(inst.closure_label) && function_map.find(inst.closure_label) == function_map.end()) ||
      (is_symbol(inst.closure_label) && function_map.count(inst.closure_label))) {
    emit_known_closure_call(inst, is_tail);
    return;
  }

  emit_generic_closure_call(inst, is_tail);
}

// Call a closure with arguments from registers
void codegen_t::emit_call(const Instruction& inst) { emit_call_common(inst, false); }

// Tail call a closure with arguments from registers
void codegen_t::emit_tail_call(const Instruction& inst) { emit_call_common(inst, true); }

// Load free variable from closure environment
void codegen_t::emit_closure_ref(const Instruction& inst) {
  if (!current_closure_self) {
    fatal("%s:%u codegen: closure-ref used outside of closure (CT)", __FILE__, __LINE__);
  }

  int env_idx = (int)fixnum(inst.opr2);
  if (env_idx < 0) {
    fatal("%s:%u codegen: closure-ref invalid index: %d", __FILE__, __LINE__, env_idx);
  }

  // Get pointer to closure environment array
  llvm::Value* env_array_ptr = getClosureEnvArrayPtr(BL, CT, current_closure_self, this->getInt64Type());

  // Get pointer to specific index
  llvm::Value* val_ptr = BL.CreateGEP(this->getInt64Type(), env_array_ptr, createInt32Constant(CT, env_idx));

  // Load the value
  llvm::Value* val = BL.CreateLoad(this->getInt64Type(), val_ptr, "free_var");

  set_reg(inst.rn1, val);
}

// Store value into closure environment
void codegen_t::emit_closure_set(const Instruction& inst) {
  if (!current_closure_self) {
    fatal("%s:%u codegen: closure-set! used outside of closure (CT)", __FILE__, __LINE__);
  }

  int env_idx = (int)fixnum(inst.opr1);
  if (env_idx < 0) {
    fatal("%s:%u codegen: closure-set! invalid index: %d", __FILE__, __LINE__, env_idx);
  }

  // Get pointer to closure environment array
  llvm::Value* env_array_ptr = getClosureEnvArrayPtr(BL, CT, current_closure_self, this->getInt64Type());

  // Get pointer to specific index
  llvm::Value* val_ptr = BL.CreateGEP(this->getInt64Type(), env_array_ptr, createInt32Constant(CT, env_idx));

  // Get value from source register
  if (inst.rn2 < 0) {
    fatal("%s:%u codegen: closure-set! missing source register", __FILE__, __LINE__);
  }
  llvm::Value* val = get_reg(inst.rn2);

  // Store the value
  BL.CreateStore(val, val_ptr);

  // Write barrier
  emitWriteBarrier(val);
}

// Load current closure object into register
void codegen_t::emit_closure_self(const Instruction& inst) {
  if (!current_closure_self) {
    fatal("%s:%u codegen: closure-self used outside of closure (CT)", __FILE__, __LINE__);
  }
  set_reg(inst.rn1, current_closure_self);
}

// Load free variable (cell) from closure environment and unbox it
void codegen_t::emit_closure_cell_ref(const Instruction& inst) {
  if (!current_closure_self) {
    fatal("%s:%u codegen: closure-cell-ref used outside of closure (CT)", __FILE__, __LINE__);
  }

  int env_idx = (int)fixnum(inst.opr2);
  if (env_idx < 0) {
    fatal("%s:%u codegen: closure-cell-ref invalid index: %d", __FILE__, __LINE__, env_idx);
  }

  // Get pointer to closure environment array
  llvm::Value* env_array_ptr = getClosureEnvArrayPtr(BL, CT, current_closure_self, this->getInt64Type());
  llvm::Value* val_ptr = BL.CreateGEP(this->getInt64Type(), env_array_ptr, createInt32Constant(CT, env_idx));

  // Load the cell object (which is a tagged pointer)
  llvm::Value* cell_obj = BL.CreateLoad(this->getInt64Type(), val_ptr, "cell_obj");

  // Get pointer to cell's value and load it
  llvm::Value* value_ptr_typed = getCellValuePtr(BL, CT, cell_obj, this->getInt64Type());
  llvm::Value* val = BL.CreateLoad(this->getInt64Type(), value_ptr_typed, "cell_val");

  set_reg(inst.rn1, val);
}

// Load free variable (cell) from closure environment and update its value
void codegen_t::emit_closure_cell_set(const Instruction& inst) {
  if (!current_closure_self) {
    fatal("%s:%u codegen: closure-cell-set! used outside of closure (CT)", __FILE__, __LINE__);
  }

  int env_idx = (int)fixnum(inst.opr1);
  if (env_idx < 0) {
    fatal("%s:%u codegen: closure-cell-set! invalid index: %d", __FILE__, __LINE__, env_idx);
  }

  // Get pointer to closure environment array
  llvm::Value* env_array_ptr = getClosureEnvArrayPtr(BL, CT, current_closure_self, this->getInt64Type());
  llvm::Value* val_ptr = BL.CreateGEP(this->getInt64Type(), env_array_ptr, createInt32Constant(CT, env_idx));

  // Load the cell object
  llvm::Value* cell_obj = BL.CreateLoad(this->getInt64Type(), val_ptr, "cell_obj");

  // Get pointer to cell's value
  llvm::Value* value_ptr_typed = getCellValuePtr(BL, CT, cell_obj, this->getInt64Type());

  // Get value from source register
  if (inst.rn2 < 0) {
    fatal("%s:%u codegen: closure-cell-set! missing source register", __FILE__, __LINE__);
  }
  llvm::Value* val = get_reg(inst.rn2);

  // Store the value
  BL.CreateStore(val, value_ptr_typed);

  // Write barrier on value being stored
  emitWriteBarrier(val);
}

// Unbox cell in source register to destination register
void codegen_t::emit_reg_cell_ref(const Instruction& inst) {
  // Get cell object from source register
  if (inst.rn2 < 0) {
    fatal("%s:%u codegen: reg-cell-ref missing source register", __FILE__, __LINE__);
  }
  llvm::Value* cell_obj = get_reg(inst.rn2);

  // Get pointer to cell's value and load it
  llvm::Value* value_ptr_typed = getCellValuePtr(BL, CT, cell_obj, this->getInt64Type());
  llvm::Value* val = BL.CreateLoad(this->getInt64Type(), value_ptr_typed, "cell_val");

  set_reg(inst.rn1, val);
}

// Update value of cell in destination register with value from source register
void codegen_t::emit_reg_cell_set(const Instruction& inst) {
  // Get cell object from destination register
  if (inst.rn1 < 0) {
    fatal("%s:%u codegen: reg-cell-set! missing destination register", __FILE__, __LINE__);
  }
  llvm::Value* cell_obj = get_reg(inst.rn1);

  // Get pointer to cell's value
  llvm::Value* value_ptr_typed = getCellValuePtr(BL, CT, cell_obj, this->getInt64Type());

  // Get value from source register
  if (inst.rn2 < 0) {
    fatal("%s:%u codegen: reg-cell-set! missing source register", __FILE__, __LINE__);
  }
  llvm::Value* val = get_reg(inst.rn2);

  // Store value
  BL.CreateStore(val, value_ptr_typed);

  // Write barrier on value being stored
  emitWriteBarrier(val);
}

// Create a cell from register value and store in the same register
void codegen_t::emit_make_cell(const Instruction& inst) {
  // Get or create c_make_cell external function
  llvm::Type* intptrTy = this->getInt64Type();
  std::vector<llvm::Type*> argTypes = {intptrTy};
  llvm::FunctionType* ft = llvm::FunctionType::get(intptrTy, argTypes, false);
  llvm::Function* make_cell_func = get_or_create_external_function("c_make_cell", ft, (void*)&c_make_cell);

  // Get value from register
  if (inst.rn1 < 0) {
    fatal("%s:%u codegen: make-cell missing register operand", __FILE__, __LINE__);
  }
  llvm::Value* val = get_reg(inst.rn1);

  // Call c_make_cell
  llvm::Value* cell = BL.CreateCall(ft, make_cell_func, {val}, "cell");

  // Store result back to register
  set_reg(inst.rn1, cell);
}
