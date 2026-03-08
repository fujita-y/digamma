// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "codegen.h"
#include "codegen_aux.h"
#include "codegen_common.h"
#include "object_heap.h"
#include "printer.h"

#include <cstddef>
#include <fstream>

#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/ExecutionEngine/Orc/AbsoluteSymbols.h>
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
#include <random>
#include "nanos_jit.h"

#define BL (*builder)
#define CT (*context_uptr)

// ============================================================================
//  compiled_code_t — RAII wrapper for JIT-compiled code
// ============================================================================

compiled_code_t::~compiled_code_t() {
  if (tracker) {
    if (auto err = tracker->remove()) {
      llvm::consumeError(std::move(err));
    }
  }
}

compiled_code_t::compiled_code_t(compiled_code_t&& other) {
  func_ptr = other.func_ptr;
  tracker = std::move(other.tracker);
  other.func_ptr = nullptr;
}

compiled_code_t& compiled_code_t::operator=(compiled_code_t&& other) {
  func_ptr = other.func_ptr;
  tracker = std::move(other.tracker);
  other.func_ptr = nullptr;
  return *this;
}

// ============================================================================
//  Static / thread-local state
// ============================================================================

thread_local codegen_t* codegen_t::s_current;

// ============================================================================
//  Constructor
// ============================================================================

codegen_t::codegen_t(std::unique_ptr<llvm::LLVMContext> ctx, nanos_jit_t* jit) : context_uptr(std::move(ctx)), jit(jit) {
  builder = std::make_unique<llvm::IRBuilder<>>(CT);
  cached_symbol_label = make_symbol("label");
  cached_symbol_apply = make_symbol("apply");
  cached_symbol_safepoint = make_symbol("safepoint");
  object_heap_t::current()->add_root(cached_symbol_label);
  object_heap_t::current()->add_root(cached_symbol_apply);
  object_heap_t::current()->add_root(cached_symbol_safepoint);
  init_opcode_map();
  s_current = this;
}

// ============================================================================
//  Compilation pipeline (phases)
// ============================================================================

// --------------------------------------------------------------------------
//  CompileScope RAII
// --------------------------------------------------------------------------

codegen_t::CompileScope::CompileScope(codegen_t& self) : self(self), saved_ctx(std::move(self.context_uptr)) {
  self.context_uptr = std::make_unique<llvm::LLVMContext>();
  self.builder = std::make_unique<llvm::IRBuilder<>>(*self.context_uptr);
}

codegen_t::CompileScope::~CompileScope() {
  self.builder.reset();
  self.context_uptr = std::move(saved_ctx);
  if (self.context_uptr) self.builder = std::make_unique<llvm::IRBuilder<>>(*self.context_uptr);
}

// --------------------------------------------------------------------------
//  configure_module helper
// --------------------------------------------------------------------------

void codegen_t::configure_module(llvm::Module& M) {
  M.setDataLayout(jit->getDataLayout());
  M.setTargetTriple(jit->getTargetTriple());
  M.setPICLevel(llvm::PICLevel::BigPIC);
  M.setPIELevel(llvm::PIELevel::Large);
}

std::string codegen_t::generate_unique_suffix() {
  static int counter = 0;
  static std::mt19937 gen(std::random_device{}());
  static std::uniform_int_distribution<uint32_t> distrib(0, UINT32_MAX);
  uint32_t val = distrib(gen);
  counter++;
  std::stringstream ss;
  ss << std::hex << std::setw(8) << std::setfill('0') << val << "_" << counter << "_" << std::this_thread::get_id();
  return ss.str();
}

// --------------------------------------------------------------------------
//  compile() — top-level entry point
// --------------------------------------------------------------------------

compiled_code_t codegen_t::compile(scm_obj_t inst_list) {
  CompileScope scope(*this);
  try {
    phase0_create_module();
    phase1_parse_instructions(inst_list);
    analyze_closure_labels();
#ifndef NDEBUG
    {
      std::ofstream ofs("/tmp/nanos.ins", std::ios::trunc);
    }
    for (const auto& func : functions) {
      dump_instructions(func.instructions);
    }
#endif
    phase2_create_functions();
    phase3_generate_code();
    phase4_optimize_and_verify();
    return phase5_finalize();
  } catch (...) {
    throw;
  }
}

// --------------------------------------------------------------------------
//  Phase 0: Module creation
// --------------------------------------------------------------------------

void codegen_t::phase0_create_module() {
  if (!closure_bridge_initialized) {
    // Initialize bridge (this creates a standalone module, compiles the body, and gives it to JIT)
    (void)get_call_closure_bridge_ptr();
    closure_bridge_initialized = true;
  }

  std::string mod_name = "jit_module_main_" + generate_unique_suffix();
  this->main_module_uptr = std::make_unique<llvm::Module>(mod_name, CT);
  this->main_module = this->main_module_uptr.get();
  configure_module(*this->main_module);

  std::string clo_mod_name = "jit_module_closures_" + generate_unique_suffix();
  this->closure_module_uptr = std::make_unique<llvm::Module>(clo_mod_name, CT);
  this->closure_module = this->closure_module_uptr.get();
  configure_module(*this->closure_module);
}

// --------------------------------------------------------------------------
//  Phase 1: Instruction parsing
// --------------------------------------------------------------------------

void codegen_t::phase1_parse_instructions(scm_obj_t inst_list) {
  parse_instructions(inst_list);

  labels.clear();
  function_map.clear();
}

// --------------------------------------------------------------------------
//  Phase 2: Function and BasicBlock creation
// --------------------------------------------------------------------------

void codegen_t::phase2_create_functions() {
  // Create the main function and all closure functions
  // The first function in 'functions' is always the main entry point.
  if (functions.empty()) return;

  // 1. Create Main Function
  {
    FunctionInfo& main_info = functions[0];
    llvm::FunctionType* funcType = llvm::FunctionType::get(this->getInt64Type(), false);
    std::string func_name = "scm_fn_" + generate_unique_suffix();
    this->main_function = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, func_name, main_module);
    this->main_function->setDSOLocal(true);
    if (!this->main_function) {
      fatal("%s:%u codegen: failed to create main function", __FILE__, __LINE__);
    }
    main_info.llvm_function = this->main_function;

    // Create generic entry block needed for allocas
    llvm::BasicBlock::Create(CT, "entry", this->main_function);
  }

  // 2. Create Closure Functions
  for (size_t i = 1; i < functions.size(); ++i) {
    FunctionInfo& info = functions[i];
    if (info.label == scm_nil) continue;

    std::string func_name = std::string((const char*)symbol_name(info.label)) + "_" + generate_unique_suffix();

    std::vector<llvm::Type*> paramTypes;
    paramTypes.push_back(this->getInt64Type());  // self

    if (info.has_rest) {
      // (self, argc, argv[])
      paramTypes.push_back(this->getInt64Type());     // argc
      paramTypes.push_back(this->getInt64PtrType());  // argv[]
    } else {
      // (self, arg0, arg1, ...)
      for (int k = 0; k < info.argc; k++) {
        paramTypes.push_back(this->getInt64Type());
      }
    }

    llvm::FunctionType* closureFuncType = llvm::FunctionType::get(this->getInt64Type(), paramTypes, false);
    llvm::Function* closure_func = llvm::Function::Create(closureFuncType, llvm::Function::ExternalLinkage, func_name, closure_module);
    add_common_closure_attributes(closure_func);

    info.llvm_function = closure_func;
    function_map[info.label] = closure_func;

    llvm::BasicBlock::Create(CT, "entry", closure_func);
  }

  // 3. Create BasicBlocks for all labels within each function
  for (auto& info : functions) {
    llvm::Function* llvm_func = info.llvm_function;
    for (const auto& inst : info.instructions) {
      if (inst.op == Opcode::LABEL) {
        // Allocate a new BasicBlock for each label.
        // Closure entry labels are also valid jump targets.
        const char* label_str = (const char*)symbol_name(inst.opr1);
        labels[inst.opr1] = llvm::BasicBlock::Create(CT, label_str, llvm_func);
      }
    }
  }
}

// --------------------------------------------------------------------------
//  Phase 3: Code generation
// --------------------------------------------------------------------------

void codegen_t::phase3_generate_code() {
  for (auto& info : functions) {
    current_function = info.llvm_function;
    current_function_info = &info;
    this->main_module = current_function->getParent();  // set current module

    // Set insert point to entry block
    llvm::BasicBlock& entry = current_function->getEntryBlock();
    BL.SetInsertPoint(&entry);

    // Create allocas for this function's register usage
    // max_reg is 0-indexed, so count is max_reg + 1. If max_reg is -1, count is 0.
    int reg_count = info.max_reg + 1;
    create_allocas(current_function, reg_count);

    // Function specific setup
    if (info.label != scm_nil) {
      // This is a closure
      current_closure_self = current_function->getArg(0);

      // Setup parameters
      if (info.has_rest) {
        // For rest closures: (self, argc, argv[])
        auto arg_it = current_function->arg_begin();
        arg_it++;                               // Skip self
        llvm::Value* actual_argc = &*arg_it++;  // Get argc
        llvm::Value* argv_ptr = &*arg_it++;     // Get argv pointer

        // Load fixed arguments from argv[] into registers
        for (int i = 0; i < info.argc; i++) {
          llvm::Value* arg_ptr = BL.CreateGEP(this->getInt64Type(), argv_ptr, createInt64Constant(CT, i));
          llvm::Value* arg_val = BL.CreateLoad(this->getInt64Type(), arg_ptr);
          set_reg(i, arg_val);
        }

        // Handle rest arguments
        setup_closure_rest_arguments(info.argc, actual_argc, argv_ptr);
      } else {
        // For non-rest closures: (self, arg0, arg1, ...)
        // Copy fixed arguments from function parameters to registers
        auto arg_it = current_function->arg_begin();
        arg_it++;  // Skip self

        for (int i = 0; i < info.argc; i++) {
          llvm::Value* arg_val = &*arg_it++;
          set_reg(i, arg_val);
        }
      }
    } else {
      // Main function
      current_closure_self = nullptr;
    }

    // Emit instructions
    for (const auto& inst : info.instructions) {
      // Emit instructions sequentially.
      // emit_label will naturally create a branch from Entry to the first label if needed.
      emit_inst(inst);
    }
  }
}

// --------------------------------------------------------------------------
//  Phase 4: Verification and optimization
// --------------------------------------------------------------------------

void codegen_t::phase4_optimize_and_verify() {
  auto optimize_module = [](llvm::Module& mod) {
    llvm::LoopAnalysisManager LAM;
    llvm::FunctionAnalysisManager FAM;
    llvm::CGSCCAnalysisManager CGAM;
    llvm::ModuleAnalysisManager MAM;

    llvm::PassBuilder PB;
    PB.registerModuleAnalyses(MAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerLoopAnalyses(LAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

    llvm::ModulePassManager MPM = PB.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O2);
    MPM.run(mod, MAM);
  };

  // Verify all functions first
  for (auto const& [label, func] : function_map) {
    if (llvm::verifyFunction(*func, &llvm::errs())) {
      fatal("%s:%u codegen: LLVM function verification failed for: %s", __FILE__, __LINE__, func->getName().str().c_str());
    }
  }
  if (llvm::verifyFunction(*main_function, &llvm::errs())) {
    fatal("%s:%u codegen: LLVM function verification failed for: %s", __FILE__, __LINE__, main_function->getName().str().c_str());
  }

  // Run optimization passes
  optimize_module(*main_module_uptr);
  if (functions.size() > 1) {
    optimize_module(*closure_module_uptr);
  }

  // Dump IR to file for debugging and inspection
#ifndef NDEBUG
  std::error_code EC;
  llvm::raw_fd_ostream dest("/tmp/nanos_main.ll", EC, llvm::sys::fs::OF_None);
  if (EC) {
    llvm::errs() << "Could not open file: " << EC.message() << "\n";
  } else {
    main_module_uptr->print(dest, nullptr);
  }

  if (functions.size() > 1) {
    prune_unused_closures();
    llvm::raw_fd_ostream dest2("/tmp/nanos_closures.ll", EC, llvm::sys::fs::OF_None);
    if (EC) {
      llvm::errs() << "Could not open file: " << EC.message() << "\n";
    } else {
      closure_module_uptr->print(dest2, nullptr);
    }
  }
#endif
}

// Helper function to find which llvm::Function an llvm::User belongs to,
// by looking through Instruction and ConstantExpr.
static llvm::Function* getUserFunction(llvm::User* U) {
  if (auto* I = llvm::dyn_cast<llvm::Instruction>(U)) {
    return I->getFunction();
  }
  if (auto* CE = llvm::dyn_cast<llvm::ConstantExpr>(U)) {
    for (llvm::User* CEU : CE->users()) {
      if (llvm::Function* F = getUserFunction(CEU)) {
        return F;
      }
    }
  }
  return nullptr;
}

void codegen_t::prune_unused_closures() {
  std::set<llvm::Function*> reachable;
  std::vector<llvm::Function*> worklist;

  llvm::Module* main_mod = main_module_uptr.get();
  llvm::Module* clo_mod = closure_module_uptr.get();

  // 1. Establish the root set from main_module.
  // Any function declaration in main_module that has uses is a root.
  for (llvm::Function& f : *main_mod) {
    if (f.isDeclaration() && !f.use_empty()) {
      if (llvm::Function* target = clo_mod->getFunction(f.getName())) {
        if (reachable.insert(target).second) {
          worklist.push_back(target);
        }
      }
    }
  }

  // 2. Build edges: for each function in closure_module, find which functions use it.
  // edge: user_func -> target_func
  std::map<llvm::Function*, std::vector<llvm::Function*>> cg;
  for (llvm::Function& target_func : *clo_mod) {
    if (target_func.isDeclaration()) continue;

    for (llvm::User* U : target_func.users()) {
      if (llvm::Function* user_func = getUserFunction(U)) {
        // If the user is inside closure_module, record the edge.
        if (user_func->getParent() == clo_mod) {
          cg[user_func].push_back(&target_func);
        }
      }
    }
  }

  // 3. Trace reachable functions in closure_module
  while (!worklist.empty()) {
    llvm::Function* curr = worklist.back();
    worklist.pop_back();

    for (llvm::Function* target : cg[curr]) {
      if (reachable.insert(target).second) {
        worklist.push_back(target);
      }
    }
  }

  // 4. Prune unreachable functions
  std::vector<llvm::Function*> to_delete;
  for (llvm::Function& f : *clo_mod) {
    if (!f.isDeclaration() && reachable.find(&f) == reachable.end()) {
      to_delete.push_back(&f);
    }
  }

  for (llvm::Function* f : to_delete) {
#ifndef NDEBUG
    std::cout << "[codegen_t::prune_unused_closures] deleting: " << f->getName().str() << "\n";
#endif
    f->eraseFromParent();
  }
}

// --------------------------------------------------------------------------
//  Phase 5: Finalize and hand off to JIT
// --------------------------------------------------------------------------

compiled_code_t codegen_t::phase5_finalize() {
  // Transfer modules to LLJIT
  std::string main_func_name = main_function->getName().str();

  // Build a ThreadSafeContext from our owned LLVMContext and hand it off to
  // the JIT along with the modules. After this point context_uptr is empty;
  // CompileScope::~CompileScope will restore the previous context.
  llvm::orc::ThreadSafeContext tsc(std::move(context_uptr));

  if (functions.size() > 1) {
    auto clo_tsm = llvm::orc::ThreadSafeModule(std::move(closure_module_uptr), tsc);

    if (auto err = jit->addIRModule(std::move(clo_tsm))) {
      fatal("%s:%u codegen: failed to add closure module to JIT: %s", __FILE__, __LINE__, llvm::toString(std::move(err)).c_str());
    }
  }

  // Create explicit tracker for main module
  auto rt = jit->getMainJITDylib().createResourceTracker();
  auto main_tsm = llvm::orc::ThreadSafeModule(std::move(main_module_uptr), tsc);

  if (auto err = jit->addIRModule(std::move(main_tsm), rt)) {
    fatal("%s:%u codegen: failed to add main module to JIT: %s", __FILE__, __LINE__, llvm::toString(std::move(err)).c_str());
  }

  // Look up the compiled function
  auto sym = jit->lookup(main_func_name);
  if (!sym) {
    fatal("%s:%u codegen: failed to look up compiled function: %s", __FILE__, __LINE__, llvm::toString(sym.takeError()).c_str());
  }

  // Clear local module pointers
  this->main_module = nullptr;
  this->closure_module = nullptr;
  this->main_function = nullptr;
  this->closure_module_uptr.reset();
  this->main_module_uptr.reset();

  return compiled_code_t(sym->toPtr<intptr_t()>(), std::move(rt));
}

// ============================================================================
//  Instruction parsing
// ============================================================================

void codegen_t::init_opcode_map() {
  opcode_map[make_symbol("const")] = Opcode::CONST;
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
    heap->add_root(pair.first);
  }
}

void codegen_t::parse_instructions(scm_obj_t inst_list) {
  functions.clear();
  closure_literals.clear();
  closure_params.clear();

  // Create main function info
  functions.emplace_back();
  FunctionInfo* current_func = &functions.back();
  current_func->label = scm_nil;

  scm_obj_t curr = inst_list;
  scm_obj_t current_closure_label = scm_nil;
  std::vector<scm_obj_t> current_literals;

  // Detect closure parameters to switch contexts.
  // Closures always start with a unique label which starts with 'C'.
  while (curr != scm_nil) {
    if (!is_cons(curr)) break;
    scm_obj_t inst_obj = CAR(curr);
    curr = CDR(curr);

    // Check for LABEL opcode to detect function switch
    if (is_cons(inst_obj) && CAR(inst_obj) == cached_symbol_label) {
      scm_obj_t label = operand(inst_obj, 1);
      if (is_closure_label(label)) {
        // Finish previous closure literals
        finish_closure_literals(current_closure_label, current_literals);

        // Start new function
        functions.emplace_back();
        current_func = &functions.back();
        current_func->label = label;
        current_closure_label = label;

        // label instruction
        parse_single_instruction(inst_obj, *current_func, current_closure_label, current_literals);

        // Insert safepoint before the first instruction of the closure
        Instruction safepoint_inst;
        safepoint_inst.op = Opcode::SAFEPOINT;
        safepoint_inst.original = make_cons(cached_symbol_safepoint, scm_nil);
        current_func->instructions.push_back(safepoint_inst);

        continue;
      }
    }

    parse_single_instruction(inst_obj, *current_func, current_closure_label, current_literals);
  }

  // Finish any remaining closure literals
  finish_closure_literals(current_closure_label, current_literals);

  // Post-process: Update max_reg for each function based on parameters
  // Main function (index 0) doesn't have parameters in registers initially (or rather, it's just entry)
  // Closures have parameters.
  for (auto& func : functions) {
    if (func.label != scm_nil && closure_params.count(func.label)) {
      auto params = closure_params[func.label];
      func.argc = params.first;
      func.has_rest = params.second;
      int argc = func.argc + (func.has_rest ? 1 : 0);
      if (argc > 0) {
        int needed_max = argc - 1;
        if (needed_max > func.max_reg) {
          func.max_reg = needed_max;
        }
      }
    }
  }
}

void codegen_t::parse_single_instruction(scm_obj_t inst_obj, FunctionInfo& func_info, scm_obj_t& current_closure_label,
                                         std::vector<scm_obj_t>& current_literals) {
  if (!is_cons(inst_obj)) return;

  Instruction inst;
  inst.original = inst_obj;
  inst.op = Opcode::UNKNOWN;

  scm_obj_t op_sym = CAR(inst_obj);

  // Look up opcode
  auto it = opcode_map.find(op_sym);
  if (it != opcode_map.end()) {
    inst.op = it->second;
  } else {
    // Unknown opcode or not in map
    assert(is_symbol(op_sym));
    fatal("%s:%u codegen: unknown opcode '%s'", __FILE__, __LINE__, symbol_name(op_sym));
    return;
  }

  switch (inst.op) {
    case Opcode::CONST:
      parse_const(inst_obj, inst, func_info, current_closure_label, current_literals);
      break;
    case Opcode::MOV:
      parse_mov(inst_obj, inst, func_info);
      break;
    case Opcode::IF:
      parse_if(inst_obj, inst, func_info);
      break;
    case Opcode::JUMP:
      parse_jump(inst_obj, inst);
      break;
    case Opcode::LABEL:
      parse_label(inst_obj, inst, current_closure_label, current_literals);
      break;
    case Opcode::RET:
      parse_ret(inst_obj, inst, func_info);
      break;
    case Opcode::MAKE_CLOSURE:
      parse_make_closure(inst_obj, inst, func_info);
      break;
    case Opcode::GLOBAL_SET:
      parse_global_set(inst_obj, inst, func_info);
      break;
    case Opcode::GLOBAL_REF:
      parse_global_ref(inst_obj, inst, func_info);
      break;
    case Opcode::CALL:
      parse_call(inst_obj, inst, func_info);
      break;
    case Opcode::TAIL_CALL:
      parse_tail_call(inst_obj, inst, func_info);
      break;
    case Opcode::CLOSURE_REF:
      parse_closure_ref(inst_obj, inst, func_info);
      break;
    case Opcode::CLOSURE_SET:
      parse_closure_set(inst_obj, inst, func_info);
      break;
    case Opcode::CLOSURE_SELF:
      parse_closure_self(inst_obj, inst, func_info);
      break;
    case Opcode::CLOSURE_CELL_REF:
      parse_closure_cell_ref(inst_obj, inst, func_info);
      break;
    case Opcode::CLOSURE_CELL_SET:
      parse_closure_cell_set(inst_obj, inst, func_info);
      break;
    case Opcode::REG_CELL_REF:
      parse_reg_cell_ref(inst_obj, inst, func_info);
      break;
    case Opcode::REG_CELL_SET:
      parse_reg_cell_set(inst_obj, inst, func_info);
      break;
    case Opcode::MAKE_CELL:
      parse_make_cell(inst_obj, inst, func_info);
      break;
    case Opcode::SAFEPOINT:
      // No operands for safepoint
      break;
    default:
      fatal("%s:%u codegen: unknown opcode %ld", __FILE__, __LINE__, (long)inst.op);
      break;
  }

  func_info.instructions.push_back(inst);
}

void codegen_t::finish_closure_literals(scm_obj_t& current_closure_label, std::vector<scm_obj_t>& current_literals) {
  if (current_closure_label != scm_nil) {
    if (!current_literals.empty()) {
      scm_obj_t vec = make_vector(current_literals.size(), scm_false);
      for (size_t i = 0; i < current_literals.size(); ++i) {
        ((scm_vector_rec_t*)to_address(vec))->elts[i] = current_literals[i];
      }
      closure_literals[current_closure_label] = vec;
    }
    current_literals.clear();
    current_closure_label = scm_nil;
  }
}

// --------------------------------------------------------------------------
//  Per-opcode parsers
// --------------------------------------------------------------------------

void codegen_t::parse_const(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info, scm_obj_t& current_closure_label,
                            std::vector<scm_obj_t>& current_literals) {
  inst.rn1 = parse_reg(operand(inst_obj, 1));
  inst.opr1 = operand(inst_obj, 2);  // val
  updateMaxRegister(inst.rn1, func_info.max_reg);

  // Collect literals if in closure
  if (current_closure_label != scm_nil) {
    if (is_cons(inst.opr1) || is_heap_object(inst.opr1)) {
      current_literals.push_back(inst.opr1);
    }
  }
}

void codegen_t::parse_mov(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info) {
  inst.rn1 = parse_reg(operand(inst_obj, 1));  // dst
  inst.rn2 = parse_reg(operand(inst_obj, 2));  // src
  updateMaxRegister(inst.rn1, func_info.max_reg);
  updateMaxRegister(inst.rn2, func_info.max_reg);
}

void codegen_t::parse_if(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info) {
  inst.opr1 = operand(inst_obj, 1);
  inst.opr2 = operand(inst_obj, 2);
  updateMaxRegister(0, func_info.max_reg);  // Implicitly uses r0
}

void codegen_t::parse_jump(const scm_obj_t& inst_obj, Instruction& inst) { inst.opr1 = operand(inst_obj, 1); }

void codegen_t::parse_label(const scm_obj_t& inst_obj, Instruction& inst, scm_obj_t& current_closure_label,
                            std::vector<scm_obj_t>& current_literals) {
  inst.opr1 = operand(inst_obj, 1);
  // Check if this is a closure label
  if (is_closure_label(inst.opr1)) {
    finish_closure_literals(current_closure_label, current_literals);
    current_closure_label = inst.opr1;
  }
}

void codegen_t::parse_ret(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info) {
  updateMaxRegister(0, func_info.max_reg);  // Implicitly uses r0
}

void codegen_t::parse_make_closure(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info) {
  inst.rn1 = parse_reg(operand(inst_obj, 1));  // dst
  inst.opr1 = operand(inst_obj, 2);            // label (C1)
  inst.free_indices = operand(inst_obj, 3);    // free indices list

  inst.argc = fixnum(operand(inst_obj, 5));
  inst.has_rest = (operand(inst_obj, 6) == scm_true);

  // Record closure parameters for function generation
  closure_params[inst.opr1] = {inst.argc, inst.has_rest};

  updateMaxRegister(inst.rn1, func_info.max_reg);

  // Update max reg for free indices
  scm_obj_t fi = inst.free_indices;
  while (is_cons(fi)) {
    int r = parse_reg(CAR(fi));
    updateMaxRegister(r, func_info.max_reg);
    fi = CDR(fi);
  }
}

void codegen_t::parse_global_set(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info) {
  inst.opr1 = operand(inst_obj, 1);  // symbol

  scm_obj_t val = operand(inst_obj, 2);
  int r = parse_reg(val);
  if (r < 0) {
    fatal("%s:%u codegen: global-set! requires a register operand", __FILE__, __LINE__);
  }
  inst.rn1 = r;
  updateMaxRegister(inst.rn1, func_info.max_reg);
}

void codegen_t::parse_global_ref(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info) {
  inst.rn1 = parse_reg(operand(inst_obj, 1));  // dst reg
  inst.opr2 = operand(inst_obj, 2);            // symbol

  if (inst.rn1 < 0) {
    fatal("%s:%u codegen: global-ref requires a register operand", __FILE__, __LINE__);
  }
  updateMaxRegister(inst.rn1, func_info.max_reg);
}

void codegen_t::parse_call(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info) {
  inst.rn1 = parse_reg(operand(inst_obj, 1));  // closure reg
  inst.argc = fixnum(operand(inst_obj, 2));

  if (inst.rn1 < 0) {
    fatal("%s:%u codegen: call requires a register operand for closure", __FILE__, __LINE__);
  }
  updateMaxRegister(inst.rn1, func_info.max_reg);
}

void codegen_t::parse_tail_call(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info) {
  inst.rn1 = parse_reg(operand(inst_obj, 1));  // closure reg
  inst.argc = fixnum(operand(inst_obj, 2));

  if (inst.rn1 < 0) {
    fatal("%s:%u codegen: tail-call requires a register operand for closure", __FILE__, __LINE__);
  }
  updateMaxRegister(inst.rn1, func_info.max_reg);
}

void codegen_t::parse_closure_ref(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info) {
  inst.rn1 = parse_reg(operand(inst_obj, 1));  // dst reg
  inst.opr2 = operand(inst_obj, 2);            // index

  if (inst.rn1 < 0) {
    fatal("%s:%u codegen: closure-ref missing register operand", __FILE__, __LINE__);
  }
  updateMaxRegister(inst.rn1, func_info.max_reg);
}

void codegen_t::parse_closure_set(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info) {
  inst.opr1 = operand(inst_obj, 1);            // index
  inst.rn2 = parse_reg(operand(inst_obj, 2));  // src reg

  if (inst.rn2 < 0) {
    fatal("%s:%u codegen: closure-set! missing register operand", __FILE__, __LINE__);
  }
  updateMaxRegister(inst.rn2, func_info.max_reg);
}

void codegen_t::parse_closure_self(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info) {
  inst.rn1 = parse_reg(operand(inst_obj, 1));  // dst reg

  if (inst.rn1 < 0) {
    fatal("%s:%u codegen: closure-self missing register operand", __FILE__, __LINE__);
  }
  updateMaxRegister(inst.rn1, func_info.max_reg);
}

void codegen_t::parse_closure_cell_ref(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info) {
  inst.rn1 = parse_reg(operand(inst_obj, 1));  // dst reg
  inst.opr2 = operand(inst_obj, 2);            // index

  if (inst.rn1 < 0) {
    fatal("%s:%u codegen: closure-cell-ref missing register operand", __FILE__, __LINE__);
  }
  updateMaxRegister(inst.rn1, func_info.max_reg);
}

void codegen_t::parse_closure_cell_set(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info) {
  inst.opr1 = operand(inst_obj, 1);            // index
  inst.rn2 = parse_reg(operand(inst_obj, 2));  // src reg

  if (inst.rn2 < 0) {
    fatal("%s:%u codegen: closure-cell-set! missing register operand", __FILE__, __LINE__);
  }
  updateMaxRegister(inst.rn2, func_info.max_reg);
}

void codegen_t::parse_reg_cell_ref(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info) {
  inst.rn1 = parse_reg(operand(inst_obj, 1));  // dst reg
  inst.rn2 = parse_reg(operand(inst_obj, 2));  // src reg

  if (inst.rn1 < 0 || inst.rn2 < 0) {
    fatal("%s:%u codegen: reg-cell-ref missing register operand", __FILE__, __LINE__);
  }
  updateMaxRegister(inst.rn1, func_info.max_reg);
  updateMaxRegister(inst.rn2, func_info.max_reg);
}

void codegen_t::parse_reg_cell_set(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info) {
  inst.rn1 = parse_reg(operand(inst_obj, 1));  // dst reg (holds cell)
  inst.rn2 = parse_reg(operand(inst_obj, 2));  // src reg (holds value)

  if (inst.rn1 < 0 || inst.rn2 < 0) {
    fatal("%s:%u codegen: reg-cell-set! missing register operand", __FILE__, __LINE__);
  }
  updateMaxRegister(inst.rn1, func_info.max_reg);
  updateMaxRegister(inst.rn2, func_info.max_reg);
}

void codegen_t::parse_make_cell(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info) {
  inst.rn1 = parse_reg(operand(inst_obj, 1));  // dst/src reg

  if (inst.rn1 < 0) {
    fatal("%s:%u codegen: make-cell missing register operand", __FILE__, __LINE__);
  }
  updateMaxRegister(inst.rn1, func_info.max_reg);
}

// ============================================================================
//  IR helpers — types, values, utilities
// ============================================================================

llvm::Value* codegen_t::get_reg(int idx) {
  if (idx < 0 || (size_t)idx >= allocas.size()) {
    fatal("%s:%u codegen: register index out of bounds: r%d (max: r%lu) in function %s", __FILE__, __LINE__, idx, allocas.size() - 1,
          current_function ? current_function->getName().str().c_str() : "unknown");
  }
  return BL.CreateLoad(this->getInt64Type(), allocas[idx]);
}

void codegen_t::set_reg(int idx, llvm::Value* val) {
  if (idx < 0 || (size_t)idx >= allocas.size()) {
    fatal("%s:%u codegen: register index out of bounds: r%d (max: r%lu) in function %s", __FILE__, __LINE__, idx, allocas.size() - 1,
          current_function ? current_function->getName().str().c_str() : "unknown");
  }
  BL.CreateStore(val, allocas[idx]);
}

void codegen_t::create_allocas(llvm::Function* f, int num_regs) {
  llvm::IRBuilder<> tmpBuilder(&f->getEntryBlock(), f->getEntryBlock().begin());
  allocas.clear();
  if (num_regs <= 0) return;

  allocas.resize(num_regs);
  for (int i = 0; i < num_regs; ++i) {
    allocas[i] = tmpBuilder.CreateAlloca(this->getInt64Type(), nullptr, "r" + std::to_string(i));
    if (!allocas[i]) {
      fatal("%s:%u codegen: failed to create alloca for register r%d", __FILE__, __LINE__, i);
    }
    // Initialize with 0
    tmpBuilder.CreateStore(createInt64Constant(CT, 0), allocas[i]);
  }
}

void codegen_t::setup_closure_rest_arguments(int fixed_argc, llvm::Value* actual_argc, llvm::Value* argv_ptr) {
  // Calculate number of rest arguments: actual_argc - fixed_argc
  llvm::Value* fixed_argc_val = createInt64Constant(CT, fixed_argc);
  llvm::Value* rest_count = BL.CreateSub(actual_argc, fixed_argc_val, "rest_count");

  // Get c_construct_rest_list helper function
  // This C function builds a Scheme list from an array of arguments
  llvm::Type* intptrTy = this->getInt64Type();
  llvm::Type* intptrPtrTy = this->getInt64PtrType();
  llvm::Type* int32Ty = this->getInt32Type();

  // Ensure count uses int32 for C call
  llvm::Value* count_i32 = BL.CreateTrunc(rest_count, int32Ty);

  // Function type for helper: (i32, intptr_t*) -> intptr_t
  std::vector<llvm::Type*> helperArgTypes = {int32Ty, intptrPtrTy};
  llvm::FunctionType* helperFT = llvm::FunctionType::get(intptrTy, helperArgTypes, false);
  llvm::Function* helper_func = get_or_create_external_function("c_construct_rest_list", helperFT, (void*)&c_construct_rest_list);

  // Calculate pointer to argv[fixed_argc] (start of rest arguments)
  llvm::Value* rest_argv_ptr = BL.CreateGEP(intptrTy, argv_ptr, fixed_argc_val, "rest_argv_ptr");

  // Call helper to construct the rest list
  llvm::Value* rest_list = BL.CreateCall(helper_func, {count_i32, rest_argv_ptr}, "rest_list");

  // Store rest list to register[fixed_argc]
  // In Scheme, if a function has signature (lambda (a b . rest) ...),
  // then 'a' is in r0, 'b' is in r1, and 'rest' is in r2
  set_reg(fixed_argc, rest_list);
}

llvm::Value* codegen_t::getClosureCodePtr(llvm::Value* closure_tagged) {
  llvm::Value* closure_ptr = untagPointer(BL, CT, closure_tagged);
  llvm::Value* code_field_ptr = BL.CreateConstInBoundsGEP1_32(BL.getInt8Ty(), closure_ptr, CLOSURE_CODE_FIELD_OFFSET);
  return BL.CreateLoad(this->getVoidPtrType(), code_field_ptr, "code_ptr");
}

void codegen_t::emitWriteBarrier(llvm::Value* value) {
  llvm::Type* intptrTy = this->getInt64Type();
  llvm::Type* voidTy = llvm::Type::getVoidTy(CT);
  std::vector<llvm::Type*> wbArgTypes = {intptrTy};
  llvm::FunctionType* wbFT = llvm::FunctionType::get(voidTy, wbArgTypes, false);
  llvm::Function* wb_func = get_or_create_external_function("c_write_barrier", wbFT, (void*)&c_write_barrier);
  BL.CreateCall(wb_func, {value});
}

llvm::Function* codegen_t::get_or_create_external_function(const char* name, llvm::FunctionType* type, void* symbol_ptr) {
  llvm::Function* func = main_module->getFunction(name);
  if (!func) {
    func = llvm::Function::Create(type, llvm::Function::ExternalLinkage, name, main_module);
    func->setDSOLocal(true);
    if (is_side_effect_free_aux_helper(name)) add_side_effect_free_attributes(func);
    // Register the symbol with the JIT's main dylib via absoluteSymbols
    llvm::orc::SymbolMap symbols;
    symbols[jit->mangleAndIntern(name)] = {llvm::orc::ExecutorAddr::fromPtr(symbol_ptr),
                                           llvm::JITSymbolFlags::Exported | llvm::JITSymbolFlags::Callable};
    if (auto err = jit->getMainJITDylib().define(llvm::orc::absoluteSymbols(std::move(symbols)))) {
      llvm::consumeError(std::move(err));  // Symbol may already be defined from a previous compile
    }
  }
  return func;
}

void codegen_t::add_side_effect_free_attributes(llvm::Function* func) {
  auto& ctx = func->getContext();
  func->addFnAttr(llvm::Attribute::getWithMemoryEffects(ctx, llvm::MemoryEffects::readOnly()));
  func->addFnAttr(llvm::Attribute::WillReturn);
  func->addFnAttr(llvm::Attribute::NoUnwind);
  func->addFnAttr(llvm::Attribute::MustProgress);
  func->addFnAttr(llvm::Attribute::NoFree);
  func->addFnAttr(llvm::Attribute::NoSync);
}

void codegen_t::add_common_closure_attributes(llvm::Function* func) {
  func->setVisibility(llvm::GlobalValue::HiddenVisibility);
  func->setCallingConv(CLOSURE_CALLING_CONV);
  func->setDSOLocal(true);
}

// ============================================================================
//  Analysis and debugging
// ============================================================================

void codegen_t::analyze_closure_labels() {
  std::map<scm_obj_t, scm_obj_t> global_closure_defs;

  struct State {
    std::map<int, scm_obj_t> regs;
    std::map<scm_obj_t, scm_obj_t> globals;

    bool merge(const State& other) {
      bool changed = false;
      for (auto const& [reg, label] : other.regs) {
        if (regs.find(reg) == regs.end()) {
          regs[reg] = label;
          changed = true;
        } else if (regs[reg] != label) {
          if (regs[reg] != scm_nil) {
            regs[reg] = scm_nil;
            changed = true;
          }
        }
      }
      for (auto const& [var, label] : other.globals) {
        if (globals.find(var) == globals.end()) {
          globals[var] = label;
          changed = true;
        } else if (globals[var] != label) {
          if (globals[var] != scm_nil) {
            globals[var] = scm_nil;
            changed = true;
          }
        }
      }
      return changed;
    }
  };

  for (auto& func : functions) {
    std::map<scm_obj_t, State> block_entry_states;
    bool changed = true;
    while (changed) {
      changed = false;
      State current_state;

      for (size_t i = 0; i < func.instructions.size(); ++i) {
        auto& inst = func.instructions[i];

        if (inst.op == Opcode::LABEL) {
          if (block_entry_states[inst.opr1].merge(current_state)) {
            changed = true;
          }
          current_state = block_entry_states[inst.opr1];
        }

        switch (inst.op) {
          case Opcode::MAKE_CLOSURE:
            current_state.regs[inst.rn1] = inst.opr1;
            break;
          case Opcode::CLOSURE_SELF:
            current_state.regs[inst.rn1] = func.label;
            break;
          case Opcode::MOV:
            current_state.regs[inst.rn1] = (current_state.regs.count(inst.rn2) ? current_state.regs[inst.rn2] : scm_nil);
            break;
          case Opcode::GLOBAL_SET:
            current_state.globals[inst.opr1] = (current_state.regs.count(inst.rn1) ? current_state.regs[inst.rn1] : scm_nil);
            if (current_state.regs.count(inst.rn1) && current_state.regs[inst.rn1] != scm_nil) {
              global_closure_defs[inst.opr1] = current_state.regs[inst.rn1];
            }
            break;
          case Opcode::GLOBAL_REF:
            if (current_state.globals.count(inst.opr2)) {
              current_state.regs[inst.rn1] = current_state.globals[inst.opr2];
            } else if (global_closure_defs.count(inst.opr2)) {
              current_state.regs[inst.rn1] = global_closure_defs[inst.opr2];
            } else {
              // Try to look up in the global environment
              scm_obj_t val = object_heap_t::current()->environment_variable_ref(inst.opr2);
              if (val != scm_undef) {
                current_state.regs[inst.rn1] = inst.opr2;
                if (is_closure(val)) {
                  closure_params[inst.opr2] = {closure_argc(val), closure_rest(val) == 1};
                }
              } else {
#ifndef NDEBUG
                std::cout << "[codegen] Unknown global or letrec closure: " << symbol_name(inst.opr2) << std::endl;
#endif
                current_state.regs[inst.rn1] = make_string((const char*)symbol_name(inst.opr2));
              }
            }
            break;
          case Opcode::CALL:
          case Opcode::TAIL_CALL:
            if (current_state.regs.count(inst.rn1)) {
              inst.closure_label = current_state.regs[inst.rn1];
            } else {
              inst.closure_label = scm_nil;
            }
            break;
          case Opcode::JUMP:
            if (block_entry_states[inst.opr1].merge(current_state)) {
              changed = true;
            }
            current_state = State();  // Conservative: reset state after jump if not fall-through
            break;
          case Opcode::IF:
            if (block_entry_states[inst.opr1].merge(current_state)) {
              changed = true;
            }
            if (block_entry_states[inst.opr2].merge(current_state)) {
              changed = true;
            }
            // State continues for fall-through (though in this IR IF usually has two labels)
            break;
          default:
            if (inst.rn1 != -1) {
              current_state.regs[inst.rn1] = scm_nil;
            }
            break;
        }
      }
    }
  }
}

void codegen_t::dump_instructions(const std::vector<Instruction>& instructions) {
  std::ofstream ofs("/tmp/nanos.ins", std::ios::app);
  if (!ofs.is_open()) return;
  printer_t printer(ofs);
  for (const auto& inst : instructions) {
    printer.write(inst.original);
    if (inst.closure_label != scm_nil) {
      ofs << " ; closure_label: ";
      printer.write(inst.closure_label);
    }
    ofs << "\n";
  }
}
