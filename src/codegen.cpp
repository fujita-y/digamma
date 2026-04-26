// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "codegen.h"
#include "codegen_aux.h"
#include "codegen_common.h"
#include "context.h"
#include "nanos_jit.h"
#include "printer.h"
#include "uniq_id.h"

#include <cstddef>
#include <fstream>
#include <iostream>
#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/ExecutionEngine/Orc/AbsoluteSymbols.h>
#include <llvm/IR/MDBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/OptimizationLevel.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar/ADCE.h>
#include <llvm/Transforms/Scalar/EarlyCSE.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/SCCP.h>
#include <llvm/Transforms/Scalar/SROA.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>
#include <llvm/Transforms/Utils/SimplifyCFGOptions.h>

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

intptr_t compiled_code_t::release_and_run() {
  // Grab the function pointer locally before clearing it from the object.
  auto f = func_ptr;
  func_ptr = nullptr;
  // Explicitly remove the LLVM tracker BEFORE running JIT code.
  // Scheme continuations captured before subr_codegen_and_run can overwrite
  // this function's stack frame (including 'tracker') during restoration.
  // By releasing here, the destructor becomes a safe no-op even on a
  // corrupted stack.
  if (tracker) {
    if (auto err = tracker->remove()) {
      llvm::consumeError(std::move(err));
    }
    tracker = nullptr;
  }
  if (f) return f();
  return 0;
}

// ============================================================================
//  Static / thread-local state
// ============================================================================

// ============================================================================
//  Constructor
// ============================================================================

codegen_t::codegen_t(std::unique_ptr<llvm::LLVMContext> ctx, nanos_jit_t* jit) : context_uptr(std::move(ctx)), jit(jit) {
  builder = std::make_unique<llvm::IRBuilder<>>(CT);
  cached_symbol_label = make_symbol("label");
  cached_symbol_apply = make_symbol("apply");
  cached_symbol_safepoint = make_symbol("safepoint");
  context::add_literal(cached_symbol_label);
  context::add_literal(cached_symbol_apply);
  context::add_literal(cached_symbol_safepoint);
  init_opcode_map();
  context::s_current_codegen = this;
}

// ============================================================================
//  Compilation pipeline (phases)
// ============================================================================



// --------------------------------------------------------------------------
//  configure_module helper
// --------------------------------------------------------------------------

void codegen_t::configure_module(llvm::Module& M) {
  M.setDataLayout(jit->getDataLayout());
  M.setTargetTriple(jit->getTargetTriple());
  M.setPICLevel(llvm::PICLevel::BigPIC);
  M.setPIELevel(llvm::PIELevel::Large);
}

std::string codegen_t::generate_unique_suffix() { return generate_process_unique_suffix(); }

// --------------------------------------------------------------------------
//  compile() — top-level entry point
// --------------------------------------------------------------------------

void codegen_t::reset_compile_state() {
  // Drop LLVM modules first (they may reference the context that CompileScope
  // is about to swap back, so release them before the context is restored).
  main_module_uptr.reset();
  closure_module_uptr.reset();
  main_module = nullptr;
  closure_module = nullptr;
  main_function = nullptr;
  current_function = nullptr;
  current_function_info = nullptr;
  current_closure_self = nullptr;
  functions.clear();
  allocas.clear();
  labels.clear();
  function_map.clear();
  closure_params.clear();
  global_closure_defs.clear();
  closure_cell_labels.clear();
  for (scm_obj_t obj : gc_protected_objects) context::gc_unprotect(obj);
  gc_protected_objects.clear();
}

compiled_code_t codegen_t::compile(scm_obj_t inst_list) {
  auto saved_ctx = std::move(this->context_uptr);
  auto saved_builder = std::move(this->builder);

  this->context_uptr = std::make_unique<llvm::LLVMContext>();
  this->builder = std::make_unique<llvm::IRBuilder<>>(*this->context_uptr);

  try {
    phase0_create_module();
    phase1_parse_instructions(inst_list);
    phase2a_analyze_closure_labels();
    phase2b_analyze_no_escape();
    phase2c_analyze_safepoints();
    phase2d_analyze_cell_stack_alloc();
#ifndef NDEBUG
    {
      std::string tid_suffix = std::to_string(std::hash<std::thread::id>{}(std::this_thread::get_id()));
      std::ofstream ofs("/tmp/nanos_" + tid_suffix + ".ins", std::ios::trunc);
    }
    for (const auto& func : functions) {
      dump_instructions(func.instructions);
    }
#endif
    phase3_create_functions();
    phase4_generate_code();
    phase5_optimize_and_verify();
    compiled_code_t code = phase6_finalize();

    for (scm_obj_t obj : gc_protected_objects) context::gc_unprotect(obj);
    gc_protected_objects.clear();

    this->context_uptr = std::move(saved_ctx);
    this->builder = std::move(saved_builder);

    return code;
  } catch (...) {
    reset_compile_state();
    this->context_uptr = std::move(saved_ctx);
    this->builder = std::move(saved_builder);
    throw;
  }
}

// --------------------------------------------------------------------------
//  Phase 0: Module creation
// --------------------------------------------------------------------------

void codegen_t::phase0_create_module() {
  if (cached_call_closure_bridge == nullptr) {
    // Initialize bridge (this creates a standalone module, compiles the body, and gives it to JIT)
    (void)call_closure_bridge();
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

void codegen_t::phase2c_analyze_safepoints() {
  for (auto& func : functions) {
    if (func.instructions.empty()) continue;
    bool needs_safepoint = false;
    for (size_t i = 0; i < func.instructions.size(); ++i) {
      auto& inst = func.instructions[i];
      if (inst.op == Opcode::MAKE_CLOSURE || inst.op == Opcode::MAKE_CELL) {
        needs_safepoint = true;
        break;
      } else if (inst.op == Opcode::CALL || inst.op == Opcode::TAIL_CALL) {
        if (is_symbol(inst.closure_label)) {
          if (is_closure_label(inst.closure_label)) {
            // std::cout << "#### phase3_analyze_safepoints: closure label " << to_string(inst.closure_label) << std::endl;
            continue;
          }
          scm_obj_t val = context::environment_variable_ref(inst.closure_label);
          if (is_closure(val)) {
            scm_closure_rec_t* closure_rec = (scm_closure_rec_t*)to_address(val);
            void* code_ptr = closure_rec->code;
            if (no_gc_code_set.contains(code_ptr)) {
              // std::cout << "#### phase3_analyze_safepoints: known subr " << to_string(inst.closure_label) << std::endl;
              continue;
            }
          }
        }
        needs_safepoint = true;
        break;
      }
    }
    if (needs_safepoint) {
      Instruction si;
      si.op = Opcode::SAFEPOINT;
      si.original = scm_nil;
      size_t insert_pos = 0;
      if (func.instructions.size() > 0 && func.instructions[0].op == Opcode::LABEL) {
        insert_pos = 1;
      }
      func.instructions.insert(func.instructions.begin() + insert_pos, si);
    } else {
      // std::cout << "#### phase3_analyze_safepoints: no safepoint for " << to_string(func.label) << std::endl;
    }
  }
}

// --------------------------------------------------------------------------
//  Phase 2: Function and BasicBlock creation
// --------------------------------------------------------------------------

void codegen_t::phase3_create_functions() {
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

void codegen_t::phase4_generate_code() {
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

void codegen_t::optimize_module(llvm::Module& mod) {
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
}

void codegen_t::phase5_optimize_and_verify() {
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
  {
    // Use thread-local suffix to avoid interleaved writes from concurrent instances.
    std::string tid_suffix = std::to_string(std::hash<std::thread::id>{}(std::this_thread::get_id()));
    std::error_code EC;
    llvm::raw_fd_ostream dest("/tmp/nanos_main_" + tid_suffix + ".ll", EC, llvm::sys::fs::OF_None);
    if (EC) {
      llvm::errs() << "Could not open file: " << EC.message() << "\n";
    } else {
      main_module_uptr->print(dest, nullptr);
    }
  }
#endif

  if (functions.size() > 1) {
#ifndef NDEBUG
    {
      std::string tid_suffix = std::to_string(std::hash<std::thread::id>{}(std::this_thread::get_id()));
      std::error_code EC;
      llvm::raw_fd_ostream dest2("/tmp/nanos_closures_" + tid_suffix + ".ll", EC, llvm::sys::fs::OF_None);
      if (EC) {
        llvm::errs() << "Could not open file: " << EC.message() << "\n";
      } else {
        closure_module_uptr->print(dest2, nullptr);
      }
    }
#endif
  }
}

// --------------------------------------------------------------------------
//  Phase 5: Finalize and hand off to JIT
// --------------------------------------------------------------------------

compiled_code_t codegen_t::phase6_finalize() {
  std::lock_guard<std::mutex> lock(jit->m_lock);

  // Transfer modules to LLJIT
  std::string main_func_name = main_function->getName().str();

  // Both modules were built in the same LLVMContext (context_uptr).
  // To avoid serialization bottlenecks and deadlocks in the multi-threaded COD JIT,
  // we decouple them by cloning the closure module into its own fresh context via bitcode.

  if (functions.size() > 1) {
    // 1. Serialize closure_module to bitcode in-memory
    llvm::SmallVector<char, 0> bitcode;
    llvm::raw_svector_ostream bitcode_stream(bitcode);
    llvm::WriteBitcodeToFile(*closure_module_uptr, bitcode_stream);

    // 2. Parse bitcode into a completely new, isolated LLVMContext
    auto fresh_ctx = std::make_unique<llvm::LLVMContext>();
    auto memory_buffer = llvm::MemoryBuffer::getMemBuffer(llvm::StringRef(bitcode.data(), bitcode.size()), "closure_bitcode", false);
    auto parsed_module = cantFail(llvm::parseBitcodeFile(*memory_buffer, *fresh_ctx));

    // 3. Wrap in an independent ThreadSafeContext & ThreadSafeModule
    llvm::orc::ThreadSafeContext fresh_tsc(std::move(fresh_ctx));
    auto clo_tsm = llvm::orc::ThreadSafeModule(std::move(parsed_module), fresh_tsc);

    if (auto err = jit->addIRModule(std::move(clo_tsm))) {
      fatal("%s:%u codegen: failed to add closure module to JIT: %s", __FILE__, __LINE__, llvm::toString(std::move(err)).c_str());
    }
  }

  // The main module takes exclusive ownership of the original context_uptr
  llvm::orc::ThreadSafeContext tsc(std::move(context_uptr));
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

void codegen_t::parse_instructions(scm_obj_t inst_list) {
  functions.clear();
  closure_params.clear();
  global_closure_defs.clear();
  closure_cell_labels.clear();

  // Create main function info
  functions.emplace_back();
  FunctionInfo* current_func = &functions.back();
  current_func->label = scm_nil;

  scm_obj_t curr = inst_list;
  scm_obj_t current_closure_label = scm_nil;

  // Detect closure parameters to switch contexts.
  // Closures always start with a unique label which starts with 'C'.
  while (curr != scm_nil) {
    if (!is_cons(curr)) break;
    scm_obj_t inst_obj = cons_car(curr);
    curr = cons_cdr(curr);

    // Check for LABEL opcode to detect function switch
    if (is_cons(inst_obj) && cons_car(inst_obj) == cached_symbol_label) {
      scm_obj_t label = operand(inst_obj, 1);
      if (is_closure_label(label)) {
        // Start new function
        functions.emplace_back();
        current_func = &functions.back();
        current_func->label = label;
        current_closure_label = label;

        // label instruction
        parse_single_instruction(inst_obj, *current_func, current_closure_label);

        continue;
      }
    }

    parse_single_instruction(inst_obj, *current_func, current_closure_label);
  }

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

void codegen_t::parse_single_instruction(scm_obj_t inst_obj, FunctionInfo& func_info, scm_obj_t& current_closure_label) {
  if (!is_cons(inst_obj)) return;

  Instruction inst;
  inst.original = inst_obj;
  inst.op = Opcode::UNKNOWN;

  scm_obj_t op_sym = cons_car(inst_obj);

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
      parse_const(inst_obj, inst, func_info, current_closure_label);
      break;
    case Opcode::UNSPECIFIED:
      parse_unspecified(inst_obj, inst, func_info);
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
      parse_label(inst_obj, inst, current_closure_label);
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

// --------------------------------------------------------------------------
//  Per-opcode parsers
// --------------------------------------------------------------------------

void codegen_t::parse_const(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info, scm_obj_t& current_closure_label) {
  inst.rn1 = parse_reg(operand(inst_obj, 1));
  inst.opr1 = operand(inst_obj, 2);  // val
  updateMaxRegister(inst.rn1, func_info.max_reg);
  // Register literal
  if (is_cons(inst.opr1) || is_heap_object(inst.opr1)) {
    context::add_literal(inst.opr1);
  }
}

void codegen_t::parse_unspecified(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info) {
  inst.rn1 = parse_reg(operand(inst_obj, 1));
  updateMaxRegister(inst.rn1, func_info.max_reg);
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

void codegen_t::parse_label(const scm_obj_t& inst_obj, Instruction& inst, scm_obj_t& current_closure_label) {
  inst.opr1 = operand(inst_obj, 1);
  // Check if this is a closure label
  if (is_closure_label(inst.opr1)) {
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

  inst.argc = fixnum(operand(inst_obj, 4));
  inst.has_rest = (operand(inst_obj, 5) == scm_true);

  // Record closure parameters for function generation
  closure_params[inst.opr1] = {inst.argc, inst.has_rest};

  updateMaxRegister(inst.rn1, func_info.max_reg);

  // Update max reg for free indices
  scm_obj_t fi = inst.free_indices;
  while (is_cons(fi)) {
    int r = parse_reg(cons_car(fi));
    updateMaxRegister(r, func_info.max_reg);
    fi = cons_cdr(fi);
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
    std::cerr << "[codegen] Malformed call instruction: ";
    printer_t(std::cerr).write(inst_obj);
    std::cerr << std::endl;
    fatal("%s:%u codegen: call requires a register operand for closure", __FILE__, __LINE__);
  }
  updateMaxRegister(inst.rn1, func_info.max_reg);
}

void codegen_t::parse_tail_call(const scm_obj_t& inst_obj, Instruction& inst, FunctionInfo& func_info) {
  inst.rn1 = parse_reg(operand(inst_obj, 1));  // closure reg
  inst.argc = fixnum(operand(inst_obj, 2));

  if (inst.rn1 < 0) {
    std::cerr << "[codegen] Malformed tail-call instruction: ";
    printer_t(std::cerr).write(inst_obj);
    std::cerr << std::endl;
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
  llvm::Value* rest_list = BL.CreateCall(helperFT, helper_func, {count_i32, rest_argv_ptr}, "rest_list");

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

void codegen_t::emit_write_barrier(llvm::Value* value) {
  llvm::Type* intptrTy = this->getInt64Type();
  llvm::Type* voidTy = llvm::Type::getVoidTy(CT);
  std::vector<llvm::Type*> wbArgTypes = {intptrTy};
  llvm::FunctionType* wbFT = llvm::FunctionType::get(voidTy, wbArgTypes, false);
  llvm::Function* wb_func = get_or_create_external_function("c_write_barrier", wbFT, (void*)&c_write_barrier);
  BL.CreateCall(wbFT, wb_func, {value});
}

llvm::Function* codegen_t::get_or_create_external_function(const char* name, llvm::FunctionType* type, void* symbol_ptr) {
  llvm::Function* func = main_module->getFunction(name);
  if (!func) {
    func = llvm::Function::Create(type, llvm::Function::ExternalLinkage, name, main_module);
    func->setDSOLocal(true);
    if (is_side_effect_free_aux_helper(name)) add_side_effect_free_attributes(func);
    if (is_never_return_aux_helper(name)) add_never_return_attributes(func);
    // Register the symbol with the JIT's main dylib via absoluteSymbols.
    // Lock jit->m_lock so concurrent codegen instances on different threads
    // don't race on the shared JIT dylib's internal state.
    llvm::orc::SymbolMap symbols;
    symbols[jit->mangleAndIntern(name)] = {llvm::orc::ExecutorAddr::fromPtr(symbol_ptr),
                                           llvm::JITSymbolFlags::Exported | llvm::JITSymbolFlags::Callable};
    {
      std::lock_guard<std::mutex> lock(jit->m_lock);
      if (auto err = jit->getMainJITDylib().define(llvm::orc::absoluteSymbols(std::move(symbols)))) {
        llvm::consumeError(std::move(err));  // Symbol may already be defined from a previous compile
      }
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

void codegen_t::add_never_return_attributes(llvm::Function* func) {
  auto& ctx = func->getContext();
  func->addFnAttr(llvm::Attribute::NoReturn);
  func->addFnAttr(llvm::Attribute::Cold);
}

void codegen_t::add_common_closure_attributes(llvm::Function* func) {
  func->setVisibility(llvm::GlobalValue::HiddenVisibility);
  func->setCallingConv(CLOSURE_CALLING_CONV);
  func->setDSOLocal(true);
  func->addFnAttr(llvm::Attribute::NoInline);
}

// ============================================================================
//  Analysis and debugging
// ============================================================================

void codegen_t::phase2a_analyze_closure_labels() {
  struct State {
    std::unordered_map<int, scm_obj_t> regs;   // reg index -> closure label
    std::unordered_map<int, scm_obj_t> cells;  // reg-cell index -> closure label
    std::unordered_map<scm_obj_t, scm_obj_t> globals;

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
      for (auto const& [cell, label] : other.cells) {
        if (cells.find(cell) == cells.end()) {
          cells[cell] = label;
          changed = true;
        } else if (cells[cell] != label) {
          if (cells[cell] != scm_nil) {
            cells[cell] = scm_nil;
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
    // Per-function map: closure_label -> {free_idx -> reg_idx}
    // Scoped to this function so REG_CELL_SET only matches closures
    // make-closure'd in the same function body.
    std::unordered_map<scm_obj_t, std::unordered_map<int, int>> make_closure_free_reg;

    std::unordered_map<scm_obj_t, State> block_entry_states;
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
            // Record which register each free-variable slot captures, so that
            // a subsequent reg-cell-set! into that register can propagate the
            // written label into closure_cell_labels for use in the closure body.
            {
              scm_obj_t fi = inst.free_indices;
              int idx = 0;
              while (is_cons(fi)) {
                int reg = parse_reg(cons_car(fi));
                make_closure_free_reg[inst.opr1][idx] = reg;
                fi = cons_cdr(fi);
                idx++;
              }
            }
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
              scm_obj_t val = context::environment_variable_ref(inst.opr2);
              if (val != scm_undef) {
                current_state.regs[inst.rn1] = inst.opr2;
                if (is_closure(val)) {
                  closure_params[inst.opr2] = {closure_argc(val), closure_rest(val) == 1};
                }
              } else {
#ifndef NDEBUG
                std::cout << "[codegen] Unknown global or letrec closure: " << symbol_name(inst.opr2) << std::endl;
#endif

                scm_obj_t string_name = make_string((const char*)symbol_name(inst.opr2));
                context::gc_protect(string_name);
                gc_protected_objects.push_back(string_name);
                current_state.regs[inst.rn1] = string_name;
              }
            }
            break;
          case Opcode::REG_CELL_SET:
            // (reg-cell-set! cell-reg value-reg): store value of rn2 into the cell held in rn1
            {
              scm_obj_t val_label = current_state.regs.count(inst.rn2) ? current_state.regs[inst.rn2] : scm_nil;
              current_state.cells[inst.rn1] = val_label;
              // Propagate into closure_cell_labels only for the letrec self-referential
              // pattern: (make-closure r0 C (... rN ...) ...) followed by
              // (make-cell rN) / (reg-cell-set! rN r0).  The cell at free-var
              // slot k of closure C holds C itself iff val_label == cl_label.
              if (is_closure_label(val_label)) {
                auto it = make_closure_free_reg.find(val_label);
                if (it != make_closure_free_reg.end()) {
                  for (auto& [free_idx, reg_idx] : it->second) {
                    if (reg_idx == inst.rn1) {
                      closure_cell_labels[val_label][free_idx] = val_label;
                    }
                  }
                }
              }
            }
            break;
          case Opcode::REG_CELL_REF:
            // (reg-cell-ref dst-reg cell-reg): load from the cell in rn2 into rn1
            current_state.regs[inst.rn1] = (current_state.cells.count(inst.rn2) ? current_state.cells[inst.rn2] : scm_nil);
            break;
          case Opcode::CLOSURE_CELL_REF: {
            // (closure-cell-ref dst-reg idx): read cell slot idx of the current closure.
            // If that slot holds a known closure label (populated when the containing
            // closure was constructed via make-closure + reg-cell-set!), propagate it.
            int cell_idx = (int)fixnum(inst.opr2);
            scm_obj_t cl_label = func.label;
            if (closure_cell_labels.count(cl_label) && closure_cell_labels[cl_label].count(cell_idx)) {
              current_state.regs[inst.rn1] = closure_cell_labels[cl_label][cell_idx];
            } else {
              current_state.regs[inst.rn1] = scm_nil;
            }
            break;
          }
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

// ============================================================================
//  Phase 2b: No-escape analysis for MAKE_CLOSURE instructions
// ============================================================================

// For each MAKE_CLOSURE instruction, determine if the resulting closure object
// can escape to heap-reachable memory.  A closure is considered non-escaping
// (no_escape = true) when the closure value never reaches heap-reachable memory.
//
// Alias tracking uses three parallel sets:
//   aliases      — registers currently holding the closure directly
//   cell_aliases — registers whose pointed-to heap-cell contains the closure
//   slot_aliases — free-var slot indices of the current closure that contain
//                  the closure value
//
// REG_CELL_SET / CLOSURE_CELL_SET are treated like MOV into a cell:
//   writing the closure into a cell does NOT immediately escape it.
//   The closure escapes only when the *cell itself* escapes, i.e., the
//   cell-register is stored to a global, passed to an unknown callee, or
//   captured as a free variable of another closure.
// REG_CELL_REF / CLOSURE_CELL_REF retrieve the value back into aliases.
//
// MAKE_CELL rn1: wraps rn1's value in a cell and writes the cell back to rn1;
//   moves the direct alias from `aliases` into `cell_aliases`.
//
// A genuine escape is declared only when:
//   - RET: r0 holds a direct or cell alias
//   - GLOBAL_SET: rn1 is a direct or cell alias
//   - CALL/TAIL_CALL arg: a direct or cell alias appears as a call argument
//   - MAKE_CLOSURE free-var: a direct or cell alias is captured
//   - CALL/TAIL_CALL callee: the closure is called through an unknown callee
//
// Note: this pass runs after phase2_analyze_closure_labels so that
// inst.closure_label is already populated for CALL/TAIL_CALL instructions.
// NOTE: function_map is populated in phase4 (after this pass), so we build
// our own local_labels set from the parsed functions list instead.

void codegen_t::phase2b_analyze_no_escape() {
  // Build the set of closure labels compiled in this batch from the already-
  // parsed functions list.  function_map is not yet populated at this phase.
  std::unordered_set<scm_obj_t> local_labels;
  for (const auto& fi : functions) {
    if (fi.label != scm_nil) local_labels.insert(fi.label);
  }

  for (auto& func : functions) {
    const auto& insts = func.instructions;
    const size_t n = insts.size();

    for (size_t mk = 0; mk < n; ++mk) {
      auto& mk_inst = func.instructions[mk];
      if (mk_inst.op != Opcode::MAKE_CLOSURE) continue;

      const int dst = mk_inst.rn1;
      if (dst < 0) continue;

      std::unordered_set<int> aliases;       // direct register aliases
      std::unordered_set<int> cell_aliases;  // cell-register aliases
      std::unordered_set<int> slot_aliases;  // closure free-var slot aliases
      aliases.insert(dst);

      bool escaped = false;
      bool cell_aliases_used = false;  // becomes true if closure ever flowed through a heap cell
      bool tail_called = false;        // becomes true if closure is tail-called from creating frame

      for (size_t i = mk + 1; i < n && !escaped; ++i) {
        const auto& inst = insts[i];

        // Track when cell_aliases gains an entry (closure stored into a heap cell).
        size_t prev_cell_count = cell_aliases.size();

        switch (inst.op) {
          // ---- RET ----------------------------------------------------------
          case Opcode::RET:
            // Returns r0; escape if r0 holds the closure or a cell with it.
            if (aliases.count(0) || cell_aliases.count(0)) {
              escaped = true;
            }
            break;

          // ---- MOV ----------------------------------------------------------
          case Opcode::MOV: {
            // Propagate both alias kinds.
            bool src_direct = inst.rn2 >= 0 && aliases.count(inst.rn2);
            bool src_cell = inst.rn2 >= 0 && cell_aliases.count(inst.rn2);
            if (inst.rn1 >= 0) {
              if (src_direct)
                aliases.insert(inst.rn1);
              else
                aliases.erase(inst.rn1);
              if (src_cell)
                cell_aliases.insert(inst.rn1);
              else
                cell_aliases.erase(inst.rn1);
            }
            break;
          }

          // ---- MAKE_CELL ----------------------------------------------------
          case Opcode::MAKE_CELL:
            // make-cell rn1: wraps rn1's current value in a heap cell and writes
            // the cell pointer back into rn1.  If rn1 held the closure directly,
            // it now holds a cell containing the closure.
            if (inst.rn1 >= 0 && aliases.count(inst.rn1)) {
              aliases.erase(inst.rn1);
              cell_aliases.insert(inst.rn1);
            }
            break;

          // ---- REG_CELL_SET -------------------------------------------------
          case Opcode::REG_CELL_SET:
            // reg-cell-set! rn1(cell-reg) rn2(value):
            // Stores rn2 into the cell object pointed to by rn1.
            // This is like MOV into a cell — does NOT immediately escape.
            // The closure in rn2 flows into the cell held by rn1.
            if (inst.rn2 >= 0 && aliases.count(inst.rn2)) {
              if (inst.rn1 >= 0) cell_aliases.insert(inst.rn1);
              // rn2 still holds the closure value directly (src not consumed).
            }
            // reg-cell-set! writes *through* rn1 (the cell pointer) — it does
            // not overwrite rn1 as a register, so direct aliases in rn1 survive.
            break;

          // ---- REG_CELL_REF -------------------------------------------------
          case Opcode::REG_CELL_REF:
            // reg-cell-ref rn1(dst) rn2(cell-reg):
            // Loads the cell's contents into rn1.
            if (inst.rn2 >= 0 && cell_aliases.count(inst.rn2)) {
              if (inst.rn1 >= 0) aliases.insert(inst.rn1);  // closure retrieved
            } else {
              if (inst.rn1 >= 0) aliases.erase(inst.rn1);  // non-closure loaded
            }
            break;

          // ---- CLOSURE_CELL_SET ---------------------------------------------
          case Opcode::CLOSURE_CELL_SET:
            // closure-cell-set! opr1(slot-idx) rn2(value):
            // Stores rn2 into free-var slot opr1 of the current closure.
            // Like MOV into a slot — does NOT immediately escape.
            if (inst.rn2 >= 0 && aliases.count(inst.rn2)) {
              slot_aliases.insert((int)fixnum(inst.opr1));
              // rn2 still holds the closure.
            }
            break;

          // ---- CLOSURE_CELL_REF ---------------------------------------------
          case Opcode::CLOSURE_CELL_REF:
            // closure-cell-ref rn1(dst) opr2(slot-idx):
            // Loads free-var slot opr2 into rn1.
            if (slot_aliases.count((int)fixnum(inst.opr2))) {
              if (inst.rn1 >= 0) aliases.insert(inst.rn1);
            } else {
              if (inst.rn1 >= 0) aliases.erase(inst.rn1);
            }
            break;

          // ---- GLOBAL_SET ---------------------------------------------------
          case Opcode::GLOBAL_SET:
            // Storing a direct alias or a cell-register globally escapes the
            // closure (directly or via the cell that holds it).
            if (inst.rn1 >= 0 && (aliases.count(inst.rn1) || cell_aliases.count(inst.rn1))) {
              escaped = true;
            }
            break;

          // ---- MAKE_CLOSURE -------------------------------------------------
          case Opcode::MAKE_CLOSURE: {
            // If a direct alias or a cell-reg alias is captured as a free
            // variable, the closure (or the cell holding it) escapes.
            scm_obj_t fi = inst.free_indices;
            while (is_cons(fi)) {
              int r = parse_reg(cons_car(fi));
              if (r >= 0 && (aliases.count(r) || cell_aliases.count(r))) {
                escaped = true;
                break;
              }
              fi = cons_cdr(fi);
            }
            // Kill any alias overwritten by this instruction's destination.
            if (inst.rn1 >= 0) {
              aliases.erase(inst.rn1);
              cell_aliases.erase(inst.rn1);
            }
            break;
          }

          // ---- CALL / TAIL_CALL --------------------------------------------
          case Opcode::CALL:
          case Opcode::TAIL_CALL: {
            // The safe-HOF exemption applies ONLY to regular CALL instructions.
            //
            // For CALL: the creating frame stays alive while the HOF executes,
            // so the HOF can safely call back into a stack-allocated closure
            // argument.
            //
            // For TAIL_CALL: the creating frame is released BEFORE the callee
            // runs, so any stack-allocated closure in an argument position will
            // be dangling when the callee (even a safe HOF) tries to call it.
            bool callee_is_safe_hof =
                inst.op == Opcode::CALL && is_symbol(inst.closure_label) && proc_arg_safe_callees.count(inst.closure_label);

            if (!callee_is_safe_hof) {
              // If a direct alias or a cell-reg alias appears as a call argument,
              // the callee may store it on the heap (or call it after frame exits).
              for (int a = 0; a < inst.argc; ++a) {
                if (aliases.count(a) || cell_aliases.count(a)) {
                  if (inst.op == Opcode::TAIL_CALL) {
                    // Frame exits before callee runs — closure would be dangling.
                    tail_called = true;
                  } else {
                    escaped = true;
                  }
                  break;
                }
              }
            }
            if (escaped) break;

            // Closure used as the callee: non-escaping only if it is a
            // locally-compiled closure (receives `self`, not heap-stored).
            if (inst.rn1 >= 0 && aliases.count(inst.rn1)) {
              bool known_local = is_symbol(inst.closure_label) && local_labels.count(inst.closure_label);
              if (!known_local) {
                escaped = true;
              } else if (inst.op == Opcode::TAIL_CALL) {
                // A tail-call from the creating frame exits that frame before
                // the closure runs — a stack-allocated struct would be dangling.
                tail_called = true;
              }
            }
            // A cell alias in callee position is conservatively an escape.
            if (!escaped && inst.rn1 >= 0 && cell_aliases.count(inst.rn1)) {
              escaped = true;
            }

            // For a regular CALL (not tail-call), the argument registers
            // r0..r(argc-1) are consumed and r0 is overwritten with the return
            // value.  Kill any stale closure aliases in those slots so that
            // subsequent aliases don't propagate a phantom alias from the
            // return-value register into later instructions.
            //
            // Example of the false positive this prevents:
            //   (mov r0 r5)      ; r0 = closure (r5 already aliased it)
            //   (call map 2)     ; map(closure, x) — safe HOF, frame stays alive
            //   (mov r4 r0)      ; WITHOUT this kill: r4 gets stale closure alias
            //   (tail-call len 1); tail-call with r0 re-loaded from r4 → false tail_called
            //
            // Note: for TAIL_CALL the analysis is already done (escaped/tail_called
            // set above) and the frame no longer matters, so no kill needed there.
            if (inst.op == Opcode::CALL) {
              for (int a = 0; a < inst.argc; ++a) {
                aliases.erase(a);
                cell_aliases.erase(a);
              }
            }
            break;
          }

          // ---- default ------------------------------------------------------
          default:
            // Any instruction that writes rn1 kills aliases there.
            if (inst.rn1 >= 0) {
              aliases.erase(inst.rn1);
              cell_aliases.erase(inst.rn1);
            }
            break;
        }

        // If cell_aliases grew this iteration, the closure flowed through a heap cell.
        if (cell_aliases.size() > prev_cell_count) cell_aliases_used = true;
      }

      mk_inst.no_escape = !escaped;
      // stack_alloc requires no_escape AND:
      //   - closure never flowed through a heap cell (cell_aliases_used = false)
      //   - closure is not tail-called from the creating frame (tail_called = false)
      // Both would allow the closure to be accessed after the creating frame exits.
      mk_inst.stack_alloc = !escaped && !cell_aliases_used && !tail_called;
    }

    // ----------------------------------------------------------------
    // Second pass: propagate no_escape from MAKE_CLOSURE and stack_alloc
    // MAKE_CELL to store instructions so that emit_reg_cell_set /
    // emit_closure_set / emit_closure_cell_set can skip the write barrier.
    //
    // We do a single linear forward sweep per function (conservative:
    // no loop-back edges are considered, but the typical patterns are
    // straight-line code generated for let/letrec).  At each point we
    // maintain the set of registers that currently hold (directly or via
    // MOV) a value produced by a no_escape MAKE_CLOSURE, or a tagged
    // pointer to a stack-allocated cell (stack_alloc MAKE_CELL).
    // ----------------------------------------------------------------
    {
      std::unordered_set<int> no_escape_regs;
      // Registers holding a stack-allocated cell pointer (stack_alloc MAKE_CELL).
      std::unordered_set<int> stack_cell_regs;

      for (auto& inst : func.instructions) {
        switch (inst.op) {
          case Opcode::MAKE_CLOSURE:
            if (inst.no_escape && inst.rn1 >= 0)
              no_escape_regs.insert(inst.rn1);
            else if (inst.rn1 >= 0)
              no_escape_regs.erase(inst.rn1);
            // MAKE_CLOSURE never produces a cell pointer.
            if (inst.rn1 >= 0) stack_cell_regs.erase(inst.rn1);
            break;

          case Opcode::MAKE_CELL:
            // Stack-allocated cells: treat the cell pointer register as
            // no_escape so that writes through it skip the write barrier.
            if (inst.stack_alloc && inst.rn1 >= 0) {
              stack_cell_regs.insert(inst.rn1);
              no_escape_regs.erase(inst.rn1);  // cell ptr, not a closure value
            } else if (inst.rn1 >= 0) {
              stack_cell_regs.erase(inst.rn1);
              no_escape_regs.erase(inst.rn1);
            }
            break;

          case Opcode::MOV:
            // Propagate through copies.
            if (inst.rn1 >= 0) {
              if (inst.rn2 >= 0 && no_escape_regs.count(inst.rn2))
                no_escape_regs.insert(inst.rn1);
              else
                no_escape_regs.erase(inst.rn1);
              if (inst.rn2 >= 0 && stack_cell_regs.count(inst.rn2))
                stack_cell_regs.insert(inst.rn1);
              else
                stack_cell_regs.erase(inst.rn1);
            }
            break;

          case Opcode::REG_CELL_SET:
            // rn2 is the value being stored into the cell held by rn1.
            // The store is no_escape if the value is no_escape OR if the
            // cell itself is stack-allocated (rn1 in stack_cell_regs).
            inst.no_escape = (inst.rn2 >= 0 && no_escape_regs.count(inst.rn2)) || (inst.rn1 >= 0 && stack_cell_regs.count(inst.rn1));
            break;

          case Opcode::CLOSURE_SET:
            // rn2 is the value being stored into the current closure's env slot.
            inst.no_escape = (inst.rn2 >= 0 && no_escape_regs.count(inst.rn2));
            break;

          case Opcode::CLOSURE_CELL_SET:
            // rn2 is the value being stored into a cell slot of the current closure.
            inst.no_escape = (inst.rn2 >= 0 && no_escape_regs.count(inst.rn2));
            break;

          default:
            // Any instruction that writes rn1 kills the tracking for that register.
            if (inst.rn1 >= 0) {
              no_escape_regs.erase(inst.rn1);
              stack_cell_regs.erase(inst.rn1);
            }
            break;
        }
      }
    }
  }
}

// ============================================================================
//  Phase 2d: Stack-alloc analysis for MAKE_CELL instructions
// ============================================================================
//
// For each MAKE_CELL instruction, determine if the resulting cell object can
// be stack-allocated.  A cell is eligible (stack_alloc = true) when:
//   1. The cell pointer only flows into exactly ONE MAKE_CLOSURE instruction
//      that itself has stack_alloc = true.
//   2. The cell pointer never escapes anywhere else:
//        - not returned (RET with r0 == cell alias)
//        - not stored to a global (GLOBAL_SET)
//        - not passed to an unknown callee as an argument
//        - not captured by a non-stack_alloc closure
//
// The analysis is a single linear forward scan per function (conservative:
// no loop-back edges).  Only the cell pointer register is tracked; the cell's
// *contents* can be anything — the write barrier is elided by the phase2b
// no_escape propagation pass once stack_alloc is set here.
//
// Constraint: exactly one capturing stack_alloc closure ("exactly-one" rule).

void codegen_t::phase2d_analyze_cell_stack_alloc() {
  for (auto& func : functions) {
    const size_t n = func.instructions.size();

    for (size_t mk = 0; mk < n; ++mk) {
      auto& mk_inst = func.instructions[mk];
      if (mk_inst.op != Opcode::MAKE_CELL) continue;

      const int cell_reg = mk_inst.rn1;
      if (cell_reg < 0) continue;

      std::unordered_set<int> aliases;  // registers holding the cell pointer
      aliases.insert(cell_reg);

      bool escaped = false;
      int capturing_closures = 0;  // number of stack_alloc closures that captured this cell

      for (size_t i = mk + 1; i < n && !escaped; ++i) {
        const auto& inst = func.instructions[i];

        switch (inst.op) {
          // ---- RET ----------------------------------------------------------
          case Opcode::RET:
            if (aliases.count(0)) escaped = true;
            break;

          // ---- MOV ----------------------------------------------------------
          case Opcode::MOV:
            if (inst.rn1 >= 0) {
              if (inst.rn2 >= 0 && aliases.count(inst.rn2))
                aliases.insert(inst.rn1);
              else
                aliases.erase(inst.rn1);
            }
            break;

          // ---- MAKE_CELL ----------------------------------------------------
          case Opcode::MAKE_CELL:
            // A subsequent MAKE_CELL overwrites the same register — kill alias.
            if (inst.rn1 >= 0) aliases.erase(inst.rn1);
            break;

          // ---- GLOBAL_SET ---------------------------------------------------
          case Opcode::GLOBAL_SET:
            if (inst.rn1 >= 0 && aliases.count(inst.rn1)) escaped = true;
            break;

          // ---- MAKE_CLOSURE -------------------------------------------------
          case Opcode::MAKE_CLOSURE: {
            // Check if a cell alias is captured as a free variable.
            scm_obj_t fi = inst.free_indices;
            bool cell_captured = false;
            while (is_cons(fi)) {
              int r = parse_reg(cons_car(fi));
              if (r >= 0 && aliases.count(r)) {
                cell_captured = true;
                break;
              }
              fi = cons_cdr(fi);
            }
            if (cell_captured) {
              if (inst.stack_alloc) {
                ++capturing_closures;
                if (capturing_closures > 1) escaped = true;  // exactly-one rule
              } else {
                escaped = true;  // cell captured by a non-stack_alloc closure
              }
            }
            // Kill any alias overwritten by this instruction's destination.
            if (inst.rn1 >= 0) aliases.erase(inst.rn1);
            break;
          }

          // ---- CALL / TAIL_CALL -------------------------------------------
          case Opcode::CALL:
          case Opcode::TAIL_CALL:
            // If a cell alias appears as a call argument to an unknown callee,
            // the callee might store it and access it after the frame exits.
            for (int a = 0; a < inst.argc && !escaped; ++a) {
              if (aliases.count(a)) escaped = true;
            }
            // Cell alias in callee position: conservative escape.
            if (!escaped && inst.rn1 >= 0 && aliases.count(inst.rn1)) escaped = true;
            // Kill aliases in argument registers consumed by the call.
            if (inst.op == Opcode::CALL) {
              for (int a = 0; a < inst.argc; ++a) aliases.erase(a);
            }
            break;

          // ---- REG_CELL_SET / REG_CELL_REF ---------------------------------
          // These use the cell's contents, not the cell pointer itself.
          // They don't move the cell pointer to another register.
          case Opcode::REG_CELL_SET:
          case Opcode::REG_CELL_REF:
            break;

          // ---- default ------------------------------------------------------
          default:
            // Any instruction that writes rn1 kills the alias there.
            if (inst.rn1 >= 0) aliases.erase(inst.rn1);
            break;
        }
      }

      // Safe to stack-allocate iff:
      //   - cell didn't escape, AND
      //   - exactly one stack_alloc closure captured it.
      mk_inst.stack_alloc = !escaped && (capturing_closures == 1);
    }
  }
}

void codegen_t::dump_instructions(const std::vector<Instruction>& instructions) {
  std::string tid_suffix = std::to_string(std::hash<std::thread::id>{}(std::this_thread::get_id()));
  std::ofstream ofs("/tmp/nanos_" + tid_suffix + ".ins", std::ios::app);
  if (!ofs.is_open()) return;
  printer_t printer(ofs);
  for (const auto& inst : instructions) {
    if (inst.original == scm_nil) {
      ofs << "(safepoint)";
    } else {
      printer.write(inst.original);
    }
    if (inst.closure_label != scm_nil) {
      ofs << " ; closure_label: ";
      printer.write(inst.closure_label);
    }
    if (inst.op == Opcode::MAKE_CLOSURE) {
      ofs << " ; no_escape: " << (inst.no_escape ? "true" : "false") << " ; stack_alloc: " << (inst.stack_alloc ? " true" : "false");
    }
    if (inst.op == Opcode::MAKE_CELL) {
      ofs << " ; stack_alloc: " << (inst.stack_alloc ? "true" : "false");
    }
    if (inst.op == Opcode::CLOSURE_SET || inst.op == Opcode::CLOSURE_CELL_SET || inst.op == Opcode::REG_CELL_SET) {
      ofs << " ; no_escape: " << (inst.no_escape ? "true" : "false");
    }
    ofs << "\n";
  }
}
