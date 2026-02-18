// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "codegen.h"
#include "codegen_aux.h"
#include "object_heap.h"
#include "printer.h"

#include <cstddef>
#include <fstream>

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

thread_local codegen_t* codegen_t::s_current;

codegen_t::codegen_t(llvm::orc::ThreadSafeContext ts_ctx, llvm::orc::LLJIT* jit)
    : ts_context(std::move(ts_ctx)), context(*ts_context.getContext()), builder(context), jit(jit) {
  cached_symbol_label = make_symbol("label");
  cached_symbol_apply = make_symbol("apply");
  object_heap_t::current()->add_root(cached_symbol_label);
  object_heap_t::current()->add_root(cached_symbol_apply);
  init_opcode_map();
  s_current = this;
}

extern "C" scm_obj_t c_make_closure(void* code, int argc, int rest, int nsize, scm_obj_t env[], scm_obj_t literals);
extern "C" void c_global_set(scm_obj_t key, scm_obj_t value);
extern "C" scm_obj_t c_make_cons(scm_obj_t car, scm_obj_t cdr);

extern "C" scm_obj_t c_make_cell(scm_obj_t value);
extern "C" scm_obj_t c_construct_rest_list(int count, intptr_t argv[]);
extern "C" void c_write_barrier(scm_obj_t obj);

// Constants
static constexpr int BRIDGE_MAX_ARGS = 10;
static constexpr int HEAP_OBJECT_TAG_OFFSET = 2;  // Offset to untag heap objects
static constexpr char CLOSURE_LABEL_PREFIX = 'C';

static constexpr int CELL_VALUE_FIELD_OFFSET = offsetof(scm_cell_rec_t, value);
static constexpr int CLOSURE_LITERALS_FIELD_OFFSET = offsetof(scm_closure_rec_t, literals);
static constexpr int CLOSURE_CODE_FIELD_OFFSET = offsetof(scm_closure_rec_t, code);
static constexpr int CLOSURE_ARGC_FIELD_OFFSET = offsetof(scm_closure_rec_t, argc);
static constexpr int CLOSURE_REST_FIELD_OFFSET = offsetof(scm_closure_rec_t, rest);
static constexpr int CLOSURE_CDECL_FIELD_OFFSET = offsetof(scm_closure_rec_t, cdecl);
static constexpr int CLOSURE_NSIZE_FIELD_OFFSET = offsetof(scm_closure_rec_t, nsize);
static constexpr int CLOSURE_ENV_FIELD_OFFSET = offsetof(scm_closure_rec_t, env);

#if LLVM_VERSION_MAJOR >= 19
static constexpr llvm::CallingConv::ID closure_calling_conv = llvm::CallingConv::Tail;
// need work to match param by adding undef
// static constexpr llvm::CallingConv::ID closure_calling_conv = llvm::CallingConv::PreserveNone;
#else
static constexpr llvm::CallingConv::ID closure_calling_conv = llvm::CallingConv::Tail;
#endif

// Helper macros for cons access
#define CAR(x) (((scm_cons_rec_t*)(x))->car)
#define CDR(x) (((scm_cons_rec_t*)(x))->cdr)

// Helper function to get nth element of a list (1-indexed, like car/cdr convention)
static inline scm_obj_t list_nth(scm_obj_t list, int n) {
  if (n <= 0) return scm_nil;
  scm_obj_t curr = list;
  while (--n > 0 && is_cons(curr)) {
    curr = CDR(curr);
  }
  return is_cons(curr) ? CAR(curr) : scm_nil;
}

// Helper function for instruction operand access (1-indexed: operand 1 is first after opcode)
static inline scm_obj_t operand(scm_obj_t inst, int n) { return list_nth(CDR(inst), n); }

// Helper function to check if a label is a closure label
static bool is_closure_label(scm_obj_t label) {
  if (!is_symbol(label)) return false;
  const char* label_name = (const char*)symbol_name(label);
  return label_name[0] == CLOSURE_LABEL_PREFIX;
}

// Helper function to count list length
static int count_list_length(scm_obj_t list) {
  int count = 0;
  scm_obj_t curr = list;
  while (is_cons(curr)) {
    count++;
    curr = CDR(curr);
  }
  return count;
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

// Helper function to create 64-bit integer constant
static inline llvm::Value* createInt64Constant(llvm::LLVMContext& ctx, uint64_t val) {
  return llvm::ConstantInt::get(ctx, llvm::APInt(64, val));
}

// Helper function to create 32-bit integer constant
static inline llvm::Value* createInt32Constant(llvm::LLVMContext& ctx, int32_t val) { return llvm::ConstantInt::get(ctx, llvm::APInt(32, val)); }

// Helper function to update max register index
static inline void updateMaxRegister(int reg_idx, int& max_reg) {
  if (reg_idx > max_reg) max_reg = reg_idx;
}

// ===== LLVM IR Helper Functions =====

// Helper to untag a heap object pointer (converts tagged scm_obj_t to raw pointer)
static inline llvm::Value* untagPointer(llvm::IRBuilder<>& builder, llvm::LLVMContext& ctx, llvm::Value* tagged_obj) {
  llvm::Value* untagged_int = builder.CreateSub(tagged_obj, createInt64Constant(ctx, HEAP_OBJECT_TAG_OFFSET));
  return builder.CreateIntToPtr(untagged_int, llvm::PointerType::get(ctx, 0));
}

// Helper to get pointer to closure's environment array
static inline llvm::Value* getClosureEnvArrayPtr(llvm::IRBuilder<>& builder, llvm::LLVMContext& ctx, llvm::Value* closure_tagged,
                                                 llvm::Type* int64Type) {
  llvm::Value* closure_ptr = untagPointer(builder, ctx, closure_tagged);
  llvm::Value* env_start_ptr = builder.CreateConstInBoundsGEP1_32(builder.getInt8Ty(), closure_ptr, CLOSURE_ENV_FIELD_OFFSET);
  return builder.CreateBitCast(env_start_ptr, llvm::PointerType::get(int64Type, 0));
}

// Helper to get pointer to cell's value field
static inline llvm::Value* getCellValuePtr(llvm::IRBuilder<>& builder, llvm::LLVMContext& ctx, llvm::Value* cell_tagged, llvm::Type* int64Type) {
  llvm::Value* cell_ptr = untagPointer(builder, ctx, cell_tagged);
  llvm::Value* value_ptr = builder.CreateConstInBoundsGEP1_32(builder.getInt8Ty(), cell_ptr, CELL_VALUE_FIELD_OFFSET);
  return builder.CreateBitCast(value_ptr, llvm::PointerType::get(int64Type, 0));
}

// Helper to finish collecting closure literals
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

// Initialize opcode map for faster lookup
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
  object_heap_t* heap = object_heap_t::current();
  for (const auto& pair : opcode_map) {
    heap->add_root(pair.first);
  }
}

// Helper to parse individual instructions
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

// Helper to parse a single instruction
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
    default:
      fatal("%s:%u codegen: unknown opcode %ld", __FILE__, __LINE__, inst.op);
      break;
  }

  func_info.instructions.push_back(inst);
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

  // First pass: identify closure parameters to effectively switch contexts
  // However, we process linearly. When we see a LABEL that corresponds to a closure
  // (which we only know if we have seen MAKE-CLOSURE or if we pre-scan), it's tricky.
  // BUT: The compiler emits code such that a closure body STARTS with a label.
  // And that label is unique.
  // We can collect closure params during parsing, but we need to know IF a label starts a new function.
  // Standard scheme practice: top level code first, then closure bodies.
  // Closures always start with a unique label. We can check `is_closure_label`.

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

llvm::Value* codegen_t::get_reg(int idx) {
  if (idx < 0 || (size_t)idx >= allocas.size()) {
    fatal("%s:%u codegen: register index out of bounds: r%d (max: r%lu) in function %s", __FILE__, __LINE__, idx, allocas.size() - 1,
          current_function ? current_function->getName().str().c_str() : "unknown");
  }
  return builder.CreateLoad(this->getInt64Type(), allocas[idx]);
}

void codegen_t::set_reg(int idx, llvm::Value* val) {
  if (idx < 0 || (size_t)idx >= allocas.size()) {
    fatal("%s:%u codegen: register index out of bounds: r%d (max: r%lu) in function %s", __FILE__, __LINE__, idx, allocas.size() - 1,
          current_function ? current_function->getName().str().c_str() : "unknown");
  }
  builder.CreateStore(val, allocas[idx]);
}

// Helper to get or create an external function declaration
llvm::Function* codegen_t::get_or_create_external_function(const char* name, llvm::FunctionType* type, void* symbol_ptr) {
  llvm::Function* func = module->getFunction(name);
  if (!func) {
    func = llvm::Function::Create(type, llvm::Function::ExternalLinkage, name, module);
  }
  // Register the symbol with the JIT's main dylib via absoluteSymbols
  llvm::orc::SymbolMap symbols;
  symbols[jit->mangleAndIntern(name)] = {llvm::orc::ExecutorAddr::fromPtr(symbol_ptr),
                                         llvm::JITSymbolFlags::Exported | llvm::JITSymbolFlags::Callable};
  if (auto err = jit->getMainJITDylib().define(llvm::orc::absoluteSymbols(std::move(symbols)))) {
    llvm::consumeError(std::move(err));  // Symbol may already be defined from a previous compile
  }
  return func;
}

// Helper to get code pointer from closure object
llvm::Value* codegen_t::getClosureCodePtr(llvm::Value* closure_tagged) {
  llvm::Value* closure_ptr = untagPointer(builder, context, closure_tagged);
  llvm::Value* code_field_ptr = builder.CreateConstInBoundsGEP1_32(builder.getInt8Ty(), closure_ptr, CLOSURE_CODE_FIELD_OFFSET);
  return builder.CreateLoad(this->getVoidPtrType(), code_field_ptr, "code_ptr");
}

// Helper to emit write barrier call
void codegen_t::emitWriteBarrier(llvm::Value* value) {
  llvm::Type* intptrTy = this->getInt64Type();
  llvm::Type* voidTy = llvm::Type::getVoidTy(context);
  std::vector<llvm::Type*> wbArgTypes = {intptrTy};
  llvm::FunctionType* wbFT = llvm::FunctionType::get(voidTy, wbArgTypes, false);
  llvm::Function* wb_func = get_or_create_external_function("c_write_barrier", wbFT, (void*)&c_write_barrier);
  builder.CreateCall(wb_func, {value});
}

// Redefining helper to take count
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
    tmpBuilder.CreateStore(createInt64Constant(context, 0), allocas[i]);
  }
}

// Setup rest arguments for a closure with extra arguments
// This creates a Scheme list from extra arguments beyond the fixed parameters
void codegen_t::setup_closure_rest_arguments(int fixed_argc, llvm::Value* actual_argc, llvm::Value* argv_ptr) {
  // Calculate number of rest arguments: actual_argc - fixed_argc
  llvm::Value* fixed_argc_val = createInt64Constant(context, fixed_argc);
  llvm::Value* rest_count = builder.CreateSub(actual_argc, fixed_argc_val, "rest_count");

  // Get c_construct_rest_list helper function
  // This C function builds a Scheme list from an array of arguments
  llvm::Type* intptrTy = this->getInt64Type();
  llvm::Type* intptrPtrTy = this->getInt64PtrType();
  llvm::Type* int32Ty = this->getInt32Type();

  // Ensure count uses int32 for C call
  llvm::Value* count_i32 = builder.CreateTrunc(rest_count, int32Ty);

  // Function type for helper: (i32, intptr_t*) -> intptr_t
  std::vector<llvm::Type*> helperArgTypes = {int32Ty, intptrPtrTy};
  llvm::FunctionType* helperFT = llvm::FunctionType::get(intptrTy, helperArgTypes, false);
  llvm::Function* helper_func = get_or_create_external_function("c_construct_rest_list", helperFT, (void*)&c_construct_rest_list);

  // Calculate pointer to argv[fixed_argc] (start of rest arguments)
  llvm::Value* rest_argv_ptr = builder.CreateGEP(intptrTy, argv_ptr, fixed_argc_val, "rest_argv_ptr");

  // Call helper to construct the rest list
  llvm::Value* rest_list = builder.CreateCall(helper_func, {count_i32, rest_argv_ptr}, "rest_list");

  // Store rest list to register[fixed_argc]
  // In Scheme, if a function has signature (lambda (a b . rest) ...),
  // then 'a' is in r0, 'b' is in r1, and 'rest' is in r2
  set_reg(fixed_argc, rest_list);
}

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
              if (is_closure(val)) {
                current_state.regs[inst.rn1] = inst.opr2;
                closure_params[inst.opr2] = {closure_argc(val), closure_rest(val) == 1};
              } else {
                current_state.regs[inst.rn1] = scm_nil;
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
  std::ofstream ofs("/tmp/nanous.ins", std::ios::app);
  if (!ofs.is_open()) return;
  printer_t printer(ofs);
  for (const auto& inst : instructions) {
    printer.print(inst.original);
    if (inst.closure_label != scm_nil) {
      ofs << " ; closure_label: ";
      printer.print(inst.closure_label);
    }
    ofs << "\n";
  }
}

intptr_t codegen_t::compile(scm_obj_t inst_list) {
  phase0_create_module();
  phase1_parse_instructions(inst_list);
  analyze_closure_labels();
  // Clear the file before dumping all functions
  {
    std::ofstream ofs("/tmp/nanous.ins", std::ios::trunc);
  }
  for (const auto& func : functions) {
    dump_instructions(func.instructions);
  }
  phase2_create_functions();
  phase3_generate_code();
  phase4_optimize_and_verify();
  return phase5_finalize();
}

void codegen_t::phase0_create_module() {
  // ===== Phase 0: Module Creation =====
  // Create a new LLVM module for this compilation unit
  std::string mod_name = "jit_module_" + std::to_string(std::rand());
  this->current_module_uptr = std::make_unique<llvm::Module>(mod_name, context);
  this->module = this->current_module_uptr.get();
}

void codegen_t::phase1_parse_instructions(scm_obj_t inst_list) {
  // ===== Phase 1: Instruction Parsing =====
  // Parse the instruction list into our internal representation
  parse_instructions(inst_list);

  labels.clear();
  function_map.clear();
}

void codegen_t::phase2_create_functions() {
  // ===== Phase 2: Function and BasicBlock Creation =====
  // Create the main function and all closure functions
  // The first function in 'functions' is always the main entry point.
  if (functions.empty()) return;

  // 1. Create Main Function
  {
    FunctionInfo& main_info = functions[0];
    llvm::FunctionType* funcType = llvm::FunctionType::get(this->getInt64Type(), false);
    std::string func_name = "scheme_func_" + std::to_string(std::rand());
    this->main_function = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, func_name, module);
    if (!this->main_function) {
      fatal("%s:%u codegen: failed to create main function", __FILE__, __LINE__);
    }
    main_info.llvm_function = this->main_function;

    // Create generic entry block needed for allocas
    llvm::BasicBlock::Create(context, "entry", this->main_function);
  }

  // 2. Create Closure Functions
  for (size_t i = 1; i < functions.size(); ++i) {
    FunctionInfo& info = functions[i];
    if (info.label == scm_nil) continue;

    std::string func_name = std::string((const char*)symbol_name(info.label)) + "_" + std::to_string(std::rand());

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
    llvm::Function* closure_func = llvm::Function::Create(closureFuncType, llvm::Function::InternalLinkage, func_name, module);
    closure_func->setCallingConv(closure_calling_conv);
    closure_func->setDSOLocal(true);

    info.llvm_function = closure_func;
    function_map[info.label] = closure_func;

    llvm::BasicBlock::Create(context, "entry", closure_func);
  }

  // 3. Create BasicBlocks for all labels within each function
  for (auto& info : functions) {
    llvm::Function* llvm_func = info.llvm_function;
    for (const auto& inst : info.instructions) {
      if (inst.op == Opcode::LABEL) {
        // Only create blocks for internal jump targets, but technically all labels get blocks.
        // Note: Closure entry labels are also labels in the instruction stream.
        // We should check if we already have a block (entry block)?
        // Actually, existing logic:
        // if (function_map.count(inst.opr1)) current_scan_func = function_map[inst.opr1];
        // The label op itself might be the start of the function.
        // If it is the start, we might skip creating a NEW block if we use the entry block?
        // But usually entry block is separate for allocas.
        // Let's just create blocks.
        const char* label_str = (const char*)symbol_name(inst.opr1);
        labels[inst.opr1] = llvm::BasicBlock::Create(context, label_str, llvm_func);
      }
    }
  }
}

void codegen_t::phase3_generate_code() {
  // ===== Phase 3: Code Generation =====
  // Generate LLVM IR for all functions

  for (auto& info : functions) {
    current_function = info.llvm_function;
    current_function_info = &info;

    // Set insert point to entry block
    llvm::BasicBlock& entry = current_function->getEntryBlock();
    builder.SetInsertPoint(&entry);

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
          llvm::Value* arg_ptr = builder.CreateGEP(this->getInt64Type(), argv_ptr, createInt64Constant(context, i));
          llvm::Value* arg_val = builder.CreateLoad(this->getInt64Type(), arg_ptr);
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
      // Skip the label instruction if it marks the start of this function to avoid
      // redundant branching/blocks if handled effectively, or just let standard emit_label handle it.
      // Standard emit_label will create a branch from Entry->FirstLabel. This is fine.
      emit_inst(inst);
    }
  }
}

void codegen_t::phase4_optimize_and_verify() {
  // ===== Phase 4: Verification and Optimization =====
  // Verify correctness and optimize the generated LLVM IR
  // Setup analysis managers
  llvm::LoopAnalysisManager LAM;
  llvm::FunctionAnalysisManager FAM;
  llvm::CGSCCAnalysisManager CGAM;
  llvm::ModuleAnalysisManager MAM;

  // Create pass builder and register analysis managers
  llvm::PassBuilder PB;
  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  // Build O2 optimization pipeline
  llvm::ModulePassManager MPM = PB.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O2);

  // Verify all functions first
  for (auto const& [label, func] : function_map) {
    if (llvm::verifyFunction(*func, &llvm::errs())) {
      fatal("%s:%u codegen: LLVM function verification failed for: %s", __FILE__, __LINE__, func->getName().str().c_str());
    }
  }
  if (llvm::verifyFunction(*main_function, &llvm::errs())) {
    fatal("%s:%u codegen: LLVM function verification failed for: %s", __FILE__, __LINE__, main_function->getName().str().c_str());
  }

  // Run optimization passes on entire module
  MPM.run(*module, MAM);

  // Dump IR to file for debugging and inspection
  std::error_code EC;
  llvm::raw_fd_ostream dest("/tmp/nanos.ll", EC, llvm::sys::fs::OF_None);
  if (EC) {
    llvm::errs() << "Could not open file: " << EC.message() << "\n";
  } else {
    module->print(dest, nullptr);
  }
}

intptr_t codegen_t::phase5_finalize() {
  // Transfer module to LLJIT
  std::string main_func_name = main_function->getName().str();
  auto tsm = llvm::orc::ThreadSafeModule(std::move(current_module_uptr), ts_context);
  if (auto err = jit->addIRModule(std::move(tsm))) {
    fatal("%s:%u codegen: failed to add module to JIT: %s", __FILE__, __LINE__, llvm::toString(std::move(err)).c_str());
  }

  // Look up the compiled function
  auto sym = jit->lookup(main_func_name);
  if (!sym) {
    fatal("%s:%u codegen: failed to look up compiled function: %s", __FILE__, __LINE__, llvm::toString(sym.takeError()).c_str());
  }

  // Clear local module pointers
  this->module = nullptr;
  this->main_function = nullptr;

  auto func = sym->toPtr<intptr_t()>();
  return func();
}

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
    default:
      fatal("%s:%u codegen: unknown opcode encountered during emission", __FILE__, __LINE__);
      break;
  }
}

// Emit a constant value to a register
void codegen_t::emit_const(const Instruction& inst) {
  if (inst.rn1 < 0) return;
  uint64_t val = (uint64_t)inst.opr1;
  llvm::Value* v = createInt64Constant(context, val);
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
  llvm::Value* cmp = builder.CreateICmpNE(cond, scm_false_v, "cond");
  builder.CreateCondBr(cmp, b1, b2);
}

// Unconditional jump to label
void codegen_t::emit_jump(const Instruction& inst) {
  llvm::BasicBlock* target = labels[inst.opr1];
  if (!target) {
    fatal("%s:%u codegen: jump target label not found", __FILE__, __LINE__);
  }
  builder.CreateBr(target);
}

// Set insertion point to label's basic block
void codegen_t::emit_label(const Instruction& inst) {
  llvm::BasicBlock* block = labels[inst.opr1];
  if (!block) {
    fatal("%s:%u codegen: label basic block not found", __FILE__, __LINE__);
  }
  llvm::BasicBlock* current = builder.GetInsertBlock();

  if (current->getTerminator() == nullptr) {
    builder.CreateBr(block);
  }

  builder.SetInsertPoint(block);
}

// Return r0 as function result
void codegen_t::emit_ret(const Instruction& inst) { builder.CreateRet(get_reg(0)); }

// Create a closure object with captured environment
void codegen_t::emit_make_closure(const Instruction& inst) {
  llvm::Function* closure_func = function_map[inst.opr1];
  if (!closure_func) {
    fatal("%s:%u codegen: closure function not found for label", __FILE__, __LINE__);
  }

  // Count free variables
  int nsize = count_list_length(inst.free_indices);

  // Get literals vector if available
  scm_obj_t literals = scm_nil;
  if (closure_literals.count(inst.opr1)) {
    literals = closure_literals[inst.opr1];
  }
  llvm::Value* literals_val = createInt64Constant(context, (uint64_t)literals);

  // Common types
  llvm::Type* intptrTy = this->getInt64Type();
  llvm::Type* voidPtrTy = this->getVoidPtrType();
  llvm::Type* int32Ty = this->getInt32Type();

  // Prepare common arguments
  llvm::Value* code_ptr = builder.CreateBitCast(closure_func, voidPtrTy);
  llvm::Value* argc = createInt32Constant(context, inst.argc);

  llvm::Value* closure;
  if (!inst.has_rest && nsize == 0) {
    if (literals == scm_nil) {
      // Simple closure optimization (no literals)
      std::vector<llvm::Type*> simpleArgTypes = {voidPtrTy, int32Ty};
      llvm::FunctionType* simpleFT = llvm::FunctionType::get(intptrTy, simpleArgTypes, false);
      llvm::Function* make_simple_closure_func = get_or_create_external_function("c_make_closure_s1", simpleFT, (void*)&c_make_closure_s1);
      closure = builder.CreateCall(make_simple_closure_func, {code_ptr, argc}, "closure");
    } else {
      // Simple closure optimization (with literals)
      std::vector<llvm::Type*> simpleArgTypes = {voidPtrTy, int32Ty, intptrTy};
      llvm::FunctionType* simpleFT = llvm::FunctionType::get(intptrTy, simpleArgTypes, false);
      llvm::Function* make_simple_closure_func = get_or_create_external_function("c_make_closure_s2", simpleFT, (void*)&c_make_closure_s2);
      closure = builder.CreateCall(make_simple_closure_func, {code_ptr, argc, literals_val}, "closure");
    }
  } else {
    // General case: Prepare environment array
    llvm::Value* env_array = nullptr;
    if (nsize > 0) {
      env_array = builder.CreateAlloca(intptrTy, createInt32Constant(context, nsize), "env");
      scm_obj_t curr = inst.free_indices;
      for (int i = 0; i < nsize; i++) {
        int reg_idx = parse_reg(CAR(curr));
        llvm::Value* reg_val = get_reg(reg_idx);
        llvm::Value* ptr = builder.CreateGEP(builder.getInt64Ty(), env_array, createInt32Constant(context, i));
        builder.CreateStore(reg_val, ptr);
        curr = CDR(curr);
      }
    } else {
      env_array = llvm::ConstantPointerNull::get(llvm::PointerType::get(context, 0));
    }

    llvm::FunctionType* ft = llvm::FunctionType::get(intptrTy, {voidPtrTy, int32Ty, int32Ty, int32Ty, voidPtrTy, intptrTy}, false);
    llvm::Function* make_closure_func = get_or_create_external_function("c_make_closure", ft, (void*)&c_make_closure);

    llvm::Value* rest = createInt32Constant(context, inst.has_rest ? 1 : 0);
    llvm::Value* nsize_val = createInt32Constant(context, nsize);
    llvm::Value* env_ptr = builder.CreateBitCast(env_array, voidPtrTy);

    std::vector<llvm::Value*> callArgs = {code_ptr, argc, rest, nsize_val, env_ptr, literals_val};
    closure = builder.CreateCall(make_closure_func, callArgs, "closure");
  }

  set_reg(inst.rn1, closure);
}

// Set global variable to value in register
void codegen_t::emit_global_set(const Instruction& inst) {
  // Get or create c_global_set external function
  llvm::Type* voidTy = llvm::Type::getVoidTy(context);
  llvm::Type* intptrTy = this->getInt64Type();
  std::vector<llvm::Type*> argTypes = {intptrTy, intptrTy};
  llvm::FunctionType* ft = llvm::FunctionType::get(voidTy, argTypes, false);
  llvm::Function* global_set_func = get_or_create_external_function("c_global_set", ft, (void*)&c_global_set);

  // Prepare arguments: symbol key and value from register
  llvm::Value* key_v = createInt64Constant(context, (uint64_t)inst.opr1);

  if (inst.rn1 < 0) {
    fatal("%s:%u codegen: global-set! missing register operand", __FILE__, __LINE__);
  }
  llvm::Value* val_v = get_reg(inst.rn1);

  std::vector<llvm::Value*> args = {key_v, val_v};
  builder.CreateCall(global_set_func, args);
}

// Load global variable value into register
void codegen_t::emit_global_ref(const Instruction& inst) {
  // Resolve the address of the global variable's value slot at compile time
  scm_obj_t cell = object_heap_t::current()->environment_variable_cell_ref(inst.opr2);
  scm_cell_rec_t* rec = (scm_cell_rec_t*)to_address(cell);

  // Create a constant for the value address
  llvm::Value* value_address = createInt64Constant(context, (uint64_t)&(rec->value));

  if (inst.rn1 < 0) {
    fatal("%s:%u codegen: global-ref missing register operand", __FILE__, __LINE__);
  }

  // Get pointer to cell's value
  llvm::Value* value_ptr = builder.CreateIntToPtr(value_address, llvm::PointerType::get(this->getInt64Type(), 0));

  // Load the value directly
  llvm::Value* val = builder.CreateLoad(this->getInt64Type(), value_ptr, "gref_val");

  set_reg(inst.rn1, val);
}

llvm::Function* codegen_t::get_or_create_call_closure_bridge() {
  const char* name = "__nanos_call_closure_bridge";
  llvm::Function* f = module->getFunction(name);
  if (f) return f;

  // Signature: i64 (i64 closure, i64 argc, i64* argv)
  llvm::Type* i64 = this->getInt64Type();
  llvm::Type* i64_ptr = this->getInt64PtrType();
  llvm::FunctionType* ft = llvm::FunctionType::get(i64, {i64, i64, i64_ptr}, false);

  // Check if the bridge is already defined in the JIT
  if (auto sym = jit->lookup(name)) {
    // Symbol exists in JIT, just provide external declaration in this module
    return llvm::Function::Create(ft, llvm::Function::ExternalLinkage, name, module);
  } else {
    // Consume the error (symbol not found)
    llvm::consumeError(sym.takeError());
  }

  f = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, name, module);

  llvm::BasicBlock* saved_block = builder.GetInsertBlock();
  llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", f);
  builder.SetInsertPoint(entry);

  auto args = f->arg_begin();
  llvm::Value* closure = args++;
  closure->setName("closure");
  llvm::Value* argc = args++;
  argc->setName("argc");
  llvm::Value* argv = args++;
  argv->setName("argv");

  llvm::Value* code_ptr = getClosureCodePtr(closure);
  llvm::Value* closure_ptr = untagPointer(builder, context, closure);

  // Load rest and cdecl flags
  llvm::Value* rest_ptr = builder.CreateConstInBoundsGEP1_32(builder.getInt8Ty(), closure_ptr, CLOSURE_REST_FIELD_OFFSET);
  llvm::Value* rest_field =
      builder.CreateLoad(builder.getInt32Ty(), builder.CreateBitCast(rest_ptr, llvm::PointerType::get(builder.getInt32Ty(), 0)), "rest");
  llvm::Value* is_rest = builder.CreateICmpNE(rest_field, builder.getInt32(0), "is_rest");

  llvm::Value* cdecl_ptr = builder.CreateConstInBoundsGEP1_32(builder.getInt8Ty(), closure_ptr, CLOSURE_CDECL_FIELD_OFFSET);
  llvm::Value* cdecl_field =
      builder.CreateLoad(builder.getInt32Ty(), builder.CreateBitCast(cdecl_ptr, llvm::PointerType::get(builder.getInt32Ty(), 0)), "cdecl");
  llvm::Value* is_cdecl = builder.CreateICmpNE(cdecl_field, builder.getInt32(0), "is_cdecl");

  llvm::BasicBlock* rest_b = llvm::BasicBlock::Create(context, "rest", f);
  llvm::BasicBlock* fixed_b = llvm::BasicBlock::Create(context, "fixed", f);
  builder.CreateCondBr(is_rest, rest_b, fixed_b);

  // Rest path: (self, argc, argv[])
  builder.SetInsertPoint(rest_b);
  {
    llvm::FunctionType* rest_ft = llvm::FunctionType::get(i64, {i64, i64, i64_ptr}, false);
    llvm::Value* fp = builder.CreateBitCast(code_ptr, llvm::PointerType::get(rest_ft, 0));

    llvm::BasicBlock* cdecl_path = llvm::BasicBlock::Create(context, "rest_cdecl", f);
    llvm::BasicBlock* scheme_path = llvm::BasicBlock::Create(context, "rest_scheme", f);
    builder.CreateCondBr(is_cdecl, cdecl_path, scheme_path);

    builder.SetInsertPoint(cdecl_path);
    auto call_c = builder.CreateCall(rest_ft, fp, {closure, argc, argv});
    call_c->setCallingConv(llvm::CallingConv::C);
    builder.CreateRet(call_c);

    builder.SetInsertPoint(scheme_path);
    auto call_s = builder.CreateCall(rest_ft, fp, {closure, argc, argv});
    call_s->setCallingConv(closure_calling_conv);
    builder.CreateRet(call_s);
  }

  // Fixed path: (self, arg0, arg1, ...)
  builder.SetInsertPoint(fixed_b);
  {
    llvm::BasicBlock* def_b = llvm::BasicBlock::Create(context, "fixed_def", f);
    llvm::SwitchInst* sw = builder.CreateSwitch(argc, def_b, BRIDGE_MAX_ARGS + 1);
    for (int n = 0; n <= BRIDGE_MAX_ARGS; ++n) {
      llvm::BasicBlock* bb = llvm::BasicBlock::Create(context, "fixed_" + std::to_string(n), f);
      sw->addCase(llvm::cast<llvm::ConstantInt>(createInt64Constant(context, (uint64_t)n)), bb);
      builder.SetInsertPoint(bb);

      std::vector<llvm::Type*> pts(n + 1, i64);
      llvm::FunctionType* fixed_ft = llvm::FunctionType::get(i64, pts, false);
      llvm::Value* fp = builder.CreateBitCast(code_ptr, llvm::PointerType::get(fixed_ft, 0));

      std::vector<llvm::Value*> call_args;
      call_args.push_back(closure);
      for (int i = 0; i < n; ++i) {
        llvm::Value* p = builder.CreateGEP(i64, argv, createInt32Constant(context, i));
        call_args.push_back(builder.CreateLoad(i64, p));
      }

      llvm::BasicBlock* cdecl_p = llvm::BasicBlock::Create(context, "fixed_cdecl_" + std::to_string(n), f);
      llvm::BasicBlock* scheme_p = llvm::BasicBlock::Create(context, "fixed_scheme_" + std::to_string(n), f);
      builder.CreateCondBr(is_cdecl, cdecl_p, scheme_p);

      builder.SetInsertPoint(cdecl_p);
      auto c = builder.CreateCall(fixed_ft, fp, call_args);
      c->setCallingConv(llvm::CallingConv::C);
      builder.CreateRet(c);

      builder.SetInsertPoint(scheme_p);
      auto s = builder.CreateCall(fixed_ft, fp, call_args);
      s->setCallingConv(closure_calling_conv);
      builder.CreateRet(s);
    }
    builder.SetInsertPoint(def_b);
    builder.CreateRet(getScmFalseValue());
  }

  if (saved_block) builder.SetInsertPoint(saved_block);
  return f;
}

void* codegen_t::get_call_closure_bridge_ptr() {
  const char* name = "__nanos_call_closure_bridge";
  auto sym = jit->lookup(name);
  if (sym) return (void*)sym->getValue();

  // If the bridge is not in the JIT, we need to compile it.
  // We must save the current module state because this might be called during another compilation or at runtime.
  auto saved_module_uptr = std::move(current_module_uptr);
  auto* saved_module = module;
  auto* saved_main_func = main_function;
  auto saved_builder_ip = builder.saveIP();

  // Create a temporary module for the bridge
  current_module_uptr = std::make_unique<llvm::Module>("bridge_module", context);
  module = current_module_uptr.get();
  module->setDataLayout(jit->getDataLayout());

  (void)get_or_create_call_closure_bridge();

  // Finalize the temporary module
  auto tsm = llvm::orc::ThreadSafeModule(std::move(current_module_uptr), ts_context);
  if (auto err = jit->addIRModule(std::move(tsm))) {
    fatal("%s:%u codegen: failed to add bridge module to JIT: %s", __FILE__, __LINE__, llvm::toString(std::move(err)).c_str());
  }

  // Restore previous state
  current_module_uptr = std::move(saved_module_uptr);
  module = saved_module;
  main_function = saved_main_func;
  builder.restoreIP(saved_builder_ip);

  // Re-lookup the symbol
  sym = jit->lookup(name);
  if (!sym) {
    fatal("%s:%u codegen: failed to look up closure bridge after compilation", __FILE__, __LINE__);
  }
  return (void*)sym->getValue();
}

void codegen_t::emit_apply_call(const Instruction& inst, bool is_tail) {
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
  llvm::Value* argc_val = createInt32Constant(context, inst.argc - 1);
  llvm::Value* argv_array = nullptr;
  if (inst.argc > 1) {
    argv_array = builder.CreateAlloca(i64, createInt32Constant(context, inst.argc - 1), "apply_argv");
    for (int i = 1; i < inst.argc; i++) {
      llvm::Value* p = builder.CreateGEP(i64, argv_array, createInt32Constant(context, i - 1));
      builder.CreateStore(get_reg(i), p);
    }
  } else {
    argv_array = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(i64_ptr));
  }

  auto call = builder.CreateCall(ft, apply_helper, {proc, argc_val, argv_array}, "apply_opt");
  if (is_tail) {
    builder.CreateRet(call);
  } else {
    set_reg(0, call);
  }
}

void codegen_t::emit_known_closure_call(const Instruction& inst, bool is_tail) {
  // Check if it is a global closure (known at compile time but not in this module's function_map)
  if (inst.closure_label != scm_nil && function_map.find(inst.closure_label) == function_map.end()) {
    // Attempt to resolve it as a global closure
    if (closure_params.find(inst.closure_label) != closure_params.end()) {
      auto [fixed_argc, has_rest] = closure_params[inst.closure_label];

      // Retrieve the actual closure object to get code pointer and cdecl
      scm_obj_t val = object_heap_t::current()->environment_variable_ref(inst.closure_label);
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
          args.push_back(createInt64Constant(context, inst.argc));
          llvm::Value* argv_array = nullptr;
          if (inst.argc > 0) {
            argv_array = builder.CreateAlloca(this->getInt64Type(), createInt32Constant(context, inst.argc), "argv");
            for (int i = 0; i < inst.argc; i++) {
              llvm::Value* arg_ptr = builder.CreateGEP(this->getInt64Type(), argv_array, createInt32Constant(context, i));
              builder.CreateStore(get_reg(i), arg_ptr);
            }
          } else {
            argv_array = llvm::ConstantPointerNull::get(llvm::PointerType::get(this->getInt64Type(), 0));
          }
          args.push_back(argv_array);
        } else {
          if (inst.argc != fixed_argc) {
            // Mismatch in argument count for fixed-arity function -> fallback to runtime error
            emit_generic_closure_call(inst, is_tail);
            return;
          }
          for (int i = 0; i < inst.argc; i++) {
            args.push_back(get_reg(i));
          }
        }

        // Create function pointer
        llvm::Value* funcPtr = createInt64Constant(context, (uint64_t)code_ptr);
        llvm::Value* typedFuncPtr = builder.CreateIntToPtr(funcPtr, llvm::PointerType::get(funcType, 0));

        // Emit call
        // If cdecl == 0 (Scheme), use tailcc. If cdecl == 1 (C), use generic ccc.
        llvm::CallInst* call = builder.CreateCall(funcType, typedFuncPtr, args, is_tail ? "tail_call_global" : "call_global");

        if (cdecl == 0) {
          call->setCallingConv(closure_calling_conv);
          if (is_tail) {
            call->setTailCallKind(llvm::CallInst::TCK_MustTail);
            builder.CreateRet(call);
          } else {
            set_reg(0, call);
          }
        } else {
          // cdecl == 1: C calling convention (default)
          call->setCallingConv(llvm::CallingConv::C);
          if (is_tail) {
            builder.CreateRet(call);
          } else {
            set_reg(0, call);
          }
        }
        return;
      }
    }
  }

  if (inst.closure_label != scm_nil && function_map.count(inst.closure_label)) {
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
      // Target expects: (self, argc, argv[])
      // We need to pack our register arguments into an array

      args.push_back(createInt64Constant(context, inst.argc));

      llvm::Value* argv_array = nullptr;
      if (inst.argc > 0) {
        argv_array = builder.CreateAlloca(this->getInt64Type(), createInt32Constant(context, inst.argc), "argv");
        for (int i = 0; i < inst.argc; i++) {
          llvm::Value* arg_ptr = builder.CreateGEP(this->getInt64Type(), argv_array, createInt32Constant(context, i));
          builder.CreateStore(get_reg(i), arg_ptr);
        }
      } else {
        argv_array = llvm::ConstantPointerNull::get(llvm::PointerType::get(this->getInt64Type(), 0));
      }
      args.push_back(argv_array);

    } else {
      // Target expects: (self, arg0, arg1, ...)
      // Pass arguments directly
      // Note: target function has fixed_argc + 1 parameters (including self).
      // The call instruction has inst.argc arguments.
      // If inst.argc != fixed_argc, fall back to generic call to let runtime handle error.
      if (inst.argc != fixed_argc) {
        emit_generic_closure_call(inst, is_tail);
        return;
      }

      for (int i = 0; i < inst.argc; i++) {
        args.push_back(get_reg(i));
      }
    }

    llvm::CallInst* call = builder.CreateCall(target_func, args, is_tail ? "tail_call_opt" : "call_opt");
    call->setCallingConv(closure_calling_conv);

    if (is_tail) {
      call->setTailCallKind(llvm::CallInst::TCK_MustTail);
      builder.CreateRet(call);
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
  paramTypes.push_back(this->getInt64Type());                             // self
  paramTypes.push_back(this->getInt64Type());                             // argc
  paramTypes.push_back(llvm::PointerType::get(this->getInt64Type(), 0));  // argv[]
  llvm::FunctionType* funcType = llvm::FunctionType::get(this->getInt64Type(), paramTypes, false);

  // Allocate argv array and populate it
  llvm::Value* argv_array = nullptr;
  if (inst.argc > 0) {
    argv_array = builder.CreateAlloca(this->getInt64Type(), createInt32Constant(context, inst.argc), "argv");
    for (int i = 0; i < inst.argc; i++) {
      llvm::Value* arg_ptr = builder.CreateGEP(this->getInt64Type(), argv_array, createInt32Constant(context, i));
      builder.CreateStore(get_reg(i), arg_ptr);
    }
  } else {
    argv_array = llvm::ConstantPointerNull::get(llvm::PointerType::get(this->getInt64Type(), 0));
  }

  std::vector<llvm::Value*> args;
  args.push_back(closure);
  args.push_back(createInt64Constant(context, inst.argc));
  args.push_back(argv_array);

  llvm::Value* func_ptr = builder.CreateBitCast(code_void_ptr, llvm::PointerType::get(funcType, 0));

  // Handle CDECL vs SCHEME (Tail) calling convention
  llvm::BasicBlock* cdecl_block = llvm::BasicBlock::Create(context, "rest_cdecl", current_function);
  llvm::BasicBlock* scheme_block = llvm::BasicBlock::Create(context, "rest_scheme", current_function);
  llvm::BasicBlock* local_merge = nullptr;

  if (!is_tail) {
    local_merge = llvm::BasicBlock::Create(context, "rest_local_merge", current_function);
  }

  // Check cdecl flag
  builder.CreateCondBr(is_cdecl, cdecl_block, scheme_block);

  // CDECL path - no musttail
  builder.SetInsertPoint(cdecl_block);
  llvm::CallInst* call_c = builder.CreateCall(funcType, func_ptr, args, "rest_call_c");
  call_c->setCallingConv(llvm::CallingConv::C);
  if (is_tail) {
    builder.CreateRet(call_c);
  } else {
    builder.CreateBr(local_merge);
  }

  // Scheme path - musttail if tail
  builder.SetInsertPoint(scheme_block);
  llvm::CallInst* call_s = builder.CreateCall(funcType, func_ptr, args, "rest_call_s");
  call_s->setCallingConv(closure_calling_conv);
  if (is_tail) {
    call_s->setTailCallKind(llvm::CallInst::TCK_MustTail);
    builder.CreateRet(call_s);
  } else {
    builder.CreateBr(local_merge);
  }

  if (!is_tail) {
    builder.SetInsertPoint(local_merge);
    llvm::PHINode* phi = builder.CreatePHI(this->getInt64Type(), 2, "rest_res");
    phi->addIncoming(call_c, cdecl_block);
    phi->addIncoming(call_s, scheme_block);

    // Explicitly transition to merge_block
    builder.CreateBr(merge_block);

    // The "result" of this block is the phi node, which we need to access from outside
    // But since we are restructuring, we can't easily return it.
    // Instead, we can add it to the merge phi node directly if we pass the merge phi node?
    // Or we structure it so `emit_generic_closure_call` handles the phi.
    // Let's rely on `rest_exit_block` being set to `local_merge` (or the block ending there)
    // and let the caller inspect the terminator or just use `phi`.
    // Actually, `rest_exit_block` = `builder.GetInsertBlock()` at end.
    // But we need the value.
    // Let's store the result in a temporary instruction? No, that's messy.
    // We can return the Value*.
    // Better: Helper returns Value*.
  }
  rest_exit_block = builder.GetInsertBlock();
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

  llvm::Value* normal_func_ptr = builder.CreateBitCast(code_void_ptr, llvm::PointerType::get(normalFuncType, 0));

  // Handle CDECL vs SCHEME (Tail) calling convention
  llvm::BasicBlock* cdecl_block = llvm::BasicBlock::Create(context, "norm_cdecl", current_function);
  llvm::BasicBlock* scheme_block = llvm::BasicBlock::Create(context, "norm_scheme", current_function);
  llvm::BasicBlock* local_merge = nullptr;

  if (!is_tail) {
    local_merge = llvm::BasicBlock::Create(context, "norm_local_merge", current_function);
  }

  // Check cdecl flag
  builder.CreateCondBr(is_cdecl, cdecl_block, scheme_block);

  // CDECL path - no musttail
  builder.SetInsertPoint(cdecl_block);
  llvm::CallInst* call_c = builder.CreateCall(normalFuncType, normal_func_ptr, normalArgs, "norm_call_c");
  call_c->setCallingConv(llvm::CallingConv::C);
  if (is_tail) {
    builder.CreateRet(call_c);
  } else {
    builder.CreateBr(local_merge);
  }

  // Scheme path - musttail if tail
  builder.SetInsertPoint(scheme_block);
  llvm::CallInst* call_s = builder.CreateCall(normalFuncType, normal_func_ptr, normalArgs, "norm_call_s");
  call_s->setCallingConv(closure_calling_conv);
  if (is_tail) {
    call_s->setTailCallKind(llvm::CallInst::TCK_MustTail);
    builder.CreateRet(call_s);
  } else {
    builder.CreateBr(local_merge);
  }

  if (!is_tail) {
    builder.SetInsertPoint(local_merge);
    llvm::PHINode* phi = builder.CreatePHI(this->getInt64Type(), 2, "norm_res");
    phi->addIncoming(call_c, cdecl_block);
    phi->addIncoming(call_s, scheme_block);
    builder.CreateBr(merge_block);
  }
  normal_exit_block = builder.GetInsertBlock();
}

void codegen_t::emit_generic_closure_call(const Instruction& inst, bool is_tail) {
  if (inst.argc <= BRIDGE_MAX_ARGS) {
    // Optimized generic call via bridge
    llvm::Function* bridge = get_or_create_call_closure_bridge();
    llvm::Type* i64 = this->getInt64Type();
    llvm::Type* i64_ptr = this->getInt64PtrType();

    // Prepare arguments for bridge: i64 (i64 closure, i64 argc, i64* argv)
    llvm::Value* closure = get_reg(inst.rn1);
    llvm::Value* argc_val = createInt64Constant(context, inst.argc);
    llvm::Value* argv_array = nullptr;
    if (inst.argc > 0) {
      argv_array = builder.CreateAlloca(i64, createInt32Constant(context, inst.argc), "bridge_argv");
      for (int i = 0; i < inst.argc; i++) {
        llvm::Value* p = builder.CreateGEP(i64, argv_array, createInt32Constant(context, i));
        builder.CreateStore(get_reg(i), p);
      }
    } else {
      argv_array = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(i64_ptr));
    }

    auto call = builder.CreateCall(bridge->getFunctionType(), bridge, {closure, argc_val, argv_array}, "bridge_call");
    // The bridge itself handles the calling convention (Scheme/CDECL) and musttail optimization internally.
    // However, the call TO the bridge is a standard C call.
    if (is_tail) {
      builder.CreateRet(call);
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
  llvm::Value* closure_ptr = untagPointer(builder, context, closure);
  llvm::Value* rest_field_ptr = builder.CreateConstInBoundsGEP1_32(builder.getInt8Ty(), closure_ptr, CLOSURE_REST_FIELD_OFFSET);
  llvm::Value* rest_field_ptr_i32 = builder.CreateBitCast(rest_field_ptr, llvm::PointerType::get(builder.getInt32Ty(), 0));
  llvm::Value* rest_flag = builder.CreateLoad(builder.getInt32Ty(), rest_field_ptr_i32, "rest");

  // Load cdecl field (already defined at top of file)
  llvm::Value* cdecl_field_ptr = builder.CreateConstInBoundsGEP1_32(builder.getInt8Ty(), closure_ptr, CLOSURE_CDECL_FIELD_OFFSET);
  llvm::Value* cdecl_field_ptr_i32 = builder.CreateBitCast(cdecl_field_ptr, llvm::PointerType::get(builder.getInt32Ty(), 0));
  llvm::Value* cdecl_flag = builder.CreateLoad(builder.getInt32Ty(), cdecl_field_ptr_i32, "cdecl");

  // Branch based on rest flag
  llvm::Value* is_rest = builder.CreateICmpNE(rest_flag, builder.getInt32(0), "is_rest");
  llvm::Value* is_cdecl = builder.CreateICmpNE(cdecl_flag, builder.getInt32(0), "is_cdecl");

  llvm::BasicBlock* rest_block = llvm::BasicBlock::Create(context, is_tail ? "rest_tail_call" : "rest_call", current_function);
  llvm::BasicBlock* normal_block = llvm::BasicBlock::Create(context, is_tail ? "normal_tail_call" : "normal_call", current_function);
  llvm::BasicBlock* merge_block = nullptr;
  if (!is_tail) {
    merge_block = llvm::BasicBlock::Create(context, "call_merge", current_function);
  }

  llvm::MDBuilder MDB(context);
  auto branch_weights = MDB.createBranchWeights(1, 2000);  // 1 = unlikely (rest), 2000 = likely (normal)
  builder.CreateCondBr(is_rest, rest_block, normal_block, branch_weights);

  llvm::BasicBlock* rest_exit_block = nullptr;
  llvm::BasicBlock* normal_exit_block = nullptr;

  // --- Rest Block ---
  builder.SetInsertPoint(rest_block);
  emit_generic_rest_call(closure, code_void_ptr, is_cdecl, inst, is_tail, merge_block, rest_exit_block);

  // --- Normal Block ---
  builder.SetInsertPoint(normal_block);
  emit_generic_normal_call(closure, code_void_ptr, is_cdecl, inst, is_tail, merge_block, normal_exit_block);

  // --- Merge Block ---
  if (!is_tail) {
    builder.SetInsertPoint(merge_block);
    llvm::PHINode* phi = builder.CreatePHI(this->getInt64Type(), 2, "call_result");

    // We need to get the result from the exit blocks.
    // But wait, `emit_generic_rest_call` created a PHI in `local_merge` (which is `rest_exit_block`).
    // So we can just take the value from there?
    // Actually, `rest_exit_block` ends with a `Br merge_block`.
    // The value we want is the PHI node in `rest_exit_block`.
    // We can assume the first instruction in `rest_exit_block` (which is `local_merge` in the helper) is the PHI.

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

void codegen_t::emit_call_common(const Instruction& inst, bool is_tail) {
  if (inst.closure_label == cached_symbol_apply) {
    emit_apply_call(inst, is_tail);
    return;
  }

  // Check if it's a known closure (global or local) call optimization
  if ((inst.closure_label != scm_nil && function_map.find(inst.closure_label) == function_map.end() &&
       closure_params.count(inst.closure_label)) ||
      (inst.closure_label != scm_nil && function_map.count(inst.closure_label))) {
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
    fatal("%s:%u codegen: closure-ref used outside of closure context", __FILE__, __LINE__);
  }

  int env_idx = (int)fixnum(inst.opr2);
  if (env_idx < 0) {
    fatal("%s:%u codegen: closure-ref invalid index: %d", __FILE__, __LINE__, env_idx);
  }

  // Get pointer to closure environment array
  llvm::Value* env_array_ptr = getClosureEnvArrayPtr(builder, context, current_closure_self, this->getInt64Type());

  // Get pointer to specific index
  llvm::Value* val_ptr = builder.CreateGEP(this->getInt64Type(), env_array_ptr, createInt32Constant(context, env_idx));

  // Load the value
  llvm::Value* val = builder.CreateLoad(this->getInt64Type(), val_ptr, "free_var");

  set_reg(inst.rn1, val);
}

// Store value into closure environment
void codegen_t::emit_closure_set(const Instruction& inst) {
  if (!current_closure_self) {
    fatal("%s:%u codegen: closure-set! used outside of closure context", __FILE__, __LINE__);
  }

  int env_idx = (int)fixnum(inst.opr1);
  if (env_idx < 0) {
    fatal("%s:%u codegen: closure-set! invalid index: %d", __FILE__, __LINE__, env_idx);
  }

  // Get pointer to closure environment array
  llvm::Value* env_array_ptr = getClosureEnvArrayPtr(builder, context, current_closure_self, this->getInt64Type());

  // Get pointer to specific index
  llvm::Value* val_ptr = builder.CreateGEP(this->getInt64Type(), env_array_ptr, createInt32Constant(context, env_idx));

  // Get value from source register
  if (inst.rn2 < 0) {
    fatal("%s:%u codegen: closure-set! missing source register", __FILE__, __LINE__);
  }
  llvm::Value* val = get_reg(inst.rn2);

  // Store the value
  builder.CreateStore(val, val_ptr);

  // Write barrier
  emitWriteBarrier(val);
}

// Load current closure object into register
void codegen_t::emit_closure_self(const Instruction& inst) {
  if (!current_closure_self) {
    fatal("%s:%u codegen: closure-self used outside of closure context", __FILE__, __LINE__);
  }
  set_reg(inst.rn1, current_closure_self);
}

// Load free variable (cell) from closure environment and unbox it
void codegen_t::emit_closure_cell_ref(const Instruction& inst) {
  if (!current_closure_self) {
    fatal("%s:%u codegen: closure-cell-ref used outside of closure context", __FILE__, __LINE__);
  }

  int env_idx = (int)fixnum(inst.opr2);
  if (env_idx < 0) {
    fatal("%s:%u codegen: closure-cell-ref invalid index: %d", __FILE__, __LINE__, env_idx);
  }

  // Get pointer to closure environment array
  llvm::Value* env_array_ptr = getClosureEnvArrayPtr(builder, context, current_closure_self, this->getInt64Type());
  llvm::Value* val_ptr = builder.CreateGEP(this->getInt64Type(), env_array_ptr, createInt32Constant(context, env_idx));

  // Load the cell object (which is a tagged pointer)
  llvm::Value* cell_obj = builder.CreateLoad(this->getInt64Type(), val_ptr, "cell_obj");

  // Get pointer to cell's value and load it
  llvm::Value* value_ptr_typed = getCellValuePtr(builder, context, cell_obj, this->getInt64Type());
  llvm::Value* val = builder.CreateLoad(this->getInt64Type(), value_ptr_typed, "cell_val");

  set_reg(inst.rn1, val);
}

// Load free variable (cell) from closure environment and update its value
void codegen_t::emit_closure_cell_set(const Instruction& inst) {
  if (!current_closure_self) {
    fatal("%s:%u codegen: closure-cell-set! used outside of closure context", __FILE__, __LINE__);
  }

  int env_idx = (int)fixnum(inst.opr1);
  if (env_idx < 0) {
    fatal("%s:%u codegen: closure-cell-set! invalid index: %d", __FILE__, __LINE__, env_idx);
  }

  // Get pointer to closure environment array
  llvm::Value* env_array_ptr = getClosureEnvArrayPtr(builder, context, current_closure_self, this->getInt64Type());
  llvm::Value* val_ptr = builder.CreateGEP(this->getInt64Type(), env_array_ptr, createInt32Constant(context, env_idx));

  // Load the cell object
  llvm::Value* cell_obj = builder.CreateLoad(this->getInt64Type(), val_ptr, "cell_obj");

  // Get pointer to cell's value
  llvm::Value* value_ptr_typed = getCellValuePtr(builder, context, cell_obj, this->getInt64Type());

  // Get value from source register
  if (inst.rn2 < 0) {
    fatal("%s:%u codegen: closure-cell-set! missing source register", __FILE__, __LINE__);
  }
  llvm::Value* val = get_reg(inst.rn2);

  // Store the value
  builder.CreateStore(val, value_ptr_typed);

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
  llvm::Value* value_ptr_typed = getCellValuePtr(builder, context, cell_obj, this->getInt64Type());
  llvm::Value* val = builder.CreateLoad(this->getInt64Type(), value_ptr_typed, "cell_val");

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
  llvm::Value* value_ptr_typed = getCellValuePtr(builder, context, cell_obj, this->getInt64Type());

  // Get value from source register
  if (inst.rn2 < 0) {
    fatal("%s:%u codegen: reg-cell-set! missing source register", __FILE__, __LINE__);
  }
  llvm::Value* val = get_reg(inst.rn2);

  // Store value
  builder.CreateStore(val, value_ptr_typed);

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
  llvm::Value* cell = builder.CreateCall(make_cell_func, {val}, "cell");

  // Store result back to register
  set_reg(inst.rn1, cell);
}
