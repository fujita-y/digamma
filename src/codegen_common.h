// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef CODEGEN_COMMON_H_INCLUDED
#define CODEGEN_COMMON_H_INCLUDED

#include "core.h"
#include "object.h"
#include <llvm/IR/IRBuilder.h>


// Constants
static constexpr int BRIDGE_MAX_ARGS = 8;
static constexpr int HEAP_OBJECT_TAG_OFFSET = 2;  // Offset to untag heap objects
static constexpr char CLOSURE_LABEL_PREFIX = 'C';

static constexpr int CELL_VALUE_FIELD_OFFSET = offsetof(scm_cell_rec_t, value);
static constexpr int CLOSURE_CODE_FIELD_OFFSET = offsetof(scm_closure_rec_t, code);
static constexpr int CLOSURE_ARGC_FIELD_OFFSET = offsetof(scm_closure_rec_t, argc);
static constexpr int CLOSURE_REST_FIELD_OFFSET = offsetof(scm_closure_rec_t, rest);
static constexpr int CLOSURE_CDECL_FIELD_OFFSET = offsetof(scm_closure_rec_t, cdecl);
static constexpr int CLOSURE_NENV_FIELD_OFFSET = offsetof(scm_closure_rec_t, nenv);
static constexpr int CLOSURE_ENV_FIELD_OFFSET = offsetof(scm_closure_rec_t, env);

static constexpr llvm::CallingConv::ID CLOSURE_CALLING_CONV = llvm::CallingConv::Tail;

// Helper function to get nth element of a list (1-indexed, like car/cdr convention)
static inline scm_obj_t list_nth(scm_obj_t list, int n) {
  if (n <= 0) return scm_nil;
  scm_obj_t curr = list;
  while (--n > 0 && is_cons(curr)) {
    curr = cons_cdr(curr);
  }
  return is_cons(curr) ? cons_car(curr) : scm_nil;
}

// Helper function for instruction operand access (1-indexed: operand 1 is first after opcode)
static inline scm_obj_t operand(scm_obj_t inst, int n) { return list_nth(cons_cdr(inst), n); }

// Helper function to check if a label is a closure label
static inline bool is_closure_label(scm_obj_t label) {
  if (!is_symbol(label)) return false;
  const char* label_name = (const char*)symbol_name(label);
  return label_name[0] == CLOSURE_LABEL_PREFIX;
}

// Helper function to count list length
static inline int count_list_length(scm_obj_t list) {
  int count = 0;
  scm_obj_t curr = list;
  while (is_cons(curr)) {
    count++;
    curr = cons_cdr(curr);
  }
  return count;
}

static inline int parse_reg(scm_obj_t s) {
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
  return env_start_ptr;
}

// Helper to get pointer to cell's value field
static inline llvm::Value* getCellValuePtr(llvm::IRBuilder<>& builder, llvm::LLVMContext& ctx, llvm::Value* cell_tagged, llvm::Type* int64Type) {
  llvm::Value* cell_ptr = untagPointer(builder, ctx, cell_tagged);
  llvm::Value* value_ptr = builder.CreateConstInBoundsGEP1_32(builder.getInt8Ty(), cell_ptr, CELL_VALUE_FIELD_OFFSET);
  return value_ptr;
}

#endif  // CODEGEN_COMMON_H_INCLUDED
