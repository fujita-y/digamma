// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "arch_arm64.h"
#include "codegen.h"
#include "codegen_aux.h"
#include "codegen_common.h"

#include <llvm/IR/MDBuilder.h>

#define BL (*builder)
#define CT (*context_uptr)

void codegen_t::emit_null_p_subr(bool is_tail) {
  llvm::Value* arg = get_reg(0);
  llvm::Value* scm_nil_val = createInt64Constant(CT, (uint64_t)scm_nil);
  llvm::Value* is_pred_cmp = BL.CreateICmpEQ(arg, scm_nil_val, "is_pred_cmp");
  llvm::Value* scm_true_val = createInt64Constant(CT, (uint64_t)scm_true);
  llvm::Value* scm_false_val = createInt64Constant(CT, (uint64_t)scm_false);
  llvm::Value* is_pred_res = BL.CreateSelect(is_pred_cmp, scm_true_val, scm_false_val, "is_pred_res");
  if (is_tail) {
    BL.CreateRet(is_pred_res);
  } else {
    set_reg(0, is_pred_res);
  }
}

void codegen_t::emit_pair_p_subr(bool is_tail) {
  llvm::Value* arg = get_reg(0);
  llvm::Value* mask_val = createInt64Constant(CT, 0x07);
  llvm::Value* masked_arg = BL.CreateAnd(arg, mask_val, "masked_arg");
  llvm::Value* zero_val = createInt64Constant(CT, 0x00);
  llvm::Value* is_pred_cmp = BL.CreateICmpEQ(masked_arg, zero_val, "is_pred_cmp");
  llvm::Value* scm_true_val = createInt64Constant(CT, (uint64_t)scm_true);
  llvm::Value* scm_false_val = createInt64Constant(CT, (uint64_t)scm_false);
  llvm::Value* is_pred_res = BL.CreateSelect(is_pred_cmp, scm_true_val, scm_false_val, "is_pred_res");
  if (is_tail) {
    BL.CreateRet(is_pred_res);
  } else {
    set_reg(0, is_pred_res);
  }
}

void codegen_t::emit_not_subr(bool is_tail) {
  llvm::Value* arg = get_reg(0);
  llvm::Value* scm_false_val = createInt64Constant(CT, (uint64_t)scm_false);
  llvm::Value* is_false_cmp = BL.CreateICmpEQ(arg, scm_false_val, "is_false_cmp");
  llvm::Value* scm_true_val = createInt64Constant(CT, (uint64_t)scm_true);
  llvm::Value* is_not_res = BL.CreateSelect(is_false_cmp, scm_true_val, scm_false_val, "is_not_res");
  if (is_tail) {
    BL.CreateRet(is_not_res);
  } else {
    set_reg(0, is_not_res);
  }
}

void codegen_t::emit_car_subr(bool is_tail) {
  llvm::Value* arg = get_reg(0);

  llvm::Value* mask_val = createInt64Constant(CT, 0x07);
  llvm::Value* masked_arg = BL.CreateAnd(arg, mask_val, "masked_arg");
  llvm::Value* zero_val = createInt64Constant(CT, 0x00);
  llvm::Value* is_cons_cmp = BL.CreateICmpEQ(masked_arg, zero_val, "is_cons_cmp");

  llvm::Function* f = BL.GetInsertBlock()->getParent();
  llvm::BasicBlock* cons_bb = llvm::BasicBlock::Create(CT, "cons_bb", f);
  llvm::BasicBlock* err_bb = llvm::BasicBlock::Create(CT, "err_bb", f);
  llvm::BasicBlock* cont_bb = llvm::BasicBlock::Create(CT, "cont_bb", f);

  llvm::MDBuilder mdb(CT);
  llvm::MDNode* branch_weights = mdb.createBranchWeights(2000, 1);
  BL.CreateCondBr(is_cons_cmp, cons_bb, err_bb, branch_weights);

  BL.SetInsertPoint(cons_bb);
  llvm::Value* car_ptr = BL.CreateIntToPtr(arg, getInt64PtrType(), "car_ptr");
  llvm::Value* car_val = BL.CreateAlignedLoad(getInt64Type(), car_ptr, llvm::Align(8), "car_val");
  BL.CreateBr(cont_bb);

  BL.SetInsertPoint(err_bb);
  llvm::FunctionType* c_error_car_ft = llvm::FunctionType::get(llvm::Type::getVoidTy(CT), {getInt64Type()}, false);
  llvm::Function* c_error_car_func = get_or_create_external_function("c_error_car", c_error_car_ft, (void*)&c_error_car);
  BL.CreateCall(c_error_car_ft, c_error_car_func, {arg});
  BL.CreateUnreachable();

  BL.SetInsertPoint(cont_bb);
  if (is_tail) {
    BL.CreateRet(car_val);
  } else {
    set_reg(0, car_val);
  }
}

void codegen_t::emit_append2_subr(bool is_tail) {
  llvm::Value* arg1 = get_reg(0);
  llvm::Value* arg2 = get_reg(1);

  llvm::FunctionType* c_append2_ft = llvm::FunctionType::get(getInt64Type(), {getInt64Type(), getInt64Type()}, false);
  llvm::Function* c_append2_func = get_or_create_external_function("c_append2", c_append2_ft, (void*)&c_append2);
  llvm::Value* result = BL.CreateCall(c_append2_ft, c_append2_func, {arg1, arg2});
  if (is_tail) {
    BL.CreateRet(result);
  } else {
    set_reg(0, result);
  }
}

void codegen_t::emit_cdr_subr(bool is_tail) {
  llvm::Value* arg = get_reg(0);

  llvm::Value* mask_val = createInt64Constant(CT, 0x07);
  llvm::Value* masked_arg = BL.CreateAnd(arg, mask_val, "masked_arg");
  llvm::Value* zero_val = createInt64Constant(CT, 0x00);
  llvm::Value* is_cons_cmp = BL.CreateICmpEQ(masked_arg, zero_val, "is_cons_cmp");

  llvm::Function* f = BL.GetInsertBlock()->getParent();
  llvm::BasicBlock* cons_bb = llvm::BasicBlock::Create(CT, "cons_bb", f);
  llvm::BasicBlock* err_bb = llvm::BasicBlock::Create(CT, "err_bb", f);
  llvm::BasicBlock* cont_bb = llvm::BasicBlock::Create(CT, "cont_bb", f);

  llvm::MDBuilder mdb(CT);
  llvm::MDNode* branch_weights = mdb.createBranchWeights(2000, 1);
  BL.CreateCondBr(is_cons_cmp, cons_bb, err_bb, branch_weights);

  BL.SetInsertPoint(cons_bb);
  llvm::Value* car_ptr = BL.CreateIntToPtr(arg, getInt64PtrType(), "car_ptr");
  llvm::Value* cdr_ptr = BL.CreateConstInBoundsGEP1_32(getInt64Type(), car_ptr, 1, "cdr_ptr");
  llvm::Value* cdr_val = BL.CreateAlignedLoad(getInt64Type(), cdr_ptr, llvm::Align(8), "cdr_val");
  BL.CreateBr(cont_bb);

  BL.SetInsertPoint(err_bb);
  llvm::FunctionType* c_error_cdr_ft = llvm::FunctionType::get(llvm::Type::getVoidTy(CT), {getInt64Type()}, false);
  llvm::Function* c_error_cdr_func = get_or_create_external_function("c_error_cdr", c_error_cdr_ft, (void*)&c_error_cdr);
  BL.CreateCall(c_error_cdr_ft, c_error_cdr_func, {arg});
  BL.CreateUnreachable();

  BL.SetInsertPoint(cont_bb);
  if (is_tail) {
    BL.CreateRet(cdr_val);
  } else {
    set_reg(0, cdr_val);
  }
}

void codegen_t::emit_eq_p_subr(bool is_tail) {
  llvm::Value* arg1 = get_reg(0);
  llvm::Value* arg2 = get_reg(1);
  llvm::Value* is_pred_cmp = BL.CreateICmpEQ(arg1, arg2, "is_pred_cmp");
  llvm::Value* scm_true_val = createInt64Constant(CT, (uint64_t)scm_true);
  llvm::Value* scm_false_val = createInt64Constant(CT, (uint64_t)scm_false);
  llvm::Value* is_pred_res = BL.CreateSelect(is_pred_cmp, scm_true_val, scm_false_val, "is_pred_res");
  if (is_tail) {
    BL.CreateRet(is_pred_res);
  } else {
    set_reg(0, is_pred_res);
  }
}

void codegen_t::emit_tc6_predicate(int tc6_num, bool is_tail) {
  llvm::Value* arg = get_reg(0);
#if USE_TBI
  llvm::Value* rot = BL.CreateIntrinsic(llvm::Intrinsic::fshl, {BL.getInt64Ty()}, {arg, arg, createInt64Constant(CT, 7)}, nullptr, "rot");
  llvm::Value* mask = createInt64Constant(CT, 0x3bf);
  llvm::Value* masked = BL.CreateAnd(rot, mask, "masked");
  llvm::Value* expected = createInt64Constant(CT, 0x100 + tc6_num);
  llvm::Value* is_pred_cmp = BL.CreateICmpEQ(masked, expected, "is_pred_cmp");

  llvm::Value* scm_true_val = createInt64Constant(CT, (uint64_t)scm_true);
  llvm::Value* scm_false_val = createInt64Constant(CT, (uint64_t)scm_false);
  llvm::Value* is_pred_res = BL.CreateSelect(is_pred_cmp, scm_true_val, scm_false_val, "is_pred_res");
  if (is_tail) {
    BL.CreateRet(is_pred_res);
  } else {
    set_reg(0, is_pred_res);
  }
#else
  llvm::Function* f = BL.GetInsertBlock()->getParent();
  llvm::BasicBlock* check_tag_bb = llvm::BasicBlock::Create(CT, "check_tag", f);
  llvm::BasicBlock* cont_bb = llvm::BasicBlock::Create(CT, "cont", f);

  llvm::Value* mask_val = createInt64Constant(CT, 0x07);
  llvm::Value* masked_arg = BL.CreateAnd(arg, mask_val, "masked_arg");
  llvm::Value* expected_heap_tag = createInt64Constant(CT, 0x02);
  llvm::Value* is_heap_obj_cmp = BL.CreateICmpEQ(masked_arg, expected_heap_tag, "is_heap_obj_cmp");

  llvm::BasicBlock* orig_bb = BL.GetInsertBlock();
  BL.CreateCondBr(is_heap_obj_cmp, check_tag_bb, cont_bb);

  BL.SetInsertPoint(check_tag_bb);
  llvm::Value* untagged_ptr = untagPointer(BL, CT, arg);
  llvm::Value* tag_val = BL.CreateAlignedLoad(BL.getInt64Ty(), untagged_ptr, llvm::Align(8), "tag_val");
  llvm::Value* tag_mask = createInt64Constant(CT, 0x3f00);
  llvm::Value* masked_tag = BL.CreateAnd(tag_val, tag_mask, "masked_tag");
  llvm::Value* expected_tag = createInt64Constant(CT, tc6_num << 8);
  llvm::Value* tag_cmp = BL.CreateICmpEQ(masked_tag, expected_tag, "tag_cmp");
  BL.CreateBr(cont_bb);

  BL.SetInsertPoint(cont_bb);
  llvm::PHINode* phi = BL.CreatePHI(BL.getInt1Ty(), 2, "is_pred_cmp");
  phi->addIncoming(llvm::ConstantInt::getFalse(CT), orig_bb);
  phi->addIncoming(tag_cmp, check_tag_bb);

  llvm::Value* scm_true_val = createInt64Constant(CT, (uint64_t)scm_true);
  llvm::Value* scm_false_val = createInt64Constant(CT, (uint64_t)scm_false);
  llvm::Value* is_pred_res = BL.CreateSelect(phi, scm_true_val, scm_false_val, "is_pred_res");
  if (is_tail) {
    BL.CreateRet(is_pred_res);
  } else {
    set_reg(0, is_pred_res);
  }
#endif
}

void codegen_t::emit_num_add_subr(bool is_tail) {
  llvm::Value* arg1 = get_reg(0);
  llvm::Value* arg2 = get_reg(1);

  llvm::Value* args_and = BL.CreateAnd(arg1, arg2, "args_and");
  llvm::Value* mask = createInt64Constant(CT, 0x01);
  llvm::Value* args_mask = BL.CreateAnd(args_and, mask, "args_mask");
  llvm::Value* is_fixnum_cmp = BL.CreateICmpEQ(args_mask, mask, "is_fixnum_cmp");

  llvm::Function* f = BL.GetInsertBlock()->getParent();
  llvm::BasicBlock* add_bb = llvm::BasicBlock::Create(CT, "add_bb", f);
  llvm::BasicBlock* err_bb = llvm::BasicBlock::Create(CT, "err_bb", f);
  llvm::BasicBlock* cont_bb = llvm::BasicBlock::Create(CT, "cont_bb", f);

  llvm::MDBuilder mdb(CT);
  llvm::MDNode* branch_weights = mdb.createBranchWeights(2000, 1);  // 2000 = likely (add_bb), 1 = unlikely (err_bb)
  BL.CreateCondBr(is_fixnum_cmp, add_bb, err_bb, branch_weights);

  BL.SetInsertPoint(add_bb);
  // Using the formula: X + Y - 1 for fixnums ((a<<1)|1 + (b<<1)|1 - 1 = ((a+b)<<1)|1)
  llvm::Value* sum = BL.CreateAdd(arg1, arg2, "sum");
  llvm::Value* res = BL.CreateSub(sum, createInt64Constant(CT, 1), "res");
  BL.CreateBr(cont_bb);

  BL.SetInsertPoint(err_bb);
  llvm::Type* intptrTy = this->getInt64Type();
  llvm::FunctionType* c_num_add_ft = llvm::FunctionType::get(intptrTy, {intptrTy, intptrTy}, false);
  llvm::Function* c_num_add_func = get_or_create_external_function("c_num_add", c_num_add_ft, (void*)&c_num_add);
  llvm::CallInst* call_err = BL.CreateCall(c_num_add_ft, c_num_add_func, {arg1, arg2}, "call_err");
  BL.CreateBr(cont_bb);

  BL.SetInsertPoint(cont_bb);
  llvm::PHINode* phi = BL.CreatePHI(intptrTy, 2, "plus_res");
  phi->addIncoming(res, add_bb);
  phi->addIncoming(call_err, err_bb);

  if (is_tail) {
    BL.CreateRet(phi);
  } else {
    set_reg(0, phi);
  }
}

void codegen_t::emit_num_sub_subr(bool is_tail) {
  llvm::Value* arg1 = get_reg(0);
  llvm::Value* arg2 = get_reg(1);

  llvm::Value* args_and = BL.CreateAnd(arg1, arg2, "args_and");
  llvm::Value* mask = createInt64Constant(CT, 0x01);
  llvm::Value* args_mask = BL.CreateAnd(args_and, mask, "args_mask");
  llvm::Value* is_fixnum_cmp = BL.CreateICmpEQ(args_mask, mask, "is_fixnum_cmp");

  llvm::Function* f = BL.GetInsertBlock()->getParent();
  llvm::BasicBlock* sub_bb = llvm::BasicBlock::Create(CT, "sub_bb", f);
  llvm::BasicBlock* err_bb = llvm::BasicBlock::Create(CT, "err_bb", f);
  llvm::BasicBlock* cont_bb = llvm::BasicBlock::Create(CT, "cont_bb", f);

  llvm::MDBuilder mdb(CT);
  llvm::MDNode* branch_weights = mdb.createBranchWeights(2000, 1);  // 2000 = likely (sub_bb), 1 = unlikely (err_bb)
  BL.CreateCondBr(is_fixnum_cmp, sub_bb, err_bb, branch_weights);

  BL.SetInsertPoint(sub_bb);
  // Using the formula: X - Y + 1 for fixnums ((a<<1)|1 - (b<<1)|1 + 1 = ((a-b)<<1)|1)
  llvm::Value* diff = BL.CreateSub(arg1, arg2, "diff");
  llvm::Value* res = BL.CreateAdd(diff, createInt64Constant(CT, 1), "res");
  BL.CreateBr(cont_bb);

  BL.SetInsertPoint(err_bb);
  llvm::Type* intptrTy = this->getInt64Type();
  llvm::FunctionType* c_num_sub_ft = llvm::FunctionType::get(intptrTy, {intptrTy, intptrTy}, false);
  llvm::Function* c_num_sub_func = get_or_create_external_function("c_num_sub", c_num_sub_ft, (void*)&c_num_sub);
  llvm::CallInst* call_err = BL.CreateCall(c_num_sub_ft, c_num_sub_func, {arg1, arg2}, "call_err");
  BL.CreateBr(cont_bb);

  BL.SetInsertPoint(cont_bb);
  llvm::PHINode* phi = BL.CreatePHI(intptrTy, 2, "minus_res");
  phi->addIncoming(res, sub_bb);
  phi->addIncoming(call_err, err_bb);

  if (is_tail) {
    BL.CreateRet(phi);
  } else {
    set_reg(0, phi);
  }
}

void codegen_t::emit_num_mul_subr(bool is_tail) {
  llvm::Value* arg1 = get_reg(0);
  llvm::Value* arg2 = get_reg(1);

  llvm::Value* args_and = BL.CreateAnd(arg1, arg2, "args_and");
  llvm::Value* mask = createInt64Constant(CT, 0x01);
  llvm::Value* args_mask = BL.CreateAnd(args_and, mask, "args_mask");
  llvm::Value* is_fixnum_cmp = BL.CreateICmpEQ(args_mask, mask, "is_fixnum_cmp");

  llvm::Function* f = BL.GetInsertBlock()->getParent();
  llvm::BasicBlock* mul_bb = llvm::BasicBlock::Create(CT, "mul_bb", f);
  llvm::BasicBlock* err_bb = llvm::BasicBlock::Create(CT, "err_bb", f);
  llvm::BasicBlock* cont_bb = llvm::BasicBlock::Create(CT, "cont_bb", f);

  llvm::MDBuilder mdb(CT);
  llvm::MDNode* branch_weights = mdb.createBranchWeights(2000, 1);
  BL.CreateCondBr(is_fixnum_cmp, mul_bb, err_bb, branch_weights);

  BL.SetInsertPoint(mul_bb);
  // Using the formula: (X >> 1) * (Y - 1) + 1 for fixnums
  // ((2x+1) >> 1) * ((2y+1) - 1) + 1 = x * 2y + 1 = 2xy + 1
  llvm::Value* x_val = BL.CreateAShr(arg1, createInt64Constant(CT, 1), "x_val");
  llvm::Value* y_tagged_even = BL.CreateSub(arg2, createInt64Constant(CT, 1), "y_tagged_even");
  llvm::Value* prod = BL.CreateMul(x_val, y_tagged_even, "prod");
  llvm::Value* res = BL.CreateAdd(prod, createInt64Constant(CT, 1), "res");
  BL.CreateBr(cont_bb);

  BL.SetInsertPoint(err_bb);
  llvm::Type* intptrTy = this->getInt64Type();
  llvm::FunctionType* c_num_mul_ft = llvm::FunctionType::get(intptrTy, {intptrTy, intptrTy}, false);
  llvm::Function* c_num_mul_func = get_or_create_external_function("c_num_mul", c_num_mul_ft, (void*)&c_num_mul);
  llvm::Value* call_err = BL.CreateCall(c_num_mul_ft, c_num_mul_func, {arg1, arg2}, "call_err");
  BL.CreateBr(cont_bb);

  BL.SetInsertPoint(cont_bb);
  llvm::PHINode* phi = BL.CreatePHI(intptrTy, 2, "mul_res");
  phi->addIncoming(res, mul_bb);
  phi->addIncoming(call_err, err_bb);

  if (is_tail) {
    BL.CreateRet(phi);
  } else {
    set_reg(0, phi);
  }
}

void codegen_t::emit_num_eq_subr(bool is_tail) {
  llvm::Value* arg1 = get_reg(0);
  llvm::Value* arg2 = get_reg(1);

  llvm::Value* args_and = BL.CreateAnd(arg1, arg2, "args_and");
  llvm::Value* mask = createInt64Constant(CT, 0x01);
  llvm::Value* args_mask = BL.CreateAnd(args_and, mask, "args_mask");
  llvm::Value* is_fixnum_cmp = BL.CreateICmpEQ(args_mask, mask, "is_fixnum_cmp");

  llvm::Function* f = BL.GetInsertBlock()->getParent();
  llvm::BasicBlock* eq_bb = llvm::BasicBlock::Create(CT, "eq_bb", f);
  llvm::BasicBlock* err_bb = llvm::BasicBlock::Create(CT, "err_bb", f);
  llvm::BasicBlock* cont_bb = llvm::BasicBlock::Create(CT, "cont_bb", f);

  llvm::MDBuilder mdb(CT);
  llvm::MDNode* branch_weights = mdb.createBranchWeights(2000, 1);  // 2000 = likely (eq_bb), 1 = unlikely (err_bb)
  BL.CreateCondBr(is_fixnum_cmp, eq_bb, err_bb, branch_weights);

  BL.SetInsertPoint(eq_bb);
  llvm::Value* is_eq = BL.CreateICmpEQ(arg1, arg2, "is_eq");
  llvm::Value* scm_true_val = createInt64Constant(CT, (uint64_t)scm_true);
  llvm::Value* scm_false_val = createInt64Constant(CT, (uint64_t)scm_false);
  llvm::Value* eq_res = BL.CreateSelect(is_eq, scm_true_val, scm_false_val, "eq_res");
  BL.CreateBr(cont_bb);

  BL.SetInsertPoint(err_bb);
  llvm::Type* intptrTy = this->getInt64Type();
  llvm::FunctionType* c_num_eq_ft = llvm::FunctionType::get(intptrTy, {intptrTy, intptrTy}, false);
  llvm::Function* c_num_eq_func = get_or_create_external_function("c_num_eq", c_num_eq_ft, (void*)&c_num_eq);
  llvm::Value* call_err = BL.CreateCall(c_num_eq_ft, c_num_eq_func, {arg1, arg2}, "call_err");
  BL.CreateBr(cont_bb);

  BL.SetInsertPoint(cont_bb);
  llvm::PHINode* phi = BL.CreatePHI(intptrTy, 2, "equal_res");
  phi->addIncoming(eq_res, eq_bb);
  phi->addIncoming(call_err, err_bb);

  if (is_tail) {
    BL.CreateRet(phi);
  } else {
    set_reg(0, phi);
  }
}

void codegen_t::emit_num_lt_subr(bool is_tail) {
  llvm::Value* arg1 = get_reg(0);
  llvm::Value* arg2 = get_reg(1);

  llvm::Value* args_and = BL.CreateAnd(arg1, arg2, "args_and");
  llvm::Value* mask = createInt64Constant(CT, 0x01);
  llvm::Value* args_mask = BL.CreateAnd(args_and, mask, "args_mask");
  llvm::Value* is_fixnum_cmp = BL.CreateICmpEQ(args_mask, mask, "is_fixnum_cmp");

  llvm::Function* f = BL.GetInsertBlock()->getParent();
  llvm::BasicBlock* lt_bb = llvm::BasicBlock::Create(CT, "lt_bb", f);
  llvm::BasicBlock* err_bb = llvm::BasicBlock::Create(CT, "err_bb", f);
  llvm::BasicBlock* cont_bb = llvm::BasicBlock::Create(CT, "cont_bb", f);

  llvm::MDBuilder mdb(CT);
  llvm::MDNode* branch_weights = mdb.createBranchWeights(2000, 1);
  BL.CreateCondBr(is_fixnum_cmp, lt_bb, err_bb, branch_weights);

  BL.SetInsertPoint(lt_bb);
  llvm::Value* is_lt = BL.CreateICmpSLT(arg1, arg2, "is_lt");
  llvm::Value* scm_true_val = createInt64Constant(CT, (uint64_t)scm_true);
  llvm::Value* scm_false_val = createInt64Constant(CT, (uint64_t)scm_false);
  llvm::Value* lt_res = BL.CreateSelect(is_lt, scm_true_val, scm_false_val, "lt_res");
  BL.CreateBr(cont_bb);

  BL.SetInsertPoint(err_bb);
  llvm::Type* intptrTy = this->getInt64Type();
  llvm::FunctionType* c_num_lt_ft = llvm::FunctionType::get(intptrTy, {intptrTy, intptrTy}, false);
  llvm::Function* c_num_lt_func = get_or_create_external_function("c_num_lt", c_num_lt_ft, (void*)&c_num_lt);
  llvm::Value* call_err = BL.CreateCall(c_num_lt_ft, c_num_lt_func, {arg1, arg2}, "call_err");
  BL.CreateBr(cont_bb);

  BL.SetInsertPoint(cont_bb);
  llvm::PHINode* phi = BL.CreatePHI(intptrTy, 2, "lt_res_phi");
  phi->addIncoming(lt_res, lt_bb);
  phi->addIncoming(call_err, err_bb);

  if (is_tail) {
    BL.CreateRet(phi);
  } else {
    set_reg(0, phi);
  }
}

void codegen_t::emit_num_gt_subr(bool is_tail) {
  llvm::Value* arg1 = get_reg(0);
  llvm::Value* arg2 = get_reg(1);

  llvm::Value* args_and = BL.CreateAnd(arg1, arg2, "args_and");
  llvm::Value* mask = createInt64Constant(CT, 0x01);
  llvm::Value* args_mask = BL.CreateAnd(args_and, mask, "args_mask");
  llvm::Value* is_fixnum_cmp = BL.CreateICmpEQ(args_mask, mask, "is_fixnum_cmp");

  llvm::Function* f = BL.GetInsertBlock()->getParent();
  llvm::BasicBlock* gt_bb = llvm::BasicBlock::Create(CT, "gt_bb", f);
  llvm::BasicBlock* err_bb = llvm::BasicBlock::Create(CT, "err_bb", f);
  llvm::BasicBlock* cont_bb = llvm::BasicBlock::Create(CT, "cont_bb", f);

  llvm::MDBuilder mdb(CT);
  llvm::MDNode* branch_weights = mdb.createBranchWeights(2000, 1);
  BL.CreateCondBr(is_fixnum_cmp, gt_bb, err_bb, branch_weights);

  BL.SetInsertPoint(gt_bb);
  llvm::Value* is_gt = BL.CreateICmpSGT(arg1, arg2, "is_gt");
  llvm::Value* scm_true_val = createInt64Constant(CT, (uint64_t)scm_true);
  llvm::Value* scm_false_val = createInt64Constant(CT, (uint64_t)scm_false);
  llvm::Value* gt_res = BL.CreateSelect(is_gt, scm_true_val, scm_false_val, "gt_res");
  BL.CreateBr(cont_bb);

  BL.SetInsertPoint(err_bb);
  llvm::Type* intptrTy = this->getInt64Type();
  llvm::FunctionType* c_num_gt_ft = llvm::FunctionType::get(intptrTy, {intptrTy, intptrTy}, false);
  llvm::Function* c_num_gt_func = get_or_create_external_function("c_num_gt", c_num_gt_ft, (void*)&c_num_gt);
  llvm::Value* call_err = BL.CreateCall(c_num_gt_ft, c_num_gt_func, {arg1, arg2}, "call_err");
  BL.CreateBr(cont_bb);

  BL.SetInsertPoint(cont_bb);
  llvm::PHINode* phi = BL.CreatePHI(intptrTy, 2, "gt_res_phi");
  phi->addIncoming(gt_res, gt_bb);
  phi->addIncoming(call_err, err_bb);

  if (is_tail) {
    BL.CreateRet(phi);
  } else {
    set_reg(0, phi);
  }
}

void codegen_t::emit_num_le_subr(bool is_tail) {
  llvm::Value* arg1 = get_reg(0);
  llvm::Value* arg2 = get_reg(1);

  llvm::Value* args_and = BL.CreateAnd(arg1, arg2, "args_and");
  llvm::Value* mask = createInt64Constant(CT, 0x01);
  llvm::Value* args_mask = BL.CreateAnd(args_and, mask, "args_mask");
  llvm::Value* is_fixnum_cmp = BL.CreateICmpEQ(args_mask, mask, "is_fixnum_cmp");

  llvm::Function* f = BL.GetInsertBlock()->getParent();
  llvm::BasicBlock* le_bb = llvm::BasicBlock::Create(CT, "le_bb", f);
  llvm::BasicBlock* err_bb = llvm::BasicBlock::Create(CT, "err_bb", f);
  llvm::BasicBlock* cont_bb = llvm::BasicBlock::Create(CT, "cont_bb", f);

  llvm::MDBuilder mdb(CT);
  llvm::MDNode* branch_weights = mdb.createBranchWeights(2000, 1);
  BL.CreateCondBr(is_fixnum_cmp, le_bb, err_bb, branch_weights);

  BL.SetInsertPoint(le_bb);
  llvm::Value* is_le = BL.CreateICmpSLE(arg1, arg2, "is_le");
  llvm::Value* scm_true_val = createInt64Constant(CT, (uint64_t)scm_true);
  llvm::Value* scm_false_val = createInt64Constant(CT, (uint64_t)scm_false);
  llvm::Value* le_res = BL.CreateSelect(is_le, scm_true_val, scm_false_val, "le_res");
  BL.CreateBr(cont_bb);

  BL.SetInsertPoint(err_bb);
  llvm::Type* intptrTy = this->getInt64Type();
  llvm::FunctionType* c_num_le_ft = llvm::FunctionType::get(intptrTy, {intptrTy, intptrTy}, false);
  llvm::Function* c_num_le_func = get_or_create_external_function("c_num_le", c_num_le_ft, (void*)&c_num_le);
  llvm::Value* call_err = BL.CreateCall(c_num_le_ft, c_num_le_func, {arg1, arg2}, "call_err");
  BL.CreateBr(cont_bb);

  BL.SetInsertPoint(cont_bb);
  llvm::PHINode* phi = BL.CreatePHI(intptrTy, 2, "le_res_phi");
  phi->addIncoming(le_res, le_bb);
  phi->addIncoming(call_err, err_bb);

  if (is_tail) {
    BL.CreateRet(phi);
  } else {
    set_reg(0, phi);
  }
}

void codegen_t::emit_num_ge_subr(bool is_tail) {
  llvm::Value* arg1 = get_reg(0);
  llvm::Value* arg2 = get_reg(1);

  llvm::Value* args_and = BL.CreateAnd(arg1, arg2, "args_and");
  llvm::Value* mask = createInt64Constant(CT, 0x01);
  llvm::Value* args_mask = BL.CreateAnd(args_and, mask, "args_mask");
  llvm::Value* is_fixnum_cmp = BL.CreateICmpEQ(args_mask, mask, "is_fixnum_cmp");

  llvm::Function* f = BL.GetInsertBlock()->getParent();
  llvm::BasicBlock* ge_bb = llvm::BasicBlock::Create(CT, "ge_bb", f);
  llvm::BasicBlock* err_bb = llvm::BasicBlock::Create(CT, "err_bb", f);
  llvm::BasicBlock* cont_bb = llvm::BasicBlock::Create(CT, "cont_bb", f);

  llvm::MDBuilder mdb(CT);
  llvm::MDNode* branch_weights = mdb.createBranchWeights(2000, 1);
  BL.CreateCondBr(is_fixnum_cmp, ge_bb, err_bb, branch_weights);

  BL.SetInsertPoint(ge_bb);
  llvm::Value* is_ge = BL.CreateICmpSGE(arg1, arg2, "is_ge");
  llvm::Value* scm_true_val = createInt64Constant(CT, (uint64_t)scm_true);
  llvm::Value* scm_false_val = createInt64Constant(CT, (uint64_t)scm_false);
  llvm::Value* ge_res = BL.CreateSelect(is_ge, scm_true_val, scm_false_val, "ge_res");
  BL.CreateBr(cont_bb);

  BL.SetInsertPoint(err_bb);
  llvm::Type* intptrTy = this->getInt64Type();
  llvm::FunctionType* c_num_ge_ft = llvm::FunctionType::get(intptrTy, {intptrTy, intptrTy}, false);
  llvm::Function* c_num_ge_func = get_or_create_external_function("c_num_ge", c_num_ge_ft, (void*)&c_num_ge);
  llvm::Value* call_err = BL.CreateCall(c_num_ge_ft, c_num_ge_func, {arg1, arg2}, "call_err");
  BL.CreateBr(cont_bb);

  BL.SetInsertPoint(cont_bb);
  llvm::PHINode* phi = BL.CreatePHI(intptrTy, 2, "ge_res_phi");
  phi->addIncoming(ge_res, ge_bb);
  phi->addIncoming(call_err, err_bb);

  if (is_tail) {
    BL.CreateRet(phi);
  } else {
    set_reg(0, phi);
  }
}

void codegen_t::emit_vector_ref_subr(bool is_tail) {
  // arg1 = vector, arg2 = fixnum index
  // scm_vector_rec_t layout (LP64):
  //   offset  0: scm_tc6_t tag  (8 bytes)
  //   offset  8: scm_obj_t* elts pointer (8 bytes)
  //   offset 16: int nsize      (4 bytes)
  llvm::Value* arg1 = get_reg(0);
  llvm::Value* arg2 = get_reg(1);

  llvm::Function* f = BL.GetInsertBlock()->getParent();
  llvm::MDBuilder mdb(CT);
  llvm::MDNode* likely = mdb.createBranchWeights(2000, 1);

  // raw_ptr = arg1 - 2  (to_address)
  llvm::Value* raw_ptr = BL.CreateIntToPtr(BL.CreateSub(arg1, createInt64Constant(CT, 2), "vref_raw_addr"), getInt64PtrType(), "vref_raw_ptr");

#if USE_TBI
  // TBI: rotate left 7, mask 0x3bf, compare with (0x100 + tc6_vector)
  llvm::BasicBlock* check_idx_bb = llvm::BasicBlock::Create(CT, "vref_check_idx_bb", f);
  llvm::BasicBlock* load_bb = llvm::BasicBlock::Create(CT, "vref_load_bb", f);
  llvm::BasicBlock* err_bb = llvm::BasicBlock::Create(CT, "vref_err_bb", f);
  llvm::BasicBlock* cont_bb = llvm::BasicBlock::Create(CT, "vref_cont_bb", f);

  llvm::Value* vref_rot = BL.CreateIntrinsic(llvm::Intrinsic::fshl, {BL.getInt64Ty()}, {arg1, arg1, createInt64Constant(CT, 7)}, nullptr, "vref_rot");
  llvm::Value* vref_type_masked = BL.CreateAnd(vref_rot, createInt64Constant(CT, 0x3bf), "vref_type_masked");
  llvm::Value* vref_is_vector = BL.CreateICmpEQ(vref_type_masked, createInt64Constant(CT, 0x100 + tc6_vector), "vref_is_vector");
  BL.CreateCondBr(vref_is_vector, check_idx_bb, err_bb, likely);
#else
  // non-TBI: heap-object check then tag-word load
  llvm::BasicBlock* check_vec_bb = llvm::BasicBlock::Create(CT, "vref_check_vec_bb", f);
  llvm::BasicBlock* check_idx_bb = llvm::BasicBlock::Create(CT, "vref_check_idx_bb", f);
  llvm::BasicBlock* load_bb = llvm::BasicBlock::Create(CT, "vref_load_bb", f);
  llvm::BasicBlock* err_bb = llvm::BasicBlock::Create(CT, "vref_err_bb", f);
  llvm::BasicBlock* cont_bb = llvm::BasicBlock::Create(CT, "vref_cont_bb", f);

  llvm::Value* vref_lo3 = BL.CreateAnd(arg1, createInt64Constant(CT, 0x07), "vref_lo3");
  llvm::Value* vref_is_heap = BL.CreateICmpEQ(vref_lo3, createInt64Constant(CT, 0x02), "vref_is_heap");
  BL.CreateCondBr(vref_is_heap, check_vec_bb, err_bb, likely);

  BL.SetInsertPoint(check_vec_bb);
  llvm::Value* vref_tag_word = BL.CreateAlignedLoad(getInt64Type(), raw_ptr, llvm::Align(8), "vref_tag_word");
  llvm::Value* vref_tag_masked = BL.CreateAnd(vref_tag_word, createInt64Constant(CT, 0x3f00), "vref_tag_masked");
  llvm::Value* vref_is_vector = BL.CreateICmpEQ(vref_tag_masked, createInt64Constant(CT, (uintptr_t)tc6_vector << 8), "vref_is_vector");
  BL.CreateCondBr(vref_is_vector, check_idx_bb, err_bb, likely);
#endif

  // --- check arg2 is fixnum and index in [0, nsize) ---
  BL.SetInsertPoint(check_idx_bb);
  llvm::Value* vref_idx_bit = BL.CreateAnd(arg2, createInt64Constant(CT, 0x01), "vref_idx_bit");
  llvm::Value* vref_is_fix = BL.CreateICmpEQ(vref_idx_bit, createInt64Constant(CT, 0x01), "vref_is_fix");
  llvm::Value* vref_idx_raw = BL.CreateAShr(arg2, createInt64Constant(CT, 1), "vref_idx_raw");
  // nsize at offset 16 (tag=8, elts ptr=8, nsize=4)
  llvm::Value* vref_nsize_ptr = BL.CreateConstInBoundsGEP1_32(BL.getInt8Ty(), raw_ptr, 16, "vref_nsize_ptr");
  llvm::Value* vref_nsize_i32 = BL.CreateAlignedLoad(BL.getInt32Ty(), vref_nsize_ptr, llvm::Align(4), "vref_nsize_i32");
  llvm::Value* vref_nsize = BL.CreateZExt(vref_nsize_i32, getInt64Type(), "vref_nsize");
  llvm::Value* vref_ge_zero = BL.CreateICmpSGE(vref_idx_raw, createInt64Constant(CT, 0), "vref_ge_zero");
  llvm::Value* vref_lt_nsize = BL.CreateICmpSLT(vref_idx_raw, vref_nsize, "vref_lt_nsize");
  llvm::Value* vref_ok = BL.CreateAnd(BL.CreateAnd(vref_is_fix, vref_ge_zero, "vref_fix_ge"), vref_lt_nsize, "vref_ok");
  BL.CreateCondBr(vref_ok, load_bb, err_bb, likely);

  // --- fast path: load elts[idx] ---
  BL.SetInsertPoint(load_bb);
  // elts pointer at offset 8 in the struct
  llvm::Value* vref_elts_ptr_ptr = BL.CreateConstInBoundsGEP1_32(BL.getInt8Ty(), raw_ptr, 8, "vref_elts_ptr_ptr");
  llvm::Value* vref_elts_addr = BL.CreateAlignedLoad(getInt64Type(), vref_elts_ptr_ptr, llvm::Align(8), "vref_elts_addr");
  llvm::Value* vref_elem_ptr = BL.CreateInBoundsGEP(getInt64Type(), BL.CreateIntToPtr(vref_elts_addr, getInt64PtrType(), "vref_elts_ptr"), vref_idx_raw, "vref_elem_ptr");
  llvm::Value* vref_elem = BL.CreateAlignedLoad(getInt64Type(), vref_elem_ptr, llvm::Align(8), "vref_elem");
  BL.CreateBr(cont_bb);

  // --- error path ---
  BL.SetInsertPoint(err_bb);
  {
    llvm::FunctionType* err_ft = llvm::FunctionType::get(llvm::Type::getVoidTy(CT), {getInt64Type(), getInt64Type()}, false);
    llvm::Function* err_func = get_or_create_external_function("c_error_vector_ref", err_ft, (void*)&c_error_vector_ref);
    BL.CreateCall(err_ft, err_func, {arg1, arg2});
    BL.CreateUnreachable();
  }

  BL.SetInsertPoint(cont_bb);
  if (is_tail) {
    BL.CreateRet(vref_elem);
  } else {
    set_reg(0, vref_elem);
  }
}

void codegen_t::emit_vector_set_subr(bool is_tail) {
  // arg1 = vector, arg2 = fixnum index, arg3 = new value
  // scm_vector_rec_t layout (LP64):
  //   offset  0: scm_tc6_t tag  (8 bytes)
  //   offset  8: scm_obj_t* elts pointer (8 bytes)
  //   offset 16: int nsize      (4 bytes)
  llvm::Value* arg1 = get_reg(0);
  llvm::Value* arg2 = get_reg(1);
  llvm::Value* arg3 = get_reg(2);

  llvm::Function* f = BL.GetInsertBlock()->getParent();
  llvm::MDBuilder mdb(CT);
  llvm::MDNode* likely = mdb.createBranchWeights(2000, 1);

  // raw_ptr = arg1 - 2  (to_address)
  llvm::Value* raw_ptr = BL.CreateIntToPtr(BL.CreateSub(arg1, createInt64Constant(CT, 2), "vset_raw_addr"), getInt64PtrType(), "vset_raw_ptr");

#if USE_TBI
  // TBI: rotate left 7, mask 0x3bf, compare with (0x100 + tc6_vector)
  llvm::BasicBlock* check_idx_bb = llvm::BasicBlock::Create(CT, "vset_check_idx_bb", f);
  llvm::BasicBlock* store_bb = llvm::BasicBlock::Create(CT, "vset_store_bb", f);
  llvm::BasicBlock* err_bb = llvm::BasicBlock::Create(CT, "vset_err_bb", f);
  llvm::BasicBlock* cont_bb = llvm::BasicBlock::Create(CT, "vset_cont_bb", f);

  llvm::Value* vset_rot = BL.CreateIntrinsic(llvm::Intrinsic::fshl, {BL.getInt64Ty()}, {arg1, arg1, createInt64Constant(CT, 7)}, nullptr, "vset_rot");
  llvm::Value* vset_type_masked = BL.CreateAnd(vset_rot, createInt64Constant(CT, 0x3bf), "vset_type_masked");
  llvm::Value* vset_is_vector = BL.CreateICmpEQ(vset_type_masked, createInt64Constant(CT, 0x100 + tc6_vector), "vset_is_vector");
  BL.CreateCondBr(vset_is_vector, check_idx_bb, err_bb, likely);
#else
  // non-TBI: heap-object check then tag-word load
  llvm::BasicBlock* check_vec_bb = llvm::BasicBlock::Create(CT, "vset_check_vec_bb", f);
  llvm::BasicBlock* check_idx_bb = llvm::BasicBlock::Create(CT, "vset_check_idx_bb", f);
  llvm::BasicBlock* store_bb = llvm::BasicBlock::Create(CT, "vset_store_bb", f);
  llvm::BasicBlock* err_bb = llvm::BasicBlock::Create(CT, "vset_err_bb", f);
  llvm::BasicBlock* cont_bb = llvm::BasicBlock::Create(CT, "vset_cont_bb", f);

  llvm::Value* vset_lo3 = BL.CreateAnd(arg1, createInt64Constant(CT, 0x07), "vset_lo3");
  llvm::Value* vset_is_heap = BL.CreateICmpEQ(vset_lo3, createInt64Constant(CT, 0x02), "vset_is_heap");
  BL.CreateCondBr(vset_is_heap, check_vec_bb, err_bb, likely);

  BL.SetInsertPoint(check_vec_bb);
  llvm::Value* vset_tag_word = BL.CreateAlignedLoad(getInt64Type(), raw_ptr, llvm::Align(8), "vset_tag_word");
  llvm::Value* vset_tag_masked = BL.CreateAnd(vset_tag_word, createInt64Constant(CT, 0x3f00), "vset_tag_masked");
  llvm::Value* vset_is_vector = BL.CreateICmpEQ(vset_tag_masked, createInt64Constant(CT, (uintptr_t)tc6_vector << 8), "vset_is_vector");
  BL.CreateCondBr(vset_is_vector, check_idx_bb, err_bb, likely);
#endif

  // --- check arg2 is fixnum and index in [0, nsize) ---
  BL.SetInsertPoint(check_idx_bb);
  llvm::Value* vset_idx_bit = BL.CreateAnd(arg2, createInt64Constant(CT, 0x01), "vset_idx_bit");
  llvm::Value* vset_is_fix = BL.CreateICmpEQ(vset_idx_bit, createInt64Constant(CT, 0x01), "vset_is_fix");
  llvm::Value* vset_idx_raw = BL.CreateAShr(arg2, createInt64Constant(CT, 1), "vset_idx_raw");
  // nsize at offset 16 (tag=8, elts ptr=8, nsize=4)
  llvm::Value* vset_nsize_ptr = BL.CreateConstInBoundsGEP1_32(BL.getInt8Ty(), raw_ptr, 16, "vset_nsize_ptr");
  llvm::Value* vset_nsize_i32 = BL.CreateAlignedLoad(BL.getInt32Ty(), vset_nsize_ptr, llvm::Align(4), "vset_nsize_i32");
  llvm::Value* vset_nsize = BL.CreateZExt(vset_nsize_i32, getInt64Type(), "vset_nsize");
  llvm::Value* vset_ge_zero = BL.CreateICmpSGE(vset_idx_raw, createInt64Constant(CT, 0), "vset_ge_zero");
  llvm::Value* vset_lt_nsize = BL.CreateICmpSLT(vset_idx_raw, vset_nsize, "vset_lt_nsize");
  llvm::Value* vset_ok = BL.CreateAnd(BL.CreateAnd(vset_is_fix, vset_ge_zero, "vset_fix_ge"), vset_lt_nsize, "vset_ok");
  BL.CreateCondBr(vset_ok, store_bb, err_bb, likely);

  // --- fast path: store arg3 into elts[idx] + write barrier ---
  BL.SetInsertPoint(store_bb);
  // elts pointer at offset 8 in the struct
  llvm::Value* vset_elts_ptr_ptr = BL.CreateConstInBoundsGEP1_32(BL.getInt8Ty(), raw_ptr, 8, "vset_elts_ptr_ptr");
  llvm::Value* vset_elts_addr = BL.CreateAlignedLoad(getInt64Type(), vset_elts_ptr_ptr, llvm::Align(8), "vset_elts_addr");
  llvm::Value* vset_elem_ptr = BL.CreateInBoundsGEP(getInt64Type(), BL.CreateIntToPtr(vset_elts_addr, getInt64PtrType(), "vset_elts_ptr"), vset_idx_raw, "vset_elem_ptr");
  emitWriteBarrier(arg3);
  BL.CreateAlignedStore(arg3, vset_elem_ptr, llvm::Align(8));
  BL.CreateBr(cont_bb);

  // --- error path ---
  BL.SetInsertPoint(err_bb);
  {
    llvm::FunctionType* err_ft = llvm::FunctionType::get(llvm::Type::getVoidTy(CT), {getInt64Type(), getInt64Type(), getInt64Type()}, false);
    llvm::Function* err_func = get_or_create_external_function("c_error_vector_set", err_ft, (void*)&c_error_vector_set);
    BL.CreateCall(err_ft, err_func, {arg1, arg2, arg3});
    BL.CreateUnreachable();
  }

  BL.SetInsertPoint(cont_bb);
  llvm::Value* scm_unspec_val = createInt64Constant(CT, (uint64_t)scm_unspecified);
  if (is_tail) {
    BL.CreateRet(scm_unspec_val);
  } else {
    set_reg(0, scm_unspec_val);
  }
}


void codegen_t::emit_tuple_ref_subr(bool is_tail) {
  // arg1 = tuple, arg2 = fixnum index
  // scm_tuple_rec_t layout (LP64):
  //   offset  0: scm_tc6_t tag   (8 bytes)
  //   offset  8: int nsize        (4 bytes)
  //   offset 12: 4 bytes padding
  //   offset 16: scm_obj_t elts[] (inline, 8 bytes each)
  llvm::Value* arg1 = get_reg(0);
  llvm::Value* arg2 = get_reg(1);

  llvm::Function* f = BL.GetInsertBlock()->getParent();
  llvm::MDBuilder mdb(CT);
  llvm::MDNode* likely = mdb.createBranchWeights(2000, 1);

  // raw_ptr = arg1 - 2  (to_address; valid in both TBI and non-TBI thanks to AArch64 TBI)
  llvm::Value* raw_ptr = BL.CreateIntToPtr(BL.CreateSub(arg1, createInt64Constant(CT, 2), "tref_raw_addr"), getInt64PtrType(), "tref_raw_ptr");

  // Declare idx_raw and elem before the preprocessor branches so they are
  // visible to the shared load block and the return/set_reg below.
  llvm::Value* idx_raw = nullptr;
  llvm::Value* elem = nullptr;

#ifndef NDEBUG
  // ---- debug: type check + index bounds check ----
  llvm::BasicBlock* check_idx_bb = llvm::BasicBlock::Create(CT, "tref_check_idx_bb", f);
  llvm::BasicBlock* load_bb = llvm::BasicBlock::Create(CT, "tref_load_bb", f);
  llvm::BasicBlock* err_bb = llvm::BasicBlock::Create(CT, "tref_err_bb", f);
  llvm::BasicBlock* cont_bb = llvm::BasicBlock::Create(CT, "tref_cont_bb", f);

  // --- type check ---
#if USE_TBI
  // TBI: rotate left 7, mask 0x3bf, compare with (0x100 + tc6_tuple)
  llvm::Value* tref_rot = BL.CreateIntrinsic(llvm::Intrinsic::fshl, {BL.getInt64Ty()}, {arg1, arg1, createInt64Constant(CT, 7)}, nullptr, "tref_rot");
  llvm::Value* tref_type_masked = BL.CreateAnd(tref_rot, createInt64Constant(CT, 0x3bf), "tref_type_masked");
  llvm::Value* tref_is_tuple = BL.CreateICmpEQ(tref_type_masked, createInt64Constant(CT, 0x100 + tc6_tuple), "tref_is_tuple");
  BL.CreateCondBr(tref_is_tuple, check_idx_bb, err_bb, likely);
#else
  // non-TBI: heap-object check then tag-word load
  llvm::BasicBlock* check_tup_bb = llvm::BasicBlock::Create(CT, "tref_check_tup_bb", f);
  llvm::Value* tref_lo3 = BL.CreateAnd(arg1, createInt64Constant(CT, 0x07), "tref_lo3");
  llvm::Value* tref_is_heap = BL.CreateICmpEQ(tref_lo3, createInt64Constant(CT, 0x02), "tref_is_heap");
  BL.CreateCondBr(tref_is_heap, check_tup_bb, err_bb, likely);
  BL.SetInsertPoint(check_tup_bb);
  llvm::Value* tref_tag_word = BL.CreateAlignedLoad(getInt64Type(), raw_ptr, llvm::Align(8), "tref_tag_word");
  llvm::Value* tref_tag_masked = BL.CreateAnd(tref_tag_word, createInt64Constant(CT, 0x3f00), "tref_tag_masked");
  llvm::Value* tref_is_tuple = BL.CreateICmpEQ(tref_tag_masked, createInt64Constant(CT, (uintptr_t)tc6_tuple << 8), "tref_is_tuple");
  BL.CreateCondBr(tref_is_tuple, check_idx_bb, err_bb, likely);
#endif

  // --- index bounds check ---
  BL.SetInsertPoint(check_idx_bb);
  llvm::Value* tref_idx_bit = BL.CreateAnd(arg2, createInt64Constant(CT, 0x01), "tref_idx_bit");
  llvm::Value* tref_is_fix = BL.CreateICmpEQ(tref_idx_bit, createInt64Constant(CT, 0x01), "tref_is_fix");
  idx_raw = BL.CreateAShr(arg2, createInt64Constant(CT, 1), "tref_idx_raw");
  llvm::Value* tref_nsize_ptr = BL.CreateConstInBoundsGEP1_32(BL.getInt8Ty(), raw_ptr, 8, "tref_nsize_ptr");
  llvm::Value* tref_nsize_i32 = BL.CreateAlignedLoad(BL.getInt32Ty(), tref_nsize_ptr, llvm::Align(4), "tref_nsize_i32");
  llvm::Value* tref_nsize = BL.CreateZExt(tref_nsize_i32, getInt64Type(), "tref_nsize");
  llvm::Value* tref_ge_zero = BL.CreateICmpSGE(idx_raw, createInt64Constant(CT, 0), "tref_ge_zero");
  llvm::Value* tref_lt_nsize = BL.CreateICmpSLT(idx_raw, tref_nsize, "tref_lt_nsize");
  llvm::Value* tref_ok = BL.CreateAnd(BL.CreateAnd(tref_is_fix, tref_ge_zero, "tref_fix_ge"), tref_lt_nsize, "tref_ok");
  BL.CreateCondBr(tref_ok, load_bb, err_bb, likely);

  // --- fast path: load elts[idx] at offset 16 ---
  BL.SetInsertPoint(load_bb);
  llvm::Value* tref_elts_base = BL.CreateConstInBoundsGEP1_32(BL.getInt8Ty(), raw_ptr, 16, "tref_elts_base");
  llvm::Value* tref_elem_ptr = BL.CreateInBoundsGEP(getInt64Type(), tref_elts_base, idx_raw, "tref_elem_ptr");
  elem = BL.CreateAlignedLoad(getInt64Type(), tref_elem_ptr, llvm::Align(8), "tref_elem");
  BL.CreateBr(cont_bb);

  // --- error path ---
  BL.SetInsertPoint(err_bb);
  {
    llvm::FunctionType* err_ft = llvm::FunctionType::get(llvm::Type::getVoidTy(CT), {getInt64Type(), getInt64Type()}, false);
    llvm::Function* err_func = get_or_create_external_function("c_error_tuple_ref", err_ft, (void*)&c_error_tuple_ref);
    BL.CreateCall(err_ft, err_func, {arg1, arg2});
    BL.CreateUnreachable();
  }

  BL.SetInsertPoint(cont_bb);

#else  // NDEBUG: type check only, no bounds check

  llvm::BasicBlock* load_bb = llvm::BasicBlock::Create(CT, "tref_load_bb", f);
  llvm::BasicBlock* err_bb = llvm::BasicBlock::Create(CT, "tref_err_bb", f);
  llvm::BasicBlock* cont_bb = llvm::BasicBlock::Create(CT, "tref_cont_bb", f);

  // --- type check ---
#if USE_TBI
  llvm::Value* tref_rot = BL.CreateIntrinsic(llvm::Intrinsic::fshl, {BL.getInt64Ty()}, {arg1, arg1, createInt64Constant(CT, 7)}, nullptr, "tref_rot");
  llvm::Value* tref_type_masked = BL.CreateAnd(tref_rot, createInt64Constant(CT, 0x3bf), "tref_type_masked");
  llvm::Value* tref_is_tuple = BL.CreateICmpEQ(tref_type_masked, createInt64Constant(CT, 0x100 + tc6_tuple), "tref_is_tuple");
  BL.CreateCondBr(tref_is_tuple, load_bb, err_bb, likely);
#else
  llvm::BasicBlock* check_tup_bb = llvm::BasicBlock::Create(CT, "tref_check_tup_bb", f);
  llvm::Value* tref_lo3 = BL.CreateAnd(arg1, createInt64Constant(CT, 0x07), "tref_lo3");
  llvm::Value* tref_is_heap = BL.CreateICmpEQ(tref_lo3, createInt64Constant(CT, 0x02), "tref_is_heap");
  BL.CreateCondBr(tref_is_heap, check_tup_bb, err_bb, likely);
  BL.SetInsertPoint(check_tup_bb);
  llvm::Value* tref_tag_word = BL.CreateAlignedLoad(getInt64Type(), raw_ptr, llvm::Align(8), "tref_tag_word");
  llvm::Value* tref_tag_masked = BL.CreateAnd(tref_tag_word, createInt64Constant(CT, 0x3f00), "tref_tag_masked");
  llvm::Value* tref_is_tuple = BL.CreateICmpEQ(tref_tag_masked, createInt64Constant(CT, (uintptr_t)tc6_tuple << 8), "tref_is_tuple");
  BL.CreateCondBr(tref_is_tuple, load_bb, err_bb, likely);
#endif

  // --- fast path: load elts[idx] at offset 16 (no bounds check) ---
  BL.SetInsertPoint(load_bb);
  idx_raw = BL.CreateAShr(arg2, createInt64Constant(CT, 1), "tref_idx_raw");
  llvm::Value* tref_elts_base = BL.CreateConstInBoundsGEP1_32(BL.getInt8Ty(), raw_ptr, 16, "tref_elts_base");
  llvm::Value* tref_elem_ptr = BL.CreateInBoundsGEP(getInt64Type(), tref_elts_base, idx_raw, "tref_elem_ptr");
  elem = BL.CreateAlignedLoad(getInt64Type(), tref_elem_ptr, llvm::Align(8), "tref_elem");
  BL.CreateBr(cont_bb);

  // --- error path ---
  BL.SetInsertPoint(err_bb);
  {
    llvm::FunctionType* err_ft = llvm::FunctionType::get(llvm::Type::getVoidTy(CT), {getInt64Type(), getInt64Type()}, false);
    llvm::Function* err_func = get_or_create_external_function("c_error_tuple_ref", err_ft, (void*)&c_error_tuple_ref);
    BL.CreateCall(err_ft, err_func, {arg1, arg2});
    BL.CreateUnreachable();
  }

  BL.SetInsertPoint(cont_bb);
#endif  // NDEBUG

  if (is_tail) {
    BL.CreateRet(elem);
  } else {
    set_reg(0, elem);
  }
}

void codegen_t::emit_tuple_set_subr(bool is_tail) {
  // arg1 = tuple, arg2 = fixnum index, arg3 = new value
  // scm_tuple_rec_t layout (LP64):
  //   offset  0: scm_tc6_t tag   (8 bytes)
  //   offset  8: int nsize        (4 bytes)
  //   offset 12: 4 bytes padding
  //   offset 16: scm_obj_t elts[] (inline, 8 bytes each)
  llvm::Value* arg1 = get_reg(0);
  llvm::Value* arg2 = get_reg(1);
  llvm::Value* arg3 = get_reg(2);

  llvm::Function* f = BL.GetInsertBlock()->getParent();
  llvm::MDBuilder mdb(CT);
  llvm::MDNode* likely = mdb.createBranchWeights(2000, 1);

  // raw_ptr = arg1 - 2  (to_address)
  llvm::Value* raw_ptr = BL.CreateIntToPtr(BL.CreateSub(arg1, createInt64Constant(CT, 2), "tset_raw_addr"), getInt64PtrType(), "tset_raw_ptr");

  // Declare idx_raw before branches so it is visible to the shared store logic.
  llvm::Value* idx_raw = nullptr;

#ifndef NDEBUG
  // ---- debug: type check + index bounds check ----
  llvm::BasicBlock* check_idx_bb = llvm::BasicBlock::Create(CT, "tset_check_idx_bb", f);
  llvm::BasicBlock* store_bb = llvm::BasicBlock::Create(CT, "tset_store_bb", f);
  llvm::BasicBlock* err_bb = llvm::BasicBlock::Create(CT, "tset_err_bb", f);
  llvm::BasicBlock* cont_bb = llvm::BasicBlock::Create(CT, "tset_cont_bb", f);

  // --- type check ---
#if USE_TBI
  llvm::Value* tset_rot = BL.CreateIntrinsic(llvm::Intrinsic::fshl, {BL.getInt64Ty()}, {arg1, arg1, createInt64Constant(CT, 7)}, nullptr, "tset_rot");
  llvm::Value* tset_type_masked = BL.CreateAnd(tset_rot, createInt64Constant(CT, 0x3bf), "tset_type_masked");
  llvm::Value* tset_is_tuple = BL.CreateICmpEQ(tset_type_masked, createInt64Constant(CT, 0x100 + tc6_tuple), "tset_is_tuple");
  BL.CreateCondBr(tset_is_tuple, check_idx_bb, err_bb, likely);
#else
  llvm::BasicBlock* check_tup_bb = llvm::BasicBlock::Create(CT, "tset_check_tup_bb", f);
  llvm::Value* tset_lo3 = BL.CreateAnd(arg1, createInt64Constant(CT, 0x07), "tset_lo3");
  llvm::Value* tset_is_heap = BL.CreateICmpEQ(tset_lo3, createInt64Constant(CT, 0x02), "tset_is_heap");
  BL.CreateCondBr(tset_is_heap, check_tup_bb, err_bb, likely);
  BL.SetInsertPoint(check_tup_bb);
  llvm::Value* tset_tag_word = BL.CreateAlignedLoad(getInt64Type(), raw_ptr, llvm::Align(8), "tset_tag_word");
  llvm::Value* tset_tag_masked = BL.CreateAnd(tset_tag_word, createInt64Constant(CT, 0x3f00), "tset_tag_masked");
  llvm::Value* tset_is_tuple = BL.CreateICmpEQ(tset_tag_masked, createInt64Constant(CT, (uintptr_t)tc6_tuple << 8), "tset_is_tuple");
  BL.CreateCondBr(tset_is_tuple, check_idx_bb, err_bb, likely);
#endif

  // --- index bounds check ---
  BL.SetInsertPoint(check_idx_bb);
  llvm::Value* tset_idx_bit = BL.CreateAnd(arg2, createInt64Constant(CT, 0x01), "tset_idx_bit");
  llvm::Value* tset_is_fix = BL.CreateICmpEQ(tset_idx_bit, createInt64Constant(CT, 0x01), "tset_is_fix");
  idx_raw = BL.CreateAShr(arg2, createInt64Constant(CT, 1), "tset_idx_raw");
  llvm::Value* tset_nsize_ptr = BL.CreateConstInBoundsGEP1_32(BL.getInt8Ty(), raw_ptr, 8, "tset_nsize_ptr");
  llvm::Value* tset_nsize_i32 = BL.CreateAlignedLoad(BL.getInt32Ty(), tset_nsize_ptr, llvm::Align(4), "tset_nsize_i32");
  llvm::Value* tset_nsize = BL.CreateZExt(tset_nsize_i32, getInt64Type(), "tset_nsize");
  llvm::Value* tset_ge_zero = BL.CreateICmpSGE(idx_raw, createInt64Constant(CT, 0), "tset_ge_zero");
  llvm::Value* tset_lt_nsize = BL.CreateICmpSLT(idx_raw, tset_nsize, "tset_lt_nsize");
  llvm::Value* tset_ok = BL.CreateAnd(BL.CreateAnd(tset_is_fix, tset_ge_zero, "tset_fix_ge"), tset_lt_nsize, "tset_ok");
  BL.CreateCondBr(tset_ok, store_bb, err_bb, likely);

  // --- fast path: store arg3 into elts[idx] at offset 16 + write barrier ---
  BL.SetInsertPoint(store_bb);
  llvm::Value* tset_elts_base = BL.CreateConstInBoundsGEP1_32(BL.getInt8Ty(), raw_ptr, 16, "tset_elts_base");
  llvm::Value* tset_elem_ptr = BL.CreateInBoundsGEP(getInt64Type(), tset_elts_base, idx_raw, "tset_elem_ptr");
  emitWriteBarrier(arg3);
  BL.CreateAlignedStore(arg3, tset_elem_ptr, llvm::Align(8));
  BL.CreateBr(cont_bb);

  // --- error path ---
  BL.SetInsertPoint(err_bb);
  {
    llvm::FunctionType* err_ft = llvm::FunctionType::get(llvm::Type::getVoidTy(CT), {getInt64Type(), getInt64Type(), getInt64Type()}, false);
    llvm::Function* err_func = get_or_create_external_function("c_error_tuple_set", err_ft, (void*)&c_error_tuple_set);
    BL.CreateCall(err_ft, err_func, {arg1, arg2, arg3});
    BL.CreateUnreachable();
  }

  BL.SetInsertPoint(cont_bb);

#else  // NDEBUG: type check only, no bounds check

  llvm::BasicBlock* store_bb = llvm::BasicBlock::Create(CT, "tset_store_bb", f);
  llvm::BasicBlock* err_bb = llvm::BasicBlock::Create(CT, "tset_err_bb", f);
  llvm::BasicBlock* cont_bb = llvm::BasicBlock::Create(CT, "tset_cont_bb", f);

  // --- type check ---
#if USE_TBI
  llvm::Value* tset_rot = BL.CreateIntrinsic(llvm::Intrinsic::fshl, {BL.getInt64Ty()}, {arg1, arg1, createInt64Constant(CT, 7)}, nullptr, "tset_rot");
  llvm::Value* tset_type_masked = BL.CreateAnd(tset_rot, createInt64Constant(CT, 0x3bf), "tset_type_masked");
  llvm::Value* tset_is_tuple = BL.CreateICmpEQ(tset_type_masked, createInt64Constant(CT, 0x100 + tc6_tuple), "tset_is_tuple");
  BL.CreateCondBr(tset_is_tuple, store_bb, err_bb, likely);
#else
  llvm::BasicBlock* check_tup_bb = llvm::BasicBlock::Create(CT, "tset_check_tup_bb", f);
  llvm::Value* tset_lo3 = BL.CreateAnd(arg1, createInt64Constant(CT, 0x07), "tset_lo3");
  llvm::Value* tset_is_heap = BL.CreateICmpEQ(tset_lo3, createInt64Constant(CT, 0x02), "tset_is_heap");
  BL.CreateCondBr(tset_is_heap, check_tup_bb, err_bb, likely);
  BL.SetInsertPoint(check_tup_bb);
  llvm::Value* tset_tag_word = BL.CreateAlignedLoad(getInt64Type(), raw_ptr, llvm::Align(8), "tset_tag_word");
  llvm::Value* tset_tag_masked = BL.CreateAnd(tset_tag_word, createInt64Constant(CT, 0x3f00), "tset_tag_masked");
  llvm::Value* tset_is_tuple = BL.CreateICmpEQ(tset_tag_masked, createInt64Constant(CT, (uintptr_t)tc6_tuple << 8), "tset_is_tuple");
  BL.CreateCondBr(tset_is_tuple, store_bb, err_bb, likely);
#endif

  // --- fast path: store arg3 into elts[idx] at offset 16 (no bounds check) + write barrier ---
  BL.SetInsertPoint(store_bb);
  idx_raw = BL.CreateAShr(arg2, createInt64Constant(CT, 1), "tset_idx_raw");
  llvm::Value* tset_elts_base = BL.CreateConstInBoundsGEP1_32(BL.getInt8Ty(), raw_ptr, 16, "tset_elts_base");
  llvm::Value* tset_elem_ptr = BL.CreateInBoundsGEP(getInt64Type(), tset_elts_base, idx_raw, "tset_elem_ptr");
  emitWriteBarrier(arg3);
  BL.CreateAlignedStore(arg3, tset_elem_ptr, llvm::Align(8));
  BL.CreateBr(cont_bb);

  // --- error path ---
  BL.SetInsertPoint(err_bb);
  {
    llvm::FunctionType* err_ft = llvm::FunctionType::get(llvm::Type::getVoidTy(CT), {getInt64Type(), getInt64Type(), getInt64Type()}, false);
    llvm::Function* err_func = get_or_create_external_function("c_error_tuple_set", err_ft, (void*)&c_error_tuple_set);
    BL.CreateCall(err_ft, err_func, {arg1, arg2, arg3});
    BL.CreateUnreachable();
  }

  BL.SetInsertPoint(cont_bb);
#endif  // NDEBUG

  llvm::Value* scm_unspec_val = createInt64Constant(CT, (uint64_t)scm_unspecified);
  if (is_tail) {
    BL.CreateRet(scm_unspec_val);
  } else {
    set_reg(0, scm_unspec_val);
  }
}
