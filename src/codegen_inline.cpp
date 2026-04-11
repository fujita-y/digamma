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