// Copyright (c) 2004-2020 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "vm.h"

#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"

using namespace llvm;
using namespace llvm::orc;

class codegen_t {
    std::unique_ptr<LLJIT> m_jit;
public:
    codegen_t();
    void compile(VM* vm, scm_closure_t closure);
private:
    void transform(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst);
    Value* emit_lookup_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst);
    Function* emit_call(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst);
    void emit_push(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst);
    void emit_push_const(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst);
    void emit_apply_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst);
    void emit_ret_const(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst);
    void emit_ret_subr(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst);
    void emit_if_true(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst);
    void emit_apply_gloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst);
    void emit_ret_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst);
    void emit_lt_n_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst);
    void emit_push_nadd_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst);

};
