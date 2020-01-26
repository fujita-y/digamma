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
    void transform(LLVMContext& C, Function* F, IRBuilder<>& IRB, VM* vm, scm_obj_t code);
    void emit_ret_const(LLVMContext& C, Function* F, IRBuilder<>& IRB, scm_obj_t operands);
    void emit_push_iloc(LLVMContext& C, Function* F, IRBuilder<>& IRB, scm_obj_t operands);
};