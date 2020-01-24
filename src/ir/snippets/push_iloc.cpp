#include "../../core.h"
#include "../../vm.h"

#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/Support/Error.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llvm::orc;

    /*
        #define OPERANDS        (CDAR(m_pc))
        CASE(VMOP_PUSH_ILOC) {
            if (m_sp < m_stack_limit) {
                m_sp[0] = *lookup_iloc(OPERANDS);
                if (m_sp[0] == scm_undef) goto ERROR_LETREC_VIOLATION;
                m_sp++;
                m_pc = CDR(m_pc);
                goto loop;
            }
            goto COLLECT_STACK_ONE;
        }
    */

#define DECLEAR_INTPTR_TYPES auto IntptrTy = (sizeof(intptr_t) == 4 ? Type::getInt32Ty(C) : Type::getInt64Ty(C)); auto IntptrPtrTy = sizeof(intptr_t) == 4 ? Type::getInt32PtrTy(C) : Type::getInt64PtrTy(C);
#define CREATE_LOAD_VM_REG(_VM_,_REG_) (IRB.CreateLoad(IntptrTy, IRB.CreateGEP(_VM_, IRB.getInt32(offsetof(VM, _REG_) / sizeof(intptr_t)))))

void emit_push_iloc(LLVMContext& C, Function* F, IRBuilder<>& IRB, scm_obj_t operands)
{
    DECLEAR_INTPTR_TYPES;

    auto vm = F->arg_begin();
    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto stack_limit = CREATE_LOAD_VM_REG(vm, m_stack_limit);

    BasicBlock* cond_true = BasicBlock::Create(C, "cond_true", F);
    BasicBlock* cond_false = BasicBlock::Create(C, "cond_false", F);
    Value* cond = IRB.CreateICmpULT(sp, stack_limit);
    IRB.CreateCondBr(cond, cond_true, cond_false);
    IRB.SetInsertPoint(cond_true);
    IRB.CreateRet(sp);
    IRB.SetInsertPoint(cond_false);
    IRB.CreateRet(stack_limit);
}

int main() {
    auto Context = llvm::make_unique<LLVMContext>();
    LLVMContext& C = *Context;

    DECLEAR_INTPTR_TYPES;

    auto M = llvm::make_unique<Module>("foobar_module", C);

    std::vector<Type*> argTypes;
    argTypes.push_back(IntptrPtrTy);    // vm
    Type* returnType = IntptrTy;
    Function* F =
        Function::Create(
            FunctionType::get(returnType, argTypes, false),
            Function::ExternalLinkage,
            "foobar_function",
            M.get());

    BasicBlock* BEGIN = BasicBlock::Create(C, "begin", F);

    IRBuilder<> IRB(BEGIN);

    emit_push_iloc(C, F, IRB, nullptr);

//    IRB.SetInsertPoint(BEGIN);
//    Value* r = IRB.getInt64(200);;
//    IRB.CreateRet(r);

//    verifyFunction(*F, &outs());
    verifyModule(*M, &outs());

    M.get()->print(outs(), nullptr);
}
