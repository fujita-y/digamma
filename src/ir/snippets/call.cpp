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

#define DECLEAR_INTPTR_TYPES \
    auto IntptrTy = (sizeof(intptr_t) == 4 ? Type::getInt32Ty(C) : Type::getInt64Ty(C)); \
    auto IntptrPtrTy = sizeof(intptr_t) == 4 ? Type::getInt32PtrTy(C) : Type::getInt64PtrTy(C);

#define CREATE_LOAD_VM_REG(_VM_,_REG_) \
    (IRB.CreateLoad(IntptrTy, IRB.CreateGEP(_VM_, IRB.getInt32(offsetof(VM, _REG_) / sizeof(intptr_t)))))

#define CREATE_STORE_VM_REG(_VM_,_REG_,_VAL_) \
    (IRB.CreateStore(_VAL_, IRB.CreateGEP(_VM_, IRB.getInt32(offsetof(VM, _REG_) / sizeof(intptr_t)))))

#define VALUE_INTPTR(_VAL_) \
    (sizeof(intptr_t) == 4 ? IRB.getInt32((intptr_t)(_VAL_)) : IRB.getInt64((intptr_t)(_VAL_)))

#define INST_NATIVE     (vm->opcode_to_instruction(VMOP_NATIVE))
#define CONS(a, d)      make_pair(vm->m_heap, (a), (d))
#define LIST1(e1)       CONS((e1), scm_nil)
#define LIST2(e1, e2)   CONS((e1), LIST1((e2)))

void emit_call(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t operands)
{
    DECLEAR_INTPTR_TYPES;

    auto vm = F->arg_begin();
    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto stack_limit = CREATE_LOAD_VM_REG(vm, m_stack_limit);

    std::vector<Type*> argTypes;
    argTypes.push_back(IntptrPtrTy);
    Type* returnType = IntptrTy;
    Function* F2 =
        Function::Create(
            FunctionType::get(returnType, argTypes, false),
            Function::ExternalLinkage,
            "cont-func-0",
            M);

    BasicBlock* CONT = BasicBlock::Create(C, "begin", F2);

    // emit inst to save address of F2 to m_cont

    auto funcPtr = IRB.CreateLoad(IntptrTy, F2);
//    CREATE_STORE_VM_REG(vm, m_value, val);

    // emit inside call op
    IRB.CreateRet(stack_limit);

    //
    IRB.SetInsertPoint(CONT);
}

int main(int argc, char** argv) {
    llvm::InitLLVM X(argc, argv);
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
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

    emit_call(C, M.get(), F, IRB, nullptr);

    IRB.CreateRet(IRB.getInt64(400));
//    IRB.SetInsertPoint(BEGIN);
//    Value* r = IRB.getInt64(200);;
//    IRB.CreateRet(r);

//    verifyFunction(*F, &outs());
    verifyModule(*M, &outs());

    M.get()->print(outs(), nullptr);
}
