
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

int main() {
    auto C = llvm::make_unique<LLVMContext>();
    auto M = llvm::make_unique<Module>("test", *C);

    auto Int64Ty = Type::getInt64Ty(*C);
    auto Int64PtrTy = Type::getInt64PtrTy(*C);

    auto IntptrTy = sizeof(intptr_t) == 4 ? Type::getInt32PtrTy(*C) : Type::getInt64PtrTy(*C);
    auto IntptrPtrTy = sizeof(intptr_t) == 4 ? Type::getInt32PtrTy(*C) : Type::getInt64PtrTy(*C);

    std::vector<Type*> argTypes;
    argTypes.push_back(Int64PtrTy);

    Type* returnType = Int64Ty;

    Function* get2nd =
        Function::Create(
            FunctionType::get(returnType, argTypes, false),
            Function::ExternalLinkage,
            "get2nd",
            M.get());

    BasicBlock *BB = BasicBlock::Create(*C, "EntryBlock", get2nd);
    IRBuilder<> IRB(BB);
    auto base = (*get2nd).arg_begin() + 0;

    // use getelementptr
    Value* index = IRB.getInt64(2);
    Value* ea = IRB.CreateGEP(base, index);
    Value* val1 = IRB.CreateLoad(Int64Ty, ea);

    // use extractelement
    //Value* val2 = IRB.CreateExtractElement(base, 2);

    IRB.CreateRet(val1);

    verifyModule(*M, &outs());

    M.get()->print(outs(), nullptr);
}
