// Copyright (c) 2004-2020 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

/*
 (current-environment (system-environment)) (define (foo) 120) (closure-compile foo)
 (current-environment (system-environment))
 (define (foo) "hello")
 (closure-code foo)
 (closure-compile foo)
 (closure-code foo)
 (foo)
*/


#include "codegen.h"
#include "arith.h"
#include "uuid.h"

#include "llvm/IR/Verifier.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Error.h"

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

static ExitOnError ExitOnErr;

codegen_t::codegen_t()
{
    auto J = ExitOnErr(LLJITBuilder().create());
    auto D = J->getDataLayout();
    auto G = ExitOnErr(orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(D.getGlobalPrefix()));
    J->getMainJITDylib().setGenerator(G);
    m_jit = std::move(J);
}

void
codegen_t::compile(VM* vm, scm_closure_t closure)
{
    char module_id[40];
    uuid_v4(module_id, sizeof(module_id));
    char function_id[40];
    uuid_v4(function_id, sizeof(function_id));

    auto Context = llvm::make_unique<LLVMContext>();
    LLVMContext& C = *Context;
    DECLEAR_INTPTR_TYPES;

    auto M = llvm::make_unique<Module>(module_id, C);

    std::vector<Type*> argTypes;
    argTypes.push_back(IntptrPtrTy);
    Type* returnType = IntptrTy;
    Function* F = Function::Create(FunctionType::get(returnType, argTypes, false), Function::ExternalLinkage, function_id, M.get());

    BasicBlock* BEGIN = BasicBlock::Create(C, "begin", F);
    IRBuilder<> IRB(BEGIN);

    transform(C, F, IRB, vm, closure->code);

    verifyModule(*M, &outs());
    M.get()->print(outs(), nullptr);

    ExitOnErr(m_jit->addIRModule(std::move(ThreadSafeModule(std::move(M), std::move(Context)))));
    m_jit->getMainJITDylib().dump(llvm::outs());

    auto symbol = ExitOnErr(m_jit->lookup(function_id));
    intptr_t (*address)(intptr_t) = (intptr_t (*)(intptr_t))symbol.getAddress();

    // rewrite closure->code reference ((native . <address>))
    // ? how to create const pairs in data segment

    vm->m_heap->write_barrier(closure->code);
    closure->code = LIST1(CONS(INST_NATIVE, CONS(intptr_to_integer(vm->m_heap, (intptr_t)address), closure->code)));

    printf("address:%p\n", address);
}

void
codegen_t::transform(LLVMContext& C, Function* F, IRBuilder<>& IRB, VM* vm, scm_obj_t code)
{
    while (code != scm_nil) {
        scm_obj_t operands = CDAR(code);
        switch (vm->instruction_to_opcode(CAAR(code))) {
            case VMOP_RET_CONST:
                emit_ret_const(C, F, IRB, operands);
                break;
            default:
                fatal("unsupported instruction");
        }
        code = CDR(code);
    }
    // emit_push_iloc(C, F, IRB, nullptr);
}

void
codegen_t::emit_ret_const(LLVMContext& C, Function* F, IRBuilder<>& IRB, scm_obj_t operands)
{
    DECLEAR_INTPTR_TYPES;
    auto vm = F->arg_begin();
    auto val = VALUE_INTPTR(operands);
    CREATE_STORE_VM_REG(vm, m_value, val);
    IRB.CreateRet(VALUE_INTPTR(VM::native_return_pop_cont));
}

void
codegen_t::emit_push_iloc(LLVMContext& C, Function* F, IRBuilder<>& IRB, scm_obj_t operands)
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
