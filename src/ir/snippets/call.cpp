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
    auto IntptrPtrTy = sizeof(intptr_t) == 4 ? Type::getInt32PtrTy(C) : Type::getInt64PtrTy(C); \
    std::vector<Type*> argTypes; \
    argTypes.push_back(IntptrPtrTy); \
    auto returnType = IntptrTy;

#define VALUE_INTPTR(_VAL_) \
    (sizeof(intptr_t) == 4 ? IRB.getInt32((intptr_t)(_VAL_)) : IRB.getInt64((intptr_t)(_VAL_)))

#define CREATE_LOAD_VM_REG(_VM_,_REG_) \
    (IRB.CreateLoad(IntptrTy, IRB.CreateGEP(_VM_, IRB.getInt32(offsetof(VM, _REG_) / sizeof(intptr_t)))))

#define CREATE_STORE_VM_REG(_VM_,_REG_,_VAL_) \
    (IRB.CreateStore(_VAL_, IRB.CreateGEP(_VM_, IRB.getInt32(offsetof(VM, _REG_) / sizeof(intptr_t)))))

#define CREATE_LOAD_CONT_REC(_CONT_,_REC_) \
    (IRB.CreateLoad(IntptrTy, IRB.CreateGEP(_CONT_, IRB.getInt32(offsetof(vm_cont_rec_t, _REC_) / sizeof(intptr_t)))))

#define CREATE_STORE_CONT_REC(_CONT_,_REC_,_VAL_) \
    (IRB.CreateStore(_VAL_, IRB.CreateGEP(_CONT_, IRB.getInt32(offsetof(vm_cont_rec_t, _REC_) / sizeof(intptr_t)))))

extern "C" void thunk_collect_stack(VM* vm, intptr_t acquire) {
    //nop
}

#define INST_NATIVE     (vm->opcode_to_instruction(VMOP_NATIVE))
#define CONS(a, d)      make_pair(vm->m_heap, (a), (d))
#define LIST1(e1)       CONS((e1), scm_nil)
#define LIST2(e1, e2)   CONS((e1), LIST1((e2)))

/*
                if ((uintptr_t)m_sp + sizeof(vm_cont_rec_t) < (uintptr_t)m_stack_limit) {
                    vm_cont_t cont = (vm_cont_t)m_sp;
                    cont->trace = m_trace;
                    cont->fp = m_fp;
                    cont->pc = CDR(m_pc);
                    cont->code = NULL;
                    cont->env = m_env;
                    cont->up = m_cont;
                    m_sp = m_fp = (scm_obj_t*)(cont + 1);
                    m_cont = &cont->up;
                    m_pc = OPERANDS;
                    m_trace = m_trace_tail = scm_unspecified;
                    goto loop;
                }
                goto COLLECT_STACK_CONT_REC;
*/

void emit_call(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    char cont_id[40];
    strncpy(cont_id, "foo", sizeof(cont_id) - 1);

    DECLEAR_INTPTR_TYPES;

    Function* K =
        Function::Create(
            FunctionType::get(returnType, argTypes, false),
            Function::ExternalLinkage,
            cont_id,
            M);

    BasicBlock* ENTRY = BasicBlock::Create(C, "entry", K);

    auto vm = F->arg_begin();
    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto stack_limit = CREATE_LOAD_VM_REG(vm, m_stack_limit);

    BasicBlock* cond_true = BasicBlock::Create(C, "cond_true", F);
    BasicBlock* cond_false = BasicBlock::Create(C, "cond_false", F);
    // check stack
    Value* cond = IRB.CreateICmpULT(IRB.CreateAdd(sp, VALUE_INTPTR(sizeof(vm_cont_rec_t))), stack_limit);
    IRB.CreateCondBr(cond, cond_true, cond_false);
    IRB.SetInsertPoint(cond_true);

    // vm_cont_t cont = (vm_cont_t)m_sp;
    auto cont = IRB.CreateBitOrPointerCast(sp, IntptrPtrTy);

    // cont->trace = m_trace;
    CREATE_STORE_CONT_REC(cont, trace, CREATE_LOAD_VM_REG(vm, m_trace));

    // cont->fp = m_fp;
    CREATE_STORE_CONT_REC(cont, fp, CREATE_LOAD_VM_REG(vm, m_fp));

    if (inst) {
        // cont->pc = CDR(m_pc);
        CREATE_STORE_CONT_REC(cont, pc, VALUE_INTPTR(CDR(inst)));
    }

    // cont->code = NULL;
    CREATE_STORE_CONT_REC(cont, code, IRB.CreateBitOrPointerCast(K, IntptrTy));

    // cont->env = m_env;
    CREATE_STORE_CONT_REC(cont, env, CREATE_LOAD_VM_REG(vm, m_env));

    // cont->up = m_cont;
    CREATE_STORE_CONT_REC(cont, up, CREATE_LOAD_VM_REG(vm, m_cont));

    // m_sp = m_fp = (scm_obj_t*)(cont + 1);
    auto ea1 = IRB.CreateBitOrPointerCast(IRB.CreateGEP(cont, VALUE_INTPTR(sizeof(vm_cont_rec_t) / sizeof(intptr_t))), IntptrTy);
    CREATE_STORE_VM_REG(vm, m_sp, ea1);
    CREATE_STORE_VM_REG(vm, m_fp, ea1);

    // m_cont = &cont->up;
    auto ea2 = IRB.CreateBitOrPointerCast(IRB.CreateGEP(cont, VALUE_INTPTR(offsetof(vm_cont_rec_t, up) / sizeof(intptr_t))), IntptrTy);
    CREATE_STORE_VM_REG(vm, m_cont, ea2);

    if (inst) {
        // m_pc = OPERANDS;
        scm_obj_t operands = CDAR(inst);
        CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(operands));
    }

    // m_trace = m_trace_tail = scm_unspecified;
    CREATE_STORE_VM_REG(vm, m_trace, VALUE_INTPTR(scm_unspecified));
    CREATE_STORE_VM_REG(vm, m_trace_tail, VALUE_INTPTR(scm_unspecified));

    // continue emit code on operants
    if (inst) {
        /*
            scm_obj_t operands = CDAR(inst);
            transform(C, M, F, IRB, operands);
        */
    }
    IRB.CreateRet(sp);  // placeholder

    IRB.SetInsertPoint(cond_false);
    // COLLECT_STACK_CONT_REC
    IRB.CreateRet(sp);  // placeholder

    //
    IRB.SetInsertPoint(ENTRY);
}

#define INST_CALL     (vm->opcode_to_instruction(VMOP_CALL))

int main(int argc, char** argv) {
    llvm::InitLLVM X(argc, argv);
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    auto Context = llvm::make_unique<LLVMContext>();
    LLVMContext& C = *Context;

    DECLEAR_INTPTR_TYPES;

    auto M = llvm::make_unique<Module>("foobar_module", C);

    Function* F =
        Function::Create(
            FunctionType::get(returnType, argTypes, false),
            Function::ExternalLinkage,
            "foobar_function",
            M.get());

    BasicBlock* BEGIN = BasicBlock::Create(C, "begin", F);

    IRBuilder<> IRB(BEGIN);

    emit_call(C, M.get(), F, IRB, NULL);

    IRB.CreateRet(VALUE_INTPTR(400));
//    IRB.SetInsertPoint(BEGIN);
//    Value* r = IRB.getInt64(200);;
//    IRB.CreateRet(r);

//    verifyFunction(*F, &outs());
    verifyModule(*M, &outs());

    M.get()->print(outs(), nullptr);
}
