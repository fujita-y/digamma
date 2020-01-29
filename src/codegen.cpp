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
/*
llc --march=x86-64 --x86-asm-syntax=intel
*/

/*

extend vm_cont_rec_t to support native cont?

(current-environment (system-environment))
(define (n m) (list 1 (m) 3))
(n (lambda () 2)) ; => (1 2 3)
(closure-code n)
(closure-compile n)


(current-environment (system-environment))
(define c)
(define (n m) (list 1 (m) 3))
(n (lambda () (call/cc (lambda (k) (set! c k) 2)))) ; => (1 2 3)
(c 1000) ; => (1 1000 3)

(closure-code n)
(closure-compile n)



 ; => (1 2 3)
(closure-code n)
(closure-compile n)


(
 (push.const . 1)
 (call
   (apply.iloc (0 . 0)))
 (push)
 (push.const . 3)
 (ret.subr #<subr list>)
)

(current-environment (system-environment))
(define (n) (list 1 2 3))
(closure-compile n)

- unsupported instruction push.const
- unsupported instruction apply.iloc
- unsupported instruction push
- unsupported instruction push.const
- unsupported instruction ret.subr

(current-environment (system-environment))
(backtrace #f)
(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))
(closure-code fib)
(closure-compile fib)

((<n.iloc (0 . 0) 2)
 (if.true
    (ret.iloc 0 . 0))
 (call
    (push.n+.iloc (0 . 0) -1)
    (apply.gloc #<gloc fib>))
 (push)
 (call
    (push.n+.iloc (0 . 0) -2)
    (apply.gloc #<gloc fib>))
 (push)
 (ret.subr #<subr +>))
*/

#include "codegen.h"
#include "arith.h"
#include "uuid.h"

#include "llvm/IR/Verifier.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Error.h"

#define DECLEAR_COMMON_TYPES \
    auto IntptrTy = (sizeof(intptr_t) == 4 ? Type::getInt32Ty(C) : Type::getInt64Ty(C)); \
    auto IntptrPtrTy = sizeof(intptr_t) == 4 ? Type::getInt32PtrTy(C) : Type::getInt64PtrTy(C); \
    auto VoidTy = Type::getVoidTy(C); \
    std::vector<Type*> argTypes; \
    argTypes.push_back(IntptrPtrTy); \
    auto returnType = IntptrTy; \

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

#define INST_NATIVE     (vm->opcode_to_instruction(VMOP_NATIVE))
#define CONS(a, d)      make_pair(vm->m_heap, (a), (d))
#define LIST1(e1)       CONS((e1), scm_nil)
#define LIST2(e1, e2)   CONS((e1), LIST1((e2)))

static int log2_of_intptr_size()
{
    if (sizeof(intptr_t) == 4) return 2;
    if (sizeof(intptr_t) == 8) return 3;
    return (int)log2(sizeof(intptr_t));
}

static ExitOnError ExitOnErr;

extern "C" void thunk_collect_stack(VM* vm, intptr_t acquire) {
    printf("- thunk_collect_stack(%p, %ld)\n", vm, acquire);
    vm->collect_stack(acquire);
}

extern "C" scm_obj_t* thunk_lookup_iloc(VM* vm, intptr_t depth, intptr_t index) {
    printf("- thunk_lookup_iloc(%p, %ld, %ld)\n", vm, depth, index);
    void* lnk = vm->m_env;
    intptr_t level = depth;
    while (level) { lnk = *(void**)lnk; level = level - 1; }
    vm_env_t env = (vm_env_t)((intptr_t)lnk - offsetof(vm_env_rec_t, up));
    return (scm_obj_t*)env - env->count + index;
}

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
    auto Context = llvm::make_unique<LLVMContext>();
    LLVMContext& C = *Context;
    DECLEAR_COMMON_TYPES;

    char module_id[40];
    uuid_v4(module_id, sizeof(module_id));
    char function_id[40];
    uuid_v4(function_id, sizeof(function_id));

    auto M = llvm::make_unique<Module>(module_id, C);
    Function* F = Function::Create(FunctionType::get(returnType, argTypes, false), Function::ExternalLinkage, function_id, M.get());

    BasicBlock* ENTRY = BasicBlock::Create(C, "entry", F);
    IRBuilder<> IRB(ENTRY);

    transform(C, M.get(), F, IRB, closure->code);

    verifyModule(*M, &outs());
    M.get()->print(outs(), nullptr);

    ExitOnErr(m_jit->addIRModule(std::move(ThreadSafeModule(std::move(M), std::move(Context)))));
    m_jit->getMainJITDylib().dump(llvm::outs());

    auto symbol = ExitOnErr(m_jit->lookup(function_id));
    intptr_t (*address)(intptr_t) = (intptr_t (*)(intptr_t))symbol.getAddress();

    vm->m_heap->write_barrier(closure->code);
    closure->code = LIST1(CONS(INST_NATIVE, CONS(intptr_to_integer(vm->m_heap, (intptr_t)address), closure->code)));

    printf("address:%p\n", address);
}

void
codegen_t::transform(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    while (inst != scm_nil) {
        switch (VM::instruction_to_opcode(CAAR(inst))) {
            case VMOP_CALL:
                F = emit_call(C, M, F, IRB, inst);
                break;
            case VMOP_IF_TRUE:
                emit_if_true(C, M, F, IRB, inst);
                break;
            case VMOP_PUSH:
                emit_push(C, M, F, IRB, inst);
                break;
            case VMOP_PUSH_CONST:
                emit_push_const(C, M, F, IRB, inst);
                break;
            case VMOP_RET_CONST:
                emit_ret_const(C, M, F, IRB, inst);
                break;
            case VMOP_APPLY_ILOC:
                emit_apply_iloc(C, M, F, IRB, inst);
                break;
            case VMOP_APPLY_GLOC:
                emit_apply_gloc(C, M, F, IRB, inst);
                break;
            case VMOP_RET_SUBR:
                emit_ret_subr(C, M, F, IRB, inst);
                break;
            case VMOP_RET_ILOC:
                emit_ret_iloc(C, M, F, IRB, inst);
                break;
            case VMOP_LT_N_ILOC:
                emit_lt_n_iloc(C, M, F, IRB, inst);
                break;
            case VMOP_PUSH_NADD_ILOC:
                emit_push_nadd_iloc(C, M, F, IRB, inst);
                break;
            default:
                printf("- unsupported instruction %s\n", ((scm_symbol_t)CAAR(inst))->name);
                break;
        }
        inst = CDR(inst);
    }
}

Function*
codegen_t::emit_call(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    DECLEAR_COMMON_TYPES;

    char cont_id[40];
    uuid_v4(cont_id, sizeof(cont_id));

    Function* K =
        Function::Create(
            FunctionType::get(returnType, argTypes, false),
            Function::ExternalLinkage,
            cont_id,
            M);

    BasicBlock* RETURN = BasicBlock::Create(C, "entry", K);

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

    // cont->pc = CDR(m_pc);
    CREATE_STORE_CONT_REC(cont, pc, VALUE_INTPTR(CDR(inst)));

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

    // m_pc = OPERANDS;
    scm_obj_t operands = CDAR(inst);
    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(operands));

    // m_trace = m_trace_tail = scm_unspecified;
    CREATE_STORE_VM_REG(vm, m_trace, VALUE_INTPTR(scm_unspecified));
    CREATE_STORE_VM_REG(vm, m_trace_tail, VALUE_INTPTR(scm_unspecified));

    // continue emit code in operands
    transform(C, M, F, IRB, operands);

    IRB.SetInsertPoint(cond_false);

    // COLLECT_STACK_CONT_REC
    auto thunk_collect_stack = M->getOrInsertFunction("thunk_collect_stack", VoidTy, IntptrPtrTy, IntptrTy);
    IRB.CreateCall(thunk_collect_stack, {vm, VALUE_INTPTR(sizeof(vm_cont_rec_t))});
    IRB.CreateBr(cond_true);

    IRB.SetInsertPoint(RETURN);
    return K;
}

void
codegen_t::emit_push_const(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto stack_limit = CREATE_LOAD_VM_REG(vm, m_stack_limit);
    BasicBlock* cond_true = BasicBlock::Create(C, "cond_true", F);
    BasicBlock* cond_false = BasicBlock::Create(C, "cond_false", F);
    Value* cond = IRB.CreateICmpULT(sp, stack_limit);
    IRB.CreateCondBr(cond, cond_true, cond_false);

    IRB.SetInsertPoint(cond_true);
    auto sp_0 = IRB.CreateBitOrPointerCast(sp, IntptrPtrTy);
    IRB.CreateStore(VALUE_INTPTR(operands), sp_0);
    CREATE_STORE_VM_REG(vm, m_sp, IRB.CreateAdd(sp, VALUE_INTPTR(sizeof(intptr_t))));

    IRB.SetInsertPoint(cond_false);
    auto thunk_collect_stack = M->getOrInsertFunction("thunk_collect_stack", VoidTy, IntptrPtrTy, IntptrTy);
    IRB.CreateCall(thunk_collect_stack, {vm, VALUE_INTPTR(sizeof(scm_obj_t))} );
    IRB.CreateBr(cond_true);

    IRB.SetInsertPoint(cond_true);
}

void
codegen_t::emit_push(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    DECLEAR_COMMON_TYPES;
    auto vm = F->arg_begin();
    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto stack_limit = CREATE_LOAD_VM_REG(vm, m_stack_limit);
    BasicBlock* cond_true = BasicBlock::Create(C, "cond_true", F);
    BasicBlock* cond_false = BasicBlock::Create(C, "cond_false", F);
    Value* cond = IRB.CreateICmpULT(sp, stack_limit);
    IRB.CreateCondBr(cond, cond_true, cond_false);

    IRB.SetInsertPoint(cond_true);
    auto sp_0 = IRB.CreateBitOrPointerCast(sp, IntptrPtrTy);
    auto value = CREATE_LOAD_VM_REG(vm, m_value);
    IRB.CreateStore(value, sp_0);
    CREATE_STORE_VM_REG(vm, m_sp, IRB.CreateAdd(sp, VALUE_INTPTR(sizeof(intptr_t))));

    IRB.SetInsertPoint(cond_false);
    auto thunk_collect_stack = M->getOrInsertFunction("thunk_collect_stack", VoidTy, IntptrPtrTy, IntptrTy);
    IRB.CreateCall(thunk_collect_stack, {vm, VALUE_INTPTR(sizeof(scm_obj_t))} );
    IRB.CreateBr(cond_true);

    IRB.SetInsertPoint(cond_true);
}

void
codegen_t::emit_ret_const(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    auto val = VALUE_INTPTR(operands);
    CREATE_STORE_VM_REG(vm, m_value, val);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

Value*
codegen_t::emit_lookup_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t loc)
{
    DECLEAR_COMMON_TYPES;
    auto vm = F->arg_begin();
    intptr_t depth = FIXNUM(CAR(loc));
    intptr_t index = FIXNUM(CDR(loc));
    auto thunk_lookup_iloc = M->getOrInsertFunction("thunk_lookup_iloc", IntptrPtrTy, IntptrPtrTy, IntptrTy, IntptrTy);
    return IRB.CreateCall(thunk_lookup_iloc, {vm, VALUE_INTPTR(depth), VALUE_INTPTR(index)});
}

void
codegen_t::emit_apply_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    auto val = IRB.CreateLoad(emit_lookup_iloc(C, M, F, IRB, CAR(operands)));
    //auto val = VALUE_INTPTR(MAKEFIXNUM(100));

    CREATE_STORE_VM_REG(vm, m_value, val);

    BasicBlock* cond_true = BasicBlock::Create(C, "cond_true", F);
    BasicBlock* cond_false = BasicBlock::Create(C, "cond_false", F);
    auto cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
    IRB.CreateCondBr(cond, cond_true, cond_false);
    IRB.SetInsertPoint(cond_false);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_apply));
    IRB.SetInsertPoint(cond_true);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_error_apply_iloc));
}

void
codegen_t::emit_ret_subr(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto fp = CREATE_LOAD_VM_REG(vm, m_fp);

    auto argc = IRB.CreateAShr(IRB.CreateSub(sp, fp), VALUE_INTPTR(log2_of_intptr_size()));
    scm_subr_t subr = (scm_subr_t)CAR(operands);
    auto subrType = FunctionType::get(IntptrTy, {IntptrPtrTy, IntptrTy, IntptrTy}, false);
    auto ptr = ConstantExpr::getIntToPtr(VALUE_INTPTR(subr->adrs), subrType->getPointerTo());
    auto val = IRB.CreateCall(ptr, {vm, argc, fp});
    CREATE_STORE_VM_REG(vm, m_value, val);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void
codegen_t::emit_if_true(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    auto value = CREATE_LOAD_VM_REG(vm, m_value);

    BasicBlock* cond_true = BasicBlock::Create(C, "cond_true", F);
    BasicBlock* cond_false = BasicBlock::Create(C, "cond_false", F);
    auto cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_false));
    IRB.CreateCondBr(cond, cond_true, cond_false);
    IRB.SetInsertPoint(cond_false);
    transform(C, M, F, IRB, operands);
    IRB.CreateBr(cond_true);
    IRB.SetInsertPoint(cond_true);
}

void
codegen_t::emit_apply_gloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_apply));
}

void
codegen_t::emit_ret_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void
codegen_t::emit_lt_n_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
}

void
codegen_t::emit_push_nadd_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
}


/*
(current-environment (system-environment))
(define (a) (current-input-port))
(closure-compile a)

(current-environment (system-environment))
(define (n) (list 1 2 3))
(closure-compile n)

(current-environment (system-environment))
(backtrace #f)
(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))
(closure-code fib)
(closure-compile fib)


*/
