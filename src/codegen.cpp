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
#include "violation.h"
#include "uuid.h"

#include "llvm/IR/Verifier.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Error.h"

#define DECLEAR_COMMON_TYPES \
    auto IntptrTy = (sizeof(intptr_t) == 4 ? Type::getInt32Ty(C) : Type::getInt64Ty(C)); \
    auto IntptrPtrTy = sizeof(intptr_t) == 4 ? Type::getInt32PtrTy(C) : Type::getInt64PtrTy(C); \
    auto VoidTy = Type::getVoidTy(C);

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

#define CREATE_LOAD_GLOC_REC(_GLOC_,_REC_) \
    (IRB.CreateLoad(IntptrTy, IRB.CreateGEP(_GLOC_, IRB.getInt32(offsetof(scm_gloc_rec_t, _REC_) / sizeof(intptr_t)))))

#define CREATE_LOAD_ENV_REC(_ENV_,_REC_) \
    (IRB.CreateLoad(IntptrTy, IRB.CreateGEP(_ENV_, IRB.getInt32(offsetof(vm_env_rec_t, _REC_) / sizeof(intptr_t)))))

#define CREATE_LOAD_PAIR_REC(_PAIR_,_REC_) \
    (IRB.CreateLoad(IntptrTy, IRB.CreateGEP(_PAIR_, IRB.getInt32(offsetof(scm_pair_rec_t, _REC_) / sizeof(intptr_t)))))

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
    printf("- thunk_collect_stack(%p, %d)\n", vm, (int)acquire);
    vm->collect_stack(acquire);
}

extern "C" scm_obj_t* thunk_lookup_iloc(VM* vm, intptr_t depth, intptr_t index) {
//    printf("- thunk_lookup_iloc(%p, %d, %d)\n", vm, (int)depth, (int)index);
    void* lnk = vm->m_env;
    intptr_t level = depth;
    while (level) { lnk = *(void**)lnk; level = level - 1; }
    vm_env_t env = (vm_env_t)((intptr_t)lnk - offsetof(vm_env_rec_t, up));
    return (scm_obj_t*)env - env->count + index;
}

extern "C" void thunk_letrec_violation(VM* vm) {
//    printf("- thunk_letrec_violation(%p)\n", vm);
    letrec_violation(vm);
}

extern "C" void thunk_error_push_car_iloc(VM* vm, scm_obj_t obj) {
    if (obj == scm_undef) letrec_violation(vm);
    wrong_type_argument_violation(vm, "car", 0, "pair", obj, 1, &obj);
}

extern "C" void thunk_error_push_cdr_iloc(VM* vm, scm_obj_t obj) {
    if (obj == scm_undef) letrec_violation(vm);
    wrong_type_argument_violation(vm, "cdr", 0, "pair", obj, 1, &obj);
}

extern "C" scm_obj_t thunk_make_pair(VM* vm, scm_obj_t car, scm_obj_t cdr) {
    return make_pair(vm->m_heap, car, cdr);
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
    char module_id[40];
    uuid_v4(module_id, sizeof(module_id));
    char function_id[40];
    uuid_v4(function_id, sizeof(function_id));

    auto Context = llvm::make_unique<LLVMContext>();
    LLVMContext& C = *Context;
    DECLEAR_COMMON_TYPES;

    auto M = llvm::make_unique<Module>(module_id, C);
    Function* F = Function::Create(FunctionType::get(IntptrTy, {IntptrPtrTy}, false), Function::ExternalLinkage, function_id, M.get());
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
            case VMOP_ILOC0:
                emit_iloc0(C, M, F, IRB, inst);
                break;
            case VMOP_IF_NULLP_RET_CONST:
                emit_if_nullp_ret_const(C, M, F, IRB, inst);
                break;
            case VMOP_PUSH_CAR_ILOC:
                emit_push_car_iloc(C, M, F, IRB, inst);
                break;
            case VMOP_PUSH_ILOC0:
                emit_push_iloc0(C, M, F, IRB, inst);
                break;
            case VMOP_PUSH_CDR_ILOC:
                emit_push_cdr_iloc(C, M, F, IRB, inst);
                break;
            case VMOP_RET_CONS:
                emit_ret_cons(C, M, F, IRB, inst);
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
    char cont_id[40];
    uuid_v4(cont_id, sizeof(cont_id));

    DECLEAR_COMMON_TYPES;
    auto vm = F->arg_begin();
    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto stack_limit = CREATE_LOAD_VM_REG(vm, m_stack_limit);

    Function* K = Function::Create(FunctionType::get(IntptrTy, {IntptrPtrTy}, false), Function::ExternalLinkage, cont_id, M);
    BasicBlock* RETURN = BasicBlock::Create(C, "entry", K);

    // stack check
    BasicBlock* stack_true = BasicBlock::Create(C, "stack_true", F);
    BasicBlock* stack_false = BasicBlock::Create(C, "stack_false", F);
    Value* stack_cond = IRB.CreateICmpULT(IRB.CreateAdd(sp, VALUE_INTPTR(sizeof(vm_cont_rec_t))), stack_limit);
    IRB.CreateCondBr(stack_cond, stack_true, stack_false);
    // stack overflow
    IRB.SetInsertPoint(stack_false);
        // COLLECT_STACK_CONT_REC
        auto thunk_collect_stack = M->getOrInsertFunction("thunk_collect_stack", VoidTy, IntptrPtrTy, IntptrTy);
        IRB.CreateCall(thunk_collect_stack, {vm, VALUE_INTPTR(sizeof(vm_cont_rec_t))});
        IRB.CreateBr(stack_true);
    // stack ok
    IRB.SetInsertPoint(stack_true);
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
    // stack check
    BasicBlock* stack_true = BasicBlock::Create(C, "stack_true", F);
    BasicBlock* stack_false = BasicBlock::Create(C, "stack_false", F);
    Value* stack_cond = IRB.CreateICmpULT(sp, stack_limit);
    IRB.CreateCondBr(stack_cond, stack_true, stack_false);
    // stack overflow
    IRB.SetInsertPoint(stack_false);
        auto thunk_collect_stack = M->getOrInsertFunction("thunk_collect_stack", VoidTy, IntptrPtrTy, IntptrTy);
        IRB.CreateCall(thunk_collect_stack, {vm, VALUE_INTPTR(sizeof(scm_obj_t))} );
        IRB.CreateBr(stack_true);
    // stack ok
    IRB.SetInsertPoint(stack_true);
        auto sp_0 = IRB.CreateBitOrPointerCast(sp, IntptrPtrTy);
        IRB.CreateStore(VALUE_INTPTR(operands), sp_0);
        CREATE_STORE_VM_REG(vm, m_sp, IRB.CreateAdd(sp, VALUE_INTPTR(sizeof(intptr_t))));
}

void
codegen_t::emit_push(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    DECLEAR_COMMON_TYPES;
    auto vm = F->arg_begin();
    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto stack_limit = CREATE_LOAD_VM_REG(vm, m_stack_limit);
    // stack check
    BasicBlock* stack_true = BasicBlock::Create(C, "stack_true", F);
    BasicBlock* stack_false = BasicBlock::Create(C, "stack_false", F);
    Value* stack_cond = IRB.CreateICmpULT(sp, stack_limit);
    IRB.CreateCondBr(stack_cond, stack_true, stack_false);
    // stack overflow
    IRB.SetInsertPoint(stack_false);
        auto thunk_collect_stack = M->getOrInsertFunction("thunk_collect_stack", VoidTy, IntptrPtrTy, IntptrTy);
        IRB.CreateCall(thunk_collect_stack, {vm, VALUE_INTPTR(sizeof(scm_obj_t))} );
        IRB.CreateBr(stack_true);
    // stack ok
    IRB.SetInsertPoint(stack_true);
        auto sp_0 = IRB.CreateBitOrPointerCast(sp, IntptrPtrTy);
        auto value = CREATE_LOAD_VM_REG(vm, m_value);
        IRB.CreateStore(value, sp_0);
        CREATE_STORE_VM_REG(vm, m_sp, IRB.CreateAdd(sp, VALUE_INTPTR(sizeof(intptr_t))));
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
codegen_t::emit_lookup_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, intptr_t depth, intptr_t index)
{
    DECLEAR_COMMON_TYPES;
    auto vm = F->arg_begin();
    if (depth == 0 && index == 0) {
        auto env = IRB.CreateBitOrPointerCast(IRB.CreateSub(CREATE_LOAD_VM_REG(vm, m_env), VALUE_INTPTR(offsetof(vm_env_rec_t, up))), IntptrPtrTy);
        auto count = CREATE_LOAD_ENV_REC(env, count);
        return IRB.CreateGEP(env, IRB.CreateNeg(count));
    } else if (depth == 0) {
        auto env = IRB.CreateBitOrPointerCast(IRB.CreateSub(CREATE_LOAD_VM_REG(vm, m_env), VALUE_INTPTR(offsetof(vm_env_rec_t, up))), IntptrPtrTy);
        auto count = CREATE_LOAD_ENV_REC(env, count);
        return IRB.CreateGEP(env, IRB.CreateSub(VALUE_INTPTR(index), count));
    }
    auto thunk_lookup_iloc = M->getOrInsertFunction("thunk_lookup_iloc", IntptrPtrTy, IntptrPtrTy, IntptrTy, IntptrTy);
    return IRB.CreateCall(thunk_lookup_iloc, {vm, VALUE_INTPTR(depth), VALUE_INTPTR(index)});
}

Value*
codegen_t::emit_lookup_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t loc)
{
    return emit_lookup_iloc(C, M, F, IRB, FIXNUM(CAR(loc)), FIXNUM(CDR(loc)));
}

void
codegen_t::emit_apply_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    auto val = IRB.CreateLoad(emit_lookup_iloc(C, M, F, IRB, CAR(operands)));
    CREATE_STORE_VM_REG(vm, m_value, val);
    BasicBlock* undef_true = BasicBlock::Create(C, "undef_true", F);
    BasicBlock* undef_false = BasicBlock::Create(C, "undef_false", F);
    auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
    IRB.CreateCondBr(undef_cond, undef_true, undef_false);
    // valid
    IRB.SetInsertPoint(undef_false);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_apply));
    // invalid
    IRB.SetInsertPoint(undef_true);
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
    BasicBlock* f9h_true = BasicBlock::Create(C, "f9h_true", F);
    BasicBlock* f9h_false = BasicBlock::Create(C, "f9h_false", F);
    auto f9h_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_false));
    IRB.CreateCondBr(f9h_cond, f9h_true, f9h_false);
    // taken
    IRB.SetInsertPoint(f9h_false);
    transform(C, M, F, IRB, operands);
    // not taken
    IRB.SetInsertPoint(f9h_true);
}

void
codegen_t::emit_apply_gloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    auto gloc = IRB.CreateBitOrPointerCast(VALUE_INTPTR(CAR(operands)), IntptrPtrTy);
    auto val = CREATE_LOAD_GLOC_REC(gloc, value);
    CREATE_STORE_VM_REG(vm, m_value, val);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_apply));
}

void
codegen_t::emit_ret_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    auto val = IRB.CreateLoad(emit_lookup_iloc(C, M, F, IRB, operands));
    CREATE_STORE_VM_REG(vm, m_value, val);
    BasicBlock* undef_true = BasicBlock::Create(C, "undef_true", F);
    BasicBlock* undef_false = BasicBlock::Create(C, "undef_false", F);
    auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
    IRB.CreateCondBr(undef_cond, undef_true, undef_false);
    // valid
    IRB.SetInsertPoint(undef_false);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
    // invalid
    IRB.SetInsertPoint(undef_true);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_error_ret_iloc));
}

void
codegen_t::emit_lt_n_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    auto val = IRB.CreateLoad(emit_lookup_iloc(C, M, F, IRB, CAR(operands)));
    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
    BasicBlock* nonfixnum_true = BasicBlock::Create(C, "nonfixnum_true", F);
    BasicBlock* nonfixnum_false = BasicBlock::Create(C, "nonfixnum_false", F);
    auto nonfixnum_cond = IRB.CreateICmpEQ(IRB.CreateAnd(val, 1), VALUE_INTPTR(0));
    IRB.CreateCondBr(nonfixnum_cond, nonfixnum_true, nonfixnum_false);
    // fixnum
    IRB.SetInsertPoint(nonfixnum_false);
        BasicBlock* cond_true = BasicBlock::Create(C, "cond_true", F);
        BasicBlock* cond_false = BasicBlock::Create(C, "cond_false", F);
        auto cond = IRB.CreateICmpSLT(val, VALUE_INTPTR(CADR(operands)));
        IRB.CreateCondBr(cond, cond_true, cond_false);
        // taken
        IRB.SetInsertPoint(cond_true);
        CREATE_STORE_VM_REG(vm, m_value, VALUE_INTPTR(scm_true));
        IRB.CreateBr(CONTINUE);
        // not taken
        IRB.SetInsertPoint(cond_false);
        CREATE_STORE_VM_REG(vm, m_value, VALUE_INTPTR(scm_false));
        IRB.CreateBr(CONTINUE);
    // others
    IRB.SetInsertPoint(nonfixnum_true);
        // TODO: fallback FALLBACK_LT_N_ILOC
        IRB.CreateBr(CONTINUE);
    IRB.SetInsertPoint(CONTINUE);
}

void
codegen_t::emit_push_nadd_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto stack_limit = CREATE_LOAD_VM_REG(vm, m_stack_limit);
    // stack check
    BasicBlock* stack_true = BasicBlock::Create(C, "stack_true", F);
    BasicBlock* stack_false = BasicBlock::Create(C, "stack_false", F);
    Value* stack_cond = IRB.CreateICmpULT(sp, stack_limit);
    IRB.CreateCondBr(stack_cond, stack_true, stack_false);
    // stack overflow
    IRB.SetInsertPoint(stack_false);
        auto thunk_collect_stack = M->getOrInsertFunction("thunk_collect_stack", VoidTy, IntptrPtrTy, IntptrTy);
        IRB.CreateCall(thunk_collect_stack, {vm, VALUE_INTPTR(sizeof(scm_obj_t))} );
        IRB.CreateBr(stack_true);
    // stack ok
    IRB.SetInsertPoint(stack_true);
        BasicBlock* nonfixnum_true = BasicBlock::Create(C, "nonfixnum_true", F);
        BasicBlock* nonfixnum_false = BasicBlock::Create(C, "nonfixnum_false", F);
        auto val = IRB.CreateLoad(emit_lookup_iloc(C, M, F, IRB, CAR(operands)));
        auto nonfixnum_cond = IRB.CreateICmpEQ(IRB.CreateAnd(val, 1), VALUE_INTPTR(0));
        IRB.CreateCondBr(nonfixnum_cond, nonfixnum_true, nonfixnum_false);
        // fixnum
        IRB.SetInsertPoint(nonfixnum_false);
            auto n = IRB.CreateAdd(IRB.CreateAShr(val, VALUE_INTPTR(1)), IRB.CreateAShr(VALUE_INTPTR(CADR(operands)), VALUE_INTPTR(1)));
            // TODO: check if n is out fixnum then IRB.CreateBr(nonfixnum_true);
            auto sp_0 = IRB.CreateBitOrPointerCast(sp, IntptrPtrTy);
            IRB.CreateStore(IRB.CreateAdd(IRB.CreateShl(n, VALUE_INTPTR(1)), VALUE_INTPTR(1)), sp_0);
            CREATE_STORE_VM_REG(vm, m_sp, IRB.CreateAdd(sp, VALUE_INTPTR(sizeof(intptr_t))));
            IRB.CreateBr(CONTINUE);
        // others
        IRB.SetInsertPoint(nonfixnum_true);
            // TODO: fallback FALLBACK_PUSH_NADD_ILOC
            IRB.CreateBr(CONTINUE);

    IRB.SetInsertPoint(CONTINUE);
}

void
codegen_t::emit_iloc0(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    auto val = IRB.CreateLoad(emit_lookup_iloc(C, M, F, IRB, 0, FIXNUM(operands)));
    CREATE_STORE_VM_REG(vm, m_value, val);
    BasicBlock* undef_true = BasicBlock::Create(C, "undef_true", F);
    BasicBlock* undef_false = BasicBlock::Create(C, "undef_false", F);
    auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
    IRB.CreateCondBr(undef_cond, undef_true, undef_false);
    // invalid
    IRB.SetInsertPoint(undef_true);
    auto thunk_letrec_violation = M->getOrInsertFunction("thunk_letrec_violation", VoidTy, IntptrPtrTy);
    IRB.CreateCall(thunk_letrec_violation, {vm});
    IRB.CreateBr(undef_false);
    // valid
    IRB.SetInsertPoint(undef_false);
}

void
codegen_t::emit_if_nullp_ret_const(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    auto value = CREATE_LOAD_VM_REG(vm, m_value);
    BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
    BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
    auto taken_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_nil));
    IRB.CreateCondBr(taken_cond, taken_true, taken_false);
    // taken
    IRB.SetInsertPoint(taken_true);
    CREATE_STORE_VM_REG(vm, m_value, VALUE_INTPTR(operands));
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
    // not taken
    IRB.SetInsertPoint(taken_false);
}

void
codegen_t::emit_push_iloc0(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto stack_limit = CREATE_LOAD_VM_REG(vm, m_stack_limit);
    // stack check
    BasicBlock* stack_true = BasicBlock::Create(C, "stack_true", F);
    BasicBlock* stack_false = BasicBlock::Create(C, "stack_false", F);
    Value* stack_cond = IRB.CreateICmpULT(sp, stack_limit);
    IRB.CreateCondBr(stack_cond, stack_true, stack_false);
    // stack overflow
    IRB.SetInsertPoint(stack_false);
        auto thunk_collect_stack = M->getOrInsertFunction("thunk_collect_stack", VoidTy, IntptrPtrTy, IntptrTy);
        IRB.CreateCall(thunk_collect_stack, {vm, VALUE_INTPTR(sizeof(scm_obj_t))} );
        IRB.CreateBr(stack_true);
    // stack ok
    IRB.SetInsertPoint(stack_true);
        auto val = IRB.CreateLoad(emit_lookup_iloc(C, M, F, IRB, 0, FIXNUM(operands)));
        auto sp_0 = IRB.CreateBitOrPointerCast(sp, IntptrPtrTy);
        IRB.CreateStore(val, sp_0);
        CREATE_STORE_VM_REG(vm, m_sp, IRB.CreateAdd(sp, VALUE_INTPTR(sizeof(intptr_t))));
}

void
codegen_t::emit_push_car_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto stack_limit = CREATE_LOAD_VM_REG(vm, m_stack_limit);
    // stack check
    BasicBlock* stack_true = BasicBlock::Create(C, "stack_true", F);
    BasicBlock* stack_false = BasicBlock::Create(C, "stack_false", F);
    Value* stack_cond = IRB.CreateICmpULT(sp, stack_limit);
    IRB.CreateCondBr(stack_cond, stack_true, stack_false);
    // stack overflow
    IRB.SetInsertPoint(stack_false);
        auto thunk_collect_stack = M->getOrInsertFunction("thunk_collect_stack", VoidTy, IntptrPtrTy, IntptrTy);
        IRB.CreateCall(thunk_collect_stack, {vm, VALUE_INTPTR(sizeof(scm_obj_t))} );
        IRB.CreateBr(stack_true);
    // stack ok
    IRB.SetInsertPoint(stack_true);
        auto val = IRB.CreateLoad(emit_lookup_iloc(C, M, F, IRB, CAR(operands)));
        // check if pair
        BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
        BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
        auto pair_cond = IRB.CreateICmpEQ(IRB.CreateAnd(val, 1), VALUE_INTPTR(0));
        IRB.CreateCondBr(pair_cond, pair_true, pair_false);
        // nonpair
        IRB.SetInsertPoint(pair_false);
            auto thunk_error_push_car_iloc = M->getOrInsertFunction("thunk_error_push_car_iloc", VoidTy, IntptrPtrTy, IntptrTy);
            IRB.CreateCall(thunk_error_push_car_iloc, {vm, val});
            IRB.CreateBr(CONTINUE);
        // pair
        IRB.SetInsertPoint(pair_true);
            auto sp_0 = IRB.CreateBitOrPointerCast(sp, IntptrPtrTy);
            IRB.CreateStore(CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(val, IntptrPtrTy), car), sp_0);
            CREATE_STORE_VM_REG(vm, m_sp, IRB.CreateAdd(sp, VALUE_INTPTR(sizeof(intptr_t))));
            IRB.CreateBr(CONTINUE);

    IRB.SetInsertPoint(CONTINUE);
}

void
codegen_t::emit_push_cdr_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto stack_limit = CREATE_LOAD_VM_REG(vm, m_stack_limit);
    // stack check
    BasicBlock* stack_true = BasicBlock::Create(C, "stack_true", F);
    BasicBlock* stack_false = BasicBlock::Create(C, "stack_false", F);
    Value* stack_cond = IRB.CreateICmpULT(sp, stack_limit);
    IRB.CreateCondBr(stack_cond, stack_true, stack_false);
    // stack overflow
    IRB.SetInsertPoint(stack_false);
        auto thunk_collect_stack = M->getOrInsertFunction("thunk_collect_stack", VoidTy, IntptrPtrTy, IntptrTy);
        IRB.CreateCall(thunk_collect_stack, {vm, VALUE_INTPTR(sizeof(scm_obj_t))} );
        IRB.CreateBr(stack_true);
    // stack ok
    IRB.SetInsertPoint(stack_true);
        auto val = IRB.CreateLoad(emit_lookup_iloc(C, M, F, IRB, CAR(operands)));
        // check if pair
        BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
        BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
        auto pair_cond = IRB.CreateICmpEQ(IRB.CreateAnd(val, 1), VALUE_INTPTR(0));
        IRB.CreateCondBr(pair_cond, pair_true, pair_false);
        // nonpair
        IRB.SetInsertPoint(pair_false);
            auto thunk_error_push_cdr_iloc = M->getOrInsertFunction("thunk_error_push_cdr_iloc", VoidTy, IntptrPtrTy, IntptrTy);
            IRB.CreateCall(thunk_error_push_cdr_iloc, {vm, val});
            IRB.CreateBr(CONTINUE);
        // pair
        IRB.SetInsertPoint(pair_true);
            auto sp_0 = IRB.CreateBitOrPointerCast(sp, IntptrPtrTy);
            IRB.CreateStore(CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(val, IntptrPtrTy), cdr), sp_0);
            CREATE_STORE_VM_REG(vm, m_sp, IRB.CreateAdd(sp, VALUE_INTPTR(sizeof(intptr_t))));
            IRB.CreateBr(CONTINUE);

    IRB.SetInsertPoint(CONTINUE);
}

void
codegen_t::emit_ret_cons(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto val = CREATE_LOAD_VM_REG(vm, m_value);
    auto sp_minus_1 = IRB.CreateLoad(IRB.CreateGEP(IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-1)));
    auto thunk_make_pair = M->getOrInsertFunction("thunk_make_pair", IntptrTy, IntptrPtrTy, IntptrTy, IntptrTy);
    CREATE_STORE_VM_REG(vm, m_value, IRB.CreateCall(thunk_make_pair, {vm, sp_minus_1, val}));
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
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

(define start (time-usage))
(fib 32)
(define end (time-usage))
(map - end start)

(1.0635550022125244 1.061279 0.0)
(1.1754271984100342 1.1738849999999998 0.0)
(1.1603269577026367 1.15823 0.0)

(0.8235650062561035 0.8224790000000004 0.0)
(0.8277268409729004 0.8261890000000003 0.00011599999999999111)
(0.7562940120697021 0.7488189999999997 0.007497999999999991)

(0.29166603088378906 0.2916669999999999 0.0)
-> 67.8%
(0.19778013229370117 0.19778099999999998 0.0)

(current-environment (system-environment))
(backtrace #f)
(define map-1
    (lambda (proc lst)
    (cond ((null? lst) '())
            (else
            (cons (proc (car lst))
                    (map-1 proc (cdr lst)))))))
(closure-code map-1)
(closure-compile map-1)

(define start (time-usage))
(define sink)
(define (b)
    (let loop ((n 1))
        (cond ((< n 100000)
               (map-1 - '(1 2 3 4 6 7 8 9 10 1 2 3 4 6 7 8 9 101 2 3 4 6 7 8 9 101 2 3 4 6 7 8 9 101 2 3 4 6 7 8 9 101 2 3 4 6 7 8 9 101 2 3 4 6 7 8 9 10))
               (loop (+ n 1))))))
(b)
(define end (time-usage))
(map - end start)

(define start (time-usage))
(define sink)
(define (b n)
    (cond ((< n 100000)
            (map-1 - '(1 2 3 4 6 7 8 9 10 1 2 3 4 6 7 8 9 101 2 3 4 6 7 8 9 101 2 3 4 6 7 8 9 101 2 3 4 6 7 8 9 101 2 3 4 6 7 8 9 101 2 3 4 6 7 8 9 10))
            (b (+ n 1)))))
(b 1)
(define end (time-usage))
(map - end start)

(1.285045862197876 1.563275 0.0)
(1.137603998184204 1.4177029999999995 0.0)
(1.1186790466308594 1.4051300000000007 0.0)

- unsupported instruction iloc.0
- unsupported instruction if.null?.ret.const
- unsupported instruction push.car.iloc
- unsupported instruction push.iloc.0
- unsupported instruction push.cdr.iloc
- unsupported instruction ret.cons

  (define-syntax time
    (syntax-rules ()
      ((_ expr)
       (destructuring-bind (real-start user-start sys-start) (time-usage)
         (let ((result (apply (lambda () expr) '())))
           (destructuring-bind (real-end user-end sys-end) (time-usage)
             (format #t
                     "~%;;~10,6f real ~11,6f user ~11,6f sys~%~!"
                     (- real-end real-start)
                     (- user-end user-start)
                     (- sys-end sys-start)))
           result)))))

(backtrace #f)
(define map-1
  (lambda (proc lst)
    (if (null? lst)
        '()
        (cons (proc (car lst))
              (map-1 proc (cdr lst))))))

(collect #t)
(define lst (make-list 100 1))
(define minus (lambda (x) (- x)))
(define sink #f)
(time (set! sink (map-1 minus lst)))

;;  0.000965 real    0.000813 user    0.000150 sys
(closure-compile map-1)

;;  0.000113 real    0.000076 user    0.000035 sys

*/
