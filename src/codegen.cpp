// Copyright (c) 2004-2020 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "codegen.h"
#include "arith.h"
#include "printer.h"
#include "violation.h"
#include "uuid.h"

#include "llvm/IR/Verifier.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Error.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

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

#define CREATE_STORE_ENV_REC(_ENV_,_REC_,_VAL_) \
    (IRB.CreateStore(_VAL_, IRB.CreateGEP(_ENV_, IRB.getInt32(offsetof(vm_env_rec_t, _REC_) / sizeof(intptr_t)))))

#define CREATE_LEA_ENV_REC(_ENV_,_REC_) \
    (IRB.CreateBitOrPointerCast(IRB.CreateGEP(_ENV_, IRB.getInt32(offsetof(vm_env_rec_t, _REC_) / sizeof(intptr_t))), IntptrTy))

#define CREATE_LOAD_PAIR_REC(_PAIR_,_REC_) \
    (IRB.CreateLoad(IntptrTy, IRB.CreateGEP(_PAIR_, IRB.getInt32(offsetof(scm_pair_rec_t, _REC_) / sizeof(intptr_t)))))

#define CREATE_PUSH_VM_STACK(_VAL_) { \
            auto sp = CREATE_LOAD_VM_REG(vm, m_sp); \
            auto sp0 = IRB.CreateBitOrPointerCast(sp, IntptrPtrTy); \
            IRB.CreateStore(_VAL_, sp0); \
            CREATE_STORE_VM_REG(vm, m_sp, IRB.CreateAdd(sp, VALUE_INTPTR(sizeof(intptr_t)))); \
        }

#define CREATE_STACK_OVERFLOW_HANDLER(_BYTES_REQUIRED_) { \
            auto sp = CREATE_LOAD_VM_REG(vm, m_sp); \
            auto stack_limit = CREATE_LOAD_VM_REG(vm, m_stack_limit); \
            BasicBlock* stack_ok = BasicBlock::Create(C, "stack_ok", F); \
            BasicBlock* stack_overflow = BasicBlock::Create(C, "stack_overflow", F); \
            Value* stack_cond = IRB.CreateICmpULT(IRB.CreateAdd(sp, VALUE_INTPTR(_BYTES_REQUIRED_)), stack_limit); \
            IRB.CreateCondBr(stack_cond, stack_ok, stack_overflow); \
            IRB.SetInsertPoint(stack_overflow); \
            auto thunk_collect_stack = M->getOrInsertFunction("thunk_collect_stack", VoidTy, IntptrPtrTy, IntptrTy); \
            IRB.CreateCall(thunk_collect_stack, {vm, VALUE_INTPTR(_BYTES_REQUIRED_)}); \
            IRB.CreateBr(stack_ok); \
            IRB.SetInsertPoint(stack_ok); \
        }

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
    //printf("- thunk_collect_stack(%p, %d)\n", vm, (int)acquire);
    vm->collect_stack(acquire);
}

extern "C" scm_obj_t* thunk_lookup_iloc(VM* vm, intptr_t depth, intptr_t index) {
    //printf("- thunk_lookup_iloc(%p, %d, %d)\n", vm, (int)depth, (int)index);
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

extern "C" void thunk_error_lt_n_iloc(VM* vm, scm_obj_t obj, scm_obj_t operands) {
    //printf("- thunk_error_lt_n_iloc(%p, %p, %p)\n", vm, obj, operands);
    if (obj == scm_undef) letrec_violation(vm);
    scm_obj_t argv[2] = { obj, operands };
    wrong_type_argument_violation(vm, "comparison(< > <= >=)", 0, "real", argv[0], 2, argv);
}

extern "C" void thunk_error_push_nadd_iloc(VM* vm, scm_obj_t obj, scm_obj_t operands) {
    if (obj == scm_undef) letrec_violation(vm);
    scm_obj_t argv[2] = { obj, operands };
    wrong_type_argument_violation(vm, "operator(+ -)", 0, "number", argv[0], 2, argv);
}

extern "C" scm_obj_t thunk_make_pair(VM* vm, scm_obj_t car, scm_obj_t cdr) {
    return make_pair(vm->m_heap, car, cdr);
}

extern "C" intptr_t thunk_number_pred(scm_obj_t obj)
{
    //printf("- thunk_number_pred(%p) => %d\n", obj, number_pred(obj));
    return (intptr_t)number_pred(obj);
}

extern "C" intptr_t thunk_real_pred(scm_obj_t obj)
{
    //printf("- thunk_real_pred(%p) => %d\n", obj, real_pred(obj));
    return (intptr_t)real_pred(obj);
}

extern "C" intptr_t thunk_n_compare(VM* vm, scm_obj_t obj, scm_obj_t operands) {
//    printer_t prt(vm, vm->m_current_output);
//    prt.format("- thunk_n_compare ~s ~s => %d ~%~!", obj, operands, n_compare(vm->m_heap, obj, operands));
    return n_compare(vm->m_heap, obj, operands);
}

extern "C" scm_obj_t thunk_arith_add(VM* vm, scm_obj_t obj, scm_obj_t operands) {
//    printer_t prt(vm, vm->m_current_output);
//    prt.format("- thunk_arith_add ~s ~s => ~s ~%~!", obj, operands, arith_add(vm->m_heap, obj, operands));
    return arith_add(vm->m_heap, obj, operands);
}

codegen_t::codegen_t()
{
    auto J = ExitOnErr(LLJITBuilder().create());
    auto D = J->getDataLayout();
    auto G = ExitOnErr(orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(D.getGlobalPrefix()));
    J->getMainJITDylib().setGenerator(G);
    m_jit = std::move(J);
    define_prepare_call();
}

ThreadSafeModule
codegen_t::optimizeModule(ThreadSafeModule TSM) {
    Module &M = *TSM.getModule();
    PassManagerBuilder B;
    B.OptLevel = 2;
    B.SizeLevel = 1;

//  puts("=== IR before optimize ===");
//  M.print(outs(), nullptr);

    legacy::FunctionPassManager FPM(&M);
    B.populateFunctionPassManager(FPM);
    FPM.doInitialization();
    for (Function &F : M) FPM.run(F);
    FPM.doFinalization();

    legacy::PassManager MPM;
    B.populateModulePassManager(MPM);
    MPM.run(M);

//  puts("*** IR after optimize ***");
//  M.print(outs(), nullptr);

    return std::move(TSM);
}

void
codegen_t::define_prepare_call()
{
    auto Context = llvm::make_unique<LLVMContext>();
    LLVMContext& C = *Context;

    DECLEAR_COMMON_TYPES;

    auto M = llvm::make_unique<Module>("intrinsics", C);
    Function* F = Function::Create(FunctionType::get(VoidTy, {IntptrPtrTy, IntptrPtrTy}, false), Function::ExternalLinkage, "prepare_call", M.get());
//  Function* F = Function::Create(FunctionType::get(VoidTy, {IntptrPtrTy, IntptrPtrTy}, false), Function::LinkOnceAnyLinkage, "prepare_call", M.get());
//  F->setCallingConv(CallingConv::Fast);
    BasicBlock* ENTRY = BasicBlock::Create(C, "entry", F);
    IRBuilder<> IRB(ENTRY);
    auto vm = F->arg_begin();
    auto cont = F->arg_begin() + 1;

    CREATE_STORE_CONT_REC(cont, trace, CREATE_LOAD_VM_REG(vm, m_trace));
    // cont->fp = m_fp;
    CREATE_STORE_CONT_REC(cont, fp, CREATE_LOAD_VM_REG(vm, m_fp));
    // cont->env = m_env;
    CREATE_STORE_CONT_REC(cont, env, CREATE_LOAD_VM_REG(vm, m_env));
    // cont->up = m_cont;
    CREATE_STORE_CONT_REC(cont, up, CREATE_LOAD_VM_REG(vm, m_cont));
    // m_trace = m_trace_tail = scm_unspecified;
    CREATE_STORE_VM_REG(vm, m_trace, VALUE_INTPTR(scm_unspecified));
    CREATE_STORE_VM_REG(vm, m_trace_tail, VALUE_INTPTR(scm_unspecified));
    // m_sp = m_fp = (scm_obj_t*)(cont + 1);
    auto ea1 = IRB.CreateBitOrPointerCast(IRB.CreateGEP(cont, VALUE_INTPTR(sizeof(vm_cont_rec_t) / sizeof(intptr_t))), IntptrTy);
    CREATE_STORE_VM_REG(vm, m_sp, ea1);
    CREATE_STORE_VM_REG(vm, m_fp, ea1);
    // m_cont = &cont->up;
    auto ea2 = IRB.CreateBitOrPointerCast(IRB.CreateGEP(cont, VALUE_INTPTR(offsetof(vm_cont_rec_t, up) / sizeof(intptr_t))), IntptrTy);
    CREATE_STORE_VM_REG(vm, m_cont, ea2);
    IRB.CreateRetVoid();

    //verifyModule(*M, &outs());
    //M.get()->print(outs(), nullptr);

    ExitOnErr(m_jit->addIRModule(optimizeModule(std::move(ThreadSafeModule(std::move(M), std::move(Context))))));
//  ExitOnErr(m_jit->addIRModule(std::move(ThreadSafeModule(std::move(M), std::move(Context)))));
    m_jit->getMainJITDylib().dump(llvm::outs());
}

void
codegen_t::compile(VM* vm, scm_closure_t closure)
{
    printer_t prt(vm, vm->m_current_output);
    prt.format("generating native code: ~s~&", closure->doc);
    if (CAAR(closure->code) == INST_NATIVE) {
        puts("- already compiled");
        return;
    }
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

    ExitOnErr(m_jit->addIRModule(optimizeModule(std::move(ThreadSafeModule(std::move(M), std::move(Context))))));
//  ExitOnErr(m_jit->addIRModule(std::move(ThreadSafeModule(std::move(M), std::move(Context)))));

    m_jit->getMainJITDylib().dump(llvm::outs());

    auto symbol = ExitOnErr(m_jit->lookup(function_id));
    intptr_t (*thunk)(intptr_t) = (intptr_t (*)(intptr_t))symbol.getAddress();

    scm_bvector_t operand = make_bvector(vm->m_heap, sizeof(intptr_t));
    *(intptr_t*)operand->elts = (intptr_t)thunk;
    scm_obj_t n_code = LIST1(CONS(INST_NATIVE, CONS(operand, closure->code)));
    vm->m_heap->write_barrier(n_code);
    closure->code = n_code;
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
            case VMOP_IF_NULLP:
                emit_if_nullp(C, M, F, IRB, inst);
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
            case VMOP_PUSH_ILOC1:
                emit_push_iloc1(C, M, F, IRB, inst);
                break;
            case VMOP_PUSH_CDR_ILOC:
                emit_push_cdr_iloc(C, M, F, IRB, inst);
                break;
            case VMOP_RET_CONS:
                emit_ret_cons(C, M, F, IRB, inst);
                break;
            case VMOP_EXTEND:
                emit_extend(C, M, F, IRB, inst);
                break;
            case VMOP_PUSH_GLOC:
                emit_push_gloc(C, M, F, IRB, inst);
                break;
            case VMOP_SUBR:
                emit_subr(C, M, F, IRB, inst);
                break;
            case VMOP_PUSH_SUBR:
                emit_push_subr(C, M, F, IRB, inst);
                break;
            default:
                printf("- unsupported instruction %s\n", ((scm_symbol_t)CAAR(inst))->name);
                break;
        }
        inst = CDR(inst);
    }
}

Value*
codegen_t::emit_lookup_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, intptr_t depth, intptr_t index)
{
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;

    if (depth == 0 && index == 0) {
        auto env = IRB.CreateBitOrPointerCast(IRB.CreateSub(CREATE_LOAD_VM_REG(vm, m_env), VALUE_INTPTR(offsetof(vm_env_rec_t, up))), IntptrPtrTy);
        auto count = CREATE_LOAD_ENV_REC(env, count);
        return IRB.CreateGEP(env, IRB.CreateNeg(count));
    } else if (depth == 0) {
        auto env = IRB.CreateBitOrPointerCast(IRB.CreateSub(CREATE_LOAD_VM_REG(vm, m_env), VALUE_INTPTR(offsetof(vm_env_rec_t, up))), IntptrPtrTy);
        auto count = CREATE_LOAD_ENV_REC(env, count);
        return IRB.CreateGEP(env, IRB.CreateSub(VALUE_INTPTR(index), count));
    } else if (depth == 1) {
        auto lnk = IRB.CreateLoad(IRB.CreateBitOrPointerCast(CREATE_LOAD_VM_REG(vm, m_env), IntptrPtrTy));
        auto env = IRB.CreateBitOrPointerCast(IRB.CreateSub(lnk, VALUE_INTPTR(offsetof(vm_env_rec_t, up))), IntptrPtrTy);
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

Function*
codegen_t::emit_call(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;

    CREATE_STACK_OVERFLOW_HANDLER(sizeof(vm_cont_rec_t));

    char cont_id[40];
    uuid_v4(cont_id, sizeof(cont_id));

    Function* K = Function::Create(FunctionType::get(IntptrTy, {IntptrPtrTy}, false), Function::ExternalLinkage, cont_id, M);
    BasicBlock* RETURN = BasicBlock::Create(C, "entry", K);

    // vm_cont_t cont = (vm_cont_t)m_sp;
    auto cont = IRB.CreateBitOrPointerCast(CREATE_LOAD_VM_REG(vm, m_sp), IntptrPtrTy);
    auto prepare_call = M->getOrInsertFunction("prepare_call", VoidTy, IntptrPtrTy, IntptrPtrTy);
    IRB.CreateCall(prepare_call, {vm, cont});
    // cont->pc = CDR(m_pc);
    CREATE_STORE_CONT_REC(cont, pc, VALUE_INTPTR(CDR(inst)));
    // cont->code = NULL;
    CREATE_STORE_CONT_REC(cont, code, IRB.CreateBitOrPointerCast(K, IntptrTy));
    // m_pc = OPERANDS;
    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(operands));
    // continue emit code in operands
    transform(C, M, F, IRB, operands);

    IRB.SetInsertPoint(RETURN);
    return K;
}

void
codegen_t::emit_push(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;
    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));
    CREATE_PUSH_VM_STACK(CREATE_LOAD_VM_REG(vm, m_value));
}

void
codegen_t::emit_push_const(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;
    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));
    CREATE_PUSH_VM_STACK(VALUE_INTPTR(operands));
}

void
codegen_t::emit_push_iloc0(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;
    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));
    CREATE_PUSH_VM_STACK(IRB.CreateLoad(emit_lookup_iloc(C, M, F, IRB, 0, FIXNUM(operands))));
}

void
codegen_t::emit_push_iloc1(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;
    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));
    CREATE_PUSH_VM_STACK(IRB.CreateLoad(emit_lookup_iloc(C, M, F, IRB, 1, FIXNUM(operands))));
}

void
codegen_t::emit_push_gloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;
    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));

    auto gloc = IRB.CreateBitOrPointerCast(VALUE_INTPTR(operands), IntptrPtrTy);
    auto val = CREATE_LOAD_GLOC_REC(gloc, value);
    BasicBlock* undef_true = BasicBlock::Create(C, "undef_true", F);
    BasicBlock* undef_false = BasicBlock::Create(C, "undef_false", F);
    auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
    IRB.CreateCondBr(undef_cond, undef_true, undef_false);
    // invalid
    IRB.SetInsertPoint(undef_true);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_error_push_gloc));
    // valid
    IRB.SetInsertPoint(undef_false);
    CREATE_PUSH_VM_STACK(val);
}

void
codegen_t::emit_push_car_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;

    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));

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
        IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_back_to_loop));
    // pair
    IRB.SetInsertPoint(pair_true);
    CREATE_PUSH_VM_STACK(CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(val, IntptrPtrTy), car));
}

void
codegen_t::emit_push_cdr_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;

    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));

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
        IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_back_to_loop));
    // pair
    IRB.SetInsertPoint(pair_true);
    CREATE_PUSH_VM_STACK(CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(val, IntptrPtrTy), cdr));
}

void
codegen_t::emit_push_nadd_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;

    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));

    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
    BasicBlock* nonfixnum_true = BasicBlock::Create(C, "nonfixnum_true", F);
    BasicBlock* nonfixnum_false = BasicBlock::Create(C, "nonfixnum_false", F);
    auto val = IRB.CreateLoad(emit_lookup_iloc(C, M, F, IRB, CAR(operands)));
    auto nonfixnum_cond = IRB.CreateICmpEQ(IRB.CreateAnd(val, 1), VALUE_INTPTR(0));
    IRB.CreateCondBr(nonfixnum_cond, nonfixnum_true, nonfixnum_false);
    // fixnum
    IRB.SetInsertPoint(nonfixnum_false);
        auto n = IRB.CreateAdd(IRB.CreateAShr(val, VALUE_INTPTR(1)), IRB.CreateAShr(VALUE_INTPTR(CADR(operands)), VALUE_INTPTR(1)));
        // if ((n <= FIXNUM_MAX) & (n >= FIXNUM_MIN)) {
        auto n_le_cond = IRB.CreateICmpSLE(n, VALUE_INTPTR(FIXNUM_MAX));
        BasicBlock* n_le_true = BasicBlock::Create(C, "n_le_true", F);
        BasicBlock* n_le_false = BasicBlock::Create(C, "n_le_false", F);
        IRB.CreateCondBr(n_le_cond, n_le_true, n_le_false);
        IRB.SetInsertPoint(n_le_true);
            auto n_ge_cond = IRB.CreateICmpSGE(n, VALUE_INTPTR(FIXNUM_MIN));
            BasicBlock* n_ge_true = BasicBlock::Create(C, "n_ge_true", F);
            BasicBlock* n_ge_false = BasicBlock::Create(C, "n_ge_false", F);
            IRB.CreateCondBr(n_ge_cond, n_ge_true, n_ge_false);
            IRB.SetInsertPoint(n_ge_true);
            CREATE_PUSH_VM_STACK(IRB.CreateAdd(IRB.CreateShl(n, VALUE_INTPTR(1)), VALUE_INTPTR(1)));
            IRB.CreateBr(CONTINUE);
    // others
    IRB.SetInsertPoint(nonfixnum_true);
        auto thunk_number_pred = M->getOrInsertFunction("thunk_number_pred", IntptrTy, IntptrTy);
        BasicBlock* nonnum_true = BasicBlock::Create(C, "nonnum_true", F);
        BasicBlock* nonnum_false = BasicBlock::Create(C, "nonnum_false", F);
        auto nonnum_cond = IRB.CreateICmpEQ(IRB.CreateCall(thunk_number_pred, {val}), VALUE_INTPTR(0));
        IRB.CreateCondBr(nonnum_cond, nonnum_true, nonnum_false);
        // not number
        IRB.SetInsertPoint(nonnum_true);
            auto thunk_error_push_nadd_iloc = M->getOrInsertFunction("thunk_error_push_nadd_iloc", VoidTy, IntptrPtrTy, IntptrTy, IntptrTy);
            IRB.CreateCall(thunk_error_push_nadd_iloc, {vm, val, VALUE_INTPTR(CADR(operands))});
            IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_back_to_loop));
        // number
        IRB.SetInsertPoint(nonnum_false);
            auto thunk_arith_add = M->getOrInsertFunction("thunk_arith_add", IntptrTy, IntptrPtrTy, IntptrTy, IntptrTy);
            CREATE_PUSH_VM_STACK(IRB.CreateCall(thunk_arith_add, {vm, val, VALUE_INTPTR(CADR(operands))}));
            IRB.CreateBr(CONTINUE);

    IRB.SetInsertPoint(n_le_false);
    IRB.CreateBr(nonnum_false);
    IRB.SetInsertPoint(n_ge_false);
    IRB.CreateBr(nonnum_false);

    IRB.SetInsertPoint(CONTINUE);
}

void
codegen_t::emit_apply_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;

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
codegen_t::emit_apply_gloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;

    auto gloc = IRB.CreateBitOrPointerCast(VALUE_INTPTR(CAR(operands)), IntptrPtrTy);
    auto val = CREATE_LOAD_GLOC_REC(gloc, value);
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
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_error_apply_gloc));
}

void
codegen_t::emit_ret_const(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;

    auto val = VALUE_INTPTR(operands);
    CREATE_STORE_VM_REG(vm, m_value, val);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void
codegen_t::emit_ret_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;

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
codegen_t::emit_ret_cons(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;

    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto val = CREATE_LOAD_VM_REG(vm, m_value);
    auto sp_minus_1 = IRB.CreateLoad(IRB.CreateGEP(IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-1)));
    auto thunk_make_pair = M->getOrInsertFunction("thunk_make_pair", IntptrTy, IntptrPtrTy, IntptrTy, IntptrTy);
    CREATE_STORE_VM_REG(vm, m_value, IRB.CreateCall(thunk_make_pair, {vm, sp_minus_1, val}));
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void
codegen_t::emit_subr(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;

    intptr_t argc = FIXNUM(CADR(operands));
    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto argv = IRB.CreateSub(sp, VALUE_INTPTR(argc << log2_of_intptr_size()));

    scm_subr_t subr = (scm_subr_t)CAR(operands);
    auto subrType = FunctionType::get(IntptrTy, {IntptrPtrTy, IntptrTy, IntptrTy}, false);
    auto ptr = ConstantExpr::getIntToPtr(VALUE_INTPTR(subr->adrs), subrType->getPointerTo());
    auto val = IRB.CreateCall(ptr, {vm, VALUE_INTPTR(argc), argv});
    CREATE_STORE_VM_REG(vm, m_value, val);
    CREATE_STORE_VM_REG(vm, m_sp, argv);
}

void
codegen_t::emit_ret_subr(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;

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
codegen_t::emit_push_subr(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;

    intptr_t argc = FIXNUM(CADR(operands));
    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto argv = IRB.CreateSub(sp, VALUE_INTPTR(argc << log2_of_intptr_size()));

    scm_subr_t subr = (scm_subr_t)CAR(operands);
    auto subrType = FunctionType::get(IntptrTy, {IntptrPtrTy, IntptrTy, IntptrTy}, false);
    auto ptr = ConstantExpr::getIntToPtr(VALUE_INTPTR(subr->adrs), subrType->getPointerTo());
    auto val = IRB.CreateCall(ptr, {vm, VALUE_INTPTR(argc), argv});
    CREATE_STORE_VM_REG(vm, m_value, val);
    CREATE_STORE_VM_REG(vm, m_sp, argv);

    BasicBlock* undef_true = BasicBlock::Create(C, "undef_true", F);
    BasicBlock* undef_false = BasicBlock::Create(C, "undef_false", F);
    auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
    IRB.CreateCondBr(undef_cond, undef_true, undef_false);
    // valid
    IRB.SetInsertPoint(undef_false);
    CREATE_PUSH_VM_STACK(val);
    IRB.CreateBr(undef_true);
    // invalid
    IRB.SetInsertPoint(undef_true);
}

void
codegen_t::emit_if_true(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;

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
codegen_t::emit_if_nullp(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;

    auto value = CREATE_LOAD_VM_REG(vm, m_value);
    BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
    BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
    auto taken_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_nil));
    IRB.CreateCondBr(taken_cond, taken_true, taken_false);
    // taken
    IRB.SetInsertPoint(taken_true);
    transform(C, M, F, IRB, operands);
    // not taken
    IRB.SetInsertPoint(taken_false);
}

void
codegen_t::emit_if_nullp_ret_const(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;

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
codegen_t::emit_iloc0(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;

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
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_back_to_loop));
    // valid
    IRB.SetInsertPoint(undef_false);
}

void
codegen_t::emit_lt_n_iloc(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;

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
        auto thunk_real_pred = M->getOrInsertFunction("thunk_real_pred", IntptrTy, IntptrTy);
        BasicBlock* nonreal_true = BasicBlock::Create(C, "nonreal_true", F);
        BasicBlock* nonreal_false = BasicBlock::Create(C, "nonreal_false", F);
        auto nonreal_cond = IRB.CreateICmpEQ(IRB.CreateCall(thunk_real_pred, {val}), VALUE_INTPTR(0));
        IRB.CreateCondBr(nonreal_cond, nonreal_true, nonreal_false);
        // not real
        IRB.SetInsertPoint(nonreal_true);
            auto thunk_error_lt_n_iloc = M->getOrInsertFunction("thunk_error_lt_n_iloc", VoidTy, IntptrPtrTy, IntptrTy, IntptrTy);
            IRB.CreateCall(thunk_error_lt_n_iloc, {vm, val, VALUE_INTPTR(CADR(operands))});
            IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_back_to_loop));
        // real
        IRB.SetInsertPoint(nonreal_false);
            auto thunk_n_compare = M->getOrInsertFunction("thunk_n_compare", IntptrTy, IntptrPtrTy, IntptrTy, IntptrTy);
            auto taken_cond = IRB.CreateICmpSLT(IRB.CreateCall(thunk_n_compare, {vm, val, VALUE_INTPTR(CADR(operands))}), VALUE_INTPTR(0));
            BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
            BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
            IRB.CreateCondBr(taken_cond, taken_true, taken_false);
            // taken
            IRB.SetInsertPoint(taken_true);
                CREATE_STORE_VM_REG(vm, m_value, VALUE_INTPTR(scm_true));
                IRB.CreateBr(CONTINUE);
            // not taken
            IRB.SetInsertPoint(taken_false);
                CREATE_STORE_VM_REG(vm, m_value, VALUE_INTPTR(scm_false));
                IRB.CreateBr(CONTINUE);
    IRB.SetInsertPoint(CONTINUE);
}

void
codegen_t::emit_extend(LLVMContext& C, Module* M, Function* F, IRBuilder<>& IRB, scm_obj_t inst)
{
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();
    DECLEAR_COMMON_TYPES;
    CREATE_STACK_OVERFLOW_HANDLER(sizeof(vm_env_rec_t));

    auto argc = VALUE_INTPTR(FIXNUM(operands));
    auto env = IRB.CreateBitOrPointerCast(CREATE_LOAD_VM_REG(vm, m_sp), IntptrPtrTy);
    CREATE_STORE_ENV_REC(env, count, argc);
    CREATE_STORE_ENV_REC(env, up, CREATE_LOAD_VM_REG(vm, m_env));
    auto ea1 = IRB.CreateAdd(IRB.CreateBitOrPointerCast(env, IntptrTy), VALUE_INTPTR(sizeof(vm_env_rec_t)));
    CREATE_STORE_VM_REG(vm, m_sp, ea1);
    CREATE_STORE_VM_REG(vm, m_fp, ea1);
    CREATE_STORE_VM_REG(vm, m_env, CREATE_LEA_ENV_REC(env, up));
}

/*

(backtrace #f)
(import (digamma time))

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))
(closure-compile fib)
(time (fib 30)) ;=> 55
;;  0.344398 real    0.344043 user    0.000139 sys
;;  0.353730 real    0.353562 user    0.000000 sys

(define (minus x) (- x))
(define lst (make-list 10 '4))
(define map-1
  (lambda (proc lst)
    (if (null? lst)
        '()
        (cons (proc (car lst))
              (map-1 proc (cdr lst))))))
(closure-compile map-1)
(time (map-1 minus lst)) ;=> (-4 -4 -4 -4 -4 -4 -4 -4 -4 -4)

(define (n m) (list 1 (m) 3))
(closure-compile n)
(n (lambda () 2)) ; => (1 2 3)

(define c)
(define (n m) (list 1 (m) 3))
(closure-compile n)
(n (lambda () (call/cc (lambda (k) (set! c k) 2)))) ; => (1 2 3)
(c 1000) ; => (1 1000 3)

generating native code: map
* unsupported instruction if.null?
* unsupported instruction push.gloc
- unsupported instruction extend
- unsupported instruction push.iloc.1
- unsupported instruction push.iloc.1
- unsupported instruction push.iloc.1
- unsupported instruction push.iloc.1
- unsupported instruction push.subr


    (define map-1
      (lambda (proc lst)
        (cond ((null? lst) '())
              (else (cons (proc (car lst))
                          (map-1 proc (cdr lst)))))))

    (define map-n
      (lambda (proc lst)
        (cond ((null? lst) '())
              (else (cons (apply proc (car lst))
                          (map-n proc (cdr lst)))))))

    (define map
      (lambda (proc lst1 . lst2)
        (if (null? lst2)
            (map-1 proc lst1)
            (map-n proc (apply list-transpose* lst1 lst2)))))

====

    (define map-1
      (lambda (proc lst)
        (cond ((null? lst) '())
              (else (cons (proc (car lst))
                          (map-1 proc (cdr lst)))))))

    (define map-n
      (lambda (proc lst)
        (cond ((null? lst) '())
              (else (cons (apply proc (car lst))
                          (map-n proc (cdr lst)))))))

    (define map
      (lambda (proc lst1 . lst2)
        (if (null? lst2)
            (map-1 proc lst1)
            (map-n proc (apply list-transpose* lst1 lst2)))))


    (closure-compile map)
    (closure-compile map-1)
    (closure-compile map-n)
    (map cons '(1 2) '(3 4))
(import (digamma time))
(time (load "test/syntax-rule-stress-test.scm"))
*/