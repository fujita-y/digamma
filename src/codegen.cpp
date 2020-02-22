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

extern scm_obj_t subr_num_add(VM* vm, int argc, scm_obj_t argv[]);

#define DECLEAR_COMMON_TYPES \
    auto IntptrTy = (sizeof(intptr_t) == 4 ? Type::getInt32Ty(C) : Type::getInt64Ty(C)); \
    auto IntptrPtrTy = sizeof(intptr_t) == 4 ? Type::getInt32PtrTy(C) : Type::getInt64PtrTy(C); \
    auto VoidTy = Type::getVoidTy(C);

#if INTPTR_MAX == INT32_MAX
    #define VALUE_INTPTR(_VAL_) IRB.getInt32((intptr_t)(_VAL_))
#elif INTPTR_MAX == INT64_MAX
    #define VALUE_INTPTR(_VAL_) IRB.getInt64((intptr_t)(_VAL_))
#else
    #error unsupported intptr_t size
#endif

#define DECLEAR_CONTEXT_VARS \
    LLVMContext& C = ctx.m_llvm_context; \
    IRBuilder<>& IRB = ctx.m_irb; \
    Module* M = ctx.m_module; \
    Function* F = ctx.m_function;

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
            auto c_collect_stack = M->getOrInsertFunction("c_collect_stack", VoidTy, IntptrPtrTy, IntptrTy); \
            IRB.CreateCall(c_collect_stack, {vm, VALUE_INTPTR(_BYTES_REQUIRED_)}); \
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

extern "C" {

    void c_collect_stack(VM* vm, intptr_t acquire) {
        //printf("- c_collect_stack(%p, %d)\n", vm, (int)acquire);
        vm->collect_stack(acquire);
        // [TODO] m_heap->m_stop_the_world check here ?
    }

    scm_obj_t* c_lookup_iloc(VM* vm, intptr_t depth, intptr_t index) {
        //printf("- c_lookup_iloc(%p, %d, %d)\n", vm, (int)depth, (int)index);
        void* lnk = vm->m_env;
        intptr_t level = depth;
        while (level) { lnk = *(void**)lnk; level = level - 1; }
        vm_env_t env = (vm_env_t)((intptr_t)lnk - offsetof(vm_env_rec_t, up));
        return (scm_obj_t*)env - env->count + index;
    }

    vm_env_t c_lookup_env(VM* vm, intptr_t depth) {
        void* lnk = vm->m_env;
        intptr_t level = depth;
        while (level) { lnk = *(void**)lnk; level = level - 1; }
        vm_env_t env = (vm_env_t)((intptr_t)lnk - offsetof(vm_env_rec_t, up));
        return env;
    }

    void c_letrec_violation(VM* vm) {
    //    printf("- c_letrec_violation(%p)\n", vm);
        letrec_violation(vm);
    }

    void c_error_push_car_iloc(VM* vm, scm_obj_t obj) {
        if (obj == scm_undef) letrec_violation(vm);
        wrong_type_argument_violation(vm, "car", 0, "pair", obj, 1, &obj);
    }

    void c_error_push_cdr_iloc(VM* vm, scm_obj_t obj) {
        if (obj == scm_undef) letrec_violation(vm);
        wrong_type_argument_violation(vm, "cdr", 0, "pair", obj, 1, &obj);
    }

    void c_error_lt_n_iloc(VM* vm, scm_obj_t obj, scm_obj_t operands) {
        //printf("- c_error_lt_n_iloc(%p, %p, %p)\n", vm, obj, operands);
        if (obj == scm_undef) letrec_violation(vm);
        scm_obj_t argv[2] = { obj, operands };
        wrong_type_argument_violation(vm, "comparison(< > <= >=)", 0, "real", argv[0], 2, argv);
    }

    void c_error_gt_n_iloc(VM* vm, scm_obj_t obj, scm_obj_t operands) {
        //printf("- c_error_lt_n_iloc(%p, %p, %p)\n", vm, obj, operands);
        if (obj == scm_undef) letrec_violation(vm);
        scm_obj_t argv[2] = { obj, operands };
        wrong_type_argument_violation(vm, "comparison(< > <= >=)", 0, "real", argv[0], 2, argv);
    }

    void c_error_push_nadd_iloc(VM* vm, scm_obj_t obj, scm_obj_t operands) {
        if (obj == scm_undef) letrec_violation(vm);
        scm_obj_t argv[2] = { obj, operands };
        wrong_type_argument_violation(vm, "operator(+ -)", 0, "number", argv[0], 2, argv);
    }

    scm_obj_t c_make_pair(VM* vm, scm_obj_t car, scm_obj_t cdr) {
        return make_pair(vm->m_heap, car, cdr);
    }

    intptr_t c_number_pred(scm_obj_t obj)
    {
        //printf("- c_number_pred(%p) => %d\n", obj, number_pred(obj));
        return (intptr_t)number_pred(obj);
    }

    intptr_t c_real_pred(scm_obj_t obj)
    {
        //printf("- c_real_pred(%p) => %d\n", obj, real_pred(obj));
        return (intptr_t)real_pred(obj);
    }

    intptr_t c_n_compare(VM* vm, scm_obj_t obj, scm_obj_t operands) {
    //    printer_t prt(vm, vm->m_current_output);
    //    prt.format("- c_n_compare ~s ~s => %d ~%~!", obj, operands, n_compare(vm->m_heap, obj, operands));
        return n_compare(vm->m_heap, obj, operands);
    }

    scm_obj_t c_arith_add(VM* vm, scm_obj_t obj, scm_obj_t operands) {
    //    printer_t prt(vm, vm->m_current_output);
    //    prt.format("- c_arith_add ~s ~s => ~s ~%~!", obj, operands, arith_add(vm->m_heap, obj, operands));
        return arith_add(vm->m_heap, obj, operands);
    }

    void c_prepare_apply(VM* vm, scm_closure_t closure) {
        // assume vm->m_sp - vm->m_fp == args
        //if (m_heap->m_stop_the_world) stop();
        intptr_t args = HDR_CLOSURE_ARGS(closure->hdr);
        vm_env_t env = (vm_env_t)vm->m_sp;
        env->count = args;
        env->up = closure->env;
        vm->m_sp = vm->m_fp = (scm_obj_t*)(env + 1);
        vm->m_pc = closure->code;
        vm->m_env = &env->up;
    }

}

codegen_t::codegen_t()
{
    auto J = ExitOnErr(LLJITBuilder().create());
    auto D = J->getDataLayout();
    auto G = ExitOnErr(orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(D.getGlobalPrefix()));
#if __clang_major__ < 10
    J->getMainJITDylib().setGenerator(G);
#else
    J->getMainJITDylib().addGenerator(std::move(G));
#endif
    m_jit = std::move(J);
    define_prepare_call();
}

ThreadSafeModule
codegen_t::optimizeModule(ThreadSafeModule TSM) {
#if __clang_major__ < 10
    Module &M = *TSM.getModule();
#else
    Module &M = *TSM.getModuleUnlocked();
#endif
    PassManagerBuilder B;
    B.OptLevel = 2;
    B.SizeLevel = 1;

    // puts("=== IR before optimize ===");
    // M.print(outs(), nullptr);

    legacy::FunctionPassManager FPM(&M);
    B.populateFunctionPassManager(FPM);
    FPM.doInitialization();
    for (Function &F : M) FPM.run(F);
    FPM.doFinalization();

    legacy::PassManager MPM;
    B.populateModulePassManager(MPM);
    MPM.run(M);

     puts("*** IR after optimize ***");
     M.print(outs(), nullptr);

    return std::move(TSM);
}

void
codegen_t::define_prepare_call()
{
    auto Context = make_unique<LLVMContext>();
    LLVMContext& C = *Context;

    DECLEAR_COMMON_TYPES;

    auto M = make_unique<Module>("intrinsics", C);
    Function* F = Function::Create(FunctionType::get(VoidTy, {IntptrPtrTy, IntptrPtrTy}, false), Function::ExternalLinkage, "prepare_call", M.get());
    F->setCallingConv(CallingConv::Fast);
    IRBuilder<> IRB(BasicBlock::Create(C, "entry", F));
    auto vm = F->arg_begin();
    auto cont = F->arg_begin() + 1;

    CREATE_STORE_CONT_REC(cont, trace, CREATE_LOAD_VM_REG(vm, m_trace));
    // cont->fp = m_fp;
    CREATE_STORE_CONT_REC(cont, fp, CREATE_LOAD_VM_REG(vm, m_fp));
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
    IRB.CreateRetVoid();

    verifyModule(*M, &outs());

//  M.get()->print(outs(), nullptr);

    ExitOnErr(m_jit->addIRModule(optimizeModule(std::move(ThreadSafeModule(std::move(M), std::move(Context))))));

//  m_jit->getMainJITDylib().dump(llvm::outs());
}

Value*
codegen_t::emit_lookup_env(context_t& ctx, intptr_t depth)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    auto vm = F->arg_begin();

    // [TODO] optimize
    if (depth == 0) {
        auto env = IRB.CreateBitOrPointerCast(IRB.CreateSub(CREATE_LOAD_VM_REG(vm, m_env), VALUE_INTPTR(offsetof(vm_env_rec_t, up))), IntptrPtrTy);
        return env;
    } else if (depth == 1) {
        auto lnk = IRB.CreateLoad(IRB.CreateBitOrPointerCast(CREATE_LOAD_VM_REG(vm, m_env), IntptrPtrTy));
        auto env = IRB.CreateBitOrPointerCast(IRB.CreateSub(lnk, VALUE_INTPTR(offsetof(vm_env_rec_t, up))), IntptrPtrTy);
        return env;
    }
    auto c_lookup_env = M->getOrInsertFunction("c_lookup_env", IntptrPtrTy, IntptrPtrTy, IntptrTy);
    return IRB.CreateCall(c_lookup_env, { vm, VALUE_INTPTR(depth) });
}

Value*
codegen_t::emit_lookup_iloc(context_t& ctx, intptr_t depth, intptr_t index)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    auto vm = F->arg_begin();

    if (depth == 0) {
        auto env = IRB.CreateBitOrPointerCast(IRB.CreateSub(CREATE_LOAD_VM_REG(vm, m_env), VALUE_INTPTR(offsetof(vm_env_rec_t, up))), IntptrPtrTy);
        auto count = CREATE_LOAD_ENV_REC(env, count);
        if (index == 0) return IRB.CreateGEP(env, IRB.CreateNeg(count));
        return IRB.CreateGEP(env, IRB.CreateSub(VALUE_INTPTR(index), count));
    } else if (depth == 1) {
        auto lnk = IRB.CreateLoad(IRB.CreateBitOrPointerCast(CREATE_LOAD_VM_REG(vm, m_env), IntptrPtrTy));
        auto env = IRB.CreateBitOrPointerCast(IRB.CreateSub(lnk, VALUE_INTPTR(offsetof(vm_env_rec_t, up))), IntptrPtrTy);
        auto count = CREATE_LOAD_ENV_REC(env, count);
        if (index == 0) return IRB.CreateGEP(env, IRB.CreateNeg(count));
        return IRB.CreateGEP(env, IRB.CreateSub(VALUE_INTPTR(index), count));
    }
    auto c_lookup_iloc = M->getOrInsertFunction("c_lookup_iloc", IntptrPtrTy, IntptrPtrTy, IntptrTy, IntptrTy);
    return IRB.CreateCall(c_lookup_iloc, { vm, VALUE_INTPTR(depth), VALUE_INTPTR(index) });
}

Value*
codegen_t::emit_lookup_iloc(context_t& ctx, scm_obj_t loc)
{
    return emit_lookup_iloc(ctx, FIXNUM(CAR(loc)), FIXNUM(CDR(loc)));
}

bool
codegen_t::is_compiled(VM* vm, scm_closure_t closure)
{
    return CAAR(closure->code) == INST_NATIVE;
}

void
codegen_t::compile(VM* vm, scm_closure_t closure)
{
    printer_t prt(vm, vm->m_current_output);
    prt.format("generating native code: ~s~&", closure->doc);
    if (is_compiled(vm, closure)) {
        puts("- already compiled");
        return;
    }
    char module_id[40];
    uuid_v4(module_id, sizeof(module_id));
    char function_id[40];
    uuid_v4(function_id, sizeof(function_id));

    auto Context = make_unique<LLVMContext>();
    LLVMContext& C = *Context;
    DECLEAR_COMMON_TYPES;

    auto M = make_unique<Module>(module_id, C);
    Function* F = Function::Create(FunctionType::get(IntptrTy, {IntptrPtrTy}, false), Function::ExternalLinkage, function_id, M.get());
    BasicBlock* ENTRY = BasicBlock::Create(C, "entry", F);
    IRBuilder<> IRB(ENTRY);

    context_t context(C, IRB);
    context.m_module = M.get();
    context.m_function = F;
    context.m_top_level_closure = closure;
    context.m_top_level_function = F;

    transform(context, closure->code);

    verifyModule(*M, &outs());

    ExitOnErr(m_jit->addIRModule(optimizeModule(std::move(ThreadSafeModule(std::move(M), std::move(Context))))));
//  ExitOnErr(m_jit->addIRModule(std::move(ThreadSafeModule(std::move(M), std::move(Context)))));

//  m_jit->getMainJITDylib().dump(llvm::outs());

    auto symbol = ExitOnErr(m_jit->lookup(function_id));
    intptr_t (*thunk)(intptr_t) = (intptr_t (*)(intptr_t))symbol.getAddress();

    scm_bvector_t operand = make_bvector(vm->m_heap, sizeof(intptr_t));
    *(intptr_t*)operand->elts = (intptr_t)thunk;
    scm_obj_t n_code = CONS(LIST2(INST_NATIVE, operand), closure->code);
    vm->m_heap->write_barrier(n_code);
    closure->code = n_code;
}

void
codegen_t::transform(context_t ctx, scm_obj_t inst)
{
    while (inst != scm_nil) {
        switch (VM::instruction_to_opcode(CAAR(inst))) {
            case VMOP_CALL: {
                ctx.m_function = emit_call(ctx, inst);
            } break;
            case VMOP_IF_TRUE: {
                emit_if_true(ctx, inst);
            } break;
            case VMOP_PUSH: {
                emit_push(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_PUSH_CONST: {
                emit_push_const(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_RET_CONST: {
                emit_ret_const(ctx, inst);
            } break;
            case VMOP_APPLY_ILOC: {
                emit_apply_iloc(ctx, inst);
            } break;
            case VMOP_APPLY_GLOC: {
                emit_apply_gloc(ctx, inst);
            } break;
            case VMOP_RET_SUBR: {
                emit_ret_subr(ctx, inst);
            } break;
            case VMOP_RET_ILOC: {
                emit_ret_iloc(ctx, inst);
            } break;
            case VMOP_LT_N_ILOC: {
                emit_lt_n_iloc(ctx, inst);
            } break;
            case VMOP_PUSH_NADD_ILOC: {
                emit_push_nadd_iloc(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_ILOC0: {
                emit_iloc0(ctx, inst);
            } break;
            case VMOP_IF_NULLP: {
                emit_if_nullp(ctx, inst);
            } break;
            case VMOP_IF_NULLP_RET_CONST: {
                emit_if_nullp_ret_const(ctx, inst);
            } break;
            case VMOP_PUSH_CAR_ILOC: {
                emit_push_car_iloc(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_PUSH_ILOC0: {
                emit_push_iloc0(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_PUSH_ILOC1: {
                emit_push_iloc1(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_PUSH_CDR_ILOC: {
                emit_push_cdr_iloc(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_RET_CONS: {
                emit_ret_cons(ctx, inst);
            } break;
            case VMOP_EXTEND: {
                emit_extend(ctx, inst);
                ctx.m_argc = 0;
            } break;
            case VMOP_PUSH_GLOC: {
                emit_push_gloc(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_SUBR: {
                emit_subr(ctx, inst);
                ctx.m_argc = 0;
            } break;
            case VMOP_PUSH_SUBR: {
                emit_push_subr(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_EXTEND_ENCLOSE_LOCAL: {
                emit_extend_enclose_local(ctx, inst);
                ctx.m_argc = 0;
            } break;
            case VMOP_APPLY_ILOC_LOCAL: {
                emit_apply_iloc_local(ctx, inst);
            } break;
            case VMOP_PUSH_ILOC: {
                emit_push_iloc(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_IF_TRUE_RET: {
                emit_if_true_ret(ctx, inst);
            } break;
            case VMOP_IF_TRUE_RET_CONST: {
                emit_if_true_ret_const(ctx, inst);
            } break;
            case VMOP_GT_N_ILOC: {
                emit_gt_n_iloc(ctx, inst);
            }
            case VMOP_GT_ILOC: {
                emit_gt_iloc(ctx, inst);
            }
            case VMOP_TOUCH_GLOC:
                break;
            default:
                printf("- unsupported instruction %s\n", ((scm_symbol_t)CAAR(inst))->name);
                break;
        }
        inst = CDR(inst);
    }
}

Function*
codegen_t::emit_call(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

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

    // continue emit code in operands
    context_t ctx2 = ctx;
    ctx2.m_argc = 0;
    transform(ctx2, operands);

    IRB.SetInsertPoint(RETURN);
    return K;
}

void
codegen_t::emit_extend(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

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

void
codegen_t::emit_extend_enclose_local(context_t& ctx, scm_obj_t inst)
{
    // printf("emit_extend_enclose_local ctx.m_depth = %d\n", ctx.m_depth);
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    CREATE_STACK_OVERFLOW_HANDLER(sizeof(vm_env_rec_t) + sizeof(scm_obj_t));
    CREATE_PUSH_VM_STACK(VALUE_INTPTR(operands));
    auto env = IRB.CreateBitOrPointerCast(CREATE_LOAD_VM_REG(vm, m_sp), IntptrPtrTy);
    CREATE_STORE_ENV_REC(env, count, VALUE_INTPTR(1));
    CREATE_STORE_ENV_REC(env, up, CREATE_LOAD_VM_REG(vm, m_env));
    auto ea1 = IRB.CreateAdd(IRB.CreateBitOrPointerCast(env, IntptrTy), VALUE_INTPTR(sizeof(vm_env_rec_t)));
    CREATE_STORE_VM_REG(vm, m_sp, ea1);
    CREATE_STORE_VM_REG(vm, m_fp, ea1);
    CREATE_STORE_VM_REG(vm, m_env, CREATE_LEA_ENV_REC(env, up));
    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
    IRB.CreateBr(CONTINUE);

    // continue emit code in operands
    char local_id[40];
    uuid_v4(local_id, sizeof(local_id));
    Function* L = Function::Create(FunctionType::get(IntptrTy, {IntptrPtrTy}, false), Function::InternalLinkage, local_id, M);
    BasicBlock* LOOP = BasicBlock::Create(C, "entry", L);
    // L->setCallingConv(CallingConv::Fast);
    ctx.m_local_functions.resize(ctx.m_depth + 1);
    ctx.m_local_functions.at(ctx.m_depth) = L;
    // printf("emit_extend_enclose_local ctx.m_local_functions.at(%d) = %p\n", ctx.m_depth, ctx.m_local_functions.at(ctx.m_depth));
    context_t ctx2 = ctx;
    ctx2.m_function = L;
    ctx2.m_depth++;
    ctx2.m_argc = 0;
    IRB.SetInsertPoint(LOOP);
    transform(ctx2, operands);

    IRB.SetInsertPoint(CONTINUE);
}

void
codegen_t::emit_apply_iloc_local(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    int level = FIXNUM(CAAR(operands));
    int function_index = ctx.m_depth - level;
    // printf("emit_apply_iloc_local ctx.m_depth = %d level = %d function_index = %d\n", ctx.m_depth, level, function_index);

    CREATE_STACK_OVERFLOW_HANDLER(sizeof(vm_env_rec_t));
    auto env2 = emit_lookup_env(ctx, level);
    auto count = CREATE_LOAD_ENV_REC(env2, count);
    int index = FIXNUM(CDAR(operands));
    auto obj = IRB.CreateLoad(index == 0 ? IRB.CreateGEP(env2, IRB.CreateNeg(count)) : IRB.CreateGEP(env2, IRB.CreateSub(VALUE_INTPTR(index), count)));
    auto env = IRB.CreateBitOrPointerCast(CREATE_LOAD_VM_REG(vm, m_sp), IntptrPtrTy);
    CREATE_STORE_ENV_REC(env, count, VALUE_INTPTR(ctx.m_argc));
    CREATE_STORE_ENV_REC(env, up, CREATE_LEA_ENV_REC(env2, up));
    auto ea1 = IRB.CreateAdd(IRB.CreateBitOrPointerCast(env, IntptrTy), VALUE_INTPTR(sizeof(vm_env_rec_t)));
    CREATE_STORE_VM_REG(vm, m_sp, ea1);
    CREATE_STORE_VM_REG(vm, m_fp, ea1);
    CREATE_STORE_VM_REG(vm, m_env, CREATE_LEA_ENV_REC(env, up));

    // printf("emit_apply_iloc_local ctx.m_local_functions.size() = %lu\n", ctx.m_local_functions.size());
    Function* L = ctx.m_local_functions[function_index];
    assert(L != nullptr);
    auto call = IRB.CreateCall(L, { vm });
    call->setTailCallKind(CallInst::TCK_MustTail);
    IRB.CreateRet(call);
    /*
    if (L == nullptr) {
      printf("emit_apply_iloc_local ctx.m_local_functions[function_index] = nullptr\n");
      CREATE_STORE_VM_REG(vm, m_pc, obj);
      IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_loop));
    } else {
      printf("emit_apply_iloc_local ctx.m_local_functions[function_index] = %p\n", L);
      auto call = IRB.CreateCall(L, { vm });
      call->setTailCallKind(CallInst::TCK_MustTail);
      IRB.CreateRet(call);
    }
    */
}

#include "codegen.inc.cpp"

/*
- unsupported instruction >n.iloc       VMOP_GT_N_ILOC
- unsupported instruction if.true.ret   VMOP_IF_TRUE_RET
- unsupported instruction push.iloc     VMOP_PUSH_ILOC
*/

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

;;  0.475678 real    0.474068 user    0.001347 sys
;;  0.264808 real    0.264689 user    0.000000 sys

(time (fib 30.0))
;;  0.585781 real    0.926367 user    0.001209 sys

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

(define acc #f)
(define add (lambda (n) (set! acc (cons n acc))))

(backtrace #f)
(define (p n m)
  (let loop1 ((n n))
    (cond ((> n 2))
          (else
            (let loop2 ((m m))
              (cond ((> m 2))
                    (else
                     (display n)
                     (display m)
                     (loop2 (+ m 1)))))
            (loop1 (+ n 1))))))
(closure-compile p)
(p 0 0)


- unsupported instruction extend.enclose+

            CASE(VMOP_APPLY_ILOC_LOCAL) {
                if ((uintptr_t)m_sp + sizeof(vm_env_rec_t) < (uintptr_t)m_stack_limit) {
                    operand_trace = CDR(OPERANDS);
                    void* lnk = m_env;
                    intptr_t level = FIXNUM(CAAR(OPERANDS));
                    while (level) { lnk = *(void**)lnk; level = level - 1; }
                    vm_env_t env2 = (vm_env_t)((intptr_t)lnk - offsetof(vm_env_rec_t, up));
                    scm_obj_t obj = *((scm_obj_t*)env2 - env2->count + FIXNUM(CDAR(OPERANDS)));
                    vm_env_t env = (vm_env_t)m_sp;
                    env->count = m_sp - m_fp;
                    env->up = &env2->up;
                    m_env = &env->up;
                    m_sp = m_fp = (scm_obj_t*)(env + 1);
                    m_pc = obj;
                    goto trace_n_loop;
                }
                goto COLLECT_STACK_ENV_REC;
            }

- unsupported instruction apply.iloc+

(define (f m n p)
    (cons #f (if m (list n) (list p))))



    --x86-asm-syntax=intel
*/
