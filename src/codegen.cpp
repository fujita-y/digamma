// Copyright (c) 2004-2020 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "codegen.h"
#include "arith.h"
#include "printer.h"
#include "violation.h"
#include "uuid.h"
#include "interpreter.h"

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
#define STACKP(p)       (((p) >= (void*)vm->m_stack_top) & ((p) < (void*)vm->m_stack_limit))

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
        //if (obj == scm_undef) letrec_violation(vm);
        wrong_type_argument_violation(vm, "car", 0, "pair", obj, 1, &obj);
    }

    void c_error_push_cdr_iloc(VM* vm, scm_obj_t obj) {
        //if (obj == scm_undef) letrec_violation(vm);
        wrong_type_argument_violation(vm, "cdr", 0, "pair", obj, 1, &obj);
    }

    void c_error_cdr_iloc(VM* vm, scm_obj_t obj) {
        //if (obj == scm_undef) letrec_violation(vm);
        wrong_type_argument_violation(vm, "cdr", 0, "pair", obj, 1, &obj);
    }

    void c_error_push_cddr_iloc(VM* vm, scm_obj_t obj) {
        //if (obj == scm_undef) letrec_violation(vm);
        wrong_type_argument_violation(vm, "cddr", 0, "appropriate list structure", obj, 1, &obj);
    }

    void c_error_push_cadr_iloc(VM* vm, scm_obj_t obj) {
        //if (obj == scm_undef) letrec_violation(vm);
        wrong_type_argument_violation(vm, "cadr", 0, "appropriate list structure", obj, 1, &obj);
    }

    intptr_t c_lt_n_iloc(VM* vm, scm_obj_t obj, scm_obj_t operands) {
        if (real_pred(obj)) {
            vm->m_value = n_compare(vm->m_heap, obj, operands) < 0 ? scm_true : scm_false;
            return 0;
        }
        scm_obj_t argv[2] = { obj, operands };
        wrong_type_argument_violation(vm, "comparison(< > <= >=)", 0, "real", argv[0], 2, argv);
        return 1;
    }

    intptr_t c_gt_n_iloc(VM* vm, scm_obj_t obj, scm_obj_t operands) {
        if (real_pred(obj)) {
            vm->m_value = n_compare(vm->m_heap, obj, operands) > 0 ? scm_true : scm_false;
            return 0;
        }
        scm_obj_t argv[2] = { obj, operands };
        wrong_type_argument_violation(vm, "comparison(< > <= >=)", 0, "real", argv[0], 2, argv);
        return 1;
    }

    intptr_t c_eq_n_iloc(VM* vm, scm_obj_t obj, scm_obj_t operands) {
        if (real_pred(obj)) {
            vm->m_value = n_compare(vm->m_heap, obj, operands) == 0 ? scm_true : scm_false;
            return 0;
        }
        scm_obj_t argv[2] = { obj, operands };
        wrong_type_argument_violation(vm, "comparison(< > <= >=)", 0, "real", argv[0], 2, argv);
        return 1;
    }

    intptr_t c_gt_iloc(VM* vm, scm_obj_t obj, scm_obj_t operands) {
        int bad = 0;
        if (real_pred(vm->m_value)) {
            if (real_pred(obj)) {
                vm->m_value = (n_compare(vm->m_heap, vm->m_value, obj) > 0) ? scm_true : scm_false;
                return 0;
            }
            bad = 1;
        }
        scm_obj_t argv[2] = { vm->m_value, obj };
        wrong_type_argument_violation(vm, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
        return 1;
    }

    intptr_t c_lt_iloc(VM* vm, scm_obj_t obj, scm_obj_t operands) {
        int bad = 0;
        if (real_pred(vm->m_value)) {
            if (real_pred(obj)) {
                vm->m_value = (n_compare(vm->m_heap, vm->m_value, obj) < 0) ? scm_true : scm_false;
                return 0;
            }
            bad = 1;
        }
        scm_obj_t argv[2] = { vm->m_value, obj };
        wrong_type_argument_violation(vm, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
        return 1;
    }

    intptr_t c_eq_iloc(VM* vm, scm_obj_t obj, scm_obj_t operands) {
        int bad = 0;
        if (real_pred(vm->m_value)) {
            if (real_pred(obj)) {
                vm->m_value = (n_compare(vm->m_heap, vm->m_value, obj) == 0) ? scm_true : scm_false;
                return 0;
            }
            bad = 1;
        }
        scm_obj_t argv[2] = { vm->m_value, obj };
        wrong_type_argument_violation(vm, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
        return 1;
    }
/*
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
*/
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

    void c_push_close(VM* vm, scm_closure_t operands) {
        if (STACKP(vm->m_env)) {
            vm->m_env = vm->save_env(vm->m_env);
            vm->update_cont(vm->m_cont);
        }
        vm->m_sp[0] = make_closure(vm->m_heap, operands, vm->m_env);
        vm->m_sp++;
    }

    intptr_t c_set_gloc(VM* vm, scm_closure_t operands) {
        scm_gloc_t gloc = (scm_gloc_t)CAR(operands);
        assert(GLOCP(gloc));
#if USE_PARALLEL_VM
#if UNSPECIFIED_GLOC_IS_SPECIAL
        if (m_interp->live_thread_count() > 1 && gloc->value != scm_unspecified) {
            if (!m_heap->in_heap(gloc)) goto ERROR_SET_GLOC_BAD_CONTEXT;
            m_interp->remember(gloc->value, m_value);
        }
#else
        if (vm->m_interp->live_thread_count() > 1) {
            if (!vm->m_heap->in_heap(gloc)) {
                thread_global_access_violation(vm, ((scm_gloc_t)CAR(operands))->variable, vm->m_value);
                return 1;
            }
            vm->m_interp->remember(gloc->value, vm->m_value);
        }
#endif
#endif
        vm->m_heap->write_barrier(vm->m_value);
        gloc->value = vm->m_value;
        return 0;
    }

}

codegen_t::codegen_t(VM* vm) : m_vm(vm)
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

    // puts("*** IR after optimize ***");
    // M.print(outs(), nullptr);

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
#if USE_LLVM_ATTRIBUTES
    F->addFnAttr(Attribute::NoUnwind);
    F->addParamAttr(0, Attribute::NoAlias);
    F->addParamAttr(0, Attribute::NoCapture);
    F->addParamAttr(1, Attribute::NoAlias);
    F->addParamAttr(1, Attribute::NoCapture);
#endif
//  for (Argument& argument : F->args()) { argument.addAttr(Attribute::NoAlias); argument.addAttr(Attribute::NoCapture); }

    IRBuilder<> IRB(BasicBlock::Create(C, "entry", F));
    auto vm = F->arg_begin();
    auto cont = F->arg_begin() + 1;

    CREATE_STORE_CONT_REC(cont, trace, VALUE_INTPTR(scm_unspecified));
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
codegen_t::is_compiled(scm_closure_t closure)
{
    VM* vm = m_vm;
    return CAAR(closure->code) == INST_NATIVE;
}

// compile one top-level function
void
codegen_t::compile(scm_closure_t closure)
{
    VM* vm = m_vm;
    printer_t prt(vm, vm->m_current_output);
    prt.format("generating native code: ~s~&", closure->doc);
    if (is_compiled(closure)) {
        puts("- already compiled");
        return;
    }
    if (std::find(m_visit.begin(), m_visit.end(), closure) != m_visit.end()) {
        puts("- already visit");
        return;
    }

    m_visit.push_back(closure);

    char module_id[40];
    uuid_v4(module_id, sizeof(module_id));
    char function_id[40];
    uuid_v4(function_id, sizeof(function_id));

    auto Context = make_unique<LLVMContext>();
    LLVMContext& C = *Context;
    DECLEAR_COMMON_TYPES;

    auto M = make_unique<Module>(module_id, C);
    Function* F = Function::Create(FunctionType::get(IntptrTy, {IntptrPtrTy}, false), Function::ExternalLinkage, function_id, M.get());
    for (Argument& argument : F->args()) { argument.addAttr(Attribute::NoAlias); argument.addAttr(Attribute::NoCapture); }
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

    scm_bvector_t bv = make_bvector(vm->m_heap, sizeof(intptr_t));
    *(intptr_t*)bv->elts = (intptr_t)thunk;
    scm_obj_t n_code = CONS(LIST2(INST_NATIVE, bv), closure->code);
    vm->m_heap->write_barrier(n_code);
    closure->code = n_code;

    m_visit.erase(std::remove(m_visit.begin(), m_visit.end(), closure), m_visit.end());
    m_lifted_functions.clear();
}

Function*
codegen_t::emit_lifted_function(context_t& ctx, scm_closure_t closure)
{
    VM* vm = m_vm;

    auto search = m_lifted_functions.find(closure);
    if (search != m_lifted_functions.end()) {
      puts(" + found in m_lifted_functions, return Function*");
      return search->second;
    }

    if (std::find(m_visit.begin(), m_visit.end(), closure) != m_visit.end()) {
        puts(" ? found in m_visit, return NULL (this should not happen?)");
        return nullptr;
    }

    puts(" + generating native code for lifted function");

    m_visit.push_back(closure);

    char function_id[40];
    uuid_v4(function_id, sizeof(function_id));

    LLVMContext& C = ctx.m_llvm_context;
    Module* M = ctx.m_module;

    DECLEAR_COMMON_TYPES;

    Function* F = Function::Create(FunctionType::get(IntptrTy, {IntptrPtrTy}, false), Function::ExternalLinkage, function_id, M);
#if USE_LLVM_ATTRIBUTES
    F->addFnAttr(Attribute::NoUnwind);
    F->addParamAttr(0, Attribute::NoAlias);
    F->addParamAttr(0, Attribute::NoCapture);
#endif

    BasicBlock* ENTRY = BasicBlock::Create(C, "entry", F);
    IRBuilder<> IRB(ENTRY);

    m_lifted_functions.insert({closure, F});

    context_t context(C, IRB);
    context.m_module = M;
    context.m_function = F;
    context.m_top_level_closure = closure;
    context.m_top_level_function = F;

    transform(context, closure->code);

    m_visit.erase(std::remove(m_visit.begin(), m_visit.end(), closure), m_visit.end());

    return F;
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
            case VMOP_ILOC: {
                emit_iloc(ctx, inst);
            } break;
            case VMOP_ILOC0: {
                emit_iloc0(ctx, inst);
            } break;
            case VMOP_ILOC1: {
                emit_iloc1(ctx, inst);
            } break;
            case VMOP_IF_NULLP: {
                emit_if_nullp(ctx, inst);
            } break;
            case VMOP_IF_EQP: {
                ctx.m_argc--;
                emit_if_eqp(ctx, inst);
            } break;
            case VMOP_IF_NULLP_RET_CONST: {
                emit_if_nullp_ret_const(ctx, inst);
            } break;
            case VMOP_PUSH_CAR_ILOC: {
                emit_push_car_iloc(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_PUSH_CADR_ILOC: {
                emit_push_cadr_iloc(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_PUSH_CDDR_ILOC: {
                emit_push_cddr_iloc(ctx, inst);
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
                intptr_t argc = FIXNUM(CADR(CDAR(inst)));
                ctx.m_argc = ctx.m_argc - argc;
            } break;
            case VMOP_PUSH_SUBR: {
                emit_push_subr(ctx, inst);
                intptr_t argc = FIXNUM(CADR(CDAR(inst)));
                ctx.m_argc = ctx.m_argc - argc + 1;
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
            case VMOP_IF_FALSE_RET: {
                emit_if_false_ret(ctx, inst);
            } break;
            case VMOP_IF_TRUE_RET_CONST: {
                emit_if_true_ret_const(ctx, inst);
            } break;
            case VMOP_GT_N_ILOC: {
                emit_gt_n_iloc(ctx, inst);
            } break;
            case VMOP_EQ_N_ILOC: {
                emit_eq_n_iloc(ctx, inst);
            } break;
            case VMOP_GT_ILOC: {
                emit_gt_iloc(ctx, inst);
            } break;
            case VMOP_LT_ILOC: {
                emit_lt_iloc(ctx, inst);
            } break;
            case VMOP_EQ_ILOC: {
                emit_eq_iloc(ctx, inst);
            } break;
            case VMOP_CDR_ILOC: {
                emit_cdr_iloc(ctx, inst);
            } break;
            case VMOP_PUSH_CONS: {
                emit_push_cons(ctx, inst);
            } break;
            case VMOP_PUSH_CLOSE: {
                emit_push_close(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_SET_GLOC: {
                emit_set_gloc(ctx, inst);
            } break;
            case VMOP_TOUCH_GLOC:
                break;
            default:
                printf("##### unsupported instruction %s ######\n", ((scm_symbol_t)CAAR(inst))->name);
                break;
        }
        inst = CDR(inst);
    }
}

#include "codegen.inc.cpp"

/*

(define (p)
  (+ 1 'o))
(closure-code p)
(p)
(closure-compile p)
(p)

(define (p) (list
(+ 1 'o)))
;(closure-code p)
;(p)
(closure-compile p)
(p)

(define (p) (list (+ 1 'o) (- 2 'p)))
(closure-code p)
(p)
(closure-compile p)
(p)

(define (p) (list (car 'o) 7))
(closure-compile p)
(p)

(define (p) (+ (list 1) 7))
(closure-compile p)
(p)

(define (p) (if (- 'i) 7 8))
(closure-compile p)
(p)

(backtrace #f)
(define (foo n m)
  (let ((a (lambda (b) (+ n b))))
    (a m)))
(closure-code foo)
(closure-compile foo)

((push.close . #<closure a>) (extend . 1) (push.iloc.1 . 1) (apply.iloc (0 . 0)))

(foo 2 3) ;=> 5

(define (p m)
  (list (car m) (cdr m)))
(p '(1 2))
(p 7)

(define (p m)
  (list
  (- m 1)
  (- m 2)))
(p 7)
(p '(1 2))

(define map
  (lambda (proc lst1 . lst2)

    (define map-1
      (lambda (proc lst)
        (cond ((null? lst) '())
              (else
               (cons (proc (car lst))
                     (map-1 proc (cdr lst)))))))

    (define map-n
      (lambda (proc lst)
        (cond ((null? lst) '())
              (else
               (cons (apply proc (car lst))
                     (map-n proc (cdr lst)))))))

    (if (null? lst2)
        (if (list? lst1)
            (map-1 proc lst1)
            (assertion-violation 'map (wrong-type-argument-message "proper list" lst1 2) (cons* proc lst1 lst2)))
        (cond ((apply list-transpose+ lst1 lst2)
               => (lambda (lst) (map-n proc lst)))
              (else
               (map-n proc lst)
               (assertion-violation 'map "expected same length proper lists" (cons* proc lst1 lst2)))))))

(import (digamma time))
(define lst (iota 1000))
(time (let loop ((n 0)) (cond ((> n 100) #t) (else (map - lst) (loop (+ n 1))))))

(define (p)
  (define (t v)
    (display v))
(let loop ((n 0))
  (cond
    ((> n 5) (t #t))
    ((= n 0) (t "0"))
    (else
      (t n)
      (loop (+ n 1))))

;;  0.024046 real    0.038616 user    0.001176 sys

(define (p)
  (define (t v)
    (display v))
  (let loop ((n 0))
    (cond
      ((> n 5) (t #t))
      ((= n 2)
       (t "*")
       (loop (+ n 1)))
      (else
        (t n)
        (loop (+ n 1))))))
*/
