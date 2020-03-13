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
#if USE_UNIFIED_STACK_CHECK
    #define CREATE_STACK_OVERFLOW_HANDLER(_BYTES_REQUIRED_) {}
#else
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
#endif

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
        vm->collect_stack(acquire);
    }

    scm_obj_t* c_lookup_iloc(VM* vm, intptr_t depth, intptr_t index) {
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
        letrec_violation(vm);
    }

    void c_error_push_car_iloc(VM* vm, scm_obj_t obj) {
        wrong_type_argument_violation(vm, "car", 0, "pair", obj, 1, &obj);
    }

    void c_error_push_cdr_iloc(VM* vm, scm_obj_t obj) {
        wrong_type_argument_violation(vm, "cdr", 0, "pair", obj, 1, &obj);
    }

    void c_error_car_iloc(VM* vm, scm_obj_t obj) {
        wrong_type_argument_violation(vm, "car", 0, "pair", obj, 1, &obj);
    }

    void c_error_cdr_iloc(VM* vm, scm_obj_t obj) {
        wrong_type_argument_violation(vm, "cdr", 0, "pair", obj, 1, &obj);
    }

    void c_error_cadr_iloc(VM* vm, scm_obj_t obj) {
        wrong_type_argument_violation(vm, "cadr", 0, "appropriate list structure", obj, 1, &obj);
    }

    void c_error_push_cddr_iloc(VM* vm, scm_obj_t obj) {
        wrong_type_argument_violation(vm, "cddr", 0, "appropriate list structure", obj, 1, &obj);
    }

    void c_error_push_cadr_iloc(VM* vm, scm_obj_t obj) {
        wrong_type_argument_violation(vm, "cadr", 0, "appropriate list structure", obj, 1, &obj);
    }

    void c_error_push_nadd_iloc(VM* vm, scm_obj_t obj, scm_obj_t operands) {
        if (obj == scm_undef) letrec_violation(vm);
        scm_obj_t argv[2] = { obj, operands };
        wrong_type_argument_violation(vm, "operator(+ -)", 0, "number", argv[0], 2, argv);
    }

    scm_obj_t c_make_pair(VM* vm, scm_obj_t car, scm_obj_t cdr) {
        return make_pair(vm->m_heap, car, cdr);
    }

    scm_obj_t c_arith_add(VM* vm, scm_obj_t obj, scm_obj_t operands) {
        return arith_add(vm->m_heap, obj, operands);
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

    intptr_t c_le_n_iloc(VM* vm, scm_obj_t obj, scm_obj_t operands) {
        if (real_pred(obj)) {
            vm->m_value = n_compare(vm->m_heap, obj, operands) <= 0 ? scm_true : scm_false;
            return 0;
        }
        scm_obj_t argv[2] = { obj, operands };
        wrong_type_argument_violation(vm, "comparison(< > <= >=)", 0, "real", argv[0], 2, argv);
        return 1;
    }

    intptr_t c_ge_n_iloc(VM* vm, scm_obj_t obj, scm_obj_t operands) {
        if (real_pred(obj)) {
            vm->m_value = n_compare(vm->m_heap, obj, operands) >= 0 ? scm_true : scm_false;
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

    intptr_t c_gt_iloc(VM* vm, scm_obj_t lhs, scm_obj_t rhs) {
        int bad = 0;
        if (real_pred(lhs)) {
            if (real_pred(rhs)) {
                vm->m_value = (n_compare(vm->m_heap, lhs, rhs) > 0) ? scm_true : scm_false;
                return 0;
            }
            bad = 1;
        }
        scm_obj_t argv[2] = { lhs, rhs };
        wrong_type_argument_violation(vm, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
        return 1;
    }

    intptr_t c_lt_iloc(VM* vm, scm_obj_t lhs, scm_obj_t rhs) {
        int bad = 0;
        if (real_pred(lhs)) {
            if (real_pred(rhs)) {
                vm->m_value = (n_compare(vm->m_heap, lhs, rhs) < 0) ? scm_true : scm_false;
                return 0;
            }
            bad = 1;
        }
        scm_obj_t argv[2] = { lhs, rhs };
        wrong_type_argument_violation(vm, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
        return 1;
    }

    intptr_t c_ge_iloc(VM* vm, scm_obj_t lhs, scm_obj_t rhs) {
        int bad = 0;
        if (real_pred(lhs)) {
            if (real_pred(rhs)) {
                vm->m_value = (n_compare(vm->m_heap, lhs, rhs) >= 0) ? scm_true : scm_false;
                return 0;
            }
            bad = 1;
        }
        scm_obj_t argv[2] = { lhs, rhs };
        wrong_type_argument_violation(vm, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
        return 1;
    }

    intptr_t c_le_iloc(VM* vm, scm_obj_t lhs, scm_obj_t rhs) {
        int bad = 0;
        if (real_pred(lhs)) {
            if (real_pred(rhs)) {
                vm->m_value = (n_compare(vm->m_heap, lhs, rhs) <= 0) ? scm_true : scm_false;
                return 0;
            }
            bad = 1;
        }
        scm_obj_t argv[2] = { lhs, rhs };
        wrong_type_argument_violation(vm, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
        return 1;
    }

    intptr_t c_eq_iloc(VM* vm, scm_obj_t lhs, scm_obj_t rhs) {
        int bad = 0;
        if (real_pred(lhs)) {
            if (real_pred(rhs)) {
                vm->m_value = (n_compare(vm->m_heap, lhs, rhs) == 0) ? scm_true : scm_false;
                return 0;
            }
            bad = 1;
        }
        scm_obj_t argv[2] = { lhs, rhs };
        wrong_type_argument_violation(vm, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
        return 1;
    }

    intptr_t c_number_pred(scm_obj_t obj)
    {
        return (intptr_t)number_pred(obj);
    }

    intptr_t c_real_pred(scm_obj_t obj)
    {
        return (intptr_t)real_pred(obj);
    }

    intptr_t c_n_compare(VM* vm, scm_obj_t obj, scm_obj_t operands) {
        return n_compare(vm->m_heap, obj, operands);
    }

    void c_prepare_apply(VM* vm, scm_closure_t closure) {
        // assume vm->m_sp - vm->m_fp == args
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

    void c_close(VM* vm, scm_closure_t operands) {
        if (STACKP(vm->m_env)) {
            vm->m_env = vm->save_env(vm->m_env);
            vm->update_cont(vm->m_cont);
        }
        vm->m_value = make_closure(vm->m_heap, operands, vm->m_env);
    }

    void c_ret_close(VM* vm, scm_closure_t operands) {
        if (STACKP(vm->m_env)) {
            vm->m_env = vm->save_env(vm->m_env);
            vm->update_cont(vm->m_cont);
        }
        vm->m_value = make_closure(vm->m_heap, operands, vm->m_env);
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

    intptr_t c_set_iloc(VM* vm, scm_closure_t operands) {
        scm_obj_t loc = CAR(operands);
        scm_obj_t* slot = c_lookup_iloc(vm, FIXNUM(CAR(loc)), FIXNUM(CDR(loc)));
        if (!STACKP(slot)) {
#if USE_PARALLEL_VM
            if (vm->m_interp->live_thread_count() > 1) {
                if (!vm->m_heap->in_heap(slot)) {
                  scm_obj_t doc = CDR(operands);
                  if (PAIRP(doc)) {
                      thread_lexical_access_violation(vm, CADAR(doc), vm->m_value);
                  } else {
                      thread_lexical_access_violation(vm, NULL, vm->m_value);
                  }
                  return 1;
                }
                if (vm->m_heap->m_child > 0) vm->m_interp->remember(*slot, vm->m_value);
            }
#endif
            vm->m_heap->write_barrier(vm->m_value);
        }
        *slot = vm->m_value;
        return 0;
    }

    void c_enclose(VM* vm, intptr_t argc) {
        vm_env_t env = (vm_env_t)((intptr_t)vm->m_env - offsetof(vm_env_rec_t, up));
        scm_obj_t* dst = (scm_obj_t*)env - env->count;
        if (STACKP(env)) {
            for (intptr_t i = 0; i < argc; i++) dst[i] = vm->m_fp[i];
        } else {
            for (intptr_t i = 0; i < argc; i++) {
                dst[i] = vm->m_fp[i];
                vm->m_heap->write_barrier(vm->m_fp[i]);
            }
        }
        vm->m_sp = vm->m_fp;
    }

    void c_extend_enclose(VM* vm, scm_obj_t operands) {
        vm->m_sp[0] = scm_undef;
        vm->m_sp++;
        vm_env_t env = (vm_env_t)vm->m_sp;
        env->count = 1;
        env->up = vm->m_env;
        vm->m_sp = vm->m_fp = (scm_obj_t*)(env + 1);
        vm->m_env = &env->up;
        vm->m_env = vm->save_env(vm->m_env);
        vm->update_cont(vm->m_cont);
        env = (vm_env_t)((intptr_t)vm->m_env - offsetof(vm_env_rec_t, up));
        scm_obj_t* slot = (scm_obj_t*)env - 1;
        *slot = make_closure(vm->m_heap, (scm_closure_t)operands, vm->m_env);
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

Function*
codegen_t::emit_prepare_call(context_t& ctx)
{
    LLVMContext& C = ctx.m_llvm_context;
    Module* M = ctx.m_module;

    DECLEAR_COMMON_TYPES;

    Function* F = Function::Create(FunctionType::get(VoidTy, {IntptrPtrTy, IntptrPtrTy}, false), Function::WeakAnyLinkage, "prepare_call", M);
    F->setCallingConv(CallingConv::Fast);
#if USE_LLVM_ATTRIBUTES
    F->addFnAttr(Attribute::NoUnwind);
    F->addParamAttr(0, Attribute::NoAlias);
    F->addParamAttr(0, Attribute::NoCapture);
    F->addParamAttr(1, Attribute::NoAlias);
    F->addParamAttr(1, Attribute::NoCapture);
#endif

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

    return F;
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
    if (is_compiled(closure)) return;
#if DEBUG_CODEGEN
    printer_t prt(vm, vm->m_current_output);
    prt.format("generating native code: ~s~&", closure->doc);
#endif

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

    context.m_intrinsics.prepare_call = emit_prepare_call(context);

    transform(context, closure->code, true);

    verifyModule(*M, &outs());

#if USE_LLVM_OPTIMIZE
    ExitOnErr(m_jit->addIRModule(optimizeModule(std::move(ThreadSafeModule(std::move(M), std::move(Context))))));
#else
    ExitOnErr(m_jit->addIRModule(std::move(ThreadSafeModule(std::move(M), std::move(Context)))));
#endif

//  m_jit->getMainJITDylib().dump(llvm::outs());

    auto symbol = ExitOnErr(m_jit->lookup(function_id));
    intptr_t (*thunk)(intptr_t) = (intptr_t (*)(intptr_t))symbol.getAddress();

    scm_bvector_t bv = make_bvector(vm->m_heap, sizeof(intptr_t));
    *(intptr_t*)bv->elts = (intptr_t)thunk;
    scm_obj_t n_code = CONS(LIST2(INST_NATIVE, bv), closure->code);
    vm->m_heap->write_barrier(n_code);
    closure->code = n_code;
    closure->hdr = closure->hdr | MAKEBITS(1, HDR_CLOSURE_COMPILED_SHIFT);

    m_lifted_functions.clear();

#if ENABLE_COMPILE_DEFERRED
    if (m_deferred_compile.size()) {
        scm_closure_t closure = m_deferred_compile.back();
        m_deferred_compile.pop_back();
        //printer_t prt(m_vm, m_vm->m_current_output);
        //prt.format("deferred compile closure: ~s~&~!", closure->doc);
        compile(closure);
        closure->hdr = closure->hdr | MAKEBITS(1, HDR_CLOSURE_COMPILED_SHIFT);
    }
#endif

}

Function*
codegen_t::get_function(context_t& ctx, scm_closure_t closure)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;

    assert(is_compiled(closure));
    scm_bvector_t bv = (scm_bvector_t)CADR(CAR(closure->code));
    intptr_t (*adrs)(intptr_t) = (intptr_t (*)(intptr_t))(*(intptr_t*)bv->elts);
    auto subrType = FunctionType::get(IntptrTy, {IntptrPtrTy}, false);
    Function* func = (Function*)ConstantExpr::getIntToPtr(VALUE_INTPTR(adrs), subrType->getPointerTo());
    return func;
}

Function*
codegen_t::emit_inner_function(context_t& ctx, scm_closure_t closure)
{
    VM* vm = m_vm;

    auto search = m_lifted_functions.find(closure);
    if (search != m_lifted_functions.end()) {
#if DEBUG_CODEGEN
      puts(" + found in m_lifted_functions, return Function*");
#endif
      return search->second;
    }

    if (is_compiled(closure)) {
#if DEBUG_CODEGEN
        puts(" + emit_inner_function: already compiled");
#endif
        return get_function(ctx, closure);
    }

#if DEBUG_CODEGEN
    puts(" + generating native code for lifted function");
#endif

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
    context.m_intrinsics = ctx.m_intrinsics;

    transform(context, closure->code, true);

    return F;
}

void
codegen_t::emit_stack_overflow_check(context_t& ctx, int nbytes)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    auto vm = F->arg_begin();

    if (nbytes) {
        //printf("emit_stack_overflow_check: %d\n", nbytes);
        auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
        auto stack_limit = CREATE_LOAD_VM_REG(vm, m_stack_limit);
        BasicBlock* stack_ok = BasicBlock::Create(C, "stack_ok", F);
        BasicBlock* stack_overflow = BasicBlock::Create(C, "stack_overflow", F);
        Value* stack_cond = IRB.CreateICmpULT(IRB.CreateAdd(sp, VALUE_INTPTR(nbytes)), stack_limit);
        IRB.CreateCondBr(stack_cond, stack_ok, stack_overflow);
        IRB.SetInsertPoint(stack_overflow);
        auto c_collect_stack = M->getOrInsertFunction("c_collect_stack", VoidTy, IntptrPtrTy, IntptrTy);
        IRB.CreateCall(c_collect_stack, {vm, VALUE_INTPTR(nbytes)});
        IRB.CreateBr(stack_ok);
        IRB.SetInsertPoint(stack_ok);
    }
}

int
codegen_t::calc_stack_size(scm_obj_t inst)
{
    int require = 0;
    int n = 0;
    while (inst != scm_nil) {
        switch (VM::instruction_to_opcode(CAAR(inst))) {
            case VMOP_IF_FALSE_CALL: {
              scm_obj_t operands = CDAR(inst);
              int n2 = calc_stack_size(operands);
              if (n + n2 > require) require = n + n2;
            } break;
            case VMOP_CALL: {
              n += sizeof(vm_cont_rec_t);
            } break;
            case VMOP_PUSH_GLOC:
            case VMOP_PUSH_SUBR:
            case VMOP_PUSH_CAR_ILOC:
            case VMOP_PUSH_CDR_ILOC:
            case VMOP_PUSH_ILOC0:
            case VMOP_PUSH_ILOC:
            case VMOP_PUSH:
            case VMOP_PUSH_CONST:
            case VMOP_PUSH_ILOC1: {
              n += sizeof(scm_obj_t);
            } break;
            case VMOP_APPLY_GLOC: {
              n += sizeof(vm_env_rec_t);
            } break;
            case VMOP_APPLY_ILOC: {
              n += sizeof(vm_env_rec_t);
            } break;
            case VMOP_APPLY_ILOC_LOCAL: {
              n += sizeof(vm_env_rec_t);
            } break;
            case VMOP_APPLY: {
              n += sizeof(vm_env_rec_t);
            } break;
            case VMOP_EXTEND: {
              n += sizeof(vm_env_rec_t);
            } break;
            case VMOP_EXTEND_ENCLOSE: {
              n += sizeof(scm_obj_t);
              n += sizeof(vm_env_rec_t);
            } break;
            case VMOP_EXTEND_ENCLOSE_LOCAL: {
              n += sizeof(scm_obj_t);
              n += sizeof(vm_env_rec_t);
            } break;
            case VMOP_EXTEND_UNBOUND: {
              scm_obj_t operands = CDAR(inst);
              int argc = FIXNUM(operands);
              n += sizeof(vm_env_rec_t);
              n += sizeof(scm_obj_t) * argc;
            } break;
            case VMOP_PUSH_CLOSE: {
              n += sizeof(scm_obj_t);
            } break;
            case VMOP_PUSH_CLOSE_LOCAL: {
              n += sizeof(scm_obj_t);
            } break;
            case VMOP_IF_TRUE:
            case VMOP_IF_EQP:
            case VMOP_IF_NULLP:
            case VMOP_IF_PAIRP:
            case VMOP_IF_SYMBOLP: {
              scm_obj_t operands = CDAR(inst);
              int n2 = calc_stack_size(operands);
              if (n + n2 > require) require = n + n2;
            } break;
            case VMOP_PUSH_NADD_ILOC:
            case VMOP_PUSH_CADR_ILOC:
            case VMOP_PUSH_CDDR_ILOC: {
              n += sizeof(scm_obj_t);
            } break;
            default:
                break;
        }
        if (n > require) require = n;
        inst = CDR(inst);
    }
    return require;
}

void
codegen_t::transform(context_t ctx, scm_obj_t inst, bool insert_stack_check)
{
  #if USE_UNIFIED_STACK_CHECK
    if (insert_stack_check) emit_stack_overflow_check(ctx, calc_stack_size(inst));
  #endif
    //printf("stack calc: %d\n", calc_stack_size(inst));
    while (inst != scm_nil) {
        // printf("emit: %s\n", ((scm_symbol_t)CAAR(inst))->name);
        switch (VM::instruction_to_opcode(CAAR(inst))) {
            case VMOP_IF_FALSE_CALL: {
                emit_if_false_call(ctx, inst);
            } break;
            case VMOP_CALL: {
                ctx.m_function = emit_call(ctx, inst);
            } break;
            case VMOP_RET_GLOC: {
                emit_ret_gloc(ctx, inst);
            } break;
            case VMOP_RET_CONST: {
                emit_ret_const(ctx, inst);
            } break;
            case VMOP_RET_ILOC: {
                emit_ret_iloc(ctx, inst);
            } break;
            case VMOP_PUSH_GLOC: {
                emit_push_gloc(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_PUSH_SUBR: {
                emit_push_subr(ctx, inst);
                intptr_t argc = FIXNUM(CADR(CDAR(inst)));
                ctx.m_argc = ctx.m_argc - argc + 1;
            } break;
            case VMOP_PUSH_CAR_ILOC: {
                emit_push_car_iloc(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_PUSH_CDR_ILOC: {
                emit_push_cdr_iloc(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_PUSH_ILOC0: {
                emit_push_iloc0(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_PUSH_ILOC: {
                emit_push_iloc(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_PUSH: {
                emit_push(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_PUSH_CONST: {
                emit_push_const(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_PUSH_ILOC1: {
                emit_push_iloc1(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_APPLY_GLOC: {
                emit_apply_gloc(ctx, inst);
            } break;
            case VMOP_RET_SUBR: {
                emit_ret_subr(ctx, inst);
            } break;
            case VMOP_APPLY_ILOC: {
                emit_apply_iloc(ctx, inst);
            } break;
            case VMOP_APPLY_ILOC_LOCAL: {
                emit_apply_iloc_local(ctx, inst);
            } break;
            case VMOP_APPLY: {
                emit_apply(ctx, inst);
            } break;
            case VMOP_EXTEND: {
                emit_extend(ctx, inst);
                ctx.m_argc = 0;
                ctx.m_depth++;
            } break;
            case VMOP_EXTEND_ENCLOSE: {
                emit_extend_enclose(ctx, inst);
                ctx.m_argc = 0;
                ctx.m_depth++;
            } break;
            case VMOP_EXTEND_ENCLOSE_LOCAL: {
                emit_extend_enclose_local(ctx, inst);
                ctx.m_argc = 0;
                ctx.m_depth++;
            } break;
            case VMOP_EXTEND_UNBOUND: {
                emit_extend_unbound(ctx, inst);
                ctx.m_argc = 0;
                ctx.m_depth++;
            } break;
            case VMOP_PUSH_CLOSE: {
                emit_push_close(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_PUSH_CLOSE_LOCAL: {
                emit_push_close_local(ctx, inst);
                ctx.m_argc++;
            } break;
            case VMOP_ENCLOSE: {
                emit_enclose(ctx, inst);
                ctx.m_argc = 0;
            } break;
            case VMOP_GLOC: {
                emit_gloc(ctx, inst);
            } break;
            case VMOP_ILOC: {
                emit_iloc(ctx, inst);
            } break;
            case VMOP_CAR_ILOC: {
                emit_car_iloc(ctx, inst);
            } break;
            case VMOP_CDR_ILOC: {
                emit_cdr_iloc(ctx, inst);
            } break;
            case VMOP_CONST: {
                emit_const(ctx, inst);
            } break;
            case VMOP_SUBR: {
                emit_subr(ctx, inst);
                intptr_t argc = FIXNUM(CADR(CDAR(inst)));
                ctx.m_argc = ctx.m_argc - argc;
            } break;
            case VMOP_ILOC1: {
                emit_iloc1(ctx, inst);
            } break;
            case VMOP_ILOC0: {
                emit_iloc0(ctx, inst);
            } break;
            case VMOP_IF_TRUE: {
                emit_if_true(ctx, inst);
            } break;
            case VMOP_IF_NULLP_RET_CONST: {
                emit_if_nullp_ret_const(ctx, inst);
            } break;
            case VMOP_IF_EQP: {
                ctx.m_argc--;
                emit_if_eqp(ctx, inst);
            } break;
            case VMOP_IF_NULLP: {
                emit_if_nullp(ctx, inst);
            } break;
            case VMOP_IF_PAIRP: {
                emit_if_pairp(ctx, inst);
            } break;
            case VMOP_IF_SYMBOLP: {
                emit_if_symbolp(ctx, inst);
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
            case VMOP_IF_FALSE_RET_CONST: {
                emit_if_false_ret_const(ctx, inst);
            } break;
            case VMOP_IF_EQP_RET_CONST: {
                ctx.m_argc--;
                emit_if_eqp_ret_const(ctx, inst);
            } break;
            case VMOP_IF_PAIRP_RET_CONST: {
                emit_if_pairp_ret_const(ctx, inst);
            } break;
            case VMOP_IF_SYMBOLP_RET_CONST: {
                emit_if_symbolp_ret_const(ctx, inst);
            } break;
            case VMOP_IF_NOT_PAIRP_RET_CONST: {
                emit_if_not_pairp_ret_const(ctx, inst);
            } break;
            case VMOP_IF_NOT_NULLP_RET_CONST: {
                emit_if_not_nullp_ret_const(ctx, inst);
            } break;
            case VMOP_IF_NOT_EQP_RET_CONST: {
                ctx.m_argc--;
                emit_if_not_eqp_ret_const(ctx, inst);
            } break;
            case VMOP_IF_NOT_SYMBOLP_RET_CONST: {
                emit_if_not_symbolp_ret_const(ctx, inst);
            } break;
            case VMOP_CLOSE: {
                emit_close(ctx, inst);
            } break;
            case VMOP_SET_GLOC: {
                emit_set_gloc(ctx, inst);
            } break;
            case VMOP_SET_ILOC: {
                emit_set_iloc(ctx, inst);
            } break;
            case VMOP_PUSH_CONS: {
                emit_push_cons(ctx, inst);
            } break;
            case VMOP_RET_CONS: {
                emit_ret_cons(ctx, inst);
            } break;
            case VMOP_RET_EQP: {
                emit_ret_eqp(ctx, inst);
            } break;
            case VMOP_RET_NULLP: {
                emit_ret_nullp(ctx, inst);
            } break;
            case VMOP_RET_PAIRP: {
                emit_ret_pairp(ctx, inst);
            } break;
            case VMOP_RET_CLOSE: {
                emit_ret_close(ctx, inst);
            } break;
            case VMOP_PUSH_NADD_ILOC: {
                emit_push_nadd_iloc(ctx, inst);
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
            case VMOP_CADR_ILOC: {
                emit_cadr_iloc(ctx, inst);
            } break;
            case VMOP_CDDR_ILOC: {
                emit_cddr_iloc(ctx, inst);
            } break;
            case VMOP_EQ_N_ILOC: {
                emit_eq_n_iloc(ctx, inst);
            } break;
            case VMOP_LT_N_ILOC: {
                emit_lt_n_iloc(ctx, inst);
            } break;
            case VMOP_GE_N_ILOC: {
                emit_ge_n_iloc(ctx, inst);
            } break;
            case VMOP_LE_N_ILOC: {
                emit_le_n_iloc(ctx, inst);
            } break;
            case VMOP_GT_N_ILOC: {
                emit_gt_n_iloc(ctx, inst);
            } break;
            case VMOP_NADD_ILOC: {
                emit_nadd_iloc(ctx, inst);
            } break;
            case VMOP_EQ_ILOC: {
                emit_eq_iloc(ctx, inst);
            } break;
            case VMOP_LT_ILOC: {
                emit_lt_iloc(ctx, inst);
            } break;
            case VMOP_LE_ILOC: {
                emit_le_iloc(ctx, inst);
            } break;
            case VMOP_GT_ILOC: {
                emit_gt_iloc(ctx, inst);
            } break;
            case VMOP_GE_ILOC: {
                emit_ge_iloc(ctx, inst);
            } break;
            // VMOP_PUSH_VECTREF_ILOC  remove
            // VMOP_VECTREF_ILOC       remove
            case VMOP_NATIVE: {
                fatal("codegen.cpp: unexpected opcode VMOP_NATIVE");
            } break;
            case VMOP_TOUCH_GLOC: {
                // nop
            } break;
            case VMOP_SUBR_GLOC_OF: {
                emit_subr_gloc_of(ctx, inst);
                intptr_t argc = FIXNUM(CADR(CDAR(inst)));
                ctx.m_argc = ctx.m_argc - argc;
            } break;
            case VMOP_PUSH_SUBR_GLOC_OF: {
                emit_push_subr_gloc_of(ctx, inst);
                intptr_t argc = FIXNUM(CADR(CDAR(inst)));
                ctx.m_argc = ctx.m_argc - argc + 1;
            } break;
            case VMOP_RET_SUBR_GLOC_OF: {
                emit_ret_subr_gloc_of(ctx, inst);
            } break;
            case VMOP_VM_ESCAPE: {
                emit_escape(ctx, inst);
            } break;
            default:
                printf("##### unsupported instruction %s ######\n", ((scm_symbol_t)CAAR(inst))->name);
                break;
        }
        inst = CDR(inst);
    }
}

void
codegen_t::emit_cond_pairp(context_t& ctx, Value* obj, BasicBlock* pair_true, BasicBlock* pair_false)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    auto vm = F->arg_begin();

    BasicBlock* cond1_true = BasicBlock::Create(C, "cond1_true", F);
    auto cond1 = IRB.CreateICmpEQ(IRB.CreateAnd(obj, VALUE_INTPTR(0x7)), VALUE_INTPTR(0x0));
    IRB.CreateCondBr(cond1, cond1_true, pair_false);
    IRB.SetInsertPoint(cond1_true);
    auto hdr = IRB.CreateLoad(IRB.CreateBitOrPointerCast(obj, IntptrPtrTy));
    auto cond2 = IRB.CreateICmpNE(IRB.CreateAnd(hdr, VALUE_INTPTR(0xf)), VALUE_INTPTR(0xa));
    IRB.CreateCondBr(cond2, pair_true, pair_false);
}

void
codegen_t::emit_cond_symbolp(context_t& ctx, Value* obj, BasicBlock* symbol_true, BasicBlock* symbol_false)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    auto vm = F->arg_begin();

    BasicBlock* cond1_true = BasicBlock::Create(C, "cond1_true", F);
    auto cond1 = IRB.CreateICmpEQ(IRB.CreateAnd(obj, VALUE_INTPTR(0x7)), VALUE_INTPTR(0x0));
    IRB.CreateCondBr(cond1, cond1_true, symbol_false);
    IRB.SetInsertPoint(cond1_true);
    auto hdr = IRB.CreateLoad(IRB.CreateBitOrPointerCast(obj, IntptrPtrTy));
    auto cond2 = IRB.CreateICmpEQ(IRB.CreateAnd(hdr, VALUE_INTPTR(HDR_TYPE_MASKBITS)), VALUE_INTPTR(scm_hdr_symbol));
    IRB.CreateCondBr(cond2, symbol_true, symbol_false);
}

#include "codegen.inc.cpp"


/*
(import (digamma pregexp))

(pregexp "^([A-F0-9]{4,6});[^;]*;([a-zA-Z]{2});[^;]*;[^;]*;[^;]*;[^;]*;[^;]*;(.*);[^;]*;[^;]*;[^;]*;([A-F0-9]{0,6});([A-F0-9]{0,6});([A-F0-9]{0,6})$")
(pregexp "^a")

define-thread-variable have issue

(import (digamma concurrent))
(define-thread-variable foo #t)
foo

  (define-syntax define-thread-variable
    (syntax-rules ()
      ((_ var init)
       (begin
         (define param (make-parameter (list init)))
         (define mutator
           (lambda (val)
             (format #t "val ~s~%~!" val)
             (param (list val))))
         (define accessor
           (lambda ()
             (let ((p (param)))
               (format #t "p ~s~%~!" p)
               (if (local-heap-object? p)
                   (car p)
                   (let ((val init)) (param (list val)) val)))))
         (define-syntax var (identifier-syntax (_ (accessor)) ((set! _ x) (mutator x))))))))

  (define-thread-variable foo #t)
  foo

*/
/*
(begin
  (define param (make-parameter (list #t)))
  (define mutator
    (lambda (val)
      (format #t "val ~s~%~!" val)
      (param (list val))))
  (define accessor
    (lambda ()
      (let ((p (param)))
        (format #t "p ~s~%~!" p)
        (if (local-heap-object? p)
            (car p)
            (let ((val #t)) (param (list val)) val)))))
  (define-syntax foo (identifier-syntax (_ (accessor)) ((set! _ x) (mutator x))))
)
*/
