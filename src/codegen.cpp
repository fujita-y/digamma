// Copyright (c) 2004-2020 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "codegen.h"
#include "arith.h"
#include "printer.h"
#include "violation.h"
#include "uuid.h"
#include "vmm.h"
#include "port.h"

#include "llvm/IR/Verifier.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Error.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

#if __clang_major__ > 9
using namespace std;
#endif

using namespace llvm;
using namespace llvm::orc;

#include "codegen.macro.cpp"
#include "codegen.thunk.cpp"

static ExitOnError ExitOnErr;

static int log2_of_intptr_size()
{
    if (sizeof(intptr_t) == 4) return 2;
    if (sizeof(intptr_t) == 8) return 3;
    return (int)log2(sizeof(intptr_t));
}

codegen_t::codegen_t(VM* vm) : m_vm(vm) { }

void
codegen_t::init()
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
#if ENABLE_COMPILE_THREAD
    m_compile_thread_terminating = false;
    m_compile_thread_lock.init();
    m_compile_thread_wake.init();
    m_compile_queue_lock.init();
    thread_start(compile_thread, this);
#endif
}

void
codegen_t::destroy()
{
#if ENABLE_COMPILE_THREAD
    m_compile_thread_terminating = true;
    {
        scoped_lock lock(m_compile_thread_lock);
        m_compile_thread_wake.signal();
    }
    do {
      usleep(100);
    } while (m_compile_thread_terminating);
    m_compile_thread_lock.destroy();
    m_compile_thread_wake.destroy();
    m_compile_queue_lock.destroy();
#endif
}

#if ENABLE_COMPILE_THREAD
thread_main_t
codegen_t::compile_thread(void* param)
{
    codegen_t& codegen = *(codegen_t*)param;
    codegen.m_compile_thread_lock.lock();
    codegen.m_compile_thread_ready = true;
    while (!codegen.m_compile_thread_terminating) {
        codegen.m_compile_thread_wake.wait(codegen.m_compile_thread_lock);
        codegen.m_compile_thread_ready = false;
        do {
            if (codegen.m_compile_thread_terminating) break;
            scm_closure_t closure = NULL;
            {
                scoped_lock lock(codegen.m_compile_queue_lock);
                if (codegen.m_compile_queue.size()) {
                    closure = codegen.m_compile_queue.back();
                }
            }
            //printf("compile thread: closure %p\n", closure);
            if (closure) codegen.compile_each(closure);
            {
                scoped_lock lock(codegen.m_compile_queue_lock);
                codegen.m_compile_queue.erase(std::remove(codegen.m_compile_queue.begin(), codegen.m_compile_queue.end(), closure), codegen.m_compile_queue.end());
                int n_more = codegen.m_compile_queue.size();
                if (n_more == 0) {
                    codegen.m_compile_thread_ready = true;
                }
            }
        } while (!codegen.m_compile_thread_ready);
    }
    codegen.m_compile_thread_lock.unlock();
    codegen.m_compile_thread_terminating = false;
    return NULL;
}
#endif

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

#if PRINT_IR
    puts("*** IR after optimize ***");
    M.print(outs(), nullptr);
#endif

    return std::move(TSM);
}

bool
codegen_t::is_compiled(scm_closure_t closure)
{
    VM* vm = m_vm;
    return closure->code != NULL;
}

// compile one top-level function
void
codegen_t::compile(scm_closure_t closure)
{
#if ENABLE_COMPILE_THREAD
    if (m_compile_thread_terminating) return;
    {
        scoped_lock lock(m_compile_queue_lock);
        if (std::find(m_compile_queue.begin(), m_compile_queue.end(), closure) != m_compile_queue.end()) return;
        m_compile_queue.push_back(closure);
    }
    if (m_compile_thread_ready) {
        scoped_lock lock(m_compile_thread_lock);
        m_compile_thread_wake.signal();
    }

#else
  #if ENABLE_COMPILE_DEFERRED
    if (std::find(m_compile_queue.begin(), m_compile_queue.end(), closure) == m_compile_queue.end()) {
        m_compile_queue.push_back(closure);

        while (m_compile_queue.size()) {
            scm_closure_t closure = m_compile_queue.back();
            //printer_t prt(m_vm, m_vm->m_current_output);
            //prt.format("deferred compile closure: ~s~&~!", closure->doc);
            compile_each(closure);
            m_compile_queue.erase(std::remove(m_compile_queue.begin(), m_compile_queue.end(), closure), m_compile_queue.end());
        }
    }
  #else
    compile_each(closure);
  #endif
#endif
}

void
codegen_t::compile_each(scm_closure_t closure)
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
#if USE_LLVM_ATTRIBUTES
    for (Argument& argument : F->args()) { argument.addAttr(Attribute::NoAlias); argument.addAttr(Attribute::NoCapture); }
#endif
    BasicBlock* ENTRY = BasicBlock::Create(C, "entry", F);
    IRBuilder<> IRB(ENTRY);

    context_t context(C, IRB);
    context.m_module = M.get();
    context.m_function = F;
    context.m_top_level_closure = closure;
    context.m_top_level_function = F;

    context.m_intrinsics.prepare_call = emit_prepare_call(context);

    transform(context, closure->pc, true);

    verifyModule(*M, &outs());

#if USE_LLVM_OPTIMIZE
    ExitOnErr(m_jit->addIRModule(optimizeModule(std::move(ThreadSafeModule(std::move(M), std::move(Context))))));
#else
    ExitOnErr(m_jit->addIRModule(std::move(ThreadSafeModule(std::move(M), std::move(Context)))));
#endif

    //m_jit->getMainJITDylib().dump(llvm::outs());

    auto symbol = ExitOnErr(m_jit->lookup(function_id));
    intptr_t (*thunk)(intptr_t) = (intptr_t (*)(intptr_t))symbol.getAddress();

    if (m_usage.min_sym > (uintptr_t)thunk) m_usage.min_sym = (uintptr_t)thunk;
    if (m_usage.max_sym < (uintptr_t)thunk) m_usage.max_sym = (uintptr_t)thunk;

    closure->code = (void*)thunk;
    m_lifted_functions.clear();
}

Function*
codegen_t::get_function(context_t& ctx, scm_closure_t closure)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;

    if (!is_compiled(closure)) fatal("%s:%u closure is not compiled", __FILE__, __LINE__);

    intptr_t (*adrs)(intptr_t) = (intptr_t (*)(intptr_t))(closure->code);
    auto subrType = FunctionType::get(IntptrTy, {IntptrPtrTy}, false);
    Function* func = (Function*)ConstantExpr::getIntToPtr(VALUE_INTPTR(adrs), subrType->getPointerTo());
    return func;
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
              scm_obj_t operands = CDAR(inst);
              int n2 = calc_stack_size(operands);
              if (n + n2 > require) require = n + n2;
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
              n += sizeof(scm_obj_t) * argc;
              n += sizeof(vm_env_rec_t);
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
    while (inst != scm_nil) {
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
                fatal("%s:%u encounter unsupported instruction %s", __FILE__, __LINE__, ((scm_symbol_t)CAAR(inst))->name);
                break;
        }
        inst = CDR(inst);
    }
}

void
codegen_t::display_codegen_statistics(scm_port_t port)
{
    scoped_lock lock(port->lock);
    port_put_byte(port, '\n');
    port_format(port, "interned top-level function   : %d\n", m_usage.globals);
    port_format(port, "uninterned top-level function : %d\n", m_usage.inners);
    port_format(port, "local loop function           : %d\n", m_usage.locals);
    port_format(port, "closure template              : %d\n", m_usage.templates);
    port_format(port, "symbol space (max - min)      : %.2fM\n\n", (m_usage.max_sym - m_usage.min_sym) / (1024.0 * 1024));
    port_flush_output(port);
}

#include "codegen.inst0.cpp"
#include "codegen.inst1.cpp"
