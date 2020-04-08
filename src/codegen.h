// Copyright (c) 2004-2020 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef CODEGEN_H_INCLUDED
#define CODEGEN_H_INCLUDED

#include "core.h"
#include "object.h"
#include "vm.h"

#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

#define USE_LLVM_ATTRIBUTES       1
#define USE_LLVM_OPTIMIZE         1
#define USE_ILOC_OPTIMIZE         1
#define USE_REG_CACHE             1

#define PRINT_IR                  0
#define DEBUG_CODEGEN             0
#define VERBOSE_CODEGEN           0

class codegen_t {
    struct context_t;
    template<int byte_offset> struct reg_cache_t {
        codegen_t::context_t* ctx;
        llvm::Value* val;
        bool need_write_back;
        llvm::IntegerType* IntptrTy;
        llvm::LLVMContext& C;
        llvm::IRBuilder<>& IRB;
        llvm::Value* load(llvm::Value* vm);
        void store(llvm::Value* vm, llvm::Value* rhs);
        void writeback(llvm::Value* vm);
        void copy(llvm::Value* vm);
        void clear();
        reg_cache_t(codegen_t::context_t* context);
    };
    struct context_t {
        llvm::LLVMContext& m_llvm_context;
        llvm::Module* m_module;
        llvm::Function* m_function;
        llvm::IRBuilder<>& m_irb;
        llvm::Function* m_top_level_function;
        scm_closure_t m_top_level_closure;
        std::map<int, llvm::Function*> m_local_functions;
        std::vector<int> m_local_var_count;
        int m_argc;
        int m_depth;
        reg_cache_t<offsetof(VM, m_sp)> reg_sp;
        reg_cache_t<offsetof(VM, m_fp)> reg_fp;
        reg_cache_t<offsetof(VM, m_env)> reg_env;
        reg_cache_t<offsetof(VM, m_cont)> reg_cont;
        reg_cache_t<offsetof(VM, m_value)> reg_value;
        bool m_disable_reg_cache;
        void reg_cache_copy(llvm::Value* vm);
        void reg_cache_copy_only_value_and_cont(llvm::Value* vm);
        void reg_cache_copy_except_sp(llvm::Value* vm);
        void reg_cache_copy_except_value(llvm::Value* vm);
        void reg_cache_copy_except_value_and_sp(llvm::Value* vm);
        void reg_cache_clear();
        void update_reg_cache_context();
        void set_local_var_count(int depth, int count);
        void set_local_var_count(int depth, scm_closure_t closure);
        int get_local_var_count(int depth);
        context_t(llvm::LLVMContext& llvm_context, llvm::IRBuilder<>& irb)
          : m_llvm_context(llvm_context), m_irb(irb), m_argc(0), m_depth(0), m_disable_reg_cache(false),
            reg_sp(this), reg_fp(this), reg_env(this), reg_cont(this), reg_value(this) { }
    };
    class reg_cache_synchronize {
        codegen_t::context_t& ctx;
        bool previous;
    public:
        reg_cache_synchronize(codegen_t::context_t& context) : ctx(context), previous(ctx.m_disable_reg_cache) {
            auto vm = ctx.m_function->arg_begin();
            ctx.reg_cache_copy(vm);
            ctx.reg_cache_clear();
            ctx.m_disable_reg_cache = true;
        }
        ~reg_cache_synchronize() {
            ctx.m_disable_reg_cache = previous;
        }
    };
    enum cc_t { LT, GT, LE, GE, EQ, };
    VM* m_vm;
    std::unique_ptr<llvm::orc::LLJIT> m_jit;
    std::map<scm_closure_t,llvm::Function*> m_lifted_functions;
#if ENABLE_COMPILE_THREAD
    mutex_t m_compile_thread_lock;
    cond_t m_compile_thread_wake;
    bool m_compile_thread_ready;
    bool m_compile_thread_terminating;
    static thread_main_t compile_thread(void* param);
#endif
    llvm::orc::ThreadSafeModule optimizeModule(llvm::orc::ThreadSafeModule TSM);
    void define_prepare_call();
    void transform(context_t ctx, scm_obj_t inst, bool insert_stack_check);
    llvm::Function* get_function(context_t& ctx, scm_closure_t closure);
public:
    codegen_t(VM* vm);
    void init();
    void destroy();
    void compile(scm_closure_t closure);
    bool is_compiled(scm_closure_t closure);
    bool m_debug;
#if ENABLE_COMPILE_DEFERRED
    std::vector<scm_closure_t> m_compile_queue;
    mutex_t m_compile_queue_lock;
#endif
    struct usage_t {
        int globals;
        int locals;
        int inners;
        int templates;
        uintptr_t min_sym;
        uintptr_t max_sym;
        usage_t() : globals(0), locals(0), inners(0), templates(0), min_sym(UINTPTR_MAX), max_sym(0) {}
    } m_usage;
    void display_codegen_statistics(scm_port_t port);
private:
    void compile_each(scm_closure_t closure);
    int calc_stack_size(scm_obj_t inst);
    void emit_stack_overflow_check(context_t& ctx, int nbytes);
    void emit_push_vm_stack(context_t& ctx, llvm::Value* val);
    void emit_prepair_apply(context_t& ctx, scm_closure_t closure);
    // llvm::Function* emit_define_prepare_call(context_t& ctx);
    void emit_cond_pairp(context_t& ctx, llvm::Value* obj, llvm::BasicBlock* pair_true, llvm::BasicBlock* pair_false);
    void emit_cond_symbolp(context_t& ctx, llvm::Value* obj, llvm::BasicBlock* symbol_true, llvm::BasicBlock* symbol_false);
    llvm::Function* emit_inner_function(context_t& ctx, scm_closure_t closure);
    llvm::Value* emit_lookup_env(context_t& ctx, intptr_t depth);
    llvm::Value* emit_lookup_iloc(context_t& ctx, intptr_t depth, intptr_t index);
    llvm::Value* emit_lookup_iloc(context_t& ctx, scm_obj_t inst);
    llvm::Value* emit_cmp_inst(context_t& ctx, cc_t cc, llvm::Value* lhs, llvm::Value* rhs);
    void emit_cc_n_iloc(context_t& ctx, scm_obj_t inst, cc_t cc, const char* cfunc);
    void emit_cc_iloc(context_t& ctx, scm_obj_t inst, cc_t cc, const char* cfunc);
    void emit_push_subr(context_t& ctx, scm_obj_t inst, scm_subr_t subr);
    void emit_subr(context_t& ctx, scm_obj_t inst, scm_subr_t subr);
    void emit_ret_subr(context_t& ctx, scm_obj_t inst, scm_subr_t subr);
    llvm::Function* emit_call(context_t& ctx, scm_obj_t inst);
    void emit_if_false_call(context_t& ctx, scm_obj_t inst);
    void emit_subr(context_t& ctx, scm_obj_t inst);
    void emit_push(context_t& ctx, scm_obj_t inst);
    void emit_push_const(context_t& ctx, scm_obj_t inst);
    void emit_push_iloc0(context_t& ctx, scm_obj_t inst);
    void emit_push_iloc1(context_t& ctx, scm_obj_t inst);
    void emit_push_gloc(context_t& ctx, scm_obj_t inst);
    void emit_push_subr(context_t& ctx, scm_obj_t inst);
    void emit_push_car_iloc(context_t& ctx, scm_obj_t inst);
    void emit_push_cdr_iloc(context_t& ctx, scm_obj_t inst);
    void emit_push_cadr_iloc(context_t& ctx, scm_obj_t inst);
    void emit_push_cddr_iloc(context_t& ctx, scm_obj_t inst);
    void emit_push_nadd_iloc(context_t& ctx, scm_obj_t inst);
    void emit_push_iloc(context_t& ctx, scm_obj_t inst);
    void emit_push_cons(context_t& ctx, scm_obj_t inst);
    void emit_push_close(context_t& ctx, scm_obj_t inst);
    void emit_apply_iloc(context_t& ctx, scm_obj_t inst);
    void emit_apply_gloc(context_t& ctx, scm_obj_t inst);
    void emit_ret_const(context_t& ctx, scm_obj_t inst);
    void emit_ret_iloc(context_t& ctx, scm_obj_t inst);
    void emit_ret_cons(context_t& ctx, scm_obj_t inst);
    void emit_ret_subr(context_t& ctx, scm_obj_t inst);
    void emit_if_true(context_t& ctx, scm_obj_t inst);
    void emit_if_nullp(context_t& ctx, scm_obj_t inst);
    void emit_if_nullp_ret_const(context_t& ctx, scm_obj_t inst);
    void emit_if_not_nullp_ret_const(context_t& ctx, scm_obj_t inst);
    void emit_if_symbolp_ret_const(context_t& ctx, scm_obj_t inst);
    void emit_if_not_symbolp_ret_const(context_t& ctx, scm_obj_t inst);
    void emit_if_pairp_ret_const(context_t& ctx, scm_obj_t inst);
    void emit_if_eqp_ret_const(context_t& ctx, scm_obj_t inst);
    void emit_if_true_ret(context_t& ctx, scm_obj_t inst);
    void emit_if_false_ret(context_t& ctx, scm_obj_t inst);
    void emit_if_eqp(context_t& ctx, scm_obj_t inst);
    void emit_gloc(context_t& ctx, scm_obj_t inst);
    void emit_iloc(context_t& ctx, scm_obj_t inst);
    void emit_iloc0(context_t& ctx, scm_obj_t inst);
    void emit_iloc1(context_t& ctx, scm_obj_t inst);
    void emit_car_iloc(context_t& ctx, scm_obj_t inst);
    void emit_cdr_iloc(context_t& ctx, scm_obj_t inst);
    void emit_lt_n_iloc(context_t& ctx, scm_obj_t inst);
    void emit_gt_n_iloc(context_t& ctx, scm_obj_t inst);
    void emit_ge_n_iloc(context_t& ctx, scm_obj_t inst);
    void emit_le_n_iloc(context_t& ctx, scm_obj_t inst);
    void emit_eq_n_iloc(context_t& ctx, scm_obj_t inst);
    void emit_extend(context_t& ctx, scm_obj_t inst);
    void emit_extend_enclose(context_t& ctx, scm_obj_t inst);
    void emit_extend_enclose_local(context_t& ctx, scm_obj_t inst);
    void emit_apply_iloc_local(context_t& ctx, scm_obj_t inst);
    void emit_if_true_ret_const(context_t& ctx, scm_obj_t inst);
    void emit_lt_iloc(context_t& ctx, scm_obj_t inst);
    void emit_gt_iloc(context_t& ctx, scm_obj_t inst);
    void emit_le_iloc(context_t& ctx, scm_obj_t inst);
    void emit_ge_iloc(context_t& ctx, scm_obj_t inst);
    void emit_eq_iloc(context_t& ctx, scm_obj_t inst);
    void emit_set_gloc(context_t& ctx, scm_obj_t inst);
    void emit_const(context_t& ctx, scm_obj_t inst);
    void emit_if_pairp(context_t& ctx, scm_obj_t inst);
    void emit_if_symbolp(context_t& ctx, scm_obj_t inst);
    void emit_cadr_iloc(context_t& ctx, scm_obj_t inst);
    void emit_cddr_iloc(context_t& ctx, scm_obj_t inst);
    void emit_if_not_pairp_ret_const(context_t& ctx, scm_obj_t inst);
    void emit_if_not_eqp_ret_const(context_t& ctx, scm_obj_t inst);
    void emit_if_false_ret_const(context_t& ctx, scm_obj_t inst);
    void emit_ret_nullp(context_t& ctx, scm_obj_t inst);
    void emit_ret_pairp(context_t& ctx, scm_obj_t inst);
    void emit_ret_gloc(context_t& ctx, scm_obj_t inst);
    void emit_ret_eqp(context_t& ctx, scm_obj_t inst);
    void emit_set_iloc(context_t& ctx, scm_obj_t inst);
    void emit_extend_unbound(context_t& ctx, scm_obj_t inst);
    void emit_enclose(context_t& ctx, scm_obj_t inst);
    void emit_push_close_local(context_t& ctx, scm_obj_t inst);
    void emit_close(context_t& ctx, scm_obj_t inst);
    void emit_ret_close(context_t& ctx, scm_obj_t inst);
    void emit_nadd_iloc(context_t& ctx, scm_obj_t inst);
    void emit_apply(context_t& ctx, scm_obj_t inst);
    void emit_escape(context_t& ctx, scm_obj_t inst);
    void emit_push_subr_gloc_of(context_t& ctx, scm_obj_t inst);
    void emit_subr_gloc_of(context_t& ctx, scm_obj_t inst);
    void emit_ret_subr_gloc_of(context_t& ctx, scm_obj_t inst);
};

#endif
