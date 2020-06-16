llvm::AllocaInst*
codegen_t::emit_alloca(context_t& ctx, llvm::Type* type)
{
    DECLEAR_CONTEXT_VARS;
    IRBuilder<> TB(&F->getEntryBlock(), F->getEntryBlock().begin());
    return TB.CreateAlloca(type);
}

Function*
codegen_t::emit_inner_function(context_t& ctx, scm_closure_t closure)
{
    VM* vm = m_vm;

    auto search = m_lifted_functions.find(closure);
    if (search != m_lifted_functions.end()) {
#if VERBOSE_CODEGEN
      puts(" + found in m_lifted_functions, return Function*");
#endif
      return search->second;
    }

    if (is_compiled(closure)) {
#if VERBOSE_CODEGEN
        puts(" + emit_inner_function: already compiled, return Function*");
#endif
        return get_function(ctx, closure);
    }
#if VERBOSE_CODEGEN
    puts(" + generating native code for new lifted function");
#endif

    char function_id[40];
    uuid_v4(function_id, sizeof(function_id));

    LLVMContext& C = ctx.m_llvm_context;
    Module* M = ctx.m_module;

    DECLEAR_COMMON_TYPES;
    Function* F = Function::Create(FunctionType::get(IntptrTy, { IntptrPtrTy }, false), Function::PrivateLinkage, function_id, M);
#if USE_LLVM_ATTRIBUTES
    for (Argument& argument : F->args()) { argument.addAttr(Attribute::NoAlias); argument.addAttr(Attribute::NoCapture); }
#endif

    BasicBlock* ENTRY = BasicBlock::Create(C, "entry", F);
    IRBuilder<> IRB(ENTRY);

    m_lifted_functions.insert({closure, F});

    context_t context(C, IRB);
    context.m_module = M;
    context.m_function = F;
    context.m_top_level_closure = closure;
    context.m_top_level_function = F;
    context.set_local_var_count(0, closure);
    context.m_depth = 1;

    transform(context, closure->pc, true);

    return F;
}

void
codegen_t::emit_stack_overflow_check(context_t& ctx, int nbytes)
{

    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    auto vm = F->arg_begin();

    ctx.reg_cache_clear();

    if (nbytes == 0) return;
    if (nbytes >= VM_STACK_BYTESIZE) fatal("%s:%u vm stack size too small", __FILE__, __LINE__);

    auto stack_limit = CREATE_LOAD_VM_REG(vm, m_stack_limit);
    BasicBlock* stack_ok = BasicBlock::Create(C, "stack_ok", F);
    BasicBlock* stack_overflow = BasicBlock::Create(C, "stack_overflow", F);
    Value* stack_cond = IRB.CreateICmpULT(IRB.CreateAdd(CREATE_LOAD_VM_REG(vm, m_sp), VALUE_INTPTR(nbytes)), stack_limit);
    IRB.CreateCondBr(stack_cond, stack_ok, stack_overflow, ctx.likely_true);

    IRB.SetInsertPoint(stack_overflow);
    auto thunkType = FunctionType::get(VoidTy, { IntptrPtrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_collect_stack), thunkType->getPointerTo());
    IRB.CreateCall(thunkType, thunk, { vm, VALUE_INTPTR(nbytes) });

    IRB.CreateBr(stack_ok);

    IRB.SetInsertPoint(stack_ok);
}

Value*
codegen_t::emit_lookup_env(context_t& ctx, intptr_t depth)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    auto vm = F->arg_begin();

    if (depth > 4) {
        ctx.reg_env.writeback(vm);
        auto thunkType = FunctionType::get(IntptrPtrTy, { IntptrPtrTy, IntptrTy }, false);
        auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_lookup_env), thunkType->getPointerTo());
        return IRB.CreateCall(thunkType, thunk, { vm, VALUE_INTPTR(depth) });
    }
    Value* target;
    auto env0 = ctx.reg_env.load(vm);
    if (depth == 0) {
        target = env0;
    } else {
        target = IRB.CreateLoad(IRB.CreateBitOrPointerCast(env0, IntptrPtrTy));
        for (int i = 1; i < depth; i++) {
            target = IRB.CreateLoad(IRB.CreateBitOrPointerCast(target, IntptrPtrTy));
        }
    }
    auto env1 = IRB.CreateGEP(IRB.CreateBitOrPointerCast(target, IntptrPtrTy), VALUE_INTPTR(- offsetof(vm_env_rec_t, up) / sizeof(intptr_t)));
    return IRB.CreateBitOrPointerCast(env1, IntptrPtrTy);
}

Value*
codegen_t::emit_lookup_iloc(context_t& ctx, intptr_t depth, intptr_t index)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    auto vm = F->arg_begin();

    if (depth <= 4) {
#if USE_ILOC_OPTIMIZE
        int n = ctx.get_local_var_count(depth);
        if (n) {
            auto env = emit_lookup_env(ctx, depth);
            return IRB.CreateGEP(env, VALUE_INTPTR(index - n));
        }
        auto env = emit_lookup_env(ctx, depth);
        auto count = CREATE_LOAD_ENV_REC(env, count);
        return IRB.CreateGEP(env, IRB.CreateSub(VALUE_INTPTR(index), count));
#else
        auto env = emit_lookup_env(ctx, depth);
        auto count = CREATE_LOAD_ENV_REC(env, count);
        if (index == 0) return IRB.CreateGEP(env, IRB.CreateNeg(count));
        return IRB.CreateGEP(env, IRB.CreateSub(VALUE_INTPTR(index), count));
#endif
    }
    ctx.reg_env.writeback(vm);
    auto thunkType = FunctionType::get(IntptrPtrTy, { IntptrPtrTy, IntptrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_lookup_iloc), thunkType->getPointerTo());
    return IRB.CreateCall(thunkType, thunk, { vm, VALUE_INTPTR(depth), VALUE_INTPTR(index) });
}

Value*
codegen_t::emit_lookup_iloc(context_t& ctx, scm_obj_t loc)
{
    return emit_lookup_iloc(ctx, FIXNUM(CAR(loc)), FIXNUM(CDR(loc)));
}

void
codegen_t::emit_push_vm_stack(context_t& ctx, Value* val)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    auto vm = F->arg_begin();

    auto sp = ctx.reg_sp.load(vm);
    IRB.CreateStore(val, IRB.CreateBitOrPointerCast(sp, IntptrPtrTy));
    auto ea0 = IRB.CreateGEP(IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(1));
    auto ea1 = IRB.CreateBitOrPointerCast(ea0, IntptrTy);
    ctx.reg_sp.store(vm, ea1);
}

void
codegen_t::emit_prepair_apply(context_t& ctx, scm_closure_t closure)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    auto vm = F->arg_begin();

    intptr_t argc = HDR_CLOSURE_ARGS(closure->hdr);
    auto env = IRB.CreateBitOrPointerCast(ctx.reg_sp.load(vm), IntptrPtrTy);
    CREATE_STORE_ENV_REC(env, count, VALUE_INTPTR(argc));
    CREATE_STORE_ENV_REC(env, up, VALUE_INTPTR(closure->env));
    auto ea0 = IRB.CreateGEP(IRB.CreateBitOrPointerCast(env, IntptrPtrTy), VALUE_INTPTR(sizeof(vm_env_rec_t) / sizeof(intptr_t)));
    auto ea1 = IRB.CreateBitOrPointerCast(ea0, IntptrTy);
    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(closure->pc));
    ctx.reg_env.store(vm, CREATE_LEA_ENV_REC(env, up));
    ctx.reg_fp.store(vm, ea1);
    ctx.reg_sp.store(vm, ea1);
}
