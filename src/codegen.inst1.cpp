
void
codegen_t::emit_push(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    emit_push_vm_stack(ctx, ctx.reg_value.load(vm));
}

void
codegen_t::emit_push_const(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    emit_push_vm_stack(ctx, VALUE_INTPTR(operands));
}

void
codegen_t::emit_push_iloc0(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    emit_push_vm_stack(ctx, IRB.CreateLoad(emit_lookup_iloc(ctx, 0, FIXNUM(operands))));
}

void
codegen_t::emit_push_iloc1(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    emit_push_vm_stack(ctx, IRB.CreateLoad(emit_lookup_iloc(ctx, 1, FIXNUM(operands))));
}

void
codegen_t::emit_push_gloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

#if ENABLE_COMPILE_REFERENCE && ENABLE_COMPILE_DEFERRED
    scm_obj_t obj = ((scm_gloc_t)operands)->value;
    if (CLOSUREP(obj)) {
        scm_closure_t closure = (scm_closure_t)obj;
        if (closure->code == NULL && !HDR_CLOSURE_INSPECTED(closure->hdr)) {
            closure->hdr = closure->hdr | MAKEBITS(1, HDR_CLOSURE_INSPECTED_SHIFT);
            m_compile_queue.push_back((scm_closure_t)obj);
            m_usage.refs++;
        }
    }
#endif

    auto gloc = IRB.CreateBitOrPointerCast(VALUE_INTPTR(operands), IntptrPtrTy);
    auto val = CREATE_LOAD_GLOC_REC(gloc, value);
    if (((scm_gloc_t)operands)->value == scm_undef) {
        BasicBlock* undef_true = BasicBlock::Create(C, "undef_ture", F);
        BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
        auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
        IRB.CreateCondBr(undef_cond, undef_true, CONTINUE, ctx.likely_false);
        IRB.SetInsertPoint(undef_true);
        ctx.reg_cache_copy(vm);
        CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
        auto thunkType = FunctionType::get(VoidTy, { IntptrPtrTy, IntptrTy }, false);
        auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_push_gloc), thunkType->getPointerTo());
        IRB.CreateCall(thunk, { vm, VALUE_INTPTR(operands) });
        IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));
        IRB.SetInsertPoint(CONTINUE);
    }
    emit_push_vm_stack(ctx, val);
}

void
codegen_t::emit_push_car_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto pair = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));

    // check if pair
    BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
    BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
    emit_cond_pairp(ctx, pair, pair_true, pair_false);

    // nonpair
    IRB.SetInsertPoint(pair_false);
    ctx.reg_cache_copy(vm);
    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
    auto thunkType = FunctionType::get(VoidTy, { IntptrPtrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_push_car_iloc), thunkType->getPointerTo());
    IRB.CreateCall(thunk, { vm, pair });
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

    // pair
    IRB.SetInsertPoint(pair_true);
    emit_push_vm_stack(ctx, CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), car));
}

void
codegen_t::emit_push_cdr_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto pair = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));

    // check if pair
    BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
    BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
    emit_cond_pairp(ctx, pair, pair_true, pair_false);

    // nonpair
    IRB.SetInsertPoint(pair_false);
    ctx.reg_cache_copy(vm);
    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
    auto thunkType = FunctionType::get(VoidTy, { IntptrPtrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_push_cdr_iloc), thunkType->getPointerTo());
    IRB.CreateCall(thunk, { vm, pair });
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

    // pair
    IRB.SetInsertPoint(pair_true);
    emit_push_vm_stack(ctx, CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), cdr));
}

void
codegen_t::emit_push_cddr_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto pair = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));

    // check if pair
    BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
    BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
    emit_cond_pairp(ctx, pair, pair_true, pair_false);

    // nonpair
    IRB.SetInsertPoint(pair_false);
    ctx.reg_cache_copy(vm);
    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
    auto thunkType = FunctionType::get(VoidTy, { IntptrPtrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_push_cddr_iloc), thunkType->getPointerTo());
    IRB.CreateCall(thunk, { vm, pair });
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

    // pair
    IRB.SetInsertPoint(pair_true);
    auto pair2 = CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), cdr);
    BasicBlock* pair2_true = BasicBlock::Create(C, "pair2_true", F);
    emit_cond_pairp(ctx, pair2, pair2_true, pair_false);

    // pair + pair
    IRB.SetInsertPoint(pair2_true);
    emit_push_vm_stack(ctx, CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair2, IntptrPtrTy), cdr));
}

void
codegen_t::emit_push_cadr_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto pair = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));

    // check if pair
    BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
    BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
    emit_cond_pairp(ctx, pair, pair_true, pair_false);

    // nonpair
    IRB.SetInsertPoint(pair_false);
    ctx.reg_cache_copy(vm);
    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
    auto thunkType = FunctionType::get(VoidTy, { IntptrPtrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_push_cadr_iloc), thunkType->getPointerTo());
    IRB.CreateCall(thunk, { vm, pair });
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

    // pair
    IRB.SetInsertPoint(pair_true);
    auto pair2 = CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), cdr);
    BasicBlock* pair2_true = BasicBlock::Create(C, "pair2_true", F);
    emit_cond_pairp(ctx, pair2, pair2_true, pair_false);

    // pair - pair
    IRB.SetInsertPoint(pair2_true);
    emit_push_vm_stack(ctx, CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair2, IntptrPtrTy), car));
}

void
codegen_t::emit_push_nadd_iloc(context_t& ctx, scm_obj_t inst)
{
    emit_nadd_iloc(ctx, inst);
    emit_push(ctx, inst);
}

void
codegen_t::emit_apply_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto val = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));
    ctx.reg_value.store(vm, val);
    ctx.reg_cache_copy(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_apply));
}

void
codegen_t::emit_apply_gloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    scm_gloc_t gloc = (scm_gloc_t)CAR(operands);
    scm_obj_t obj = gloc->value;
    if (obj == ctx.m_top_level_closure && HDR_CLOSURE_ARGS(ctx.m_top_level_closure->hdr) == ctx.m_argc) {
#if VERBOSE_CODEGEN
        printf("emit_apply_gloc: self recursive: %s\n", symbol->name);
#endif
        emit_prepair_apply(ctx, ctx.m_top_level_closure);
        ctx.reg_cache_copy_except_value(vm);

        auto call2 = IRB.CreateCall(ctx.m_top_level_function, { vm });
        call2->setTailCallKind(CallInst::TCK_MustTail);
        IRB.CreateRet(call2);
        return;
    }
    if (CLOSUREP(obj) && SYMBOLP(gloc->variable)) {
        scm_symbol_t symbol = (scm_symbol_t)gloc->variable;
        scm_closure_t closure = (scm_closure_t)obj;
        if (strchr(symbol->name, IDENTIFIER_RENAME_DELIMITER)) {
#if VERBOSE_CODEGEN
            printf("emit_apply_gloc: uninterned gloc: %s\n", symbol->name);
#endif
            if (closure->env == NULL) {
                Function* F2 = emit_inner_function(ctx, closure);
                if (F2 == NULL) fatal("%s:%u inconsistent state", __FILE__, __LINE__);
                m_usage.inners++;
                emit_prepair_apply(ctx, closure);
                ctx.reg_cache_copy_except_value(vm);
                auto call2 = IRB.CreateCall(F2, { vm });
                call2->setTailCallKind(CallInst::TCK_MustTail);
                IRB.CreateRet(call2);
                return;
            } else {
                if (m_debug) {
                    printf("hazard: emit_apply_gloc: closure have non NULL environment\n");
                }
#if VERBOSE_CODEGEN
                printf("emit_apply_gloc: out of top level context, closure->env != NULL: %s\n", symbol->name);
#endif
            }
        } else if (strchr(symbol->name, IDENTIFIER_LIBRARY_SUFFIX)) {
#if VERBOSE_CODEGEN
            printf("emit_apply_gloc: library top level: %s\n", symbol->name);
#endif
            if (HDR_CLOSURE_ARGS(closure->hdr) == ctx.m_argc) {
                if (closure->code) {
                    auto procType = FunctionType::get(IntptrTy, { IntptrPtrTy }, false);
                    auto ptr = ConstantExpr::getIntToPtr(VALUE_INTPTR(closure->code), procType->getPointerTo());
                    emit_prepair_apply(ctx, closure);
                    ctx.reg_cache_copy_except_value(vm);
                    auto call = IRB.CreateCall(ptr, { vm });
                    call->setTailCallKind(CallInst::TCK_MustTail);
                    IRB.CreateRet(call);
                    return;
                } else {
#if VERBOSE_CODEGEN
                    printf("emit_apply_gloc: library top level not compiled: %s\n", symbol->name);
#endif
                }
            } else {
#if VERBOSE_CODEGEN
                printf("emit_apply_gloc: library top level ctx.m_argc does not match: %s\n", symbol->name);
#endif
            }
        }
    }

#if ENABLE_COMPILE_REFERENCE && ENABLE_COMPILE_DEFERRED
    if (CLOSUREP(obj)) {
        scm_closure_t closure = (scm_closure_t)obj;
        if (closure->code == NULL && !HDR_CLOSURE_INSPECTED(closure->hdr)) {
            closure->hdr = closure->hdr | MAKEBITS(1, HDR_CLOSURE_INSPECTED_SHIFT);
            m_compile_queue.push_back((scm_closure_t)obj);
            m_usage.refs++;
        }
    }
#endif

    auto val = CREATE_LOAD_GLOC_REC(IRB.CreateBitOrPointerCast(VALUE_INTPTR(gloc), IntptrPtrTy), value);
    if (gloc->value == scm_undef) {
        BasicBlock* undef_true = BasicBlock::Create(C, "undef_ture", F);
        BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
        auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
        IRB.CreateCondBr(undef_cond, undef_true, CONTINUE, ctx.likely_false);
        IRB.SetInsertPoint(undef_true);
        ctx.reg_cache_copy(vm);
        CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
        auto thunkType = FunctionType::get(VoidTy, { IntptrPtrTy, IntptrTy }, false);
        auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_apply_gloc), thunkType->getPointerTo());
        IRB.CreateCall(thunk, { vm, VALUE_INTPTR(gloc) });
        IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));
        IRB.SetInsertPoint(CONTINUE);
    }
    ctx.reg_value.store(vm, val);
    ctx.reg_cache_copy(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_apply));
}

void
codegen_t::emit_ret_const(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    ctx.reg_value.store(vm, VALUE_INTPTR(operands));
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void
codegen_t::emit_ret_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto val = IRB.CreateLoad(emit_lookup_iloc(ctx, operands));
    ctx.reg_value.store(vm, val);
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void
codegen_t::emit_ret_cons(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto sp = ctx.reg_sp.load(vm);
    auto val = ctx.reg_value.load(vm);

    auto sp_minus_1 = IRB.CreateLoad(IRB.CreateGEP(IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-1)));
    auto thunkType = FunctionType::get(IntptrTy, { IntptrPtrTy, IntptrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_make_pair), thunkType->getPointerTo());
    ctx.reg_value.store(vm, IRB.CreateCall(thunk, { vm, sp_minus_1, val }));
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void
codegen_t::emit_if_true(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto value = ctx.reg_value.load(vm);

    BasicBlock* f9h_true = BasicBlock::Create(C, "f9h_true", F);
    BasicBlock* f9h_false = BasicBlock::Create(C, "f9h_false", F);
    auto f9h_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_false));
    IRB.CreateCondBr(f9h_cond, f9h_true, f9h_false, ctx.likely_false);

    // taken
    IRB.SetInsertPoint(f9h_false);
    transform(ctx, operands, false);

    // not taken
    IRB.SetInsertPoint(f9h_true);
}

void
codegen_t::emit_if_nullp(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto value = ctx.reg_value.load(vm);

    BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
    BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
    auto taken_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_nil));
    IRB.CreateCondBr(taken_cond, taken_true, taken_false, ctx.likely_false);

    // taken
    IRB.SetInsertPoint(taken_true);
    transform(ctx, operands, false);

    // not taken
    IRB.SetInsertPoint(taken_false);
}

void
codegen_t::emit_if_eqp(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto sp = ctx.reg_sp.load(vm);
    auto ea = IRB.CreateGEP(IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-1));
    ctx.reg_sp.store(vm, IRB.CreateBitOrPointerCast(ea, IntptrTy));
    auto val1 = IRB.CreateLoad(ea);
    auto val2 = ctx.reg_value.load(vm);

    BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
    BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
    auto taken_cond = IRB.CreateICmpEQ(val1, val2);
    IRB.CreateCondBr(taken_cond, taken_true, taken_false, ctx.likely_false);

    // taken
    IRB.SetInsertPoint(taken_true);
    transform(ctx, operands, false);

    // not taken
    IRB.SetInsertPoint(taken_false);
}

void
codegen_t::emit_if_nullp_ret_const(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto value = ctx.reg_value.load(vm);

    BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
    BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
    auto taken_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_nil));
    IRB.CreateCondBr(taken_cond, taken_true, taken_false, ctx.likely_false);

    // taken
    IRB.SetInsertPoint(taken_true);
    ctx.reg_value.store(vm, VALUE_INTPTR(operands));
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

    // not taken
    IRB.SetInsertPoint(taken_false);
}

void
codegen_t::emit_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto val = IRB.CreateLoad(emit_lookup_iloc(ctx, operands));
    ctx.reg_value.store(vm, val);
}

void
codegen_t::emit_iloc0(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto val = IRB.CreateLoad(emit_lookup_iloc(ctx, 0, FIXNUM(operands)));
    ctx.reg_value.store(vm, val);
}

void
codegen_t::emit_iloc1(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto val = IRB.CreateLoad(emit_lookup_iloc(ctx, 1, FIXNUM(operands)));
    ctx.reg_value.store(vm, val);
}

void
codegen_t::emit_push_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    emit_push_vm_stack(ctx, IRB.CreateLoad(emit_lookup_iloc(ctx, operands)));
}

void
codegen_t::emit_if_true_ret(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto value = ctx.reg_value.load(vm);

    BasicBlock* value_false = BasicBlock::Create(C, "value_false", F);
    BasicBlock* value_nonfalse = BasicBlock::Create(C, "value_nonfalse", F);
    auto value_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_false));
    IRB.CreateCondBr(value_cond, value_false, value_nonfalse, ctx.likely_false);

    // pop
    IRB.SetInsertPoint(value_nonfalse);
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

    // continue
    IRB.SetInsertPoint(value_false);
}

void
codegen_t::emit_if_false_ret(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto value = ctx.reg_value.load(vm);

    BasicBlock* value_false = BasicBlock::Create(C, "value_false", F);
    BasicBlock* value_nonfalse = BasicBlock::Create(C, "value_nonfalse", F);
    auto value_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_false));
    IRB.CreateCondBr(value_cond, value_false, value_nonfalse, ctx.likely_false);

    // pop
    IRB.SetInsertPoint(value_false);
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

    // continue
    IRB.SetInsertPoint(value_nonfalse);
}

void
codegen_t::emit_if_true_ret_const(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto value = ctx.reg_value.load(vm);

    BasicBlock* value_false = BasicBlock::Create(C, "value_false", F);
    BasicBlock* value_nonfalse = BasicBlock::Create(C, "value_nonfalse", F);
    auto value_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_false));
    IRB.CreateCondBr(value_cond, value_false, value_nonfalse, ctx.likely_false);

    // pop
    IRB.SetInsertPoint(value_nonfalse);
    ctx.reg_value.store(vm, VALUE_INTPTR(operands));
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

    // continue
    IRB.SetInsertPoint(value_false);
}

Value*
codegen_t::emit_cmp_inst(context_t& ctx, cc_t cc, Value* lhs, Value* rhs)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    switch (cc) {
      case LT: return IRB.CreateICmpSLT(lhs, rhs);
      case GT: return IRB.CreateICmpSGT(lhs, rhs);
      case LE: return IRB.CreateICmpSLE(lhs, rhs);
      case GE: return IRB.CreateICmpSGE(lhs, rhs);
      case EQ: return IRB.CreateICmpEQ(lhs, rhs);
    }
}

void
codegen_t::emit_cc_n_iloc(context_t& ctx, scm_obj_t inst, cc_t cc, void* c_func)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto lhs = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));
    auto rhs = VALUE_INTPTR(CADR(operands));

    auto retval = emit_alloca(ctx, IntptrTy);

    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
    BasicBlock* nonfixnum_true = BasicBlock::Create(C, "nonfixnum_true", F);
    BasicBlock* nonfixnum_false = BasicBlock::Create(C, "nonfixnum_false", F);
    auto nonfixnum_cond = IRB.CreateICmpEQ(IRB.CreateAnd(lhs, 1), VALUE_INTPTR(0));
    IRB.CreateCondBr(nonfixnum_cond, nonfixnum_true, nonfixnum_false, ctx.likely_false);

    // fixnum
    IRB.SetInsertPoint(nonfixnum_false);
    BasicBlock* cond_true = BasicBlock::Create(C, "cond_true", F);
    BasicBlock* cond_false = BasicBlock::Create(C, "cond_false", F);
    auto cond = emit_cmp_inst(ctx, cc, lhs, rhs);
    IRB.CreateCondBr(cond, cond_true, cond_false, ctx.likely_false);

    // taken
    IRB.SetInsertPoint(cond_true);
    IRB.CreateStore(VALUE_INTPTR(scm_true), retval);
    IRB.CreateBr(CONTINUE);

    // not taken
    IRB.SetInsertPoint(cond_false);
    IRB.CreateStore(VALUE_INTPTR(scm_false), retval);
    IRB.CreateBr(CONTINUE);

    // others
    IRB.SetInsertPoint(nonfixnum_true);
    ctx.reg_cache_copy_except_value(vm);
    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
    auto thunkType = FunctionType::get(IntptrTy, { IntptrPtrTy, IntptrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_func), thunkType->getPointerTo());
    auto ans = IRB.CreateCall(thunk, { vm, lhs, rhs });

    BasicBlock* fallback_success = BasicBlock::Create(C, "fallback_success", F);
    BasicBlock* fallback_fail = BasicBlock::Create(C, "fallback_fail", F);
    auto fallback_cond = IRB.CreateICmpNE(ans, VALUE_INTPTR(0));
    IRB.CreateCondBr(fallback_cond, fallback_success, fallback_fail, ctx.likely_true);

    IRB.SetInsertPoint(fallback_fail);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

    IRB.SetInsertPoint(fallback_success);
    IRB.CreateStore(ans, retval);
    IRB.CreateBr(CONTINUE);

    IRB.SetInsertPoint(CONTINUE);
    ctx.reg_value.store(vm, IRB.CreateLoad(retval));
}

void
codegen_t::emit_lt_n_iloc(context_t& ctx, scm_obj_t inst)
{
  emit_cc_n_iloc(ctx, inst, LT, (void*)c_lt_n_iloc);
}

void
codegen_t::emit_gt_n_iloc(context_t& ctx, scm_obj_t inst)
{
  emit_cc_n_iloc(ctx, inst, GT, (void*)c_gt_n_iloc);
}

void
codegen_t::emit_ge_n_iloc(context_t& ctx, scm_obj_t inst)
{
  emit_cc_n_iloc(ctx, inst, GE, (void*)c_ge_n_iloc);
}

void
codegen_t::emit_le_n_iloc(context_t& ctx, scm_obj_t inst)
{
  emit_cc_n_iloc(ctx, inst, LE, (void*)c_le_n_iloc);
}

void
codegen_t::emit_eq_n_iloc(context_t& ctx, scm_obj_t inst)
{
  emit_cc_n_iloc(ctx, inst, EQ, (void*)c_eq_n_iloc);
}


void
codegen_t::emit_cc_iloc(context_t& ctx, scm_obj_t inst, cc_t cc, void* c_func)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto lhs = ctx.reg_value.load(vm);
    auto rhs = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));

    auto retval = emit_alloca(ctx, IntptrTy);

    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
    BasicBlock* nonfixnum_true = BasicBlock::Create(C, "nonfixnum_true", F);
    BasicBlock* nonfixnum_false = BasicBlock::Create(C, "nonfixnum_false", F);
    auto nonfixnum_cond = IRB.CreateICmpEQ(IRB.CreateAnd(lhs, IRB.CreateAnd(rhs, 1)), VALUE_INTPTR(0));
    IRB.CreateCondBr(nonfixnum_cond, nonfixnum_true, nonfixnum_false, ctx.likely_false);

    // fixnum
    IRB.SetInsertPoint(nonfixnum_false);
    BasicBlock* cond_true = BasicBlock::Create(C, "cond_true", F);
    BasicBlock* cond_false = BasicBlock::Create(C, "cond_false", F);
    auto cond = emit_cmp_inst(ctx, cc, lhs, rhs);
    IRB.CreateCondBr(cond, cond_true, cond_false, ctx.likely_false);

    // taken
    IRB.SetInsertPoint(cond_true);
    IRB.CreateStore(VALUE_INTPTR(scm_true), retval);
    IRB.CreateBr(CONTINUE);

    // not taken
    IRB.SetInsertPoint(cond_false);
    IRB.CreateStore(VALUE_INTPTR(scm_false), retval);
    IRB.CreateBr(CONTINUE);

    // others
    IRB.SetInsertPoint(nonfixnum_true);
    ctx.reg_cache_copy_except_value(vm);
    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
    auto thunkType = FunctionType::get(IntptrTy, { IntptrPtrTy, IntptrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_func), thunkType->getPointerTo());
    auto ans = IRB.CreateCall(thunk, { vm, lhs, rhs });

    BasicBlock* fallback_success = BasicBlock::Create(C, "fallback_success", F);
    BasicBlock* fallback_fail = BasicBlock::Create(C, "fallback_fail", F);
    auto fallback_cond = IRB.CreateICmpNE(ans, VALUE_INTPTR(0));
    IRB.CreateCondBr(fallback_cond, fallback_success, fallback_fail, ctx.likely_true);

    IRB.SetInsertPoint(fallback_fail);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

    IRB.SetInsertPoint(fallback_success);
    IRB.CreateStore(ans, retval);
    IRB.CreateBr(CONTINUE);

    IRB.SetInsertPoint(CONTINUE);
    ctx.reg_value.store(vm, IRB.CreateLoad(retval));
}

void
codegen_t::emit_gt_iloc(context_t& ctx, scm_obj_t inst)
{
  emit_cc_iloc(ctx, inst, GT, (void*)c_gt_iloc);
}

void
codegen_t::emit_lt_iloc(context_t& ctx, scm_obj_t inst)
{
  emit_cc_iloc(ctx, inst, LT, (void*)c_lt_iloc);
}

void
codegen_t::emit_ge_iloc(context_t& ctx, scm_obj_t inst)
{
  emit_cc_iloc(ctx, inst, GE, (void*)c_ge_iloc);
}

void
codegen_t::emit_le_iloc(context_t& ctx, scm_obj_t inst)
{
  emit_cc_iloc(ctx, inst, LE, (void*)c_le_iloc);
}

void
codegen_t::emit_eq_iloc(context_t& ctx, scm_obj_t inst)
{
  emit_cc_iloc(ctx, inst, EQ, (void*)c_eq_iloc);
}

Function*
codegen_t::emit_call(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    char cont_id[40];
    uuid_v4(cont_id, sizeof(cont_id));

    Function* K = Function::Create(FunctionType::get(IntptrTy, { IntptrPtrTy }, false), Function::PrivateLinkage, cont_id, M);
#if USE_LLVM_ATTRIBUTES
    for (Argument& argument : F->args()) { argument.addAttr(Attribute::NoAlias); argument.addAttr(Attribute::NoCapture); }
#endif

    BasicBlock* RETURN = BasicBlock::Create(C, "entry", K);

    // vm_cont_t cont = (vm_cont_t)m_sp;
    auto cont = IRB.CreateBitOrPointerCast(ctx.reg_sp.load(vm), IntptrPtrTy);
    // cont->pc
    CREATE_STORE_CONT_REC(cont, pc, VALUE_INTPTR(CDR(inst)));
    // cont->trace
    CREATE_STORE_CONT_REC(cont, trace, VALUE_INTPTR(scm_unspecified));
    // cont->fp
    CREATE_STORE_CONT_REC(cont, fp, ctx.reg_fp.load(vm));
    // cont->env
    CREATE_STORE_CONT_REC(cont, env, ctx.reg_env.load(vm));
    // cont->code
    CREATE_STORE_CONT_REC(cont, code, IRB.CreateBitOrPointerCast(K, IntptrTy));
    // cont->up
    CREATE_STORE_CONT_REC(cont, up, ctx.reg_cont.load(vm));
    // m_sp
    auto ea1 = IRB.CreateBitOrPointerCast(IRB.CreateGEP(cont, VALUE_INTPTR(sizeof(vm_cont_rec_t) / sizeof(intptr_t))), IntptrTy);
    ctx.reg_sp.store(vm, ea1);
    // m_fp
    ctx.reg_fp.store(vm, ea1);
    // m_cont
    auto ea2 = IRB.CreateBitOrPointerCast(IRB.CreateGEP(cont, VALUE_INTPTR(offsetof(vm_cont_rec_t, up) / sizeof(intptr_t))), IntptrTy);
    ctx.reg_cont.store(vm, ea2);

    context_t ctx2 = ctx;
    ctx2.m_argc = 0;
    transform(ctx2, operands, false);

    IRB.SetInsertPoint(RETURN);
    ctx.reg_cache_clear();
    return K;
}

void
codegen_t::emit_if_false_call(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto value = ctx.reg_value.load(vm);

    BasicBlock* value_false = BasicBlock::Create(C, "value_false", F);
    BasicBlock* value_nonfalse = BasicBlock::Create(C, "value_nonfalse", F);
    auto value_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_false));
    IRB.CreateCondBr(value_cond, value_false, value_nonfalse, ctx.likely_false);

    // taken
    IRB.SetInsertPoint(value_false);
    context_t ctx2 = ctx;
    ctx2.m_argc = 0;
    transform(ctx2, operands, false);

    // no taken
    IRB.SetInsertPoint(value_nonfalse);
}

void
codegen_t::emit_extend(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto argc = VALUE_INTPTR(FIXNUM(operands));
    auto env = IRB.CreateBitOrPointerCast(ctx.reg_sp.load(vm), IntptrPtrTy);
    CREATE_STORE_ENV_REC(env, count, argc);
    CREATE_STORE_ENV_REC(env, up, ctx.reg_env.load(vm));
    auto ea0 = IRB.CreateGEP(IRB.CreateBitOrPointerCast(env, IntptrPtrTy), VALUE_INTPTR(sizeof(vm_env_rec_t) / sizeof(intptr_t)));
    auto ea1 = IRB.CreateBitOrPointerCast(ea0, IntptrTy);
    ctx.reg_sp.store(vm, ea1);
    ctx.reg_fp.store(vm, ea1);
    ctx.reg_env.store(vm, CREATE_LEA_ENV_REC(env, up));
    ctx.set_local_var_count(ctx.m_depth, FIXNUM(operands));
}

void
codegen_t::emit_extend_enclose_local(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    emit_push_vm_stack(ctx, VALUE_INTPTR(CDR(operands)));
    auto env = IRB.CreateBitOrPointerCast(ctx.reg_sp.load(vm), IntptrPtrTy);
    CREATE_STORE_ENV_REC(env, count, VALUE_INTPTR(1));
    CREATE_STORE_ENV_REC(env, up, ctx.reg_env.load(vm));
    auto ea0 = IRB.CreateGEP(IRB.CreateBitOrPointerCast(env, IntptrPtrTy), VALUE_INTPTR(sizeof(vm_env_rec_t) / sizeof(intptr_t)));
    auto ea1 = IRB.CreateBitOrPointerCast(ea0, IntptrTy);
    ctx.reg_sp.store(vm, ea1);
    ctx.reg_fp.store(vm, ea1);
    ctx.reg_env.store(vm, CREATE_LEA_ENV_REC(env, up));

    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
    IRB.CreateBr(CONTINUE);

    // continue emit code in operands
    char local_id[40];
    uuid_v4(local_id, sizeof(local_id));
    Function* L = Function::Create(FunctionType::get(IntptrTy, { IntptrPtrTy }, false), Function::PrivateLinkage, local_id, M);
#if USE_LLVM_ATTRIBUTES
    for (Argument& argument : F->args()) { argument.addAttr(Attribute::NoAlias); argument.addAttr(Attribute::NoCapture); }
#endif

    BasicBlock* LOOP = BasicBlock::Create(C, "entry", L);

    int function_index = ctx.m_depth + (0 << 16);
    ctx.m_local_functions[function_index] = L;
    ctx.set_local_var_count(ctx.m_depth, 1);
    m_usage.locals++;

    context_t ctx2 = ctx;
    ctx2.m_function = L;
    int nargs = FIXNUM(CAR(CAR(operands))) + FIXNUM(CADR(CAR(operands)));
    ctx2.set_local_var_count(ctx2.m_depth, 1);
    ctx2.set_local_var_count(ctx2.m_depth + 1, nargs);
    ctx2.m_depth += 2;
    ctx2.m_argc = 0;
    ctx2.reg_cache_clear();

    IRB.SetInsertPoint(LOOP);
    transform(ctx2, CDR(operands), true);

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
    int index = FIXNUM(CDAR(operands));
    int function_index = ctx.m_depth - level - 1 + (index << 16);

    if (ctx.m_depth - level - 1 < 0 || ctx.m_local_functions[function_index] == NULL) {
#if VERBOSE_CODEGEN
        printf("emit_apply_iloc_local: out of local context, level = %d index = %d ctx.m_depth = %d ctx.m_depth - level - 1 = %x \n", level, index, ctx.m_depth, ctx.m_depth - level - 1);
#endif
        if (m_debug) {
            if (ctx.m_depth - level - 1 < 0) {
                printf("hazard: emit_apply_iloc_local: referencing free variable (%d - %d)\n", ctx.m_depth, level);
            } else if (ctx.m_local_functions[function_index] == NULL) {
                printf("hazard: emit_apply_iloc_local: ctx.m_local_functions[%d] == NULL\n", function_index);
            }
        }
        CREATE_STORE_VM_REG(vm, m_pc, IRB.CreateLoad(emit_lookup_iloc(ctx, level, index)));
        auto env2 = emit_lookup_env(ctx, level);
        auto count = CREATE_LOAD_ENV_REC(env2, count);
        auto obj = IRB.CreateLoad(index == 0 ? IRB.CreateGEP(env2, IRB.CreateNeg(count)) : IRB.CreateGEP(env2, IRB.CreateSub(VALUE_INTPTR(index), count)));
        auto env = IRB.CreateBitOrPointerCast(ctx.reg_sp.load(vm), IntptrPtrTy);
        CREATE_STORE_ENV_REC(env, count, VALUE_INTPTR(ctx.m_argc));
        CREATE_STORE_ENV_REC(env, up, CREATE_LEA_ENV_REC(env2, up));
        auto ea0 = IRB.CreateGEP(IRB.CreateBitOrPointerCast(env, IntptrPtrTy), VALUE_INTPTR(sizeof(vm_env_rec_t) / sizeof(intptr_t)));
        auto ea1 = IRB.CreateBitOrPointerCast(ea0, IntptrTy);
        ctx.reg_sp.store(vm, ea1);
        ctx.reg_fp.store(vm, ea1);
        ctx.reg_env.store(vm, CREATE_LEA_ENV_REC(env, up));
        ctx.reg_cache_copy(vm);
        IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_loop));
    } else {
        auto env2 = emit_lookup_env(ctx, level);
        auto count = CREATE_LOAD_ENV_REC(env2, count);
        auto obj = IRB.CreateLoad(index == 0 ? IRB.CreateGEP(env2, IRB.CreateNeg(count)) : IRB.CreateGEP(env2, IRB.CreateSub(VALUE_INTPTR(index), count)));
        auto env = IRB.CreateBitOrPointerCast(ctx.reg_sp.load(vm), IntptrPtrTy);
        CREATE_STORE_ENV_REC(env, count, VALUE_INTPTR(ctx.m_argc));
        CREATE_STORE_ENV_REC(env, up, CREATE_LEA_ENV_REC(env2, up));
        auto ea0 = IRB.CreateGEP(IRB.CreateBitOrPointerCast(env, IntptrPtrTy), VALUE_INTPTR(sizeof(vm_env_rec_t) / sizeof(intptr_t)));
        auto ea1 = IRB.CreateBitOrPointerCast(ea0, IntptrTy);
        ctx.reg_sp.store(vm, ea1);
        ctx.reg_fp.store(vm, ea1);
        ctx.reg_env.store(vm, CREATE_LEA_ENV_REC(env, up));
        ctx.reg_cache_copy_except_value(vm);
        Function* L = ctx.m_local_functions[function_index];
        if (L == NULL) {
            fatal("%s:%u emit_apply_iloc_local L = %p, level = %d index = %d ctx.m_depth = %d function_index = %x \n", __FILE__, __LINE__, L, level, index, ctx.m_depth, function_index);
        }
        auto call = IRB.CreateCall(L, { vm });
        call->setTailCallKind(CallInst::TCK_MustTail);
        IRB.CreateRet(call);
    }
}

void
codegen_t::emit_push_cons(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto sp = ctx.reg_sp.load(vm);
    auto val = ctx.reg_value.load(vm);

    auto ea = IRB.CreateGEP(IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-1));
    auto sp_minus_1 = IRB.CreateLoad(ea);
    auto thunkType = FunctionType::get(IntptrTy, { IntptrPtrTy, IntptrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_make_pair), thunkType->getPointerTo());
    IRB.CreateStore(IRB.CreateCall(thunk, { vm, sp_minus_1, val }), ea);
}

void
codegen_t::emit_car_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto pair = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));

    // check if pair
    BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
    BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
    emit_cond_pairp(ctx, pair, pair_true, pair_false);

    // nonpair
    IRB.SetInsertPoint(pair_false);
    ctx.reg_cache_copy(vm);
    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
    auto thunkType = FunctionType::get(VoidTy, { IntptrPtrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_car_iloc), thunkType->getPointerTo());
    IRB.CreateCall(thunk, { vm, pair });
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

    // pair
    IRB.SetInsertPoint(pair_true);
    ctx.reg_value.store(vm, CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), car));
}

void
codegen_t::emit_cdr_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto pair = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));

    // check if pair
    BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
    BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
    emit_cond_pairp(ctx, pair, pair_true, pair_false);

    // nonpair
    IRB.SetInsertPoint(pair_false);
    ctx.reg_cache_copy(vm);
    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
    auto thunkType = FunctionType::get(VoidTy, { IntptrPtrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_cdr_iloc), thunkType->getPointerTo());
    IRB.CreateCall(thunk, { vm, pair });
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

    // pair
    IRB.SetInsertPoint(pair_true);
    ctx.reg_value.store(vm, CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), cdr));
}

void
codegen_t::emit_set_gloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto thunkType = FunctionType::get(VoidTy, { IntptrPtrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_set_gloc), thunkType->getPointerTo());
    IRB.CreateCall(thunk, { vm, VALUE_INTPTR(operands) });
}

void
codegen_t::emit_const(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    ctx.reg_value.store(vm, VALUE_INTPTR(operands));
}

void
codegen_t::emit_if_pairp(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
    BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);

    emit_cond_pairp(ctx, ctx.reg_value.load(vm), taken_true, taken_false);

    // taken
    IRB.SetInsertPoint(taken_true);
    transform(ctx, operands, false);

    // not taken
    IRB.SetInsertPoint(taken_false);
}

void
codegen_t::emit_if_eqp_ret_const(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto sp = ctx.reg_sp.load(vm);
    auto ea = IRB.CreateGEP(IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-1));
    ctx.reg_sp.store(vm, IRB.CreateBitOrPointerCast(ea, IntptrTy));


    auto val1 = IRB.CreateLoad(ea);
    auto val2 = ctx.reg_value.load(vm);

    BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
    BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
    auto taken_cond = IRB.CreateICmpEQ(val1, val2);
    IRB.CreateCondBr(taken_cond, taken_true, taken_false, ctx.likely_false);

    // taken
    IRB.SetInsertPoint(taken_true);
    ctx.reg_value.store(vm, VALUE_INTPTR(operands));
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

    // not taken
    IRB.SetInsertPoint(taken_false);
}

void
codegen_t::emit_cadr_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto pair = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));

    // check if pair
    BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
    BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
    emit_cond_pairp(ctx, pair, pair_true, pair_false);

    // nonpair
    IRB.SetInsertPoint(pair_false);
    ctx.reg_cache_copy(vm);
    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
    auto thunkType = FunctionType::get(VoidTy, { IntptrPtrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_cadr_iloc), thunkType->getPointerTo());
    IRB.CreateCall(thunk, { vm, pair });
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

    // pair
    IRB.SetInsertPoint(pair_true);
    auto pair2 = CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), cdr);
    BasicBlock* pair2_true = BasicBlock::Create(C, "pair2_true", F);
    emit_cond_pairp(ctx, pair2, pair2_true, pair_false);

    // pair + pair
    IRB.SetInsertPoint(pair2_true);
    ctx.reg_value.store(vm, CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair2, IntptrPtrTy), car));
}

void
codegen_t::emit_cddr_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto pair = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));

    // check if pair
    BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
    BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
    emit_cond_pairp(ctx, pair, pair_true, pair_false);

    // nonpair
    IRB.SetInsertPoint(pair_false);
    ctx.reg_cache_copy(vm);
    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
    auto thunkType = FunctionType::get(VoidTy, { IntptrPtrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_cadr_iloc), thunkType->getPointerTo());
    IRB.CreateCall(thunk, { vm, pair });
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

    // pair
    IRB.SetInsertPoint(pair_true);
    auto pair2 = CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), cdr);
    BasicBlock* pair2_true = BasicBlock::Create(C, "pair2_true", F);
    emit_cond_pairp(ctx, pair2, pair2_true, pair_false);

    // pair - pair
    IRB.SetInsertPoint(pair2_true);
    ctx.reg_value.store(vm, CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair2, IntptrPtrTy), cdr));
}

void
codegen_t::emit_if_not_eqp_ret_const(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto sp = ctx.reg_sp.load(vm);

    auto ea = IRB.CreateGEP(IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-1));
    ctx.reg_sp.store(vm, IRB.CreateBitOrPointerCast(ea, IntptrTy));

    auto val1 = IRB.CreateLoad(ea);
    auto val2 = ctx.reg_value.load(vm);

    BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
    BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
    auto taken_cond = IRB.CreateICmpNE(val1, val2);
    IRB.CreateCondBr(taken_cond, taken_true, taken_false, ctx.likely_false);

    // taken
    IRB.SetInsertPoint(taken_true);
    ctx.reg_value.store(vm, VALUE_INTPTR(operands));
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

    // not taken
    IRB.SetInsertPoint(taken_false);
}

void
codegen_t::emit_if_not_pairp_ret_const(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto value = ctx.reg_value.load(vm);

    BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
    BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
    emit_cond_pairp(ctx, value, pair_true, pair_false);

    // not pair
    IRB.SetInsertPoint(pair_false);
    ctx.reg_value.store(vm, VALUE_INTPTR(operands));
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

    // pair
    IRB.SetInsertPoint(pair_true);
}

void
codegen_t::emit_if_false_ret_const(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto value = ctx.reg_value.load(vm);

    BasicBlock* value_false = BasicBlock::Create(C, "value_false", F);
    BasicBlock* value_nonfalse = BasicBlock::Create(C, "value_nonfalse", F);
    auto value_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_false));
    IRB.CreateCondBr(value_cond, value_false, value_nonfalse, ctx.likely_false);

    // pop
    IRB.SetInsertPoint(value_false);
    ctx.reg_value.store(vm, VALUE_INTPTR(operands));
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

    // continue
    IRB.SetInsertPoint(value_nonfalse);
}

void
codegen_t::emit_ret_nullp(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto value = ctx.reg_value.load(vm);

    BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
    BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
    auto taken_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_nil));
    IRB.CreateCondBr(taken_cond, taken_true, taken_false, ctx.likely_false);

    // taken
    IRB.SetInsertPoint(taken_true);
    ctx.reg_value.store(vm, VALUE_INTPTR(scm_true));
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

    // not taken
    IRB.SetInsertPoint(taken_false);
    ctx.reg_value.store(vm, VALUE_INTPTR(scm_false));
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void
codegen_t::emit_ret_pairp(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto value = ctx.reg_value.load(vm);

    BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
    BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
    emit_cond_pairp(ctx, value, taken_true, taken_false);

    // taken
    IRB.SetInsertPoint(taken_true);
    ctx.reg_value.store(vm, VALUE_INTPTR(scm_true));
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

    // not taken
    IRB.SetInsertPoint(taken_false);
    ctx.reg_value.store(vm, VALUE_INTPTR(scm_false));
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void
codegen_t::emit_ret_gloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto gloc = IRB.CreateBitOrPointerCast(VALUE_INTPTR(operands), IntptrPtrTy);
    ctx.reg_value.store(vm, CREATE_LOAD_GLOC_REC(gloc, value));
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void
codegen_t::emit_ret_eqp(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto sp = ctx.reg_sp.load(vm);
    auto ea = IRB.CreateGEP(IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-1));
    ctx.reg_sp.store(vm, IRB.CreateBitOrPointerCast(ea, IntptrTy));

    auto val1 = IRB.CreateLoad(ea);
    auto val2 = ctx.reg_value.load(vm);

    BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
    BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
    auto taken_cond = IRB.CreateICmpEQ(val1, val2);
    IRB.CreateCondBr(taken_cond, taken_true, taken_false, ctx.likely_false);

    // taken
    IRB.SetInsertPoint(taken_true);
    ctx.reg_value.store(vm, VALUE_INTPTR(scm_true));
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

    // not taken
    IRB.SetInsertPoint(taken_false);
    ctx.reg_value.store(vm, VALUE_INTPTR(scm_false));
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void
codegen_t::emit_set_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto thunkType = FunctionType::get(VoidTy, { IntptrPtrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_set_iloc), thunkType->getPointerTo());
    IRB.CreateCall(thunk, { vm, VALUE_INTPTR(operands) });
}

void
codegen_t::emit_extend_unbound(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    int argc = FIXNUM(operands);
    for (intptr_t i = 0; i < argc; i++) {
        emit_push_vm_stack(ctx, VALUE_INTPTR(scm_undef));
    }
    auto env = IRB.CreateBitOrPointerCast(ctx.reg_sp.load(vm), IntptrPtrTy);
    CREATE_STORE_ENV_REC(env, count, VALUE_INTPTR(argc));
    CREATE_STORE_ENV_REC(env, up, ctx.reg_env.load(vm));
    auto ea0 = IRB.CreateGEP(IRB.CreateBitOrPointerCast(env, IntptrPtrTy), VALUE_INTPTR(sizeof(vm_env_rec_t) / sizeof(intptr_t)));
    auto ea1 = IRB.CreateBitOrPointerCast(ea0, IntptrTy);
    ctx.reg_sp.store(vm, ea1);
    ctx.reg_fp.store(vm, ea1);
    ctx.reg_env.store(vm, CREATE_LEA_ENV_REC(env, up));

    ctx.set_local_var_count(ctx.m_depth, argc);
}

void
codegen_t::emit_enclose(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    int argc = FIXNUM(operands);
    auto thunkType = FunctionType::get(VoidTy, { IntptrPtrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_enclose), thunkType->getPointerTo());
    IRB.CreateCall(thunk, { vm, VALUE_INTPTR(argc) });
}

void
codegen_t::emit_push_close(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

#if ENABLE_COMPILE_DEFERRED
    m_compile_queue.push_back((scm_closure_t)operands);
    m_usage.templates++;
#endif

    auto thunkType = FunctionType::get(VoidTy, { IntptrPtrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_push_close), thunkType->getPointerTo());
    IRB.CreateCall(thunk, { vm, VALUE_INTPTR(operands) });
}

void
codegen_t::emit_ret_close(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

#if ENABLE_COMPILE_DEFERRED
    m_compile_queue.push_back((scm_closure_t)operands);
    m_usage.templates++;
#endif

    ctx.reg_cache_copy(vm);
    auto thunkType = FunctionType::get(IntptrTy, { IntptrPtrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_ret_close), thunkType->getPointerTo());
    IRB.CreateCall(thunk, { vm, VALUE_INTPTR(operands) });
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void
codegen_t::emit_close(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

#if ENABLE_COMPILE_DEFERRED
    m_compile_queue.push_back((scm_closure_t)operands);
    m_usage.templates++;
#endif

    auto thunkType = FunctionType::get(VoidTy, { IntptrPtrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_close), thunkType->getPointerTo());
    IRB.CreateCall(thunk, { vm, VALUE_INTPTR(operands) });
}

void
codegen_t::emit_push_close_local(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    emit_push_vm_stack(ctx, VALUE_INTPTR(CDR(operands)));

    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
    IRB.CreateBr(CONTINUE);

    // continue emit code in operands
    char local_id[40];
    uuid_v4(local_id, sizeof(local_id));
    Function* L = Function::Create(FunctionType::get(IntptrTy, { IntptrPtrTy }, false), Function::PrivateLinkage, local_id, M);
#if USE_LLVM_ATTRIBUTES
    for (Argument& argument : F->args()) { argument.addAttr(Attribute::NoAlias); argument.addAttr(Attribute::NoCapture); }
#endif
    BasicBlock* LOCAL = BasicBlock::Create(C, "entry", L);
    int function_index = ctx.m_depth - 1 + (ctx.m_argc << 16);
    ctx.m_local_functions[function_index] = L;
    m_usage.locals++;

#if VERBOSE_CODEGEN
    printf("emit_push_close_local level = %d index = %d function_index = %x\n", ctx.m_depth, ctx.m_argc, function_index);
#endif

    context_t ctx2 = ctx;
    ctx2.m_function = L;
    int nargs = FIXNUM(CAR(CAR(operands))) + FIXNUM(CADR(CAR(operands)));
    ctx2.set_local_var_count(ctx2.m_depth, nargs);
    ctx2.m_depth++;
    ctx2.m_argc = 0;
    ctx2.reg_cache_clear();

    IRB.SetInsertPoint(LOCAL);
    transform(ctx2, CDR(operands), true);

    IRB.SetInsertPoint(CONTINUE);
}

void
codegen_t::emit_gloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

#if ENABLE_COMPILE_REFERENCE && ENABLE_COMPILE_DEFERRED
    scm_obj_t obj = ((scm_gloc_t)operands)->value;
    if (CLOSUREP(obj)) {
        scm_closure_t closure = (scm_closure_t)obj;
        if (closure->code == NULL && !HDR_CLOSURE_INSPECTED(closure->hdr)) {
            closure->hdr = closure->hdr | MAKEBITS(1, HDR_CLOSURE_INSPECTED_SHIFT);
            m_compile_queue.push_back((scm_closure_t)obj);
            m_usage.refs++;
        }
    }
#endif

    auto gloc = IRB.CreateBitOrPointerCast(VALUE_INTPTR(operands), IntptrPtrTy);
    auto val = CREATE_LOAD_GLOC_REC(gloc, value);
    if (((scm_gloc_t)operands)->value == scm_undef) {
        BasicBlock* undef_true = BasicBlock::Create(C, "undef_ture", F);
        BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
        auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
        IRB.CreateCondBr(undef_cond, undef_true, CONTINUE, ctx.likely_false);
        IRB.SetInsertPoint(undef_true);
        ctx.reg_cache_copy(vm);
        CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
        auto thunkType = FunctionType::get(VoidTy, { IntptrPtrTy, IntptrTy }, false);
        auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_gloc), thunkType->getPointerTo());
        IRB.CreateCall(thunk, { vm, VALUE_INTPTR(operands) });
        IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));
        IRB.SetInsertPoint(CONTINUE);
    }
    ctx.reg_value.store(vm, val);
}

void
codegen_t::emit_if_symbolp_ret_const(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto value = ctx.reg_value.load(vm);

    BasicBlock* symbol_true = BasicBlock::Create(C, "symbol_true", F);
    BasicBlock* symbol_false = BasicBlock::Create(C, "symbol_false", F);
    emit_cond_symbolp(ctx, value, symbol_true, symbol_false);

    // taken
    IRB.SetInsertPoint(symbol_true);
    ctx.reg_value.store(vm, VALUE_INTPTR(operands));
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

    // not taken
    IRB.SetInsertPoint(symbol_false);
}

void
codegen_t::emit_if_pairp_ret_const(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto value = ctx.reg_value.load(vm);

    BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
    BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
    emit_cond_pairp(ctx, value, pair_true, pair_false);

    // taken
    IRB.SetInsertPoint(pair_true);
    ctx.reg_value.store(vm, VALUE_INTPTR(operands));
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

    // not taken
    IRB.SetInsertPoint(pair_false);
}

void
codegen_t::emit_if_not_nullp_ret_const(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto value = ctx.reg_value.load(vm);

    BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
    BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
    auto taken_cond = IRB.CreateICmpNE(value, VALUE_INTPTR(scm_nil));
    IRB.CreateCondBr(taken_cond, taken_true, taken_false, ctx.likely_false);

    // taken
    IRB.SetInsertPoint(taken_true);
    ctx.reg_value.store(vm, VALUE_INTPTR(operands));
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

    // not taken
    IRB.SetInsertPoint(taken_false);
}

void
codegen_t::emit_if_not_symbolp_ret_const(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto value = ctx.reg_value.load(vm);

    BasicBlock* symbol_true = BasicBlock::Create(C, "symbol_true", F);
    BasicBlock* symbol_false = BasicBlock::Create(C, "symbol_false", F);
    emit_cond_symbolp(ctx, value, symbol_true, symbol_false);

    // taken
    IRB.SetInsertPoint(symbol_false);
    ctx.reg_value.store(vm, VALUE_INTPTR(operands));
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

    // not taken
    IRB.SetInsertPoint(symbol_true);
}

void
codegen_t::emit_nadd_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto retval = emit_alloca(ctx, IntptrTy);

    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
    BasicBlock* fixnum_true = BasicBlock::Create(C, "fixnum_true", F);
    BasicBlock* fallback = BasicBlock::Create(C, "fallback", F);
    auto val = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));
    auto fixnum_cond = IRB.CreateICmpNE(IRB.CreateAnd(val, 1), VALUE_INTPTR(0));
    IRB.CreateCondBr(fixnum_cond, fixnum_true, fallback);

    // fixnum
    IRB.SetInsertPoint(fixnum_true);
    auto intr = Intrinsic::getDeclaration(ctx.m_module, llvm::Intrinsic::ID(Intrinsic::sadd_with_overflow), { IntptrTy });
    auto rs = IRB.CreateCall(intr, { val, VALUE_INTPTR((uintptr_t)CADR(operands) - 1) });
    auto ans = IRB.CreateExtractValue(rs, { 0 });
    auto overflow = IRB.CreateExtractValue(rs, { 1 });
    auto valid_cond = IRB.CreateICmpEQ(overflow, IRB.getInt1(false));
    BasicBlock* valid_true = BasicBlock::Create(C, "valid_true", F);
    IRB.CreateCondBr(valid_cond, valid_true, fallback, ctx.likely_true);
    IRB.SetInsertPoint(valid_true);
    IRB.CreateStore(ans, retval);
    IRB.CreateBr(CONTINUE);

    // fallback
    IRB.SetInsertPoint(fallback);
    ctx.reg_cache_copy_except_value(vm);

    auto thunkType = FunctionType::get(IntptrTy, { IntptrPtrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_nadd_iloc), thunkType->getPointerTo());
    auto res = IRB.CreateCall(thunk, { vm, VALUE_INTPTR(operands) });

    auto success_cond = IRB.CreateICmpNE(res, VALUE_INTPTR(0));
    BasicBlock* fallback_fail = BasicBlock::Create(C, "fallback_fail", F);
    BasicBlock* fallback_success = BasicBlock::Create(C, "fallback_success", F);
    IRB.CreateCondBr(success_cond, fallback_success, fallback_fail, ctx.likely_true);

    IRB.SetInsertPoint(fallback_fail);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

    IRB.SetInsertPoint(fallback_success);
    IRB.CreateStore(res, retval);
    IRB.CreateBr(CONTINUE);

    IRB.SetInsertPoint(CONTINUE);
    ctx.reg_value.store(vm, IRB.CreateLoad(retval));
}

void
codegen_t::emit_extend_enclose(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto thunkType = FunctionType::get(VoidTy, { IntptrPtrTy, IntptrTy }, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_extend_enclose), thunkType->getPointerTo());
    IRB.CreateCall(thunk, { vm, VALUE_INTPTR(operands) });
    ctx.set_local_var_count(ctx.m_depth, 1);
}

void
codegen_t::emit_if_symbolp(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
    BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);

    emit_cond_symbolp(ctx, ctx.reg_value.load(vm), taken_true, taken_false);

    // taken
    IRB.SetInsertPoint(taken_true);
    transform(ctx, operands, false);

    // not taken
    IRB.SetInsertPoint(taken_false);
}

void
codegen_t::emit_apply(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_apply));
}

void
codegen_t::emit_escape(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_escape));
}

void
codegen_t::emit_push_subr(context_t& ctx, scm_obj_t inst, scm_subr_t subr)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);

    intptr_t argc = FIXNUM(CADR(operands));
    auto sp = ctx.reg_sp.load(vm);
    auto argv = IRB.CreateGEP(IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-argc));

    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
    auto subrType = FunctionType::get(IntptrTy, { IntptrPtrTy, IntptrTy, IntptrPtrTy }, false);
    auto ptr = ConstantExpr::getIntToPtr(VALUE_INTPTR(subr->adrs), subrType->getPointerTo());
    auto val = IRB.CreateCall(ptr, { vm, VALUE_INTPTR(argc), argv });

    ctx.reg_sp.store(vm, IRB.CreateBitOrPointerCast(argv, IntptrTy));
    emit_push_vm_stack(ctx, val);

    BasicBlock* undef_true = BasicBlock::Create(C, "undef_true", F);
    auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
    IRB.CreateCondBr(undef_cond, undef_true, CONTINUE, ctx.likely_false);

    // invalid
    IRB.SetInsertPoint(undef_true);
    ctx.reg_cache_copy_except_sp(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

    IRB.SetInsertPoint(CONTINUE);
}

void
codegen_t::emit_push_subr(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    emit_push_subr(ctx, inst, (scm_subr_t)CAR(operands));
}

void
codegen_t::emit_push_subr_gloc_of(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    emit_push_subr(ctx, inst, (scm_subr_t)(((scm_gloc_t)CAR(operands))->value));
}

void
codegen_t::emit_subr(context_t& ctx, scm_obj_t inst, scm_subr_t subr)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);

    intptr_t argc = FIXNUM(CADR(operands));
    auto sp = ctx.reg_sp.load(vm);
    auto argv = IRB.CreateGEP(IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-argc));

    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
    auto subrType = FunctionType::get(IntptrTy, { IntptrPtrTy, IntptrTy, IntptrPtrTy }, false);
    auto ptr = ConstantExpr::getIntToPtr(VALUE_INTPTR(subr->adrs), subrType->getPointerTo());
    auto callee = FunctionCallee(subrType, ptr);
    auto val = IRB.CreateCall(callee, { vm, VALUE_INTPTR(argc), argv });
#if USE_LLVM_ATTRIBUTES
    for (Argument& argument : F->args()) { argument.addAttr(Attribute::NoAlias); argument.addAttr(Attribute::NoCapture); }
#endif

    ctx.reg_sp.store(vm, IRB.CreateBitOrPointerCast(argv, IntptrTy));
    ctx.reg_value.store(vm, val);

    BasicBlock* undef_true = BasicBlock::Create(C, "undef_true", F);
    auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
    IRB.CreateCondBr(undef_cond, undef_true, CONTINUE, ctx.likely_false);

    // invalid
    IRB.SetInsertPoint(undef_true);
    ctx.reg_cache_copy_except_sp(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

    IRB.SetInsertPoint(CONTINUE);
}

void
codegen_t::emit_subr(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    emit_subr(ctx, inst, (scm_subr_t)CAR(operands));
}

void
codegen_t::emit_subr_gloc_of(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    emit_subr(ctx, inst, (scm_subr_t)(((scm_gloc_t)CAR(operands))->value));
}

void
codegen_t::emit_ret_subr(context_t& ctx, scm_obj_t inst, scm_subr_t subr)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto sp = ctx.reg_sp.load(vm);
    auto fp = ctx.reg_fp.load(vm);
    auto argc = VALUE_INTPTR(ctx.m_argc);

    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
    auto subrType = FunctionType::get(IntptrTy, { IntptrPtrTy, IntptrTy, IntptrTy }, false);
    auto ptr = ConstantExpr::getIntToPtr(VALUE_INTPTR(subr->adrs), subrType->getPointerTo());
    auto val = IRB.CreateCall(ptr, { vm, argc, fp });

    ctx.reg_value.store(vm, val);

    BasicBlock* undef_true = BasicBlock::Create(C, "undef_true", F);
    BasicBlock* undef_false = BasicBlock::Create(C, "undef_false", F);
    auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
    IRB.CreateCondBr(undef_cond, undef_true, undef_false, ctx.likely_false);

    // valid
    IRB.SetInsertPoint(undef_false);
    ctx.reg_cache_copy_only_value_and_cont(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

    // invalid
    IRB.SetInsertPoint(undef_true);
    ctx.reg_cache_copy_except_sp(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));}

void
codegen_t::emit_ret_subr(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    emit_ret_subr(ctx, inst, (scm_subr_t)CAR(operands));
}

void
codegen_t::emit_ret_subr_gloc_of(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    emit_ret_subr(ctx, inst, (scm_subr_t)(((scm_gloc_t)CAR(operands))->value));
}

void
codegen_t::emit_cond_pairp(context_t& ctx, Value* obj, BasicBlock* pair_true, BasicBlock* pair_false)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    auto vm = F->arg_begin();

    BasicBlock* cond1_true = BasicBlock::Create(C, "cond1_true", F);
    auto cond1 = IRB.CreateICmpEQ(IRB.CreateAnd(obj, VALUE_INTPTR(0x7)), VALUE_INTPTR(0x0));
    IRB.CreateCondBr(cond1, cond1_true, pair_false, ctx.likely_true);
    IRB.SetInsertPoint(cond1_true);
    auto hdr = IRB.CreateLoad(IRB.CreateBitOrPointerCast(obj, IntptrPtrTy));
    auto cond2 = IRB.CreateICmpNE(IRB.CreateAnd(hdr, VALUE_INTPTR(HDR_ATTR_MASKBITS)), VALUE_INTPTR(HDR_ATTR_BOXED));
    IRB.CreateCondBr(cond2, pair_true, pair_false, ctx.likely_true);
}

void
codegen_t::emit_cond_symbolp(context_t& ctx, Value* obj, BasicBlock* symbol_true, BasicBlock* symbol_false)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    auto vm = F->arg_begin();

    BasicBlock* cond1_true = BasicBlock::Create(C, "cond1_true", F);
    auto cond1 = IRB.CreateICmpEQ(IRB.CreateAnd(obj, VALUE_INTPTR(0x7)), VALUE_INTPTR(0x0));
    IRB.CreateCondBr(cond1, cond1_true, symbol_false, ctx.likely_true);
    IRB.SetInsertPoint(cond1_true);
    auto hdr = IRB.CreateLoad(IRB.CreateBitOrPointerCast(obj, IntptrPtrTy));
    auto cond2 = IRB.CreateICmpEQ(IRB.CreateAnd(hdr, VALUE_INTPTR(HDR_TYPE_MASKBITS)), VALUE_INTPTR(scm_hdr_symbol));
    IRB.CreateCondBr(cond2, symbol_true, symbol_false, ctx.likely_true);
}
