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

void
codegen_t::emit_push(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));
    CREATE_PUSH_VM_STACK(CREATE_LOAD_VM_REG(vm, m_value));
}

void
codegen_t::emit_push_const(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));
    CREATE_PUSH_VM_STACK(VALUE_INTPTR(operands));
}

void
codegen_t::emit_push_iloc0(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));
    CREATE_PUSH_VM_STACK(IRB.CreateLoad(emit_lookup_iloc(ctx, 0, FIXNUM(operands))));
}

void
codegen_t::emit_push_iloc1(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));
    CREATE_PUSH_VM_STACK(IRB.CreateLoad(emit_lookup_iloc(ctx, 1, FIXNUM(operands))));
}

void
codegen_t::emit_push_gloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));
    auto gloc = IRB.CreateBitOrPointerCast(VALUE_INTPTR(operands), IntptrPtrTy);
    auto val = CREATE_LOAD_GLOC_REC(gloc, value);
/*
    BasicBlock* undef_true = BasicBlock::Create(C, "undef_true", F);
    BasicBlock* undef_false = BasicBlock::Create(C, "undef_false", F);
    auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
    IRB.CreateCondBr(undef_cond, undef_true, undef_false);
    // invalid
    IRB.SetInsertPoint(undef_true);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_error_push_gloc));
    // valid
    IRB.SetInsertPoint(undef_false);
*/
    CREATE_PUSH_VM_STACK(val);
}

void
codegen_t::emit_push_car_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));
    auto pair = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));
    // check if pair
    BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
    BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);

    emit_cond_pairp(ctx, pair, pair_true, pair_false);
//    auto pair_cond = CREATE_PAIRP(pair);
//    IRB.CreateCondBr(pair_cond, pair_true, pair_false);
    // nonpair
    IRB.SetInsertPoint(pair_false);
        CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
        auto c_error_push_car_iloc = M->getOrInsertFunction("c_error_push_car_iloc", VoidTy, IntptrPtrTy, IntptrTy);
        IRB.CreateCall(c_error_push_car_iloc, { vm, pair });
        IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));
    // pair
    IRB.SetInsertPoint(pair_true);
    CREATE_PUSH_VM_STACK(CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), car));
}

void
codegen_t::emit_push_cdr_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));
    auto pair = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));
    // check if pair
    BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
    BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);

    emit_cond_pairp(ctx, pair, pair_true, pair_false);
//    auto pair_cond = CREATE_PAIRP(pair);
//    IRB.CreateCondBr(pair_cond, pair_true, pair_false);
    // nonpair
    IRB.SetInsertPoint(pair_false);
        CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
        auto c_error_push_cdr_iloc = M->getOrInsertFunction("c_error_push_cdr_iloc", VoidTy, IntptrPtrTy, IntptrTy);
        IRB.CreateCall(c_error_push_cdr_iloc, {vm, pair});
        IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));
    // pair
    IRB.SetInsertPoint(pair_true);
    CREATE_PUSH_VM_STACK(CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), cdr));
}

void
codegen_t::emit_push_cddr_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));
    auto pair = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));
    // check if pair
    BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
    BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);

    emit_cond_pairp(ctx, pair, pair_true, pair_false);
    // nonpair
    IRB.SetInsertPoint(pair_false);
        CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
        auto c_error_push_cddr_iloc = M->getOrInsertFunction("c_error_push_cddr_iloc", VoidTy, IntptrPtrTy, IntptrTy);
        IRB.CreateCall(c_error_push_cddr_iloc, {vm, pair});
        IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));
    // pair
    IRB.SetInsertPoint(pair_true);
        auto pair2 = CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), cdr);
        BasicBlock* pair2_true = BasicBlock::Create(C, "pair2_true", F);
        emit_cond_pairp(ctx, pair2, pair2_true, pair_false);
    // pair + pair
    IRB.SetInsertPoint(pair2_true);
      CREATE_PUSH_VM_STACK(CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair2, IntptrPtrTy), cdr));
}

void
codegen_t::emit_push_cadr_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));
    auto pair = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));
    // check if pair
    BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
    BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);


    emit_cond_pairp(ctx, pair, pair_true, pair_false);
//    auto pair_cond = CREATE_PAIRP(pair);
//    IRB.CreateCondBr(pair_cond, pair_true, pair_false);
    // nonpair
    IRB.SetInsertPoint(pair_false);
        CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
        auto c_error_push_cadr_iloc = M->getOrInsertFunction("c_error_push_cadr_iloc", VoidTy, IntptrPtrTy, IntptrTy);
        IRB.CreateCall(c_error_push_cadr_iloc, {vm, pair});
        IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));
    // pair
    IRB.SetInsertPoint(pair_true);
        auto pair2 = CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), cdr);

        BasicBlock* pair2_true = BasicBlock::Create(C, "pair2_true", F);
        emit_cond_pairp(ctx, pair2, pair2_true, pair_false);
//        auto pair2_cond = CREATE_PAIRP(pair2);
//        IRB.CreateCondBr(pair2_cond, pair2_true, pair_false);
    // pair + pair
    IRB.SetInsertPoint(pair2_true);
      CREATE_PUSH_VM_STACK(CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair2, IntptrPtrTy), car));
}

void
codegen_t::emit_push_nadd_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));
    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
    BasicBlock* nonfixnum_true = BasicBlock::Create(C, "nonfixnum_true", F);
    BasicBlock* nonfixnum_false = BasicBlock::Create(C, "nonfixnum_false", F);
    auto val = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));
    auto nonfixnum_cond = IRB.CreateICmpEQ(IRB.CreateAnd(val, 1), VALUE_INTPTR(0));
    IRB.CreateCondBr(nonfixnum_cond, nonfixnum_true, nonfixnum_false);
    // fixnum
    IRB.SetInsertPoint(nonfixnum_false);
        auto intr = Intrinsic::getDeclaration(ctx.m_module, llvm::Intrinsic::ID(Intrinsic::sadd_with_overflow), { IntptrTy });
        auto rs = IRB.CreateCall(intr, { val, VALUE_INTPTR((uintptr_t)CADR(operands) - 1) });
        auto ans = IRB.CreateExtractValue(rs, { 0 });
        auto overflow = IRB.CreateExtractValue(rs, { 1 });
        auto ans_valid_cond = IRB.CreateICmpEQ(overflow, IRB.getInt1(false));
        BasicBlock* ans_valid_true = BasicBlock::Create(C, "ans_valid_true", F);
        BasicBlock* ans_valid_false = BasicBlock::Create(C, "ans_valid_false", F);
        IRB.CreateCondBr(ans_valid_cond, ans_valid_true, ans_valid_false);
        IRB.SetInsertPoint(ans_valid_true);
        CREATE_PUSH_VM_STACK(ans);
        IRB.CreateBr(CONTINUE);
    // others
    IRB.SetInsertPoint(nonfixnum_true);
        auto c_number_pred = M->getOrInsertFunction("c_number_pred", IntptrTy, IntptrTy);
        auto nonnum_cond = IRB.CreateICmpEQ(IRB.CreateCall(c_number_pred, {val}), VALUE_INTPTR(0));
        BasicBlock* nonnum_true = BasicBlock::Create(C, "nonnum_true", F);
        BasicBlock* nonnum_false = BasicBlock::Create(C, "nonnum_false", F);
        IRB.CreateCondBr(nonnum_cond, nonnum_true, nonnum_false);
        // not number
        IRB.SetInsertPoint(nonnum_true);
            CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
            auto c_error_push_nadd_iloc = M->getOrInsertFunction("c_error_push_nadd_iloc", VoidTy, IntptrPtrTy, IntptrTy, IntptrTy);
            IRB.CreateCall(c_error_push_nadd_iloc, {vm, val, VALUE_INTPTR(CADR(operands))});
            IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));
        // number
        IRB.SetInsertPoint(nonnum_false);
            auto c_arith_add = M->getOrInsertFunction("c_arith_add", IntptrTy, IntptrPtrTy, IntptrTy, IntptrTy);
            CREATE_PUSH_VM_STACK(IRB.CreateCall(c_arith_add, {vm, val, VALUE_INTPTR(CADR(operands))}));
            IRB.CreateBr(CONTINUE);
    IRB.SetInsertPoint(ans_valid_false);
    IRB.CreateBr(nonnum_false);

    IRB.SetInsertPoint(CONTINUE);
}

void
codegen_t::emit_apply_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto val = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));
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
codegen_t::emit_apply_gloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    scm_gloc_t gloc = (scm_gloc_t)CAR(operands);
    scm_obj_t obj = gloc->value;
    if (obj == ctx.m_top_level_closure && HDR_CLOSURE_ARGS(ctx.m_top_level_closure->hdr) == ctx.m_argc) {
        puts("+ self recursive");
        CREATE_STACK_OVERFLOW_HANDLER(sizeof(vm_env_rec_t));
        // printf("argc = %d\n", ctx.m_argc);
        auto c_prepare_apply = M->getOrInsertFunction("c_prepare_apply", VoidTy, IntptrPtrTy, IntptrTy);
        IRB.CreateCall(c_prepare_apply, {vm, VALUE_INTPTR(ctx.m_top_level_closure)});
        auto call = IRB.CreateCall(ctx.m_top_level_function, { vm });
        call->setTailCallKind(CallInst::TCK_MustTail);
        IRB.CreateRet(call);
        return;
    } else {
        if (CLOSUREP(obj) && SYMBOLP(gloc->variable)) {
            //printf("obj %p\n", obj);
            //printf("ctx.m_top_level_closure %p\n", ctx.m_top_level_closure);
            //printf("closure argc %ld\n", HDR_CLOSURE_ARGS(ctx.m_top_level_closure->hdr));
            //printf("ctx.m_argc %d\n", ctx.m_argc);
            scm_symbol_t symbol = (scm_symbol_t)gloc->variable;
            if (strchr(symbol->name, IDENTIFIER_RENAME_DELIMITER)) {
                printf("+ uninterned gloc found: %s\n", symbol->name);
                scm_closure_t closure = (scm_closure_t)obj;
                Function* F2 = emit_lifted_function(ctx, closure); // [TODO] return non null even if two call site exists
                CREATE_STACK_OVERFLOW_HANDLER(sizeof(vm_env_rec_t));

                auto c_prepare_apply = M->getOrInsertFunction("c_prepare_apply", VoidTy, IntptrPtrTy, IntptrTy);
                auto call = IRB.CreateCall(c_prepare_apply, { vm, VALUE_INTPTR(closure) });
#if USE_LLVM_ATTRIBUTES
                auto attrs = AttributeList::get(C, AttributeList::FunctionIndex, Attribute::NoUnwind);
                attrs = attrs.addParamAttribute(C, 0, Attribute::NoAlias);
                attrs = attrs.addParamAttribute(C, 0, Attribute::NoCapture);
                call->setAttributes(attrs);
#endif
                if (F2 == nullptr) {
                    puts("*** F2 is null");
                    scm_bvector_t bv = (scm_bvector_t)CADR(CAR(closure->code));
                    intptr_t (*adrs)(intptr_t) = (intptr_t (*)(intptr_t))(*(intptr_t*)bv->elts);
                    auto subrType = FunctionType::get(IntptrTy, {IntptrPtrTy}, false);
                    auto func = ConstantExpr::getIntToPtr(VALUE_INTPTR(adrs), subrType->getPointerTo());
                    auto call = IRB.CreateCall(func, {vm});
                    call->setTailCallKind(CallInst::TCK_MustTail);
                    IRB.CreateRet(call);
                    return;
                } else {
                    auto call = IRB.CreateCall(F2, {vm});
                    call->setTailCallKind(CallInst::TCK_MustTail);
                    IRB.CreateRet(call);
                    return;
                }
            }
        }

        // we cannot assume what is in gloc box at this point

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
}

void
codegen_t::emit_ret_const(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto val = VALUE_INTPTR(operands);
    CREATE_STORE_VM_REG(vm, m_value, val);
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
codegen_t::emit_ret_cons(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto val = CREATE_LOAD_VM_REG(vm, m_value);
    auto sp_minus_1 = IRB.CreateLoad(IRB.CreateGEP(IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-1)));
    auto c_make_pair = M->getOrInsertFunction("c_make_pair", IntptrTy, IntptrPtrTy, IntptrTy, IntptrTy);
    CREATE_STORE_VM_REG(vm, m_value, IRB.CreateCall(c_make_pair, {vm, sp_minus_1, val}));
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void
codegen_t::emit_subr(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);

    intptr_t argc = FIXNUM(CADR(operands));
    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto argv = IRB.CreateSub(sp, VALUE_INTPTR(argc << log2_of_intptr_size()));

    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
    scm_subr_t subr = (scm_subr_t)CAR(operands);
    auto subrType = FunctionType::get(IntptrTy, {IntptrPtrTy, IntptrTy, IntptrTy}, false);
    auto ptr = ConstantExpr::getIntToPtr(VALUE_INTPTR(subr->adrs), subrType->getPointerTo());
    auto val = IRB.CreateCall(ptr, {vm, VALUE_INTPTR(argc), argv});

    CREATE_STORE_VM_REG(vm, m_value, val);
    CREATE_STORE_VM_REG(vm, m_sp, IRB.CreateSub(CREATE_LOAD_VM_REG(vm, m_sp), VALUE_INTPTR(argc << log2_of_intptr_size())));

    BasicBlock* undef_true = BasicBlock::Create(C, "undef_true", F);
    auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
    IRB.CreateCondBr(undef_cond, undef_true, CONTINUE);
    // invalid
    IRB.SetInsertPoint(undef_true);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

    IRB.SetInsertPoint(CONTINUE);
}

void
codegen_t::emit_ret_subr(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto fp = CREATE_LOAD_VM_REG(vm, m_fp);
    auto argc = VALUE_INTPTR(ctx.m_argc);

    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
    scm_subr_t subr = (scm_subr_t)CAR(operands);
    auto subrType = FunctionType::get(IntptrTy, {IntptrPtrTy, IntptrTy, IntptrTy}, false);
    auto ptr = ConstantExpr::getIntToPtr(VALUE_INTPTR(subr->adrs), subrType->getPointerTo());
    auto val = IRB.CreateCall(ptr, {vm, argc, fp});

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
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));
}

void
codegen_t::emit_push_subr(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);

    intptr_t argc = FIXNUM(CADR(operands));
    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto argv = IRB.CreateSub(sp, VALUE_INTPTR(argc << log2_of_intptr_size()));

    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
    scm_subr_t subr = (scm_subr_t)CAR(operands);
    auto subrType = FunctionType::get(IntptrTy, {IntptrPtrTy, IntptrTy, IntptrTy}, false);
    auto ptr = ConstantExpr::getIntToPtr(VALUE_INTPTR(subr->adrs), subrType->getPointerTo());
    auto val = IRB.CreateCall(ptr, {vm, VALUE_INTPTR(argc), argv});

    CREATE_STORE_VM_REG(vm, m_sp, IRB.CreateSub(CREATE_LOAD_VM_REG(vm, m_sp), VALUE_INTPTR(argc << log2_of_intptr_size())));
    CREATE_STORE_VM_REG(vm, m_value, val);
    CREATE_PUSH_VM_STACK(val);

    BasicBlock* undef_true = BasicBlock::Create(C, "undef_true", F);
    auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
    IRB.CreateCondBr(undef_cond, undef_true, CONTINUE);
    // invalid
    IRB.SetInsertPoint(undef_true);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

    IRB.SetInsertPoint(CONTINUE);
}

void
codegen_t::emit_if_true(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
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
    transform(ctx, operands);
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

    auto value = CREATE_LOAD_VM_REG(vm, m_value);
    BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
    BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
    auto taken_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_nil));
    IRB.CreateCondBr(taken_cond, taken_true, taken_false);
    // taken
    IRB.SetInsertPoint(taken_true);
    transform(ctx, operands);
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

    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto ea = IRB.CreateGEP(IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-1));
    CREATE_STORE_VM_REG(vm, m_sp, IRB.CreateBitOrPointerCast(ea, IntptrTy));
    auto val1 = IRB.CreateLoad(ea);
    auto val2 = CREATE_LOAD_VM_REG(vm, m_value);

    BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
    BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
    auto taken_cond = IRB.CreateICmpEQ(val1, val2);
    IRB.CreateCondBr(taken_cond, taken_true, taken_false);
    // taken
    IRB.SetInsertPoint(taken_true);
    transform(ctx, operands);
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
codegen_t::emit_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto val = IRB.CreateLoad(emit_lookup_iloc(ctx, operands));
    CREATE_STORE_VM_REG(vm, m_value, val);
}

void
codegen_t::emit_iloc0(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto val = IRB.CreateLoad(emit_lookup_iloc(ctx, 0, FIXNUM(operands)));
    CREATE_STORE_VM_REG(vm, m_value, val);
}

void
codegen_t::emit_iloc1(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto val = IRB.CreateLoad(emit_lookup_iloc(ctx, 1, FIXNUM(operands)));
    CREATE_STORE_VM_REG(vm, m_value, val);
}

void
codegen_t::emit_push_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));
    CREATE_PUSH_VM_STACK(IRB.CreateLoad(emit_lookup_iloc(ctx, operands)));
}

void
codegen_t::emit_if_true_ret(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto value = CREATE_LOAD_VM_REG(vm, m_value);
    BasicBlock* value_false = BasicBlock::Create(C, "value_false", F);
    BasicBlock* value_nonfalse = BasicBlock::Create(C, "value_nonfalse", F);
    auto value_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_false));
    IRB.CreateCondBr(value_cond, value_false, value_nonfalse);
    // pop
    IRB.SetInsertPoint(value_nonfalse);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
    // loop
    IRB.SetInsertPoint(value_false);
}

void
codegen_t::emit_if_false_ret(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto value = CREATE_LOAD_VM_REG(vm, m_value);
    BasicBlock* value_false = BasicBlock::Create(C, "value_false", F);
    BasicBlock* value_nonfalse = BasicBlock::Create(C, "value_nonfalse", F);
    auto value_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_false));
    IRB.CreateCondBr(value_cond, value_false, value_nonfalse);
    // pop
    IRB.SetInsertPoint(value_false);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
    // loop
    IRB.SetInsertPoint(value_nonfalse);
}

void
codegen_t::emit_if_true_ret_const(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto value = CREATE_LOAD_VM_REG(vm, m_value);
    BasicBlock* value_false = BasicBlock::Create(C, "value_false", F);
    BasicBlock* value_nonfalse = BasicBlock::Create(C, "value_nonfalse", F);
    auto value_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_false));
    IRB.CreateCondBr(value_cond, value_false, value_nonfalse);
    // pop
    IRB.SetInsertPoint(value_nonfalse);
    CREATE_STORE_VM_REG(vm, m_value, VALUE_INTPTR(operands));
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
    // loop
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
codegen_t::emit_cc_n_iloc(context_t& ctx, scm_obj_t inst, cc_t cc, const char* cfunc)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto lhs = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));
    auto rhs = VALUE_INTPTR(CADR(operands));
    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
    BasicBlock* nonfixnum_true = BasicBlock::Create(C, "nonfixnum_true", F);
    BasicBlock* nonfixnum_false = BasicBlock::Create(C, "nonfixnum_false", F);
    auto nonfixnum_cond = IRB.CreateICmpEQ(IRB.CreateAnd(lhs, 1), VALUE_INTPTR(0));
    IRB.CreateCondBr(nonfixnum_cond, nonfixnum_true, nonfixnum_false);
    // fixnum
    IRB.SetInsertPoint(nonfixnum_false);
        BasicBlock* cond_true = BasicBlock::Create(C, "cond_true", F);
        BasicBlock* cond_false = BasicBlock::Create(C, "cond_false", F);
        auto cond = emit_cmp_inst(ctx, cc, lhs, rhs);
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
        CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
        auto c_function = M->getOrInsertFunction(cfunc, IntptrTy, IntptrPtrTy, IntptrTy, IntptrTy);
        auto fallback_cond = IRB.CreateICmpEQ(IRB.CreateCall(c_function, { vm, lhs, rhs }), VALUE_INTPTR(0));
        BasicBlock* fallback_fail = BasicBlock::Create(C, "fallback_fail", F);
        IRB.CreateCondBr(fallback_cond, CONTINUE, fallback_fail);
        IRB.SetInsertPoint(fallback_fail);
        IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));
    IRB.SetInsertPoint(CONTINUE);
}

void
codegen_t::emit_lt_n_iloc(context_t& ctx, scm_obj_t inst)
{
  emit_cc_n_iloc(ctx, inst, LT, "c_lt_n_iloc");
}

void
codegen_t::emit_gt_n_iloc(context_t& ctx, scm_obj_t inst)
{
  emit_cc_n_iloc(ctx, inst, GT, "c_gt_n_iloc");
}

void
codegen_t::emit_eq_n_iloc(context_t& ctx, scm_obj_t inst)
{
  emit_cc_n_iloc(ctx, inst, EQ, "c_eq_n_iloc");
}

void
codegen_t::emit_cc_iloc(context_t& ctx, scm_obj_t inst, cc_t cc, const char* cfunc)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto lhs = CREATE_LOAD_VM_REG(vm, m_value);
    auto rhs = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));
    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
    BasicBlock* nonfixnum_true = BasicBlock::Create(C, "nonfixnum_true", F);
    BasicBlock* nonfixnum_false = BasicBlock::Create(C, "nonfixnum_false", F);
    auto nonfixnum_cond = IRB.CreateICmpEQ(IRB.CreateAnd(lhs, IRB.CreateAnd(rhs, 1)), VALUE_INTPTR(0));
    IRB.CreateCondBr(nonfixnum_cond, nonfixnum_true, nonfixnum_false);
    // fixnum
    IRB.SetInsertPoint(nonfixnum_false);
        BasicBlock* cond_true = BasicBlock::Create(C, "cond_true", F);
        BasicBlock* cond_false = BasicBlock::Create(C, "cond_false", F);
        auto cond = emit_cmp_inst(ctx, cc, lhs, rhs);
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
        CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
        auto c_function = M->getOrInsertFunction(cfunc, IntptrTy, IntptrPtrTy, IntptrTy, IntptrTy);
        auto fallback_cond = IRB.CreateICmpEQ(IRB.CreateCall(c_function, { vm, lhs, rhs }), VALUE_INTPTR(0));
        BasicBlock* fallback_fail = BasicBlock::Create(C, "fallback_fail", F);
        IRB.CreateCondBr(fallback_cond, CONTINUE, fallback_fail);
        IRB.SetInsertPoint(fallback_fail);
        IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));
    IRB.SetInsertPoint(CONTINUE);
}

void
codegen_t::emit_gt_iloc(context_t& ctx, scm_obj_t inst)
{
  emit_cc_iloc(ctx, inst, GT, "c_gt_iloc");
}

void
codegen_t::emit_lt_iloc(context_t& ctx, scm_obj_t inst)
{
  emit_cc_iloc(ctx, inst, LT, "c_lt_iloc");
}

void
codegen_t::emit_eq_iloc(context_t& ctx, scm_obj_t inst)
{
  emit_cc_iloc(ctx, inst, EQ, "c_eq_iloc");
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
#if USE_LLVM_ATTRIBUTES
    K->addFnAttr(Attribute::NoUnwind);
    K->addParamAttr(0, Attribute::NoAlias);
    K->addParamAttr(0, Attribute::NoCapture);
#endif

    BasicBlock* RETURN = BasicBlock::Create(C, "entry", K);

    // vm_cont_t cont = (vm_cont_t)m_sp;
    auto cont = IRB.CreateBitOrPointerCast(CREATE_LOAD_VM_REG(vm, m_sp), IntptrPtrTy);

    auto prepare_call = M->getOrInsertFunction("prepare_call", VoidTy, IntptrPtrTy, IntptrPtrTy);
    auto call = IRB.CreateCall(prepare_call, { vm, cont });
    call->setCallingConv(CallingConv::Fast);
#if USE_LLVM_ATTRIBUTES
    auto attrs = AttributeList::get(C, AttributeList::FunctionIndex, Attribute::NoUnwind);
    attrs = attrs.addParamAttribute(C, 0, Attribute::NoAlias);
    attrs = attrs.addParamAttribute(C, 0, Attribute::NoCapture);
    attrs = attrs.addParamAttribute(C, 1, Attribute::NoAlias);
    attrs = attrs.addParamAttribute(C, 1, Attribute::NoCapture);
    call->setAttributes(attrs);
#endif

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
#if USE_LLVM_ATTRIBUTES
    L->addFnAttr(Attribute::NoUnwind);
    L->addParamAttr(0, Attribute::NoAlias);
    L->addParamAttr(0, Attribute::NoCapture);
#endif

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

void
codegen_t::emit_push_cons(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto val = CREATE_LOAD_VM_REG(vm, m_value);
    auto ea = IRB.CreateGEP(IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-1));
    auto sp_minus_1 = IRB.CreateLoad(ea);
    auto c_make_pair = M->getOrInsertFunction("c_make_pair", IntptrTy, IntptrPtrTy, IntptrTy, IntptrTy);
    IRB.CreateStore(IRB.CreateCall(c_make_pair, {vm, sp_minus_1, val}), ea);
}

void
codegen_t::emit_push_close(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));
    auto c_push_close = M->getOrInsertFunction("c_push_close", IntptrTy, IntptrPtrTy, IntptrTy);
    IRB.CreateCall(c_push_close, { vm, VALUE_INTPTR(operands) });
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
        CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
        auto c_error_car_iloc = M->getOrInsertFunction("c_error_car_iloc", VoidTy, IntptrPtrTy, IntptrTy);
        IRB.CreateCall(c_error_car_iloc, {vm, pair});
        IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));
    // pair
    IRB.SetInsertPoint(pair_true);
    CREATE_STORE_VM_REG(vm, m_value, CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), car));
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
        CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
        auto c_error_cdr_iloc = M->getOrInsertFunction("c_error_cdr_iloc", VoidTy, IntptrPtrTy, IntptrTy);
        IRB.CreateCall(c_error_cdr_iloc, {vm, pair});
        IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));
    // pair
    IRB.SetInsertPoint(pair_true);
    CREATE_STORE_VM_REG(vm, m_value, CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), cdr));
}

void
codegen_t::emit_set_gloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    BasicBlock* success_false = BasicBlock::Create(C, "success_false", F);
    BasicBlock* success_true = BasicBlock::Create(C, "success_true", F);
    auto c_set_gloc = M->getOrInsertFunction("c_set_gloc", IntptrTy, IntptrPtrTy, IntptrTy);
    auto success_cond = IRB.CreateICmpEQ(IRB.CreateCall(c_set_gloc, { vm, VALUE_INTPTR(operands) }), VALUE_INTPTR(0));
    IRB.CreateCondBr(success_cond, success_true, success_false);
    IRB.SetInsertPoint(success_false);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));
    IRB.SetInsertPoint(success_true);
}

void
codegen_t::emit_const(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    CREATE_STORE_VM_REG(vm, m_value, VALUE_INTPTR(operands));
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

    emit_cond_pairp(ctx, CREATE_LOAD_VM_REG(vm, m_value), taken_true, taken_false);
    // taken
    IRB.SetInsertPoint(taken_true);
    transform(ctx, operands);
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

    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto ea = IRB.CreateGEP(IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-1));
    CREATE_STORE_VM_REG(vm, m_sp, IRB.CreateBitOrPointerCast(ea, IntptrTy));
    auto val1 = IRB.CreateLoad(ea);
    auto val2 = CREATE_LOAD_VM_REG(vm, m_value);

    BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
    BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
    auto taken_cond = IRB.CreateICmpEQ(val1, val2);
    IRB.CreateCondBr(taken_cond, taken_true, taken_false);
    // taken
    IRB.SetInsertPoint(taken_true);
    CREATE_STORE_VM_REG(vm, m_value, VALUE_INTPTR(operands));
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
        CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
        auto c_error_cadr_iloc = M->getOrInsertFunction("c_error_cadr_iloc", VoidTy, IntptrPtrTy, IntptrTy);
        IRB.CreateCall(c_error_cadr_iloc, {vm, pair});
        IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));
    // pair
    IRB.SetInsertPoint(pair_true);
        auto pair2 = CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), cdr);
        BasicBlock* pair2_true = BasicBlock::Create(C, "pair2_true", F);
        emit_cond_pairp(ctx, pair2, pair2_true, pair_false);
    // pair + pair
    IRB.SetInsertPoint(pair2_true);
      CREATE_STORE_VM_REG(vm, m_value, CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair2, IntptrPtrTy), car));
}

void
codegen_t::emit_if_not_eqp_ret_const(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto ea = IRB.CreateGEP(IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-1));
    CREATE_STORE_VM_REG(vm, m_sp, IRB.CreateBitOrPointerCast(ea, IntptrTy));
    auto val1 = IRB.CreateLoad(ea);
    auto val2 = CREATE_LOAD_VM_REG(vm, m_value);

    BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
    BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
    auto taken_cond = IRB.CreateICmpNE(val1, val2);
    IRB.CreateCondBr(taken_cond, taken_true, taken_false);
    // taken
    IRB.SetInsertPoint(taken_true);
    CREATE_STORE_VM_REG(vm, m_value, VALUE_INTPTR(operands));
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

    auto value = CREATE_LOAD_VM_REG(vm, m_value);
    BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
    BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);

    emit_cond_pairp(ctx, value, pair_true, pair_false);
    // not pair
    IRB.SetInsertPoint(pair_false);
    CREATE_STORE_VM_REG(vm, m_value, VALUE_INTPTR(operands));
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

    auto value = CREATE_LOAD_VM_REG(vm, m_value);
    BasicBlock* value_false = BasicBlock::Create(C, "value_false", F);
    BasicBlock* value_nonfalse = BasicBlock::Create(C, "value_nonfalse", F);
    auto value_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_false));
    IRB.CreateCondBr(value_cond, value_false, value_nonfalse);
    // pop
    IRB.SetInsertPoint(value_false);
    CREATE_STORE_VM_REG(vm, m_value, VALUE_INTPTR(operands));
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
    // loop
    IRB.SetInsertPoint(value_nonfalse);
}

void
codegen_t::emit_ret_nullp(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
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
    CREATE_STORE_VM_REG(vm, m_value, VALUE_INTPTR(scm_true));
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
    // not taken
    IRB.SetInsertPoint(taken_false);
    CREATE_STORE_VM_REG(vm, m_value, VALUE_INTPTR(scm_false));
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void
codegen_t::emit_ret_pairp(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto value = CREATE_LOAD_VM_REG(vm, m_value);
    BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
    BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
    emit_cond_pairp(ctx, value, taken_true, taken_false);
    // taken
    IRB.SetInsertPoint(taken_true);
    CREATE_STORE_VM_REG(vm, m_value, VALUE_INTPTR(scm_true));
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
    // not taken
    IRB.SetInsertPoint(taken_false);
    CREATE_STORE_VM_REG(vm, m_value, VALUE_INTPTR(scm_false));
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

/*

browse
> (cc init)
generating native code: init
##### unsupported instruction set.iloc ######
+ uninterned gloc found: .L15`58*
 + generating native code for lifted function
##### unsupported instruction set.iloc ######
+ self recursive

> (cc browse-random)
generating native code: browse-random
##### unsupported instruction ret.gloc ######

> (cc randomize)
generating native code: randomize
##### unsupported instruction set.iloc ######
##### unsupported instruction set.iloc ######
##### unsupported instruction set.iloc ######

> (cc match)
generating native code: match
##### unsupported instruction ret.eq? ######

*/
