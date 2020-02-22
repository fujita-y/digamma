
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
codegen_t::emit_push_car_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));
    auto val = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));
    // check if pair
    BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
    BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
    auto pair_cond = IRB.CreateICmpEQ(IRB.CreateAnd(val, 1), VALUE_INTPTR(0));
    IRB.CreateCondBr(pair_cond, pair_true, pair_false);
    // nonpair
    IRB.SetInsertPoint(pair_false);
        auto c_error_push_car_iloc = M->getOrInsertFunction("c_error_push_car_iloc", VoidTy, IntptrPtrTy, IntptrTy);
        IRB.CreateCall(c_error_push_car_iloc, {vm, val});
        IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_back_to_loop));
    // pair
    IRB.SetInsertPoint(pair_true);
    CREATE_PUSH_VM_STACK(CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(val, IntptrPtrTy), car));
}

void
codegen_t::emit_push_cdr_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    CREATE_STACK_OVERFLOW_HANDLER(sizeof(scm_obj_t));
    auto val = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));
    // check if pair
    BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
    BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
    auto pair_cond = IRB.CreateICmpEQ(IRB.CreateAnd(val, 1), VALUE_INTPTR(0));
    IRB.CreateCondBr(pair_cond, pair_true, pair_false);
    // nonpair
    IRB.SetInsertPoint(pair_false);
        auto c_error_push_cdr_iloc = M->getOrInsertFunction("c_error_push_cdr_iloc", VoidTy, IntptrPtrTy, IntptrTy);
        IRB.CreateCall(c_error_push_cdr_iloc, {vm, val});
        IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_back_to_loop));
    // pair
    IRB.SetInsertPoint(pair_true);
    CREATE_PUSH_VM_STACK(CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(val, IntptrPtrTy), cdr));
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
            auto c_error_push_nadd_iloc = M->getOrInsertFunction("c_error_push_nadd_iloc", VoidTy, IntptrPtrTy, IntptrTy, IntptrTy);
            IRB.CreateCall(c_error_push_nadd_iloc, {vm, val, VALUE_INTPTR(CADR(operands))});
            IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_back_to_loop));
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

    scm_obj_t obj = ((scm_gloc_t)CAR(operands))->value;
    if (obj == ctx.m_top_level_closure && HDR_CLOSURE_ARGS(ctx.m_top_level_closure->hdr) == ctx.m_argc) {
        // recursive
        CREATE_STACK_OVERFLOW_HANDLER(sizeof(vm_env_rec_t));
        // printf("argc = %d\n", ctx.m_argc);
        auto c_prepare_apply = M->getOrInsertFunction("c_prepare_apply", VoidTy, IntptrPtrTy, IntptrTy);
        IRB.CreateCall(c_prepare_apply, {vm, VALUE_INTPTR(ctx.m_top_level_closure)});
        auto call = IRB.CreateCall(ctx.m_top_level_function, { vm });
        call->setTailCallKind(CallInst::TCK_MustTail);
        IRB.CreateRet(call);
    } else {
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

/*
void
codegen_t::emit_ret_subr(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    BasicBlock* FALLBACK = BasicBlock::Create(C, "fallback", F);

    auto sp = CREATE_LOAD_VM_REG(vm, m_sp);
    auto fp = CREATE_LOAD_VM_REG(vm, m_fp);
    auto argc = IRB.CreateAShr(IRB.CreateSub(sp, fp), VALUE_INTPTR(log2_of_intptr_size()));
    scm_subr_t subr = (scm_subr_t)CAR(operands);

    if (ctx.m_argc == 2 && subr->adrs == subr_num_add) {
        puts("shortcut");
        auto fp_0 = IRB.CreateLoad(IRB.CreateGEP(IRB.CreateBitOrPointerCast(fp, IntptrPtrTy), VALUE_INTPTR(0)));
        auto fp_1 = IRB.CreateLoad(IRB.CreateGEP(IRB.CreateBitOrPointerCast(fp, IntptrPtrTy), VALUE_INTPTR(1)));
        auto fallback_cond = IRB.CreateICmpEQ(IRB.CreateAnd(IRB.CreateAnd(fp_0, fp_1), VALUE_INTPTR(1)), VALUE_INTPTR(0));
        BasicBlock* fallback_false = BasicBlock::Create(C, "fixnum_true", F);
        IRB.CreateCondBr(fallback_cond, FALLBACK, fallback_false);
        IRB.SetInsertPoint(fallback_false);
        auto intr = Intrinsic::getDeclaration(ctx.m_module, llvm::Intrinsic::ID(Intrinsic::sadd_with_overflow), { IntptrTy });
        auto rs = IRB.CreateCall(intr, { fp_0, IRB.CreateSub(fp_1, VALUE_INTPTR(1)) });
        auto ans = IRB.CreateExtractValue(rs, { 0 });
        auto overflow = IRB.CreateExtractValue(rs, { 1 });
        auto ans_valid_cond = IRB.CreateICmpEQ(overflow, IRB.getInt1(false));
        BasicBlock* ans_valid_true = BasicBlock::Create(C, "ans_valid_true", F);
        IRB.CreateCondBr(ans_valid_cond, ans_valid_true, FALLBACK);
        IRB.SetInsertPoint(ans_valid_true);
        CREATE_STORE_VM_REG(vm, m_value, ans);
        IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
    } else {
        IRB.CreateBr(FALLBACK);
    }

    IRB.SetInsertPoint(FALLBACK);

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
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_back_to_loop));
}
*/

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
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_back_to_loop));
}

void
codegen_t::emit_push_subr(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

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
codegen_t::emit_iloc0(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto val = IRB.CreateLoad(emit_lookup_iloc(ctx, 0, FIXNUM(operands)));
    CREATE_STORE_VM_REG(vm, m_value, val);

    BasicBlock* undef_true = BasicBlock::Create(C, "undef_true", F);
    BasicBlock* undef_false = BasicBlock::Create(C, "undef_false", F);
    auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
    IRB.CreateCondBr(undef_cond, undef_true, undef_false);
    // invalid
    IRB.SetInsertPoint(undef_true);
    auto c_letrec_violation = M->getOrInsertFunction("c_letrec_violation", VoidTy, IntptrPtrTy);
    IRB.CreateCall(c_letrec_violation, {vm});
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_back_to_loop));
    // valid
    IRB.SetInsertPoint(undef_false);
}

void
codegen_t::emit_lt_n_iloc(context_t& ctx, scm_obj_t inst)
{
    DECLEAR_CONTEXT_VARS;
    DECLEAR_COMMON_TYPES;
    scm_obj_t operands = CDAR(inst);
    auto vm = F->arg_begin();

    auto val = IRB.CreateLoad(emit_lookup_iloc(ctx, CAR(operands)));
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
        auto c_real_pred = M->getOrInsertFunction("c_real_pred", IntptrTy, IntptrTy);
        BasicBlock* nonreal_true = BasicBlock::Create(C, "nonreal_true", F);
        BasicBlock* nonreal_false = BasicBlock::Create(C, "nonreal_false", F);
        auto nonreal_cond = IRB.CreateICmpEQ(IRB.CreateCall(c_real_pred, {val}), VALUE_INTPTR(0));
        IRB.CreateCondBr(nonreal_cond, nonreal_true, nonreal_false);
        // not real
        IRB.SetInsertPoint(nonreal_true);
            auto c_error_lt_n_iloc = M->getOrInsertFunction("c_error_lt_n_iloc", VoidTy, IntptrPtrTy, IntptrTy, IntptrTy);
            IRB.CreateCall(c_error_lt_n_iloc, {vm, val, VALUE_INTPTR(CADR(operands))});
            IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_back_to_loop));
        // real
        IRB.SetInsertPoint(nonreal_false);
            auto c_n_compare = M->getOrInsertFunction("c_n_compare", IntptrTy, IntptrPtrTy, IntptrTy, IntptrTy);
            auto taken_cond = IRB.CreateICmpSLT(IRB.CreateCall(c_n_compare, {vm, val, VALUE_INTPTR(CADR(operands))}), VALUE_INTPTR(0));
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
