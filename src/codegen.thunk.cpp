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
        vm->m_heap->write_barrier(vm->m_value);
        gloc->value = vm->m_value;
        return 0;
    }

    intptr_t c_set_iloc(VM* vm, scm_closure_t operands) {
        scm_obj_t loc = CAR(operands);
        scm_obj_t* slot = c_lookup_iloc(vm, FIXNUM(CAR(loc)), FIXNUM(CDR(loc)));
        if (!STACKP(slot)) {
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

    intptr_t c_push_nadd_iloc(VM* vm, scm_obj_t operands) {
        scm_obj_t loc = CAR(operands);
        scm_obj_t obj = *c_lookup_iloc(vm, FIXNUM(CAR(loc)), FIXNUM(CDR(loc)));
        if (number_pred(obj)) {
            vm->m_sp[0] = arith_add(vm->m_heap, obj, CADR(operands));
            vm->m_sp++;
            return 0;
        }
        scm_obj_t argv[2] = { obj, CADR(operands) };
        wrong_type_argument_violation(vm, "operator(+ -)", 0, "number", argv[0], 2, argv);
        return 1;
    }

    intptr_t c_nadd_iloc(VM* vm, scm_obj_t operands) {
        scm_obj_t loc = CAR(operands);
        scm_obj_t obj = *c_lookup_iloc(vm, FIXNUM(CAR(loc)), FIXNUM(CDR(loc)));
        if (number_pred(obj)) {
            vm->m_value = arith_add(vm->m_heap, obj, CADR(operands));
            return 0;
        }
        scm_obj_t argv[2] = { obj, CADR(operands) };
        wrong_type_argument_violation(vm, "operator(+ -)", 0, "number", argv[0], 2, argv);
        return 1;
    }
}
