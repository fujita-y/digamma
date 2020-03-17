// Copyright (c) 2004-2020 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "vm.h"
#include "printer.h"
#include "violation.h"
#include "codegen.h"

/*
 (current-environment (system-environment)) (native-compile)
*/

// native-compile
/*
scm_obj_t
subr_native_compile(VM* vm, int argc, scm_obj_t argv[])
{
//    orcjit_compile();
    return scm_unspecified;
}
*/
/*
 (current-environment (system-environment)) (define (foo) 120) (closure-compile foo)
*/

// closure-compile
scm_obj_t
subr_closure_compile(VM* vm, int argc, scm_obj_t argv[])
{
#if ENABLE_LLVM_JIT
    if (argc == 1) {
        if (CLOSUREP(argv[0])) {
            scm_closure_t closure = (scm_closure_t)argv[0];
            if (vm->m_codegen) {
                vm->m_codegen->compile(closure);
                return scm_unspecified;
            } else {
                implementation_restriction_violation(vm, "closure-compile", "not available on this vm", MAKEFIXNUM(vm->m_id), argc, argv);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "closure-compile", 0, "closure", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "closure-compile", 1, 1, argc, argv);
    return scm_undef;
#else
    implementation_restriction_violation(vm, "closure-compile", "not available on this vm", MAKEFIXNUM(vm->m_id), argc, argv);
    return scm_undef;
#endif
}

// display-codegen-statistic
scm_obj_t
subr_display_codegen_statistics(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        if (vm->m_codegen) {
            vm->m_codegen->display_codegen_statistics(vm->m_current_output);
            return scm_unspecified;
        } else {
            implementation_restriction_violation(vm, "display-codegen-statistic", "codegen not available on this vm", MAKEFIXNUM(vm->m_id), argc, argv);
            return scm_undef;
        }
    }
    wrong_number_of_arguments_violation(vm, "display-codegen-statistic", 0, 0, argc, argv);
    return scm_undef;
}

void
init_subr_codegen(object_heap_t* heap)
{
#define DEFSUBR(SYM, FUNC)  heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("display-codegen-statistic", subr_display_codegen_statistics);
    DEFSUBR("closure-compile", subr_closure_compile);
}
