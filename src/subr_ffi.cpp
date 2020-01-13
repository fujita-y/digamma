// Copyright (c) 2004-2020 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "vm.h"
#include "ffi.h"
#include "file.h"
#include "heap.h"
#include "subr.h"
#include "arith.h"
#include "violation.h"

#include <iostream>
#include <cstdint>
#include <string>

#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/IR/Verifier.h"

using namespace llvm;

#define FFI_C_TYPE_VOID            0x0000
#define FFI_C_TYPE_BOOL            0x0001
#define FFI_C_TYPE_SHORT           0x0002
#define FFI_C_TYPE_INT             0x0003
#define FFI_C_TYPE_INTPTR          0x0004
#define FFI_C_TYPE_USHORT          0x0005
#define FFI_C_TYPE_UINT            0x0006
#define FFI_C_TYPE_UINTPTR         0x0007
#define FFI_C_TYPE_FLOAT           0x0008
#define FFI_C_TYPE_DOUBLE          0x0009
#define FFI_C_TYPE_STRING          0x000a
#define FFI_C_TYPE_SIZE_T          0x000b
#define FFI_C_TYPE_INT8_T          0x000c
#define FFI_C_TYPE_UINT8_T         0x000d
#define FFI_C_TYPE_INT16_T         0x000e
#define FFI_C_TYPE_UINT16_T        0x000f
#define FFI_C_TYPE_INT32_T         0x0010
#define FFI_C_TYPE_UINT32_T        0x0011
#define FFI_C_TYPE_INT64_T         0x0012
#define FFI_C_TYPE_UINT64_T        0x0013
#define FFI_C_TYPE_MASK            0x00ff

// load-shared-object
scm_obj_t
subr_load_shared_object(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        void* hdl = load_shared_object(NULL);
        if (hdl) return uintptr_to_integer(vm->m_heap, (uintptr_t)hdl);
        invalid_argument_violation(vm, "load-shared-object", last_shared_object_error(), NULL, -1, argc, argv);
        return scm_undef;
    }
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            void* hdl = load_shared_object(string);
            if (hdl) return uintptr_to_integer(vm->m_heap, (uintptr_t)hdl);
            invalid_argument_violation(vm, "load-shared-object", last_shared_object_error(), NULL, -1, argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "load-shared-object", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "load-shared-object", 0, 1, argc, argv);
    return scm_undef;
}

// lookup-shared-object
scm_obj_t
subr_lookup_shared_object(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        void* hdl;
        if (exact_positive_integer_pred(argv[0])) {
            if (exact_integer_to_uintptr(argv[0], (uintptr_t*)&hdl) == false) {
                invalid_argument_violation(vm, "lookup-shared-object", "value out of bound,", argv[0], 0, argc, argv);
                return scm_undef;
            }
        } else {
            wrong_type_argument_violation(vm, "lookup-shared-object", 0, "shared object handle", argv[0], argc, argv);
            return scm_undef;
        }
        if (STRINGP(argv[1]) || SYMBOLP(argv[1])) {
            uintptr_t adrs = (uintptr_t)lookup_shared_object(hdl, argv[1]);
            if (adrs == 0) return scm_false;
            return uintptr_to_integer(vm->m_heap, adrs);
        }
        wrong_type_argument_violation(vm, "lookup-shared-object", 1, "string or symbol", argv[1], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "lookup-shared-object", 2, 2, argc, argv);
    return scm_undef;
}

// call-shared-object
scm_obj_t
subr_call_shared_object(VM* vm, int argc, scm_obj_t argv[])
{
    llvm::InitializeNativeTarget();

    /* LLVM Global Context */
    llvm::LLVMContext ctx;
    /* Module */
    Module * module = new Module("MyModule", ctx);

    /* IRBuilder */
    IRBuilder<> builder(ctx);

    /* Create execution engine */
    std::string err_str;
    ExecutionEngine * engine = EngineBuilder(module).setErrorStr(&err_str).create();
    if (!engine) {
        std::cerr << "Could not create ExecutionEngine:"
                  << err_str << std::endl;
        return scm_undef;
    }

    /* Type of value I want to return - 64-bit integer */
    IntegerType * int64_ty = Type::getInt64Ty(ctx);

    /* Actual value to return */
    int64_t value = 765;

    /* LLVM Value */
    Value * v = ConstantInt::get(int64_ty, value);

    /* Create prototype of result function - int64_t() */
    std::vector<Type *> arguments;
    FunctionType * proto = FunctionType::get(int64_ty, arguments, false);

    /* Create function body */
    Function * fce = Function::Create(proto, Function::ExternalLinkage, "MyFunction", module);
    BasicBlock * bb = BasicBlock::Create(ctx, "entry", fce);

    /* Point builder to add instructions to function body */
    builder.SetInsertPoint(bb);

    /* Return value */
    builder.CreateRet(v);

    /* Function pass manager */
    FunctionPassManager pass_manager(module);

    /* Add data layout */
    pass_manager.add(new DataLayout(*engine->getDataLayout()));
    pass_manager.doInitialization();

    /* Do optimizations */
    pass_manager.run(*fce);

    /* JIT it */
    void * fce_ptr = engine->getPointerToFunction(fce);
    int64_t (*fce_typed_ptr)() = (int64_t(*)())fce_ptr;

    /* Excute JITed function */
    std::cout << "Result: " << (*fce_typed_ptr)()
              << std::endl << std::endl;

    /* Dump LLVM IR */
    std::cout << "LLVM IR dump: " << std::endl;
    module->dump();



//raise_error(vm,"call-shared-object","implementationdoesnotsupportthisfeature",0,argc,argv);
return scm_undef;
}

void init_subr_ffi(object_heap_t* heap)
{
    #define DEFSUBR(SYM, FUNC)  heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("load-shared-object", subr_load_shared_object);
    DEFSUBR("lookup-shared-object", subr_lookup_shared_object);
    DEFSUBR("call-shared-object", subr_call_shared_object);
}

/*
objdump -p digamma

(define libm.so (load-shared-object "libm.so"))
(lookup-shared-object libm.so "cos")

(define libc.so (load-shared-object "libc.so.6"))
(lookup-shared-object libc.so "puts")
(call-shared-object (lookup-shared-object libc.so "puts"))
*/
