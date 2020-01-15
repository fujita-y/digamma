// Copyright (c) 2004-2020 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "vm.h"
#include <ffi.h>
#include "file.h"
#include "heap.h"
#include "subr.h"
#include "arith.h"
#include "violation.h"

#define FFI_RETURN_TYPE_VOID        0x0000
#define FFI_RETURN_TYPE_BOOL        0x0001
#define FFI_RETURN_TYPE_SHORT       0x0002
#define FFI_RETURN_TYPE_INT         0x0003
#define FFI_RETURN_TYPE_INTPTR      0x0004
#define FFI_RETURN_TYPE_USHORT      0x0005
#define FFI_RETURN_TYPE_UINT        0x0006
#define FFI_RETURN_TYPE_UINTPTR     0x0007
#define FFI_RETURN_TYPE_FLOAT       0x0008
#define FFI_RETURN_TYPE_DOUBLE      0x0009
#define FFI_RETURN_TYPE_STRING      0x000a
#define FFI_RETURN_TYPE_SIZE_T      0x000b
#define FFI_RETURN_TYPE_INT8_T      0x000c
#define FFI_RETURN_TYPE_UINT8_T     0x000d
#define FFI_RETURN_TYPE_INT16_T     0x000e
#define FFI_RETURN_TYPE_UINT16_T    0x000f
#define FFI_RETURN_TYPE_INT32_T     0x0010
#define FFI_RETURN_TYPE_UINT32_T    0x0011
#define FFI_RETURN_TYPE_INT64_T     0x0012
#define FFI_RETURN_TYPE_UINT64_T    0x0013
#define FFI_RETURN_TYPE_MASK        0x00ff

#define FFI_CALL_TYPE_STDCALL       0x0100
#define FFI_CALL_TYPE_MASK          0xff00

#define CALLBACK_RETURN_TYPE_INTPTR     0x0000
#define CALLBACK_RETURN_TYPE_INT64_T    0x0001
#define CALLBACK_RETURN_TYPE_FLOAT      0x0002
#define CALLBACK_RETURN_TYPE_DOUBLE     0x0003
#define CALLBACK_RETURN_TYPE_MASK       0x00ff
#define CALLBACK_CALL_TYPE_STDCALL      0x0100
#define CALLBACK_CALL_TYPE_MASK         0xff00

#define FFI_MAX_ARGC 32

class capture_errno {
    VM* m_vm;
public:
    capture_errno(VM* vm) {
        m_vm = vm;
        errno = m_vm->m_shared_object_errno;
    }
    ~capture_errno() {
        m_vm->m_shared_object_errno = errno;
    }
};

class c_arguments_t {
    union value_t {
        int8_t s8; int16_t s16; int32_t s32; int64_t s64;
        uint8_t u8; uint16_t u16; uint32_t u32; uint64_t u64;
        intptr_t ip; float f32; double f64;
    };
    int         m_argc;
    value_t*    m_argv[FFI_MAX_ARGC];
    ffi_type*   m_type[FFI_MAX_ARGC];
    value_t     m_value[FFI_MAX_ARGC];
public:
    c_arguments_t() : m_argc(0) {
        for (int i = 0; i < FFI_MAX_ARGC; i++) m_argv[i] = &m_value[i];
    }
    int argc() { return m_argc; }
    void** argv() { return (void**)m_argv; }
    ffi_type** types() { return m_type; }
    const char* add(VM* vm, scm_obj_t obj, int signature) {
        if (m_argc >= FFI_MAX_ARGC) fatal("fatal: c function argument count overflow");
        if (FIXNUMP(obj) || BIGNUMP(obj)) {
            if (signature == 'x') {
                m_type[m_argc] = &ffi_type_sint64;
                m_value[m_argc].s64 = coerce_exact_integer_to_int64(obj);
            } else if (signature == 'i' || signature == 'p' || signature == '*') {
                m_type[m_argc] = &ffi_type_pointer;
                m_value[m_argc].ip = coerce_exact_integer_to_intptr(obj);
            } else if (signature == 'f') {
                m_type[m_argc] = &ffi_type_float;
                m_value[m_argc].f32 = real_to_double(obj);
            } else if (signature == 'd') {
                m_type[m_argc] = &ffi_type_double;
                m_value[m_argc].f64 = real_to_double(obj);
            } else {
                goto bad_signature;
            }
        } else if (FLONUMP(obj)) {
            if (signature == 'f') {
                scm_flonum_t flonum = (scm_flonum_t)obj;
                m_type[m_argc] = &ffi_type_float;
                m_value[m_argc].f32 = flonum->value;
            } else if (signature == 'd' || signature == '*') {
                scm_flonum_t flonum = (scm_flonum_t)obj;
                m_type[m_argc] = &ffi_type_double;
                m_value[m_argc].f64 = flonum->value;
            } else {
                goto bad_signature;
            }
        } else if (BVECTORP(obj)) {
            if (signature != 'p' && signature != '*') goto bad_signature;
            scm_bvector_t bvector = (scm_bvector_t)obj;
            m_type[m_argc] = &ffi_type_pointer;
            m_value[m_argc].ip = (intptr_t)bvector->elts;
        } else if (VECTORP(obj)) {
            if (signature != 'c') goto bad_signature;
            scm_vector_t vector = (scm_vector_t)obj;
            int n = vector->count;
            if (n == 0) return "nonempty vector";
            assert(n);
            if (!FIXNUMP(vector->elts[0])) return "vector contains fixnum in first element";
            int ref = FIXNUM(vector->elts[0]);
            scm_bvector_t bvector = make_bvector(vm->m_heap, sizeof(intptr_t) * (n - 1));
            for (int i = 0; i < n - 1; i++) {
                if (BVECTORP(vector->elts[i + 1])) {
                    *(uint8_t**)(bvector->elts + sizeof(intptr_t) * i) = ((scm_bvector_t)vector->elts[i + 1])->elts;
                } else {
                    return "vector of bytevector";
                }
            }
            while (ref) {
                intptr_t datum = (intptr_t)bvector->elts;
                bvector = make_bvector(vm->m_heap, sizeof(intptr_t));
                *(intptr_t*)(bvector->elts) = datum;
                ref--;
            }
            m_type[m_argc] = &ffi_type_pointer;
            m_value[m_argc].ip = (intptr_t)bvector->elts;
        } else {
            goto bad_signature;
        }
        m_argc++;
        return NULL;

    bad_signature:
        switch (signature) {
            case 'i': case 'x': return "exact integer";
            case 'p': return "exact integer or bytevector";
            case 'c': return "vector";
            case 'f': case 'd': return "real";
            case '*': return "exact integer, real, or bytevector";
            default: return "invalid c function argument type";
        }
    }
};

/*
(import (rnrs) (digamma ffi))
(define libc (load-shared-object "libc.so.6")) ; Ubuntu 8.10
(define qsort
    (c-function libc "libc"
      void qsort (void* int int [c-callback int (void* void*)])))
(define comp
    (lambda (a1 a2)
      (display "[scheme proc invoked]") (newline)
      (let ((n1 (bytevector-u32-native-ref (make-bytevector-mapping a1 4) 0))
            (n2 (bytevector-u32-native-ref (make-bytevector-mapping a2 4) 0)))
        (cond ((= n1 n2) 0)
              ((> n1 n2) 1)
              (else -1)))))
(define nums (uint-list->bytevector '(10000 1000 10 100000 100) (native-endianness) 4))
(bytevector->uint-list nums (native-endianness) 4) ; => (10000 1000 10 100000 100)
(qsort nums 5 4 comp)
(bytevector->uint-list nums (native-endianness) 4) ; => (10 100 1000 10000 100000)
*/

class c_trampoline_t {
    int             m_argc;
    char*           m_signature;
    ffi_type*       m_type[FFI_MAX_ARGC];
    scm_closure_t   m_scm_closure;
    void*           m_address;
    int             m_ret_type;
    ffi_closure*    m_ffi_closure;
    ffi_cif         m_cif;
    VM*             m_vm;

    static void c_callback(ffi_cif* cif, void* ret, void* args[], void* context) {
        /*
        puts("callback");
        void* arg0 = args[0]; // arg0 point box that parameter is stored
        void* arg1 = args[1];
        printf("n0 %d\n", **(int**)arg0);
        printf("n1 %d\n", **(int**)arg1);
        *(int*)ret = **(int**)arg0 < **(int**)arg1;
*/

        c_trampoline_t* trampoline = (c_trampoline_t*)context;
        //printf("ret type %d\n", trampoline->m_ret_type);

        VM* vm = trampoline->m_vm;
        try {
            scm_obj_t* argv = (scm_obj_t*)alloca(sizeof(scm_obj_t*) * trampoline->m_argc);
            for (int i = 0; i < trampoline->m_argc; i++) {
                char c = trampoline->m_signature[i];
                void* arg = args[i];
                switch (c) {
                    case 'L': {
                        int8_t s8 = *(int8_t*)arg;
                        argv[i] = s8 ? MAKEFIXNUM(1) : MAKEFIXNUM(0);
                    } break;
                    case 'u': {
                        int8_t s8 = *(int8_t*)arg;
                        argv[i] = intptr_to_integer(vm->m_heap, s8);
                    } break;
                    case 'U': {
                        uint8_t u8 = *(uint8_t*)arg;
                        argv[i] = uintptr_to_integer(vm->m_heap, u8);
                    } break;
                    case 'b': {
                        int16_t s16 = *(int16_t*)arg;
                        argv[i] = intptr_to_integer(vm->m_heap, s16);
                    } break;
                    case 'B': {
                        uint16_t u16 = *(uint16_t*)arg;
                        argv[i] = uintptr_to_integer(vm->m_heap, u16);
                    } break;
                    case 'q': {
                        int32_t s32 = *(int32_t*)arg;
                        argv[i] = int32_to_integer(vm->m_heap, s32);
                    } break;
                    case 'Q': {
                        uint32_t u32 = *(uint32_t*)arg;
                        argv[i] = uint32_to_integer(vm->m_heap, u32);
                    } break;
                    case 'o': {
                        int64_t s64 = *(int64_t*)arg;
                        argv[i] = int64_to_integer(vm->m_heap, s64);
                    } break;
                    case 'O': {
                        uint64_t u64 = *(uint64_t*)arg;
                        argv[i] = uint64_to_integer(vm->m_heap, u64);
                    } break;
                    case 'f': {
                        float f32 = *(float*)arg;
                        argv[i] = make_flonum(vm->m_heap, f32);
                    } break;
                    case 'd': {
                        double f64 = *(double*)arg;
                        argv[i] = make_flonum(vm->m_heap, f64);
                    } break;

                    default: fatal("fatal: invalid callback argument signature %c\n[exit]\n", c);
                }
            }
            scm_obj_t ans = vm->call_scheme_argv(trampoline->m_scm_closure, trampoline->m_argc, argv);
            switch (trampoline->m_ret_type & CALLBACK_RETURN_TYPE_MASK) {
                case CALLBACK_RETURN_TYPE_INT64_T: {
                    if (exact_integer_pred(ans)) *(int64_t*)ret = coerce_exact_integer_to_int64(ans);
                    else *(int64_t*)ret = 0;
                } break;
                case CALLBACK_RETURN_TYPE_INTPTR: {
                    if (exact_integer_pred(ans)) *(intptr_t*)ret = coerce_exact_integer_to_intptr(ans);
                    else *(intptr_t*)ret = 0;
                } break;
                case CALLBACK_RETURN_TYPE_DOUBLE: {
                    if (real_valued_pred(ans)) *(double*)ret = real_to_double(ans);
                    else *(double*)ret = 0.0;
                } break;
                case CALLBACK_RETURN_TYPE_FLOAT: {
                    if (real_valued_pred(ans)) *(float*)ret = real_to_double(ans);
                    else *(float*)ret = 0.0;
                } break;

                default: fatal("fatal: invalid callback return type %d\n", trampoline->m_ret_type);
            }
           // *(int*)ret = FIXNUM(ans);
        } catch (vm_exit_t& e) {
            exit(e.m_code);
        } catch (...) {
            fatal("fatal: unhandled exception in callback\n[exit]\n");
        }
    }

public:
    c_trampoline_t(VM* vm, int ret_type, const char* signature, scm_closure_t closure)
        : m_vm(vm), m_ret_type(ret_type), m_scm_closure(closure), m_address(NULL), m_ffi_closure(NULL) {
        m_argc = strlen(signature);
        m_signature = new char[m_argc + 1];
        strncpy(m_signature, signature, m_argc + 1);
        for (int i = 0; i < m_argc; i++) {
            char c = m_signature[i];
            switch (c) {
                case 'L': {
                    m_type[i] = &ffi_type_sint8;
                } break;
                case 'u': {
                    m_type[i] = &ffi_type_sint8;
                } break;
                case 'U': {
                    m_type[i] = &ffi_type_uint8;
                } break;
                case 'b': {
                    m_type[i] = &ffi_type_sint16;
                } break;
                case 'B': {
                    m_type[i] = &ffi_type_uint16;
                } break;
                case 'q': {
                    m_type[i] = &ffi_type_sint32;
                } break;
                case 'Q': {
                    m_type[i] = &ffi_type_uint32;
                } break;
                case 'o': {
                    m_type[i] = &ffi_type_sint64;
                } break;
                case 'O': {
                    m_type[i] = &ffi_type_uint64;
                } break;
                case 'f': {
                    m_type[i] = &ffi_type_float;
                } break;
                case 'd': {
                    m_type[i] = &ffi_type_double;
                } break;

                default: fatal("fatal: invalid callback argument signature %c\n[exit]\n", c);
            }
        }
        ffi_type* ffi_ret_type = NULL;
        switch (m_ret_type & CALLBACK_RETURN_TYPE_MASK) {
            case CALLBACK_RETURN_TYPE_INT64_T: ffi_ret_type = &ffi_type_uint64; break;
            case CALLBACK_RETURN_TYPE_INTPTR: ffi_ret_type = &ffi_type_pointer; break;
            case CALLBACK_RETURN_TYPE_DOUBLE: ffi_ret_type = &ffi_type_double; break;
            case CALLBACK_RETURN_TYPE_FLOAT: ffi_ret_type = &ffi_type_float; break;

            default: fatal("fatal: invalid callback return type %d\n", m_ret_type);
        }
        m_ffi_closure = (ffi_closure*)ffi_closure_alloc(sizeof(ffi_closure), &m_address);
        if (m_ffi_closure) {
            if (ffi_prep_cif(&m_cif, FFI_DEFAULT_ABI, m_argc, ffi_ret_type, m_type) == FFI_OK) {
                if (ffi_prep_closure_loc(m_ffi_closure, &m_cif, c_callback, this, m_address) == FFI_OK) {
                    return;
                }
            }
        }
        fatal("%s:%u internal error: cannot prepare c callback trampoline",__FILE__ , __LINE__);
    }
    ~c_trampoline_t() {
        ffi_closure_free(m_ffi_closure);
        delete [] m_signature;
    }
    scm_obj_t address() {
        return uintptr_to_integer(m_vm->m_heap, (uintptr_t)m_address);
    }
};

static intptr_t
call_c_intptr(VM* vm, void* func, c_arguments_t& args)
{
    capture_errno sync(vm);
    ffi_cif cif;
    intptr_t rc;
    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, args.argc(), &ffi_type_pointer, args.types()) == FFI_OK) {
        ffi_call(&cif, FFI_FN(func), &rc, args.argv());
    } else {
        fatal("%s:%u internal error: cannot prepare c function call",__FILE__ , __LINE__);
    }
    return rc;
}

static int64_t
call_c_int64(VM* vm, void* func, c_arguments_t& args)
{
    capture_errno sync(vm);
    ffi_cif cif;
    int64_t rc;
    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, args.argc(), &ffi_type_sint64, args.types()) == FFI_OK) {
        ffi_call(&cif, FFI_FN(func), &rc, args.argv());
    } else {
        fatal("%s:%u internal error: cannot prepare c function call",__FILE__ , __LINE__);
    }
    return rc;
}

static float
call_c_float(VM* vm, void* func, c_arguments_t& args)
{
    capture_errno sync(vm);
    ffi_cif cif;
    float rc;
    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, args.argc(), &ffi_type_float, args.types()) == FFI_OK) {
        ffi_call(&cif, FFI_FN(func), &rc, args.argv());
    } else {
        fatal("%s:%u internal error: cannot prepare c function call",__FILE__ , __LINE__);
    }
    return rc;
}

static double
call_c_double(VM* vm, void* func, c_arguments_t& args)
{
    capture_errno sync(vm);
    ffi_cif cif;
    double rc;
    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, args.argc(), &ffi_type_double, args.types()) == FFI_OK) {
        ffi_call(&cif, FFI_FN(func), &rc, args.argv());
    } else {
        fatal("%s:%u internal error: cannot prepare c function call",__FILE__ , __LINE__);
    }
    return rc;
}

/*
(define libc (load-shared-object "libc.so.6"))
(define c-puts (lookup-shared-object libc "puts"))
(call-shared-object 0 c-puts #f "p" (string->utf8/nul "hello world!")) ;=> hello world!
(define c-strcat (lookup-shared-object libc "strcat"))
(call-shared-object 10 c-strcat #f "pp" (string->utf8/nul "foo!") (string->utf8/nul "bar!")) ;=> "foo!bar!""
*/

// call-shared-object
scm_obj_t
subr_call_shared_object(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc >= 4) {
        if (!FIXNUMP(argv[0])) {
            wrong_type_argument_violation(vm, "call-shared-object", 0, "fixnum", argv[0], argc, argv);
            return scm_undef;
        }
        int type = FIXNUM(argv[0]);
        void *func = NULL;
        if (exact_positive_integer_pred(argv[1])) {
            if (exact_integer_to_uintptr(argv[1], (uintptr_t*)&func) == false) {
                invalid_argument_violation(vm, "call-shared-object", "value out of bound,", argv[1], 1, argc, argv);
                return scm_undef;
            }
        } else {
            wrong_type_argument_violation(vm, "call-shared-object", 1, "c function address", argv[1], argc, argv);
            return scm_undef;
        }
        const char* who;
        if (SYMBOLP(argv[2])) {
            who = ((scm_symbol_t)argv[2])->name;
        } else if (argv[2] == scm_false) {
            who = "call-shared-object";
        } else {
            wrong_type_argument_violation(vm, "call-shared-object", 2, "symbol or #f", argv[2], argc, argv);
            return scm_undef;
        }
        const char* signature;
        if (STRINGP(argv[3])) {
            signature = ((scm_string_t)argv[3])->name;
        } else {
            wrong_type_argument_violation(vm, "call-shared-object", 3, "string", argv[3], argc, argv);
            return scm_undef;
        }
        if (argc - 4 <= FFI_MAX_ARGC) {
            c_arguments_t args;
            for (int i = 4; i < argc; i++) {
                const char* err = args.add(vm, argv[i], signature[0]);
                if (err) {
                    wrong_type_argument_violation(vm, who, i, err, argv[i], argc, argv);
                    return scm_undef;
                }
                if (signature[0] != '*') signature++;
            }
            switch (type & FFI_RETURN_TYPE_MASK) {
                case FFI_RETURN_TYPE_VOID: {
                    call_c_intptr(vm, func, args);
                    return scm_unspecified;
                }
                case FFI_RETURN_TYPE_STRING: {
                    char* p;
                    p = (char*)call_c_intptr(vm, func, args);
                    if (p == NULL) return MAKEFIXNUM(0);
                    return make_string(vm->m_heap, p);
                }
                case FFI_RETURN_TYPE_SIZE_T: {
                    if (sizeof(size_t) == sizeof(int)) {
                        unsigned int retval;
                        retval = (unsigned int)call_c_intptr(vm, func, args);
                        return uint_to_integer(vm->m_heap, retval);
                    }
                    uintptr_t retval;
                    retval = (uintptr_t)call_c_intptr(vm, func, args);
                    return uintptr_to_integer(vm->m_heap, retval);
                }
                case FFI_RETURN_TYPE_BOOL: {
                    return (call_c_intptr(vm, func, args) & 0xff) ? MAKEFIXNUM(1) : MAKEFIXNUM(0);
                }
                case FFI_RETURN_TYPE_SHORT: {
                    return int_to_integer(vm->m_heap, (short)call_c_intptr(vm, func, args));
                }
                case FFI_RETURN_TYPE_INT: {
                    return int_to_integer(vm->m_heap, (int)call_c_intptr(vm, func, args));
                }
                case FFI_RETURN_TYPE_INTPTR: {
                    return intptr_to_integer(vm->m_heap, call_c_intptr(vm, func, args));
                }
                case FFI_RETURN_TYPE_USHORT: {
                    return uint_to_integer(vm->m_heap, (unsigned short)call_c_intptr(vm, func, args));
                }
                case FFI_RETURN_TYPE_UINT: {
                    return uint_to_integer(vm->m_heap, (unsigned int)call_c_intptr(vm, func, args));
                }
                case FFI_RETURN_TYPE_UINTPTR: {
                    return uintptr_to_integer(vm->m_heap, (uintptr_t)call_c_intptr(vm, func, args));
                }
                case FFI_RETURN_TYPE_FLOAT: {
                    return make_flonum(vm->m_heap, call_c_float(vm, func, args));
                }
                case FFI_RETURN_TYPE_DOUBLE: {
                    return make_flonum(vm->m_heap, call_c_double(vm, func, args));
                }
                case FFI_RETURN_TYPE_INT8_T: {
                    return int_to_integer(vm->m_heap, (int8_t)call_c_intptr(vm, func, args));
                }
                case FFI_RETURN_TYPE_UINT8_T: {
                    return uint_to_integer(vm->m_heap, (uint8_t)call_c_intptr(vm, func, args));
                }
                case FFI_RETURN_TYPE_INT16_T: {
                    return int_to_integer(vm->m_heap, (int16_t)call_c_intptr(vm, func, args));
                }
                case FFI_RETURN_TYPE_UINT16_T: {
                    return uint_to_integer(vm->m_heap, (uint16_t)call_c_intptr(vm, func, args));
                }
                case FFI_RETURN_TYPE_INT32_T: {
                    return int_to_integer(vm->m_heap, (int32_t)call_c_intptr(vm, func, args));
                }
                case FFI_RETURN_TYPE_UINT32_T: {
                    return uint_to_integer(vm->m_heap, (uint32_t)call_c_intptr(vm, func, args));
                }
                case FFI_RETURN_TYPE_INT64_T: {
                    return int64_to_integer(vm->m_heap, (int64_t)call_c_int64(vm, func, args));
                }
                case FFI_RETURN_TYPE_UINT64_T: {
                    return uint64_to_integer(vm->m_heap, (uint64_t)call_c_int64(vm, func, args));
                }
            }
            invalid_argument_violation(vm, "call-shared-object", "invalid c function return type", argv[0], 0, argc, argv);
            return scm_undef;
        }
        invalid_argument_violation(vm, "call-shared-object", "too many arguments,", MAKEFIXNUM(argc), -1, argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "call-shared-object", 4, -1, argc, argv);
    return scm_undef;
}

// shared-object-errno
scm_obj_t
subr_shared_object_errno(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return int_to_integer(vm->m_heap, vm->m_shared_object_errno);
    if (argc == 1) {
        if (exact_integer_pred(argv[0])) {
            int val;
            if (exact_integer_to_int(argv[0], &val)) {
                errno = val;
                vm->m_shared_object_errno = val;
                return scm_unspecified;
            }
            invalid_argument_violation(vm, "shared-object-errno", "value out of range,", argv[0], 0, argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "shared-object-errno", 0, "exact integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "shared-object-errno", 0, 1, argc, argv);
    return scm_undef;
}

/*
void callback(ffi_cif* cif, void* ret, void* args[], void* context)
{
  puts("callback");
  void* arg0 = args[0]; // arg0 point box that parameter is stored
  void* arg1 = args[1];
  printf("n0 %d\n", **(int**)arg0);
  printf("n1 %d\n", **(int**)arg1);
  *(int*)ret = **(int**)arg0 < **(int**)arg1;
}

// call-shared-object
scm_obj_t
subr_call_shared_object(VM* vm, int argc, scm_obj_t argv[])
{

    ffi_cif ccif;
    ffi_type* cargs[2];
    void *address;
    ffi_closure* closure = (ffi_closure*)ffi_closure_alloc(sizeof(ffi_closure), &address);
    if (closure) {
        cargs[0] = &ffi_type_pointer;
        cargs[1] = &ffi_type_pointer;
        if (ffi_prep_cif(&ccif, FFI_DEFAULT_ABI, 2, &ffi_type_sint, cargs) == FFI_OK) {
            if (ffi_prep_closure_loc(closure, &ccif, callback, (void*)1234, address) == FFI_OK) {
                puts("closure created");
            }
        }
    }

    // (call-shared-object)

    int* base = (int*)malloc(sizeof(int) * 5);
    base[0] = 3;
    base[1] = 5;
    base[2] = 7;
    base[3] = 2;
    base[4] = 1;

    // void qsort (void* base, size_t num, size_t size, int (*comparator)(const void*, const void*));
    ffi_cif cif;
    ffi_type* args[4];
    void* values[4];
    int rc;
    int p1 = 5;
    int p2 = 4;
    args[0] = &ffi_type_pointer;
    values[0] = &base;
    args[1] = &ffi_type_sint;
    values[1] = &p1;
    args[2] = &ffi_type_sint;
    values[2] = &p2;
    args[3] = &ffi_type_pointer;
    values[3] = &address;
    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 4, &ffi_type_void, args) == FFI_OK) {
        puts("called");
        ffi_call(&cif, FFI_FN(qsort), &rc, values);
    } else {
        puts("error");
    }
    for (int i = 0; i < 5; i++) printf("%d ", base[i]);
    puts("");


    raise_error(vm, "call-shared-object", "implementation does not support this feature", 0, argc, argv);
    return scm_undef;
}
*/

// (import (digamma ffi))
// int (*comparator)(const void*, const void*))
// (define proc (lambda (n m) (> n m)))
// (make-cdecl-callback 'int '(void* void*) proc)


// make-callback-trampoline
scm_obj_t
subr_make_callback_trampoline(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 3) {
        if (exact_non_negative_integer_pred(argv[0])) {
            if (STRINGP(argv[1])) {
                const char* signature = ((scm_string_t)argv[1])->name;
                if (CLOSUREP(argv[2])) {
                    scoped_lock lock(vm->m_heap->m_trampolines->lock);
                    c_trampoline_t* trampoline = new c_trampoline_t(vm, FIXNUM(argv[0]), signature, (scm_closure_t)argv[2]);
                    return trampoline->address();
                    // return make_callback(vm, FIXNUM(argv[0]), signature, (scm_ffi_closure_t)argv[2]);
                }
                wrong_type_argument_violation(vm, "make-callback-trampoline", 2, "closure", argv[2], argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "make-callback-trampoline", 1, "string", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "make-callback-trampoline", 0, "exact non-negative integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-callback-trampoline", 3, 3, argc, argv);
    return scm_undef;
}

void init_subr_ffi(object_heap_t* heap)
{
    #define DEFSUBR(SYM, FUNC)  heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("call-shared-object", subr_call_shared_object);
    DEFSUBR("make-callback-trampoline", subr_make_callback_trampoline);
    DEFSUBR("shared-object-errno", subr_shared_object_errno);
}

/*
objdump -p digamma

(define libm.so (load-shared-object "libm.so"))
(lookup-shared-object libm.so "qsort")

(define libc.so (load-shared-object "libc.so.6"))
(lookup-shared-object libc.so "puts")
(call-shared-object (lookup-shared-object libc.so "puts"))
*/
