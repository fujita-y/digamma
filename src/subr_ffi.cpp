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

void callback(ffi_cif* cif, void* ret, void** args, void* context)
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
    ffi_call(&cif, (void(*)(void))qsort, &rc, values);
  } else {
      puts("error");
  }
  for (int i = 0; i < 5; i++) printf("%d ", base[i]);
  puts("");

/*

  ffi_cif cif;
  ffi_type* args[4];
  void* values[4];
  const char* s;
  int rc;

  args[0] = &ffi_type_pointer;
  values[0] = &s;

  if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 1, &ffi_type_uint, args) == FFI_OK) {
    s = "Hello World!";
    ffi_call(&cif, (void(*)(void))puts, &rc, values);
    s = "This is cool!";
    ffi_call(&cif, (void(*)(void))puts, &rc, values);
  }
*/
  raise_error(vm, "call-shared-object", "implementation does not support this feature", 0, argc, argv);
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
(lookup-shared-object libm.so "qsort")

(define libc.so (load-shared-object "libc.so.6"))
(lookup-shared-object libc.so "puts")
(call-shared-object (lookup-shared-object libc.so "puts"))
*/
