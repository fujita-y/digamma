// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef OBJECT_H_INCLUDED
#define OBJECT_H_INCLUDED

#include "core.h"
#include "arch_arm64.h"

#include <sanitizer/hwasan_interface.h>
#include <ucontext.h>
#include <variant>
/*

|<                                fixnum 63bit                                >1| fixnum
|---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- .000| cons pointer
|-< tc6 >- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- .010| tc6 heap object pointer
|<                                flonum 61bit                              >100| short flonum
|.... .... .... .... .... .... .... .... .... .... .... .... ..< tc6 > 0000 .110| tc6 heap object tag
|.... .... .... .... .... .... .... .... .... .... .... .... ..00 0000 0000 .110| tc6: symbol
|.... .... .... .... .... .... .... .... .... .... .... .... ..00 0001 0000 .110| tc6: string
|.... .... .... .... .... .... .... .... .... .... .... .... ..00 0010 0000 .110| tc6: bignum
|<               ucs4                  > .... .... .... .... .... .... 0001 .110| char
|.... .... .... .... .... .... .... .... .... .... .... .... .... .... 0010 .110| scm_true
|.... .... .... .... .... .... .... .... .... .... .... .... .... .... 0011 .110| scm_false
|.... .... .... .... .... .... .... .... .... .... .... .... .... .... 0100 .110| scm_nil
|.... .... .... .... .... .... .... .... .... .... .... .... .... .... 0101 .110| scm_undef
|.... .... .... .... .... .... .... .... .... .... .... .... .... .... 0110 .110| scm_unspecified
|.... .... .... .... .... .... .... .... .... .... .... .... .... .... 0111 .110| scm_eof
|.... .... .... .... .... .... .... .... .... .... .... .... .... .... 1000 .110| scm_hash_free
|.... .... .... .... .... .... .... .... .... .... .... .... .... .... 1001 .110| scm_hash_used
|.... .... .... .... .... .... .... .... .... .... .... .... .... .... 1010 .110| scm_proc_apply
|.... .... .... .... .... .... .... .... .... .... .... .... .... .... 1011 .110| scm_proc_callcc
|.... .... .... .... .... .... .... .... .... .... .... .... .... .... 1100 .110| scm_proc_apply_values

*/

typedef uintptr_t scm_obj_t;
typedef uintptr_t scm_tc6_t;

constexpr uintptr_t tc6_symbol = 0;
constexpr uintptr_t tc6_string = 1;
constexpr uintptr_t tc6_long_flonum = 2;
constexpr uintptr_t tc6_vector = 3;
constexpr uintptr_t tc6_values = 4;
constexpr uintptr_t tc6_u8vector = 5;
constexpr uintptr_t tc6_hashtable = 6;
constexpr uintptr_t tc6_closure = 7;
constexpr uintptr_t tc6_environment = 8;
constexpr uintptr_t tc6_cell = 9;
constexpr uintptr_t tc6_escape = 10;
constexpr uintptr_t tc6_continuation = 11;
constexpr uintptr_t tc6_port = 12;
/*
constexpr uintptr_t tc6_subr = ;
constexpr uintptr_t tc6_continuation = ;
constexpr uintptr_t tc6_bignum = ;
constexpr uintptr_t tc6_subr = ;
constexpr uintptr_t tc6_gloc = ;
constexpr uintptr_t tc6_tuple = ;
constexpr uintptr_t tc6_complex = ;
constexpr uintptr_t tc6_rational = ;
constexpr uintptr_t tc6_heapenv = ;
constexpr uintptr_t tc6_heapcont = ;
constexpr uintptr_t tc6_weakmapping = ;
constexpr uintptr_t tc6_weakhashtable = ;
constexpr uintptr_t tc6_socket = ;
*/

inline scm_obj_t singleton(uintptr_t val) {
  assert(val >= 2 && val <= 15);
  return (val << 4) | 0x06;
}

const scm_obj_t scm_true = singleton(2);
const scm_obj_t scm_false = singleton(3);
const scm_obj_t scm_nil = singleton(4);
const scm_obj_t scm_undef = singleton(5);
const scm_obj_t scm_unspecified = singleton(6);
const scm_obj_t scm_eof = singleton(7);
const scm_obj_t scm_hash_free = singleton(8);
const scm_obj_t scm_hash_deleted = singleton(9);
/*
const scm_obj_t scm_timeout = singleton(?);
const scm_obj_t scm_shutdown = singleton(?);
const scm_obj_t scm_proc_apply = singleton(?);
const scm_obj_t scm_proc_callcc = singleton(?);
const scm_obj_t scm_proc_apply_values = singleton(?);
*/

typedef unsigned int (*hash_proc_t)(scm_obj_t obj, unsigned int bound);
typedef bool (*equiv_proc_t)(scm_obj_t obj1, scm_obj_t obj2);

struct scm_cons_rec_t {
  scm_obj_t car;
  scm_obj_t cdr;
};

struct scm_cell_rec_t {
  scm_tc6_t tag;
  scm_obj_t value;
};

struct scm_long_flonum_rec_t {
  scm_tc6_t tag;
  double value;
};

struct scm_symbol_rec_t {
  scm_tc6_t tag;
  uint8_t* name;  // uninterned symbol contains <prefix-size> after '\0'
};

struct scm_string_rec_t {
  scm_tc6_t tag;
  uint8_t* name;
};

struct scm_vector_rec_t {
  scm_tc6_t tag;
  scm_obj_t* elts;
  int nsize;
};

struct scm_values_rec_t {
  scm_tc6_t tag;
  scm_obj_t* elts;
  int nsize;
};

struct scm_u8vector_rec_t {
  scm_tc6_t tag;
  uint8_t* elts;
  int nsize;
};

struct hashtable_aux_t {
  int capacity;
  int used;
  int live;
  scm_obj_t elts[1];  // [ key ... val ... ]
};

struct scm_hashtable_rec_t {
  scm_tc6_t tag;
  hash_proc_t hash;
  equiv_proc_t equiv;
  hashtable_aux_t* aux;
};

struct scm_closure_rec_t {
  scm_tc6_t tag;
  void* code;
  int argc;
  int rest;
  int cdecl;
  int nenv;
  scm_obj_t env[1];  // free variables
};

struct scm_environment_rec_t {
  scm_tc6_t tag;
  scm_obj_t name;       // symbol
  scm_obj_t variables;  // hashtable
  scm_obj_t macros;     // hashtable
};

struct scm_escape_rec_t {
  scm_tc6_t tag;
  scm_obj_t winders;
  scm_obj_t retval;
  ucontext_t* uctx;
  uint64_t sp;
  bool invoked;
};

struct scm_continuation_rec_t {
  scm_tc6_t tag;
  scm_obj_t winders;
  ucontext_t* uctx;
  uint8_t* stack_copy;
  uint8_t* shadow_copy;
  uint64_t stack_bottom;
  size_t stack_size;
};

struct port_aux_t {
  std::variant<std::monostate, std::ostream*, std::istream*, std::iostream*, std::ofstream*, std::ifstream*, std::fstream*, std::stringstream*>
      stream;
  bool owned;
};

struct scm_port_rec_t {
  scm_tc6_t tag;
  scm_obj_t name;
  port_aux_t* aux;
};

inline bool is_cons(scm_obj_t x) { return (x & 0x07) == 0x00; }
inline bool is_heap_object(scm_obj_t x) { return (x & 0x07) == 0x02; }

inline bool is_tc6(scm_obj_t x, uintptr_t tc6) {
#if USE_TBI
  uint64_t bits = __builtin_rotateleft64(x, 7);
  return (bits & 0x3bf) == (0x100 + tc6);
#else
  if (!is_heap_object(x)) return false;
  scm_tc6_t tag = *(scm_tc6_t*)(x - 2);
  return (tag & 0x3f00) == (tc6 << 8);
#endif
}

inline uintptr_t tag_tc6(scm_tc6_t tag) { return (tag & 0x3f00) >> 8; }

inline uintptr_t heap_tc6(scm_obj_t x) {
  assert(is_heap_object(x));
#if USE_TBI
  return __builtin_rotateleft64(x, 7) & 0x3f;
#else
  return tag_tc6(*(scm_tc6_t*)(x - 2));
#endif
}

inline void* to_address(scm_obj_t x) {
  assert(is_heap_object(x));
  return (void*)(x - 2);
}

inline bool is_fixnum(scm_obj_t x) { return (x & 0x01) == 0x01; }
inline bool is_char(scm_obj_t x) { return (x & 0xf7) == 0x16; }
inline bool is_short_flonum(scm_obj_t x) { return (x & 0x07) == 0x04; }
inline bool is_long_flonum(scm_obj_t x) { return is_tc6(x, tc6_long_flonum); }
inline bool is_symbol(scm_obj_t x) { return is_tc6(x, tc6_symbol); }
inline bool is_string(scm_obj_t x) { return is_tc6(x, tc6_string); }
inline bool is_vector(scm_obj_t x) { return is_tc6(x, tc6_vector); }
inline bool is_values(scm_obj_t x) { return is_tc6(x, tc6_values); }
inline bool is_u8vector(scm_obj_t x) { return is_tc6(x, tc6_u8vector); }
inline bool is_hashtable(scm_obj_t x) { return is_tc6(x, tc6_hashtable); }
inline bool is_closure(scm_obj_t x) { return is_tc6(x, tc6_closure); }
inline bool is_environment(scm_obj_t x) { return is_tc6(x, tc6_environment); }
inline bool is_cell(scm_obj_t x) { return is_tc6(x, tc6_cell); }
inline bool is_escape(scm_obj_t x) { return is_tc6(x, tc6_escape); }
inline bool is_continuation(scm_obj_t x) { return is_tc6(x, tc6_continuation); }
inline bool is_port(scm_obj_t x) { return is_tc6(x, tc6_port); }

inline scm_obj_t make_fixnum(int64_t i64) { return (i64 << 1) | 0x01; }
inline scm_obj_t make_char(uintptr_t ucs4) { return (ucs4 << 32) | 0x16; }

scm_obj_t make_cons(scm_obj_t car, scm_obj_t cdr);
scm_obj_t make_cell(scm_obj_t value);
scm_obj_t make_list(int len, ...);
scm_obj_t make_list2(scm_obj_t first, scm_obj_t second);
scm_obj_t make_flonum(double d);
scm_obj_t make_symbol(const char* name);
scm_obj_t make_uninterned_symbol(const char* name);
scm_obj_t make_string(const char* name);
scm_obj_t make_vector(int nsize, scm_obj_t init);
scm_obj_t make_values(int nsize);
scm_obj_t make_u8vector(int nsize);
scm_obj_t make_hashtable(hash_proc_t hash, equiv_proc_t equiv, int capacity);
scm_obj_t make_closure(void* code, int argc, int rest, int nsize, scm_obj_t env[], int cdecl);
scm_obj_t make_environment(scm_obj_t name);
scm_obj_t make_escape(ucontext_t* uctx, uintptr_t sp, scm_obj_t winders);
scm_obj_t make_continuation(ucontext_t* uctx, size_t stack_size, uint8_t* stack_copy, uint8_t* shadow_copy, uint64_t stack_bottom,
                            scm_obj_t winders);
scm_obj_t make_port(scm_obj_t name);

inline intptr_t fixnum(scm_obj_t x) {
  assert(is_fixnum(x));
  return ((intptr_t)x >> 1);
}

double flonum(scm_obj_t x);
uint8_t* symbol_name(scm_obj_t x);
uint8_t* string_name(scm_obj_t x);

scm_obj_t environment_variables(scm_obj_t x);
scm_obj_t environment_macros(scm_obj_t x);
uint8_t* environment_name(scm_obj_t x);

inline int vector_nsize(scm_obj_t x) {
  assert(is_vector(x));
  return ((scm_vector_rec_t*)to_address(x))->nsize;
}

inline scm_obj_t* vector_elts(scm_obj_t x) {
  assert(is_vector(x));
  return ((scm_vector_rec_t*)to_address(x))->elts;
}

inline int values_nsize(scm_obj_t x) {
  assert(is_values(x));
  return ((scm_values_rec_t*)to_address(x))->nsize;
}

inline scm_obj_t* values_elts(scm_obj_t x) {
  assert(is_values(x));
  return ((scm_values_rec_t*)to_address(x))->elts;
}

inline int u8vector_nsize(scm_obj_t x) {
  assert(is_u8vector(x));
  return ((scm_u8vector_rec_t*)to_address(x))->nsize;
}
inline uint8_t* u8vector_elts(scm_obj_t x) {
  assert(is_u8vector(x));
  return ((scm_u8vector_rec_t*)to_address(x))->elts;
}
inline scm_obj_t cell_value(scm_obj_t x) {
  assert(is_cell(x));
  return ((scm_cell_rec_t*)to_address(x))->value;
}

void cell_value_set(scm_obj_t x, scm_obj_t v);

inline int closure_argc(scm_obj_t x) {
  assert(is_closure(x));
  return ((scm_closure_rec_t*)to_address(x))->argc;
}

inline int closure_rest(scm_obj_t x) {
  assert(is_closure(x));
  return ((scm_closure_rec_t*)to_address(x))->rest;
}

inline int closure_nenv(scm_obj_t x) {
  assert(is_closure(x));
  return ((scm_closure_rec_t*)to_address(x))->nenv;
}

bool is_symbol_interned(scm_obj_t x);

inline scm_obj_t cons_car(scm_obj_t x) {
  assert(is_cons(x));
  return ((scm_cons_rec_t*)(x))->car;
}

inline scm_obj_t cons_cdr(scm_obj_t x) {
  assert(is_cons(x));
  return ((scm_cons_rec_t*)(x))->cdr;
}

std::string to_string(scm_obj_t obj);

#endif
