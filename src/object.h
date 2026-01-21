// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef OBJECT_H_INCLUDED
#define OBJECT_H_INCLUDED

#include "core.h"

#if defined(NDEBUG)
  #define USE_TBI 1
#else
  #define USE_TBI 0
#endif

/*

|<                               fixnum 63bit                                 >1| fixnum
|---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- .000| cons pointer
|-< tc6 >- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- .010| tc6 heap object pointer
|<                               61bit flonum                               >100| short flonum
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
constexpr uintptr_t tc6_bignum = 2;
constexpr uintptr_t tc6_long_flonum = 3;
constexpr uintptr_t tc6_cont = 4;
constexpr uintptr_t tc6_closure = 5;
constexpr uintptr_t tc6_subr = 6;
constexpr uintptr_t tc6_vector = 7;
constexpr uintptr_t tc6_port = 8;
constexpr uintptr_t tc6_values = 9;
constexpr uintptr_t tc6_hashtable = 10;
constexpr uintptr_t tc6_gloc = 11;
constexpr uintptr_t tc6_tuple = 12;
constexpr uintptr_t tc6_weakhashtable = 13;
constexpr uintptr_t tc6_u8vector = 14;
constexpr uintptr_t tc6_complex = 15;
constexpr uintptr_t tc6_rational = 16;
constexpr uintptr_t tc6_heapenv = 17;
constexpr uintptr_t tc6_heapcont = 18;
constexpr uintptr_t tc6_weakmapping = 19;
constexpr uintptr_t tc6_environment = 20;
constexpr uintptr_t tc6_socket = 21;

inline scm_obj_t singleton(uintptr_t val) { return (val << 4) | 0x06; }

const scm_obj_t scm_true = singleton(2);
const scm_obj_t scm_false = singleton(3);
const scm_obj_t scm_nil = singleton(4);
const scm_obj_t scm_undef = singleton(5);
const scm_obj_t scm_unspecified = singleton(6);
const scm_obj_t scm_eof = singleton(7);
const scm_obj_t scm_timeout = singleton(8);
const scm_obj_t scm_shutdown = singleton(9);
const scm_obj_t scm_hash_free = singleton(10);
const scm_obj_t scm_hash_deleted = singleton(11);
const scm_obj_t scm_proc_apply = singleton(12);
const scm_obj_t scm_proc_callcc = singleton(13);
const scm_obj_t scm_proc_apply_values = singleton(14);

struct scm_cons_rec_t {
  scm_obj_t car;
  scm_obj_t cdr;
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

struct scm_u8vector_rec_t {
  scm_tc6_t tag;
  uint8_t* elts;
  int nsize;
};

inline bool is_heap_pointer(scm_obj_t x) { return (x & 0x07) == 0x02; }

inline bool is_tc6(scm_obj_t x, uintptr_t tc6) {
#if USE_TBI
  uint64_t bits = __builtin_rotateleft64(x, 7);
  return (bits & 0x1bf) == (0x100 + tc6);
#else
  if (!is_heap_pointer(x)) return false;
  scm_tc6_t tag = *(scm_tc6_t*)(x & ~0x07);
  return ((tag >> 8) & 0x3f) == tc6;
#endif
}

inline void* to_address(scm_obj_t x) {
  assert((x & 0x07) == 0x02);
  return (void*)(x & ~0x07);
}

inline bool is_cons(scm_obj_t x) { return (x & 0x07) == 0x00; }
inline bool is_fixnum(scm_obj_t x) { return (x & 0x01) == 0x01; }
inline bool is_char(scm_obj_t x) { return (x & 0x17) == 0x16; }
inline bool is_short_flonum(scm_obj_t x) { return (x & 0x07) == 0x04; }
inline bool is_long_flonum(scm_obj_t x) { return is_tc6(x, tc6_long_flonum); }
inline bool is_symbol(scm_obj_t x) { return is_tc6(x, tc6_symbol); }
inline bool is_string(scm_obj_t x) { return is_tc6(x, tc6_string); }
inline bool is_vector(scm_obj_t x) { return is_tc6(x, tc6_vector); }
inline bool is_u8vector(scm_obj_t x) { return is_tc6(x, tc6_u8vector); }
inline scm_obj_t make_fixnum(int64_t i64) { return (i64 << 1) | 0x01; }
inline scm_obj_t make_char(uintptr_t ucs4) { return (ucs4 << 32) | 0x16; }

scm_obj_t make_cons(scm_obj_t car, scm_obj_t cdr);
scm_obj_t make_list(int len, ...);
scm_obj_t make_flonum(double d);
scm_obj_t make_symbol(const char* name);
scm_obj_t make_string(const char* name);
scm_obj_t make_vector(int nsize, scm_obj_t init);
scm_obj_t make_u8vector(int nsize);

inline intptr_t fixnum(scm_obj_t x) { return ((intptr_t)x >> 1); }
double flonum(scm_obj_t x);
uint8_t* symbol_name(scm_obj_t x);
uint8_t* string_name(scm_obj_t x);

inline int vector_nsize(scm_obj_t x) { return ((scm_vector_rec_t*)to_address(x))->nsize; }
inline scm_obj_t* vector_elts(scm_obj_t x) { return ((scm_vector_rec_t*)to_address(x))->elts; }
inline int u8vector_nsize(scm_obj_t x) { return ((scm_u8vector_rec_t*)to_address(x))->nsize; }
inline uint8_t* u8vector_elts(scm_obj_t x) { return ((scm_u8vector_rec_t*)to_address(x))->elts; }

#endif
