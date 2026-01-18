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
|---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- .000| cons (pointer)
|-< tc6 >- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- .010| heap object (pointer)
|<                               61bit flonum                               >100| short flonum (immediate)
|.... .... .... .... .... .... .... .... .... .... .... .... ..< tc6 > 0000 .110| heap tag (immediate)
|<               ucs4                  > .... .... .... .... .... .... 0001 .110| char (immediate)
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

|.... .... .... .... .... .... .... .... .... .... .... .... .000 000. 0000 .110| heap tag: symbol
|.... .... .... .... .... .... .... .... .... .... .... .... .000 001. 0000 .110| heap tag: string
|.... .... .... .... .... .... .... .... .... .... .... .... .000 010. 0000 .110| heap tag: bignum

*/

typedef void* scm_obj_t;
typedef uintptr_t scm_hdr_t;
typedef scm_obj_t scm_char_t;
typedef scm_obj_t scm_fixnum_t;
typedef scm_obj_t scm_short_flonum_t;
typedef scm_obj_t scm_long_flonum_t;

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
constexpr uintptr_t tc6_bvector = 14;
constexpr uintptr_t tc6_complex = 15;
constexpr uintptr_t tc6_rational = 16;
constexpr uintptr_t tc6_heapenv = 17;
constexpr uintptr_t tc6_heapcont = 18;
constexpr uintptr_t tc6_weakmapping = 19;
constexpr uintptr_t tc6_environment = 20;
constexpr uintptr_t tc6_socket = 21;

inline scm_obj_t singleton(uintptr_t val) { return (scm_obj_t)((val << 4) | 0x06); }

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

struct scm_pair_rec_t {
  scm_obj_t car;
  scm_obj_t cdr;
};

struct scm_long_flonum_rec_t {
  scm_hdr_t hdr;
  double value;
};

inline bool is_tc6(scm_obj_t x, uintptr_t tc6) {
#if USE_TBI
  uint64_t bits = __builtin_rotateleft64((uintptr_t)x, 7);
  return (bits & 0x1bf) == (0x100 + tc6);
#else
  if (((uintptr_t)x & 0x07) != 0x02) return false;
  scm_hdr_t hdr = *(scm_hdr_t*)((uintptr_t)x & ~0x07);
  return ((hdr >> 8) & 0x3f) == tc6;
#endif
}

inline bool is_fixnum(scm_obj_t x) { return ((uintptr_t)x & 0x01) == 0x01; }
inline bool is_char(scm_obj_t x) { return ((uintptr_t)x & 0x17) == 0x16; }
inline bool is_short_flonum(scm_obj_t x) { return ((uintptr_t)x & 0x07) == 0x04; }
inline bool is_long_flonum(scm_obj_t x) { return is_tc6(x, tc6_long_flonum); }

inline scm_fixnum_t make_fixnum(int64_t i64) { return (scm_fixnum_t)((i64 << 1) | 0x01); }
inline scm_char_t make_char(uintptr_t ucs4) { return (scm_char_t)((ucs4 << 32) | 0x16); }

scm_obj_t make_flonum(double d);
double flonum(scm_obj_t x);

inline intptr_t fixnum(scm_obj_t x) { return ((intptr_t)x >> 1); }

#endif
