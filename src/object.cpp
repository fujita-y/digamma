// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "concurrent_slab.h"
#include "object_heap.h"

constexpr int short_flonum_tag_shift = 3;
constexpr uint64_t short_flonum_tag = 0x4;
constexpr uint64_t short_flonum_tag_mask = 0x7;
constexpr uint64_t short_flonum_offset = 0x7000000000000000ULL;

inline scm_hdr_t* hdr_address(scm_obj_t x) { return (scm_hdr_t*)((uintptr_t)x & ~0x07); }
inline uint64_t hdr_tc6(uintptr_t tc6) { return (tc6 << 8) | 0x06; }

inline scm_obj_t tag_heap_object(void* x, uintptr_t tc6) {
#if USE_TBI
  return (scm_obj_t)((uintptr_t)x | 0x02 | tc6 << 57);
#else
  return (scm_obj_t)((uintptr_t)x | 0x02);
#endif
}

static uint64_t double_to_short_flonum(double d) {
  constexpr uint64_t positive_zero = short_flonum_tag;
  constexpr uint64_t negative_zero = short_flonum_tag | (1 << short_flonum_tag_shift);
  uint64_t val = __builtin_bit_cast(uint64_t, d);
  int exp = (val >> 52) & 0x7FF;
  if (exp == 0) {
    return ((intptr_t)val < 0) ? negative_zero : positive_zero;
  }
  if (exp < 896 || exp > 1151) {
    return 0;  // out of range
  }
  uint64_t rot = __builtin_rotateleft64(val, 1);
  rot -= short_flonum_offset;
  if (rot == 0) return 0;
  return (rot << short_flonum_tag_shift) + short_flonum_tag;
}

static double short_flonum_to_double(uint64_t u64) {
  assert((u64 & short_flonum_tag_mask) == short_flonum_tag);
  uint64_t val = u64 >> short_flonum_tag_shift;
  if (val == 0) return 0.0;
  if (val == 1) return -0.0;
  val += short_flonum_offset;
  uint64_t res = __builtin_rotateright64(val, 1);
  return __builtin_bit_cast(double, res);
}

scm_obj_t make_flonum(double d) {
  uint64_t u64 = double_to_short_flonum(d);
  if (u64 == 0) {
    void* obj = object_heap_t::current()->m_flonums.new_object();
    ((scm_long_flonum_rec_t*)obj)->hdr = hdr_tc6(tc6_long_flonum);
    ((scm_long_flonum_rec_t*)obj)->value = d;
    return tag_heap_object(obj, tc6_long_flonum);
  }
  return (scm_obj_t)u64;
}

double flonum(scm_obj_t x) {
  if (is_short_flonum(x)) {
    return short_flonum_to_double((uint64_t)x);
  }
  if (is_long_flonum(x)) {
    scm_hdr_t* hdr = hdr_address(x);
    return ((scm_long_flonum_rec_t*)hdr)->value;
  }
  fatal("%s:%u internal error: flonum expected.", __FILE__, __LINE__);
}
