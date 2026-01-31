// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "object_heap.h"

inline scm_obj_t tc6_pointer(void* x, uintptr_t tc6_num) {
  assert(((uintptr_t)x & 0x07) == 0);
#if USE_TBI
  return (uintptr_t)x | 0x02 | tc6_num << 57;
#else
  return (uintptr_t)x | 0x02;
#endif
}

inline scm_tc6_t tc6_tag(uintptr_t tc6_num) { return (tc6_num << 8) | 0x06; }

constexpr int short_flonum_tag_shift = 3;
constexpr uint64_t short_flonum_tag = 0x4;
constexpr uint64_t short_flonum_tag_mask = 0x7;
constexpr uint64_t short_flonum_offset = 0x7000000000000000ULL;
constexpr uint64_t positive_zero = short_flonum_tag;
constexpr uint64_t negative_zero = short_flonum_tag | (1 << short_flonum_tag_shift);

static uint64_t double_to_short_flonum(double d) {
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
    scm_long_flonum_rec_t* obj = (scm_long_flonum_rec_t*)object_heap_t::current()->alloc_flonum();
    obj->tag = tc6_tag(tc6_long_flonum);
    obj->value = d;
    return tc6_pointer(obj, tc6_long_flonum);
  }
  return u64;
}

double flonum(scm_obj_t x) {
  if (is_short_flonum(x)) {
    return short_flonum_to_double((uint64_t)x);
  }
  if (is_long_flonum(x)) {
    scm_long_flonum_rec_t* rec = (scm_long_flonum_rec_t*)to_address(x);
    return rec->value;
  }
  fatal("%s:%u internal error: flonum expected.", __FILE__, __LINE__);
}

scm_obj_t make_symbol(const char* name) {
  scm_symbol_rec_t* obj = (scm_symbol_rec_t*)object_heap_t::current()->alloc_symbol();
  int n = strlen(name) + 1;
  uint8_t* datum = (uint8_t*)object_heap_t::current()->alloc_private(n);
  memcpy(datum, name, n);
  obj->tag = tc6_tag(tc6_symbol);
  obj->name = datum;
  return tc6_pointer(obj, tc6_symbol);
}

uint8_t* symbol_name(scm_obj_t x) {
  if (!is_symbol(x)) fatal("%s:%u internal error: symbol expected.", __FILE__, __LINE__);
  return ((scm_symbol_rec_t*)to_address(x))->name;
}

scm_obj_t make_string(const char* name) {
  scm_string_rec_t* obj = (scm_string_rec_t*)object_heap_t::current()->alloc_string();
  int n = strlen(name) + 1;
  uint8_t* datum = (uint8_t*)object_heap_t::current()->alloc_private(n);
  memcpy(datum, name, n);
  obj->tag = tc6_tag(tc6_string);
  obj->name = datum;
  return tc6_pointer(obj, tc6_string);
}

scm_obj_t make_vector(int nsize, scm_obj_t init) {
  scm_vector_rec_t* obj = (scm_vector_rec_t*)object_heap_t::current()->alloc_vector();
  obj->tag = tc6_tag(tc6_vector);
  obj->elts = (scm_obj_t*)object_heap_t::current()->alloc_private(nsize * sizeof(scm_obj_t));
  obj->nsize = nsize;
  for (int i = 0; i < nsize; i++) {
    obj->elts[i] = init;
  }
  return tc6_pointer(obj, tc6_vector);
}

scm_obj_t make_u8vector(int nsize) {
  scm_u8vector_rec_t* obj = (scm_u8vector_rec_t*)object_heap_t::current()->alloc_u8vector(nsize);
  uint8_t* elts = (uint8_t*)object_heap_t::current()->alloc_private(nsize * sizeof(uint8_t));
  memset(elts, 0, nsize * sizeof(uint8_t));
  obj->tag = tc6_tag(tc6_u8vector);
  obj->elts = elts;
  obj->nsize = nsize;
  return tc6_pointer(obj, tc6_u8vector);
}

scm_obj_t make_hash_table(hash_proc_t hash, equiv_proc_t equiv, int capacity) {
  scm_hashtable_rec_t* obj = (scm_hashtable_rec_t*)object_heap_t::current()->alloc_hashtable();
  obj->tag = tc6_tag(tc6_hashtable);
  obj->lock.init();
  obj->hash = hash;
  obj->equiv = equiv;
  size_t aux_size = sizeof(hashtable_aux_t) + sizeof(scm_obj_t) * ((capacity + capacity) - 1);
  obj->aux = (hashtable_aux_t*)object_heap_t::current()->alloc_private(aux_size);
  obj->aux->capacity = capacity;
  obj->aux->used = 0;
  obj->aux->live = 0;
  for (int i = 0; i < (capacity * 2); i++) obj->aux->elts[i] = scm_hash_free;
  return tc6_pointer(obj, tc6_hashtable);
}

uint8_t* string_name(scm_obj_t x) {
  if (!is_string(x)) fatal("%s:%u internal error: string expected.", __FILE__, __LINE__);
  return ((scm_string_rec_t*)to_address(x))->name;
}

scm_obj_t make_cons(scm_obj_t car, scm_obj_t cdr) {
  scm_cons_rec_t* obj = (scm_cons_rec_t*)object_heap_t::current()->alloc_cons();
  obj->car = car;
  obj->cdr = cdr;
  return (scm_obj_t)obj;
}

scm_obj_t make_list(int len, ...) {
  va_list ap;
  va_start(ap, len);
  if (len == 0) return scm_nil;
  scm_cons_rec_t* obj = (scm_cons_rec_t*)make_cons(va_arg(ap, scm_obj_t), scm_nil);
  scm_cons_rec_t* tail = obj;
  for (int i = 1; i < len; i++) {
    scm_cons_rec_t* e = (scm_cons_rec_t*)make_cons(va_arg(ap, scm_obj_t), scm_nil);
    tail->cdr = (scm_obj_t)e;
    tail = e;
  }
  va_end(ap);
  return (scm_obj_t)obj;
}
