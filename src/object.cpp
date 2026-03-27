// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "hash.h"
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
    object_heap_t& heap = *object_heap_t::current();
    scm_long_flonum_rec_t* rec = (scm_long_flonum_rec_t*)heap.alloc_flonum();
    rec->value = d;
    rec->tag = tc6_tag(tc6_long_flonum);
    return tc6_pointer(rec, tc6_long_flonum);
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
  object_heap_t& heap = *object_heap_t::current();
  std::lock_guard<std::mutex> lock(heap.m_symbol_table_mutex);
  auto it = heap.m_symbol_table.find(name);
  if (it != heap.m_symbol_table.end()) return it->second;
  scm_symbol_rec_t* rec = (scm_symbol_rec_t*)heap.alloc_symbol();
  int n = strlen(name) + 1;
  uint8_t* datum = (uint8_t*)heap.alloc_private(n);
  memcpy(datum, name, n);
  rec->tag = tc6_tag(tc6_symbol);
  rec->name = datum;
  scm_obj_t obj = tc6_pointer(rec, tc6_symbol);
  heap.m_symbol_table[name] = obj;
  return obj;
}

scm_obj_t make_uninterned_symbol(const char* name) {
  object_heap_t& heap = *object_heap_t::current();
  scm_symbol_rec_t* rec = (scm_symbol_rec_t*)heap.alloc_symbol();
  int n = strlen(name) + 1;
  uint8_t* datum = (uint8_t*)heap.alloc_private(n);
  memcpy(datum, name, n);
  rec->tag = tc6_tag(tc6_symbol);
  rec->name = datum;
  scm_obj_t obj = tc6_pointer(rec, tc6_symbol);
  return obj;
}

bool is_symbol_interned(scm_obj_t x) {
  if (!is_symbol(x)) fatal("%s:%u internal error: symbol expected.", __FILE__, __LINE__);
  object_heap_t& heap = *object_heap_t::current();
  std::lock_guard<std::mutex> lock(heap.m_symbol_table_mutex);
  auto it = heap.m_symbol_table.find((char*)symbol_name(x));
  return it != heap.m_symbol_table.end();
}

uint8_t* symbol_name(scm_obj_t x) {
  if (!is_symbol(x)) fatal("%s:%u internal error: symbol expected.", __FILE__, __LINE__);
  return ((scm_symbol_rec_t*)to_address(x))->name;
}

scm_obj_t make_string(const char* name) {
  object_heap_t& heap = *object_heap_t::current();
  scm_string_rec_t* rec = (scm_string_rec_t*)heap.alloc_string();
  int n = strlen(name) + 1;
  uint8_t* datum = (uint8_t*)heap.alloc_private(n);
  memcpy(datum, name, n);
  rec->name = datum;
  rec->tag = tc6_tag(tc6_string);
  return tc6_pointer(rec, tc6_string);
}

uint8_t* string_name(scm_obj_t x) {
  if (!is_string(x)) fatal("%s:%u internal error: string expected.", __FILE__, __LINE__);
  return ((scm_string_rec_t*)to_address(x))->name;
}

scm_obj_t make_vector(int nsize, scm_obj_t init) {
  object_heap_t& heap = *object_heap_t::current();
  scm_vector_rec_t* rec = (scm_vector_rec_t*)heap.alloc_vector();
  rec->elts = (scm_obj_t*)heap.alloc_private(nsize * sizeof(scm_obj_t));
  rec->nsize = nsize;
  for (int i = 0; i < nsize; i++) {
    rec->elts[i] = init;
  }
  rec->tag = tc6_tag(tc6_vector);
  return tc6_pointer(rec, tc6_vector);
}

scm_obj_t make_values(int nsize) {
  object_heap_t& heap = *object_heap_t::current();
  scm_values_rec_t* rec = (scm_values_rec_t*)heap.alloc_values();
  rec->elts = (scm_obj_t*)heap.alloc_private(nsize * sizeof(scm_obj_t));
  rec->nsize = nsize;
  for (int i = 0; i < nsize; i++) {
    rec->elts[i] = scm_unspecified;
  }
  rec->tag = tc6_tag(tc6_values);
  return tc6_pointer(rec, tc6_values);
}

scm_obj_t make_u8vector(int nsize) {
  object_heap_t& heap = *object_heap_t::current();
  scm_u8vector_rec_t* rec = (scm_u8vector_rec_t*)heap.alloc_u8vector(nsize);
  uint8_t* elts = (uint8_t*)heap.alloc_private(nsize * sizeof(uint8_t));
  memset(elts, 0, nsize * sizeof(uint8_t));
  rec->elts = elts;
  rec->nsize = nsize;
  rec->tag = tc6_tag(tc6_u8vector);
  return tc6_pointer(rec, tc6_u8vector);
}

scm_obj_t make_hashtable(hash_proc_t hash, equiv_proc_t equiv, int capacity) {
  object_heap_t& heap = *object_heap_t::current();
  scm_hashtable_rec_t* rec = (scm_hashtable_rec_t*)heap.alloc_hashtable();
  int nsize = calc_hashtable_size(capacity);
  rec->hash = hash;
  rec->equiv = equiv;
  size_t aux_size = sizeof(hashtable_aux_t) + sizeof(scm_obj_t) * ((nsize + nsize) - 1);
  rec->aux = (hashtable_aux_t*)heap.alloc_private(aux_size);
  rec->aux->capacity = nsize;
  rec->aux->used = 0;
  rec->aux->live = 0;
  for (int i = 0; i < (nsize * 2); i++) rec->aux->elts[i] = scm_hash_free;
  rec->tag = tc6_tag(tc6_hashtable);
  return tc6_pointer(rec, tc6_hashtable);
}

scm_obj_t make_closure(void* code, int argc, int rest, int nsize, scm_obj_t env[], int cdecl) {
  object_heap_t& heap = *object_heap_t::current();
  scm_closure_rec_t* rec = (scm_closure_rec_t*)heap.alloc_collectible(sizeof(scm_closure_rec_t) + (nsize - 1) * sizeof(scm_obj_t));
  rec->code = code;
  rec->argc = argc;
  rec->rest = rest;
  rec->nenv = nsize;
  rec->cdecl = cdecl;
  for (int i = 0; i < nsize; i++) {
    rec->env[i] = env[i];
  }
  rec->tag = tc6_tag(tc6_closure);
  return tc6_pointer(rec, tc6_closure);
}

scm_obj_t make_environment(scm_obj_t name) {
  object_heap_t& heap = *object_heap_t::current();
  scm_environment_rec_t* rec = (scm_environment_rec_t*)heap.alloc_environment();
  rec->name = name;
  rec->variables = make_hashtable(symbol_hash, symbol_equiv, 16);
  rec->macros = make_hashtable(symbol_hash, symbol_equiv, 16);
  rec->tag = tc6_tag(tc6_environment);
  return tc6_pointer(rec, tc6_environment);
}

scm_obj_t environment_variables(scm_obj_t x) {
  if (!is_environment(x)) fatal("%s:%u internal error: environment expected.", __FILE__, __LINE__);
  return ((scm_environment_rec_t*)to_address(x))->variables;
}

scm_obj_t environment_macros(scm_obj_t x) {
  if (!is_environment(x)) fatal("%s:%u internal error: environment expected.", __FILE__, __LINE__);
  return ((scm_environment_rec_t*)to_address(x))->macros;
}

uint8_t* environment_name(scm_obj_t x) {
  if (!is_environment(x)) fatal("%s:%u internal error: environment expected.", __FILE__, __LINE__);
  return symbol_name(((scm_environment_rec_t*)to_address(x))->name);
}

scm_obj_t make_escape(ucontext_t* uctx, uintptr_t sp, scm_obj_t winders) {
  object_heap_t& heap = *object_heap_t::current();
  scm_escape_rec_t* rec = (scm_escape_rec_t*)heap.alloc_collectible(sizeof(scm_escape_rec_t));
  rec->invoked = false;
  rec->retval = scm_undef;
  rec->uctx = (ucontext_t*)malloc(sizeof(ucontext_t));
  *rec->uctx = *uctx;
  rec->sp = sp;
  rec->winders = winders;
  rec->tag = tc6_tag(tc6_escape);
  return tc6_pointer(rec, tc6_escape);
}

scm_obj_t make_continuation(ucontext_t* uctx, size_t stack_size, uint8_t* stack_copy, uint8_t* shadow_copy, uint64_t stack_bottom,
                            scm_obj_t winders) {
  object_heap_t& heap = *object_heap_t::current();
  scm_continuation_rec_t* rec = (scm_continuation_rec_t*)heap.alloc_collectible(sizeof(scm_continuation_rec_t));
  rec->uctx = (ucontext_t*)malloc(sizeof(ucontext_t));
  *rec->uctx = *uctx;
  rec->stack_size = stack_size;
  rec->stack_copy = stack_copy;
  rec->shadow_copy = shadow_copy;
  rec->stack_bottom = stack_bottom;
  rec->winders = winders;
  rec->tag = tc6_tag(tc6_continuation);
  return tc6_pointer(rec, tc6_continuation);
}

scm_obj_t make_cell(scm_obj_t value) {
  object_heap_t& heap = *object_heap_t::current();
  scm_cell_rec_t* rec = (scm_cell_rec_t*)heap.alloc_cell();
  rec->value = value;
  rec->tag = tc6_tag(tc6_cell);
  return tc6_pointer(rec, tc6_cell);
}

scm_obj_t make_port(scm_obj_t name) {
  object_heap_t& heap = *object_heap_t::current();
  scm_port_rec_t* rec = (scm_port_rec_t*)heap.alloc_port();
  rec->name = name;
  rec->aux = new port_aux_t;
  rec->aux->stream = std::monostate{};
  rec->aux->owned = false;
  rec->tag = tc6_tag(tc6_port);
  return tc6_pointer(rec, tc6_port);
}

void cell_value_set(scm_obj_t x, scm_obj_t v) {
  object_heap_t::current()->write_barrier(v);
  ((scm_cell_rec_t*)to_address(x))->value = v;
}

scm_obj_t make_cons(scm_obj_t car, scm_obj_t cdr) {
  object_heap_t& heap = *object_heap_t::current();
  scm_cons_rec_t* rec = (scm_cons_rec_t*)heap.alloc_cons();
  rec->cdr = cdr;
  rec->car = car;
  return (scm_obj_t)rec;
}

scm_obj_t make_list(int len, ...) {
  va_list ap;
  va_start(ap, len);
  if (len == 0) return scm_nil;
  scm_cons_rec_t* rec = (scm_cons_rec_t*)make_cons(va_arg(ap, scm_obj_t), scm_nil);
  scm_cons_rec_t* tail = rec;
  for (int i = 1; i < len; i++) {
    scm_cons_rec_t* e = (scm_cons_rec_t*)make_cons(va_arg(ap, scm_obj_t), scm_nil);
    tail->cdr = (scm_obj_t)e;
    tail = e;
  }
  va_end(ap);
  return (scm_obj_t)rec;
}

scm_obj_t make_list2(scm_obj_t first, scm_obj_t second) { return make_cons(first, make_cons(second, scm_nil)); }
