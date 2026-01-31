// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "object_heap.h"
#include "bit.h"

inline int bytes_to_bucket(uint32_t x)  // see bit.cpp
{
  assert(x >= 16);  // (1 << 4)
  uint32_t n = 0;
  uint32_t c = 16;
  x = x - 1;
  do {
    uint32_t y = x >> c;
    if (y != 0) {
      n = n + c;
      x = y;
    }
    c = c >> 1;
  } while (c != 0);
  return n + x - 4;
}

thread_local object_heap_t* object_heap_t::s_current;

void object_heap_t::init(size_t pool_size, size_t init_size) {
  m_concurrent_pool.init(pool_size, init_size);
  m_concurrent_heap.init(&m_concurrent_pool);

  m_trip_bytes = 0;
  m_collect_trip_bytes = init_size / 8;

  m_concurrent_heap.set_trace_proc([this](void* obj) { this->trace(obj); });
  m_concurrent_heap.set_finalize_proc([this](void* obj) { this->finalize(obj); });
  m_concurrent_heap.set_snapshot_root_proc([this]() { this->snapshot_root(); });
  m_concurrent_heap.set_clear_trip_bytes_proc([this]() { this->m_trip_bytes = 0; });
  m_concurrent_heap.set_update_weak_reference_proc([this]() { this->update_weak_reference(); });
#if HPDEBUG
  m_concurrent_heap.set_debug_post_completation_proc([this]() { this->consistency_check(); });
  m_concurrent_heap.set_debug_check_slab_proc([this](void* slab) { this->validate_concurrent_slab(slab); });
#endif

  m_cons.init(&m_concurrent_heap, clp2(sizeof(scm_cons_rec_t)), true, false);
  m_flonums.init(&m_concurrent_heap, clp2(sizeof(scm_long_flonum_rec_t)), true, false);
  m_symbols.init(&m_concurrent_heap, clp2(sizeof(scm_symbol_rec_t)), true, true);
  m_strings.init(&m_concurrent_heap, clp2(sizeof(scm_string_rec_t)), true, true);
  m_vectors.init(&m_concurrent_heap, clp2(sizeof(scm_vector_rec_t)), true, true);
  m_u8vectors.init(&m_concurrent_heap, clp2(sizeof(scm_u8vector_rec_t)), true, true);
  m_hashtables.init(&m_concurrent_heap, clp2(sizeof(scm_hashtable_rec_t)), true, true);
  for (int n = 0; n < array_sizeof(m_privates); n++) m_privates[n].init(&m_concurrent_heap, 1 << (n + 4), false, false);

  s_current = this;
}

void object_heap_t::destroy() {
  m_concurrent_heap.terminate();
  slab_traits_t* traits = SLAB_TRAITS_OF(m_concurrent_pool.m_pool);
  for (int i = 0; i < m_concurrent_pool.m_pool_watermark; i++) {
    if (GCSLABP(m_concurrent_pool.m_pool[i])) {
      traits->cache->iterate(m_concurrent_pool.m_pool + ((intptr_t)i << SLAB_SIZE_SHIFT), renounce, NULL);
    }
    traits = (slab_traits_t*)((intptr_t)traits + SLAB_SIZE);
  }
  m_concurrent_pool.destroy();
}

void* object_heap_t::alloc_object(concurrent_slab_t& slab) {
  m_trip_bytes += slab.m_object_size;
  if (m_trip_bytes >= m_collect_trip_bytes) m_concurrent_heap.collect();
  do {
    void* obj = slab.new_collectible_object();
    if (obj) return obj;
  } while (m_concurrent_pool.extend_pool(SLAB_SIZE));
  fatal("fatal: heap memory overflow (%.2fMB)\n[exit]\n", m_concurrent_pool.m_pool_size / (1024.0 * 1024.0));
  return NULL;
}

void* object_heap_t::alloc_private(size_t size) {
  m_trip_bytes += size;
  if (m_trip_bytes >= m_collect_trip_bytes) m_concurrent_heap.collect();
  int bucket = 0;
  if (size > 16) bucket = bytes_to_bucket(size);
  if (bucket < array_sizeof(m_privates)) {
    do {
      void* obj = m_privates[bucket].new_object();
      if (obj) return obj;
    } while (m_concurrent_pool.extend_pool(SLAB_SIZE));
    fatal("fatal: heap memory overflow (%.2fMB)\n[exit]\n", m_concurrent_pool.m_pool_size / (1024.0 * 1024.0));
  } else {
    do {
      void* obj = m_concurrent_pool.allocate(size, false, false);
      if (obj) return obj;
    } while (m_concurrent_pool.extend_pool(size));
    fatal("fatal: heap memory overflow (%.2fMB)\n[exit]\n", m_concurrent_pool.m_pool_size / (1024.0 * 1024.0));
  }
  return NULL;
}

void object_heap_t::delete_private(void* obj) {
  assert(m_concurrent_pool.is_not_collectible(obj));
  slab_traits_t* traits = SLAB_TRAITS_OF(obj);
  traits->cache->delete_object(obj);
}

void object_heap_t::shade(scm_obj_t obj) {
  if (is_cons(obj)) {
    m_concurrent_heap.shade((void*)obj);
    return;
  }
  if (!is_heap_object(obj)) return;
  // TODO: add additional early return if TBI enabled
  m_concurrent_heap.shade(to_address(obj));
}

void object_heap_t::trace(void* obj) {
  assert(m_concurrent_pool.is_collectible(obj));
  slab_traits_t* traits = SLAB_TRAITS_OF(obj);
  if (traits->cache->test_and_set_mark(obj)) return;
  if (traits->cache == &m_cons) {
    scm_cons_rec_t* rec = (scm_cons_rec_t*)obj;
    shade(rec->car);
    shade(rec->cdr);
    return;
  }
  if (traits->cache == &m_vectors) {
    scm_vector_rec_t* rec = (scm_vector_rec_t*)obj;
    for (int i = 0; i < rec->nsize; i++) {
      shade(rec->elts[i]);
    }
    return;
  }
  if (traits->cache == &m_hashtables) {
    scm_hashtable_rec_t* rec = (scm_hashtable_rec_t*)obj;
    hashtable_aux_t* aux = rec->aux;
    for (int i = 0; i < aux->capacity * 2; i++) {
      shade(aux->elts[i]);
    }
    return;
  }
}

void object_heap_t::finalize(void* obj) {
  assert(m_concurrent_pool.is_collectible(obj));
  slab_traits_t* traits = SLAB_TRAITS_OF(obj);
  if (traits->cache == &m_symbols) {
    scm_symbol_rec_t* rec = (scm_symbol_rec_t*)obj;
    delete_private(rec->name);
    return;
  }
  if (traits->cache == &m_strings) {
    scm_string_rec_t* rec = (scm_string_rec_t*)obj;
    delete_private(rec->name);
    return;
  }
  if (traits->cache == &m_u8vectors) {
    scm_u8vector_rec_t* rec = (scm_u8vector_rec_t*)obj;
    delete_private(rec->elts);
    return;
  }
  if (traits->cache == &m_hashtables) {
    scm_hashtable_rec_t* rec = (scm_hashtable_rec_t*)obj;
    delete_private(rec->aux);
    return;
  }
}

void object_heap_t::snapshot_root() {
  for (auto it = m_root_set.begin(); it != m_root_set.end(); it++) shade(*it);
}

void object_heap_t::update_weak_reference() {}
#if HPDEBUG
void object_heap_t::consistency_check() {}
void object_heap_t::validate_concurrent_slab(void* slab) {}
#endif
void object_heap_t::renounce(void* obj, int size, void* refcon) {}
