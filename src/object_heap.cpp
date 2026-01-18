// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "object_heap.h"
#include "bit.h"

thread_local object_heap_t* object_heap_t::s_current;

void object_heap_t::init(size_t pool_size, size_t init_size) {
  m_concurrent_pool.init(pool_size, init_size);
  m_concurrent_heap.init(&m_concurrent_pool);

  m_concurrent_heap.set_trace_proc([this](void* obj) { this->trace(obj); });
  m_concurrent_heap.set_finalize_proc([this](void* obj) { this->finalize(obj); });
  m_concurrent_heap.set_snapshot_root_proc([this]() { this->snapshot_root(); });
  m_concurrent_heap.set_clear_trip_bytes_proc([this]() { this->m_trip_bytes = 0; });
  m_concurrent_heap.set_update_weak_reference_proc([this]() { this->update_weak_reference(); });
#if HPDEBUG
  m_concurrent_heap.set_debug_post_completation_proc([this]() { this->consistency_check(); });
  m_concurrent_heap.set_debug_check_slab_proc([this](void* slab) { this->validate_concurrent_slab(slab); });
#endif

  m_cons.init(&m_concurrent_heap, clp2(sizeof(scm_pair_rec_t)), true, false);
  m_flonums.init(&m_concurrent_heap, clp2(sizeof(scm_long_flonum_rec_t)), true, false);

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

void object_heap_t::trace(void* obj) {}
void object_heap_t::finalize(void* obj) {}
void object_heap_t::snapshot_root() {}
void object_heap_t::update_weak_reference() {}
#if HPDEBUG
void object_heap_t::consistency_check() {}
void object_heap_t::validate_concurrent_slab(void* slab) {}
#endif
void object_heap_t::renounce(void* obj, int size, void* refcon) {}
