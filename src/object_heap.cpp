// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "object_heap.h"
#include "bit.h"
#include "environment.h"
#include "port.h"

thread_local scm_obj_t object_heap_t::s_continuation_captured_retval = scm_undef;
thread_local scm_obj_t object_heap_t::s_current_winders = scm_nil;
thread_local object_heap_t* object_heap_t::s_current;

static constexpr int symbol_table_reserve_size = 4096;

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

void object_heap_t::init(size_t pool_size, size_t init_size) {
  m_concurrent_pool.init(pool_size, init_size);
  m_concurrent_heap.init(&m_concurrent_pool);

  m_trip_bytes = 0;
  m_collect_trip_bytes = init_size / 8;
  m_symbol_table.reserve(symbol_table_reserve_size);

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
  m_cells.init(&m_concurrent_heap, clp2(sizeof(scm_cell_rec_t)), true, false);
  m_flonums.init(&m_concurrent_heap, clp2(sizeof(scm_long_flonum_rec_t)), true, false);
  m_symbols.init(&m_concurrent_heap, clp2(sizeof(scm_symbol_rec_t)), true, true);
  m_strings.init(&m_concurrent_heap, clp2(sizeof(scm_string_rec_t)), true, true);
  m_vectors.init(&m_concurrent_heap, clp2(sizeof(scm_vector_rec_t)), true, true);
  m_values.init(&m_concurrent_heap, clp2(sizeof(scm_values_rec_t)), true, true);
  m_u8vectors.init(&m_concurrent_heap, clp2(sizeof(scm_u8vector_rec_t)), true, true);
  m_hashtables.init(&m_concurrent_heap, clp2(sizeof(scm_hashtable_rec_t)), true, true);
  m_environments.init(&m_concurrent_heap, clp2(sizeof(scm_environment_rec_t)), true, false);
  m_ports.init(&m_concurrent_heap, clp2(sizeof(scm_port_rec_t)), true, true);
  for (int n = 0; n < array_sizeof(m_collectibles); n++) m_collectibles[n].init(&m_concurrent_heap, 1 << (n + 4), true, true);
  for (int n = 0; n < array_sizeof(m_privates); n++) m_privates[n].init(&m_concurrent_heap, 1 << (n + 4), false, false);

  s_current = this;
}

void object_heap_t::destroy() {
  m_concurrent_heap.terminate();
  slab_traits_t* traits = SLAB_TRAITS_OF(m_concurrent_pool.m_pool);
  for (int i = 0; i < m_concurrent_pool.m_pool_watermark; i++) {
    if (GCSLABP(m_concurrent_pool.m_pool[i])) {
      traits->owner->iterate(m_concurrent_pool.m_pool + ((intptr_t)i << SLAB_SIZE_SHIFT), renounce, NULL);
    }
    traits = (slab_traits_t*)((intptr_t)traits + SLAB_SIZE);
  }
  m_concurrent_pool.destroy();
  s_current = nullptr;
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

void* object_heap_t::alloc_collectible(size_t nsize) {
  m_trip_bytes += nsize;
  if (m_trip_bytes >= m_collect_trip_bytes) m_concurrent_heap.collect();
  int bucket = bytes_to_bucket(nsize);
  if (bucket < array_sizeof(m_collectibles)) {
    do {
      void* obj = m_collectibles[bucket].new_collectible_object();
      if (obj) return obj;
    } while (m_concurrent_pool.extend_pool(SLAB_SIZE));
    fatal("fatal: heap memory overflow (%dMB)\n[exit]\n", m_concurrent_pool.m_pool_size / (1024 * 1024));
  } else {
    fatal("%s:%u collectible object over %d bytes not supported but %d bytes requested", __FILE__, __LINE__,
          1 << (array_sizeof(m_collectibles) + 3), nsize);
  }
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
  if (m_concurrent_pool.in_slab(obj)) {
    assert(!m_concurrent_pool.is_collectible(obj));
    slab_traits_t* traits = SLAB_TRAITS_OF(obj);
    traits->owner->delete_object(obj);
  } else {
    m_concurrent_pool.deallocate(obj);
  }
}

void object_heap_t::sweep_symbol_table() {
  std::lock_guard<std::mutex> lock(m_symbol_table_mutex);
  auto it = m_symbol_table.begin();
  while (it != m_symbol_table.end()) {
    scm_obj_t value = it->second;
    assert(is_symbol(value));
    void* p = to_address(value);
    slab_traits_t* traits = SLAB_TRAITS_OF(p);
    if (traits->owner->state(p)) {
      ++it;
    } else {
      it = m_symbol_table.erase(it);
    }
  }
}

void object_heap_t::shade(scm_obj_t obj) {
  if (is_cons(obj)) {
    m_concurrent_heap.shade((void*)obj);
    return;
  }
  if (!is_heap_object(obj)) return;
  m_concurrent_heap.shade(to_address(obj));
}

void object_heap_t::trace(void* obj) {
  assert(m_concurrent_pool.is_collectible(obj));
  slab_traits_t* traits = SLAB_TRAITS_OF(obj);
  if (traits->owner->test_and_set_mark(obj)) return;
  if (traits->owner == &m_cons) {
    scm_cons_rec_t* rec = (scm_cons_rec_t*)obj;
    shade(rec->car);
    shade(rec->cdr);
    return;
  }

  uintptr_t tag = *(uintptr_t*)obj;
  uintptr_t tc6 = (tag & 0x3f00) >> 8;

  switch (tc6) {
    case tc6_cell: {
      scm_cell_rec_t* rec = (scm_cell_rec_t*)obj;
      shade(rec->value);
      return;
    }
    case tc6_vector: {
      scm_vector_rec_t* rec = (scm_vector_rec_t*)obj;
      for (int i = 0; i < rec->nsize; i++) {
        shade(rec->elts[i]);
      }
      return;
    }
    case tc6_values: {
      scm_values_rec_t* rec = (scm_values_rec_t*)obj;
      for (int i = 0; i < rec->nsize; i++) {
        shade(rec->elts[i]);
      }
      return;
    }
    case tc6_hashtable: {
      scm_hashtable_rec_t* rec = (scm_hashtable_rec_t*)obj;
      hashtable_aux_t* aux = rec->aux;
      for (int i = 0; i < aux->capacity * 2; i++) {
        shade(aux->elts[i]);
      }
      return;
    }
    case tc6_environment: {
      scm_environment_rec_t* rec = (scm_environment_rec_t*)obj;
      shade(rec->name);
      shade(rec->variables);
      shade(rec->macros);
      return;
    }
    case tc6_port: {
      scm_port_rec_t* rec = (scm_port_rec_t*)obj;
      shade(rec->name);
      return;
    }
    case tc6_closure: {
      scm_closure_rec_t* rec = (scm_closure_rec_t*)obj;
      for (int i = 0; i < rec->nenv; i++) {
        shade(rec->env[i]);
      }
      return;
    }
    case tc6_escape: {
      scm_escape_rec_t* rec = (scm_escape_rec_t*)obj;
      shade(rec->winders);
      shade(rec->retval);
      return;
    }
    case tc6_continuation: {
      scm_continuation_rec_t* rec = (scm_continuation_rec_t*)obj;
      shade(rec->winders);
      m_concurrent_heap.trace_memory_range((uint64_t)rec->uctx, (uint64_t)rec->uctx + sizeof(ucontext_t));
      m_concurrent_heap.trace_memory_range((uint64_t)rec->stack_copy, (uint64_t)rec->stack_copy + rec->stack_size);
      return;
    }
    case tc6_long_flonum:
    case tc6_symbol:
    case tc6_string:
    case tc6_u8vector:
      return;
  }
  assert(false);
}

void object_heap_t::finalize(void* obj) {
  assert(m_concurrent_pool.is_collectible(obj));
  slab_traits_t* traits = SLAB_TRAITS_OF(obj);
  assert(traits->owner != &m_cons);

  uintptr_t tag = *(uintptr_t*)obj;
  uintptr_t tc6 = (tag & 0x3f00) >> 8;

  switch (tc6) {
    case tc6_symbol: {
      scm_symbol_rec_t* rec = (scm_symbol_rec_t*)obj;
      delete_private(rec->name);
      return;
    }
    case tc6_string: {
      scm_string_rec_t* rec = (scm_string_rec_t*)obj;
      delete_private(rec->name);
      return;
    }
    case tc6_u8vector: {
      scm_u8vector_rec_t* rec = (scm_u8vector_rec_t*)obj;
      delete_private(rec->elts);
      return;
    }
    case tc6_hashtable: {
      scm_hashtable_rec_t* rec = (scm_hashtable_rec_t*)obj;
      delete_private(rec->aux);
      return;
    }
    case tc6_vector: {
      scm_vector_rec_t* rec = (scm_vector_rec_t*)obj;
      delete_private(rec->elts);
      return;
    }
    case tc6_values: {
      scm_values_rec_t* rec = (scm_values_rec_t*)obj;
      delete_private(rec->elts);
      return;
    }
    case tc6_port: {
      scm_port_rec_t* rec = (scm_port_rec_t*)obj;
      port_finalize(rec);
      return;
    }
    case tc6_escape: {
      scm_escape_rec_t* rec = (scm_escape_rec_t*)obj;
      if (rec->uctx) free(rec->uctx);
      rec->uctx = nullptr;
      return;
    }
    case tc6_continuation: {
      scm_continuation_rec_t* rec = (scm_continuation_rec_t*)obj;
      if (rec->uctx) free(rec->uctx);
      if (rec->stack_copy) free(rec->stack_copy);
      if (rec->shadow_copy) free(rec->shadow_copy);
      rec->uctx = nullptr;
      rec->stack_copy = nullptr;
      rec->shadow_copy = nullptr;
      return;
    }
    case tc6_closure:
    case tc6_long_flonum:
    case tc6_environment:
    case tc6_cell:
      return;
  }
  assert(false);
}

void object_heap_t::enqueue_root(scm_obj_t obj) {
  if (is_cons(obj)) {
    m_concurrent_heap.enqueue_root((void*)obj);
  } else if (is_heap_object(obj)) {
    m_concurrent_heap.enqueue_root(to_address(obj));
  }
}

void object_heap_t::snapshot_root() {
  for (auto it = m_root_set.begin(); it != m_root_set.end(); it++) enqueue_root(*it);
  for (auto it = environment::s_literals.begin(); it != environment::s_literals.end(); it++) enqueue_root(*it);
  enqueue_root(environment::s_interaction_environment);
  enqueue_root(environment::s_system_environment);
  enqueue_root(environment::s_current_environment);
  enqueue_root(s_current_winders);
  enqueue_root(s_continuation_captured_retval);
}

void object_heap_t::update_weak_reference() { sweep_symbol_table(); }

void object_heap_t::renounce(void* obj, int size, void* refcon) {}

#if HPDEBUG
void object_heap_t::consistency_check() {}
void object_heap_t::validate_concurrent_slab(void* slab) {}
#endif
