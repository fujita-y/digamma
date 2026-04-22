// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "object_heap.h"
#include "bit.h"
#include "context.h"
#include "port.h"
#include "subr.h"

#define DEFALUT_COLLECT_TRIP_BYTES (4 * 1024 * 1024)

void object_heap_t::init(size_t pool_size, size_t init_size) {
  m_concurrent_pool.init(pool_size, init_size);
  m_concurrent_heap.init(&m_concurrent_pool);

  m_trip_bytes = 0;
  m_collect_trip_bytes = DEFALUT_COLLECT_TRIP_BYTES;

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

  // cache configuration: allow slabs to be reused instead of returned to pool on each sweep
  int base_cache_limit = (int)(m_collect_trip_bytes / SLAB_SIZE);
  m_cons.m_cache_limit = base_cache_limit;
  m_cells.m_cache_limit = base_cache_limit / 4;
  m_flonums.m_cache_limit = base_cache_limit / 8;
  m_symbols.m_cache_limit = base_cache_limit / 16;
  m_strings.m_cache_limit = base_cache_limit / 16;
  m_vectors.m_cache_limit = base_cache_limit / 16;
  m_values.m_cache_limit = base_cache_limit / 16;
  m_u8vectors.m_cache_limit = base_cache_limit / 16;
  m_hashtables.m_cache_limit = base_cache_limit / 16;
  m_environments.m_cache_limit = base_cache_limit / 16;
  m_ports.m_cache_limit = base_cache_limit / 16;
  for (int n = 0; n < array_sizeof(m_collectibles); n++) m_collectibles[n].m_cache_limit = base_cache_limit / 8;

  context::s_current_object_heap = this;
}

void object_heap_t::destroy() {
  m_concurrent_heap.terminate();
  slab_traits_t* traits = SLAB_TRAITS_OF(m_concurrent_pool.m_pool);
  for (int i = 0; i < m_concurrent_pool.m_pool_watermark; i++) {
    if (GCSLABP(m_concurrent_pool.m_pool[i]) && traits->owner != &m_cons) {
      traits->owner->iterate(m_concurrent_pool.m_pool + ((intptr_t)i << SLAB_SIZE_SHIFT), renounce, NULL);
    }
    traits = (slab_traits_t*)((intptr_t)traits + SLAB_SIZE);
  }
  m_concurrent_pool.destroy();
  context::s_current_object_heap = nullptr;
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
  std::lock_guard<std::mutex> lock(m_symbols_mutex);
  auto it = m_symbol_map.begin();
  while (it != m_symbol_map.end()) {
    scm_obj_t value = it->second;
    assert(is_symbol(value));
    void* p = to_address(value);
    slab_traits_t* traits = SLAB_TRAITS_OF(p);
    if (traits->owner->state(p)) {
      ++it;
    } else {
      it = m_symbol_map.erase(it);
    }
  }
}

void object_heap_t::shade(scm_obj_t obj) {
  if (is_cons(obj)) {
    m_concurrent_heap.shade((void*)obj);
    return;
  }
  if (!is_heap_object(obj)) return;
  void* addr = to_address(obj);
  // Stack-allocated closures and cells (from the stack_alloc optimization)
  // have a valid heap tag but live on the mutator stack, not in the GC pool.
  // The GC must not try to trace or mark them — they are kept live by the
  // stack frame and are guaranteed to be dead before any future GC cycle.
  if (!m_concurrent_pool.in_pool(addr)) {
    // note: object tag on stack could be overwritten.
    return;
  }
  m_concurrent_heap.shade(addr);
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

  uintptr_t tc6 = tag_tc6_num(*(scm_tc6_t*)obj);

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
    case tc6_tuple: {
      scm_tuple_rec_t* rec = (scm_tuple_rec_t*)obj;
      for (int i = 0; i < rec->nsize; i++) {
        shade(rec->elts[i]);
      }
      return;
    }
    case tc6_future: {
      scm_future_rec_t* rec = (scm_future_rec_t*)obj;
      shade(rec->closure);
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

  uintptr_t tc6 = tag_tc6_num(*(scm_tc6_t*)obj);

  switch (tc6) {
    case tc6_symbol: {
      scm_symbol_rec_t* rec = (scm_symbol_rec_t*)obj;
      // Only free externally-allocated name. Inline names are part of the same
      // collectible slab and reclaimed automatically by the GC.
      if (rec->name != (uint8_t*)((uintptr_t)rec + sizeof(scm_symbol_rec_t))) {
        delete_private(rec->name);
      }
      return;
    }
    case tc6_string: {
      scm_string_rec_t* rec = (scm_string_rec_t*)obj;
      // Same inline check as tc6_symbol above.
      if (rec->name != (uint8_t*)((uintptr_t)rec + sizeof(scm_string_rec_t))) {
        delete_private(rec->name);
      }
      return;
    }
    case tc6_u8vector: {
      scm_u8vector_rec_t* rec = (scm_u8vector_rec_t*)obj;
      if (rec->owned) {
        free(rec->elts);
        rec->elts = nullptr;
      }
      return;
    }
    case tc6_hashtable: {
      scm_hashtable_rec_t* rec = (scm_hashtable_rec_t*)obj;
      delete_private(rec->aux);
      return;
    }
    case tc6_vector: {
      scm_vector_rec_t* rec = (scm_vector_rec_t*)obj;
      // Only free externally-allocated elts. If elts points directly after the
      // header the elements are inline in the same collectible slab object and
      // will be reclaimed automatically — no explicit free needed.
      if (rec->elts != (scm_obj_t*)((uintptr_t)rec + sizeof(scm_vector_rec_t))) {
        delete_private(rec->elts);
      }
      return;
    }
    case tc6_values: {
      scm_values_rec_t* rec = (scm_values_rec_t*)obj;
      // Same inline check as tc6_vector above.
      if (rec->elts != (scm_obj_t*)((uintptr_t)rec + sizeof(scm_values_rec_t))) {
        delete_private(rec->elts);
      }
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
    case tc6_future: {
      scm_future_rec_t* rec = (scm_future_rec_t*)obj;
      if (rec->future) delete (boost::fibers::shared_future<scm_obj_t>*)rec->future;
      rec->future = nullptr;
      return;
    }
    case tc6_tuple:
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
    void* addr = to_address(obj);
    if (!m_concurrent_pool.in_pool(addr)) {
      assert(is_closure(obj) || is_cell(obj));  // only closures and cells may be stack-allocated
      return;                                   // stack-allocated object — skip
    }
    m_concurrent_heap.enqueue_root(addr);
  }
}

void object_heap_t::snapshot_root() {
  for (auto it = context::s_gc_protected.begin(); it != context::s_gc_protected.end(); it++) enqueue_root(*it);
  for (auto it = context::s_literals.begin(); it != context::s_literals.end(); it++) enqueue_root(*it);
  enqueue_root(context::s_interaction_environment);
  enqueue_root(context::s_system_environment);
  enqueue_root(context::s_current_environment);
  enqueue_root(context::s_current_winders);
  enqueue_root(context::s_continuation_captured_retval);
  enqueue_root(context::s_standard_input_port);
  enqueue_root(context::s_standard_output_port);
  enqueue_root(context::s_standard_error_port);
  enqueue_root(context::s_current_input_port);
  enqueue_root(context::s_current_output_port);
  enqueue_root(context::s_current_error_port);
  for (auto closure : context::s_trampolines) {
    if (closure) enqueue_root(closure);
  }
  scan_fiber_stacks();
}

void object_heap_t::update_weak_reference() { sweep_symbol_table(); }

void object_heap_t::renounce(void* obj, int size, void* refcon) {
  uintptr_t tc6 = tag_tc6_num(*(scm_tc6_t*)obj);

  switch (tc6) {
    case tc6_u8vector: {
      scm_u8vector_rec_t* rec = (scm_u8vector_rec_t*)obj;
      if (rec->owned) {
        free(rec->elts);
        rec->elts = nullptr;
      }
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
    case tc6_future: {
      scm_future_rec_t* rec = (scm_future_rec_t*)obj;
      if (rec->future) delete (boost::fibers::shared_future<scm_obj_t>*)rec->future;
      rec->future = nullptr;
      return;
    }
    case tc6_symbol:
    case tc6_string:
    case tc6_hashtable:
    case tc6_vector:
    case tc6_values:
    case tc6_tuple:
    case tc6_closure:
    case tc6_long_flonum:
    case tc6_environment:
    case tc6_cell:
      return;
  }
  assert(false);
}

#if HPDEBUG
void object_heap_t::consistency_check() {}
void object_heap_t::validate_concurrent_slab(void* slab) {}
#endif
