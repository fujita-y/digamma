// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef OBJECT_HEAP_H_INCLUDED
#define OBJECT_HEAP_H_INCLUDED

#include "core.h"
#include "object.h"
#include "concurrent_heap.h"
#include "concurrent_pool.h"
#include "concurrent_slab.h"

class object_heap_t {
 public:
 private:
  thread_local static object_heap_t* s_current;
  concurrent_pool_t m_concurrent_pool;
  concurrent_heap_t m_concurrent_heap;
  concurrent_slab_t m_cons;
  concurrent_slab_t m_cells;
  concurrent_slab_t m_flonums;
  concurrent_slab_t m_symbols;
  concurrent_slab_t m_strings;
  concurrent_slab_t m_vectors;
  concurrent_slab_t m_values;
  concurrent_slab_t m_u8vectors;
  concurrent_slab_t m_hashtables;
  concurrent_slab_t m_environments;
  concurrent_slab_t m_ports;
  concurrent_slab_t m_collectibles[8];  // 16-32-64-128-256-512-1024-2048
  concurrent_slab_t m_privates[8];      // 16-32-64-128-256-512-1024-2048

  uint64_t m_trip_bytes;

  static void renounce(void* obj, int size, void* refcon);
  void shade(scm_obj_t obj);
  __attribute__((no_sanitize("hwaddress"))) void trace(void* obj);
  void finalize(void* obj);
  void snapshot_root();
  void update_weak_reference();
  void sweep_symbol_table();
  void delete_private(void* obj);
  void enqueue_root(scm_obj_t obj);

  void* alloc_object(concurrent_slab_t& slab) {
    m_trip_bytes += slab.m_object_size;
    if (m_trip_bytes >= m_collect_trip_bytes) [[unlikely]] {
      m_concurrent_heap.collect();
    }
    do {
      void* obj = slab.new_collectible_object();
      if (obj) [[likely]] {
        return obj;
      }
    } while (m_concurrent_pool.extend_pool(SLAB_SIZE));
    fatal("fatal: heap memory overflow (%.2fMB)\n[exit]\n", m_concurrent_pool.m_pool_size / (1024.0 * 1024.0));
    return NULL;
  }

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

#if HPDEBUG
  void consistency_check();
  void validate_concurrent_slab(void* slab);
#endif

 public:
  void init(size_t pool_size, size_t init_size);
  void destroy();
  void safepoint() { m_concurrent_heap.safepoint(); }
  void* is_live_object(uint64_t addr) { return m_concurrent_heap.is_live_object(addr); }
  bool* stop_the_world_ptr() { return &m_concurrent_heap.m_stop_the_world; }
  void collect() { m_concurrent_heap.collect(); }
  mutex_t& collector_lock() { return m_concurrent_heap.m_collector_lock; }
  void* alloc_cons() { return alloc_object(m_cons); }
  void* alloc_cell() { return alloc_object(m_cells); }
  void* alloc_flonum() { return alloc_object(m_flonums); }
  void* alloc_symbol() { return alloc_object(m_symbols); }
  void* alloc_string() { return alloc_object(m_strings); }
  void* alloc_vector() { return alloc_object(m_vectors); }
  void* alloc_values() { return alloc_object(m_values); }
  void* alloc_u8vector(int nsize) { return alloc_object(m_u8vectors); }
  void* alloc_hashtable() { return alloc_object(m_hashtables); }
  void* alloc_environment() { return alloc_object(m_environments); }
  void* alloc_port() { return alloc_object(m_ports); }
  void* alloc_private(size_t size);

  void* alloc_collectible(size_t nsize) {
    m_trip_bytes += nsize;
    if (m_trip_bytes >= m_collect_trip_bytes) [[unlikely]] {
      m_concurrent_heap.collect();
    }
    int bucket = bytes_to_bucket(nsize);
    if (bucket < array_sizeof(m_collectibles)) [[likely]] {
      do {
        void* obj = m_collectibles[bucket].new_collectible_object();
        if (obj) [[likely]] {
          return obj;
        }
      } while (m_concurrent_pool.extend_pool(SLAB_SIZE));
      fatal("fatal: heap memory overflow (%dMB)\n[exit]\n", m_concurrent_pool.m_pool_size / (1024 * 1024));
    } else {
      fatal("%s:%u collectible object over %d bytes not supported but %d bytes requested", __FILE__, __LINE__,
            1 << (array_sizeof(m_collectibles) + 3), nsize);
    }
    return NULL;
  }

  void write_barrier(scm_obj_t obj) {
    if (is_cons(obj)) {
      m_concurrent_heap.write_barrier((void*)obj);
      return;
    }
    if (is_heap_object(obj)) m_concurrent_heap.write_barrier(to_address(obj));
  }

  uint64_t m_collect_trip_bytes;

  object_heap_t() = default;
  ~object_heap_t() = default;
  object_heap_t(const object_heap_t&) = delete;
  object_heap_t& operator=(const object_heap_t&) = delete;
  object_heap_t(object_heap_t&&) = delete;
  object_heap_t& operator=(object_heap_t&&) = delete;

  static object_heap_t* current() {
    assert(s_current);
    return s_current;
  }
};

#endif
