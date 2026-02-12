// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef OBJECT_HEAP_H_INCLUDED
#define OBJECT_HEAP_H_INCLUDED

#include "core.h"
#include "object.h"
#include <mutex>
#include <set>
#include <unordered_map>
#include "concurrent_heap.h"
#include "concurrent_pool.h"
#include "concurrent_slab.h"

class object_heap_t {
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
  concurrent_slab_t m_u8vectors;
  concurrent_slab_t m_hashtables;
  concurrent_slab_t m_environments;
  concurrent_slab_t m_subrs;
  concurrent_slab_t m_collectibles[8];  // 16-32-64-128-256-512-1024-2048
  concurrent_slab_t m_privates[8];      // 16-32-64-128-256-512-1024-2048

  uint64_t m_trip_bytes;
  std::set<scm_obj_t> m_root_set;

  void* alloc_object(concurrent_slab_t& slab);
  static void renounce(void* obj, int size, void* refcon);
  void shade(scm_obj_t obj);
  void trace(void* obj);
  void finalize(void* obj);
  void snapshot_root();
  void update_weak_reference();
  void sweep_symbol_table();
  void delete_private(void* obj);
  void enqueue_root(scm_obj_t obj);
#if HPDEBUG
  void consistency_check();
  void validate_concurrent_slab(void* slab);
#endif

 public:
  void init(size_t pool_size, size_t init_size);
  void destroy();
  void safepoint() { m_concurrent_heap.safepoint(); }
  void* alloc_cons() { return alloc_object(m_cons); }
  void* alloc_cell() { return alloc_object(m_cells); }
  void* alloc_flonum() { return alloc_object(m_flonums); }
  void* alloc_symbol() { return alloc_object(m_symbols); }
  void* alloc_string() { return alloc_object(m_strings); }
  void* alloc_vector() { return alloc_object(m_vectors); }
  void* alloc_u8vector(int nsize) { return alloc_object(m_u8vectors); }
  void* alloc_hashtable() { return alloc_object(m_hashtables); }
  void* alloc_environment() { return alloc_object(m_environments); }
  void* alloc_subr() { return alloc_object(m_subrs); }
  void* alloc_collectible(size_t size);
  void* alloc_private(size_t size);

  std::mutex m_symbol_table_mutex;
  std::unordered_map<std::string, scm_obj_t> m_symbol_table;

  void environment_variable_set(scm_obj_t key, scm_obj_t value);
  scm_obj_t environment_variable_ref(scm_obj_t key);
  scm_obj_t environment_variable_cell_ref(scm_obj_t key);
  scm_obj_t m_environment;

  void write_barrier(scm_obj_t obj) {
    if (is_cons(obj)) {
      m_concurrent_heap.write_barrier((void*)obj);
      return;
    }
    if (is_heap_object(obj)) m_concurrent_heap.write_barrier(to_address(obj));
  }

  void add_root(scm_obj_t obj) { m_root_set.insert(obj); }
  void remove_root(scm_obj_t obj) { m_root_set.erase(obj); }

  uint64_t m_collect_trip_bytes;

  object_heap_t() {};
  ~object_heap_t() {};
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
