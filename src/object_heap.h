// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef OBJECT_HEAP_H_INCLUDED
#define OBJECT_HEAP_H_INCLUDED

#include "core.h"
#include "concurrent_heap.h"
#include "concurrent_pool.h"
#include "concurrent_slab.h"

class object_heap_t {
 private:
  thread_local static object_heap_t* s_current;
  concurrent_pool_t m_concurrent_pool;
  concurrent_heap_t m_concurrent_heap;
  int64_t m_trip_bytes;

  static void renounce(void* obj, int size, void* refcon);
  void trace(void* obj);
  void finalize(void* obj);
  void snapshot_root();
  void update_weak_reference();
#if HPDEBUG
  void consistency_check();
  void validate_concurrent_slab(void* slab);
#endif

 public:
  void init(size_t pool_size, size_t init_size);
  void destroy();
  concurrent_slab_t m_cons;
  concurrent_slab_t m_flonums;

  object_heap_t() {};
  ~object_heap_t() {};
  object_heap_t(const object_heap_t&) = delete;
  object_heap_t& operator=(const object_heap_t&) = delete;
  object_heap_t(object_heap_t&&) = delete;
  object_heap_t& operator=(object_heap_t&&) = delete;

  static object_heap_t* current() { return s_current; }
};
#endif
