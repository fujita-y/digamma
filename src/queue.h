// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef LOCKFREE_QUEUE_H_INCLUDED
#define LOCKFREE_QUEUE_H_INCLUDED

#include <atomic>
#include <cassert>
#include <cstdlib>
#include <cstring>

#include "core.h"

// Single-producer / single-consumer lock-free bounded ring buffer.
//
// Invariants
//   - capacity MUST be a power of two (asserted in init()).
//   - Exactly one thread calls try_put()      (the mutator).
//   - Exactly one thread calls try_get() / batch_get() / count() / empty()
//        (the collector).
//   - m_head is only *written* by the consumer; *read* by the producer.
//   - m_tail is only *written* by the producer; *read* by the consumer.
//
// Memory ordering
//   - The producer stores to m_tail with release after writing the slot,
//     so the consumer's acquire load of m_tail sees the written data.
//   - The consumer stores to m_head with release after reading the slot,
//     so the producer's acquire load of m_head sees the freed slot.

template <typename T> class spsc_lockfree_queue_t {
  spsc_lockfree_queue_t(const spsc_lockfree_queue_t&) = delete;
  spsc_lockfree_queue_t& operator=(const spsc_lockfree_queue_t&) = delete;
  spsc_lockfree_queue_t(spsc_lockfree_queue_t&&) = delete;
  spsc_lockfree_queue_t& operator=(spsc_lockfree_queue_t&&) = delete;

  // Both indices are monotonically increasing; the actual buffer position
  // is obtained via (index & m_mask).  This avoids the ambiguity between
  // "full" and "empty" that a plain head==tail scheme would have.
  alignas(64) std::atomic<uint32_t> m_head;  // written by consumer
  alignas(64) std::atomic<uint32_t> m_tail;  // written by producer
  T* m_buf;
  uint32_t m_capacity;
  uint32_t m_mask;

 public:
  spsc_lockfree_queue_t() = default;
  ~spsc_lockfree_queue_t() = default;

  void init(int nelts) {
    assert(nelts > 0);
    assert((nelts & (nelts - 1)) == 0);  // must be power-of-two
    m_capacity = static_cast<uint32_t>(nelts);
    m_mask = m_capacity - 1;
    m_buf = static_cast<T*>(std::malloc(m_capacity * sizeof(T)));
    if (!m_buf) fatal("%s:%u memory overflow on malloc lockfree_queue_t", __FILE__, __LINE__);
    m_head.store(0, std::memory_order_relaxed);
    m_tail.store(0, std::memory_order_relaxed);
  }

  void destroy() {
    std::free(m_buf);
    m_buf = nullptr;
  }

  // --- producer (mutator) side ---

  // Non-blocking enqueue.  Returns true on success, false if the queue is full.
  bool try_put(T datum) {
    uint32_t tail = m_tail.load(std::memory_order_relaxed);
    uint32_t head = m_head.load(std::memory_order_acquire);
    if (tail - head >= m_capacity) return false;  // full
    m_buf[tail & m_mask] = datum;
    m_tail.store(tail + 1, std::memory_order_release);
    return true;
  }

  // --- consumer (collector) side ---

  // Non-blocking dequeue of a single element.
  bool try_get(T* datum) {
    uint32_t head = m_head.load(std::memory_order_relaxed);
    uint32_t tail = m_tail.load(std::memory_order_acquire);
    if (head == tail) return false;  // empty
    *datum = m_buf[head & m_mask];
    m_head.store(head + 1, std::memory_order_release);
    return true;
  }

  // Non-blocking bulk dequeue — returns number of elements copied into data[].
  int batch_get(T data[], int max_num) {
    uint32_t head = m_head.load(std::memory_order_relaxed);
    uint32_t tail = m_tail.load(std::memory_order_acquire);
    uint32_t avail = tail - head;
    if (avail == 0) return 0;
    uint32_t take = (avail < static_cast<uint32_t>(max_num)) ? avail : static_cast<uint32_t>(max_num);

    // Copy elements, handling the ring-buffer wrap with two segments.
    uint32_t pos = head & m_mask;
    uint32_t seg1 = m_capacity - pos;  // slots until end-of-buffer
    if (seg1 > take) seg1 = take;
    std::memcpy(data, m_buf + pos, seg1 * sizeof(T));

    uint32_t seg2 = take - seg1;
    if (seg2 > 0) std::memcpy(data + seg1, m_buf, seg2 * sizeof(T));

    m_head.store(head + take, std::memory_order_release);
    return static_cast<int>(take);
  }

  // Approximate count — safe to call from either thread, but the value
  // may be stale by the time the caller acts on it.
  int count() const {
    uint32_t tail = m_tail.load(std::memory_order_acquire);
    uint32_t head = m_head.load(std::memory_order_acquire);
    return static_cast<int>(tail - head);
  }

  bool empty() const { return count() == 0; }

  int limit() const { return static_cast<int>(m_capacity); }
};

#endif
