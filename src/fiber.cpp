// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "fiber.h"
#include "context.h"
#include "object_heap.h"

#include "asio.h"

#include <boost/asio.hpp>
#include <boost/fiber/all.hpp>
#include <boost/fiber/type.hpp>
#include <chrono>
#include <deque>
#include <thread>

// ---------------------------------------------------------------------------
// priority_scheduler — Asio Round Robin variant
//
// All fiber scheduling and Asio I/O run on the same (main) thread.
// When the scheduler has nothing ready, suspend_until() polls the io_context
// directly instead of blocking on a condition_variable.  Completion handlers
// therefore execute inline during suspend_until(), fulfill their promises, and
// call awakened() — all on the main thread — before suspend_until() returns.
//
// Consequences:
//   • No background thread, no condition_variable, no mutex on the queue.
//   • notify() is a no-op: Asio drives wakeup via the scheduler loop.
// ---------------------------------------------------------------------------
class priority_scheduler : public boost::fibers::algo::algorithm {
 private:
  std::deque<boost::fibers::context*> ready_queue_;
  bool focus_main_ = false;

 public:
  priority_scheduler() = default;

  // Request that the next pick_next() favours the main fiber context.
  void set_focus_main(bool enable) { focus_main_ = enable; }

  // Called (on the main thread) when a fiber becomes runnable.
  void awakened(boost::fibers::context* ctx) noexcept override { ready_queue_.push_back(ctx); }

  boost::fibers::context* pick_next() noexcept override {
    if (focus_main_) {
      focus_main_ = false;
      for (auto it = ready_queue_.begin(); it != ready_queue_.end(); ++it) {
        if ((*it)->is_context(boost::fibers::type::main_context)) {
          boost::fibers::context* ctx = *it;
          ready_queue_.erase(it);
          return ctx;
        }
      }
    }
    if (ready_queue_.empty()) return nullptr;
    boost::fibers::context* ctx = ready_queue_.front();
    ready_queue_.pop_front();
    return ctx;
  }

  bool has_ready_fibers() const noexcept override { return !ready_queue_.empty(); }

  // Called by the fiber library when there are no ready fibers.
  // We drive the io_context here so that Asio completion handlers (which
  // call promise::set_value → awakened()) run inline on this thread.
  void suspend_until(std::chrono::steady_clock::time_point const& abs_time) noexcept override {
    if (context::s_asio_context) {
      auto& ctx = context::s_asio_context->ctx;

      // Restart in case a previous stop() drained the context.
      if (ctx.stopped()) ctx.restart();

      // 1. Drain all immediately-ready completions.
      ctx.poll();

      // 2. If still no ready fibers, block for up to the deadline waiting
      //    for the next Asio event (timer expiry, I/O completion, etc.).
      if (ready_queue_.empty()) {
        if ((std::chrono::steady_clock::time_point::max)() == abs_time) {
          // No deadline: wait indefinitely for the next Asio event.
          ctx.run_one();
        } else {
          auto now = std::chrono::steady_clock::now();
          if (abs_time > now) {
            ctx.run_one_for(abs_time - now);
          }
        }
      }
    } else {
      // No Asio context available (e.g., during early init or unit tests that
      // don't create a daemon): fall back to a timed sleep so fiber timers
      // (fiber-sleep-for) still work correctly.
      if ((std::chrono::steady_clock::time_point::max)() != abs_time) {
        std::this_thread::sleep_until(abs_time);
      }
    }
  }

  // No-op: wakeup is driven entirely by Asio completions firing inside
  // suspend_until(), so there is nothing external to signal.
  void notify() noexcept override {}
};

void fiber_set_focus_main(bool enable) {
  auto* sched = boost::fibers::context::active()->get_scheduler();
  auto* priority = dynamic_cast<priority_scheduler*>(sched);
  if (!priority) return;
  priority->set_focus_main(enable);
}

void fiber_scan_stacks() {
  for (const auto& info : context::s_fiber_stacks) {
    uintptr_t bottom = (uintptr_t)info.stack_bottom;
    uintptr_t top = bottom - info.stack_size;
    object_heap_t::current()->shapshot_memory_range(top, bottom);
  }
}

int fiber_live_count() { return context::s_live_fiber_count; }

void fiber_init_scheduler() { boost::fibers::use_scheduling_algorithm<priority_scheduler>(); }
