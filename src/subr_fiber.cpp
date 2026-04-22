// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "codegen_aux.h"
#include "context.h"
#include "object_heap.h"
#include "subr.h"

#include <boost/fiber/all.hpp>
#include <chrono>
#include <deque>
#include <thread>

class priority_scheduler : public boost::fibers::algo::algorithm {
 private:
  std::deque<boost::fibers::context*> ready_queue_;
  bool focus_main_ = false;

 public:
  void set_focus_main(bool enable) { focus_main_ = enable; }

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

  void suspend_until(std::chrono::steady_clock::time_point const& suspend_time) noexcept override {
    if (std::chrono::steady_clock::time_point::max() != suspend_time) {
      std::this_thread::sleep_until(suspend_time);
    }
  }

  void notify() noexcept override {}
};

void fiber_set_focus_main(bool enable) {
  auto* sched = boost::fibers::context::active()->get_scheduler();
  auto* priority = dynamic_cast<priority_scheduler*>(sched);
  if (priority) priority->set_focus_main(enable);
}

void scan_fiber_stacks() {
  for (const auto& info : context::s_fiber_stacks) {
    uintptr_t bottom = (uintptr_t)info.stack_bottom;
    uintptr_t top = bottom - info.stack_size;
    object_heap_t::current()->shapshot_memory_range(top, bottom);
  }
}

SUBR subr_fiber(scm_obj_t self, scm_obj_t closure) {
  if (!is_closure(closure)) throw std::runtime_error("fiber: argument must be a procedure");

  scm_obj_t future = make_future(closure, nullptr);
  context::gc_protect(future);

  auto task = boost::fibers::packaged_task<scm_obj_t()>([future]() {
    try {
      scm_future_rec_t* rec = (scm_future_rec_t*)to_address(future);
      scm_obj_t result = c_call_closure_thunk_0(rec->closure);
      context::gc_unprotect(future);
      return result;
    } catch (...) {
      context::gc_unprotect(future);
      throw;
    }
  });

  auto shared_future = task.get_future();
  boost::fibers::fiber(std::allocator_arg, context::s_fiber_stack_allocator, std::move(task)).detach();

  scm_future_rec_t* rec = (scm_future_rec_t*)to_address(future);
  rec->future = new boost::fibers::shared_future<scm_obj_t>(shared_future.share());
  return future;
}

SUBR subr_future_get(scm_obj_t self, scm_obj_t obj) {
  if (!is_future(obj)) throw std::runtime_error("future-get: argument must be a future");
  scm_future_rec_t* rec = (scm_future_rec_t*)to_address(obj);
  auto* future = (boost::fibers::shared_future<scm_obj_t>*)rec->future;
  return future->get();
}

SUBR subr_future_wait(scm_obj_t self, scm_obj_t obj) {
  if (!is_future(obj)) throw std::runtime_error("future-wait: argument must be a future");
  scm_future_rec_t* rec = (scm_future_rec_t*)to_address(obj);
  auto* future = (boost::fibers::shared_future<scm_obj_t>*)rec->future;
  future->wait();
  return scm_unspecified;
}

SUBR subr_future_wait_for(scm_obj_t self, scm_obj_t obj, scm_obj_t msec) {
  if (!is_future(obj)) throw std::runtime_error("future-wait-for: first argument must be a future");
  int64_t ms;
  if (is_fixnum(msec)) {
    ms = fixnum(msec);
  } else if (is_flonum(msec)) {
    ms = (int64_t)flonum(msec);
  } else {
    throw std::runtime_error("future-wait-for: second argument must be a number");
  }
  scm_future_rec_t* rec = (scm_future_rec_t*)to_address(obj);
  auto* fut = (boost::fibers::shared_future<scm_obj_t>*)rec->future;
  if (fut->wait_for(std::chrono::milliseconds(ms)) == boost::fibers::future_status::ready) return scm_false;
  return scm_true;
}

SUBR subr_future_p(scm_obj_t self, scm_obj_t obj) { return is_future(obj) ? scm_true : scm_false; }

SUBR subr_fiber_sleep_for(scm_obj_t self, scm_obj_t msec) {
  int64_t ms;
  if (is_fixnum(msec)) {
    ms = fixnum(msec);
  } else if (is_flonum(msec)) {
    ms = (int64_t)flonum(msec);
  } else {
    throw std::runtime_error("fiber-sleep-for: argument must be a number");
  }
  boost::this_fiber::sleep_for(std::chrono::milliseconds(ms));
  return scm_unspecified;
}

SUBR subr_fiber_yield(scm_obj_t self) {
  boost::this_fiber::yield();
  return scm_unspecified;
}

void init_subr_fiber() {
  boost::fibers::use_scheduling_algorithm<priority_scheduler>();
  auto reg = [](const char* name, void* func, int req, int opt) {
    context::environment_variable_set(make_symbol(name), make_closure(func, req, opt, 0, nullptr, 1));
  };

  reg("fiber", (void*)subr_fiber, 1, 0);
  reg("fiber-yield", (void*)subr_fiber_yield, 0, 0);
  reg("fiber-sleep-for", (void*)subr_fiber_sleep_for, 1, 0);
  reg("future-get", (void*)subr_future_get, 1, 0);
  reg("future-wait", (void*)subr_future_wait, 1, 0);
  reg("future-wait-for", (void*)subr_future_wait_for, 2, 0);
  reg("future?", (void*)subr_future_p, 1, 0);
}
