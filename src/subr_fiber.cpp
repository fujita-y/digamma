// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "codegen_aux.h"
#include "context.h"
#include "object_heap.h"
#include "subr.h"

#include "asio.h"
#include "fiber.h"

#include <boost/asio.hpp>
#include <boost/fiber/all.hpp>
#include <boost/fiber/type.hpp>
#include <chrono>
#include <deque>
#include <thread>

SUBR subr_fiber(scm_obj_t self, scm_obj_t closure) {
  if (!is_closure(closure)) throw std::runtime_error("fiber: argument must be a procedure");

  scm_obj_t future = make_future(closure, nullptr);
  context::gc_protect(future);

  auto task = boost::fibers::packaged_task<scm_obj_t()>([future]() {
    try {
      scm_future_rec_t* rec = (scm_future_rec_t*)to_address(future);
      scm_obj_t result = c_call_closure_thunk_0(rec->datum);
      context::gc_unprotect(future);
      context::s_live_fiber_count--;
      return result;
    } catch (...) {
      context::gc_unprotect(future);
      context::s_live_fiber_count--;
      throw;
    }
  });

  scm_future_rec_t* rec = (scm_future_rec_t*)to_address(future);
  rec->future = new boost::fibers::shared_future<scm_obj_t>(task.get_future().share());

  context::s_live_fiber_count++;
  boost::fibers::fiber(std::allocator_arg, context::s_fiber_stack_allocator, std::move(task)).detach();

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
  auto reg = [](const char* name, void* func, int req, bool opt) {
    context::environment_variable_set(make_symbol(name), make_closure(func, req, opt ? 1 : 0, 0, nullptr, 1));
  };

  reg("fiber", (void*)subr_fiber, 1, false);
  reg("fiber-yield", (void*)subr_fiber_yield, 0, false);
  reg("fiber-sleep-for", (void*)subr_fiber_sleep_for, 1, false);
  reg("future-get", (void*)subr_future_get, 1, false);
  reg("future-wait", (void*)subr_future_wait, 1, false);
  reg("future-wait-for", (void*)subr_future_wait_for, 2, false);
  reg("future?", (void*)subr_future_p, 1, false);
}
