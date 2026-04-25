// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef ASIO_H_INCLUDED
#define ASIO_H_INCLUDED

#include "core.h"
#include "object.h"

#include <boost/asio.hpp>

// asio_context owns the io_context that is polled inline by the fiber
// scheduler (Asio Round Robin pattern).  There is no background thread;
// the io_context is driven by priority_scheduler::suspend_until().
class asio_context {
 public:
  boost::asio::io_context ctx;
  boost::asio::executor_work_guard<boost::asio::io_context::executor_type> work_guard;
  asio_context() : work_guard(boost::asio::make_work_guard(ctx)) {}
  ~asio_context() { ctx.stop(); }
};

void asio_stream_finalize(scm_port_rec_t* rec);
scm_obj_t asio_get_bytes_async(scm_obj_t port, scm_obj_t bv);
scm_obj_t asio_put_bytes_async(scm_obj_t port, const uint8_t* byte, int bsize);

#endif
