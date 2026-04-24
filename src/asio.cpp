// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "context.h"
#include "object_heap.h"

#include "asio.h"

static boost::asio::posix::stream_descriptor* get_asio_stream(scm_port_rec_t* rec) {
  if (rec->asio_stream == nullptr) {
    int new_fd = ::dup(rec->fd);
    if (new_fd < 0) throw std::system_error(errno, std::generic_category(), "dup failed");
    rec->asio_stream = new boost::asio::posix::stream_descriptor(context::s_asio_context->ctx);
    rec->asio_stream->assign(new_fd);
  }
  return rec->asio_stream;
}

void asio_stream_finalize(scm_port_rec_t* rec) {
  if (rec->asio_stream) {
    boost::system::error_code ec;
    auto unused = rec->asio_stream->close(ec);
    delete rec->asio_stream;
    rec->asio_stream = nullptr;
  }
}

// ---------------------------------------------------------------------------
// asio_put_bytes_async
//
// Asio Round Robin design: the completion handler runs inline on the main
// thread inside priority_scheduler::suspend_until(), so it can set the
// boost::fibers::promise directly without any cross-thread bridge.
// ---------------------------------------------------------------------------
scm_obj_t asio_put_bytes_async(scm_obj_t port, const uint8_t* byte, int bsize) {
  assert(is_port(port));
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  assert(context::s_asio_context);
  assert(rec->fd >= 0);

  // Handle empty write immediately — no I/O needed.
  if (bsize == 0) {
    auto p = std::make_shared<boost::fibers::promise<scm_obj_t>>();
    auto sf = std::make_unique<boost::fibers::shared_future<scm_obj_t>>(p->get_future().share());
    p->set_value(make_fixnum(0));
    return make_future(scm_unspecified, sf.release());
  }

  auto* stream = get_asio_stream(rec);

  // Copy data into a shared buffer so the lambda is self-contained.
  auto data_ptr = std::make_shared<std::vector<uint8_t>>(byte, byte + bsize);

  // Single promise: the Asio handler fulfills it directly (same thread).
  auto res_promise = std::make_shared<boost::fibers::promise<scm_obj_t>>();
  auto res_shared_f = std::make_unique<boost::fibers::shared_future<scm_obj_t>>(res_promise->get_future().share());
  scm_obj_t future_obj = make_future(scm_unspecified, res_shared_f.release());

  context::gc_protect(port);

  boost::asio::async_write(*stream, boost::asio::buffer(*data_ptr),
                           [res_promise, data_ptr, port](const boost::system::error_code& ec, std::size_t n) {
                             // Runs on the main thread inside suspend_until().
                             if (ec) {
                               res_promise->set_exception(std::make_exception_ptr(boost::system::system_error(ec)));
                             } else {
                               res_promise->set_value(make_fixnum(n));
                             }
                             context::gc_unprotect(port);
                           });

  return future_obj;
}

// ---------------------------------------------------------------------------
// asio_get_bytes_async
//
// Same round-robin approach: single promise fulfilled directly by the Asio
// completion handler on the main thread.
// ---------------------------------------------------------------------------
scm_obj_t asio_get_bytes_async(scm_obj_t port, scm_obj_t bv) {
  assert(is_port(port));
  assert(is_u8vector(bv));
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  assert(context::s_asio_context);
  assert(rec->fd >= 0);

  int bsize = u8vector_nsize(bv);

  // Immediate return for empty buffer.
  if (bsize == 0) {
    auto p = std::make_shared<boost::fibers::promise<scm_obj_t>>();
    auto sf = std::make_unique<boost::fibers::shared_future<scm_obj_t>>(p->get_future().share());
    p->set_value(make_fixnum(0));
    return make_future(bv, sf.release());
  }

  auto* stream = get_asio_stream(rec);

  auto res_promise = std::make_shared<boost::fibers::promise<scm_obj_t>>();
  auto res_shared_f = std::make_unique<boost::fibers::shared_future<scm_obj_t>>(res_promise->get_future().share());
  scm_obj_t future_obj = make_future(bv, res_shared_f.release());

  context::gc_protect(future_obj);
  context::gc_protect(port);

  boost::asio::async_read(*stream, boost::asio::buffer(u8vector_elts(bv), bsize),
                          [res_promise, future_obj, port](const boost::system::error_code& ec, std::size_t n) {
                            // Runs on the main thread inside suspend_until().
                            if (ec && ec != boost::asio::error::eof) {
                              res_promise->set_exception(std::make_exception_ptr(boost::system::system_error(ec)));
                            } else if (n == 0) {
                              res_promise->set_value(scm_eof);
                            } else {
                              scm_future_rec_t* fut_rec = (scm_future_rec_t*)to_address(future_obj);
                              if (n == (std::size_t)u8vector_nsize(fut_rec->datum)) {
                                res_promise->set_value(fut_rec->datum);
                              } else {
                                // Partial read: return a correctly-sized bytevector.
                                scm_obj_t new_bv = make_u8vector(n);
                                memcpy(u8vector_elts(new_bv), u8vector_elts(fut_rec->datum), n);
                                object_heap_t::current()->write_barrier(new_bv);
                                fut_rec->datum = new_bv;
                                res_promise->set_value(new_bv);
                              }
                            }
                            context::gc_unprotect(future_obj);
                            context::gc_unprotect(port);
                          });

  return future_obj;
}
