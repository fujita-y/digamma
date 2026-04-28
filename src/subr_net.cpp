// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

// ---------------------------------------------------------------------------
// subr_net.cpp — network I/O subrs
//
// (https-get       url port) → string
// (https-get-async url port) → future
//
// Both use Boost.Beast over the shared Asio io_context (priority_scheduler
// "Asio Round Robin" design).  Each async step suspends the calling fiber
// via a boost::fibers::promise fulfilled inside the Asio completion handler,
// so the OS thread is never blocked.
//
// async chain: resolve → connect → TLS handshake → write → read → shutdown
// ---------------------------------------------------------------------------

#include "core.h"
#include "object.h"
#include "context.h"
#include "fiber.h"
#include "subr.h"

#include "asio.h"

#include <boost/asio.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/beast.hpp>
#include <boost/beast/ssl.hpp>
#include <boost/fiber/all.hpp>

#include <chrono>
#include <string>

namespace net = boost::asio;
namespace ssl = boost::asio::ssl;
namespace beast = boost::beast;
namespace http = boost::beast::http;

using error_code = boost::system::error_code;

// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// fiber_await
//
// Suspend the calling fiber until an async operation completes, then return
// the first argument passed to the completion handler (typically error_code).
// Works for any handler signature: extra trailing args are discarded.
// ---------------------------------------------------------------------------
template <typename R = error_code, typename AsyncOp> static R fiber_await(AsyncOp&& op) {
  boost::fibers::promise<R> prom;
  auto fut = prom.get_future();
  std::forward<AsyncOp>(op)([p = std::move(prom)](const R& v, auto&&...) mutable { p.set_value(v); });
  return fut.get();
}

// Specialisation for async_resolve which returns (ec, results) together.
static std::pair<error_code, net::ip::tcp::resolver::results_type> fiber_await_resolve(net::ip::tcp::resolver& resolver, const std::string& host,
                                                                                       const std::string& port) {
  using Results = net::ip::tcp::resolver::results_type;
  using Pair = std::pair<error_code, Results>;

  boost::fibers::promise<Pair> prom;
  auto fut = prom.get_future();
  resolver.async_resolve(host, port, [p = std::move(prom)](const error_code& ec, Results r) mutable { p.set_value({ec, std::move(r)}); });
  return fut.get();
}

// ---------------------------------------------------------------------------
// parse_url_port
//
// Shared argument validation for both subrs.  Strips scheme prefix, isolates
// the bare hostname, and converts port to a string.  Throws std::runtime_error
// with the given prefix on invalid input.
// ---------------------------------------------------------------------------
static void parse_url_port(const char* who, scm_obj_t url_obj, scm_obj_t port_obj, std::string& host, std::string& port_str) {
  if (!is_string(url_obj)) throw std::runtime_error(std::string(who) + ": first argument must be a string (url)");

  std::string url(reinterpret_cast<const char*>(string_name(url_obj)));
  if (url.rfind("https://", 0) == 0)
    url.erase(0, 8);
  else if (url.rfind("http://", 0) == 0)
    url.erase(0, 7);

  auto slash = url.find('/');
  host = (slash != std::string::npos) ? url.substr(0, slash) : url;
  if (host.empty()) throw std::runtime_error(std::string(who) + ": url must not be empty");

  if (is_fixnum(port_obj)) {
    intptr_t pn = fixnum(port_obj);
    if (pn <= 0 || pn > 65535) throw std::runtime_error(std::string(who) + ": port must be in range 1-65535");
    port_str = std::to_string(pn);
  } else if (is_string(port_obj)) {
    port_str = reinterpret_cast<const char*>(string_name(port_obj));
  } else {
    throw std::runtime_error(std::string(who) + ": second argument must be a fixnum or string (port)");
  }
}

// ---------------------------------------------------------------------------
// https_get_impl
//
// Performs a full HTTPS GET on the calling fiber.  Every async step is
// awaited via fiber_await so no OS thread is ever blocked.  Per-step
// deadlines prevent hangs on unreachable/filtered ports.
//
// Throws std::runtime_error (wrapping the underlying Boost error) on any
// network or protocol failure so callers need no extra try/catch.
// ---------------------------------------------------------------------------
static std::string https_get_impl(const std::string& host, const std::string& port, std::chrono::seconds timeout = std::chrono::seconds(30)) {
  assert(context::s_asio_context && "https-get requires an active Asio context");
  net::io_context& ioc = context::s_asio_context->ctx;

  ssl::context ssl_ctx(ssl::context::tlsv12_client);
  ssl_ctx.set_default_verify_paths();
  ssl_ctx.set_verify_mode(ssl::verify_peer);

  net::ip::tcp::resolver resolver(ioc);
  beast::ssl_stream<beast::tcp_stream> stream(ioc, ssl_ctx);

  // SNI is required by most HTTPS servers.
  if (!SSL_set_tlsext_host_name(stream.native_handle(), host.c_str())) {
    error_code ec(static_cast<int>(::ERR_get_error()), net::error::get_ssl_category());
    throw boost::system::system_error(ec);
  }

  auto throw_if = [](error_code ec) {
    if (ec) throw boost::system::system_error(ec);
  };
  auto& tcp = beast::get_lowest_layer(stream);

  try {
    // 1. Resolve
    auto [ec_r, endpoints] = fiber_await_resolve(resolver, host, port);
    throw_if(ec_r);

    // 2. Connect (TCP)
    tcp.expires_after(timeout);
    throw_if(fiber_await([&](auto h) { tcp.async_connect(endpoints, std::move(h)); }));

    // 3. TLS handshake
    tcp.expires_after(timeout);
    throw_if(fiber_await([&](auto h) { stream.async_handshake(ssl::stream_base::client, std::move(h)); }));

    // 4. Send HTTP GET
    http::request<http::empty_body> req(http::verb::get, "/", 11);
    req.set(http::field::host, host);
    req.set(http::field::user_agent, "nanos/1.0 beast");
    req.set(http::field::accept, "*/*");
    req.prepare_payload();

    tcp.expires_after(timeout);
    throw_if(fiber_await([&](auto h) { http::async_write(stream, req, std::move(h)); }));

    // 5. Read response
    beast::flat_buffer buf;
    http::response<http::string_body> res;

    tcp.expires_after(timeout);
    error_code ec_r2 = fiber_await([&](auto h) { http::async_read(stream, buf, res, std::move(h)); });
    if (ec_r2 && ec_r2 != net::error::eof) throw boost::system::system_error(ec_r2);

    // 6. TLS shutdown (best-effort — SSL_R_SHORT_READ is normal)
    tcp.expires_never();
    auto nouse = fiber_await([&](auto h) { stream.async_shutdown(std::move(h)); });

    return std::move(res.body());

  } catch (const boost::system::system_error& e) {
    throw std::runtime_error(e.what());
  }
}

// ---------------------------------------------------------------------------
// subr_https_get — (https-get url port) → string
//
// Suspends the calling fiber for the full request; no OS thread is blocked.
// ---------------------------------------------------------------------------
SUBR subr_https_get(scm_obj_t self, scm_obj_t url_obj, scm_obj_t port_obj) {
  std::string host, port;
  parse_url_port("https-get", url_obj, port_obj, host, port);
  return make_string(https_get_impl(host, port).c_str());
}

// ---------------------------------------------------------------------------
// subr_https_get_async — (https-get-async url port) → future
//
// Returns a Scheme future immediately; a detached fiber performs the request.
// Mirrors the subr_fiber pattern: packaged_task → shared_future in
// scm_future_rec_t, with the future GC-rooted until the fiber exits.
// ---------------------------------------------------------------------------
SUBR subr_https_get_async(scm_obj_t self, scm_obj_t url_obj, scm_obj_t port_obj) {
  std::string host, port;
  parse_url_port("https-get-async", url_obj, port_obj, host, port);

  scm_obj_t future_obj = make_future(scm_unspecified, nullptr);
  context::gc_protect(future_obj);

  auto task = boost::fibers::packaged_task<scm_obj_t()>([future_obj, host, port]() mutable -> scm_obj_t {
    fiber_unwind_guard guard(future_obj);

    return make_string(https_get_impl(host, port).c_str());
    // Any exception propagates into the future automatically.
  });

  auto* rec = static_cast<scm_future_rec_t*>(to_address(future_obj));
  rec->future = new boost::fibers::shared_future<scm_obj_t>(task.get_future().share());

  context::s_live_fiber_count++;
  boost::fibers::fiber(std::allocator_arg, context::s_fiber_stack_allocator, std::move(task)).detach();

  return future_obj;
}

// ---------------------------------------------------------------------------
// init_subr_net — register network subrs in the Scheme environment
// ---------------------------------------------------------------------------
void init_subr_net() {
  auto reg = [](const char* name, void* func, int req, bool opt) {
    context::environment_variable_set(make_symbol(name), make_closure(func, req, opt ? 1 : 0, 0, nullptr, 1));
  };

  reg("https-get", (void*)subr_https_get, 2, false);
  reg("https-get-async", (void*)subr_https_get_async, 2, false);
}
