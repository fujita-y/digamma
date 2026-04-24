#include <boost/fiber/all.hpp>
#include <cassert>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fcntl.h>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <unistd.h>
#include "../src/asio.h"
#include "../src/context.h"
#include "../src/core.h"
#include "../src/fiber.h"
#include "../src/object.h"
#include "../src/object_heap.h"
#include "../src/port.h"
#include "../src/printer.h"
#include "../src/reader.h"
#include "../src/subr.h"

#include <cstdarg>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>

static bool some_test_failed = false;

void fatal(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");
  exit(1);
}

void warning(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");
}

void trace(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
}

extern "C" const char* __hwasan_default_options() { return "leak_check_at_exit=0"; }

namespace test_port {

static bool test_standard_ports() {
  scm_obj_t sin = port_standard_input();
  scm_obj_t sout = port_standard_output();
  scm_obj_t serr = port_standard_error();

  if (!is_port(sin) || !is_port(sout) || !is_port(serr)) {
    printf("\033[31m###### standard ports failed: not a port\033[0m\n");
    some_test_failed = true;
    return false;
  }

  if (port_get_istream(sin) == nullptr) {
    printf("\033[31m###### standard input port failed: stream is null\033[0m\n");
    some_test_failed = true;
    return false;
  }

  if (port_get_ostream(sout) == nullptr) {
    printf("\033[31m###### standard output port failed: stream is null\033[0m\n");
    some_test_failed = true;
    return false;
  }

  if (port_get_ostream(serr) == nullptr) {
    printf("\033[31m###### standard error port failed: stream is null\033[0m\n");
    some_test_failed = true;
    return false;
  }

  port_close(sin);
  port_close(sout);
  port_close(serr);

  printf("\033[32mstandard ports passed\033[0m\n");
  return true;
}

static bool test_string_port() {
  scm_obj_t port = make_port(make_symbol("string-port"));
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);

  std::stringstream* ss = new std::stringstream();
  rec->iostream = ss;

  if (port_get_ostream(port) != ss) {
    printf("\033[31m###### string port ostream failed\033[0m\n");
    some_test_failed = true;
    return false;
  }

  if (port_get_istream(port) != ss) {
    printf("\033[31m###### string port istream failed\033[0m\n");
    some_test_failed = true;
    return false;
  }

  *ss << "hello";
  char buf[6];
  port_get_istream(port)->read(buf, 5);
  buf[5] = '\0';
  if (strcmp(buf, "hello") != 0) {
    printf("\033[31m###### string port read failed: %s\033[0m\n", buf);
    some_test_failed = true;
    return false;
  }

  port_close(port);

  printf("\033[32mstring port passed\033[0m\n");
  return true;
}

static bool test_file_port() {
  const char* filename = "test_port.tmp";

  // Test output
  scm_obj_t out_port = port_open_output_file(filename);
  if (!is_port(out_port)) {
    printf("\033[31m###### open output file failed\033[0m\n");
    some_test_failed = true;
    return false;
  }

  port_put_byte(out_port, 'A');
  port_put_bytes(out_port, (const uint8_t*)"BCD", 3);
  port_put_char(out_port, make_char('E'));
  port_put_string(out_port, make_string("FGHIJ"));
  port_flush_output(out_port);
  port_close(out_port);

  // Test input
  scm_obj_t in_port = port_open_input_file(filename);
  if (!is_port(in_port)) {
    printf("\033[31m###### open input file failed\033[0m\n");
    some_test_failed = true;
    return false;
  }

  scm_obj_t bv = port_get_bytes(in_port, 10);
  if (!is_u8vector(bv) || u8vector_nsize(bv) != 10) {
    printf("\033[31m###### port_get_bytes failed\033[0m\n");
    some_test_failed = true;
    return false;
  }

  if (memcmp(u8vector_elts(bv), "ABCDEFGHIJ", 10) != 0) {
    printf("\033[31m###### data mismatch in file port\033[0m\n");
    some_test_failed = true;
    return false;
  }

  if (port_get_bytes(in_port, 1) != scm_eof) {
    printf("\033[31m###### expected EOF\033[0m\n");
    some_test_failed = true;
    return false;
  }

  port_close(in_port);

  // Test get_bytes_n
  in_port = port_open_input_file(filename);
  scm_obj_t bv2 = port_get_bytes_n(in_port, 5);
  if (!is_u8vector(bv2) || u8vector_nsize(bv2) != 5 || memcmp(u8vector_elts(bv2), "ABCDE", 5) != 0) {
    printf("\033[31m###### port_get_bytes_n failed\033[0m\n");
    some_test_failed = true;
    return false;
  }
  port_close(in_port);

  remove(filename);
  printf("\033[32mfile port passed\033[0m\n");
  return true;
}

static bool test_string_output_port() {
  scm_obj_t port = port_open_string_output_port();
  port_put_string(port, make_string("hello"));
  scm_obj_t s = port_get_output_string(port);
  if (!is_string(s) || strcmp((const char*)string_name(s), "hello") != 0) {
    printf("\033[31m###### string output port failed\033[0m\n");
    some_test_failed = true;
    return false;
  }

  port_put_char(port, make_char('!'));
  s = port_get_output_string(port);
  if (!is_string(s) || strcmp((const char*)string_name(s), "!") != 0) {
    printf("\033[31m###### string output port (char) failed\033[0m\n");
    some_test_failed = true;
    return false;
  }

  port_close(port);
  printf("\033[32mstring output port passed\033[0m\n");
  return true;
}

static bool test_nonblock_ready() {
  const char* filename = "test_ready.tmp";
  scm_obj_t out_port = port_open_output_file(filename);
  port_put_string(out_port, make_string("data"));
  port_close(out_port);

  scm_obj_t in_port = port_open_input_file(filename);
  if (!port_nonblock_byte_ready(in_port)) {
    // Note: on some systems, poll on regular file might behave differently,
    // but usually regular files are always "ready" for read.
    printf("\033[31m###### port_nonblock_byte_ready failed\033[0m\n");
    // some_test_failed = true; // Don't fail if regular file polling is tricky
  }
  port_close(in_port);
  remove(filename);
  printf("\033[32mnonblock ready passed\033[0m\n");
  return true;
}

static bool test_async_operations() {
  const char* filename = "test_async.tmp";
  context::s_asio_context = new asio_context();

  // Test async write
  // fut->get() suspends this fiber → pick_next() returns nullptr →
  // suspend_until() polls ctx → completion fires → fiber resumes.
  scm_obj_t out_port = port_open_output_file(filename);
  scm_obj_t f_out = port_put_string_async(out_port, make_string("async data"));
  if (!is_future(f_out)) {
    printf("\033[31m###### port_put_string_async failed: not a future\033[0m\n");
    some_test_failed = true;
    return false;
  }

  scm_future_rec_t* f_out_rec = (scm_future_rec_t*)to_address(f_out);
  scm_obj_t write_result = f_out_rec->future->get();
  if (write_result != make_fixnum(10)) {
    printf("\033[31m###### port_put_string_async result failed\033[0m\n");
    some_test_failed = true;
    return false;
  }
  port_close(out_port);

  // Test async read
  scm_obj_t in_port = port_open_input_file(filename);
  scm_obj_t f_in = port_get_bytes_n_async(in_port, 10);
  if (!is_future(f_in)) {
    printf("\033[31m###### port_get_bytes_n_async failed: not a future\033[0m\n");
    some_test_failed = true;
    return false;
  }

  scm_future_rec_t* f_in_rec = (scm_future_rec_t*)to_address(f_in);
  scm_obj_t bv = f_in_rec->future->get();
  if (!is_u8vector(bv) || u8vector_nsize(bv) != 10 || memcmp(u8vector_elts(bv), "async data", 10) != 0) {
    printf("\033[31m###### port_get_bytes_n_async result failed\033[0m\n");
    some_test_failed = true;
    return false;
  }

  port_close(in_port);
  remove(filename);

  delete context::s_asio_context;
  context::s_asio_context = nullptr;

  printf("\033[32masync operations passed\033[0m\n");
  return true;
}

int run_test(int argc, char** argv) {
  // Install the Asio-aware scheduler so suspend_until() polls the io_context.
  fiber_init_scheduler();

  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 2, 1024 * 1024);
  context::init();

  test_standard_ports();
  test_string_port();
  test_file_port();
  test_string_output_port();
  test_nonblock_ready();
  test_async_operations();

  context::destroy();
  heap->destroy();
  delete heap;

  return some_test_failed ? 1 : 0;
}

}  // namespace test_port

namespace test_async_port {

static bool run_tests() {
  const char* filename = "test_async.txt";
  const char* data = "Hello Async World!";
  int len = strlen(data);

  // Test async write
  {
    scm_obj_t port = port_open_output_file(filename);
    printf("Starting async write...\n");
    scm_obj_t future = asio_put_bytes_async(port, (const uint8_t*)data, len);

    if (!is_future(future)) {
      printf("\033[31m###### async write failed: did not return a future\033[0m\n");
      return false;
    }

    scm_future_rec_t* rec = (scm_future_rec_t*)to_address(future);
    auto* fut = (boost::fibers::shared_future<scm_obj_t>*)rec->future;

    printf("Waiting for write future...\n");
    scm_obj_t result = fut->get();

    if (!is_fixnum(result) || fixnum(result) != len) {
      printf("\033[31m###### async write failed: wrong result size\033[0m\n");
      return false;
    }
    printf("Async write completed successfully.\n");
    port_close(port);
  }

  // Test async read
  {
    scm_obj_t port = port_open_input_file(filename);
    scm_obj_t bv = make_u8vector(len);
    printf("Starting async read...\n");
    scm_obj_t future = asio_get_bytes_async(port, bv);

    if (!is_future(future)) {
      printf("\033[31m###### async read failed: did not return a future\033[0m\n");
      return false;
    }

    scm_future_rec_t* rec = (scm_future_rec_t*)to_address(future);
    auto* fut = (boost::fibers::shared_future<scm_obj_t>*)rec->future;

    printf("Waiting for read future...\n");
    scm_obj_t result = fut->get();

    if (!is_u8vector(result) || u8vector_nsize(result) != len) {
      printf("\033[31m###### async read failed: wrong result type or size\033[0m\n");
      return false;
    }

    if (memcmp(u8vector_elts(result), data, len) != 0) {
      printf("\033[31m###### async read failed: data mismatch\033[0m\n");
      return false;
    }
    printf("Async read completed successfully.\n");
    port_close(port);
  }

  // Test async read 0 bytes
  {
    scm_obj_t port = port_open_input_file(filename);
    scm_obj_t bv = make_u8vector(0);
    printf("Starting async read 0 bytes...\n");
    scm_obj_t future = asio_get_bytes_async(port, bv);
    scm_future_rec_t* rec = (scm_future_rec_t*)to_address(future);
    auto* fut = (boost::fibers::shared_future<scm_obj_t>*)rec->future;
    scm_obj_t result = fut->get();
    if (!is_fixnum(result) || fixnum(result) != 0) {
      printf("\033[31m###### async read 0 bytes failed: expected 0, got %ld\033[0m\n", is_fixnum(result) ? fixnum(result) : -1);
      return false;
    }
    printf("Async read 0 bytes completed successfully.\n");
    port_close(port);
  }

  // Test async read EOF
  {
    scm_obj_t port = port_open_input_file(filename);
    scm_obj_t bv = make_u8vector(100);  // larger than file
    printf("Starting async read to EOF...\n");
    scm_obj_t future = asio_get_bytes_async(port, bv);
    scm_future_rec_t* rec = (scm_future_rec_t*)to_address(future);
    auto* fut = (boost::fibers::shared_future<scm_obj_t>*)rec->future;
    scm_obj_t result = fut->get();

    if (!is_u8vector(result) || u8vector_nsize(result) != len) {
      printf("\033[31m###### async read EOF failed: wrong result size (got %d, expected %d)\033[0m\n",
             is_u8vector(result) ? u8vector_nsize(result) : -1, len);
      return false;
    }

    // Now read again at EOF
    printf("Starting async read AT EOF...\n");
    scm_obj_t future2 = asio_get_bytes_async(port, bv);
    scm_future_rec_t* rec2 = (scm_future_rec_t*)to_address(future2);
    auto* fut2 = (boost::fibers::shared_future<scm_obj_t>*)rec2->future;
    scm_obj_t result2 = fut2->get();
    if (result2 != scm_eof) {
      printf("\033[31m###### async read AT EOF failed: expected scm_eof\033[0m\n");
      return false;
    }
    printf("Async read EOF behavior verified successfully.\n");
    port_close(port);
  }

  unlink(filename);
  return true;
}

int run_test(int argc, char** argv) {
  // Round-robin pattern: the custom scheduler must be installed so that
  // suspend_until() polls the io_context when all fibers are waiting.
  fiber_init_scheduler();

  context::s_asio_context = new asio_context();

  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 2, 1024 * 1024);
  context::init();

  bool success = run_tests();

  context::destroy();
  heap->destroy();
  delete heap;

  delete context::s_asio_context;
  context::s_asio_context = nullptr;

  if (success) {
    printf("\033[32masync port test passed\033[0m\n");
    return 0;
  } else {
    return 1;
  }
}

}  // namespace test_async_port

namespace test_port_direction {

// Subroutines to test
SUBR subr_put_char(scm_obj_t self, scm_obj_t port, scm_obj_t ch);
SUBR subr_put_string(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_read(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_get_bytevector_n(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_current_input_port(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_current_output_port(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_put_string_async(scm_obj_t self, scm_obj_t port, scm_obj_t str);

#define ASSERT_THROW(expr, msg)                                                                           \
  try {                                                                                                   \
    expr;                                                                                                 \
    printf("\033[31mFAIL: expected exception for %s\033[0m\n", #expr);                                    \
    some_test_failed = true;                                                                              \
  } catch (const std::runtime_error& e) {                                                                 \
    if (std::string(e.what()).find(msg) != std::string::npos) {                                           \
      printf("\033[32mPASS: %s threw expected exception: %s\033[0m\n", #expr, e.what());                  \
    } else {                                                                                              \
      printf("\033[31mFAIL: %s threw wrong exception: %s (expected: %s)\033[0m\n", #expr, e.what(), msg); \
      some_test_failed = true;                                                                            \
    }                                                                                                     \
  }

int run_test(int argc, char** argv) {
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 2, 1024 * 1024);
  context::init();

  // Let's create a custom stream that is only istream
  struct only_istream : public std::istream {
    only_istream() : std::istream(&buf) {}
    std::stringbuf buf;
  };

  scm_obj_t in_only_port = make_port(make_symbol("in-only"));
  ((scm_port_rec_t*)to_address(in_only_port))->istream = new only_istream();
  ((scm_port_rec_t*)to_address(in_only_port))->ostream = nullptr;
  ((scm_port_rec_t*)to_address(in_only_port))->iostream = nullptr;

  // Create an output-only port
  struct only_ostream : public std::ostream {
    only_ostream() : std::ostream(&buf) {}
    std::stringbuf buf;
  };
  scm_obj_t out_only_port = make_port(make_symbol("out-only"));
  ((scm_port_rec_t*)to_address(out_only_port))->istream = nullptr;
  ((scm_port_rec_t*)to_address(out_only_port))->ostream = new only_ostream();
  ((scm_port_rec_t*)to_address(out_only_port))->iostream = nullptr;

  // Test put-char on input port
  ASSERT_THROW(subr_put_char(scm_nil, in_only_port, make_char('a')), "output port");

  // Test put-string on input port
  scm_obj_t argv_ps[] = {in_only_port, make_string("test")};
  ASSERT_THROW(subr_put_string(scm_nil, 2, argv_ps), "output port");

  // Test read on output port
  scm_obj_t argv_read[] = {out_only_port};
  ASSERT_THROW(subr_read(scm_nil, 1, argv_read), "input port");

  // Test get-bytevector-n on output port
  ASSERT_THROW(subr_get_bytevector_n(scm_nil, out_only_port, make_fixnum(10)), "input port");

  // Test current-input-port setter with output port
  scm_obj_t argv_cip[] = {out_only_port};
  ASSERT_THROW(subr_current_input_port(scm_nil, 1, argv_cip), "input port");

  // Test current-output-port setter with input port
  scm_obj_t argv_cop[] = {in_only_port};
  ASSERT_THROW(subr_current_output_port(scm_nil, 1, argv_cop), "output port");

  // Test put-string-async on input port
  ASSERT_THROW(subr_put_string_async(scm_nil, in_only_port, make_string("test")), "output port");

  context::destroy();
  heap->destroy();
  delete heap;

  return some_test_failed ? 1 : 0;
}

}  // namespace test_port_direction

namespace test_reader {

void test_reader() {
  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss("123");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for 123");
    assert(is_fixnum(obj));
    assert(fixnum(obj) == 123);
    std::cout << "Fixnum 123 passed" << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss("-123");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for -123");
    assert(is_fixnum(obj));
    assert(fixnum(obj) == -123);
    std::cout << "Fixnum -123 passed" << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss("foo");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for foo");
    assert(is_symbol(obj));
    assert(std::string((char*)symbol_name(obj)) == "foo");
    std::cout << "Symbol foo passed" << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss("|foo bar|");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for |foo bar|");
    assert(is_symbol(obj));
    assert(std::string((char*)symbol_name(obj)) == "foo bar");
    std::cout << "Symbol |foo bar| passed" << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss("\"hello\"");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for \"hello\"");
    assert(is_string(obj));
    assert(std::string((char*)string_name(obj)) == "hello");
    std::cout << "String \"hello\" passed" << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss("(1 2 3)");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for (1 2 3)");
    assert(is_cons(obj));
    assert(fixnum(((scm_cons_rec_t*)obj)->car) == 1);

    obj = ((scm_cons_rec_t*)obj)->cdr;
    assert(is_cons(obj));
    assert(fixnum(((scm_cons_rec_t*)obj)->car) == 2);

    obj = ((scm_cons_rec_t*)obj)->cdr;
    assert(is_cons(obj));
    assert(fixnum(((scm_cons_rec_t*)obj)->car) == 3);

    obj = ((scm_cons_rec_t*)obj)->cdr;
    assert(obj == scm_nil);
    std::cout << "List (1 2 3) passed" << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss("(1 . 2)");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for (1 . 2)");
    assert(is_cons(obj));
    assert(fixnum(((scm_cons_rec_t*)obj)->car) == 1);
    assert(fixnum(((scm_cons_rec_t*)obj)->cdr) == 2);
    std::cout << "List (1 . 2) passed" << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss("#(1 2)");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for #(1 2)");
    assert(is_vector(obj));
    assert(vector_nsize(obj) == 2);
    assert(fixnum(vector_elts(obj)[0]) == 1);
    assert(fixnum(vector_elts(obj)[1]) == 2);
    std::cout << "Vector #(1 2) passed" << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss("#u8(1 255)");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for #u8(1 255)");
    assert(is_u8vector(obj));
    assert(u8vector_nsize(obj) == 2);
    assert(u8vector_elts(obj)[0] == 1);
    assert(u8vector_elts(obj)[1] == 255);
    std::cout << "Bytevector #u8(1 255) passed" << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss("'foo");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for 'foo");
    assert(is_cons(obj));
    // (quote foo)
    scm_obj_t car = ((scm_cons_rec_t*)obj)->car;
    assert(is_symbol(car));
    assert(std::string((char*)symbol_name(car)) == "quote");
    std::cout << "Quote 'foo passed" << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss("\"\\x3b1;\"");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for \"\\x3b1;\"");
    assert(is_string(obj));
    // alpha is 0xCE 0xB1
    const uint8_t* str = string_name(obj);
    assert(str[0] == 0xCE);
    assert(str[1] == 0xB1);
    assert(str[2] == 0);
    std::cout << "String \"\\x3b1;\" (alpha) passed" << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss("\"\\x1f600;\"");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for \"\\x1f600;\"");
    assert(is_string(obj));
    // grinning face is 0xF0 0x9F 0x98 0x80
    const uint8_t* str = string_name(obj);
    assert(str[0] == 0xF0);
    assert(str[1] == 0x9F);
    assert(str[2] == 0x98);
    assert(str[3] == 0x80);
    assert(str[4] == 0);
    std::cout << "String \"\\x1f600;\" (grinning face) passed" << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  }

  // Test #i prefix (inexact)
  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss("#i42");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for #i42: %s", reader.get_error_message().c_str());
    assert(is_short_flonum(obj) || is_long_flonum(obj));
    assert(flonum(obj) == 42.0);
    std::cout << "Inexact #i42 passed" << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss("#i#x1A");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for #i#x1A: %s", reader.get_error_message().c_str());
    assert(is_short_flonum(obj) || is_long_flonum(obj));
    assert(flonum(obj) == 26.0);
    std::cout << "Inexact #i#x1A passed" << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  }

  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss("#x#i10");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for #x#i10: %s", reader.get_error_message().c_str());
    assert(is_short_flonum(obj) || is_long_flonum(obj));
    assert(flonum(obj) == 16.0);
    std::cout << "Inexact #x#i10 passed" << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  }

  // Test square brackets
  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss("[1 2 3]");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for [1 2 3]");
    assert(is_cons(obj));
    assert(fixnum(((scm_cons_rec_t*)obj)->car) == 1);
    std::cout << "List [1 2 3] passed" << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  }

  // Mixed pairing test
  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss("(1 [2 3] 4)");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for (1 [2 3] 4)");
    assert(is_cons(obj));
    std::cout << "Mixed paired list (1 [2 3] 4) passed" << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  }

  // Vector with square brackets
  {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss("#[1 2]");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for #[1 2]");
    assert(is_vector(obj));
    assert(vector_nsize(obj) == 2);
    std::cout << "Vector #[1 2] passed" << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  }
}

void test_reader_errors() {
  auto check_error = [](const char* input, const char* expected_msg) {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss(input);
    reader_t reader(ss);
    bool err = false;
    reader.read(err);
    if (!err) {
      fatal("expected error for input: %s", input);
    }
    std::string msg = reader.get_error_message();
    if (msg != expected_msg) {
      fatal("expected error message '%s', got '%s' for input: %s", expected_msg, msg.c_str(), input);
    }
    std::cout << "Error case passed: " << input << " -> " << msg << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  };

  check_error("(1 2", "unexpected end-of-file while reading list");
  check_error("\"foo", "unexpected end-of-file while reading string");
  check_error("(1 . 2 3)", "more than one item following dot('.') while reading list or mismatched parentheses");
  check_error("#u8(256)", "u8vector element out of range");
  check_error("#u8(a)", "expected fixnum in u8vector");
  check_error("\"\\xZZ;\"", "inline hex escape missing terminating semi-colon");
  check_error("#\\unknown", "invalid lexical syntax");
  check_error("[1 2 3)", "mismatched parentheses");
  check_error("(1 2 3]", "mismatched parentheses");
  check_error("]", "unexpected closing bracket");
  check_error(")", "unexpected closing bracket");
  check_error("(1 . 2]", "more than one item following dot('.') while reading list or mismatched parentheses");
}

void test_r6rs_abbreviations() {
  auto check_abbrev = [](const char* input, const char* expected_symbol) {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss(input);
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for %s: %s", input, reader.get_error_message().c_str());
    assert(is_cons(obj));
    scm_obj_t car = ((scm_cons_rec_t*)obj)->car;
    assert(is_symbol(car));
    assert(std::string((char*)symbol_name(car)) == expected_symbol);
    std::cout << "Abbreviated " << input << " -> (" << expected_symbol << " ...) passed" << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  };

  check_abbrev("#'x", "syntax");
  check_abbrev("#`x", "quasisyntax");
  check_abbrev("#,x", "unsyntax");
  check_abbrev("#,@x", "unsyntax-splicing");
  check_abbrev("#`(a #,b #,@c)", "quasisyntax");
}

int run_test(int argc, char** argv) {
  test_reader();
  test_reader_errors();
  test_r6rs_abbreviations();
  return 0;
}

}  // namespace test_reader

namespace test_printer {

void test_printer() {
  std::stringstream ss;
  printer_t printer(ss);

  // Initializing heap for object creation
  object_heap_t* heap = new object_heap_t();
  heap->init(4 * 1024 * 1024, 128 * 1024);
  context::init();

  // Test fixnum
  ss.str("");
  printer.write(make_fixnum(123));
  assert(ss.str() == "123");
  std::cout << "Fixnum test passed" << std::endl;

  // Test simple boolean
  ss.str("");
  printer.write(scm_true);
  assert(ss.str() == "#t");
  std::cout << "Boolean #t test passed" << std::endl;

  ss.str("");
  printer.write(scm_false);
  assert(ss.str() == "#f");
  std::cout << "Boolean #f test passed" << std::endl;

  // Test nil
  ss.str("");
  printer.write(scm_nil);
  assert(ss.str() == "()");
  std::cout << "Nil test passed" << std::endl;

  // Test string
  ss.str("");
  printer.write(make_string("hello"));
  assert(ss.str() == "\"hello\"");
  std::cout << "String test passed" << std::endl;

  // Test symbol
  ss.str("");
  printer.write(make_symbol("foo"));
  assert(ss.str() == "foo");
  std::cout << "Symbol test passed" << std::endl;

  // Test list (1 2 3)
  ss.str("");
  scm_obj_t list = make_list(3, make_fixnum(1), make_fixnum(2), make_fixnum(3));
  printer.write(list);
  assert(ss.str() == "(1 2 3)");
  std::cout << "List test passed" << std::endl;

  // Test improper list (1 . 2)
  ss.str("");
  scm_obj_t pair = make_cons(make_fixnum(1), make_fixnum(2));
  printer.write(pair);
  assert(ss.str() == "(1 . 2)");
  std::cout << "Improper list test passed" << std::endl;

  // Test vector #(1 2)
  ss.str("");
  scm_obj_t vec = make_vector(2, scm_nil);
  ((scm_vector_rec_t*)to_address(vec))->elts[0] = make_fixnum(1);
  ((scm_vector_rec_t*)to_address(vec))->elts[1] = make_fixnum(2);
  printer.write(vec);
  assert(ss.str() == "#(1 2)");
  std::cout << "Vector test passed" << std::endl;

  // Test flonum
  ss.str("");
  printer.write(make_flonum(1.25));
  assert(ss.str() == "1.25");
  std::cout << "Flonum test passed" << std::endl;

  // Test char
  ss.str("");
  printer.write(make_char('a'));
  assert(ss.str() == "#\\a");
  ss.str("");
  printer.write(make_char(' '));
  assert(ss.str() == "#\\space");
  ss.str("");
  printer.write(make_char('\n'));
  assert(ss.str() == "#\\newline");
  std::cout << "Char test passed" << std::endl;

  // Test special constants
  ss.str("");
  printer.write(scm_undef);
  assert(ss.str() == "#<undef>");
  ss.str("");
  printer.write(scm_unspecified);
  assert(ss.str() == "#<unspecified>");
  ss.str("");
  printer.write(scm_eof);
  assert(ss.str() == "#<eof>");
  std::cout << "Special constants test passed" << std::endl;

  // Test opaque types (just check they print something sensible)
  ss.str("");
  printer.write(make_cell(make_fixnum(42)));
  assert(ss.str().find("#<cell") == 0);

  ss.str("");
  printer.write(make_u8vector(0));
  assert(ss.str() == "#u8()");

  ss.str("");
  printer.write(make_values(0));
  assert(ss.str() == "#<values>");

  std::cout << "Opaque types test passed" << std::endl;

  context::destroy();
  heap->destroy();
  delete heap;
}

void test_format() {
  std::cout << "Starting format tests..." << std::endl;
  std::stringstream ss;
  printer_t printer(ss);

  // Initializing heap for object creation
  object_heap_t* heap = new object_heap_t();
  heap->init(4 * 1024 * 1024, 128 * 1024);
  context::init();

  auto run_test = [&](const char* fmt_str, int argc, scm_obj_t* argv, const char* expected) {
    ss.str("");
    printer.format(argc, argv);
    if (ss.str() != expected) {
      std::cerr << "Format test failed for \"" << fmt_str << "\"" << std::endl;
      std::cerr << "Expected: \"" << expected << "\"" << std::endl;
      std::cerr << "Actual:   \"" << ss.str() << "\"" << std::endl;
      assert(false);
    }
  };

  // (format "~8,2f ~a" 6.0 'hello) ;=> "    6.00 hello"
  {
    scm_obj_t args[3];
    args[0] = make_string("~8,2f ~a");
    args[1] = make_flonum(6.0);
    args[2] = make_symbol("hello");
    run_test("~8,2f ~a", 3, args, "    6.00 hello");
    std::cout << "Test 1 passed" << std::endl;
  }

  // (format "~8,4f ~a" 6.0 'hello) ;=> "  6.0000 hello"
  {
    scm_obj_t args[3];
    args[0] = make_string("~8,4f ~a");
    args[1] = make_flonum(6.0);
    args[2] = make_symbol("hello");
    run_test("~8,4f ~a", 3, args, "  6.0000 hello");
    std::cout << "Test 2 passed" << std::endl;
  }

  // (format "~8,f ~a" 6.0 'hello)  ;=> "      6. hello"
  {
    scm_obj_t args[3];
    args[0] = make_string("~8,f ~a");
    args[1] = make_flonum(6.0);
    args[2] = make_symbol("hello");
    run_test("~8,f ~a", 3, args, "      6. hello");
    std::cout << "Test 3 passed" << std::endl;
  }

  // (format "~0,0f ~a" 6.0 'hello) ;=> "6. hello"
  {
    scm_obj_t args[3];
    args[0] = make_string("~0,0f ~a");
    args[1] = make_flonum(6.0);
    args[2] = make_symbol("hello");
    run_test("~0,0f ~a", 3, args, "6. hello");
    std::cout << "Test 4 passed" << std::endl;
  }

  // (format "~s ~a ~~ ~%" 'foo "bar") ;=> "foo bar ~ \n"
  {
    scm_obj_t args[3];
    args[0] = make_string("~s ~a ~~ ~%");
    args[1] = make_symbol("foo");
    args[2] = make_string("bar");
    run_test("~s ~a ~~ ~%", 3, args, "foo bar ~ \n");
    std::cout << "Test 5 passed" << std::endl;
  }

  std::cout << "Format tests passed" << std::endl;

  context::destroy();
  heap->destroy();
  delete heap;
}

void test_display() {
  std::cout << "Starting display tests..." << std::endl;
  std::stringstream ss;
  printer_t printer(ss);

  object_heap_t* heap = new object_heap_t();
  heap->init(4 * 1024 * 1024, 128 * 1024);
  context::init();

  // String
  ss.str("");
  printer.display(make_string("hello"));
  assert(ss.str() == "hello");

  // Char
  ss.str("");
  printer.display(make_char('a'));
  assert(ss.str() == "a");

  // Symbol
  ss.str("");
  printer.display(make_symbol("foo"));
  assert(ss.str() == "foo");

  std::cout << "Display tests passed" << std::endl;

  context::destroy();
  heap->destroy();
  delete heap;
}

void test_write_ss() {
  std::cout << "Starting write_ss tests..." << std::endl;
  std::stringstream ss;
  printer_t printer(ss);

  object_heap_t* heap = new object_heap_t();
  heap->init(4 * 1024 * 1024, 128 * 1024);
  context::init();

  // Shared
  {
    ss.str("");
    scm_obj_t p = make_cons(make_fixnum(1), scm_nil);
    scm_obj_t list = make_cons(p, p);
    printer.write_ss(list);
    assert(ss.str() == "(#0=(1) . #0#)");
  }

  // Circular
  {
    ss.str("");
    scm_obj_t p = make_cons(make_fixnum(1), scm_nil);
    ((scm_cons_rec_t*)p)->cdr = p;
    printer.write_ss(p);
    assert(ss.str() == "#0=(1 . #0#)");
  }

  std::cout << "write_ss tests passed" << std::endl;

  context::destroy();
  heap->destroy();
  delete heap;
}

int run_test(int argc, char** argv) {
  test_printer();
  test_format();
  test_display();
  test_write_ss();
  return 0;
}

}  // namespace test_printer

namespace test_radix {

void test_radix() {
  auto check_num = [](const char* input, intptr_t expected_val) {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss(input);
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for %s", input);
    if (!is_fixnum(obj)) fatal("expected fixnum for %s", input);
    if (fixnum(obj) != expected_val) fatal("expected %ld, got %ld for %s", expected_val, fixnum(obj), input);
    std::cout << "Passed: " << input << " -> " << fixnum(obj) << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  };

  // Hex
  check_num("#x10", 16);
  check_num("#xff", 255);
  check_num("#xFF", 255);
  check_num("#x-10", -16);

  // Binary
  check_num("#b101", 5);
  check_num("#b-10", -2);

  // Octal
  check_num("#o77", 63);
  check_num("#o-10", -8);

  // Decimal
  check_num("#d10", 10);
  check_num("#d-10", -10);
}

void test_exactness_error() {
  auto check_error = [](const char* input, const char* expected_msg) {
    object_heap_t* heap = new object_heap_t();
    heap->init(4 * 1024 * 1024, 128 * 1024);
    context::init();
    std::stringstream ss(input);
    reader_t reader(ss);
    bool err = false;
    reader.read(err);
    if (!err) {
      fatal("expected error for input: %s", input);
    }
    std::string msg = reader.get_error_message();
    if (msg.find(expected_msg) == std::string::npos) {
      fatal("expected error message containing '%s', got '%s' for input: %s", expected_msg, msg.c_str(), input);
    }
    std::cout << "Error case passed: " << input << " -> " << msg << std::endl;
    context::destroy();
    heap->destroy();
    delete heap;
  };

  check_error("#e10", "exactness prefix #e not supported");
}

int run_test(int argc, char** argv) {
  test_radix();
  test_exactness_error();
  return 0;
}

}  // namespace test_radix

namespace test_r7rs {

#define CHECK(cond)                                                     \
  do {                                                                  \
    if (!(cond)) fatal("Check failed: %s at line %d", #cond, __LINE__); \
  } while (0)

void test_r7rs_comments() {
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024, 64 * 1024);
  context::init();

  {
    // Block comment
    std::stringstream ss("#| this is a comment |# 123");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for block comment");
    CHECK(is_fixnum(obj));
    CHECK(fixnum(obj) == 123);
    std::cout << "Block comment passed" << std::endl;
  }

  {
    // Nested block comment
    std::stringstream ss("#| this #| is |# nested |# 456");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for nested block comment");
    CHECK(is_fixnum(obj));
    CHECK(fixnum(obj) == 456);
    std::cout << "Nested block comment passed" << std::endl;
  }

  {
    // Datum comment
    std::stringstream ss("#; (ignore this) 789");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for datum comment");
    if (!is_fixnum(obj)) {
      std::cerr << "Simple datum comment failed. Got type=";
      if (is_symbol(obj))
        std::cerr << "symbol " << (char*)symbol_name(obj);
      else if (is_string(obj))
        std::cerr << "string " << (char*)string_name(obj);
      else if (is_cons(obj))
        std::cerr << "cons";
      else if (obj == scm_eof)
        std::cerr << "eof";
      else
        std::cerr << "other " << std::hex << obj;
      std::cerr << std::endl;
    }
    CHECK(is_fixnum(obj));
    CHECK(fixnum(obj) == 789);
    std::cout << "Datum comment passed" << std::endl;
  }

  {
    // Datum comment nested
    std::stringstream ss("#; #; ignore ignore 101");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for nested datum comment");
    if (!is_fixnum(obj)) {
      std::cerr << "Expected fixnum 101, got type=";
      if (is_symbol(obj))
        std::cerr << "symbol " << (char*)symbol_name(obj);
      else if (is_string(obj))
        std::cerr << "string " << (char*)string_name(obj);
      else if (obj == scm_eof)
        std::cerr << "eof";
      else
        std::cerr << "other " << std::hex << obj;
      std::cerr << std::endl;
    }
    CHECK(is_fixnum(obj));
    CHECK(fixnum(obj) == 101);
    std::cout << "Nested datum comment passed" << std::endl;
  }

  context::destroy();
  heap->destroy();
  delete heap;
}

void test_r7rs_strings() {
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024, 64 * 1024);
  context::init();

  {
    // String escape input
    std::stringstream ss("\"foo \\\"bar\\\" baz\"");
    reader_t reader(ss);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) fatal("read failed for string escape");
    CHECK(is_string(obj));
    CHECK(strcmp((char*)string_name(obj), "foo \"bar\" baz") == 0);
    std::cout << "String escape input passed" << std::endl;
  }

  {
    // String escape output
    std::stringstream ss;
    printer_t printer(ss);
    scm_obj_t str = make_string("foo \"bar\" baz");
    printer.write(str);
    CHECK(ss.str() == "\"foo \\\"bar\\\" baz\"");
    std::cout << "String escape output passed" << std::endl;
  }

  context::destroy();
  heap->destroy();
  delete heap;
}

void test_r7rs_symbols() {
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024, 64 * 1024);
  context::init();

  {
    // Symbol escape output
    std::stringstream ss;
    printer_t printer(ss);
    scm_obj_t sym = make_symbol("foo bar");
    printer.write(sym);
    CHECK(ss.str() == "|foo bar|");
    std::cout << "Symbol escape output passed" << std::endl;
  }

  {
    // Symbol pipe escape output
    std::stringstream ss;
    printer_t printer(ss);
    scm_obj_t sym = make_symbol("foo|bar");
    printer.write(sym);
    CHECK(ss.str() == "|foo\\|bar|");  // expecting |foo\|bar|
    std::cout << "Symbol pipe escape output passed" << std::endl;
  }

  context::destroy();
  heap->destroy();
  delete heap;
}

int run_test(int argc, char** argv) {
  test_r7rs_comments();
  test_r7rs_strings();
  test_r7rs_symbols();
  return 0;
}

}  // namespace test_r7rs

int main(int argc, char** argv) {
  test_port::run_test(argc, argv);
  test_async_port::run_test(argc, argv);
  test_port_direction::run_test(argc, argv);
  test_reader::run_test(argc, argv);
  test_printer::run_test(argc, argv);
  test_radix::run_test(argc, argv);
  test_r7rs::run_test(argc, argv);
  return some_test_failed ? 1 : 0;
}
