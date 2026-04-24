// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "utf8.h"

#include "asio.h"

#include <boost/asio.hpp>
#include <boost/fiber/all.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/device/file_descriptor.hpp>
#include <boost/iostreams/stream.hpp>
#include <cstring>
#include <fcntl.h>
#include <iostream>
#include <istream>
#include <ostream>
#include <poll.h>
#include <sstream>
#include <system_error>
#include <unistd.h>
#include <vector>

template <typename T> void safe_delete(T*& ptr) {
  delete ptr;
  ptr = nullptr;
}

static scm_obj_t port_object(scm_obj_t name, std::istream* is, std::ostream* os, std::iostream* ios, int fd) {
  scm_obj_t port = make_port(name);
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  rec->istream = is;
  rec->ostream = os;
  rec->iostream = ios;
  rec->fd = fd;
  return port;
}

void port_finalize(scm_port_rec_t* rec) {
  asio_stream_finalize(rec);
  safe_delete(rec->iostream);
  safe_delete(rec->istream);
  safe_delete(rec->ostream);
}

void port_close(scm_obj_t port) {
  assert(is_port(port));
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  port_finalize(rec);
}

std::ostream* port_get_ostream(scm_obj_t port) {
  assert(is_port(port));
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  if (rec->iostream != nullptr) return rec->iostream;
  return rec->ostream;
}

std::istream* port_get_istream(scm_obj_t port) {
  assert(is_port(port));
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  if (rec->iostream != nullptr) return rec->iostream;
  return rec->istream;
}

scm_obj_t port_standard_input() {
  using source_t = boost::iostreams::file_descriptor_source;
  auto* in = new boost::iostreams::stream<source_t>(source_t(STDIN_FILENO, boost::iostreams::never_close_handle));
  return port_object(make_symbol("standard-input-port"), in, nullptr, nullptr, STDIN_FILENO);
}

scm_obj_t port_standard_output() {
  using sink_t = boost::iostreams::file_descriptor_sink;
  auto* out = new boost::iostreams::stream<sink_t>(sink_t(STDOUT_FILENO, boost::iostreams::never_close_handle));
  return port_object(make_symbol("standard-output-port"), nullptr, out, nullptr, STDOUT_FILENO);
}

scm_obj_t port_standard_error() {
  using sink_t = boost::iostreams::file_descriptor_sink;
  auto* err = new boost::iostreams::stream<sink_t>(sink_t(STDERR_FILENO, boost::iostreams::never_close_handle));
  return port_object(make_symbol("standard-error-port"), nullptr, err, nullptr, STDERR_FILENO);
}

scm_obj_t port_open_input_file(const char* filename) {
  int fd = open(filename, O_RDONLY);
  if (fd < 0) throw std::runtime_error("open-file-input-port: cannot open file: " + std::string(filename));
  using source_t = boost::iostreams::file_descriptor_source;
  auto* ifs = new boost::iostreams::stream<source_t>(source_t(fd, boost::iostreams::close_handle));
  return port_object(make_string(filename), ifs, nullptr, nullptr, fd);
}

scm_obj_t port_open_output_file(const char* filename) {
  int fd = open(filename, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  if (fd < 0) throw std::runtime_error("open-file-output-port: cannot open file: " + std::string(filename));
  using sink_t = boost::iostreams::file_descriptor_sink;
  auto* ofs = new boost::iostreams::stream<sink_t>(sink_t(fd, boost::iostreams::close_handle));
  return port_object(make_string(filename), nullptr, ofs, nullptr, fd);
}

scm_obj_t port_flush_output(scm_obj_t port) {
  assert(is_port(port));
  std::ostream* out = port_get_ostream(port);
  if (out != nullptr) {
    out->flush();
  } else {
    throw std::runtime_error("port_flush_output: port does not support output.");
  }
  return scm_unspecified;
}

scm_obj_t port_put_bytes(scm_obj_t port, const uint8_t* byte, int bsize) {
  assert(is_port(port));
  std::ostream* out = port_get_ostream(port);
  if (out != nullptr) {
    out->write(reinterpret_cast<const char*>(byte), bsize);
    if (out->fail()) {
      throw std::runtime_error("port_put_bytes: output failed.");
    }
  } else {
    throw std::runtime_error("port_put_bytes: port does not support output.");
  }
  return scm_unspecified;
}

scm_obj_t port_put_byte(scm_obj_t port, uint8_t byte) { return port_put_bytes(port, &byte, 1); }

scm_obj_t port_put_char(scm_obj_t port, scm_obj_t ch) {
  assert(is_port(port));
  assert(is_char(ch));
  uint32_t c = (uint32_t)(ch >> 32);
  uint8_t utf8[4];
  int n = cnvt_ucs4_to_utf8(c, utf8);
  return port_put_bytes(port, utf8, n);
}

scm_obj_t port_put_string(scm_obj_t port, scm_obj_t str) {
  assert(is_port(port));
  assert(is_string(str));
  const char* s = (const char*)string_name(str);
  return port_put_bytes(port, (const uint8_t*)s, (int)strlen(s));
}

scm_obj_t port_open_string_output_port() {
  return port_object(make_symbol("string-output-port"), nullptr, nullptr, new std::stringstream(), -1);
}

scm_obj_t port_get_output_string(scm_obj_t port) {
  assert(is_port(port));
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  if (rec->iostream != nullptr) {
    std::stringstream* ss = dynamic_cast<std::stringstream*>(rec->iostream);
    if (ss != nullptr) {
      std::string s = ss->str();
      ss->str("");
      ss->clear();
      return make_string(s.c_str());
    }
  }
  throw std::runtime_error("port_get_output_string: port is not a string output port.");
}

bool fd_nonblock_byte_ready(int fd) {
  struct pollfd pfd;
  pfd.fd = fd;
  pfd.events = POLLIN;
  pfd.revents = 0;
  int state = poll(&pfd, 1, 0);
  if (state < 0) {
    if (errno == EINTR) return false;
    throw std::system_error(errno, std::generic_category(), "poll failed");
  }
  return (state > 0 && (pfd.revents & (POLLIN | POLLHUP | POLLERR)));
}

bool port_nonblock_byte_ready(scm_obj_t port) {
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  if (rec->fd < 0) return true;
  return fd_nonblock_byte_ready(rec->fd);
}

scm_obj_t port_get_bytes(scm_obj_t port, int bsize) {
  assert(is_port(port));
  std::istream* in = port_get_istream(port);
  assert(in);
  if (bsize == 0) return make_u8vector(0);
  std::vector<uint8_t> buffer(bsize);
  in->read(reinterpret_cast<char*>(buffer.data()), bsize);
  std::streamsize read_size = in->gcount();
  if (read_size == 0) {
    if (in->eof()) return scm_eof;
    throw std::runtime_error("port_get_bytes: read failed.");
  }
  scm_obj_t bv = make_u8vector(read_size);
  memcpy(u8vector_elts(bv), buffer.data(), read_size);
  return bv;
}

scm_obj_t port_get_bytes_n(scm_obj_t port, int n) {
  assert(is_port(port));
  return port_get_bytes(port, n);
}

scm_obj_t port_put_string_async(scm_obj_t port, scm_obj_t str) {
  assert(is_port(port));
  assert(is_string(str));
  const char* s = (const char*)string_name(str);
  return asio_put_bytes_async(port, (const uint8_t*)s, (int)strlen(s));
}

scm_obj_t port_get_bytes_n_async(scm_obj_t port, int n) {
  assert(is_port(port));
  scm_obj_t bv = make_u8vector(n);
  return asio_get_bytes_async(port, bv);
}
