// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"

#include <fstream>   // IWYU pragma: keep
#include <iostream>  // IWYU pragma: keep
#include <istream>   // IWYU pragma: keep
#include <ostream>   // IWYU pragma: keep
#include <sstream>   // IWYU pragma: keep
#include <variant>
#include <cstring>
#include "utf8.h"

template <typename T> inline constexpr bool is_output_stream() {
  return std::is_same_v<T, std::ostream*> || std::is_same_v<T, std::iostream*> || std::is_same_v<T, std::ofstream*> ||
         std::is_same_v<T, std::fstream*> || std::is_same_v<T, std::stringstream*>;
}

template <typename T> inline constexpr bool is_input_stream() {
  return std::is_same_v<T, std::istream*> || std::is_same_v<T, std::iostream*> || std::is_same_v<T, std::ifstream*> ||
         std::is_same_v<T, std::fstream*> || std::is_same_v<T, std::stringstream*>;
}

void port_finalize(scm_port_rec_t* rec) {
  if (rec->aux == nullptr) return;
  if (rec->aux->owned) {
    std::visit(
        [](auto&& arg) {
          using T = std::decay_t<decltype(arg)>;
          if constexpr (!std::is_same_v<T, std::monostate>) {
            if (arg != nullptr) delete arg;
          }
        },
        rec->aux->stream);
  }
  rec->aux->stream = std::monostate{};
  delete rec->aux;
  rec->aux = nullptr;
}

void port_close(scm_obj_t port) {
  assert(is_port(port));
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  port_finalize(rec);
}

std::ostream* port_get_ostream(scm_obj_t port) {
  assert(is_port(port));
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  return std::visit(
      [](auto&& arg) -> std::ostream* {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (is_output_stream<T>()) return arg;
        return nullptr;
      },
      rec->aux->stream);
}

std::istream* port_get_istream(scm_obj_t port) {
  assert(is_port(port));
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  return std::visit(
      [](auto&& arg) -> std::istream* {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (is_input_stream<T>()) return arg;
        return nullptr;
      },
      rec->aux->stream);
}

scm_obj_t port_standard_input() {
  scm_obj_t port = make_port(make_symbol("standard-input-port"));
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  rec->aux->owned = false;
  rec->aux->stream = &std::cin;
  return port;
}

scm_obj_t port_standard_output() {
  scm_obj_t port = make_port(make_symbol("standard-output-port"));
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  rec->aux->owned = false;
  rec->aux->stream = &std::cout;
  return port;
}

scm_obj_t port_standard_error() {
  scm_obj_t port = make_port(make_symbol("standard-error-port"));
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  rec->aux->owned = false;
  rec->aux->stream = &std::cerr;
  return port;
}

scm_obj_t port_open_input_file(const char* filename) {
  std::cout << "port_open_input_file: " << filename << std::endl;
  std::ifstream* ifs = new std::ifstream(filename, std::ios::binary);
  if (!ifs->is_open()) {
    delete ifs;
    throw std::runtime_error("open-file-input-port: cannot open file: " + std::string(filename));
  }
  scm_obj_t port = make_port(make_string(filename));
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  rec->aux->owned = true;
  rec->aux->stream = ifs;
  return port;
}

scm_obj_t port_open_output_file(const char* filename) {
  std::ofstream* ofs = new std::ofstream(filename, std::ios::binary);
  if (!ofs->is_open()) {
    delete ofs;
    throw std::runtime_error("open-file-output-port: cannot open file: " + std::string(filename));
  }
  scm_obj_t port = make_port(make_string(filename));
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  rec->aux->owned = true;
  rec->aux->stream = ofs;
  return port;
}

scm_obj_t port_flush_output(scm_obj_t port) {
  assert(is_port(port));
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  std::visit(
      [](auto&& arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (is_output_stream<T>()) {
          if (arg != nullptr) {
            arg->flush();
          } else {
            throw std::runtime_error("port_flush_output: port stream is null.");
          }
        } else {
          throw std::runtime_error("port_flush_output: port does not support output.");
        }
      },
      rec->aux->stream);
  return scm_unspecified;
}

scm_obj_t port_put_bytes(scm_obj_t port, const uint8_t* byte, int bsize) {
  assert(is_port(port));
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  std::visit(
      [byte, bsize](auto&& arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (is_output_stream<T>()) {
          if (arg != nullptr) {
            arg->write(reinterpret_cast<const char*>(byte), bsize);
            if (arg->fail()) {
              throw std::runtime_error("port_put_bytes: output failed.");
            }
          } else {
            throw std::runtime_error("port_put_bytes: port stream is null.");
          }
        } else {
          throw std::runtime_error("port_put_bytes: port does not support output.");
        }
      },
      rec->aux->stream);
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
  std::stringstream* ss = new std::stringstream();
  scm_obj_t port = make_port(make_symbol("string-output-port"));
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  rec->aux->owned = true;
  rec->aux->stream = ss;
  return port;
}

scm_obj_t port_get_output_string(scm_obj_t port) {
  assert(is_port(port));
  scm_port_rec_t* rec = (scm_port_rec_t*)to_address(port);
  return std::visit(
      [](auto&& arg) -> scm_obj_t {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, std::stringstream*>) {
          if (arg != nullptr) {
            std::string s = arg->str();
            arg->str("");
            arg->clear();
            return make_string(s.c_str());
          } else {
            throw std::runtime_error("port_get_output_string: port stream is null.");
          }
        } else {
          throw std::runtime_error("port_get_output_string: port is not a string output port.");
        }
      },
      rec->aux->stream);
}
