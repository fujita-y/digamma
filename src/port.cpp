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

template <typename T> inline constexpr bool is_output_stream() {
  return std::is_same_v<T, std::ostream*> || std::is_same_v<T, std::iostream*> || std::is_same_v<T, std::ofstream*> ||
         std::is_same_v<T, std::fstream*> || std::is_same_v<T, std::stringstream*>;
}

template <typename T> inline constexpr bool is_input_stream() {
  return std::is_same_v<T, std::istream*> || std::is_same_v<T, std::iostream*> || std::is_same_v<T, std::ifstream*> ||
         std::is_same_v<T, std::fstream*> || std::is_same_v<T, std::stringstream*>;
}

void port_finalize(scm_port_rec_t* rec) {
  port_aux_t* aux = rec->aux;
  if (aux == nullptr) return;
  if (aux->owned) {
    std::visit(
        [](auto&& arg) {
          using T = std::decay_t<decltype(arg)>;
          if constexpr (!std::is_same_v<T, std::monostate>) {
            if (arg != nullptr) {
              delete arg;
            }
          }
        },
        aux->stream);
  }
  aux->stream = std::monostate{};
  delete aux;
  aux = nullptr;
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
/*
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
*/